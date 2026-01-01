use crate::engine::types::{BranchInfo, BranchIntent};
use crate::topology::{HistoryContext, Intent, VirtualTopology};
use git2::Oid;
use std::collections::HashMap;

pub fn build_topology(
    branches: &[BranchInfo],
    intents: &HashMap<String, BranchIntent>,
    history: &HistoryContext,
    show_remote: bool,
) -> anyhow::Result<VirtualTopology> {
    let mut topo = VirtualTopology::new();

    for b in branches {
        if !show_remote && b.is_remote {
            continue;
        }
        topo.add_branch(&b.name, b.oid);
    }

    for b in branches {
        if !show_remote && b.is_remote {
            continue;
        }
        let intent = intents.get(&b.name);
        if let Some(split_data) = intent.and_then(|i| i.pending_split.as_ref()) {
            let mut parent = match intent.and_then(|i| i.parent.as_ref()) {
                Some(p) => p.clone(),
                None => b.original_parent.clone(),
            };

            for part in &split_data.parts {
                topo.add_branch(&part.name, Oid::from_bytes(&[0; 20]).unwrap());
                let _ = topo.set_parent(&part.name, parent.as_deref(), None, Intent::Implicit);
                parent = Some(part.name.clone());
            }

            let _ = topo.set_parent(&b.name, parent.as_deref(), None, Intent::Implicit);
            continue;
        }

        let parent_name = match intent.and_then(|i| i.parent.as_ref()) {
            Some(p) => p.as_deref(),
            None => b.original_parent.as_deref(),
        };

        if let Some(parent_name) = parent_name {
            let mut resolved_oid = None;

            let parent_is_visible =
                if let Some(br) = branches.iter().find(|br| br.name == *parent_name) {
                    show_remote || !br.is_remote
                } else {
                    false
                };

            if parent_is_visible {
                topo.set_parent(&b.name, Some(parent_name), None, Intent::Implicit)?;
            } else {
                if let Ok(oid) = Oid::from_str(parent_name) {
                    resolved_oid = Some(oid);
                } else if Some(parent_name) == b.upstream.as_deref() {
                    resolved_oid = b.upstream_oid;
                }

                if let Some(oid) = resolved_oid {
                    if let Some(visible_parent) = history.resolve_visible_parent(oid, Some(&b.name))
                    {
                        if topo.branches.contains_key(&visible_parent) {
                            topo.set_parent(
                                &b.name,
                                Some(&visible_parent),
                                None,
                                Intent::Implicit,
                            )?;
                        } else {
                            topo.set_parent(&b.name, None, Some(oid), Intent::Implicit)?;
                        }
                    } else {
                        topo.set_parent(&b.name, None, Some(oid), Intent::Implicit)?;
                    }
                } else if parent_name.starts_with("origin/") || parent_name.starts_with("upstream/")
                {
                    let suffix = if let Some(s) = parent_name.strip_prefix("origin/") {
                        s
                    } else {
                        parent_name.strip_prefix("upstream/").unwrap_or(parent_name)
                    };
                    if let Some(br) = branches.iter().find(|br| br.name == suffix) {
                        if br.name == b.name {
                            if let Some(visible_parent) =
                                history.resolve_visible_parent(b.oid, Some(&b.name))
                                && topo.branches.contains_key(&visible_parent)
                            {
                                topo.set_parent(
                                    &b.name,
                                    Some(&visible_parent),
                                    None,
                                    Intent::Implicit,
                                )?;
                            }
                        } else if topo.branches.contains_key(&br.name) {
                            topo.set_parent(&b.name, Some(&br.name), None, Intent::Implicit)?;
                        }
                    }
                }
            }
        }

        // Heuristic parent is still repo data
        if let Some(h_parent) = &b.heuristic_parent
            && branches.iter().any(|br| br.name == *h_parent)
        {
            let _ = topo.set_parent(&b.name, Some(h_parent), None, Intent::Heuristic);
        }
    }

    Ok(topo)
}

pub fn flatten_branches(
    branches: &[BranchInfo],
    intents: &HashMap<String, BranchIntent>,
    history: &HistoryContext,
    show_remote: bool,
) -> anyhow::Result<Vec<(String, usize)>> {
    let topo = build_topology(branches, intents, history, show_remote)?;
    Ok(topo.flatten())
}

pub fn is_descendant(
    branches: &[BranchInfo],
    intents: &HashMap<String, BranchIntent>,
    potential_child: &str,
    potential_parent: &str,
) -> bool {
    let mut current = potential_child.to_string();
    let branch_map: HashMap<String, &BranchInfo> =
        branches.iter().map(|b| (b.name.clone(), b)).collect();

    let mut visited = std::collections::HashSet::new();

    while let Some(b) = branch_map.get(&current) {
        if !visited.insert(current.clone()) {
            break; // Cycle detected
        }
        let intent = intents.get(&current);
        let parent = match intent.and_then(|i| i.parent.as_ref()) {
            Some(p) => p.as_deref(),
            None => b.original_parent.as_deref(),
        };

        if let Some(p) = parent {
            if p == potential_parent {
                return true;
            }
            current = p.to_string();
        } else {
            break;
        }
    }
    false
}

pub fn apply_move(
    intents: &mut HashMap<String, BranchIntent>,
    name: &str,
    new_parent: Option<String>,
) -> anyhow::Result<()> {
    let intent = intents.entry(name.to_string()).or_default();
    intent.parent = Some(new_parent);

    // We can't easily update has_conflict here anymore since it's not in the struct.
    // The UI or the state manager will have to handle conflict checking.

    Ok(())
}
