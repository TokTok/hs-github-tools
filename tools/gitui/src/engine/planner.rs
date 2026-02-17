use crate::engine::git::Git;
use crate::engine::topology::flatten_branches;
use crate::engine::types::{
    BranchInfo, BranchIntent, Operation, RepositorySnapshot, get_local_name,
};
use git2::Oid;
use std::collections::{HashMap, HashSet};

pub fn calculate_plan(
    snapshot: &RepositorySnapshot,
    intents: &HashMap<String, BranchIntent>,
) -> anyhow::Result<Vec<Operation>> {
    let mut branch_map: HashMap<String, &BranchInfo> = snapshot
        .branches
        .iter()
        .map(|b| (b.name.clone(), b))
        .collect();

    for b in &snapshot.branches {
        for alias in &b.aliases {
            branch_map.insert(alias.clone(), b);
        }
    }

    let flattened = flatten_branches(&snapshot.branches, intents, &snapshot.history, true)?;

    let mut ctx = PlanContext {
        snapshot,
        intents,
        branch_map,
        flattened,
        plan: Vec::new(),
        rebased_or_reset_branches: HashSet::new(),
        effectively_dirty: snapshot.is_dirty,
        submitted_branches: HashSet::new(),
    };

    ctx.calculate()?;
    Ok(ctx.plan)
}

struct PlanContext<'a> {
    snapshot: &'a RepositorySnapshot,
    intents: &'a HashMap<String, BranchIntent>,
    branch_map: HashMap<String, &'a BranchInfo>,
    flattened: Vec<(String, usize)>,
    plan: Vec<Operation>,
    rebased_or_reset_branches: HashSet<String>,
    effectively_dirty: bool,
    submitted_branches: HashSet<String>,
}

impl<'a> PlanContext<'a> {
    fn calculate(&mut self) -> anyhow::Result<()> {
        self.handle_amends_and_splits()?;
        self.handle_submits()?;
        self.handle_localizations()?;
        self.handle_resets_and_rebases()?;
        self.handle_post_ops()?;
        Ok(())
    }

    fn handle_amends_and_splits(&mut self) -> anyhow::Result<()> {
        for (branch_name, _) in &self.flattened {
            if let Some(branch_info) = self.branch_map.get(branch_name) {
                let intent = self.intents.get(branch_name).cloned().unwrap_or_default();

                if intent.pending_amend {
                    self.plan.push(Operation::Amend {
                        message: intent.pending_amend_message.clone(),
                    });
                    self.rebased_or_reset_branches.insert(branch_name.clone());
                    self.effectively_dirty = false;
                }

                if let Some(split_data) = &intent.pending_split {
                    let parent = intent
                        .parent
                        .as_ref()
                        .cloned()
                        .flatten()
                        .or_else(|| branch_info.original_parent.clone())
                        .unwrap_or_else(|| "master".to_string());

                    self.plan.push(Operation::Split {
                        branch: branch_name.clone(),
                        parent,
                        data: split_data.clone(),
                    });
                    self.rebased_or_reset_branches.insert(branch_name.clone());
                    for part in &split_data.parts {
                        self.rebased_or_reset_branches.insert(part.name.clone());
                    }
                }
            }
        }
        Ok(())
    }

    fn handle_submits(&mut self) -> anyhow::Result<()> {
        let main_branch_name = self
            .snapshot
            .branches
            .iter()
            .find(|b| b.name == "master" || b.name == "main")
            .map(|b| b.name.clone())
            .unwrap_or_else(|| "master".to_string());

        let mut branches_to_delete = HashSet::new();

        for b in &self.snapshot.branches {
            let intent = self.intents.get(&b.name).cloned().unwrap_or_default();
            if intent.pending_submit && !self.effectively_dirty {
                self.plan.push(Operation::Submit {
                    branch: b.name.clone(),
                    target: main_branch_name.clone(),
                });
                self.rebased_or_reset_branches
                    .insert(main_branch_name.clone());

                branches_to_delete.insert(b.name.clone());

                // Also delete all local ancestors in the stack, as they are now merged.
                for other_b in &self.snapshot.branches {
                    if other_b.name == main_branch_name
                        || other_b.name == b.name
                        || !other_b.is_local
                    {
                        continue;
                    }

                    if crate::engine::topology::is_descendant(
                        &self.snapshot.branches,
                        self.intents,
                        &b.name,
                        &other_b.name,
                    ) {
                        branches_to_delete.insert(other_b.name.clone());
                        self.submitted_branches.insert(other_b.name.clone());
                    }
                }

                self.submitted_branches.insert(b.name.clone());
            }
        }

        // We sort the deletions to ensure a stable order for tests and a clean UI.
        let mut sorted_deletions: Vec<_> = branches_to_delete.into_iter().collect();
        sorted_deletions.sort();

        for branch_name in sorted_deletions {
            self.plan.push(Operation::Delete {
                branch: branch_name,
            });
        }

        Ok(())
    }

    fn handle_localizations(&mut self) -> anyhow::Result<()> {
        for (branch_name, _) in &self.flattened {
            if let Some(branch_info) = self.branch_map.get(branch_name) {
                let intent = self.intents.get(branch_name).cloned().unwrap_or_default();

                let parent = intent
                    .parent
                    .as_ref()
                    .cloned()
                    .flatten()
                    .or_else(|| branch_info.original_parent.clone());

                let needs_localization = intent.pending_localize
                    || (branch_info.is_remote && parent != branch_info.original_parent);

                if needs_localization && !self.effectively_dirty {
                    let local_name = get_local_name(branch_name);
                    if !self.branch_map.contains_key(local_name) || intent.pending_localize {
                        self.plan.push(Operation::Localize {
                            branch: branch_name.clone(),
                        });
                    }
                }
            }
        }
        Ok(())
    }

    fn handle_resets_and_rebases(&mut self) -> anyhow::Result<()> {
        for (branch_name, _) in &self.flattened {
            if self.submitted_branches.contains(branch_name) {
                continue;
            }

            if let Some(branch_info) = self.branch_map.get(branch_name) {
                let intent = self.intents.get(branch_name).cloned().unwrap_or_default();

                if intent.pending_amend || intent.pending_split.is_some() {
                    continue;
                }

                let effective_name = if branch_info.is_remote {
                    get_local_name(branch_name).to_string()
                } else {
                    branch_name.clone()
                };

                let mut acted = false;

                // Handle Reset/Sync to Upstream
                if intent.pending_reset && !self.effectively_dirty {
                    let is_main = branch_name == "master" || branch_name == "main";
                    if is_main
                        && let Some(upstream_info) =
                            self.branch_map.get(&format!("upstream/{}", branch_name))
                        && upstream_info.oid != branch_info.oid
                    {
                        self.plan.push(Operation::Sync {
                            branch: branch_name.clone(),
                            onto: format!("upstream/{}", branch_name),
                            predicted_conflict: None,
                        });
                        self.rebased_or_reset_branches.insert(branch_name.clone());
                        acted = true;
                    }

                    if !acted {
                        let is_noop = branch_info.upstream_oid == Some(branch_info.oid);

                        if !is_noop {
                            if let Some(upstream_name) = &branch_info.upstream {
                                let upstream_oid = get_rebase_upstream(
                                    branch_info,
                                    &self.branch_map,
                                    upstream_name,
                                );
                                self.plan.push(Operation::Rebase {
                                    branch: effective_name.clone(),
                                    onto: upstream_name.clone(),
                                    upstream: upstream_oid,
                                    predicted_conflict: None,
                                });
                            } else {
                                self.plan.push(Operation::Reset {
                                    branch: effective_name.clone(),
                                });
                            }
                            self.rebased_or_reset_branches.insert(branch_name.clone());
                            acted = true;
                        }
                    }
                }

                // Handle Rebase onto Parent
                if !acted && !self.effectively_dirty {
                    let parent = intent
                        .parent
                        .as_ref()
                        .cloned()
                        .flatten()
                        .or_else(|| branch_info.original_parent.clone());

                    let parent_changed = parent != branch_info.original_parent;
                    let parent_rebased = parent
                        .as_ref()
                        .is_some_and(|p| self.rebased_or_reset_branches.contains(p));

                    let upstream_oid_str = parent
                        .as_ref()
                        .and_then(|p| get_rebase_upstream(branch_info, &self.branch_map, p));

                    let current_topo_parent = self
                        .snapshot
                        .history
                        .oid_to_ancestor
                        .get(&branch_info.oid)
                        .and_then(|o| *o);

                    let upstream_differs = if let (Some(u_str), Some(current_p_oid)) =
                        (&upstream_oid_str, current_topo_parent)
                    {
                        if let Ok(u_oid) = Oid::from_str(u_str) {
                            u_oid != current_p_oid
                        } else {
                            false
                        }
                    } else {
                        false
                    };

                    if (parent_changed || parent_rebased || upstream_differs)
                        && let Some(new_parent_name) = parent
                    {
                        let mut is_noop = false;
                        let p_oid = resolve_oid(&new_parent_name, &self.branch_map, branch_info);

                        if !parent_rebased
                            && let Some(oid) = p_oid
                            && (oid == branch_info.oid || current_topo_parent == Some(oid))
                        {
                            // Already parented under the head of the target branch.
                            is_noop = true;
                        }

                        if !is_noop {
                            self.plan.push(Operation::Rebase {
                                branch: effective_name,
                                onto: new_parent_name,
                                upstream: upstream_oid_str,
                                predicted_conflict: None,
                            });
                            self.rebased_or_reset_branches.insert(branch_name.clone());
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn handle_post_ops(&mut self) -> anyhow::Result<()> {
        for b in &self.snapshot.branches {
            let intent = self.intents.get(&b.name).cloned().unwrap_or_default();
            if intent.pending_push {
                self.plan.push(Operation::Push {
                    branch: b.name.clone(),
                });
            }
            if intent.pending_delete {
                self.plan.push(Operation::Delete {
                    branch: b.name.clone(),
                });
            }
        }

        for (branch_name, _) in &self.flattened {
            if let Some(new_name) = self
                .intents
                .get(branch_name)
                .and_then(|i| i.pending_rename.as_ref())
            {
                self.plan.push(Operation::Rename {
                    branch: branch_name.clone(),
                    new_name: new_name.clone(),
                });
            }
        }
        Ok(())
    }
}

pub fn predict_conflicts(
    plan: &mut [Operation],
    git: &dyn Git,
    branches: &[BranchInfo],
    progress: Option<&dyn Fn(usize, Option<bool>)>,
) {
    let mut future_oids: HashMap<String, Oid> =
        branches.iter().map(|b| (b.name.clone(), b.oid)).collect();

    for (i, op) in plan.iter_mut().enumerate() {
        match op {
            Operation::Submit { branch, target } => {
                if let Some(branch_oid) = future_oids.get(branch).copied() {
                    future_oids.insert(target.clone(), branch_oid);
                }
                if let Some(p) = progress {
                    p(i, None);
                }
            }
            Operation::Rebase {
                branch,
                onto,
                upstream,
                predicted_conflict,
            } => {
                let (branch_oid, onto_oid) = (
                    future_oids.get(branch).copied(),
                    future_oids.get(onto).copied(),
                );
                let base_oid = upstream.as_ref().and_then(|u| Oid::from_str(u).ok());
                let res = if let (Some(b_oid), Some(o_oid)) = (branch_oid, onto_oid) {
                    git.check_conflict_between(b_oid, o_oid, base_oid).ok()
                } else {
                    None
                };
                *predicted_conflict = res;

                if let Some(o_oid) = onto_oid {
                    future_oids.insert(branch.clone(), o_oid);
                }

                if let Some(p) = progress {
                    p(i, res);
                }
            }
            Operation::Sync {
                branch,
                onto,
                predicted_conflict,
            } => {
                let (branch_oid, onto_oid) = (
                    future_oids.get(branch).copied(),
                    future_oids.get(onto).copied(),
                );
                let res = if let (Some(b_oid), Some(o_oid)) = (branch_oid, onto_oid) {
                    git.is_descendant(b_oid, o_oid).map(|is_desc| !is_desc).ok()
                } else {
                    None
                };
                *predicted_conflict = res;

                if let Some(o_oid) = onto_oid {
                    future_oids.insert(branch.clone(), o_oid);
                }

                if let Some(p) = progress {
                    p(i, res);
                }
            }
            Operation::Reset { branch } => {
                if let Some(b_info) = branches.iter().find(|b| b.name == *branch)
                    && let Some(u_oid) = b_info.upstream_oid
                {
                    future_oids.insert(branch.clone(), u_oid);
                }
                if let Some(p) = progress {
                    p(i, None);
                }
            }
            Operation::Localize { branch } => {
                if let Some(branch_oid) = future_oids.get(branch).copied() {
                    let local_name = crate::engine::types::get_local_name(branch);
                    future_oids.insert(local_name.to_string(), branch_oid);
                }
                if let Some(p) = progress {
                    p(i, None);
                }
            }
            Operation::Rename { branch, new_name } => {
                if let Some(branch_oid) = future_oids.get(branch).copied() {
                    future_oids.insert(new_name.clone(), branch_oid);
                }
                if let Some(p) = progress {
                    p(i, None);
                }
            }
            _ => {
                if let Some(p) = progress {
                    p(i, None);
                }
            }
        }
    }
}

fn resolve_oid(
    name: &str,
    branch_map: &HashMap<String, &BranchInfo>,
    context_branch: &BranchInfo,
) -> Option<Oid> {
    if let Some(info) = branch_map.get(name) {
        Some(info.oid)
    } else if let Ok(oid) = Oid::from_str(name) {
        Some(oid)
    } else if Some(name) == context_branch.upstream.as_deref() {
        context_branch.upstream_oid
    } else {
        None
    }
}

fn get_rebase_upstream(
    branch_info: &BranchInfo,
    branch_map: &HashMap<String, &BranchInfo>,
    onto_name: &str,
) -> Option<String> {
    if let (Some(h_parent), Some(h_oid)) = (
        &branch_info.heuristic_parent,
        branch_info.heuristic_upstream_oid,
    ) && h_parent == onto_name
    {
        return Some(h_oid.to_string());
    }

    branch_info
        .original_parent
        .as_ref()
        .and_then(|orig| resolve_oid(orig, branch_map, branch_info).map(|oid| oid.to_string()))
}
