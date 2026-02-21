use crate::diff_utils;
use crate::engine::types::{BranchInfo, CommitInfo, SplitData};
use crate::patch_utils;
use crate::topology::HistoryContext;
use git2::{BranchType, Oid, Repository};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

pub trait Git: Send + Sync {
    fn get_branches(
        &self,
        progress: Option<&dyn Fn(String, f64)>,
    ) -> anyhow::Result<(Vec<BranchInfo>, HistoryContext)>;
    fn get_current_branch(&self) -> anyhow::Result<String>;
    fn check_conflict(
        &self,
        branch_name: &str,
        new_parent_name: &str,
        base_name: Option<&str>,
    ) -> anyhow::Result<bool>;
    fn check_conflict_between(
        &self,
        oid_a: Oid,
        oid_b: Oid,
        base_oid: Option<Oid>,
    ) -> anyhow::Result<bool>;
    fn is_descendant(&self, ancestor: Oid, descendant: Oid) -> anyhow::Result<bool>;
    fn rebase(&self, branch: &str, onto: &str) -> anyhow::Result<bool>;
    fn checkout(&self, branch: &str) -> anyhow::Result<()>;
    fn push(&self, branch: &str) -> anyhow::Result<bool>;
    fn reset_to_upstream(&self, branch: &str) -> anyhow::Result<bool>;
    fn delete_branch(&self, branch: &str) -> anyhow::Result<bool>;
    fn run_command(&self, args: &[String]) -> anyhow::Result<bool>;
    fn is_dirty(&self) -> anyhow::Result<bool>;
    fn find_parent_for_oid(
        &self,
        oid: Oid,
        exclude_name: &str,
        branches: &HashMap<String, Oid>,
    ) -> Option<String>;
    fn get_commit_log(&self, branch: &str) -> anyhow::Result<Vec<CommitInfo>>;
    fn get_diff(&self, branch: &str, parent: &str) -> anyhow::Result<Vec<diff_utils::FileDiff>>;
    fn split_branch(&self, branch: &str, parent: &str, data: &SplitData) -> anyhow::Result<()>;
}

pub struct RealGit {
    pub repo: std::sync::Mutex<Repository>,
    pub path: PathBuf,
    descendant_cache: std::sync::Mutex<HashMap<(Oid, Oid), bool>>,
    branch_tips_cache: std::sync::Mutex<Option<Vec<(String, Oid)>>>,
}

pub(crate) struct RawBranchData {
    pub name: String,
    pub oid: Oid,
    pub is_local: bool,
    pub is_remote: bool,
    pub upstream_oid: Option<Oid>,
    pub upstream_name: Option<String>,
}

impl RealGit {
    pub fn new<P: AsRef<Path>>(path: P) -> anyhow::Result<Self> {
        let repo = Repository::discover(path.as_ref())?;
        Ok(Self {
            repo: std::sync::Mutex::new(repo),
            path: path.as_ref().to_path_buf(),
            descendant_cache: std::sync::Mutex::new(HashMap::new()),
            branch_tips_cache: std::sync::Mutex::new(None),
        })
    }

    fn git_cmd(&self) -> std::process::Command {
        let mut cmd = std::process::Command::new("git");
        cmd.arg("-C")
            .arg(&self.path)
            .env_remove("GIT_DIR")
            .env_remove("GIT_WORK_TREE");
        cmd
    }

    fn repo(&self) -> anyhow::Result<std::sync::MutexGuard<'_, Repository>> {
        self.repo
            .lock()
            .map_err(|_| anyhow::anyhow!("Repository mutex poisoned"))
    }

    fn detect_heuristic_parent(
        &self,
        repo: &Repository,
        oid: Oid,
        branch_name: &str,
        parent_oid: Option<Oid>,
        summary_to_branch: &HashMap<String, String>,
        branch_to_oid: &HashMap<String, Oid>,
    ) -> (Option<String>, Option<Oid>) {
        let mut heuristic_parent = None;
        let mut heuristic_upstream_oid = None;

        if let Ok(mut walk) = repo.revwalk() {
            let _ = walk.push(oid);
            let _ = walk.set_sorting(git2::Sort::TOPOLOGICAL);
            for commit_oid in walk.flatten() {
                if let Ok(commit) = repo.find_commit(commit_oid)
                    && let Some(summary) = commit.summary()
                    && let Some(other_branch) = summary_to_branch.get(summary)
                    && other_branch != branch_name
                {
                    // Find OID of other_branch
                    let other_oid = branch_to_oid.get(other_branch);

                    // Avoid picking a descendant as a heuristic parent (prevents cycles)
                    if let Some(&o_oid) = other_oid
                        && repo.graph_descendant_of(o_oid, oid).unwrap_or(false)
                    {
                        continue;
                    }

                    // We only care about this as a heuristic if it's NOT the current topological parent tip.
                    // If it matches the topological parent but that parent has moved, it IS a valid heuristic (divergence).
                    let is_at_other_tip = Some(&commit_oid) == other_oid;

                    // If it's another branch tip at OUR tip, it's just an alias, not a parent.
                    if is_at_other_tip && commit_oid == oid {
                        continue;
                    }

                    let is_current_parent_tip = is_at_other_tip && Some(commit_oid) == parent_oid;

                    if !is_current_parent_tip {
                        heuristic_parent = Some(other_branch.clone());
                        heuristic_upstream_oid = Some(commit_oid);
                    }
                    break;
                }
            }
        }
        (heuristic_parent, heuristic_upstream_oid)
    }

    fn calculate_ahead_behind(
        &self,
        repo: &Repository,
        oid: Oid,
        other_oid: Option<Oid>,
        is_local: bool,
    ) -> (usize, usize) {
        if let (Some(uoid), true) = (other_oid, is_local)
            && let Ok((a, b)) = repo.graph_ahead_behind(oid, uoid)
        {
            (a, b)
        } else {
            (0, 0)
        }
    }

    pub(crate) fn collect_raw_branch_data(&self) -> anyhow::Result<Vec<RawBranchData>> {
        let mut branch_data = Vec::new();
        let repo = self.repo()?;

        let local_branches = repo.branches(Some(BranchType::Local))?;
        for branch_res in local_branches {
            let (branch, _) = branch_res?;
            let name = branch
                .name()?
                .ok_or_else(|| anyhow::anyhow!("Invalid branch name"))?
                .to_string();
            let oid = branch.get().peel_to_commit()?.id();
            let mut upstream_name = None;
            let mut upstream_oid = None;
            if let Ok(upstream) = branch.upstream()
                && let Some(uname) = upstream.name()?.map(|s| s.to_string())
            {
                upstream_name = Some(uname);
                upstream_oid = upstream.get().peel_to_commit().ok().map(|c| c.id());
            }
            branch_data.push(RawBranchData {
                name,
                oid,
                is_local: true,
                is_remote: false,
                upstream_oid,
                upstream_name,
            });
        }

        let mut branch_names_set: HashSet<String> =
            branch_data.iter().map(|b| b.name.clone()).collect();
        let remote_branches = repo.branches(Some(BranchType::Remote))?;
        for branch_res in remote_branches {
            let (branch, _) = branch_res?;
            let name = branch
                .name()?
                .ok_or_else(|| anyhow::anyhow!("Invalid branch name"))?
                .to_string();

            if name.contains("/HEAD")
                || (!name.starts_with("origin/") && !name.starts_with("upstream/"))
            {
                continue;
            }

            if branch_names_set.contains(&name) {
                continue;
            }

            let oid = branch.get().peel_to_commit()?.id();
            branch_data.push(RawBranchData {
                name: name.clone(),
                oid,
                is_local: false,
                is_remote: true,
                upstream_oid: None,
                upstream_name: None,
            });
            branch_names_set.insert(name);
        }
        let branch_tips = branch_data
            .iter()
            .map(|b| (b.name.clone(), b.oid))
            .collect();
        *self
            .branch_tips_cache
            .lock()
            .map_err(|_| anyhow::anyhow!("Branch tips cache mutex poisoned"))? = Some(branch_tips);

        Ok(branch_data)
    }

    pub(crate) fn get_representative_map(
        &self,
        branch_data: &[RawBranchData],
    ) -> HashMap<Oid, String> {
        let mut representative_map: HashMap<Oid, (String, bool)> = HashMap::new();
        for b in branch_data {
            representative_map
                .entry(b.oid)
                .and_modify(|(current_best_name, current_best_is_local)| {
                    let is_better = if b.is_local && !*current_best_is_local {
                        true
                    } else if !b.is_local && *current_best_is_local {
                        false
                    } else {
                        is_better_name(&b.name, current_best_name)
                    };

                    if is_better {
                        *current_best_name = b.name.clone();
                        *current_best_is_local = b.is_local;
                    }
                })
                .or_insert_with(|| (b.name.clone(), b.is_local));
        }
        representative_map
            .into_iter()
            .map(|(oid, (name, _))| (oid, name))
            .collect()
    }

    pub(crate) fn get_topological_parents(
        &self,
        representative_map: &HashMap<Oid, String>,
        progress: Option<&dyn Fn(String, f64)>,
    ) -> anyhow::Result<HashMap<Oid, Option<Oid>>> {
        let repo = self.repo()?;
        let mut oid_to_parent_oid: HashMap<Oid, Option<Oid>> = HashMap::new();
        let unique_oids: Vec<Oid> = representative_map.keys().cloned().collect();

        // Optimization: Instead of N walks, we do 1 walk.
        // We track "active searches".
        // active_branches: Map<CommitOID, Set<BranchIndex>>
        // If a branch index is in the set for CommitOID, it means that branch
        // is looking for a parent, and has reached CommitOID.

        let mut active_branches: HashMap<Oid, HashSet<usize>> = HashMap::new();
        let mut walk = repo.revwalk()?;

        if let Some(p) = progress {
            p("Analyzing branch topology...".to_string(), 0.3);
        }

        for (i, &oid) in unique_oids.iter().enumerate() {
            walk.push(oid)?;
            active_branches.entry(oid).or_default().insert(i);
        }

        walk.set_sorting(git2::Sort::TOPOLOGICAL)?;

        for commit_oid_res in walk {
            let commit_oid = commit_oid_res?;

            if let Some(indices) = active_branches.remove(&commit_oid) {
                if indices.is_empty() {
                    continue;
                }

                // Check if the current commit is itself a branch tip (a potential parent)
                let is_tip = representative_map.contains_key(&commit_oid);

                // Get parents to propagate search
                let commit = repo.find_commit(commit_oid)?;
                let parents: Vec<Oid> = commit.parent_ids().collect();

                for idx in indices {
                    let branch_oid = unique_oids[idx];

                    // If we hit a tip, and it's NOT the branch itself, we found the parent.
                    if is_tip && branch_oid != commit_oid {
                        oid_to_parent_oid.insert(branch_oid, Some(commit_oid));
                    } else {
                        // Otherwise, propagate the search to parents
                        if parents.is_empty() {
                            // Reached root, no parent found
                            oid_to_parent_oid.insert(branch_oid, None);
                        } else {
                            for &p_oid in &parents {
                                active_branches.entry(p_oid).or_default().insert(idx);
                            }
                        }
                    }
                }
            }

            if active_branches.is_empty() {
                break;
            }
        }

        Ok(oid_to_parent_oid)
    }
}

pub fn is_better_name(other: &str, current: &str) -> bool {
    if other == current {
        return false;
    }
    match (other, current) {
        ("master", _) => true,
        (_, "master") => false,
        ("main", _) => true,
        (_, "main") => false,
        _ => {
            if other.starts_with("upstream/") && current.starts_with("origin/") {
                return true;
            }
            if other.starts_with("origin/") && current.starts_with("upstream/") {
                return false;
            }
            other < current
        }
    }
}

pub fn is_better_heuristic_parent(new: &str, existing: &str) -> bool {
    if new == existing {
        return false;
    }

    let new_is_main = new == "master" || new == "main";
    let existing_is_main = existing == "master" || existing == "main";

    if !new_is_main && existing_is_main {
        return true;
    }
    if new_is_main && !existing_is_main {
        return false;
    }

    let new_is_remote = new.starts_with("origin/") || new.starts_with("upstream/");
    let existing_is_remote = existing.starts_with("origin/") || existing.starts_with("upstream/");

    if !new_is_remote && existing_is_remote {
        return true;
    }
    if new_is_remote && !existing_is_remote {
        return false;
    }

    new < existing
}

impl Git for RealGit {
    fn find_parent_for_oid(
        &self,
        oid: Oid,
        exclude_name: &str,
        branch_map: &HashMap<String, Oid>,
    ) -> Option<String> {
        let mut best_parent: Option<String> = None;
        let mut best_base: Option<Oid> = None;

        for (other_name, other_oid) in branch_map {
            if exclude_name == other_name {
                continue;
            }

            if oid == *other_oid && !is_better_name(other_name, exclude_name) {
                continue;
            }

            // Avoid deadlock: check_merged_status locks Repo -> Cache.
            // We must not hold Cache lock while acquiring Repo lock.
            let is_descendant = if oid == *other_oid {
                true
            } else {
                let key = (oid, *other_oid);
                if let Some(&val) = self.descendant_cache.lock().unwrap().get(&key) {
                    val
                } else {
                    let val = self
                        .repo
                        .lock()
                        .unwrap()
                        .graph_descendant_of(oid, *other_oid)
                        .unwrap_or(false);
                    self.descendant_cache.lock().unwrap().insert(key, val);
                    val
                }
            };

            if is_descendant {
                let is_better = if let Some(current_best_oid) = best_base {
                    if *other_oid == current_best_oid {
                        let best_parent_name = best_parent.as_ref().expect("Invariant violated");
                        is_better_name(other_name, best_parent_name)
                    } else {
                        let key = (*other_oid, current_best_oid);
                        if let Some(&val) = self.descendant_cache.lock().unwrap().get(&key) {
                            val
                        } else {
                            let val = self
                                .repo
                                .lock()
                                .unwrap()
                                .graph_descendant_of(*other_oid, current_best_oid)
                                .unwrap_or(false);
                            self.descendant_cache.lock().unwrap().insert(key, val);
                            val
                        }
                    }
                } else {
                    true
                };

                if is_better {
                    best_base = Some(*other_oid);
                    best_parent = Some(other_name.clone());
                }
            }
        }
        best_parent
    }

    fn get_branches(
        &self,
        progress: Option<&dyn Fn(String, f64)>,
    ) -> anyhow::Result<(Vec<BranchInfo>, HistoryContext)> {
        if let Some(p) = progress {
            p("Collecting branch data...".to_string(), 0.1);
        }
        let branch_data = self.collect_raw_branch_data()?;

        if let Some(p) = progress {
            p("Mapping branches...".to_string(), 0.2);
        }
        let representative_map = self.get_representative_map(&branch_data);

        if let Some(p) = progress {
            p("Calculating topology...".to_string(), 0.3);
        }
        let oid_to_parent_oid = self.get_topological_parents(&representative_map, progress)?;

        let mut history = HistoryContext::new();
        history.oid_to_ancestor = oid_to_parent_oid.clone();
        history.oid_to_visible_branch = representative_map.clone();

        let repo = self.repo()?;
        let main_oids: Vec<Oid> = ["master", "main"]
            .iter()
            .filter_map(|name| {
                branch_data
                    .iter()
                    .find(|b| &b.name == name && b.is_local)
                    .map(|b| b.oid)
            })
            .collect();

        let mut remote_aliases: HashMap<String, Vec<String>> = HashMap::new();
        let mut filtered_branch_data = Vec::new();
        for b in branch_data {
            let best_rep = representative_map
                .get(&b.oid)
                .ok_or_else(|| anyhow::anyhow!("Failed to find representative for OID"))?;
            if &b.name != best_rep && b.is_remote {
                remote_aliases
                    .entry(best_rep.clone())
                    .or_default()
                    .push(b.name.clone());
            } else {
                filtered_branch_data.push(b);
            }
        }

        let mut summary_to_branch: HashMap<String, String> = HashMap::new();
        for b in &filtered_branch_data {
            if let Ok(commit) = repo.find_commit(b.oid)
                && let Some(summary) = commit.summary()
            {
                summary_to_branch
                    .entry(summary.to_string())
                    .and_modify(|existing| {
                        if is_better_heuristic_parent(&b.name, existing) {
                            *existing = b.name.clone();
                        }
                    })
                    .or_insert_with(|| b.name.clone());
            }
        }

        let mut branch_to_oid: HashMap<String, Oid> = HashMap::new();
        for b in &filtered_branch_data {
            branch_to_oid.insert(b.name.clone(), b.oid);
        }

        // Pre-calculate merge status
        let mut merge_status_map: HashMap<Oid, (bool, bool)> = HashMap::new();
        {
            // Recursive helper to resolve status
            fn get_status(
                repo: &Repository,
                oid: Oid,
                main_oids: &[Oid],
                oid_to_parent_oid: &HashMap<Oid, Option<Oid>>,
                descendant_cache: &mut HashMap<(Oid, Oid), bool>,
                cache: &mut HashMap<Oid, (bool, bool)>,
            ) -> (bool, bool) {
                if let Some(&res) = cache.get(&oid) {
                    return res;
                }

                // If this is one of the main branches, it is merged and not ahead (relative to itself)
                if main_oids.contains(&oid) {
                    let res = (true, false);
                    cache.insert(oid, res);
                    return res;
                }

                let parent_opt = oid_to_parent_oid.get(&oid).copied().flatten();

                let (p_merged, p_ahead) = if let Some(p_oid) = parent_opt {
                    get_status(
                        repo,
                        p_oid,
                        main_oids,
                        oid_to_parent_oid,
                        descendant_cache,
                        cache,
                    )
                } else {
                    (false, false) // Default for root (will force check)
                };

                // Optimization 1: If parent is NOT merged, child cannot be merged.
                let is_merged = if !p_merged && parent_opt.is_some() {
                    false
                } else {
                    // Standard check
                    let mut merged = false;
                    for &m_oid in main_oids {
                        if m_oid == oid {
                            merged = true;
                            break;
                        }
                        let is_desc = *descendant_cache.entry((m_oid, oid)).or_insert_with(|| {
                            repo.graph_descendant_of(m_oid, oid).unwrap_or(false)
                        });
                        if is_desc {
                            merged = true;
                            break;
                        }
                    }
                    merged
                };

                // Optimization 2: If parent IS ahead, child MUST be ahead.
                let is_ahead = if p_ahead {
                    true
                } else {
                    let mut ahead = false;
                    for &m_oid in main_oids {
                        if m_oid == oid {
                            continue;
                        }
                        let is_desc = *descendant_cache.entry((oid, m_oid)).or_insert_with(|| {
                            repo.graph_descendant_of(oid, m_oid).unwrap_or(false)
                        });
                        if is_desc {
                            ahead = true;
                            break;
                        }
                    }
                    ahead
                };

                let res = (is_merged, is_ahead);
                cache.insert(oid, res);
                res
            }

            let mut descendant_cache_guard = self.descendant_cache.lock().unwrap();
            for b in &filtered_branch_data {
                get_status(
                    &repo,
                    b.oid,
                    &main_oids,
                    &oid_to_parent_oid,
                    &mut descendant_cache_guard,
                    &mut merge_status_map,
                );
            }
        }

        let mut branches = Vec::new();
        let total = filtered_branch_data.len();
        for (i, b) in filtered_branch_data.into_iter().enumerate() {
            if let Some(p) = progress {
                p(
                    format!("Processing branch {}/{}: {}", i + 1, total, b.name),
                    0.4 + (i as f64 / total as f64) * 0.5,
                );
            }
            let best_rep = representative_map
                .get(&b.oid)
                .ok_or_else(|| anyhow::anyhow!("Missing representative for OID"))?;
            let parent = if &b.name != best_rep {
                Some(best_rep.clone())
            } else {
                oid_to_parent_oid
                    .get(&b.oid)
                    .and_then(|p_oid| p_oid.as_ref())
                    .map(|p_oid| {
                        representative_map
                            .get(p_oid)
                            .cloned()
                            .ok_or_else(|| anyhow::anyhow!("Missing representative for parent OID"))
                    })
                    .transpose()?
            };

            let mut aliases = remote_aliases.remove(&b.name).unwrap_or_default();
            aliases.sort();

            let p_oid = oid_to_parent_oid.get(&b.oid).and_then(|o| *o);
            let (heuristic_parent, heuristic_upstream_oid) = self.detect_heuristic_parent(
                &repo,
                b.oid,
                &b.name,
                p_oid,
                &summary_to_branch,
                &branch_to_oid,
            );

            let (ahead, behind) =
                self.calculate_ahead_behind(&repo, b.oid, b.upstream_oid, b.is_local);

            // Optimization: Look up parent OID from map instead of revparse
            let mut parent_to_check = parent.clone();
            if let Some(p) = &parent
                && let Some(stripped) = p
                    .strip_prefix("origin/")
                    .or_else(|| p.strip_prefix("upstream/"))
                && branch_to_oid.contains_key(stripped)
            {
                parent_to_check = Some(stripped.to_string());
            }

            let parent_oid_val = parent_to_check
                .as_ref()
                .and_then(|p_name| branch_to_oid.get(p_name))
                .copied();

            let (parent_ahead, parent_behind) =
                self.calculate_ahead_behind(&repo, b.oid, parent_oid_val, b.is_local);

            let (mut is_merged, is_ahead) = merge_status_map
                .get(&b.oid)
                .copied()
                .unwrap_or((false, false));

            if ["master", "main"].contains(&b.name.as_str()) {
                is_merged = false;
            }

            branches.push(BranchInfo {
                name: b.name,
                oid: b.oid,
                upstream_oid: b.upstream_oid,
                original_parent: parent,
                children: Vec::new(),
                ahead,
                behind,
                parent_ahead,
                parent_behind,
                upstream: b.upstream_name,
                is_merged,
                is_ahead,
                is_local: b.is_local,
                is_remote: b.is_remote,
                aliases,
                heuristic_parent,
                heuristic_upstream_oid,
            });
        }

        if let Some(p) = progress {
            p("Finalizing branch list...".to_string(), 0.9);
        }

        branches.sort_by(|a, b| a.name.cmp(&b.name));

        Ok((branches, history))
    }

    fn get_current_branch(&self) -> anyhow::Result<String> {
        let repo = self.repo()?;
        let head = repo.head()?;
        if head.is_branch() {
            let name = head
                .shorthand()
                .ok_or_else(|| anyhow::anyhow!("Invalid branch name"))?;
            Ok(name.to_string())
        } else {
            anyhow::bail!("Not on a branch")
        }
    }

    fn check_conflict(
        &self,
        branch_name: &str,
        new_parent_name: &str,
        base_name: Option<&str>,
    ) -> anyhow::Result<bool> {
        let repo = self.repo()?;
        let branch_oid = repo.revparse_single(branch_name)?.peel_to_commit()?.id();
        let parent_oid = repo
            .revparse_single(new_parent_name)?
            .peel_to_commit()?
            .id();
        let base_oid = base_name
            .map(|name| {
                repo.revparse_single(name)
                    .and_then(|obj| Ok(obj.peel_to_commit()?.id()))
            })
            .transpose()?;

        drop(repo);
        self.check_conflict_between(branch_oid, parent_oid, base_oid)
    }

    fn check_conflict_between(
        &self,
        branch_oid: Oid,
        parent_oid: Oid,
        base_oid: Option<Oid>,
    ) -> anyhow::Result<bool> {
        let repo = self.repo()?;
        let our_commit = repo.find_commit(branch_oid)?;
        let their_commit = repo.find_commit(parent_oid)?;

        let index = if let Some(b_oid) = base_oid {
            let base_commit = repo.find_commit(b_oid)?;
            repo.merge_trees(
                &base_commit.tree()?,
                &their_commit.tree()?,
                &our_commit.tree()?,
                None,
            )?
        } else {
            repo.merge_commits(&their_commit, &our_commit, None)?
        };

        Ok(index.has_conflicts())
    }

    fn is_descendant(&self, ancestor: Oid, descendant: Oid) -> anyhow::Result<bool> {
        let repo = self.repo()?;
        Ok(repo.graph_descendant_of(descendant, ancestor)?)
    }

    fn rebase(&self, branch: &str, onto: &str) -> anyhow::Result<bool> {
        let status = self.git_cmd().args(["rebase", onto, branch]).status()?;
        Ok(status.success())
    }

    fn checkout(&self, branch: &str) -> anyhow::Result<()> {
        let status = self.git_cmd().args(["checkout", branch]).status()?;
        if status.success() {
            Ok(())
        } else {
            anyhow::bail!("Checkout failed")
        }
    }

    fn push(&self, branch: &str) -> anyhow::Result<bool> {
        let status = self
            .git_cmd()
            .args(["push", "--force-with-lease", "origin", branch])
            .status()?;
        Ok(status.success())
    }

    fn delete_branch(&self, branch: &str) -> anyhow::Result<bool> {
        let status = self.git_cmd().args(["branch", "-d", branch]).status()?;
        Ok(status.success())
    }

    fn run_command(&self, args: &[String]) -> anyhow::Result<bool> {
        let status = self.git_cmd().args(args).status()?;
        Ok(status.success())
    }

    fn is_dirty(&self) -> anyhow::Result<bool> {
        let repo = self.repo()?;
        let mut status_options = git2::StatusOptions::new();
        status_options.include_untracked(false);
        status_options.update_index(true);
        status_options.exclude_submodules(true);
        let statuses = repo.statuses(Some(&mut status_options))?;

        Ok(!statuses.is_empty())
    }

    fn reset_to_upstream(&self, branch: &str) -> anyhow::Result<bool> {
        self.checkout(branch)?;
        let status = self.git_cmd().args(["reset", "--hard", "@{u}"]).status()?;
        Ok(status.success())
    }

    fn get_commit_log(&self, branch: &str) -> anyhow::Result<Vec<CommitInfo>> {
        let start_oid = {
            let repo = self.repo()?;
            repo.revparse_single(branch)?.peel_to_commit()?.id()
        };

        let branch_tips = {
            let mut cache = self
                .branch_tips_cache
                .lock()
                .map_err(|_| anyhow::anyhow!("Branch tips cache mutex poisoned"))?;
            if cache.is_none() {
                drop(cache);
                let _ = self.collect_raw_branch_data();
                cache = self
                    .branch_tips_cache
                    .lock()
                    .map_err(|_| anyhow::anyhow!("Branch tips cache mutex poisoned"))?;
            }
            cache
                .clone()
                .ok_or_else(|| anyhow::anyhow!("Failed to populate branch tips cache"))?
        };

        let repo = self.repo()?;
        let mut walk = repo.revwalk()?;
        walk.push(start_oid)?;
        walk.set_sorting(git2::Sort::TOPOLOGICAL)?;

        for (other_name, other_oid) in branch_tips {
            if other_oid == start_oid {
                let other_shorthand = other_name.rsplit('/').next().unwrap_or(&other_name);
                let branch_shorthand = branch.rsplit('/').next().unwrap_or(branch);
                if other_shorthand != branch_shorthand {
                    let _ = walk.hide(other_oid);
                }
            } else {
                let is_descendant = *self
                    .descendant_cache
                    .lock()
                    .map_err(|_| anyhow::anyhow!("Descendant cache mutex poisoned"))?
                    .entry((other_oid, start_oid))
                    .or_insert_with(|| {
                        repo.graph_descendant_of(other_oid, start_oid)
                            .unwrap_or(false)
                    });

                if !is_descendant {
                    let _ = walk.hide(other_oid);
                }
            }
        }

        let mut logs = Vec::new();
        for commit_oid_res in walk {
            let commit_oid = commit_oid_res?;

            let commit = repo.find_commit(commit_oid)?;
            let summary = commit.summary().unwrap_or("").to_string();
            let author = commit.author().name().unwrap_or("").to_string();
            let short_id = commit.id().to_string()[..7].to_string();
            logs.push(CommitInfo {
                id: short_id,
                summary,
                author,
            });

            if logs.len() >= 10 {
                break;
            }
        }
        Ok(logs)
    }

    fn get_diff(&self, branch: &str, parent: &str) -> anyhow::Result<Vec<diff_utils::FileDiff>> {
        let repo = self.repo()?;
        let branch_oid = repo.revparse_single(branch)?.peel_to_commit()?.id();
        let parent_oid = repo.revparse_single(parent)?.peel_to_commit()?.id();
        let branch_tree = repo.find_commit(branch_oid)?.tree()?;
        let parent_tree = repo.find_commit(parent_oid)?.tree()?;
        let mut diff = repo.diff_tree_to_tree(Some(&parent_tree), Some(&branch_tree), None)?;
        let mut find_opts = git2::DiffFindOptions::new();
        find_opts.renames(true);
        find_opts.remove_unmodified(true);
        diff.find_similar(Some(&mut find_opts))?;
        diff_utils::parse_diff(&diff)
    }

    fn split_branch(&self, branch: &str, parent: &str, data: &SplitData) -> anyhow::Result<()> {
        let repo = self.repo()?;

        let branch_obj = repo.revparse_single(branch)?;
        let branch_commit = branch_obj.peel_to_commit()?;
        let parent_obj = repo.revparse_single(parent)?;
        let mut current_parent_commit = parent_obj.peel_to_commit()?;

        let mut diff = repo.diff_tree_to_tree(
            Some(&current_parent_commit.tree()?),
            Some(&branch_commit.tree()?),
            None,
        )?;
        let mut find_opts = git2::DiffFindOptions::new();
        find_opts.renames(true);
        find_opts.remove_unmodified(true);
        diff.find_similar(Some(&mut find_opts))?;

        let signature = repo.signature()?;

        for part in &data.parts {
            let new_tree_oid = patch_utils::apply_selected_hunks_to_tree(
                &repo,
                &current_parent_commit.tree()?,
                &diff,
                &part.selected_hunks,
            )?;

            let new_tree = repo.find_tree(new_tree_oid)?;

            let new_commit_oid = repo.commit(
                None,
                &signature,
                &signature,
                &part.commit_message,
                &new_tree,
                &[&current_parent_commit],
            )?;

            let new_commit = repo.find_commit(new_commit_oid)?;
            repo.reference(
                &format!("refs/heads/{}", part.name),
                new_commit_oid,
                true,
                "split part",
            )?;

            current_parent_commit = new_commit;
        }

        let final_commit_oid = repo.commit(
            None,
            &signature,
            &signature,
            branch_commit.message().unwrap_or(""),
            &branch_commit.tree()?,
            &[&current_parent_commit],
        )?;

        repo.reference(
            &format!("refs/heads/{}", branch),
            final_commit_oid,
            true,
            "split branch",
        )?;

        Ok(())
    }
}
