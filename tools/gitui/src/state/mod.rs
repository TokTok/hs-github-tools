pub mod actions;
pub mod input;
pub mod reducer;
pub mod types;

pub use types::*;

use crate::engine::Operation;

impl AppState {
    pub fn update(&mut self, msg: Msg) -> Vec<Effect> {
        let mut effects = Vec::new();

        if !matches!(msg, Msg::Tick) {
            self.needs_redraw = true;
        }

        match msg {
            Msg::KeyPressed(key) => {
                effects.extend(self.handle_key(key));
            }
            Msg::Tick => {
                effects.extend(self.handle_tick());
            }
            Msg::BranchesLoaded(res) => match res {
                Ok((branches, history, is_dirty)) => {
                    self.branches = branches;
                    self.history = history;
                    self.is_dirty = is_dirty;
                    self.is_loading = false;
                    self.refresh_tree(None);

                    let initial_branch = self.initial_branch.clone();
                    if !initial_branch.is_empty()
                        && let Some(idx) = self
                            .flattened_tree
                            .iter()
                            .position(|(n, _)| n == &initial_branch)
                    {
                        self.list_state.select(Some(idx));
                    }

                    if self.list_state.selected().is_none() && !self.flattened_tree.is_empty() {
                        self.list_state.select(Some(0));
                    }

                    effects.extend(self.on_selection_change());
                }
                Err(e) => {
                    self.is_loading = false;
                    self.error_message = Some(format!("Error loading branches: {}", e));
                }
            },
            Msg::CurrentBranchLoaded(res) => match res {
                Ok(name) => {
                    self.initial_branch = name.clone();
                    if let Some(idx) = self.flattened_tree.iter().position(|(n, _)| n == &name) {
                        self.list_state.select(Some(idx));
                        effects.extend(self.on_selection_change());
                    }
                }
                Err(e) => {
                    self.error_message = Some(format!("Error getting current branch: {}", e));
                }
            },
            Msg::ConflictChecked(branch, onto, res) => {
                if let ConflictCheckState::Checking {
                    branch: ref b,
                    onto: ref o,
                } = self.conflict_check
                    && b == &branch
                    && o == &onto
                {
                    self.conflict_check = ConflictCheckState::Idle;
                    match res {
                        Ok(has_conflict) => {
                            self.conflict_cache
                                .insert((branch.clone(), onto), has_conflict);
                            if self.grabbed_branch.as_ref() == Some(&branch) {
                                self.mutate_intent(&branch, |i| {
                                    i.has_conflict = has_conflict;
                                });
                            }
                        }
                        Err(e) => {
                            self.error_message = Some(format!("Error checking conflict: {}", e));
                        }
                    }
                }
            }
            Msg::CommitLogLoaded(branch, res) => {
                if let SidebarState::Loading {
                    branch: ref loading_branch,
                } = self.sidebar
                    && loading_branch == &branch
                {
                    match res {
                        Ok(commits) => {
                            self.sidebar = SidebarState::Ready { branch, commits };
                        }
                        Err(_) => {
                            self.sidebar = SidebarState::Idle;
                            // We don't show an error for commit log failure to avoid being too noisy
                        }
                    }
                }
            }
            Msg::ProgressUpdated {
                message,
                percentage,
            } => {
                self.is_loading = true;
                self.progress_message = message;
                self.progress_percentage = percentage;
            }
            Msg::ConflictsPredicted(plan) => {
                self.plan = Some(plan);
                self.is_predicting_conflicts = false;
            }
            Msg::PredictionProgress { index, result } => {
                self.prediction_index = index;
                if let Some(plan) = &mut self.plan
                    && index < plan.len()
                    && let Operation::Rebase {
                        predicted_conflict, ..
                    } = &mut plan[index]
                {
                    *predicted_conflict = result;
                }
            }
            Msg::DiffLoaded(_branch, res) => {
                self.is_loading = false;
                match res {
                    Ok(files) => {
                        self.split_state = Some(crate::split_state::SplitState::new(files));
                    }
                    Err(e) => {
                        self.error_message = Some(format!("Error loading diff: {}", e));
                        self.mode = AppMode::Tree;
                    }
                }
            }
        }

        effects
    }
}
