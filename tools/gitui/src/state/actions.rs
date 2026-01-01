use crate::engine::{BranchIntent, Intent, build_topology};
use crate::state::types::{AppMode, AppState, ConflictCheckState, Effect, SidebarState};
use crate::ui::SPINNERS;
use std::time::{Duration, Instant};

impl AppState {
    pub fn mutate_intent<F>(&mut self, branch_name: &str, f: F)
    where
        F: FnOnce(&mut BranchIntent),
    {
        let intent = self.intents.entry(branch_name.to_string()).or_default();
        f(intent);
    }

    pub fn get_intent(&self, branch_name: &str) -> BranchIntent {
        self.intents.get(branch_name).cloned().unwrap_or_default()
    }

    pub fn get_effective_parent(&self, branch_name: &str) -> Option<String> {
        let b = self.branches.iter().find(|b| b.name == branch_name)?;
        let intent = self.intents.get(branch_name);
        match intent.and_then(|i| i.parent.as_ref()) {
            Some(p) => p.clone(),
            None => b.original_parent.clone(),
        }
    }

    pub fn try_apply_move(&mut self, name: &str, new_parent: Option<String>) -> bool {
        let mut topo = match build_topology(
            &self.branches,
            &self.intents,
            &self.history,
            self.show_remote,
        ) {
            Ok(t) => t,
            Err(_) => return false,
        };

        let is_valid = topo
            .set_parent(name, new_parent.as_deref(), None, Intent::Structural)
            .is_ok();

        if is_valid {
            let is_remote = self
                .branches
                .iter()
                .find(|b| b.name == name)
                .map(|b| b.is_remote)
                .unwrap_or(false);

            self.mutate_intent(name, |i| {
                i.parent = Some(new_parent);
                if is_remote {
                    i.pending_localize = true;
                }
            });
            true
        } else {
            false
        }
    }

    pub fn handle_tick(&mut self) -> Vec<Effect> {
        let mut effects = Vec::new();

        if self.is_loading || self.is_log_loading() {
            self.spinner_index = (self.spinner_index + 1) % SPINNERS.len();
            self.needs_redraw = true;
        }

        // Debounced commit log fetch
        if let SidebarState::Debouncing { ref branch, since } = self.sidebar
            && since.elapsed() >= Duration::from_millis(500)
        {
            let branch = branch.clone();
            self.sidebar = SidebarState::Loading {
                branch: branch.clone(),
            };
            self.needs_redraw = true;
            effects.push(Effect::FetchCommitLog { branch });
        }

        // Debounced conflict check
        if let ConflictCheckState::Debouncing {
            ref branch,
            ref onto,
            ref base,
            since,
        } = self.conflict_check
            && since.elapsed() >= Duration::from_millis(200)
        {
            if let Some(&has_conflict) =
                self.conflict_cache
                    .get(&(branch.clone(), onto.clone(), base.clone()))
            {
                let branch = branch.clone();
                self.conflict_check = ConflictCheckState::Idle;
                self.mutate_intent(&branch, |i| {
                    i.has_conflict = has_conflict;
                });
                self.needs_redraw = true;
            } else {
                let branch = branch.clone();
                let onto = onto.clone();
                let base = base.clone();
                self.conflict_check = ConflictCheckState::Checking {
                    branch: branch.clone(),
                    onto: onto.clone(),
                    base: base.clone(),
                };
                self.needs_redraw = true;
                effects.push(Effect::CheckConflict { branch, onto, base });
            }
        }

        effects
    }

    pub fn on_selection_change(&mut self) -> Vec<Effect> {
        if let Some(idx) = self.list_state.selected() {
            if let Some((name, _)) = self.flattened_tree.get(idx) {
                let name = name.clone();
                self.sidebar = SidebarState::Debouncing {
                    branch: name,
                    since: Instant::now(),
                };
            }
        } else {
            self.sidebar = SidebarState::Idle;
        }
        Vec::new()
    }

    pub fn on_move_change(&mut self) {
        if let (Some(grabbed), Some(target)) = (&self.grabbed_branch, &self.target_parent) {
            let base = self.get_effective_parent(grabbed);
            self.conflict_check = ConflictCheckState::Debouncing {
                branch: grabbed.clone(),
                onto: target.clone(),
                base,
                since: Instant::now(),
            };
        } else {
            self.conflict_check = ConflictCheckState::Idle;
        }
    }

    pub fn is_log_loading(&self) -> bool {
        matches!(self.sidebar, SidebarState::Loading { .. })
    }

    pub fn get_selected_branch_name(&self) -> Option<String> {
        self.list_state
            .selected()
            .and_then(|i| self.flattened_tree.get(i).map(|(n, _)| n.clone()))
    }

    pub fn refresh_tree(&mut self, preview_move: Option<(&str, Option<&str>)>) {
        let selected_name = self.get_selected_branch_name();

        let mut topo = match build_topology(
            &self.branches,
            &self.intents,
            &self.history,
            self.show_remote,
        ) {
            Ok(t) => t,
            Err(_) => return,
        };
        if let Some((grabbed, target)) = preview_move {
            let _ = topo.set_parent(grabbed, target, None, Intent::Structural);
        }

        self.virtual_layer.apply(&mut topo);

        let current_order: Vec<String> =
            self.flattened_tree.iter().map(|(n, _)| n.clone()).collect();
        topo.set_visual_memory(current_order);
        self.flattened_tree = topo.flatten();

        if let Some(name) = selected_name
            && let Some(idx) = self.flattened_tree.iter().position(|(n, _)| n == &name)
        {
            self.list_state.select(Some(idx));
        }
    }

    pub fn update_target_parent(&mut self) {
        if let Some(grabbed) = self.grabbed_branch.clone() {
            if let Some(idx) = self.list_state.selected() {
                if idx >= self.flattened_tree.len() {
                    return;
                }
                let hovered = &self.flattened_tree[idx].0;

                if hovered == &grabbed {
                    self.target_parent = self.get_effective_parent(&grabbed);
                } else {
                    let topo = match build_topology(
                        &self.branches,
                        &self.intents,
                        &self.history,
                        self.show_remote,
                    ) {
                        Ok(t) => t,
                        Err(_) => return,
                    };
                    // Check if target (hovered) is a descendant of grabbed.
                    let mut is_desc = false;
                    if let Some(&target_idx) = topo.branches.get(hovered)
                        && let Some(&grab_idx) = topo.branches.get(&grabbed)
                        && petgraph::algo::has_path_connecting(
                            &topo.graph,
                            target_idx,
                            grab_idx,
                            None,
                        )
                    {
                        is_desc = true;
                    }

                    if !is_desc {
                        self.target_parent = Some(hovered.clone());
                    } else {
                        self.target_parent = None;
                    }
                }
            }
            let target = self.target_parent.clone();
            self.refresh_tree(Some((&grabbed, target.as_deref())));
            self.on_move_change();
        }
    }

    pub fn clear_pending_operations(&mut self) {
        self.intents.clear();
        self.virtual_layer = crate::topology::virtual_layer::VirtualLayer::new();
        self.plan = None;
        self.mode = AppMode::Tree;
        self.show_preview = false;
    }
}
