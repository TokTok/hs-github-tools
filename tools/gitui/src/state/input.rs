use crate::engine::{BranchInfo, BranchIntent, RepositorySnapshot, calculate_plan};
use crate::split_state::{SplitState, SplitViewMode};
use crate::state::types::{AppMode, AppState, Effect, PromptAction, PromptFocus, PromptState};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use unicode_segmentation::UnicodeSegmentation;

impl AppState {
    pub fn handle_key(&mut self, key: KeyEvent) -> Vec<Effect> {
        if self.error_message.is_some() {
            self.error_message = None;
            return Vec::new();
        }

        if self.show_quit_confirmation {
            return self.handle_quit_confirmation(key);
        }

        match self.mode {
            AppMode::Split => {
                let mut split_state = self.split_state.take();
                let effects = if let Some(ref mut s) = split_state {
                    self.handle_key_split_inner(s, key)
                } else {
                    match (key.code, key.modifiers) {
                        (KeyCode::Esc, _) | (KeyCode::Char('q'), _) => {
                            self.is_loading = false;
                            self.mode = AppMode::Tree;
                            Vec::new()
                        }
                        _ => Vec::new(),
                    }
                };
                self.split_state = split_state;
                effects
            }
            AppMode::Preview => self.handle_key_preview(key),
            AppMode::Tree => self.handle_key_tree(key),
            AppMode::Prompt => self.handle_key_prompt(key),
        }
    }

    pub fn handle_key_prompt(&mut self, key: KeyEvent) -> Vec<Effect> {
        let mut prompt = match self.prompt.take() {
            Some(p) => p,
            None => {
                self.mode = AppMode::Tree;
                return Vec::new();
            }
        };

        match key.code {
            KeyCode::Tab => {
                prompt.focus = match prompt.focus {
                    PromptFocus::Input => PromptFocus::Ok,
                    PromptFocus::Ok => PromptFocus::Cancel,
                    PromptFocus::Cancel => PromptFocus::Input,
                };
                self.prompt = Some(prompt);
                return Vec::new();
            }
            KeyCode::BackTab => {
                prompt.focus = match prompt.focus {
                    PromptFocus::Input => PromptFocus::Cancel,
                    PromptFocus::Ok => PromptFocus::Input,
                    PromptFocus::Cancel => PromptFocus::Ok,
                };
                self.prompt = Some(prompt);
                return Vec::new();
            }
            KeyCode::Esc => {
                self.mode = AppMode::Split;
                self.prompt = None;
            }
            KeyCode::Enter => {
                match prompt.focus {
                    PromptFocus::Input
                        if prompt.action.is_multiline()
                            && !key.modifiers.contains(KeyModifiers::CONTROL) =>
                    {
                        prompt.value.insert(prompt.cursor_position, '\n');
                        prompt.cursor_position += 1;
                        self.prompt = Some(prompt);
                        return Vec::new();
                    }
                    PromptFocus::Cancel => {
                        self.mode = AppMode::Split;
                        self.prompt = None;
                        return Vec::new();
                    }
                    _ => {}
                }

                let value = prompt.value.clone();
                match prompt.action {
                    PromptAction::SplitPartName => {
                        let branch_name = self.splitting_branch.as_deref().unwrap_or("");
                        let part_num = self
                            .split_state
                            .as_ref()
                            .map(|s| s.parts.len() + 1)
                            .unwrap_or(1);
                        let default_msg = format!("Split from {}: part {}", branch_name, part_num);
                        let cursor_pos = default_msg.len();

                        // Switch to message prompt
                        self.prompt = Some(PromptState {
                            title: format!("Commit Message for: {}", value),
                            value: default_msg,
                            cursor_position: cursor_pos,
                            action: PromptAction::SplitPartMessage,
                            focus: PromptFocus::Input,
                        });
                        self.splitting_branch_temp_name = Some(value);
                        self.mode = AppMode::Prompt;
                    }
                    PromptAction::SplitPartMessage => {
                        let name = self.splitting_branch_temp_name.take().unwrap_or_default();
                        let message = value;
                        if let Some(ref mut split_state) = self.split_state {
                            let selected = split_state.current_selection.drain().collect();
                            split_state.parts.push(crate::split_state::SplitPart {
                                name,
                                commit_message: message,
                                selected_hunks: selected,
                            });
                            split_state.rebuild_view();
                        }
                        self.mode = AppMode::Split;
                        self.prompt = None;
                    }
                    PromptAction::AmendMessage => {
                        let message = self.prompt.as_ref().unwrap().value.clone();
                        if let Some(selected_name) = self.get_selected_branch_name() {
                            self.mutate_intent(&selected_name, |i| {
                                i.pending_amend_message = Some(message);
                            });
                        }
                    }
                    PromptAction::RenameBranch => {
                        let new_name = self.prompt.as_ref().unwrap().value.clone();
                        if let Some(selected_name) = self.get_selected_branch_name() {
                            self.mutate_intent(&selected_name, |i| {
                                i.pending_rename = Some(new_name);
                            });
                        }
                    }
                }
            }
            KeyCode::Char(c) if prompt.focus == PromptFocus::Input => {
                prompt.value.insert(prompt.cursor_position, c);
                prompt.cursor_position += c.len_utf8();
                self.prompt = Some(prompt);
            }
            KeyCode::Backspace if prompt.focus == PromptFocus::Input => {
                if prompt.cursor_position > 0 {
                    let pre_cursor = &prompt.value[..prompt.cursor_position];
                    if let Some(last_grapheme) = pre_cursor.graphemes(true).next_back() {
                        let len = last_grapheme.len();
                        prompt.cursor_position -= len;
                        prompt
                            .value
                            .drain(prompt.cursor_position..prompt.cursor_position + len);
                    }
                }
                self.prompt = Some(prompt);
            }
            KeyCode::Left => {
                if prompt.cursor_position > 0 {
                    let pre_cursor = &prompt.value[..prompt.cursor_position];
                    if let Some(last_grapheme) = pre_cursor.graphemes(true).next_back() {
                        prompt.cursor_position -= last_grapheme.len();
                    }
                }
                self.prompt = Some(prompt);
            }
            KeyCode::Right => {
                if prompt.cursor_position < prompt.value.len() {
                    let post_cursor = &prompt.value[prompt.cursor_position..];
                    if let Some(first_grapheme) = post_cursor.graphemes(true).next() {
                        prompt.cursor_position += first_grapheme.len();
                    }
                }
                self.prompt = Some(prompt);
            }
            KeyCode::Up => {
                if prompt.action.is_multiline() {
                    let pre_cursor = &prompt.value[..prompt.cursor_position];
                    // Find start of current line (searching backwards for '\n')
                    let current_line_start = pre_cursor.rfind('\n').map(|i| i + 1).unwrap_or(0);

                    // Calculate "visual" column (grapheme count)
                    let current_line_content = &pre_cursor[current_line_start..];
                    let col_graphemes = current_line_content.graphemes(true).count();

                    if current_line_start > 0 {
                        // There is a previous line
                        let prev_line_end = current_line_start - 1;
                        let text_before = &prompt.value[..prev_line_end];
                        let prev_line_start = text_before.rfind('\n').map(|i| i + 1).unwrap_or(0);

                        let prev_line_content = &prompt.value[prev_line_start..prev_line_end];

                        // Find equivalent grapheme position
                        let mut target_offset = 0;
                        for (current_grapheme_count, g) in
                            prev_line_content.graphemes(true).enumerate()
                        {
                            if current_grapheme_count == col_graphemes {
                                break;
                            }
                            target_offset += g.len();
                        }
                        prompt.cursor_position = prev_line_start + target_offset;
                    }
                }
                self.prompt = Some(prompt);
            }
            KeyCode::Down => {
                if prompt.action.is_multiline() {
                    // Check if there is a next line
                    let post_cursor = &prompt.value[prompt.cursor_position..];
                    if let Some(newline_idx_rel) = post_cursor.find('\n') {
                        let next_line_start = prompt.cursor_position + newline_idx_rel + 1;

                        // Calculate current column
                        let pre_cursor = &prompt.value[..prompt.cursor_position];
                        let current_line_start = pre_cursor.rfind('\n').map(|i| i + 1).unwrap_or(0);
                        let current_line_content = &pre_cursor[current_line_start..];
                        let col_graphemes = current_line_content.graphemes(true).count();

                        // Find next line content
                        let rest = &prompt.value[next_line_start..];
                        let next_line_end_rel = rest.find('\n').unwrap_or(rest.len());
                        let next_line_content = &rest[..next_line_end_rel];

                        // Find equivalent grapheme position
                        let mut target_offset = 0;
                        for (current_grapheme_count, g) in
                            next_line_content.graphemes(true).enumerate()
                        {
                            if current_grapheme_count == col_graphemes {
                                break;
                            }
                            target_offset += g.len();
                        }
                        prompt.cursor_position = next_line_start + target_offset;
                    }
                }
                self.prompt = Some(prompt);
            }
            _ => {
                self.prompt = Some(prompt);
            }
        }
        Vec::new()
    }

    pub fn handle_key_split_inner(
        &mut self,
        split_state: &mut SplitState,
        key: KeyEvent,
    ) -> Vec<Effect> {
        match (key.code, key.modifiers) {
            (KeyCode::Esc, _) | (KeyCode::Char('q'), _) => {
                self.mode = AppMode::Tree;
                self.splitting_branch = None;
                self.split_state = None;
                return Vec::new();
            }
            (KeyCode::Char('j'), _) | (KeyCode::Down, _) => {
                split_state.next();
            }
            (KeyCode::Char('k'), _) | (KeyCode::Up, _) => {
                split_state.prev();
            }
            (KeyCode::PageDown, _) => {
                split_state.page_down(15);
            }
            (KeyCode::PageUp, _) => {
                split_state.page_up(15);
            }
            (KeyCode::Char('l'), _) | (KeyCode::Right, _) => {
                if split_state.mode == SplitViewMode::Hunks {
                    split_state.enter_lines();
                } else {
                    split_state.enter_hunks();
                }
            }
            (KeyCode::Char('h'), _) | (KeyCode::Left, _) => {
                if split_state.mode == SplitViewMode::Lines {
                    split_state.exit_lines();
                } else {
                    split_state.exit_hunks();
                }
            }
            (KeyCode::Char(' '), _) => {
                split_state.toggle_selection();
            }
            (KeyCode::Enter, _) => {
                if !split_state.current_selection.is_empty() {
                    // If something is selected, Enter creates a new incremental part.
                    let branch_name = self.splitting_branch.as_deref().unwrap_or("");
                    let part_num = split_state.parts.len() + 1;
                    self.mode = AppMode::Prompt;
                    self.prompt = Some(PromptState {
                        title: format!("Branch Name for Part {}", part_num),
                        value: format!("{}-{}", branch_name, part_num),
                        cursor_position: format!("{}-{}", branch_name, part_num).len(),
                        action: PromptAction::SplitPartName,
                        focus: PromptFocus::Input,
                    });
                    return Vec::new();
                }

                if !split_state.parts.is_empty() {
                    let branch_name = match self.splitting_branch.take() {
                        Some(n) => n,
                        None => {
                            self.mode = AppMode::Tree;
                            return Vec::new();
                        }
                    };

                    let branch_info = match self.branches.iter().find(|b| b.name == branch_name) {
                        Some(b) => b,
                        None => {
                            self.mode = AppMode::Tree;
                            return Vec::new();
                        }
                    };

                    let mut parent = self
                        .get_effective_parent(&branch_name)
                        .unwrap_or_else(|| "master".to_string());

                    // Add to virtual layer
                    self.virtual_layer.hide_branch(&branch_name);

                    let mut parts_data = Vec::new();
                    for part in &split_state.parts {
                        self.virtual_layer.add_virtual_branch(
                            part.name.clone(),
                            Some(parent.clone()),
                            None,
                        );
                        parts_data.push((
                            part.name.clone(),
                            part.selected_hunks.clone(),
                            part.commit_message.clone(),
                        ));
                        parent = part.name.clone();
                    }

                    self.virtual_layer.add_virtual_branch(
                        branch_name.clone(),
                        Some(parent.clone()),
                        Some(branch_info.oid),
                    );

                    self.refresh_tree(None);

                    let mut split_data = crate::engine::SplitData { parts: Vec::new() };
                    for part in &split_state.parts {
                        split_data.parts.push(crate::engine::SplitPartData {
                            name: part.name.clone(),
                            commit_message: part.commit_message.clone(),
                            selected_hunks: part.selected_hunks.clone(),
                        });
                    }
                    self.mutate_intent(&branch_name, |i| {
                        i.pending_split = Some(split_data);
                    });

                    self.mode = AppMode::Tree;
                    self.split_state = None;
                }
            }
            _ => {}
        }
        Vec::new()
    }

    pub fn handle_key_preview(&mut self, key: KeyEvent) -> Vec<Effect> {
        match (key.code, key.modifiers) {
            (KeyCode::Esc, _) | (KeyCode::Char('q'), _) | (KeyCode::Char('v'), _) => {
                self.show_preview = false;
                self.mode = AppMode::Tree;
                Vec::new()
            }
            (KeyCode::Char('c'), _) => vec![Effect::ApplyAndQuit(
                self.branches.clone(),
                self.intents.clone(),
            )],
            _ => Vec::new(),
        }
    }

    pub fn handle_key_tree(&mut self, key: KeyEvent) -> Vec<Effect> {
        match (key.code, key.modifiers) {
            (KeyCode::Char('q'), _) => self.handle_quit(),
            (KeyCode::Esc, _) => self.handle_escape(),
            (KeyCode::Char('v'), _) => self.handle_preview_toggle(),
            (KeyCode::Char('a'), _) => self.handle_remote_toggle(),
            (KeyCode::Char('j'), _) | (KeyCode::Down, _) => self.handle_navigation(1),
            (KeyCode::Char('k'), _) | (KeyCode::Up, _) => self.handle_navigation(-1),
            (KeyCode::Char('h'), _) | (KeyCode::Left, _) => self.handle_move_to_root(),
            (KeyCode::Char(' '), _) => self.handle_space(),
            (KeyCode::Char('p'), _) => {
                let is_dirty = self.is_dirty;
                self.toggle_pending_action(|b, i| {
                    if !is_dirty
                        || i.pending_amend
                        || i.pending_split.is_some()
                        || i.parent != Some(b.original_parent.clone())
                    {
                        i.pending_push = !i.pending_push;
                    }
                });
                Vec::new()
            }
            (KeyCode::Char('s'), _) => {
                self.toggle_pending_action(|b, i| {
                    if b.can_submit() {
                        i.pending_submit = !i.pending_submit;
                    }
                });
                Vec::new()
            }
            (KeyCode::Char('x'), _) => self.handle_trigger_split(),
            (KeyCode::Char('d'), _) => {
                self.toggle_pending_action(|b, i| {
                    if b.is_local {
                        i.pending_delete = !i.pending_delete;
                    }
                });
                Vec::new()
            }
            (KeyCode::Char('r'), _) => {
                if !self.show_preview && !self.is_dirty {
                    self.handle_reset();
                }
                Vec::new()
            }
            (KeyCode::Char('f'), _) => {
                self.toggle_pending_action(|b, i| {
                    if b.is_remote {
                        i.pending_localize = !i.pending_localize;
                    }
                });
                Vec::new()
            }
            (KeyCode::Char('m'), _) => {
                let initial_branch = self.initial_branch.clone();
                self.toggle_pending_action(|b, i| {
                    if b.is_local && b.name == initial_branch {
                        i.pending_amend = !i.pending_amend;
                    }
                });
                Vec::new()
            }
            (KeyCode::Char('M'), _) => self.handle_trigger_amend_message(),
            (KeyCode::Char('R'), _) => self.handle_trigger_rename(),
            (KeyCode::Char('c'), _) => vec![Effect::ApplyAndQuit(
                self.branches.clone(),
                self.intents.clone(),
            )],
            (KeyCode::Char('u'), _) => self.handle_trigger_converge(),
            _ => Vec::new(),
        }
    }

    fn handle_trigger_split(&mut self) -> Vec<Effect> {
        if let Some(idx) = self.list_state.selected()
            && idx < self.flattened_tree.len()
        {
            let branch_name = self.flattened_tree[idx].0.clone();
            if let Some(branch_info) = self.branches.iter().find(|b| b.name == branch_name)
                && branch_info.is_local
                && branch_info.parent_ahead == 1
                && let Some(parent) = self.get_effective_parent(&branch_name)
            {
                let parent = parent.clone();
                self.is_loading = true;
                self.progress_message = format!("Fetching diff for {}...", branch_name);
                self.progress_percentage = 0.0;
                self.mode = AppMode::Split;
                self.splitting_branch = Some(branch_name.clone());
                return vec![Effect::FetchDiff {
                    branch: branch_name,
                    parent,
                }];
            }
        }
        Vec::new()
    }

    fn handle_trigger_amend_message(&mut self) -> Vec<Effect> {
        let initial_branch = self.initial_branch.clone();
        let is_local = self
            .branches
            .iter()
            .find(|b| b.name == initial_branch)
            .map(|b| b.is_local)
            .unwrap_or(false);

        if is_local {
            self.mutate_intent(&initial_branch, |i| {
                i.pending_amend = true;
            });
            self.mode = AppMode::Prompt;
            self.prompt = Some(PromptState {
                title: "Amend Commit Message".to_string(),
                value: String::new(),
                cursor_position: 0,
                action: PromptAction::AmendMessage,
                focus: PromptFocus::Input,
            });
        }
        Vec::new()
    }

    fn handle_trigger_rename(&mut self) -> Vec<Effect> {
        if let Some(idx) = self.list_state.selected()
            && let Some((name, _)) = self.flattened_tree.get(idx)
        {
            let name = name.clone();
            if let Some(b) = self.branches.iter().find(|b| b.name == name)
                && b.is_local
                && b.upstream.is_none()
            {
                self.mode = AppMode::Prompt;
                self.prompt = Some(PromptState {
                    title: format!("Rename branch '{}' to:", name),
                    value: name.clone(),
                    cursor_position: name.len(),
                    action: PromptAction::RenameBranch,
                    focus: PromptFocus::Input,
                });
                self.splitting_branch_temp_name = Some(name); // Reuse temp field
            }
        }
        Vec::new()
    }

    fn handle_trigger_converge(&mut self) -> Vec<Effect> {
        if let Some(idx) = self.list_state.selected()
            && let Some((name, _)) = self.flattened_tree.get(idx)
        {
            let name = name.clone();
            let mut heuristic_parent = None;

            if let Some(b) = self.branches.iter().find(|b| b.name == name)
                && let Some(hp) = &b.heuristic_parent
            {
                let mut should_converge = false;
                if self.get_effective_parent(&name).as_ref() != Some(hp) {
                    should_converge = true;
                } else {
                    // Names match, check OIDs
                    let current_parent_oid =
                        self.history.oid_to_ancestor.get(&b.oid).and_then(|o| *o);
                    if let Some(h_oid) = b.heuristic_upstream_oid
                        && Some(h_oid) != current_parent_oid
                    {
                        should_converge = true;
                    }
                }

                if should_converge {
                    heuristic_parent = Some(hp.clone());
                }
            }

            if let Some(hp) = heuristic_parent {
                let current_parent = self.get_effective_parent(&name);
                if current_parent.as_ref() == Some(&hp) {
                    // Already converged, toggle back to original
                    if let Some(b) = self.branches.iter().find(|b| b.name == name) {
                        let orig = b.original_parent.clone();
                        self.try_apply_move(&name, orig);
                    }
                } else {
                    self.try_apply_move(&name, Some(hp));
                }
                self.refresh_tree(None);
                self.on_move_change();
            }
        }
        Vec::new()
    }

    pub fn handle_quit_confirmation(&mut self, key: KeyEvent) -> Vec<Effect> {
        match (key.code, key.modifiers) {
            (KeyCode::Char('y'), _) | (KeyCode::Char('q'), _) => vec![Effect::Quit],
            (KeyCode::Char('n'), _) | (KeyCode::Esc, _) => {
                self.show_quit_confirmation = false;
                Vec::new()
            }
            _ => Vec::new(),
        }
    }

    pub fn handle_quit(&mut self) -> Vec<Effect> {
        if self.show_preview {
            self.show_preview = false;
            Vec::new()
        } else {
            let snapshot = RepositorySnapshot {
                branches: self.branches.clone(),
                history: self.history.clone(),
                is_dirty: self.is_dirty,
            };
            let plan = calculate_plan(&snapshot, &self.intents).unwrap_or_default();
            if plan.is_empty() {
                vec![Effect::Quit]
            } else {
                self.show_quit_confirmation = true;
                Vec::new()
            }
        }
    }

    pub fn handle_escape(&mut self) -> Vec<Effect> {
        if self.grabbed_branch.is_some() {
            self.grabbed_branch = None;
            self.target_parent = None;
            self.refresh_tree(None);
            let effects = self.on_selection_change();
            self.on_move_change();
            effects
        } else {
            Vec::new()
        }
    }

    pub fn handle_preview_toggle(&mut self) -> Vec<Effect> {
        if !self.show_preview {
            let snapshot = RepositorySnapshot {
                branches: self.branches.clone(),
                history: self.history.clone(),
                is_dirty: self.is_dirty,
            };
            let plan = match calculate_plan(&snapshot, &self.intents) {
                Ok(p) => p,
                Err(_) => return Vec::new(), // TODO: handle error in UI
            };
            self.show_preview = true;
            self.mode = AppMode::Preview;
            self.plan = Some(plan.clone());
            if !plan.is_empty() {
                self.is_predicting_conflicts = true;
                return vec![Effect::PredictConflicts {
                    plan,
                    branches: self.branches.clone(),
                }];
            }
        }
        Vec::new()
    }

    pub fn handle_remote_toggle(&mut self) -> Vec<Effect> {
        if !self.show_preview && !self.is_loading {
            self.show_remote = !self.show_remote;
            self.is_loading = true;
            return vec![Effect::FetchBranches];
        }
        Vec::new()
    }

    pub fn handle_move_to_root(&mut self) -> Vec<Effect> {
        if self.grabbed_branch.is_some() {
            self.target_parent = None;
            if let Some(grabbed) = self.grabbed_branch.clone() {
                self.refresh_tree(Some((&grabbed, None)));
                self.on_move_change();
            }
        }
        Vec::new()
    }

    pub fn handle_space(&mut self) -> Vec<Effect> {
        if !self.show_preview
            && !self.is_dirty
            && let Some(idx) = self.list_state.selected()
            && idx < self.flattened_tree.len()
        {
            let branch_name = self.flattened_tree[idx].0.clone();
            if let Some(grabbed) = self.grabbed_branch.take() {
                self.finalize_move(grabbed);
            } else {
                self.grabbed_branch = Some(branch_name);
                self.update_target_parent();
            }
        }
        Vec::new()
    }

    pub fn handle_navigation(&mut self, delta: i32) -> Vec<Effect> {
        if self.show_preview || self.flattened_tree.is_empty() {
            return Vec::new();
        }

        let current = self.list_state.selected().unwrap_or(0) as i32;
        let len = self.flattened_tree.len() as i32;

        let mut next = ((current + delta).rem_euclid(len)) as usize;

        if let Some(grabbed) = &self.grabbed_branch {
            // Find range of grabbed branch + its descendants in flattened tree
            if let Some(start_idx) = self.flattened_tree.iter().position(|(n, _)| n == grabbed) {
                let start_depth = self.flattened_tree[start_idx].1;
                let mut end_idx = start_idx + 1;
                while end_idx < self.flattened_tree.len() {
                    if self.flattened_tree[end_idx].1 <= start_depth {
                        break;
                    }
                    end_idx += 1;
                }

                // If next lands inside the grabbed subtree, skip it
                if next >= start_idx && next < end_idx {
                    if delta > 0 {
                        // Moving down: skip to end
                        next = end_idx % self.flattened_tree.len();
                    } else {
                        // Moving up: skip to start - 1
                        if start_idx == 0 {
                            next = self.flattened_tree.len() - 1;
                        } else {
                            next = start_idx - 1;
                        }
                    }
                }
            }
        }

        self.list_state.select(Some(next));
        self.update_target_parent();
        self.on_selection_change()
    }

    pub fn toggle_pending_action<F>(&mut self, f: F)
    where
        F: FnOnce(&BranchInfo, &mut BranchIntent),
    {
        if let Some(idx) = self.list_state.selected()
            && idx < self.flattened_tree.len()
        {
            let branch_name = self.flattened_tree[idx].0.clone();
            let info = self
                .branches
                .iter()
                .find(|b| b.name == branch_name)
                .cloned();
            if let Some(info) = info {
                self.mutate_intent(&branch_name, |i| {
                    f(&info, i);
                });
                self.on_selection_change();
            }
        }
    }

    pub fn finalize_move(&mut self, grabbed: String) {
        let new_parent = self.target_parent.clone();
        self.try_apply_move(&grabbed, new_parent);
        self.target_parent = None;
        self.refresh_tree(None);
        self.on_selection_change();
        self.on_move_change();
    }

    pub fn handle_reset(&mut self) {
        if let Some(idx) = self.list_state.selected()
            && idx < self.flattened_tree.len()
        {
            let branch_name = self.flattened_tree[idx].0.clone();
            let b_info = self
                .branches
                .iter()
                .find(|b| b.name == branch_name)
                .cloned();
            if let Some(b) = b_info {
                self.mutate_intent(&branch_name, |i| {
                    i.pending_reset = !i.pending_reset;
                    if i.pending_reset {
                        if let Some(u) = b.upstream.clone() {
                            i.parent = Some(Some(u));
                        }
                    } else {
                        i.parent = Some(b.original_parent.clone());
                    }
                });
                self.refresh_tree(None);
                self.on_selection_change();
                self.on_move_change();
            }
        }
    }
}
