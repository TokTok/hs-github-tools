use crate::diff_utils::FileDiff;
use ratatui::widgets::ListState;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SplitViewMode {
    Files,
    Hunks,
    Lines,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SplitPart {
    pub name: String,
    pub commit_message: String,
    pub selected_hunks: HashSet<(String, usize)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RenderedItem {
    FileHeader {
        file_idx: usize,
    },
    HunkHeader {
        file_idx: usize,
        hunk_idx: usize,
    },
    Line {
        file_idx: usize,
        hunk_idx: usize,
        line_idx: usize,
    },
}

impl RenderedItem {
    pub fn file_idx(&self) -> usize {
        match self {
            RenderedItem::FileHeader { file_idx } => *file_idx,
            RenderedItem::HunkHeader { file_idx, .. } => *file_idx,
            RenderedItem::Line { file_idx, .. } => *file_idx,
        }
    }

    pub fn hunk_idx(&self) -> Option<usize> {
        match self {
            RenderedItem::HunkHeader { hunk_idx, .. } => Some(*hunk_idx),
            RenderedItem::Line { hunk_idx, .. } => Some(*hunk_idx),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct SplitState {
    pub files: Vec<FileDiff>,
    pub parts: Vec<SplitPart>,
    pub current_selection: HashSet<(String, usize)>,
    pub mode: SplitViewMode,
    pub list_state: ListState,
    pub rendered_items: Vec<RenderedItem>,
    pub selected_view_idx: usize,
    pub focused_file_idx: usize,
    pub focused_hunk_idx: usize,
    /// Which hunk is expanded in Lines mode.
    /// (file_idx, hunk_idx)
    pub expanded_hunk: Option<(usize, usize)>,
}

impl SplitState {
    pub fn new(files: Vec<FileDiff>) -> Self {
        let mut state = Self {
            files,
            parts: Vec::new(),
            current_selection: HashSet::new(),
            mode: SplitViewMode::Files,
            list_state: ListState::default(),
            rendered_items: Vec::new(),
            selected_view_idx: 0,
            focused_file_idx: 0,
            focused_hunk_idx: 0,
            expanded_hunk: None,
        };
        state.rebuild_view();
        state
    }

    pub fn is_hunk_taken(&self, file_path: &str, hunk_idx: usize) -> bool {
        self.parts.iter().any(|p| {
            p.selected_hunks
                .contains(&(file_path.to_string(), hunk_idx))
        })
    }

    pub fn part_for_hunk(&self, file_path: &str, hunk_idx: usize) -> Option<usize> {
        self.parts.iter().position(|p| {
            p.selected_hunks
                .contains(&(file_path.to_string(), hunk_idx))
        })
    }

    pub fn rebuild_view(&mut self) {
        // Save previously selected item to restore focus later
        let prev_selected_item = self.rendered_items.get(self.selected_view_idx).cloned();

        self.rendered_items.clear();

        for (f_idx, file) in self.files.iter().enumerate() {
            let available_hunks: Vec<usize> = file
                .hunks
                .iter()
                .enumerate()
                .filter(|(h_idx, _)| !self.is_hunk_taken(&file.path, *h_idx))
                .map(|(h_idx, _)| h_idx)
                .collect();

            // Only show file header if it has available hunks or if it's empty (no hunks at all)
            if available_hunks.is_empty() && !file.hunks.is_empty() {
                continue;
            }

            self.rendered_items
                .push(RenderedItem::FileHeader { file_idx: f_idx });

            let is_file_expanded = match self.mode {
                SplitViewMode::Files => false,
                _ => f_idx == self.focused_file_idx,
            };

            if is_file_expanded {
                for h_idx in available_hunks {
                    self.rendered_items.push(RenderedItem::HunkHeader {
                        file_idx: f_idx,
                        hunk_idx: h_idx,
                    });

                    if self.mode == SplitViewMode::Lines
                        && self.expanded_hunk == Some((f_idx, h_idx))
                    {
                        for l_idx in 0..file.hunks[h_idx].lines.len() {
                            self.rendered_items.push(RenderedItem::Line {
                                file_idx: f_idx,
                                hunk_idx: h_idx,
                                line_idx: l_idx,
                            });
                        }
                    }
                }
            }
        }

        // Restore selection or find something sensible
        if let Some(prev) = prev_selected_item {
            if let Some(new_idx) = self.rendered_items.iter().position(|item| item == &prev) {
                self.selected_view_idx = new_idx;
            } else {
                // Try to find a logical fallback
                let fallback = match prev {
                    RenderedItem::Line {
                        file_idx, hunk_idx, ..
                    } => Some(RenderedItem::HunkHeader { file_idx, hunk_idx }),
                    RenderedItem::HunkHeader { file_idx, .. }
                        if self.mode == SplitViewMode::Files =>
                    {
                        Some(RenderedItem::FileHeader { file_idx })
                    }
                    _ => None,
                };

                let mut found_fallback = false;
                if let Some(f) = fallback {
                    if let Some(new_idx) = self.rendered_items.iter().position(|item| item == &f) {
                        self.selected_view_idx = new_idx;
                        found_fallback = true;
                    } else {
                        // If hunk header not found (e.g. hunk taken), try file header
                        if let RenderedItem::HunkHeader { file_idx, .. } = f {
                            let f2 = RenderedItem::FileHeader { file_idx };
                            if let Some(new_idx) =
                                self.rendered_items.iter().position(|item| item == &f2)
                            {
                                self.selected_view_idx = new_idx;
                                found_fallback = true;
                            }
                        }
                    }
                }

                if !found_fallback {
                    self.selected_view_idx = self
                        .selected_view_idx
                        .min(self.rendered_items.len().saturating_sub(1));
                }
            }
        } else {
            self.selected_view_idx = self
                .selected_view_idx
                .min(self.rendered_items.len().saturating_sub(1));
        }

        // Update focused trackers based on selection
        if let Some(item) = self.rendered_items.get(self.selected_view_idx) {
            match item {
                RenderedItem::FileHeader { file_idx } => {
                    self.focused_file_idx = *file_idx;
                }
                RenderedItem::HunkHeader { file_idx, hunk_idx } => {
                    self.focused_file_idx = *file_idx;
                    self.focused_hunk_idx = *hunk_idx;
                }
                RenderedItem::Line {
                    file_idx, hunk_idx, ..
                } => {
                    self.focused_file_idx = *file_idx;
                    self.focused_hunk_idx = *hunk_idx;
                }
            }
        }

        self.list_state.select(Some(self.selected_view_idx));
    }

    fn can_navigate(&self, from: &RenderedItem, to: &RenderedItem) -> bool {
        if self.mode == SplitViewMode::Files {
            return true;
        }

        if from.file_idx() != to.file_idx() {
            return false;
        }

        if matches!(to, RenderedItem::FileHeader { .. }) {
            return false;
        }

        if self.mode == SplitViewMode::Lines && from.hunk_idx() != to.hunk_idx() {
            return false;
        }
        true
    }

    pub fn next(&mut self) {
        if self.rendered_items.is_empty() {
            return;
        }
        if self.selected_view_idx + 1 < self.rendered_items.len() {
            let current = &self.rendered_items[self.selected_view_idx];
            let next = &self.rendered_items[self.selected_view_idx + 1];

            if self.can_navigate(current, next) {
                self.selected_view_idx += 1;
                self.rebuild_view();
            }
        }
    }

    pub fn prev(&mut self) {
        if self.rendered_items.is_empty() {
            return;
        }
        if self.selected_view_idx > 0 {
            let current = &self.rendered_items[self.selected_view_idx];
            let prev = &self.rendered_items[self.selected_view_idx - 1];

            if self.can_navigate(current, prev) {
                self.selected_view_idx -= 1;
                self.rebuild_view();
            }
        }
    }

    pub fn page_down(&mut self, height: usize) {
        for _ in 0..height {
            if self.rendered_items.is_empty() {
                break;
            }
            if self.selected_view_idx + 1 < self.rendered_items.len() {
                let current = &self.rendered_items[self.selected_view_idx];
                let next = &self.rendered_items[self.selected_view_idx + 1];

                if self.can_navigate(current, next) {
                    self.selected_view_idx += 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        self.rebuild_view();
    }

    pub fn page_up(&mut self, height: usize) {
        for _ in 0..height {
            if self.rendered_items.is_empty() {
                break;
            }
            if self.selected_view_idx > 0 {
                let current = &self.rendered_items[self.selected_view_idx];
                let prev = &self.rendered_items[self.selected_view_idx - 1];

                if self.can_navigate(current, prev) {
                    self.selected_view_idx -= 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        self.rebuild_view();
    }

    pub fn enter_hunks(&mut self) {
        if self.mode == SplitViewMode::Files
            && let Some(RenderedItem::FileHeader { file_idx }) =
                self.rendered_items.get(self.selected_view_idx)
            && !self.files[*file_idx].hunks.is_empty()
        {
            self.mode = SplitViewMode::Hunks;
            self.focused_file_idx = *file_idx;
            self.rebuild_view();
            // Move to the first hunk of this file
            if let Some(pos) = self.rendered_items.iter().position(|item| match item {
                RenderedItem::HunkHeader { file_idx: fi, .. } => *fi == self.focused_file_idx,
                _ => false,
            }) {
                self.selected_view_idx = pos;
                self.rebuild_view();
            }
            // When entering hunks, scroll so the file header is at the top
            *self.list_state.offset_mut() = self.selected_view_idx.saturating_sub(1);
        }
    }

    pub fn exit_hunks(&mut self) {
        if self.mode == SplitViewMode::Hunks {
            let target_file_idx = self.focused_file_idx;
            self.mode = SplitViewMode::Files;
            self.rebuild_view();
            // Move back to file header
            if let Some(pos) = self.rendered_items.iter().position(|item| match item {
                RenderedItem::FileHeader { file_idx } => *file_idx == target_file_idx,
                _ => false,
            }) {
                self.selected_view_idx = pos;
                self.rebuild_view();
            }
        }
    }

    pub fn enter_lines(&mut self) {
        if self.mode == SplitViewMode::Hunks
            && let Some(RenderedItem::HunkHeader { file_idx, hunk_idx }) =
                self.rendered_items.get(self.selected_view_idx)
        {
            self.mode = SplitViewMode::Lines;
            self.expanded_hunk = Some((*file_idx, *hunk_idx));
            self.rebuild_view();
            // Move to first line
            if let Some(pos) = self.rendered_items.iter().position(|item| match item {
                RenderedItem::Line {
                    file_idx: fi,
                    hunk_idx: hi,
                    ..
                } => *fi == self.focused_file_idx && *hi == self.focused_hunk_idx,
                _ => false,
            }) {
                self.selected_view_idx = pos;
                self.rebuild_view();
            }
            // Try to keep the file header visible
            *self.list_state.offset_mut() = self
                .rendered_items
                .iter()
                .position(|item| match item {
                    RenderedItem::FileHeader { file_idx } => *file_idx == self.focused_file_idx,
                    _ => false,
                })
                .unwrap_or(0);
        }
    }

    pub fn exit_lines(&mut self) {
        if self.mode == SplitViewMode::Lines {
            let target_file_idx = self.focused_file_idx;
            let target_hunk_idx = self.focused_hunk_idx;
            self.mode = SplitViewMode::Hunks;
            self.expanded_hunk = None;
            self.rebuild_view();
            // Move back to hunk header
            if let Some(pos) = self.rendered_items.iter().position(|item| match item {
                RenderedItem::HunkHeader {
                    file_idx: fi,
                    hunk_idx: hi,
                } => *fi == target_file_idx && *hi == target_hunk_idx,
                _ => false,
            }) {
                self.selected_view_idx = pos;
                self.rebuild_view();
            }
        }
    }

    pub fn toggle_selection(&mut self) {
        let item = match self.rendered_items.get(self.selected_view_idx) {
            Some(i) => i.clone(),
            None => return,
        };

        match item {
            RenderedItem::FileHeader { file_idx } => {
                let file = &self.files[file_idx];
                let available_hunks: Vec<usize> = file
                    .hunks
                    .iter()
                    .enumerate()
                    .filter(|(i, _)| !self.is_hunk_taken(&file.path, *i))
                    .map(|(i, _)| i)
                    .collect();

                if available_hunks.is_empty() {
                    if file.hunks.is_empty() && !self.is_hunk_taken(&file.path, 0) {
                        let key = (file.path.clone(), 0);
                        if self.current_selection.contains(&key) {
                            self.current_selection.remove(&key);
                        } else {
                            self.current_selection.insert(key);
                        }
                    }
                    return;
                }

                let all_selected = available_hunks
                    .iter()
                    .all(|&i| self.current_selection.contains(&(file.path.clone(), i)));

                if all_selected {
                    for &i in &available_hunks {
                        self.current_selection.remove(&(file.path.clone(), i));
                    }
                } else {
                    for &i in &available_hunks {
                        self.current_selection.insert((file.path.clone(), i));
                    }
                }
            }
            RenderedItem::HunkHeader { file_idx, hunk_idx }
            | RenderedItem::Line {
                file_idx, hunk_idx, ..
            } => {
                let file_path = &self.files[file_idx].path;
                let key = (file_path.clone(), hunk_idx);
                if self.current_selection.contains(&key) {
                    self.current_selection.remove(&key);
                } else {
                    self.current_selection.insert(key);
                }
            }
        }
        self.rebuild_view();
    }

    pub fn get_focused_file_idx(&self) -> Option<usize> {
        Some(self.focused_file_idx)
    }

    pub fn get_focused_hunk_idx(&self) -> Option<usize> {
        match self.rendered_items.get(self.selected_view_idx) {
            Some(RenderedItem::HunkHeader { hunk_idx, .. }) => Some(*hunk_idx),
            Some(RenderedItem::Line { hunk_idx, .. }) => Some(*hunk_idx),
            _ => None,
        }
    }

    pub fn get_focused_line_idx(&self) -> Option<usize> {
        match self.rendered_items.get(self.selected_view_idx) {
            Some(RenderedItem::Line { line_idx, .. }) => Some(*line_idx),
            _ => None,
        }
    }
}
