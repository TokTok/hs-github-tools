use crate::diff_utils::FileDiff;
use crate::engine::transaction::Transaction;
use crate::engine::{BranchInfo, BranchIntent, CommitInfo, HistoryContext, Operation};
use crate::split_state::SplitState;
use crate::topology::virtual_layer::VirtualLayer;
use crossterm::event::KeyEvent;
use ratatui::widgets::ListState;
use std::collections::HashMap;
use std::time::Instant;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SidebarState {
    Idle,
    Debouncing {
        branch: String,
        since: Instant,
    },
    Loading {
        branch: String,
    },
    Ready {
        branch: String,
        commits: Vec<CommitInfo>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConflictCheckState {
    Idle,
    Debouncing {
        branch: String,
        onto: String,
        base: Option<String>,
        since: Instant,
    },
    Checking {
        branch: String,
        onto: String,
        base: Option<String>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AppMode {
    Tree,
    Split,
    Preview,
    Prompt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PromptFocus {
    Input,
    Ok,
    Cancel,
}

pub struct PromptState {
    pub title: String,
    pub value: String,
    pub cursor_position: usize,
    pub action: PromptAction,
    pub focus: PromptFocus,
}

pub enum PromptAction {
    SplitPartName,
    SplitPartMessage,
    AmendMessage,
    RenameBranch,
}

impl PromptAction {
    pub fn is_multiline(&self) -> bool {
        matches!(
            self,
            PromptAction::SplitPartMessage | PromptAction::AmendMessage
        )
    }
}

pub struct AppState {
    pub branches: Vec<BranchInfo>,
    pub history: HistoryContext,
    pub list_state: ListState,
    pub flattened_tree: Vec<(String, usize)>,
    pub show_preview: bool,
    pub grabbed_branch: Option<String>,
    pub target_parent: Option<String>,
    pub is_loading: bool,
    pub spinner_index: usize,
    pub progress_message: String,
    pub progress_percentage: f64,
    pub show_quit_confirmation: bool,
    pub conflict_cache: HashMap<(String, String, Option<String>), bool>,
    pub is_dirty: bool,
    pub show_remote: bool,
    pub initial_branch: String,
    pub sidebar: SidebarState,
    pub conflict_check: ConflictCheckState,
    pub needs_redraw: bool,
    pub plan: Option<Vec<Operation>>,
    pub is_predicting_conflicts: bool,
    pub prediction_index: usize,
    pub split_state: Option<SplitState>,
    pub splitting_branch: Option<String>,
    pub virtual_layer: VirtualLayer,
    pub mode: AppMode,
    pub transaction: Option<Transaction>,
    pub prompt: Option<PromptState>,
    pub splitting_branch_temp_name: Option<String>,
    pub intents: HashMap<String, BranchIntent>,
    pub error_message: Option<String>,
}

impl Default for AppState {
    fn default() -> Self {
        let mut list_state = ListState::default();
        list_state.select(Some(0));
        Self {
            branches: Vec::new(),
            history: HistoryContext::new(),
            list_state,
            flattened_tree: Vec::new(),
            show_preview: false,
            grabbed_branch: None,
            target_parent: None,
            is_loading: false,
            spinner_index: 0,
            progress_message: String::new(),
            progress_percentage: 0.0,
            show_quit_confirmation: false,
            conflict_cache: HashMap::new(),
            is_dirty: false,
            show_remote: false,
            initial_branch: String::new(),
            sidebar: SidebarState::Idle,
            conflict_check: ConflictCheckState::Idle,
            needs_redraw: true,
            plan: None,
            is_predicting_conflicts: false,
            prediction_index: 0,
            split_state: None,
            splitting_branch: None,
            virtual_layer: VirtualLayer::new(),
            mode: AppMode::Tree,
            transaction: None,
            prompt: None,
            splitting_branch_temp_name: None,
            intents: HashMap::new(),
            error_message: None,
        }
    }
}

pub enum Msg {
    KeyPressed(KeyEvent),
    Tick,
    BranchesLoaded(anyhow::Result<(Vec<BranchInfo>, HistoryContext, bool)>),
    CurrentBranchLoaded(anyhow::Result<String>),
    ConflictChecked(String, String, Option<String>, anyhow::Result<bool>),
    CommitLogLoaded(String, anyhow::Result<Vec<CommitInfo>>),
    ProgressUpdated { message: String, percentage: f64 },
    ConflictsPredicted(Vec<Operation>),
    PredictionProgress { index: usize, result: Option<bool> },
    DiffLoaded(String, anyhow::Result<Vec<FileDiff>>),
}

#[derive(Debug, Clone)]
pub enum Effect {
    FetchBranches,
    FetchCurrentBranch,
    CheckConflict {
        branch: String,
        onto: String,
        base: Option<String>,
    },
    FetchCommitLog {
        branch: String,
    },
    PredictConflicts {
        plan: Vec<Operation>,
        branches: Vec<BranchInfo>,
    },
    FetchDiff {
        branch: String,
        parent: String,
    },
    Quit,
    ApplyAndQuit(Vec<BranchInfo>, HashMap<String, BranchIntent>),
}
