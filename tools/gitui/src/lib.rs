use crossterm::{
    event::{DisableMouseCapture, EnableMouseCapture},
    execute,
    style::Stylize,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use ratatui::{Terminal, backend::CrosstermBackend};
use std::io;
use std::path::Path;
use tokio::sync::mpsc;

pub mod diff_utils;
pub mod engine;
pub mod patch_utils;
pub mod runtime;
pub mod split_state;
pub mod state;
pub mod testing;
pub mod topology;
pub mod ui;

use engine::{
    BranchInfo, CommitInfo, ConflictMode, Executor, Git, HistoryContext, Operation, RealGit,
    RebaseConflict, ShellExecutor, calculate_plan, flatten_branches,
};
use serde::Serialize;

#[derive(Debug, Clone, Copy, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanIntent {
    Submit,
    Sync,
    Converge,
}

#[derive(Debug, Clone, Serialize)]
pub struct Plan {
    pub intent: PlanIntent,
    pub branch: String,
    pub ops: Vec<Operation>,
    /// Branches `build_cascade_plan` could not anchor — no detectable parent.
    /// Empty for other plan builders.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub skipped_branches: Vec<String>,
}

#[derive(Debug, Clone, Serialize, serde::Deserialize)]
#[serde(tag = "status", rename_all = "snake_case")]
pub enum OpResult {
    Ok {
        branch: String,
    },
    Conflict {
        branch: String,
        conflicted_paths: Vec<String>,
        rebase_in_progress: bool,
    },
    Error {
        branch: String,
        message: String,
    },
    /// Not rebased, because the rebase of the branch it sits on was aborted.
    Blocked {
        branch: String,
        parent: String,
    },
}

impl OpResult {
    pub fn branch(&self) -> &str {
        match self {
            OpResult::Ok { branch }
            | OpResult::Conflict { branch, .. }
            | OpResult::Error { branch, .. }
            | OpResult::Blocked { branch, .. } => branch,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RepoState {
    Clean,
    Rebase,
    RebaseInteractive,
    RebaseMerge,
    ApplyMailbox,
    ApplyMailboxOrRebase,
    Merge,
    Revert,
    CherryPick,
    Bisect,
    Other,
}

#[derive(Debug, Clone, Serialize)]
pub struct RepoStatus {
    pub state: RepoState,
    pub conflicted_paths: Vec<String>,
    pub is_dirty: bool,
    pub current_branch: Option<String>,
}

/// Path to the `git` binary. Defaults to the PATH-resolved `git`; set
/// `GITUI_GIT_BIN` (e.g. via bazel `env = {"GITUI_GIT_BIN": "$(rootpath @git)"}`)
/// to point at a hermetic copy when running in a sandboxed environment.
pub fn git_bin() -> String {
    std::env::var("GITUI_GIT_BIN").unwrap_or_else(|_| "git".to_string())
}

pub fn print_tree<P: AsRef<Path>>(path: P, show_remote: bool) -> anyhow::Result<()> {
    print_tree_to(path, show_remote, &mut std::io::stdout())
}

pub fn print_tree_to<P: AsRef<Path>, W: std::io::Write>(
    path: P,
    show_remote: bool,
    writer: &mut W,
) -> anyhow::Result<()> {
    let git = RealGit::new(path.as_ref())?;
    let (branches, history) = git.get_branches(None)?;
    let intents = std::collections::HashMap::<String, crate::engine::BranchIntent>::new();
    let flattened = flatten_branches(&branches, &intents, &history, show_remote)?;
    let is_workspace_dirty = git.is_dirty().unwrap_or(false);
    let current_branch = git.get_current_branch().unwrap_or_default();

    if is_workspace_dirty {
        writeln!(
            writer,
            "{}",
            "Workspace is DIRTY - Read Only Mode".red().bold()
        )?;
    }

    let max_name_len = flattened
        .iter()
        .map(|(name, depth)| {
            let base_name_len = name.len();
            // A static print has no pending intents.
            base_name_len + (*depth * 2)
        })
        .max()
        .unwrap_or(0);

    for (name, depth) in flattened {
        let indent = "  ".repeat(depth);
        let Some(branch_info) = branches.iter().find(|b| b.name == name) else {
            continue;
        };

        let is_current = name == current_branch;
        let prefix = if is_current { "* " } else { "  " };

        let display_name_str = format!("{}{}{}", prefix, indent, name);
        let padding_len = max_name_len.saturating_sub(display_name_str.len()) + 2;
        let padding = " ".repeat(padding_len);

        let mut styled_name = display_name_str.stylize();

        if branch_info.is_remote {
            styled_name = styled_name.dark_grey();
        }

        if branch_info.ahead > 0 || branch_info.behind > 0 {
            styled_name = styled_name.yellow();
        }

        if is_current {
            styled_name = styled_name.bold();
        }

        write!(writer, "{}", styled_name)?;

        if !branch_info.aliases.is_empty() {
            write!(
                writer,
                "{}{}",
                padding,
                branch_info.aliases.join(", ").dark_grey()
            )?;
        }

        if branch_info.ahead > 0 || branch_info.behind > 0 {
            write!(
                writer,
                " {}",
                format!("({}↑ {}↓)", branch_info.ahead, branch_info.behind).yellow()
            )?;
        }

        if branch_info.can_submit() {
            write!(writer, " {}", "[READY]".green().bold())?;
        }

        if branch_info.is_merged {
            write!(writer, " {}", "[MERGED]".green())?;
        }

        if (branch_info.heuristic_parent.is_some()
            && branch_info.heuristic_parent.as_ref() != branch_info.original_parent.as_ref())
            || branch_info.parent_behind > 0
        {
            write!(writer, " {}", "[DIVERGED]".red().bold())?;
        }

        if let Some(pr) = &branch_info.pr {
            let label = format!("#{}", pr.number);
            let styled = if pr.is_draft {
                format!("{} (draft)", label).dark_grey()
            } else {
                label.cyan()
            };
            write!(writer, " {}", styled)?;
        }

        writeln!(writer)?;
    }
    Ok(())
}

pub fn build_submit_plan<P: AsRef<Path>>(path: P, branch_name: &str) -> anyhow::Result<Plan> {
    let git = RealGit::new(path.as_ref())?;
    let (mut branches, history) = git.get_branches(None)?;
    let is_dirty = git.is_dirty().unwrap_or(false);

    let branch = branches
        .iter_mut()
        .find(|b| b.name == branch_name)
        .ok_or_else(|| anyhow::anyhow!("Branch not found: {}", branch_name))?;

    if !branch.can_submit() {
        anyhow::bail!("Branch is not ready to submit: {}", branch_name);
    }

    let mut intents = std::collections::HashMap::new();
    intents.insert(
        branch_name.to_string(),
        crate::engine::BranchIntent {
            pending_submit: true,
            ..Default::default()
        },
    );

    let snapshot = crate::engine::RepositorySnapshot {
        branches,
        history,
        is_dirty,
    };

    let mut ops = calculate_plan(&snapshot, &intents)?;
    engine::predict_conflicts(&mut ops, &git, &snapshot.branches, None);

    Ok(Plan {
        intent: PlanIntent::Submit,
        branch: branch_name.to_string(),
        ops,
        skipped_branches: Vec::new(),
    })
}

pub fn print_submit_plan<P: AsRef<Path>>(path: P, branch_name: &str) -> anyhow::Result<()> {
    print_submit_plan_to(path, branch_name, &mut std::io::stdout())
}

pub fn print_submit_plan_to<P: AsRef<Path>, W: std::io::Write>(
    path: P,
    branch_name: &str,
    writer: &mut W,
) -> anyhow::Result<()> {
    let plan = build_submit_plan(path, branch_name)?;
    print_plan_to(&plan, writer)
}

pub fn build_sync_plan<P: AsRef<Path>>(path: P, branch_name: &str) -> anyhow::Result<Plan> {
    let git = RealGit::new(path.as_ref())?;
    let (branches, history) = git.get_branches(None)?;
    let is_dirty = git.is_dirty().unwrap_or(false);

    if !branches.iter().any(|b| b.name == branch_name) {
        anyhow::bail!("Branch not found: {}", branch_name);
    }

    let mut intents = std::collections::HashMap::new();
    intents.insert(
        branch_name.to_string(),
        crate::engine::BranchIntent {
            pending_reset: true,
            ..Default::default()
        },
    );

    let snapshot = crate::engine::RepositorySnapshot {
        branches,
        history,
        is_dirty,
    };

    let mut ops = calculate_plan(&snapshot, &intents)?;
    engine::predict_conflicts(&mut ops, &git, &snapshot.branches, None);

    Ok(Plan {
        intent: PlanIntent::Sync,
        branch: branch_name.to_string(),
        ops,
        skipped_branches: Vec::new(),
    })
}

/// `build_sync_plan` plus cascade-resume: overrides the topology's parent
/// for branches the planner would otherwise drop, so re-running after a
/// halted cascade picks up the residual work. Used by `--apply --sync`.
/// True for two branches whose tips have the same summary.
fn follows_heuristic_parents_to_itself(branches: &[engine::BranchInfo], name: &str) -> bool {
    let parent_of = |name: &str| {
        branches
            .iter()
            .find(|b| b.name == name)
            .and_then(|b| b.heuristic_parent.as_deref())
    };
    let mut seen = std::collections::HashSet::new();
    let mut current = parent_of(name);
    while let Some(parent) = current {
        if parent == name {
            return true;
        }
        if !seen.insert(parent) {
            return false;
        }
        current = parent_of(parent);
    }
    false
}

pub fn build_cascade_plan<P: AsRef<Path>>(path: P, branch_name: &str) -> anyhow::Result<Plan> {
    let git = RealGit::new(path.as_ref())?;
    let (branches, history) = git.get_branches(None)?;
    let is_dirty = git.is_dirty().unwrap_or(false);

    if !branches.iter().any(|b| b.name == branch_name) {
        anyhow::bail!("Branch not found: {}", branch_name);
    }

    let mut intents = std::collections::HashMap::new();
    intents.insert(
        branch_name.to_string(),
        crate::engine::BranchIntent {
            pending_reset: true,
            ..Default::default()
        },
    );

    // Override the topology's parent where the planner would otherwise drop
    // the branch: no parent found (use the heuristic parent, or report it
    // skipped), or a parent whose tip is an ancestor of the sync target (the
    // target moved past it, so "the parent hasn't moved" and no op). Mutate
    // original_parent so get_rebase_upstream sees the new parent.
    let target_oid = branches
        .iter()
        .find(|b| b.name == branch_name)
        .map(|b| b.oid);
    let name_to_oid: std::collections::HashMap<String, git2::Oid> =
        branches.iter().map(|b| (b.name.clone(), b.oid)).collect();
    let local_names: std::collections::HashSet<String> = branches
        .iter()
        .filter(|b| !b.is_remote)
        .map(|b| b.name.clone())
        .collect();
    let in_cycle: std::collections::HashSet<String> = branches
        .iter()
        .filter(|b| follows_heuristic_parents_to_itself(&branches, &b.name))
        .map(|b| b.name.clone())
        .collect();
    let mut branches = branches;
    let mut skipped_branches = Vec::new();
    for branch in branches.iter_mut() {
        if branch.is_remote || branch.name == branch_name {
            continue;
        }
        if in_cycle.contains(&branch.name) {
            branch.heuristic_parent = None;
            branch.heuristic_upstream_oid = None;
        }

        let parent_is_ancestor_of_target = match (&branch.original_parent, target_oid) {
            (Some(parent_name), Some(t_oid)) if parent_name != branch_name => name_to_oid
                .get(parent_name)
                .map(|&p_oid| p_oid != t_oid && git.is_descendant(p_oid, t_oid).unwrap_or(false))
                .unwrap_or(false),
            _ => false,
        };

        if parent_is_ancestor_of_target {
            // A parent found in the branch's history that hasn't landed in
            // the target is a rewritten parent: the branch stays on it.
            let rewritten_parent = branch.heuristic_parent.clone().filter(|h_parent| {
                h_parent != branch_name
                    && local_names.contains(h_parent)
                    && match (name_to_oid.get(h_parent), target_oid) {
                        (Some(&h_oid), Some(t_oid)) => {
                            h_oid != t_oid && !git.is_descendant(h_oid, t_oid).unwrap_or(false)
                        }
                        _ => false,
                    }
            });
            if let Some(h_parent) = rewritten_parent {
                crate::engine::topology::apply_move(&mut intents, &branch.name, Some(h_parent))?;
                continue;
            }
            branch.original_parent = Some(branch_name.to_string());
            // Clear heuristic upstream oid so get_rebase_upstream falls
            // back through to the corrected original_parent.
            branch.heuristic_upstream_oid = None;
            crate::engine::topology::apply_move(
                &mut intents,
                &branch.name,
                Some(branch_name.to_string()),
            )?;
        } else if branch.original_parent.is_none() {
            if let Some(h_parent) = branch.heuristic_parent.clone() {
                crate::engine::topology::apply_move(&mut intents, &branch.name, Some(h_parent))?;
            } else {
                skipped_branches.push(branch.name.clone());
            }
        }
    }

    let snapshot = crate::engine::RepositorySnapshot {
        branches,
        history,
        is_dirty,
    };

    let mut ops = calculate_plan(&snapshot, &intents)?;
    engine::predict_conflicts(&mut ops, &git, &snapshot.branches, None);

    Ok(Plan {
        intent: PlanIntent::Sync,
        branch: branch_name.to_string(),
        ops,
        skipped_branches,
    })
}

pub fn print_sync_plan<P: AsRef<Path>>(path: P, branch_name: &str) -> anyhow::Result<()> {
    print_sync_plan_to(path, branch_name, &mut std::io::stdout())
}

pub fn print_sync_plan_to<P: AsRef<Path>, W: std::io::Write>(
    path: P,
    branch_name: &str,
    writer: &mut W,
) -> anyhow::Result<()> {
    let plan = build_sync_plan(path, branch_name)?;
    print_plan_to(&plan, writer)
}

pub fn build_converge_plan<P: AsRef<Path>>(path: P, branch_name: &str) -> anyhow::Result<Plan> {
    let git = RealGit::new(path.as_ref())?;
    let (branches, history) = git.get_branches(None)?;
    let is_dirty = git.is_dirty().unwrap_or(false);

    let branch = branches
        .iter()
        .find(|b| b.name == branch_name)
        .ok_or_else(|| anyhow::anyhow!("Branch not found: {}", branch_name))?;

    let h_parent = branch.heuristic_parent.clone().ok_or_else(|| {
        anyhow::anyhow!("No heuristic parent detected for branch: {}", branch_name)
    })?;

    let mut intents = std::collections::HashMap::new();
    crate::engine::topology::apply_move(&mut intents, branch_name, Some(h_parent))?;

    let snapshot = crate::engine::RepositorySnapshot {
        branches,
        history,
        is_dirty,
    };

    let mut ops = calculate_plan(&snapshot, &intents)?;
    engine::predict_conflicts(&mut ops, &git, &snapshot.branches, None);

    Ok(Plan {
        intent: PlanIntent::Converge,
        branch: branch_name.to_string(),
        ops,
        skipped_branches: Vec::new(),
    })
}

pub fn print_converge_plan<P: AsRef<Path>>(path: P, branch_name: &str) -> anyhow::Result<()> {
    print_converge_plan_to(path, branch_name, &mut std::io::stdout())
}

pub fn print_converge_plan_to<P: AsRef<Path>, W: std::io::Write>(
    path: P,
    branch_name: &str,
    writer: &mut W,
) -> anyhow::Result<()> {
    let plan = build_converge_plan(path, branch_name)?;
    print_plan_to(&plan, writer)
}

pub fn print_plan_to<W: std::io::Write>(plan: &Plan, writer: &mut W) -> anyhow::Result<()> {
    if plan.ops.is_empty() {
        writeln!(writer, "No operations to perform.")?;
        return Ok(());
    }
    let verb = match plan.intent {
        PlanIntent::Submit => "submit",
        PlanIntent::Sync => "sync",
        PlanIntent::Converge => "converge",
    };
    writeln!(writer, "Plan to {} {}:", verb, plan.branch)?;
    for op in &plan.ops {
        let label = match op {
            Operation::Rebase {
                predicted_conflict: Some(true),
                ..
            } => " [CONFLICT]",
            Operation::Sync {
                predicted_conflict: Some(true),
                ..
            } => " [NOT FF]",
            Operation::Rebase {
                predicted_conflict: Some(false),
                ..
            } => " [CLEAN]",
            Operation::Sync {
                predicted_conflict: Some(false),
                ..
            } => " [FF]",
            _ => "",
        };
        writeln!(writer, "  {}{}", op, label)?;
    }
    Ok(())
}

/// Runs one operation. A conflicting rebase is left in the working tree for the user.
pub fn execute_op<P: AsRef<Path>>(path: P, op: &Operation) -> anyhow::Result<OpResult> {
    execute_op_with(path, op, ConflictMode::Leave)
}

pub fn execute_op_with<P: AsRef<Path>>(
    path: P,
    op: &Operation,
    on_conflict: ConflictMode,
) -> anyhow::Result<OpResult> {
    let git = RealGit::new(path.as_ref())?;
    let exec = ShellExecutor::new(on_conflict);
    match exec.execute(&git, op, path.as_ref()) {
        Ok(_) => Ok(OpResult::Ok {
            branch: op.target_branch().to_string(),
        }),
        Err(e) => {
            if let Some(conflict) = e.downcast_ref::<RebaseConflict>() {
                return Ok(OpResult::Conflict {
                    branch: conflict.branch.clone(),
                    conflicted_paths: conflict.paths.clone(),
                    rebase_in_progress: false,
                });
            }
            let status = repo_status(path.as_ref())?;
            let in_progress = !matches!(status.state, RepoState::Clean);
            if !status.conflicted_paths.is_empty() || in_progress {
                Ok(OpResult::Conflict {
                    branch: op.target_branch().to_string(),
                    conflicted_paths: status.conflicted_paths,
                    rebase_in_progress: matches!(
                        status.state,
                        RepoState::Rebase | RepoState::RebaseInteractive | RepoState::RebaseMerge
                    ),
                })
            } else {
                Ok(OpResult::Error {
                    branch: op.target_branch().to_string(),
                    message: e.to_string(),
                })
            }
        }
    }
}

const SYNC_ORIGIN_KEY: &str = "gitui.sync-origin";

fn capture_initial_branch(path: &Path, current: Option<&str>) -> Option<String> {
    let repo = git2::Repository::open(path).ok()?;
    let mut cfg = repo.config().ok()?;
    if let Ok(saved) = cfg.get_string(SYNC_ORIGIN_KEY) {
        return Some(saved);
    }
    let branch = current?;
    if repo.find_branch(branch, git2::BranchType::Local).is_err() {
        return None;
    }
    let _ = cfg.set_str(SYNC_ORIGIN_KEY, branch);
    Some(branch.to_string())
}

fn finalize_initial_branch(path: &Path, branch: &str) {
    if let Ok(repo) = git2::Repository::open(path) {
        if repo.find_branch(branch, git2::BranchType::Local).is_ok() {
            let already_there = repo
                .head()
                .ok()
                .and_then(|h| h.shorthand().map(str::to_owned))
                .as_deref()
                == Some(branch);
            if !already_there {
                let _ = std::process::Command::new("git")
                    .arg("-C")
                    .arg(path)
                    .args(["checkout", "-q", branch])
                    .status();
            }
        }
        if let Ok(mut cfg) = repo.config() {
            let _ = cfg.remove(SYNC_ORIGIN_KEY);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OnConflict {
    /// Stop in the conflict, for the user to resolve it and apply the plan again.
    Stop,
    /// Leave the conflicting branch alone and go on with the branches that
    /// don't sit on it.
    Abort,
}

pub fn apply_plan<P: AsRef<Path>>(path: P, plan: &Plan) -> anyhow::Result<Vec<OpResult>> {
    apply_plan_with(path, plan, OnConflict::Stop)
}

pub fn apply_plan_with<P: AsRef<Path>>(
    path: P,
    plan: &Plan,
    on_conflict: OnConflict,
) -> anyhow::Result<Vec<OpResult>> {
    let path = path.as_ref();
    let pre_status = repo_status(path)?;
    if !matches!(pre_status.state, RepoState::Clean) {
        let branch = pre_status
            .current_branch
            .unwrap_or_else(|| "(detached)".to_string());
        return Ok(vec![OpResult::Error {
            branch,
            message: format!(
                "refusing to apply plan: repository is in {:?} state; \
                 first resolve or abort the in-progress operation",
                pre_status.state
            ),
        }]);
    }

    let initial_branch = capture_initial_branch(path, pre_status.current_branch.as_deref());

    let mut results = Vec::with_capacity(plan.ops.len());
    let mut blocked = std::collections::HashSet::new();
    let mut finished = true;
    for op in &plan.ops {
        if let Operation::Rebase { branch, onto, .. } = op
            && blocked.contains(onto)
        {
            blocked.insert(branch.clone());
            results.push(OpResult::Blocked {
                branch: branch.clone(),
                parent: onto.clone(),
            });
            continue;
        }

        let mode = match on_conflict {
            OnConflict::Stop => ConflictMode::Leave,
            OnConflict::Abort => ConflictMode::Report,
        };
        let res = execute_op_with(path, op, mode)?;
        if let OpResult::Conflict { branch, .. } = &res
            && on_conflict == OnConflict::Abort
        {
            blocked.insert(branch.clone());
        }
        let go_on = match &res {
            OpResult::Ok { .. } => true,
            OpResult::Conflict { branch, .. } => blocked.contains(branch),
            _ => false,
        };
        results.push(res);
        if !go_on {
            finished = false;
            break;
        }
    }

    if finished && let Some(branch) = initial_branch.as_deref() {
        finalize_initial_branch(path, branch);
    }

    Ok(results)
}

pub fn repo_status<P: AsRef<Path>>(path: P) -> anyhow::Result<RepoStatus> {
    let repo = git2::Repository::open(path.as_ref())?;
    let state = match repo.state() {
        git2::RepositoryState::Clean => RepoState::Clean,
        git2::RepositoryState::Merge => RepoState::Merge,
        git2::RepositoryState::Revert | git2::RepositoryState::RevertSequence => RepoState::Revert,
        git2::RepositoryState::CherryPick | git2::RepositoryState::CherryPickSequence => {
            RepoState::CherryPick
        }
        git2::RepositoryState::Bisect => RepoState::Bisect,
        git2::RepositoryState::Rebase => RepoState::Rebase,
        git2::RepositoryState::RebaseInteractive => RepoState::RebaseInteractive,
        git2::RepositoryState::RebaseMerge => RepoState::RebaseMerge,
        git2::RepositoryState::ApplyMailbox => RepoState::ApplyMailbox,
        git2::RepositoryState::ApplyMailboxOrRebase => RepoState::ApplyMailboxOrRebase,
    };

    let mut opts = git2::StatusOptions::new();
    opts.include_untracked(false);
    opts.include_ignored(false);
    let statuses = repo.statuses(Some(&mut opts))?;
    let mut conflicted_paths = Vec::new();
    let mut is_dirty = false;
    for entry in statuses.iter() {
        let s = entry.status();
        if s.contains(git2::Status::CONFLICTED)
            && let Some(p) = entry.path()
        {
            conflicted_paths.push(p.to_string());
        }
        if s.intersects(
            git2::Status::INDEX_NEW
                | git2::Status::INDEX_MODIFIED
                | git2::Status::INDEX_DELETED
                | git2::Status::INDEX_RENAMED
                | git2::Status::INDEX_TYPECHANGE
                | git2::Status::WT_MODIFIED
                | git2::Status::WT_DELETED
                | git2::Status::WT_RENAMED
                | git2::Status::WT_TYPECHANGE
                | git2::Status::CONFLICTED,
        ) {
            is_dirty = true;
        }
    }

    let current_branch = repo
        .head()
        .ok()
        .and_then(|h| h.shorthand().map(|s| s.to_string()));

    Ok(RepoStatus {
        state,
        conflicted_paths,
        is_dirty,
        current_branch,
    })
}

pub enum GitCommand {
    GetBranches,
    GetCurrentBranch,
    CheckConflict {
        branch: String,
        onto: String,
        base: Option<String>,
    },
    GetCommitLog {
        branch: String,
    },
    FetchDiff {
        branch: String,
        parent: String,
    },
    PredictConflicts {
        plan: Vec<Operation>,
        branches: Vec<BranchInfo>,
    },
}

pub enum GitResponse {
    Branches(anyhow::Result<(Vec<BranchInfo>, HistoryContext, bool)>),
    CurrentBranch(anyhow::Result<String>),
    Progress { message: String, percentage: f64 },
    ConflictCheck(String, String, Option<String>, anyhow::Result<bool>),
    CommitLog(String, anyhow::Result<Vec<CommitInfo>>),
    DiffLoaded(String, anyhow::Result<Vec<diff_utils::FileDiff>>),
    ConflictsPredicted(Vec<Operation>),
    PredictionProgress { index: usize, result: Option<bool> },
}

pub struct TerminalGuard {
    pub terminal: Terminal<CrosstermBackend<io::Stdout>>,
}

impl TerminalGuard {
    pub fn new() -> anyhow::Result<Self> {
        enable_raw_mode()?;
        let mut stdout = io::stdout();
        execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
        let backend = CrosstermBackend::new(stdout);
        let terminal = Terminal::new(backend)?;
        Ok(Self { terminal })
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        let _ = disable_raw_mode();
        let _ = execute!(
            self.terminal.backend_mut(),
            LeaveAlternateScreen,
            DisableMouseCapture
        );
        let _ = self.terminal.show_cursor();
    }
}

pub async fn run<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    runtime::run_runtime(path.as_ref()).await
}

fn spawn_worker(
    path: std::path::PathBuf,
    mut cmd_rx: mpsc::Receiver<GitCommand>,
    resp_tx: mpsc::Sender<GitResponse>,
) -> std::thread::JoinHandle<()> {
    std::thread::spawn(move || {
        let git = match RealGit::new(&path) {
            Ok(g) => g,
            Err(_) => return,
        };

        while let Some(cmd) = cmd_rx.blocking_recv() {
            match cmd {
                GitCommand::GetBranches => {
                    let progress = |message: String, percentage: f64| {
                        let _ = resp_tx.blocking_send(GitResponse::Progress {
                            message,
                            percentage,
                        });
                    };
                    let branches_res = git.get_branches(Some(&progress));
                    progress("Checking for dirty worktree...".to_string(), 0.95);
                    let dirty_res = git.is_dirty();

                    let res = match (branches_res, dirty_res) {
                        (Ok((b, h)), Ok(d)) => Ok((b, h, d)),
                        (Err(e), _) => Err(e),
                        (_, Err(e)) => Err(e),
                    };

                    let _ = resp_tx.blocking_send(GitResponse::Branches(res));
                }
                GitCommand::GetCurrentBranch => {
                    let _ =
                        resp_tx.blocking_send(GitResponse::CurrentBranch(git.get_current_branch()));
                }
                GitCommand::CheckConflict { branch, onto, base } => {
                    let res = git.check_conflict(&branch, &onto, base.as_deref());
                    let _ =
                        resp_tx.blocking_send(GitResponse::ConflictCheck(branch, onto, base, res));
                }
                GitCommand::GetCommitLog { branch } => {
                    let res = git.get_commit_log(&branch);
                    let _ = resp_tx.blocking_send(GitResponse::CommitLog(branch, res));
                }
                GitCommand::FetchDiff { branch, parent } => {
                    let _ = resp_tx.blocking_send(GitResponse::Progress {
                        message: format!("Calculating diff for {}...", branch),
                        percentage: 0.1,
                    });
                    let res = (|| {
                        let repo = git
                            .repo
                            .lock()
                            .map_err(|_| anyhow::anyhow!("Repository mutex poisoned"))?;
                        let branch_oid = repo.revparse_single(&branch)?.peel_to_commit()?.id();
                        let parent_oid = repo.revparse_single(&parent)?.peel_to_commit()?.id();
                        let branch_tree = repo.find_commit(branch_oid)?.tree()?;
                        let parent_tree = repo.find_commit(parent_oid)?.tree()?;
                        let diff =
                            repo.diff_tree_to_tree(Some(&parent_tree), Some(&branch_tree), None)?;
                        let res = diff_utils::parse_diff(&diff);
                        let _ = resp_tx.blocking_send(GitResponse::Progress {
                            message: format!("Parsing diff for {}...", branch),
                            percentage: 0.5,
                        });
                        res
                    })();
                    let _ = resp_tx.blocking_send(GitResponse::DiffLoaded(branch, res));
                }
                GitCommand::PredictConflicts { mut plan, branches } => {
                    let tx = resp_tx.clone();
                    let progress = move |index: usize, result: Option<bool>| {
                        let _ = tx.blocking_send(GitResponse::PredictionProgress { index, result });
                    };
                    engine::predict_conflicts(&mut plan, &git, &branches, Some(&progress));
                    let _ = resp_tx.blocking_send(GitResponse::ConflictsPredicted(plan));
                }
            }
        }
    })
}

pub fn execute_plan<P: AsRef<Path>>(
    git: &dyn Git,
    branches: &[BranchInfo],
    intents: &std::collections::HashMap<String, crate::engine::BranchIntent>,
    history: &HistoryContext,
    path: P,
) -> anyhow::Result<()> {
    let is_dirty = git.is_dirty().unwrap_or(false);
    let snapshot = crate::engine::RepositorySnapshot {
        branches: branches.to_vec(),
        history: history.clone(),
        is_dirty,
    };
    let plan = calculate_plan(&snapshot, intents)?;
    execute_given_plan(git, &plan, path)
}

pub fn execute_rebases<P: AsRef<Path>>(
    git: &dyn Git,
    branches: &[BranchInfo],
    intents: &std::collections::HashMap<String, crate::engine::BranchIntent>,
    history: &HistoryContext,
    path: P,
) -> anyhow::Result<()> {
    let is_dirty = git.is_dirty().unwrap_or(false);
    let snapshot = crate::engine::RepositorySnapshot {
        branches: branches.to_vec(),
        history: history.clone(),
        is_dirty,
    };
    let plan = calculate_plan(&snapshot, intents)?;
    let rebase_only_plan: Vec<_> = plan
        .into_iter()
        .filter(|op| matches!(op, Operation::Rebase { .. }))
        .collect();
    execute_given_plan(git, &rebase_only_plan, path)
}

fn execute_given_plan<P: AsRef<Path>>(
    git: &dyn Git,
    plan: &[Operation],
    path: P,
) -> anyhow::Result<()> {
    let executor = ShellExecutor::new(ConflictMode::Shell);
    for op in plan {
        executor.execute(git, op, path.as_ref())?;
    }
    Ok(())
}
