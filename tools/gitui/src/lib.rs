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
    BranchInfo, CommitInfo, Executor, Git, HistoryContext, Operation, RealGit, ShellExecutor,
    calculate_plan, flatten_branches,
};

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
            // Since this is a static tree print, we don't have pending intents here usually,
            // but for completeness we'll check the (empty) intents.
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

        if let Some(h_parent) = &branch_info.heuristic_parent
            && Some(h_parent) != branch_info.original_parent.as_ref()
        {
            write!(writer, " {}", "[DIVERGED]".red().bold())?;
        }

        writeln!(writer)?;
    }
    Ok(())
}

pub fn print_submit_plan<P: AsRef<Path>>(path: P, branch_name: &str) -> anyhow::Result<()> {
    print_submit_plan_to(path, branch_name, &mut std::io::stdout())
}

pub fn print_submit_plan_to<P: AsRef<Path>, W: std::io::Write>(
    path: P,
    branch_name: &str,
    writer: &mut W,
) -> anyhow::Result<()> {
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

    let mut plan = calculate_plan(&snapshot, &intents)?;
    engine::predict_conflicts(&mut plan, &git, &snapshot.branches, None);

    if plan.is_empty() {
        writeln!(writer, "No operations to perform.")?;
    } else {
        writeln!(writer, "Plan to submit {}:", branch_name)?;
        for op in plan {
            let label = match op {
                Operation::Rebase {
                    predicted_conflict: Some(true),
                    ..
                } => " [CONFLICT]",
                Operation::Rebase {
                    predicted_conflict: Some(false),
                    ..
                } => " [CLEAN]",
                _ => "",
            };
            writeln!(writer, "  {}{}", op, label)?;
        }
    }

    Ok(())
}

pub fn print_sync_plan<P: AsRef<Path>>(path: P, branch_name: &str) -> anyhow::Result<()> {
    print_sync_plan_to(path, branch_name, &mut std::io::stdout())
}

pub fn print_sync_plan_to<P: AsRef<Path>, W: std::io::Write>(
    path: P,
    branch_name: &str,
    writer: &mut W,
) -> anyhow::Result<()> {
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

    let mut plan = calculate_plan(&snapshot, &intents)?;
    engine::predict_conflicts(&mut plan, &git, &snapshot.branches, None);

    if plan.is_empty() {
        writeln!(writer, "No operations to perform.")?;
    } else {
        writeln!(writer, "Plan to sync {}:", branch_name)?;
        for op in plan {
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
    }

    Ok(())
}

pub fn print_converge_plan<P: AsRef<Path>>(path: P, branch_name: &str) -> anyhow::Result<()> {
    print_converge_plan_to(path, branch_name, &mut std::io::stdout())
}

pub fn print_converge_plan_to<P: AsRef<Path>, W: std::io::Write>(
    path: P,
    branch_name: &str,
    writer: &mut W,
) -> anyhow::Result<()> {
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

    let mut plan = calculate_plan(&snapshot, &intents)?;
    engine::predict_conflicts(&mut plan, &git, &snapshot.branches, None);

    if plan.is_empty() {
        writeln!(writer, "No operations to perform.")?;
    } else {
        writeln!(writer, "Plan to converge {}:", branch_name)?;
        for op in plan {
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
    }

    Ok(())
}

pub enum GitCommand {
    GetBranches,
    GetCurrentBranch,
    CheckConflict {
        branch: String,
        onto: String,
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
    ConflictCheck(String, String, anyhow::Result<bool>),
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
                GitCommand::CheckConflict { branch, onto } => {
                    let res = git.check_conflict(&branch, &onto);
                    let _ = resp_tx.blocking_send(GitResponse::ConflictCheck(branch, onto, res));
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
    let executor = ShellExecutor::new(true);
    for op in plan {
        executor.execute(git, op, path.as_ref())?;
    }
    Ok(())
}
