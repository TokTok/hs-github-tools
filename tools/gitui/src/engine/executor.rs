use crate::engine::git::Git;
use crate::engine::refs::RebaseOutcome;
use crate::engine::types::{Operation, get_local_name};
use std::path::Path;
use std::process::Command;

pub trait Executor {
    fn execute(&self, git: &dyn Git, op: &Operation, path: &Path) -> anyhow::Result<bool>;
}

/// What to do when a rebase conflicts. Only the first two touch the working tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConflictMode {
    /// Rebase in the working tree and open a shell there to resolve it.
    Shell,
    /// Rebase in the working tree and stop there, for the user to resolve.
    Leave,
    /// Leave the branch and the working tree alone: fail with [`RebaseConflict`].
    Report,
}

/// A rebase that conflicted and was left alone.
#[derive(Debug)]
pub struct RebaseConflict {
    pub branch: String,
    pub paths: Vec<String>,
}

impl std::fmt::Display for RebaseConflict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} conflicts in {}", self.branch, self.paths.join(", "))
    }
}

impl std::error::Error for RebaseConflict {}

pub struct ShellExecutor {
    pub on_conflict: ConflictMode,
}

impl ShellExecutor {
    pub fn new(on_conflict: ConflictMode) -> Self {
        Self { on_conflict }
    }
}

fn run(git: &dyn Git, args: &[&str]) -> anyhow::Result<()> {
    let cmd: Vec<String> = args.iter().map(|a| a.to_string()).collect();
    anyhow::ensure!(
        git.run_command(&cmd)?,
        "Command failed: git {}",
        args.join(" ")
    );
    Ok(())
}

impl Executor for ShellExecutor {
    fn execute(&self, git: &dyn Git, op: &Operation, path: &Path) -> anyhow::Result<bool> {
        match op {
            Operation::Split {
                branch,
                parent,
                data,
            } => {
                eprintln!("Splitting {}...", branch);
                git.split_branch(branch, parent, data)?;
            }
            Operation::Rebase {
                branch,
                onto,
                upstream,
                predicted_conflict: _,
            } => {
                eprintln!("Rebasing {} onto {}...", branch, onto);
                let RebaseOutcome::Conflict { paths } =
                    git.rebase(branch, onto, upstream.as_deref())?
                else {
                    return Ok(true);
                };
                if self.on_conflict == ConflictMode::Report {
                    return Err(RebaseConflict {
                        branch: branch.clone(),
                        paths,
                    }
                    .into());
                }
                for cmd in op.commands() {
                    if !git.run_command(&cmd)? {
                        if self.on_conflict == ConflictMode::Shell {
                            drop_to_shell(path)?;
                        } else {
                            anyhow::bail!(
                                "Rebase failed for {}. Manual resolution required.",
                                branch
                            );
                        }
                    }
                }
            }
            Operation::Sync { branch, onto, .. } => {
                eprintln!("Executing: {}", op);
                git.move_branch(branch, onto, true)?;
                run(git, &["push", "origin", branch])?;
            }
            Operation::Submit { branch, target } => {
                eprintln!("Executing: {}", op);
                run(
                    git,
                    &["push", "upstream", &format!("{}:{}", branch, target)],
                )?;
                run(git, &["push", "origin", "--delete", branch])?;
                git.move_branch(target, branch, true)?;
                run(git, &["push", "origin", target])?;
            }
            Operation::Reset { branch } => {
                eprintln!("Executing: {}", op);
                git.move_branch(branch, &format!("{}@{{u}}", branch), false)?;
            }
            Operation::Delete {
                branch,
                merged_into: Some(target),
            } => {
                eprintln!("Executing: {}", op);
                git.delete_branch(branch, target)?;
            }
            Operation::Localize { branch } => {
                eprintln!("Executing: {}", op);
                let local = get_local_name(branch);
                git.move_branch(local, branch, false)?;
                run(git, &["branch", "-q", "--set-upstream-to", branch, local])?;
            }
            _ => {
                eprintln!("Executing: {}", op);
                for cmd in op.commands() {
                    if !git.run_command(&cmd)? {
                        anyhow::bail!("Command failed: git {}", cmd.join(" "));
                    }
                }
            }
        }
        Ok(true)
    }
}

fn drop_to_shell<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    println!("\nRebase conflict! Dropping to shell.");
    println!("Resolve conflicts (git add, etc.) and exit the shell to continue.");

    let shell = std::env::var("SHELL").unwrap_or_else(|_| "sh".to_string());
    Command::new(shell).current_dir(path).status()?;

    Ok(())
}
