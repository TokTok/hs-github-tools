use crate::engine::git::Git;
use crate::engine::types::Operation;
use std::path::Path;
use std::process::Command;

pub trait Executor {
    fn execute(&self, git: &dyn Git, op: &Operation, path: &Path) -> anyhow::Result<bool>;
}

pub struct ShellExecutor {
    pub interactive: bool,
}

impl ShellExecutor {
    pub fn new(interactive: bool) -> Self {
        Self { interactive }
    }
}

impl Executor for ShellExecutor {
    fn execute(&self, git: &dyn Git, op: &Operation, path: &Path) -> anyhow::Result<bool> {
        match op {
            Operation::Split {
                branch,
                parent,
                data,
            } => {
                println!("Splitting {}...", branch);
                git.split_branch(branch, parent, data)?;
                Ok(true)
            }
            Operation::Rebase {
                branch,
                onto,
                upstream: _,
                predicted_conflict: _,
            } => {
                println!("Rebasing {} onto {}...", branch, onto);
                let cmds = op.commands();
                for cmd in cmds {
                    if !git.run_command(&cmd)? {
                        if self.interactive {
                            drop_to_shell(path)?;
                        } else {
                            anyhow::bail!(
                                "Rebase failed for {}. Manual resolution required.",
                                branch
                            );
                        }
                    }
                }
                Ok(true)
            }
            _ => {
                println!("Executing: {}", op);
                let cmds = op.commands();
                for cmd in cmds {
                    if !git.run_command(&cmd)? {
                        anyhow::bail!("Command failed: git {}", cmd.join(" "));
                    }
                }
                Ok(true)
            }
        }
    }
}

fn drop_to_shell<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    println!("\nRebase conflict! Dropping to shell.");
    println!("Resolve conflicts (git add, etc.) and exit the shell to continue.");

    let shell = std::env::var("SHELL").unwrap_or_else(|_| "sh".to_string());
    Command::new(shell).current_dir(path).status()?;

    Ok(())
}
