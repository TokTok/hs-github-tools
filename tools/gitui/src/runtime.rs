use crate::engine::{BranchInfo, Git, RealGit};
use crate::state::{AppMode, AppState, Effect, Msg};
use crate::{GitCommand, GitResponse, TerminalGuard, execute_plan, spawn_worker, ui};
use crossterm::event::{self, Event};
use ratatui::Terminal;
use ratatui::backend::Backend;
use std::path::Path;
use std::time::Duration;
use tokio::sync::mpsc;

pub async fn run_runtime<P: AsRef<Path>>(path: P) -> anyhow::Result<()> {
    let mut state = AppState::default();
    let path = path.as_ref();

    loop {
        let (cmd_tx, cmd_rx) = mpsc::channel::<GitCommand>(32);
        let (resp_tx, mut resp_rx) = mpsc::channel::<GitResponse>(32);

        let path_clone = path.to_path_buf();
        let _worker = spawn_worker(path_clone, cmd_rx, resp_tx);

        // Initial data
        cmd_tx.send(GitCommand::GetBranches).await?;
        cmd_tx.send(GitCommand::GetCurrentBranch).await?;

        let mut guard = TerminalGuard::new()?;

        let run_res = run_loop(&mut guard.terminal, &mut state, &cmd_tx, &mut resp_rx).await;

        drop(guard);

        match run_res {
            Ok(Some((branches_to_execute, intents))) => {
                let git = RealGit::new(path)?;
                let exec_res =
                    execute_plan(&git, &branches_to_execute, &intents, &state.history, path);

                if exec_res.is_ok() {
                    state.clear_pending_operations();
                    if let Ok(current) = git.get_current_branch()
                        && current != state.initial_branch
                    {
                        let branch_exists = |name: &str| -> bool {
                            git.run_command(&[
                                "show-ref".to_string(),
                                "--verify".to_string(),
                                "--quiet".to_string(),
                                format!("refs/heads/{}", name),
                            ])
                            .unwrap_or(false)
                        };

                        if branch_exists(&state.initial_branch) {
                            println!("\nRestoring initial branch: {}...", state.initial_branch);
                            let _ = git.checkout(&state.initial_branch);
                        } else {
                            println!(
                                "\nInitial branch {} no longer exists.",
                                state.initial_branch
                            );

                            let mut children: Vec<String> = branches_to_execute
                                .iter()
                                .filter(|b| {
                                    let intent = intents.get(&b.name);
                                    let effective_parent =
                                        match intent.and_then(|i| i.parent.as_ref()) {
                                            Some(p) => p.clone(),
                                            None => b.original_parent.clone(),
                                        };
                                    effective_parent.as_ref() == Some(&state.initial_branch)
                                })
                                .map(|b| b.name.clone())
                                .collect();
                            children.sort();

                            let mut target = None;
                            for child in children {
                                if branch_exists(&child) {
                                    target = Some(child);
                                    break;
                                }
                            }

                            if target.is_none() {
                                if branch_exists("master") {
                                    target = Some("master".to_string());
                                } else if branch_exists("main") {
                                    target = Some("main".to_string());
                                }
                            }

                            if let Some(t) = target {
                                if t != current {
                                    println!("Switching to: {}...", t);
                                    let _ = git.checkout(&t);
                                } else {
                                    println!("Staying on {}.", current);
                                }
                            } else {
                                println!("Staying on {}.", current);
                            }
                        }
                    }
                }

                println!("\nExecution finished. Press Enter to return to TUI...");
                let mut _buf = String::new();
                let _ = std::io::stdin().read_line(&mut _buf);

                if let Err(e) = exec_res {
                    eprintln!("Error during execution: {:?}", e);
                    let _ = std::io::stdin().read_line(&mut _buf);
                }
            }
            Ok(None) => break,
            Err(e) => {
                eprintln!("{:?}", e);
                break;
            }
        }
    }

    Ok(())
}

enum LoopControl {
    Continue,
    Break(
        Option<(
            Vec<BranchInfo>,
            std::collections::HashMap<String, crate::engine::BranchIntent>,
        )>,
    ),
}

async fn run_loop<B: Backend>(
    terminal: &mut Terminal<B>,
    state: &mut AppState,
    cmd_tx: &mpsc::Sender<GitCommand>,
    resp_rx: &mut mpsc::Receiver<GitResponse>,
) -> anyhow::Result<
    Option<(
        Vec<BranchInfo>,
        std::collections::HashMap<String, crate::engine::BranchIntent>,
    )>,
>
where
    anyhow::Error: From<<B as Backend>::Error>,
{
    loop {
        // Draw
        if state.needs_redraw {
            terminal.draw(|f| match state.mode {
                AppMode::Preview => {
                    let plan = state.plan.as_ref().cloned().unwrap_or_default();
                    ui::draw_preview(f, &plan, state);
                }
                AppMode::Split => {
                    ui::draw_split_view(f, state);
                }
                AppMode::Tree => {
                    ui::draw_main(f, state);
                }
                AppMode::Prompt => {
                    ui::draw_prompt(f, state);
                }
            })?;
            state.needs_redraw = false;
        }

        // Handle Input & Ticks
        let timeout = Duration::from_millis(50);
        if event::poll(timeout)? {
            let event = event::read()?;
            if let Event::Key(key) = event {
                let effects = state.update(Msg::KeyPressed(key));
                if let LoopControl::Break(res) = handle_effects(effects, cmd_tx).await? {
                    return Ok(res);
                }
            }
        } else {
            let effects = state.update(Msg::Tick);
            if let LoopControl::Break(res) = handle_effects(effects, cmd_tx).await? {
                return Ok(res);
            }
        }

        // Handle Worker Responses
        while let Ok(resp) = resp_rx.try_recv() {
            let msg = match resp {
                GitResponse::Branches(res) => Msg::BranchesLoaded(res),
                GitResponse::CurrentBranch(res) => Msg::CurrentBranchLoaded(res),
                GitResponse::ConflictCheck(branch, onto, res) => {
                    Msg::ConflictChecked(branch, onto, res)
                }
                GitResponse::CommitLog(branch, res) => Msg::CommitLogLoaded(branch, res),
                GitResponse::Progress {
                    message,
                    percentage,
                } => Msg::ProgressUpdated {
                    message,
                    percentage,
                },
                GitResponse::ConflictsPredicted(plan) => Msg::ConflictsPredicted(plan),
                GitResponse::PredictionProgress { index, result } => {
                    Msg::PredictionProgress { index, result }
                }
                GitResponse::DiffLoaded(branch, res) => Msg::DiffLoaded(branch, res),
            };
            let effects = state.update(msg);
            if let LoopControl::Break(res) = handle_effects(effects, cmd_tx).await? {
                return Ok(res);
            }
        }
    }
}

async fn handle_effects(
    effects: Vec<Effect>,
    cmd_tx: &mpsc::Sender<GitCommand>,
) -> anyhow::Result<LoopControl> {
    for effect in effects {
        match effect {
            Effect::FetchBranches => {
                cmd_tx.send(GitCommand::GetBranches).await?;
            }
            Effect::FetchCurrentBranch => {
                cmd_tx.send(GitCommand::GetCurrentBranch).await?;
            }
            Effect::CheckConflict { branch, onto } => {
                cmd_tx
                    .send(GitCommand::CheckConflict { branch, onto })
                    .await?;
            }
            Effect::FetchCommitLog { branch } => {
                cmd_tx.send(GitCommand::GetCommitLog { branch }).await?;
            }
            Effect::PredictConflicts { plan, branches } => {
                cmd_tx
                    .send(GitCommand::PredictConflicts { plan, branches })
                    .await?;
            }
            Effect::FetchDiff { branch, parent } => {
                cmd_tx
                    .send(GitCommand::FetchDiff { branch, parent })
                    .await?;
            }
            Effect::Quit => return Ok(LoopControl::Break(None)),
            Effect::ApplyAndQuit(branches, intents) => {
                return Ok(LoopControl::Break(Some((branches, intents))));
            }
        }
    }
    Ok(LoopControl::Continue)
}
