use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use gitui::engine::{Git, RealGit};
use gitui::execute_plan;
use gitui::state::{AppState, Effect, Msg};
use gitui::testing::{create_commit, run_git, setup_repo};

#[tokio::test]
async fn test_push_flow_p_then_c() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup a "remote"
    let remote_dir = tempfile::tempdir().unwrap();
    run_git(remote_dir.path().to_str().unwrap(), &["init", "--bare"]);
    run_git(
        path_str,
        &[
            "remote",
            "add",
            "origin",
            remote_dir.path().to_str().unwrap(),
        ],
    );

    // 2. Create a branch and a commit
    create_commit(&repo, "push.txt", "push me", "Push commit");
    repo.branch(
        "push-branch",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 3. Initialize AppState
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..AppState::default()
    };
    state.refresh_tree(None);

    // 4. Navigate to "push-branch" (it should be at index 1, index 0 is master)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('j'),
        KeyModifiers::NONE,
    )));
    assert_eq!(state.get_selected_branch_name().unwrap(), "push-branch");

    // 5. Press 'p' to toggle push
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('p'),
        KeyModifiers::NONE,
    )));
    assert!(state.get_intent("push-branch").pending_push);

    // 6. Press 'c' to commit/execute
    let effects = state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('c'),
        KeyModifiers::NONE,
    )));

    // 7. Verify ApplyAndQuit effect
    let mut branches_to_exec = None;
    let mut intents_to_exec = None;
    for effect in effects {
        if let Effect::ApplyAndQuit(b, i) = effect {
            branches_to_exec = Some(b);
            intents_to_exec = Some(i);
        }
    }

    let branches_to_exec = branches_to_exec.expect("ApplyAndQuit effect not found");
    let intents_to_exec = intents_to_exec.expect("ApplyAndQuit effect not found");
    assert!(intents_to_exec.get("push-branch").unwrap().pending_push);

    // 8. Execute the plan
    execute_plan(
        &git,
        &branches_to_exec,
        &intents_to_exec,
        &state.history,
        path_str,
    )
    .unwrap();

    // 9. Verify it was pushed to remote
    let output = run_git(path_str, &["ls-remote", "origin", "push-branch"]);
    assert!(!output.is_empty(), "Branch should exist on remote");
}
