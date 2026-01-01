use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use gitui::engine::{Git, Operation, RealGit};
use gitui::state::{AppMode, AppState, Msg};
use gitui::testing::{create_commit, run_git, setup_repo};

#[tokio::test]
async fn test_tui_sync_master_plan() {
    let (dir, repo, mut master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    if master != "master" && master != "main" {
        run_git(path_str, &["branch", "-m", &master, "master"]);
        master = "master".to_string();
    }

    // 1. Setup 'upstream' remote
    let upstream_dir = tempfile::tempdir().unwrap();
    run_git(upstream_dir.path().to_str().unwrap(), &["init", "--bare"]);
    run_git(
        path_str,
        &[
            "remote",
            "add",
            "upstream",
            upstream_dir.path().to_str().unwrap(),
        ],
    );

    // 2. Setup 'origin' remote
    let origin_dir = tempfile::tempdir().unwrap();
    run_git(origin_dir.path().to_str().unwrap(), &["init", "--bare"]);
    run_git(
        path_str,
        &[
            "remote",
            "add",
            "origin",
            origin_dir.path().to_str().unwrap(),
        ],
    );

    // 3. Make 'upstream/master' ahead
    create_commit(&repo, "upstream.txt", "upstream", "Upstream commit");
    run_git(
        path_str,
        &["push", "upstream", &format!("{}:{}", master, master)],
    );

    // Reset local master back to initial commit
    run_git(path_str, &["reset", "--hard", "HEAD^"]);

    // 4. Setup local master to track origin/master
    run_git(
        path_str,
        &["push", "origin", &format!("{}:{}", master, master)],
    );
    run_git(
        path_str,
        &[
            "branch",
            &format!("--set-upstream-to=origin/{}", master),
            &master,
        ],
    );

    // Fetch upstream so it exists
    run_git(path_str, &["fetch", "upstream"]);

    // 5. Initialize TUI state (show_remote = false by default)
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        is_dirty: false,
        show_remote: false,
        ..AppState::default()
    };
    state.refresh_tree(None);

    // Ensure master is selected
    if let Some(idx) = state.flattened_tree.iter().position(|(n, _)| n == &master) {
        state.list_state.select(Some(idx));
    } else {
        panic!("master branch not found in flattened tree");
    }

    // 6. Press 'r' to sync
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('r'),
        KeyModifiers::NONE,
    )));

    // Verify intent is set
    assert!(state.get_intent(&master).pending_reset);

    // 7. Press 'v' to preview
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('v'),
        KeyModifiers::NONE,
    )));

    assert_eq!(state.mode, AppMode::Preview);
    let plan = state.plan.as_ref().expect("Plan should be calculated");

    let has_sync = plan.iter().any(|op| matches!(op, Operation::Sync { .. }));
    assert!(
        has_sync,
        "Plan should contain a Sync operation even if remote branches are not shown in UI. Current plan: {:?}",
        plan
    );
}
