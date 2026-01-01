use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use git2::Signature;
use gitui::engine::{Git, RealGit, build_topology, calculate_plan};
use gitui::state::{AppState, ConflictCheckState, Effect, Msg};
use gitui::testing::{create_commit, run_git, run_git_with_env, setup_repo};
use gitui::ui;
use insta::assert_snapshot;
use ratatui::Terminal;
use ratatui::backend::TestBackend;

fn buffer_to_string(buffer: &ratatui::buffer::Buffer) -> String {
    let mut s = String::new();
    for y in 0..buffer.area.height {
        for x in 0..buffer.area.width {
            s.push_str(buffer[(x, y)].symbol());
        }
        s.push('\n');
    }
    s
}

fn configure_insta() -> insta::Settings {
    let mut settings = insta::Settings::clone_current();
    if let Ok(workspace_dir) = std::env::var("BUILD_WORKSPACE_DIRECTORY") {
        let mut p = std::path::PathBuf::from(workspace_dir);
        p.push("hs-github-tools");
        p.push("tools");
        p.push("gitui");
        p.push("test");
        p.push("snapshots");
        settings.set_snapshot_path(p);
    } else {
        // Fallback: Check if we are in Bazel sandbox structure
        let cwd = std::env::current_dir().unwrap();
        let sandbox_path = cwd.join("hs-github-tools/tools/gitui/test/snapshots");
        if sandbox_path.exists() {
            settings.set_snapshot_path(sandbox_path);
        } else {
            // Cargo fallback
            // In cargo, we are at .../gitui
            let cargo_path = cwd.join("test/snapshots");
            settings.set_snapshot_path(cargo_path);
        }
    }
    settings
}

#[tokio::test]
async fn test_tui_reset_stability_groups_perf() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup scenario: master -> groups-perf
    // master is at 'Initial commit'
    let gp_oid = create_commit(&repo, "f.txt", "c", "groups-perf commit");
    repo.branch("groups-perf", &repo.find_commit(gp_oid).unwrap(), false)
        .unwrap();

    // Create a bare remote
    let remote_dir = tempfile::tempdir().unwrap();
    let remote_path = remote_dir.path().to_str().unwrap();
    std::process::Command::new("git")
        .arg("init")
        .arg("--bare")
        .arg(remote_path)
        .status()
        .unwrap();

    // Add remote and push groups-perf
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["remote", "add", "origin", remote_path])
        .status()
        .unwrap();

    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["push", "origin", "groups-perf:groups-perf"])
        .status()
        .unwrap();

    // Set upstream properly
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args([
            "branch",
            "--set-upstream-to=origin/groups-perf",
            "groups-perf",
        ])
        .status()
        .unwrap();

    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["fetch", "origin"])
        .status()
        .unwrap();

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let intents = std::collections::HashMap::new();

    // Verify initial state: groups-perf should be a child of master/main
    let initial_topo = build_topology(&branches, &intents, &history, false).unwrap();
    let initial_flat = initial_topo.flatten();
    let gp_depth = initial_flat
        .iter()
        .find(|(n, _)| n == "groups-perf")
        .map(|(_, d)| *d)
        .expect("groups-perf not found");
    assert!(
        gp_depth > 0,
        "groups-perf should be indented under master initially"
    );

    // 2. Setup AppState
    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // 3. Simulate events
    // Use 'j' to select groups-perf. It should be after master.
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('j'),
        KeyModifiers::NONE,
    )));
    // Press 'r' to trigger reset
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('r'),
        KeyModifiers::NONE,
    )));

    // 4. Render and Inspect the final buffer
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();

    let buffer = terminal.backend().buffer();

    let mut found_groups_perf = false;
    let mut indentation_correct = false;

    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }

        if line.contains("groups-perf") && (line.contains("[REBASE]") || line.contains("[RESET]")) {
            found_groups_perf = true;
            // The debug output showed: " 2: │>>  groups-perf [REBASE] [MERGED] ..."
            // The marker is ">>", followed by 2 spaces of indentation for depth 1.
            if line.contains(">>  groups-perf") {
                indentation_correct = true;
            }
        }
    }

    assert!(
        found_groups_perf,
        "groups-perf [REBASE]/[RESET] not found in UI"
    );
    assert!(
        indentation_correct,
        "groups-perf has incorrect indentation!"
    );
}

#[tokio::test]
async fn test_tui_noop_preview_is_empty() {
    let (dir, _repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup scenario: master tracking origin/master, already in sync
    let remote_dir = tempfile::tempdir().unwrap();
    let remote_path = remote_dir.path().to_str().unwrap();
    std::process::Command::new("git")
        .arg("init")
        .arg("--bare")
        .arg(remote_path)
        .status()
        .unwrap();
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["remote", "add", "origin", remote_path])
        .status()
        .unwrap();
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["push", "origin", &format!("{}:{}", master, master)])
        .status()
        .unwrap();
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args([
            "branch",
            &format!("--set-upstream-to=origin/{}", master),
            &master,
        ])
        .status()
        .unwrap();

    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["fetch", "origin"])
        .status()
        .unwrap();

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    // Verify it's actually in sync
    {
        let main_info = branches.iter().find(|b| b.name == master).unwrap();
        assert_eq!(main_info.ahead, 0);
        assert_eq!(main_info.behind, 0);
        assert!(main_info.upstream_oid.is_some());
        assert_eq!(main_info.upstream_oid, Some(main_info.oid));
    }

    // 2. Setup AppState
    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // 3. Simulate events: 'r' (reset), 'v' (preview)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('r'),
        KeyModifiers::NONE,
    )));
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('v'),
        KeyModifiers::NONE,
    )));

    // 4. Verify preview buffer
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal
        .draw(|f| {
            let snapshot = gitui::engine::RepositorySnapshot {
                branches: state.branches.clone(),
                history: state.history.clone(),
                is_dirty: state.is_dirty,
            };
            let plan = calculate_plan(&snapshot, &state.intents).unwrap();
            ui::draw_preview(f, &plan, &state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let mut found_no_ops_msg = false;
    let mut found_git_command = false;

    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("No operations to perform.") {
            found_no_ops_msg = true;
        }
        if line.contains("git rebase") || line.contains("git reset") {
            found_git_command = true;
        }
    }

    assert!(
        found_no_ops_msg,
        "Preview should say 'No operations to perform.'. Buffer:\n{:?}",
        buffer
    );
    assert!(
        !found_git_command,
        "Preview should NOT show any git commands for a no-op reset"
    );
}

#[tokio::test]
async fn test_tui_quit_confirmation() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    let remote_dir = tempfile::tempdir().unwrap();
    let remote_path = remote_dir.path().to_str().unwrap();
    std::process::Command::new("git")
        .arg("init")
        .arg("--bare")
        .arg(remote_path)
        .status()
        .unwrap();
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["remote", "add", "origin", remote_path])
        .status()
        .unwrap();
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["push", "origin", "main:main"])
        .status()
        .unwrap();

    // Create a commit on local only
    create_commit(&repo, "f.txt", "c", "local change");
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["branch", "--set-upstream-to=origin/main", "main"])
        .status()
        .unwrap();

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // 1. Press 'r' (reset) -> pending operation created
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('r'),
        KeyModifiers::NONE,
    )));

    // 2. Press 'q' (quit) -> should show confirmation
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('q'),
        KeyModifiers::NONE,
    )));
    assert!(state.show_quit_confirmation);

    // 3. Press 'n' (no) -> stay in app
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('n'),
        KeyModifiers::NONE,
    )));
    assert!(!state.show_quit_confirmation);

    // 4. Press 'q' then 'y' -> quit
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('q'),
        KeyModifiers::NONE,
    )));
    let effects = state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('y'),
        KeyModifiers::NONE,
    )));

    // Verification: ensure Effect::Quit was emitted
    assert!(effects.iter().any(|e| matches!(e, Effect::Quit)));
}

#[tokio::test]
async fn test_tui_move_remote_to_localize() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup a remote and a branch on it
    let remote_dir = tempfile::tempdir().unwrap();
    let remote_path = remote_dir.path().to_str().unwrap();
    std::process::Command::new("git")
        .arg("init")
        .arg("--bare")
        .arg(remote_path)
        .status()
        .unwrap();
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["remote", "add", "origin", remote_path])
        .status()
        .unwrap();

    // Create 'feat' on remote
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["checkout", "-b", "feat"])
        .status()
        .unwrap();
    let sig = Signature::now("Test", "test@example.com").unwrap();
    let parent = repo.head().unwrap().peel_to_commit().unwrap();
    let tree = repo
        .find_tree(repo.index().unwrap().write_tree().unwrap())
        .unwrap();
    repo.commit(Some("HEAD"), &sig, &sig, "Feat commit", &tree, &[&parent])
        .unwrap();

    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["push", "origin", "feat"])
        .status()
        .unwrap();
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["checkout", &master])
        .status()
        .unwrap();
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["branch", "-D", "feat"])
        .status()
        .unwrap();
    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["fetch", "origin"])
        .status()
        .unwrap();

    let git = RealGit::new(path_str).unwrap();
    // Start with remote branches visible
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        show_remote: true,
        ..Default::default()
    };
    state.refresh_tree(None);

    // 2. Simulate TUI events
    // 1. Move to origin/feat (should be index 1 if master is index 0)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('j'),
        KeyModifiers::NONE,
    )));
    // 2. Press Space to grab
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));
    // 3. Move back to master
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('k'),
        KeyModifiers::NONE,
    )));
    // 4. Press Space to release
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));

    // 3. Verify buffer contains "feat [LOCALIZE]" and NOT "origin/feat"
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();

    let buffer = terminal.backend().buffer();
    let mut found_localized_feat = false;
    let mut found_origin_feat = false;

    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("feat") && line.contains("[LOCALIZE]") {
            found_localized_feat = true;
        }
        if line.contains("origin/feat") {
            found_origin_feat = true;
        }
    }

    assert!(
        found_localized_feat,
        "UI should show 'feat [LOCALIZE]' after move"
    );
    assert!(
        !found_origin_feat,
        "UI should NOT show 'origin/feat' after move"
    );
}

#[tokio::test]
async fn test_tui_predictive_conflict_detection() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup conflicting branches
    // Base: a.txt="base"
    let base_oid = create_commit(&repo, "f.txt", "c", "Base");

    // feat1: a.txt="feat1"
    run_git(path_str, &["checkout", "--detach", &base_oid.to_string()]);
    std::fs::write(dir.path().join("a.txt"), "feat1").unwrap();
    run_git(path_str, &["add", "a.txt"]);
    let feat1_oid = create_commit(&repo, "f.txt", "c", "Feat1");
    repo.branch("feat1", &repo.find_commit(feat1_oid).unwrap(), false)
        .unwrap();

    // feat2: a.txt="feat2"
    run_git(path_str, &["checkout", "--detach", &base_oid.to_string()]);
    std::fs::write(dir.path().join("a.txt"), "feat2").unwrap();
    run_git(path_str, &["add", "a.txt"]);
    let feat2_oid = create_commit(&repo, "f.txt", "c", "Feat2");
    repo.branch("feat2", &repo.find_commit(feat2_oid).unwrap(), false)
        .unwrap();

    // 2. Setup AppState
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // 3. Setup TUI and simulate events
    // Select feat2 (index 2: master, feat1, feat2)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('j'),
        KeyModifiers::NONE,
    )));
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('j'),
        KeyModifiers::NONE,
    )));
    // Grab feat2
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));
    // Move to hover over feat1
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('k'),
        KeyModifiers::NONE,
    )));

    // Transition state machine to Checking so it accepts the result
    state.conflict_check = ConflictCheckState::Checking {
        branch: "feat2".to_string(),
        onto: "feat1".to_string(),
    };

    // Simulate Worker Response
    state.update(Msg::ConflictChecked(
        "feat2".to_string(),
        "feat1".to_string(),
        Ok(true),
    ));

    // 4. Verify conflict warning
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();

    let buffer = terminal.backend().buffer();
    let mut found_conflict = false;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("feat2") && line.contains("(CONFLICT)") {
            found_conflict = true;
        }
    }
    assert!(
        found_conflict,
        "UI should show (CONFLICT) for feat2 when hovering over feat1"
    );
}

#[tokio::test]
async fn test_tui_realtime_reflattening() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup sibling branches
    create_commit(&repo, "f.txt", "c", "feat1 commit");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    create_commit(&repo, "f.txt", "c", "feat2 commit");
    repo.branch(
        "feat2",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // Simulate events
    // Select feat2 (index 2: master, feat1, feat2)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('j'),
        KeyModifiers::NONE,
    )));
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('j'),
        KeyModifiers::NONE,
    )));
    // Grab feat2
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));

    // 2. Render and verify buffer shows feat2 indented (since we preserve parent on grab)
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();

    let buffer = terminal.backend().buffer();
    let mut feat2_line = None;

    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("feat2") {
            feat2_line = Some(line.clone());
            // It should have ">>" marker and leading spaces (depth 1 -> 2 spaces + padding)
            // ">>    feat2"
            assert!(
                line.contains(">>    feat2"),
                "feat2 should be indented when grabbed. line: {}",
                line
            );
        }
    }
    feat2_line.expect("feat2 not found");
}

#[tokio::test]
async fn test_tui_move_skip_behavior() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup 3 root branches on different commits to ensure they are all roots
    let initial_commit = repo.head().unwrap().peel_to_commit().unwrap();

    // a-branch
    create_commit(&repo, "f.txt", "c", "commit a");
    repo.branch(
        "a-branch",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // b-branch (back from initial)
    repo.set_head_detached(initial_commit.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit b");
    repo.branch(
        "b-branch",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // c-branch (back from initial)
    repo.set_head_detached(initial_commit.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit c");
    repo.branch(
        "c-branch",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // Cleanup: move to a-branch and delete master
    run_git(path_str, &["checkout", "a-branch"]);
    run_git(path_str, &["branch", "-D", &master]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: "a-branch".to_string(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // Simulate events
    // Start: a-branch (0), b-branch (1), c-branch (2)
    // Select c-branch
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('j'),
        KeyModifiers::NONE,
    )));
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('j'),
        KeyModifiers::NONE,
    )));
    // Grab c-branch
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));
    // Move to hover a-branch (index 0)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('k'),
        KeyModifiers::NONE,
    )));
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('k'),
        KeyModifiers::NONE,
    )));
    // State now: a-branch (0), c-branch (1, child of a), b-branch (2)

    // Press Down. Should skip c-branch (1) and land on b-branch (2).
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('j'),
        KeyModifiers::NONE,
    )));

    // Press 'h' to move c-branch to root.
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('h'),
        KeyModifiers::NONE,
    )));

    // 2. Render and verify final state: c-branch is a root and at index 2
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();

    let buffer = terminal.backend().buffer();
    let mut c_branch_line = None;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("c-branch") {
            c_branch_line = Some(line);
            break;
        }
    }

    let line = c_branch_line.expect("c-branch not found");
    // Indent of 2 spaces for depth 1 + 2 spaces padding = 4 spaces.
    // Root should only have 2 spaces padding.
    assert!(
        !line.contains("    c-branch"),
        "c-branch should be root (depth 0). Actual line: '{}'",
        line
    );
}

#[tokio::test]
async fn test_tui_cancel_grab() {
    let (dir, _repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup root branches
    run_git(path_str, &["branch", "feat1"]);
    let initial_oid = run_git(path_str, &["rev-parse", "HEAD"]);
    run_git(path_str, &["checkout", "--detach", &initial_oid]);
    run_git(path_str, &["branch", "-D", &master]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: "feat1".to_string(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // 1. Grab feat1
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));
    assert!(state.grabbed_branch.is_some());

    // 2. Cancel grab with 'Esc'
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Esc,
        KeyModifiers::NONE,
    )));
    assert!(state.grabbed_branch.is_none());

    // 3. Grab again
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));
    assert!(state.grabbed_branch.is_some());

    // 4. Cancel grab with 'Esc' (was 'Esc' in original code too)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Esc,
        KeyModifiers::NONE,
    )));
    assert!(state.grabbed_branch.is_none());
}

#[tokio::test]
async fn test_tui_move_sorting() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup branches exactly as in the user report:
    // master (root)
    //   groups-perf (child of master)
    // loglogdata (root, sorts before master)

    // Create groups-perf as child of master
    create_commit(&repo, "f.txt", "c", "groups-perf commit");
    run_git(path_str, &["branch", "groups-perf"]);

    // Create loglogdata as a root sibling (off initial commit)
    let initial_oid = run_git(path_str, &["rev-parse", "HEAD^"]);
    run_git(path_str, &["checkout", "--detach", &initial_oid]);
    create_commit(&repo, "f.txt", "c", "loglogdata commit");
    run_git(path_str, &["branch", "loglogdata"]);

    // Switch back to master (default)
    run_git(path_str, &["checkout", &master]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // Initial Tree (Alphabetical Roots: loglogdata < master):
    // 0: loglogdata (root)
    // 1: master (root)
    // 2:   groups-perf (child)
    assert_eq!(state.flattened_tree[0].0, "loglogdata");
    assert_eq!(state.flattened_tree[1].0, master);
    assert_eq!(state.flattened_tree[2].0, "groups-perf");

    // Cursor starts at 0: loglogdata
    // 1. Grab loglogdata
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));
    // 2. Press Down. Skips loglogdata, lands on master (index 1).
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('j'),
        KeyModifiers::NONE,
    )));

    // 3. Render and verify sorting: groups-perf should be ABOVE loglogdata under master
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();

    let buffer = terminal.backend().buffer();
    let mut gp_y = None;
    let mut log_y = None;

    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("groups-perf") {
            gp_y = Some(y);
        }
        if line.contains("loglogdata") {
            log_y = Some(y);
        }
    }

    let y1 = gp_y.expect("groups-perf not found");
    let y2 = log_y.expect("loglogdata not found");

    assert!(
        y1 < y2,
        "groups-perf (y={}) should be above loglogdata (y={})",
        y1,
        y2
    );
}

#[tokio::test]
async fn test_tui_subtree_movement() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();
    let initial_commit = repo.head().unwrap().peel_to_commit().unwrap();

    // Create 4 children of master
    for i in 1..=4 {
        // master is at initial_commit
        repo.set_head_detached(initial_commit.id()).unwrap();
        create_commit(&repo, "f.txt", "c", &format!("commit {}", i));
        repo.branch(
            &format!("branch_{}", i),
            &repo.head().unwrap().peel_to_commit().unwrap(),
            false,
        )
        .unwrap();
    }

    // Create Stacked Subtree (also child of master)
    // stack_root
    repo.set_head_detached(initial_commit.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit stack root");
    let root_oid = repo.head().unwrap().peel_to_commit().unwrap();
    repo.branch("stack_root", &root_oid, false).unwrap();

    // stack_child_1
    repo.set_head_detached(root_oid.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit stack child 1");
    let c1_oid = repo.head().unwrap().peel_to_commit().unwrap();
    repo.branch("stack_child_1", &c1_oid, false).unwrap();

    // stack_child_2
    repo.set_head_detached(c1_oid.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit stack child 2");
    let c2_oid = repo.head().unwrap().peel_to_commit().unwrap();
    repo.branch("stack_child_2", &c2_oid, false).unwrap();

    // stack_child_3
    repo.set_head_detached(c2_oid.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit stack child 3");
    repo.branch(
        "stack_child_3",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // Create z_branch (child of master, should appear after stack_root)
    repo.set_head_detached(initial_commit.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit z");
    repo.branch(
        "z_branch",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // Switch to master to view the tree from root
    run_git(path_str, &["checkout", &master]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // Initial Tree (master is root):
    // 0: master
    // 1:   branch_1
    // 2:   branch_2
    // 3:   branch_3
    // 4:   branch_4
    // 5:   stack_root
    // 6:     stack_child_1
    // 7:       stack_child_2
    // 8:         stack_child_3
    // 9:   z_branch

    // Navigate to stack_root (index 5)
    for _ in 0..5 {
        state.update(Msg::KeyPressed(KeyEvent::new(
            KeyCode::Char('j'),
            KeyModifiers::NONE,
        )));
    }

    // Grab stack_root
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));

    // Move Up 5 steps to master (index 0)
    for _ in 0..5 {
        state.update(Msg::KeyPressed(KeyEvent::new(
            KeyCode::Char('k'),
            KeyModifiers::NONE,
        )));
    }

    // Release (place back into master)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));

    // After release, the drag state (target_parent) is cleared.
    // Verify the move was successful by checking the branch's effective parent.
    assert_eq!(
        state.get_effective_parent("stack_root"),
        Some(master.clone()),
        "stack_root should have master as parent"
    );

    // Verify UI with Snapshot
    // Increase height to see all lines + borders
    let backend = TestBackend::new(50, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();

    let output = buffer_to_string(terminal.backend().buffer());

    let settings = configure_insta();
    settings.bind(|| {
        assert_snapshot!(output);
    });
}

#[tokio::test]
async fn test_tui_subtree_movement_in_progress() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();
    let initial_commit = repo.head().unwrap().peel_to_commit().unwrap();

    // Create 4 children of master
    for i in 1..=4 {
        // master is at initial_commit
        repo.set_head_detached(initial_commit.id()).unwrap();
        create_commit(&repo, "f.txt", "c", &format!("commit {}", i));
        repo.branch(
            &format!("branch_{}", i),
            &repo.head().unwrap().peel_to_commit().unwrap(),
            false,
        )
        .unwrap();
    }

    // Create Stacked Subtree (also child of master)
    // stack_root
    repo.set_head_detached(initial_commit.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit stack root");
    let root_oid = repo.head().unwrap().peel_to_commit().unwrap();
    repo.branch("stack_root", &root_oid, false).unwrap();

    // stack_child_1
    repo.set_head_detached(root_oid.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit stack child 1");
    let c1_oid = repo.head().unwrap().peel_to_commit().unwrap();
    repo.branch("stack_child_1", &c1_oid, false).unwrap();

    // stack_child_2
    repo.set_head_detached(c1_oid.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit stack child 2");
    let c2_oid = repo.head().unwrap().peel_to_commit().unwrap();
    repo.branch("stack_child_2", &c2_oid, false).unwrap();

    // stack_child_3
    repo.set_head_detached(c2_oid.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit stack child 3");
    repo.branch(
        "stack_child_3",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // Create z_branch (child of master, should appear after stack_root)
    repo.set_head_detached(initial_commit.id()).unwrap();
    create_commit(&repo, "f.txt", "c", "commit z");
    repo.branch(
        "z_branch",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // Switch to master to view the tree from root
    run_git(path_str, &["checkout", &master]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // Initial Tree (master is root):
    // 0: master
    // 1:   branch_1
    // 2:   branch_2
    // 3:   branch_3
    // 4:   branch_4
    // 5:   stack_root
    // 6:     stack_child_1
    // 7:       stack_child_2
    // 8:         stack_child_3
    // 9:   z_branch

    // Navigate to stack_root (index 5)
    for _ in 0..5 {
        state.update(Msg::KeyPressed(KeyEvent::new(
            KeyCode::Char('j'),
            KeyModifiers::NONE,
        )));
    }

    // Grab stack_root
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));

    // Move Up 5 steps to master (index 0)
    for _ in 0..5 {
        state.update(Msg::KeyPressed(KeyEvent::new(
            KeyCode::Char('k'),
            KeyModifiers::NONE,
        )));
    }

    // Move Down 2 steps (to branch_2)
    for _ in 0..2 {
        state.update(Msg::KeyPressed(KeyEvent::new(
            KeyCode::Char('j'),
            KeyModifiers::NONE,
        )));
    }

    // Verify UI with Snapshot while still grabbed
    let backend = TestBackend::new(50, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();

    let output = buffer_to_string(terminal.backend().buffer());

    let settings = configure_insta();
    settings.bind(|| {
        assert_snapshot!(output);
    });
}

#[tokio::test]
async fn test_tui_converge_heuristic_sync() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // Detach HEAD so master doesn't move
    let master_oid = repo.head().unwrap().target().unwrap();
    repo.set_head_detached(master_oid).unwrap();

    // 1. master (C0) -> fake-net (C1)
    let oid1 = create_commit(&repo, "f.txt", "c", "fake-net");
    repo.branch("fake-net", &repo.find_commit(oid1).unwrap(), false)
        .unwrap();

    // 2. fake-net (C1) -> benchmarks (C2)
    repo.set_head_detached(oid1).unwrap();
    let oid2 = create_commit(&repo, "f.txt", "c", "benchmarks");
    repo.branch("benchmarks", &repo.find_commit(oid2).unwrap(), false)
        .unwrap();

    // 3. Reset fake-net back to master (C0)
    run_git(path_str, &["checkout", "fake-net"]);
    run_git(path_str, &["reset", "--hard", &master_oid.to_string()]);
    // Also amend it so its summary matches "fake-net" (heuristic detection)
    run_git(
        path_str,
        &[
            "commit",
            "--allow-empty",
            "--amend",
            "-m",
            "fake-net",
            "--no-edit",
        ],
    );

    run_git(path_str, &["checkout", &master]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // Verify benchmarks is initially under master because it's diverged from fake-net OID
    assert_eq!(
        state.get_effective_parent("benchmarks").as_deref(),
        Some(master.as_str()),
        "benchmarks should have fallen back to master initially because it's diverged"
    );

    // Verify heuristic parent of benchmarks is 'fake-net'
    // (matched summary of C1 in history with new fake-net tip)
    let benchmarks_info = state
        .branches
        .iter()
        .find(|b| b.name == "benchmarks")
        .unwrap();
    assert_eq!(
        benchmarks_info.heuristic_parent.as_deref(),
        Some("fake-net")
    );
    assert!(benchmarks_info.heuristic_upstream_oid.is_some());
    assert_eq!(
        benchmarks_info.heuristic_upstream_oid.unwrap(),
        oid1,
        "Heuristic upstream should be the old C1"
    );

    // 4. Press 'u' on benchmarks.
    let b_pos = state
        .flattened_tree
        .iter()
        .position(|(n, _)| n == "benchmarks")
        .unwrap();
    state.list_state.select(Some(b_pos));
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('u'),
        KeyModifiers::NONE,
    )));

    // 5. Verify plan
    let snapshot = gitui::engine::RepositorySnapshot {
        branches: state.branches.clone(),
        history: state.history.clone(),
        is_dirty: state.is_dirty,
    };
    let plan = calculate_plan(&snapshot, &state.intents).unwrap();

    assert!(
        !plan.is_empty(),
        "Plan should NOT be empty when converging even if parent_changed is false, if heuristic upstream differs"
    );

    match &plan[0] {
        gitui::engine::Operation::Rebase {
            branch,
            onto,
            upstream,
            ..
        } => {
            assert_eq!(branch, "benchmarks");
            assert_eq!(onto, "fake-net");
            assert_eq!(
                upstream.as_ref().unwrap(),
                &oid1.to_string(),
                "Should rebase FROM the old C1 OID"
            );
        }
        _ => panic!("Expected rebase operation"),
    }

    // 6. Render and verify [REBASE] label is present
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();

    let buffer = terminal.backend().buffer();
    let mut found_benchmarks_with_rebase = false;

    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("benchmarks") && line.contains("[REBASE]") {
            found_benchmarks_with_rebase = true;
        }
    }

    assert!(
        found_benchmarks_with_rebase,
        "benchmarks should show [REBASE] after sync"
    );
}

#[tokio::test]
async fn test_tui_converge_toggle() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    let master_oid = repo.head().unwrap().target().unwrap();

    // 1. master (C0) -> feat (C1)
    let oid1 = create_commit(&repo, "f.txt", "c", "feat");
    repo.branch("feat", &repo.find_commit(oid1).unwrap(), false)
        .unwrap();

    // 2. feat (C1) -> subfeat (C2)
    repo.set_head_detached(oid1).unwrap();
    let oid2 = create_commit(&repo, "f.txt", "c", "subfeat");
    repo.branch("subfeat", &repo.find_commit(oid2).unwrap(), false)
        .unwrap();

    // Move feat to be sibling of master (C0)
    run_git(path_str, &["checkout", "feat"]);
    run_git(path_str, &["reset", "--hard", &master_oid.to_string()]);
    run_git(path_str, &["commit", "--allow-empty", "-m", "feat"]);

    // Move main back to C0 so C1 is not a branch head
    run_git(path_str, &["checkout", &master]);
    run_git(path_str, &["reset", "--hard", &master_oid.to_string()]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // subfeat should be under master initially because it's diverged
    assert_eq!(
        state.get_effective_parent("subfeat").as_deref(),
        Some(master.as_str())
    );

    let subfeat_pos = state
        .flattened_tree
        .iter()
        .position(|(n, _)| n == "subfeat")
        .unwrap();
    state.list_state.select(Some(subfeat_pos));

    // 1. Press 'u' -> should converge to 'feat'
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('u'),
        KeyModifiers::NONE,
    )));
    assert_eq!(
        state.get_effective_parent("subfeat").as_deref(),
        Some("feat")
    );

    // 2. Press 'u' again -> should undo (back to master)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('u'),
        KeyModifiers::NONE,
    )));
    assert_eq!(
        state.get_effective_parent("subfeat").as_deref(),
        Some(master.as_str())
    );
}

#[tokio::test]
async fn test_tui_intent_toggles() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup a branch that is ready for everything
    // Start from detached HEAD so master doesn't move
    let master_oid = repo.head().unwrap().target().unwrap();
    repo.set_head_detached(master_oid).unwrap();

    // Create feat commit
    let feat_oid1 = create_commit(&repo, "f.txt", "c", "feat commit");
    repo.branch("feat", &repo.find_commit(feat_oid1).unwrap(), false)
        .unwrap();

    // Create a bare remote and push feat to it
    let remote_dir = tempfile::tempdir().unwrap();
    let remote_path = remote_dir.path().to_str().unwrap();
    run_git(remote_path, &["init", "--bare"]);
    run_git(path_str, &["remote", "add", "origin", remote_path]);
    run_git(path_str, &["push", "origin", "feat:feat"]);
    run_git(
        path_str,
        &["branch", "--set-upstream-to=origin/feat", "feat"],
    );
    run_git(path_str, &["fetch", "origin"]);

    // Create a remote-only branch
    run_git(path_str, &["checkout", "-b", "remote-only"]);
    create_commit(&repo, "remote.txt", "c", "remote only commit");
    run_git(path_str, &["push", "origin", "remote-only:remote-only"]);
    run_git(path_str, &["checkout", "feat"]);
    run_git(path_str, &["branch", "-D", "remote-only"]);
    run_git(path_str, &["fetch", "origin"]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: "feat".to_string(),
        show_remote: true,
        ..Default::default()
    };
    state.refresh_tree(None);

    let feat_pos = state
        .flattened_tree
        .iter()
        .position(|(n, _)| n == "feat")
        .unwrap();
    state.list_state.select(Some(feat_pos));

    // 'p': pending_push
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('p'),
        KeyModifiers::NONE,
    )));
    assert!(state.get_intent("feat").pending_push);
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('p'),
        KeyModifiers::NONE,
    )));
    assert!(!state.get_intent("feat").pending_push);

    // 's': pending_submit (feat should be ready to submit since it's local and ahead of master, and in sync with origin/feat)
    // Add another commit and push it to origin/feat to keep it in sync
    run_git(path_str, &["checkout", "feat"]);
    create_commit(&repo, "f2.txt", "c", "feat ahead of master");
    run_git(path_str, &["push", "origin", "feat:feat"]);
    let (branches, history) = git.get_branches(None).unwrap();
    state.branches = branches;
    state.history = history;
    state.refresh_tree(None);

    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('s'),
        KeyModifiers::NONE,
    )));
    assert!(state.get_intent("feat").pending_submit);
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('s'),
        KeyModifiers::NONE,
    )));
    assert!(!state.get_intent("feat").pending_submit);

    // 'd': pending_delete
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('d'),
        KeyModifiers::NONE,
    )));
    assert!(state.get_intent("feat").pending_delete);
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('d'),
        KeyModifiers::NONE,
    )));
    assert!(!state.get_intent("feat").pending_delete);

    // 'r': pending_reset
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('r'),
        KeyModifiers::NONE,
    )));
    assert!(state.get_intent("feat").pending_reset);
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('r'),
        KeyModifiers::NONE,
    )));
    assert!(!state.get_intent("feat").pending_reset);

    // 'f': pending_localize (on a remote branch)
    let remote_only_pos = state
        .flattened_tree
        .iter()
        .position(|(n, _)| n == "origin/remote-only")
        .unwrap();
    state.list_state.select(Some(remote_only_pos));
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('f'),
        KeyModifiers::NONE,
    )));
    assert!(state.get_intent("origin/remote-only").pending_localize);
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('f'),
        KeyModifiers::NONE,
    )));
    assert!(!state.get_intent("origin/remote-only").pending_localize);

    // 'm': pending_amend (on initial branch)
    state.initial_branch = "feat".to_string();
    let feat_pos = state
        .flattened_tree
        .iter()
        .position(|(n, _)| n == "feat")
        .unwrap();
    state.list_state.select(Some(feat_pos));
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('m'),
        KeyModifiers::NONE,
    )));
    assert!(state.get_intent("feat").pending_amend);
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('m'),
        KeyModifiers::NONE,
    )));
    assert!(!state.get_intent("feat").pending_amend);
}

#[tokio::test]
async fn test_tui_convergence_full_cycle() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    let master_oid = repo.head().unwrap().target().unwrap();

    // 1. master (C0) -> feat (C1)
    let oid1 = create_commit(&repo, "f.txt", "c", "feat");
    repo.branch("feat", &repo.find_commit(oid1).unwrap(), false)
        .unwrap();

    // 2. feat (C1) -> subfeat (C2)
    repo.set_head_detached(oid1).unwrap();
    let oid2 = create_commit(&repo, "f.txt", "c", "subfeat");
    repo.branch("subfeat", &repo.find_commit(oid2).unwrap(), false)
        .unwrap();

    // Move feat to be sibling of master (C0)
    run_git(path_str, &["checkout", "feat"]);
    run_git(path_str, &["reset", "--hard", &master_oid.to_string()]);
    // Use fixed time for the new feat commit as well (matches setup_repo)
    run_git_with_env(
        path_str,
        &["commit", "--allow-empty", "-m", "feat"],
        vec![
            ("GIT_AUTHOR_DATE", "1735686000 +0000"),
            ("GIT_COMMITTER_DATE", "1735686000 +0000"),
        ],
    );

    // Move main back to C0
    run_git(path_str, &["checkout", &master]);
    run_git(path_str, &["reset", "--hard", &master_oid.to_string()]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: master.clone(),
        ..Default::default()
    };
    state.refresh_tree(None);

    // subfeat should be diverged
    let subfeat_pos = state
        .flattened_tree
        .iter()
        .position(|(n, _)| n == "subfeat")
        .unwrap();
    state.list_state.select(Some(subfeat_pos));

    // Verify [DIVERGED] is present, and NO [REBASE]
    {
        let backend = TestBackend::new(80, 20);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();
        let buffer = terminal.backend().buffer();
        let line = (0..80)
            .map(|x| buffer[(x, 1 + subfeat_pos as u16)].symbol())
            .collect::<String>();
        assert!(line.contains("subfeat"));
        assert!(line.contains("[DIVERGED]"));
        assert!(!line.contains("[REBASE]"));
    }

    // 1. Press 'u' -> should converge to 'feat'
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('u'),
        KeyModifiers::NONE,
    )));

    // Verify [DIVERGED] is GONE, and [REBASE] is present
    {
        let backend = TestBackend::new(80, 20);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();
        let buffer = terminal.backend().buffer();
        let line = (0..80)
            .map(|x| buffer[(x, 1 + subfeat_pos as u16)].symbol())
            .collect::<String>();
        assert!(line.contains("subfeat"));
        assert!(!line.contains("[DIVERGED]"));
        assert!(line.contains("[REBASE]"));
    }

    // 2. Simulate "Completion": actually rebase in repo and reload
    // We rebase subfeat onto feat. subfeat summary was "subfeat".
    // feat tip summary is "feat".
    run_git(path_str, &["checkout", "subfeat"]);
    run_git(path_str, &["rebase", "feat"]);
    run_git(path_str, &["checkout", &master]);

    let (branches, history) = git.get_branches(None).unwrap();
    {
        let subfeat = branches.iter().find(|b| b.name == "subfeat").unwrap();
        // Check that subfeat's heuristic parent is updated to point to the NEW feat
        assert_eq!(subfeat.heuristic_parent.as_deref(), Some("feat"));
    }
    state.branches = branches;
    state.history = history;
    state.intents.clear(); // Clear intents as if we just started fresh after execution
    state.refresh_tree(None);

    // Verify UI is clean
    {
        let backend = TestBackend::new(80, 20);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal.draw(|f| ui::draw_main(f, &mut state)).unwrap();
        let buffer = terminal.backend().buffer();
        let subfeat_new_pos = state
            .flattened_tree
            .iter()
            .position(|(n, _)| n == "subfeat")
            .unwrap();
        let line = (0..80)
            .map(|x| buffer[(x, 1 + subfeat_new_pos as u16)].symbol())
            .collect::<String>();
        assert!(line.contains("subfeat"));
        assert!(!line.contains("[DIVERGED]"));
        assert!(!line.contains("[REBASE]"));
    }

    // 3. Press 'v' -> verify no operations
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('v'),
        KeyModifiers::NONE,
    )));
    {
        let backend = TestBackend::new(80, 20);
        let mut terminal = Terminal::new(backend).unwrap();
        terminal
            .draw(|f| {
                let snapshot = gitui::engine::RepositorySnapshot {
                    branches: state.branches.clone(),
                    history: state.history.clone(),
                    is_dirty: state.is_dirty,
                };
                let plan = calculate_plan(&snapshot, &state.intents).unwrap();
                ui::draw_preview(f, &plan, &state);
            })
            .unwrap();
        let buffer = terminal.backend().buffer();
        let mut found_no_ops = false;
        for y in 0..20 {
            let line = (0..80).map(|x| buffer[(x, y)].symbol()).collect::<String>();
            if line.contains("No operations to perform.") {
                found_no_ops = true;
                break;
            }
        }
        assert!(found_no_ops);
    }
}

// end of file
