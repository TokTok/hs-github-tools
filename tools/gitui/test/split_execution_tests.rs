use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use gitui::engine::{Git, Operation, RealGit};
use gitui::state::{AppMode, AppState, Effect, Msg};
use gitui::testing::{create_commit, run_git, setup_repo};

#[tokio::test]
async fn test_split_execution_clears_plan() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create a branch with 2 files to split, and a child branch
    run_git(path_str, &["checkout", "-b", "feat"]);
    let path_a = dir.path().join("a.txt");
    let path_b = dir.path().join("b.txt");
    std::fs::write(&path_a, "change a\n").unwrap();
    std::fs::write(&path_b, "change b\n").unwrap();
    run_git(path_str, &["add", "a.txt", "b.txt"]);
    run_git(path_str, &["commit", "-m", "Giant commit"]);

    run_git(path_str, &["checkout", "-b", "child"]);
    create_commit(&repo, "child.txt", "child content", "Child commit");
    run_git(path_str, &["checkout", "feat"]);

    // 2. Initialize TUI state
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        ..AppState::default()
    };
    state.refresh_tree(None);

    // Select "feat"
    if let Some(idx) = state.flattened_tree.iter().position(|(n, _)| n == "feat") {
        state.list_state.select(Some(idx));
    }

    // 3. Trigger split
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('x'),
        KeyModifiers::NONE,
    )));

    // Load diff and select a.txt for feat-1
    let diff = git.get_diff("feat", &master).unwrap();
    state.update(Msg::DiffLoaded("feat".to_string(), Ok(diff)));

    // Select first file
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));

    // Start Split part creation (trigger Name prompt)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Enter,
        KeyModifiers::NONE,
    )));

    // Confirm name (move to Message prompt)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Enter,
        KeyModifiers::NONE,
    )));

    // Confirm message (Ctrl+Enter to return to Split mode)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Enter,
        KeyModifiers::CONTROL,
    )));

    // Now back in Split mode with parts.len() == 1

    // Press Enter again in Split mode to finalize all splits
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Enter,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.mode, AppMode::Tree);

    // 4. Verify plan exists
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('v'),
        KeyModifiers::NONE,
    )));
    let plan = state.plan.as_ref().unwrap();
    assert!(
        !plan.is_empty(),
        "Plan should not be empty before execution"
    );

    // The plan should contain a Split and a Rebase
    assert!(plan.iter().any(|op| matches!(op, Operation::Split { .. })));
    assert!(plan.iter().any(|op| matches!(op, Operation::Rebase { .. })));

    // 5. Simulate "Apply and Quit" (pressing 'c' in preview)
    let effects = state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('c'),
        KeyModifiers::NONE,
    )));

    let mut found_apply = false;
    for effect in effects {
        if let Effect::ApplyAndQuit(branches, intents) = effect {
            found_apply = true;
            // Execute the plan as runtime.rs would
            gitui::execute_plan(&git, &branches, &intents, &state.history, path_str).unwrap();
            state.clear_pending_operations();
        }
    }
    assert!(found_apply);

    // 6. NOW THE CRITICAL PART: After execution, if we "return" to the app (simulated by reloading state),
    // the plan should be empty.

    let (new_branches, new_history) = git.get_branches(None).unwrap();
    let is_dirty = git.is_dirty().unwrap();
    state.update(Msg::BranchesLoaded(Ok((
        new_branches,
        new_history,
        is_dirty,
    ))));

    // 7. Verify topology after reload
    let names: Vec<String> = state
        .flattened_tree
        .iter()
        .map(|(n, _)| n.clone())
        .collect();

    assert!(names.contains(&"feat-1".to_string()));
    assert!(names.contains(&"feat".to_string()));
    assert!(names.contains(&"child".to_string()));

    let master_idx = names.iter().position(|n| n == &master).unwrap();
    let feat1_idx = names.iter().position(|n| n == "feat-1").unwrap();
    let feat_idx = names.iter().position(|n| n == "feat").unwrap();
    let child_idx = names.iter().position(|n| n == "child").unwrap();

    assert!(feat1_idx > master_idx);
    assert!(feat_idx > feat1_idx);
    assert!(child_idx > feat_idx);

    let depths: Vec<usize> = state.flattened_tree.iter().map(|(_, d)| *d).collect();
    assert_eq!(depths[feat1_idx], depths[master_idx] + 1);
    assert_eq!(depths[feat_idx], depths[feat1_idx] + 1);
    assert_eq!(depths[child_idx], depths[feat_idx] + 1);

    // 8. Try to quit - it should NOT ask for confirmation because plan should be empty
    let snapshot = gitui::engine::RepositorySnapshot {
        branches: state.branches.clone(),
        history: state.history.clone(),
        is_dirty: state.is_dirty,
    };
    let plan = gitui::engine::calculate_plan(&snapshot, &state.intents).unwrap();

    let quit_effects = state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('q'),
        KeyModifiers::NONE,
    )));
    assert!(
        quit_effects.iter().any(|e| matches!(e, Effect::Quit)),
        "Should quit directly if plan is empty. Plan: {:?}",
        plan
    );
    assert!(
        !state.show_quit_confirmation,
        "Should not show quit confirmation after execution"
    );
}
