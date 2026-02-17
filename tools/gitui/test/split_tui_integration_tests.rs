use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use git2::{Signature, Time};
use gitui::engine::{Git, RealGit};
use gitui::split_state::SplitViewMode;
use gitui::state::{AppMode, AppState, Msg};
use gitui::testing::setup_repo;
use gitui::ui;
use ratatui::{Terminal, backend::TestBackend};

#[tokio::test]
async fn test_tui_split_branch_workflow() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create a 1-commit branch that modifies two files
    let path_a = dir.path().join("a.txt");
    std::fs::write(&path_a, "original a\n").unwrap();
    let path_b = dir.path().join("b.txt");
    std::fs::write(&path_b, "original b\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("a.txt")).unwrap();
        index.add_path(std::path::Path::new("b.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Base commit", &tree, &[&parent])
            .unwrap();
    }

    // Now create the "feat" branch with changes to both
    std::fs::write(&path_a, "modified a\n").unwrap();
    std::fs::write(&path_b, "modified b\n").unwrap();

    let feat_oid = {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("a.txt")).unwrap();
        index.add_path(std::path::Path::new("b.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686001, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Giant commit", &tree, &[&parent])
            .unwrap()
    };

    let giant_commit = repo.find_commit(feat_oid).unwrap();
    let base_commit = giant_commit.parents().next().unwrap();

    repo.set_head_detached(feat_oid).unwrap();
    repo.branch("feat", &giant_commit, true).unwrap();
    repo.branch(&master, &base_commit, true).unwrap();

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        initial_branch: "feat".to_string(),
        ..AppState::default()
    };
    state.refresh_tree(None);

    // Ensure "feat" is selected
    if let Some(idx) = state.flattened_tree.iter().position(|(n, _)| n == "feat") {
        state.list_state.select(Some(idx));
    } else {
        panic!("feat branch not found in flattened tree");
    }

    // 2. Press 'x' to trigger split
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('x'),
        KeyModifiers::NONE,
    )));
    assert_eq!(state.mode, AppMode::Split);

    // 3. Manually load diff
    let diff = git.get_diff("feat", &master).unwrap();
    state.update(Msg::DiffLoaded("feat".to_string(), Ok(diff)));

    {
        let split_state = state.split_state.as_ref().unwrap();
        assert_eq!(split_state.files.len(), 2);
        assert_eq!(split_state.mode, SplitViewMode::Files);
    }

    // 4. Select only first file
    // feat branch modifies a.txt and b.txt.
    // By default, focus is on first file (a.txt).
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char(' '),
        KeyModifiers::NONE,
    )));

    {
        let split_state = state.split_state.as_ref().unwrap();
        assert_eq!(split_state.current_selection.len(), 1);
        let selected = split_state.current_selection.iter().next().unwrap();
        assert_eq!(selected.0, "a.txt");
    }

    // 5. Press 'Enter' to trigger name prompt (since we have a selection)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Enter,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.mode, AppMode::Prompt);
    assert_eq!(state.prompt.as_ref().unwrap().value, "feat-1");

    // Verify it's rendered on screen
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal.draw(|f| ui::draw_prompt(f, &state)).unwrap();
    let buffer = terminal.backend().buffer();

    let mut found_default_name = false;
    for y in 0..20 {
        let mut line = String::new();
        for x in 0..80 {
            line.push_str(buffer[(x, y)].symbol());
        }
        if line.contains("feat-1") {
            found_default_name = true;
            break;
        }
    }
    assert!(
        found_default_name,
        "Default branch name 'feat-1' not found on screen"
    );

    // 6. Confirm name -> message prompt
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Enter,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.mode, AppMode::Prompt);
    assert!(
        state
            .prompt
            .as_ref()
            .unwrap()
            .title
            .contains("Commit Message")
    );

    // 7. Confirm message -> back to split view
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Enter,
        KeyModifiers::CONTROL,
    )));
    assert_eq!(state.mode, AppMode::Split);
    {
        let split_state = state.split_state.as_ref().unwrap();
        assert_eq!(split_state.parts.len(), 1);
        assert_eq!(split_state.parts[0].name, "feat-1");
    }

    // 8. Confirm all splits
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Enter,
        KeyModifiers::NONE,
    )));

    // 9. Verify final state
    assert_eq!(state.mode, AppMode::Tree);

    // Verify virtual layer
    assert!(state.virtual_layer.is_hidden("feat"));
    assert!(state.virtual_layer.is_virtual("feat-1"));
    assert!(state.virtual_layer.is_virtual("feat"));

    // Verify tree structure
    state.refresh_tree(None);

    // Test rendering
    let backend = ratatui::backend::TestBackend::new(80, 20);
    let mut terminal = ratatui::Terminal::new(backend).unwrap();
    terminal
        .draw(|f| gitui::ui::draw_main(f, &mut state))
        .unwrap();

    // Tree should now be master -> feat-1 -> feat
    let names: Vec<String> = state
        .flattened_tree
        .iter()
        .map(|(n, _)| n.clone())
        .collect();
    assert!(names.contains(&master));
    assert!(names.contains(&"feat-1".to_string()));
    assert!(names.contains(&"feat".to_string()));

    let master_idx = names.iter().position(|n| n == &master).unwrap();
    let feat1_idx = names.iter().position(|n| n == "feat-1").unwrap();
    let feat_idx = names.iter().position(|n| n == "feat").unwrap();

    assert!(feat1_idx > master_idx);
    assert!(feat_idx > feat1_idx);

    let depths: Vec<usize> = state.flattened_tree.iter().map(|(_, d)| *d).collect();
    assert_eq!(depths[feat1_idx], depths[master_idx] + 1);
    assert_eq!(depths[feat_idx], depths[feat1_idx] + 1);
}
