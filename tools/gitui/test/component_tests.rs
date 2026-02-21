use gitui::engine::BranchInfo;
use gitui::state::AppState;
use gitui::testing::mock_oid;
use gitui::ui::main_view::render_branch_row;
use ratatui::Terminal;
use ratatui::backend::TestBackend;
use ratatui::widgets::List;

#[test]
fn test_render_branch_row_diverged() {
    let state = AppState::default();
    let name = "my-branch";
    let oid = mock_oid(1);

    let info = BranchInfo {
        name: name.to_string(),
        oid,
        original_parent: Some("master".to_string()),
        heuristic_parent: Some("other".to_string()), // DIVERGED!
        ..Default::default()
    };

    let item = render_branch_row(name, 1, &info, &state, 20);

    let backend = TestBackend::new(80, 1);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal
        .draw(|f| {
            let list = List::new(vec![item]);
            f.render_widget(list, f.area());
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let mut line = String::new();
    for x in 0..80 {
        line.push_str(buffer[(x, 0)].symbol());
    }

    assert!(line.contains("my-branch"));
    assert!(line.contains("[DIVERGED]"));
}

#[test]
fn test_render_branch_row_reparented_conflict() {
    let mut state = AppState::default();
    let name = "my-branch";
    let oid = mock_oid(1);

    let info = BranchInfo {
        name: name.to_string(),
        oid,
        original_parent: Some("master".to_string()),
        ..Default::default()
    };

    // Set intent to move it to "new-parent" and mark as conflict
    state.mutate_intent(name, |i| {
        i.parent = Some(Some("new-parent".to_string()));
        i.has_conflict = true;
    });

    let item = render_branch_row(name, 0, &info, &state, 20);

    let backend = TestBackend::new(80, 1);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal
        .draw(|f| {
            let list = List::new(vec![item]);
            f.render_widget(list, f.area());
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let mut line = String::new();
    for x in 0..80 {
        line.push_str(buffer[(x, 0)].symbol());
    }

    assert!(line.contains("my-branch"));
    assert!(line.contains("[REBASE]"));
    assert!(line.contains("(CONFLICT)"));
}

#[test]
fn test_render_branch_row_localized() {
    let mut state = AppState::default();
    let name = "origin/my-branch";
    let oid = mock_oid(1);

    let info = BranchInfo {
        name: name.to_string(),
        oid,
        is_remote: true,
        ..Default::default()
    };

    state.mutate_intent(name, |i| {
        i.pending_localize = true;
    });

    let item = render_branch_row(name, 0, &info, &state, 20);

    let backend = TestBackend::new(80, 1);
    let mut terminal = Terminal::new(backend).unwrap();
    terminal
        .draw(|f| {
            let list = List::new(vec![item]);
            f.render_widget(list, f.area());
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let mut line = String::new();
    for x in 0..80 {
        line.push_str(buffer[(x, 0)].symbol());
    }

    // Should show localized name (without origin/)
    assert!(line.contains("my-branch"));
    assert!(!line.contains("origin/my-branch"));
    assert!(line.contains("[LOCALIZE]"));
}
