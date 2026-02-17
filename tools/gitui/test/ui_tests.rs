use git2::Oid;
use gitui::diff_utils::{DiffLine, FileDiff, Hunk, LineType};
use gitui::engine::{BranchInfo, CommitInfo};
use gitui::split_state::{SplitState, SplitViewMode};
use gitui::state::{AppState, SidebarState};
use gitui::ui;
use ratatui::{Terminal, backend::TestBackend};

#[test]
fn test_ui_loading_bar_rendered() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let oid = Oid::from_bytes(&[0; 20]).unwrap();
    let mut state = AppState {
        is_loading: true,
        progress_message: "Testing Progress".to_string(),
        progress_percentage: 0.5,
        branches: vec![BranchInfo {
            name: "master".to_string(),
            oid,
            ..Default::default()
        }],
        ..AppState::default()
    };

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();

    let mut found_msg = false;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("Testing Progress") {
            found_msg = true;
            break;
        }
    }
    assert!(found_msg, "Progress message not found in UI output");
}

#[test]
fn test_ui_no_loading_bar_when_not_loading() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let oid = Oid::from_bytes(&[0; 20]).unwrap();
    let mut state = AppState {
        is_loading: false,
        branches: vec![BranchInfo {
            name: "master".to_string(),
            oid,
            ..Default::default()
        }],
        ..AppState::default()
    };

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();

    let mut found_msg = false;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("Testing Progress") {
            found_msg = true;
            break;
        }
    }
    assert!(
        !found_msg,
        "Progress message found in UI output when not loading"
    );
}

#[test]
fn test_ui_renders_aliases() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let oid = Oid::from_bytes(&[0; 20]).unwrap();
    let mut state = AppState {
        branches: vec![BranchInfo {
            name: "master".to_string(),
            oid,
            aliases: vec!["origin/master".to_string(), "upstream/master".to_string()],
            ..Default::default()
        }],
        flattened_tree: vec![("master".to_string(), 0)],
        ..AppState::default()
    };

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();

    let mut found_alias = false;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("master") && line.contains("origin/master, upstream/master") {
            found_alias = true;
            break;
        }
    }
    assert!(found_alias, "Branch aliases not found in UI output");
}

#[test]
fn test_ui_renders_commit_sidebar() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let oid = Oid::from_bytes(&[0; 20]).unwrap();
    let mut state = AppState {
        sidebar: SidebarState::Ready {
            branch: "master".to_string(),
            commits: vec![CommitInfo {
                id: "abc1234".to_string(),
                summary: "Fix bug".to_string(),
                author: "Alice".to_string(),
            }],
        },
        flattened_tree: vec![("master".to_string(), 0)],
        branches: vec![BranchInfo {
            name: "master".to_string(),
            oid,
            ..Default::default()
        }],
        ..AppState::default()
    };

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let mut found_commit_id = false;
    let mut found_summary = false;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("abc1234") {
            found_commit_id = true;
        }
        if line.contains("Fix bug") {
            found_summary = true;
        }
    }
    assert!(found_commit_id, "Commit ID not found in sidebar");
    assert!(found_summary, "Commit summary not found in sidebar");
}

#[test]
fn test_ui_renders_aligned_aliases() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let oid = Oid::from_bytes(&[0; 20]).unwrap();
    let mut state = AppState {
        branches: vec![
            BranchInfo {
                name: "short".to_string(),
                oid,
                aliases: vec!["origin/short".to_string()],
                ..Default::default()
            },
            BranchInfo {
                name: "much-longer-name".to_string(),
                oid,
                ..Default::default()
            },
        ],
        flattened_tree: vec![
            ("short".to_string(), 0),
            ("much-longer-name".to_string(), 1),
        ],
        ..AppState::default()
    };

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();

    let mut alias_offset = None;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("short") && line.contains("origin/short") {
            alias_offset = Some(line.find("origin/short").unwrap());
            break;
        }
    }

    assert!(alias_offset.is_some(), "Alias not found");
    // "much-longer-name" depth 1 -> len 16 + 2 = 18.
    // "short" depth 0 -> len 5.
    // max_name_len = 18.
    // Padding = 18 - 5 + 2 = 15.
    // Offset within the item is 20.
    // ratatui List with Borders::ALL and highlight_symbol(">>") adds 5 chars of prefix (1 for border, 2 for symbol, 2 for padding).
    assert_eq!(alias_offset.unwrap(), 25, "Alias not properly aligned");
}

#[test]
fn test_ui_renders_multiline_commits() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState {
        sidebar: SidebarState::Ready {
            branch: "master".to_string(),
            commits: vec![CommitInfo {
                id: "abc1234".to_string(),
                summary: "Multiline Summary".to_string(),
                author: "Alice".to_string(),
            }],
        },
        flattened_tree: vec![("master".to_string(), 0)],
        branches: vec![BranchInfo {
            name: "master".to_string(),
            oid: Oid::from_bytes(&[0; 20]).unwrap(),
            ..Default::default()
        }],
        ..AppState::default()
    };

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();

    let mut found_header_y = None;
    let mut found_summary_y = None;

    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("abc1234") && line.contains("Alice") {
            found_header_y = Some(y);
        }
        if line.contains("Multiline Summary") {
            found_summary_y = Some(y);
        }
    }

    assert!(found_header_y.is_some(), "Commit header not found");
    assert!(found_summary_y.is_some(), "Commit summary not found");
    assert_eq!(
        found_summary_y.unwrap(),
        found_header_y.unwrap() + 1,
        "Summary should be on the line after the header"
    );
}

#[test]
fn test_ui_renders_aligned_aliases_with_localize() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let oid = Oid::from_bytes(&[0; 20]).unwrap();
    let mut state = AppState {
        branches: vec![
            BranchInfo {
                name: "origin/short".to_string(),
                oid,
                aliases: vec!["alias".to_string()],
                ..Default::default()
            },
            BranchInfo {
                name: "long-name".to_string(),
                oid,
                ..Default::default()
            },
        ],
        flattened_tree: vec![
            ("origin/short".to_string(), 0),
            ("long-name".to_string(), 0),
        ],
        ..AppState::default()
    };
    state.mutate_intent("origin/short", |i| {
        i.pending_localize = true;
    });

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();

    let mut alias_offset = None;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("short") && line.contains("alias") && !line.contains("origin/short") {
            alias_offset = Some(line.find("alias").unwrap());
            break;
        }
    }

    assert!(alias_offset.is_some(), "Alias not found");
    // "long-name" len 9.
    // "origin/short" -> "short" len 5.
    // max_name_len = 9.
    // Padding = 9 - 5 + 2 = 6.
    // Offset within item = 11.
    // Offset in line = 5 (prefix) + 11 = 16.
    assert_eq!(
        alias_offset.unwrap(),
        16,
        "Alias not properly aligned with localization"
    );
}

#[test]
fn test_ui_renders_sidebar_spinner() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let oid = Oid::from_bytes(&[0; 20]).unwrap();
    let mut state = AppState {
        sidebar: SidebarState::Loading {
            branch: "master".to_string(),
        },
        spinner_index: 0,
        flattened_tree: vec![("master".to_string(), 0)],
        branches: vec![BranchInfo {
            name: "master".to_string(),
            oid,
            ..Default::default()
        }],
        ..AppState::default()
    };

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let spinner_char = ui::SPINNERS[0];
    let mut found_spinner = false;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("Recent Commits") && line.contains(spinner_char) {
            found_spinner = true;
            break;
        }
    }
    assert!(found_spinner, "Sidebar spinner not found while loading");
}

#[test]
fn test_ui_renders_split_view() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let file = FileDiff {
        path: "test.txt".to_string(),
        hunks: vec![Hunk {
            header: "@@ -1,1 +1,1 @@".to_string(),
            lines: vec![DiffLine {
                content: "line1\n".to_string(),
                line_type: LineType::Addition,
                old_lineno: None,
                new_lineno: Some(1),
            }],
            old_start: 1,
            old_lines: 0,
            new_start: 1,
            new_lines: 1,
        }],
    };

    let mut split_state = SplitState::new(vec![file]);
    split_state.mode = SplitViewMode::Hunks;
    split_state.rebuild_view();
    split_state
        .current_selection
        .insert(("test.txt".to_string(), 0));
    state.split_state = Some(split_state);

    terminal
        .draw(|f| {
            ui::draw_split_view(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let mut found_file = false;
    let mut found_selected_hunk = false;

    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("test.txt") {
            found_file = true;
        }
        if line.contains("[x]") && line.contains("@@ -1,1 +1,1 @@") {
            found_selected_hunk = true;
        }
    }

    assert!(found_file, "File name not found in split view");
    assert!(
        found_selected_hunk,
        "Selected hunk [x] not found in split view"
    );
}

#[test]
fn test_ui_renders_split_mode_loading() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState {
        mode: gitui::state::AppMode::Split,
        split_state: None, // Still loading
        is_loading: true,
        progress_message: "Fetching diff".to_string(),
        progress_percentage: 0.4,
        spinner_index: 0,
        ..AppState::default()
    };

    terminal
        .draw(|f| {
            ui::draw_split_view(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let mut found_progress = false;

    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("Fetching diff") {
            found_progress = true;
            break;
        }
    }

    assert!(
        found_progress,
        "Progress message not found in Split Mode loading view"
    );
}

#[test]
fn test_ui_renders_tree_scrollbar() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let oid = git2::Oid::from_bytes(&[0; 20]).unwrap();
    // Add many branches to ensure scrollbar would be useful (though it should render anyway)
    for i in 0..30 {
        let name = format!("branch-{}", i);
        state.branches.push(gitui::engine::BranchInfo {
            name: name.clone(),
            oid,
            ..Default::default()
        });
        state.flattened_tree.push((name, 0));
    }
    state.list_state.select(Some(5));

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let mut found_scrollbar = false;

    // ScrollbarOrientation::VerticalRight with borders renders on the right edge.
    // The main layout partition is 60% of 80, resulting in a width of 48 characters.
    // We scan the buffer to verify the scrollbar symbols ("↑" and "↓") are present.
    // main_layout[0].inner(...) with horizontal: 0 keeps width 48.
    // Scrollbar symbols are "↑" and "↓".

    for y in 0..buffer.area.height {
        for x in 0..buffer.area.width {
            let symbol = buffer[(x, y)].symbol();
            if symbol == "↑" || symbol == "↓" {
                found_scrollbar = true;
                break;
            }
        }
    }

    assert!(
        found_scrollbar,
        "Scrollbar symbols (↑ or ↓) not found in tree view"
    );
}

#[test]
fn test_ui_shows_push_help_when_pending_amend() {
    let backend = TestBackend::new(100, 20);
    let mut terminal = Terminal::new(backend).unwrap();

    let oid = Oid::from_bytes(&[0; 20]).unwrap();
    let mut state = AppState {
        initial_branch: "feat".to_string(),
        branches: vec![BranchInfo {
            name: "feat".to_string(),
            oid,
            is_local: true,
            ahead: 0,
            ..Default::default()
        }],
        flattened_tree: vec![("feat".to_string(), 0)],
        ..AppState::default()
    };
    state.mutate_intent("feat", |i| {
        i.pending_amend = true;
    });
    state.list_state.select(Some(0));

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let mut found_push_help = false;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("p: push") {
            found_push_help = true;
            break;
        }
    }
    assert!(
        found_push_help,
        "Push help should be shown when pending_amend is true"
    );
}

#[test]
fn test_ui_shows_push_help_when_dirty_on_initial_branch() {
    let backend = TestBackend::new(100, 20);
    let mut terminal = Terminal::new(backend).unwrap();

    let oid = Oid::from_bytes(&[0; 20]).unwrap();
    let mut state = AppState {
        initial_branch: "feat".to_string(),
        is_dirty: true,
        branches: vec![BranchInfo {
            name: "feat".to_string(),
            oid,
            is_local: true,
            ahead: 0,
            ..Default::default()
        }],
        flattened_tree: vec![("feat".to_string(), 0)],
        ..AppState::default()
    };
    state.list_state.select(Some(0));

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    let mut found_push_help = false;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("p: push") {
            found_push_help = true;
            break;
        }
    }
    assert!(
        found_push_help,
        "Push help should be shown when is_dirty is true on initial branch"
    );
}

#[test]
fn test_ui_no_tree_scrollbar_when_items_fit() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let oid = git2::Oid::from_bytes(&[0; 20]).unwrap();
    // 5 branches fit in height 20 (viewport height is roughly 17 lines)
    for i in 0..5 {
        let name = format!("branch-{}", i);
        state.branches.push(BranchInfo {
            name: name.clone(),
            oid,
            ..Default::default()
        });
        state.flattened_tree.push((name, 0));
    }

    terminal
        .draw(|f| {
            ui::draw_main(f, &mut state);
        })
        .unwrap();

    let buffer = terminal.backend().buffer();
    for y in 0..buffer.area.height {
        for x in 0..buffer.area.width {
            let symbol = buffer[(x, y)].symbol();
            assert!(
                symbol != "↑" && symbol != "↓",
                "Scrollbar symbols found when items should fit at ({}, {})",
                x,
                y
            );
        }
    }
}

// end of file
