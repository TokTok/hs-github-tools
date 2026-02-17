use gitui::diff_utils::{DiffLine, FileDiff, Hunk, LineType};
use gitui::split_state::{RenderedItem, SplitPart, SplitState, SplitViewMode};
use gitui::state::AppState;
use gitui::ui;
use ratatui::{Terminal, backend::TestBackend};

#[test]
fn test_file_header_hidden_when_all_hunks_taken() {
    let file1 = FileDiff {
        path: "a.txt".to_string(),
        hunks: vec![Hunk {
            header: "h1".to_string(),
            ..Default::default()
        }],
    };
    let file2 = FileDiff {
        path: "b.txt".to_string(),
        hunks: vec![Hunk {
            header: "h2".to_string(),
            ..Default::default()
        }],
    };

    let mut state = SplitState::new(vec![file1, file2]);
    assert_eq!(state.rendered_items.len(), 2); // Two file headers

    // Take the only hunk of file 1
    state.parts.push(SplitPart {
        name: "p1".to_string(),
        commit_message: "m1".to_string(),
        selected_hunks: [("a.txt".to_string(), 0)].into_iter().collect(),
    });
    state.rebuild_view();

    // Now only file 2 should be visible
    assert_eq!(state.rendered_items.len(), 1);
    match &state.rendered_items[0] {
        RenderedItem::FileHeader { file_idx } => assert_eq!(*file_idx, 1),
        _ => panic!("Expected file header for file 2"),
    }
}

#[test]
fn test_split_state_cursor_recovery_when_hunk_taken() {
    let file1 = FileDiff {
        path: "a.txt".to_string(),
        hunks: vec![
            Hunk {
                header: "h1".to_string(),
                ..Default::default()
            },
            Hunk {
                header: "h2".to_string(),
                ..Default::default()
            },
        ],
    };

    let mut state = SplitState::new(vec![file1]);
    state.mode = SplitViewMode::Hunks;
    state.rebuild_view();

    // 1. Focus hunk 2 (second hunk of file 1)
    state.next(); // Focus hunk 1
    state.next(); // Focus hunk 2
    assert_eq!(state.get_focused_hunk_idx(), Some(1));

    // 2. Take hunk 2
    state.parts.push(SplitPart {
        name: "p1".to_string(),
        commit_message: "m1".to_string(),
        selected_hunks: [("a.txt".to_string(), 1)].into_iter().collect(),
    });
    state.rebuild_view();

    // 3. Cursor should move to hunk 1 (since hunk 2 is gone)
    assert_eq!(state.get_focused_hunk_idx(), Some(0));
    assert_eq!(state.mode, SplitViewMode::Hunks);
}

#[test]
fn test_taken_hunks_not_rendered() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let file = FileDiff {
        path: "test.txt".to_string(),
        hunks: vec![
            Hunk {
                header: "hunk1".to_string(),
                ..Default::default()
            },
            Hunk {
                header: "hunk2".to_string(),
                ..Default::default()
            },
        ],
    };

    let mut split_state = SplitState::new(vec![file]);
    split_state.mode = SplitViewMode::Hunks;
    // Mark hunk1 as taken
    split_state.parts.push(SplitPart {
        name: "p1".to_string(),
        commit_message: "m1".to_string(),
        selected_hunks: [("test.txt".to_string(), 0)].into_iter().collect(),
    });

    split_state.mode = SplitViewMode::Hunks;
    split_state.rebuild_view();

    state.split_state = Some(split_state);

    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();

    let mut found_hunk1 = false;
    let mut found_hunk2 = false;
    for y in 0..20 {
        let mut line = String::new();
        for x in 0..80 {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("hunk1") {
            found_hunk1 = true;
        }
        if line.contains("hunk2") {
            found_hunk2 = true;
        }
    }

    assert!(
        !found_hunk1,
        "hunk1 should be hidden because it is already in a part"
    );
    assert!(found_hunk2, "hunk2 should be visible");
}

#[test]
fn test_split_state_navigation() {
    let file1 = FileDiff {
        path: "a.txt".to_string(),
        hunks: vec![
            Hunk {
                header: "h1".to_string(),
                lines: vec![DiffLine {
                    content: "l1\n".to_string(),
                    ..Default::default()
                }],
                ..Default::default()
            },
            Hunk {
                header: "h2".to_string(),
                lines: vec![DiffLine {
                    content: "l2\n".to_string(),
                    ..Default::default()
                }],
                ..Default::default()
            },
        ],
    };
    let file2 = FileDiff {
        path: "b.txt".to_string(),
        hunks: vec![Hunk {
            header: "h3".to_string(),
            lines: vec![DiffLine {
                content: "l3\n".to_string(),
                ..Default::default()
            }],
            ..Default::default()
        }],
    };

    let mut state = SplitState::new(vec![file1, file2]);

    assert_eq!(state.mode, SplitViewMode::Files);
    assert_eq!(state.get_focused_file_idx(), Some(0));

    state.next();
    assert_eq!(state.get_focused_file_idx(), Some(1));

    state.prev();
    assert_eq!(state.get_focused_file_idx(), Some(0));

    // Enter hunks
    state.enter_hunks();
    assert_eq!(state.mode, SplitViewMode::Hunks);
    assert_eq!(state.get_focused_hunk_idx(), Some(0));

    state.next();
    assert_eq!(state.get_focused_hunk_idx(), Some(1));

    state.next(); // Should stop at the end of the current file's hunks
    assert_eq!(state.get_focused_file_idx(), Some(0));
    assert_eq!(state.get_focused_hunk_idx(), Some(1));

    state.prev();
    assert_eq!(state.get_focused_hunk_idx(), Some(0));

    state.prev(); // Should stop at the start of the current file's hunks
    assert_eq!(state.get_focused_file_idx(), Some(0));
    assert_eq!(state.get_focused_hunk_idx(), Some(0));

    state.enter_lines();
    assert_eq!(state.mode, SplitViewMode::Lines);
    state.exit_lines();
    assert_eq!(state.mode, SplitViewMode::Hunks);

    // Exit hunks
    state.exit_hunks();
    assert_eq!(state.mode, SplitViewMode::Files);
}

#[test]
fn test_navigation_bug_exit_hunks() {
    let file0 = FileDiff {
        path: "file0.txt".to_string(),
        hunks: vec![Hunk {
            header: "h0".to_string(),
            ..Default::default()
        }],
    };
    let file1 = FileDiff {
        path: "file1.txt".to_string(),
        hunks: vec![Hunk {
            header: "h1".to_string(),
            ..Default::default()
        }],
    };
    let file2 = FileDiff {
        path: "file2.txt".to_string(),
        hunks: vec![Hunk {
            header: "h2".to_string(),
            ..Default::default()
        }],
    };

    let mut state = SplitState::new(vec![file0, file1, file2]);

    // 1. We are on file0
    assert_eq!(state.get_focused_file_idx(), Some(0));

    // 2. Move to file1
    state.next();
    assert_eq!(state.get_focused_file_idx(), Some(1));

    // 3. Enter hunks of file1
    state.enter_hunks();
    assert_eq!(state.mode, SplitViewMode::Hunks);
    assert_eq!(state.get_focused_file_idx(), Some(1));
    assert_eq!(state.get_focused_hunk_idx(), Some(0));
    // Index in Hunks mode:
    // 0: File0 (Header)
    // 1: File1 (Header)
    // 2: Hunk1 (Header)  <-- we are here
    // 3: File2 (Header)
    assert_eq!(state.selected_view_idx, 2);

    // 4. Exit hunks
    state.exit_hunks();

    // 5. We should be back on File1
    assert_eq!(state.mode, SplitViewMode::Files);
    assert_eq!(
        state.get_focused_file_idx(),
        Some(1),
        "Should be on File1 after exiting hunks"
    );
    assert_eq!(
        state.selected_view_idx, 1,
        "Should be at index 1 (File1) in Files mode"
    );
}

#[test]
fn test_split_state_page_navigation() {
    let mut files = Vec::new();
    for i in 0..50 {
        files.push(FileDiff {
            path: format!("file-{}.txt", i),
            hunks: Vec::new(),
        });
    }

    let mut state = SplitState::new(files);
    assert_eq!(state.get_focused_file_idx(), Some(0));

    state.page_down(15);
    assert_eq!(state.get_focused_file_idx(), Some(15));

    state.page_down(100); // Beyond end
    assert_eq!(state.get_focused_file_idx(), Some(49));

    state.page_up(10);
    assert_eq!(state.get_focused_file_idx(), Some(39));

    state.page_up(100); // Beyond start
    assert_eq!(state.get_focused_file_idx(), Some(0));
}

#[test]
fn test_split_state_line_navigation() {
    let file1 = FileDiff {
        path: "a.txt".to_string(),
        hunks: vec![Hunk {
            header: "h1".to_string(),
            lines: vec![
                DiffLine {
                    content: "l1\n".to_string(),
                    ..Default::default()
                },
                DiffLine {
                    content: "l2\n".to_string(),
                    ..Default::default()
                },
            ],
            ..Default::default()
        }],
    };

    let mut state = SplitState::new(vec![file1]);
    state.enter_hunks();

    assert_eq!(state.get_focused_hunk_idx(), Some(0));
    assert_eq!(state.mode, SplitViewMode::Hunks);
    assert_eq!(state.list_state.selected(), Some(1)); // File header (0), Hunk header (1)

    // In Hunks mode, next should skip lines (but here there's only one hunk)
    state.next();
    assert_eq!(state.get_focused_hunk_idx(), Some(0)); // Still 0 because only one hunk
    assert_eq!(state.mode, SplitViewMode::Hunks);

    // Enter lines mode
    state.enter_lines();
    assert_eq!(state.mode, SplitViewMode::Lines);
    assert_eq!(state.get_focused_line_idx(), Some(0));
    assert_eq!(state.list_state.selected(), Some(2)); // Hunk line 0

    state.next();
    assert_eq!(state.get_focused_line_idx(), Some(1));
    assert_eq!(state.list_state.selected(), Some(3)); // Hunk line 1

    state.next();
    assert_eq!(state.get_focused_line_idx(), Some(1)); // Trapped in hunk

    state.prev();
    assert_eq!(state.get_focused_line_idx(), Some(0));

    state.prev();
    assert_eq!(state.get_focused_line_idx(), None);

    state.prev();
    assert_eq!(state.get_focused_line_idx(), None); // Trapped in hunk (at header level)

    state.exit_lines();
    assert_eq!(state.mode, SplitViewMode::Hunks);
}

#[test]
fn test_hunks_mode_sticky_header() {
    let mut hunks = Vec::new();
    for i in 0..20 {
        hunks.push(Hunk {
            header: format!("hunk-{}", i),
            ..Default::default()
        });
    }
    let file1 = FileDiff {
        path: "file1.txt".to_string(),
        hunks,
    };

    let mut state = SplitState::new(vec![file1]);
    state.enter_hunks();

    // Move to next hunk
    state.next();
    assert_eq!(state.get_focused_hunk_idx(), Some(1));
    // Offset should be the file index (0)
    assert_eq!(state.list_state.offset(), 0);

    // Move down more
    for _ in 0..10 {
        state.next();
    }
    assert_eq!(state.get_focused_hunk_idx(), Some(11));
    assert_eq!(
        state.list_state.offset(),
        0,
        "File header should remain at the top"
    );
}

#[test]
fn test_lines_mode_marker_rendering() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let file = FileDiff {
        path: "test.txt".to_string(),
        hunks: vec![Hunk {
            header: "@@ hunk1 @@".to_string(),
            lines: vec![
                DiffLine {
                    content: "line1\n".to_string(),
                    line_type: LineType::Addition,
                    ..Default::default()
                },
                DiffLine {
                    content: "line2\n".to_string(),
                    line_type: LineType::Addition,
                    ..Default::default()
                },
            ],
            ..Default::default()
        }],
    };

    let mut split_state = SplitState::new(vec![file]);
    split_state.enter_hunks();
    split_state.enter_lines();
    split_state.next(); // Focus second line ("line2")
    state.split_state = Some(split_state);

    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();

    // Search for the marker ">>" and "line2" on the same line
    let mut found_line_marker = false;
    for y in 0..20 {
        let mut line_content = String::new();
        for x in 0..80 {
            line_content.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line_content.contains(">>") && line_content.contains("+line2") {
            found_line_marker = true;
            break;
        }
    }

    assert!(
        found_line_marker,
        "Focus marker '>>' not found on focused line in Lines mode"
    );
}

#[test]
fn test_lines_mode_scrolling_and_marker() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let file = FileDiff {
        path: "test.txt".to_string(),
        hunks: vec![Hunk {
            header: "@@ hunk1 @@".to_string(),
            lines: vec![
                DiffLine {
                    content: "l1\n".to_string(),
                    ..Default::default()
                },
                DiffLine {
                    content: "l2\n".to_string(),
                    ..Default::default()
                },
                DiffLine {
                    content: "l3\n".to_string(),
                    ..Default::default()
                },
            ],
            ..Default::default()
        }],
    };

    let mut split_state = SplitState::new(vec![file]);
    split_state.enter_hunks();
    split_state.enter_lines();

    // Initially on line 1
    state.split_state = Some(split_state);
    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();
    assert!(buffer_contains_line(buffer, ">>", "l1"));

    // Scroll down to line 2
    if let Some(ref mut s) = state.split_state {
        s.next();
    }
    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();
    assert!(buffer_contains_line(buffer, ">>", "l2"));
    assert!(!buffer_contains_line(buffer, ">>", "l1"));

    // Scroll down to line 3
    if let Some(ref mut s) = state.split_state {
        s.next();
    }
    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();
    assert!(buffer_contains_line(buffer, ">>", "l3"));
}

fn buffer_contains_line(buffer: &ratatui::buffer::Buffer, marker: &str, content: &str) -> bool {
    for y in 0..buffer.area.height {
        let mut line_str = String::new();
        for x in 0..buffer.area.width {
            line_str.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line_str.contains(marker) && line_str.contains(content) {
            return true;
        }
    }
    false
}

#[test]
fn test_split_state_selection() {
    let file1 = FileDiff {
        path: "a.txt".to_string(),
        hunks: vec![
            Hunk {
                header: "h1".to_string(),
                ..Default::default()
            },
            Hunk {
                header: "h2".to_string(),
                ..Default::default()
            },
        ],
    };

    let mut state = SplitState::new(vec![file1]);

    // Toggle entire file
    state.toggle_selection();
    assert_eq!(state.current_selection.len(), 2);
    assert!(state.current_selection.contains(&("a.txt".to_string(), 0)));
    assert!(state.current_selection.contains(&("a.txt".to_string(), 1)));

    state.toggle_selection();
    assert_eq!(state.current_selection.len(), 0);

    // Toggle individual hunk
    state.enter_hunks();
    state.toggle_selection();
    assert_eq!(state.current_selection.len(), 1);
    assert!(state.current_selection.contains(&("a.txt".to_string(), 0)));
}

#[test]
fn test_ui_dynamic_help_text() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let file = FileDiff {
        path: "test.txt".to_string(),
        hunks: vec![Hunk {
            header: "h1".to_string(),
            ..Default::default()
        }],
    };

    let split_state = SplitState::new(vec![file]);
    state.split_state = Some(split_state);

    // 1. No selection -> should show "enter: finish"
    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();
    assert!(buffer_contains_line(buffer, "enter: finish", ""));

    // 2. With selection -> should show "enter: new part"
    if let Some(ref mut s) = state.split_state {
        s.toggle_selection();
    }
    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();
    assert!(buffer_contains_line(buffer, "enter: new part", ""));
}

#[test]
fn test_ui_renders_split_view_modes() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let file = FileDiff {
        path: "test.txt".to_string(),
        hunks: vec![Hunk {
            header: "@@ hunk1 @@".to_string(),
            lines: vec![DiffLine {
                content: "line1\n".to_string(),
                line_type: LineType::Addition,
                ..Default::default()
            }],
            ..Default::default()
        }],
    };

    let split_state = SplitState::new(vec![file]);
    state.split_state = Some(split_state);

    // Test Files Mode
    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();
    let mut found_help_files = false;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("->: view hunks") {
            found_help_files = true;
        }
    }
    assert!(found_help_files, "Help text for Files mode not found");

    // Test Hunks Mode
    if let Some(ref mut s) = state.split_state {
        s.enter_hunks();
    }
    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();
    let mut found_help_hunks = false;
    let mut found_hunk_indicator = false;
    for y in 0..buffer.area.height {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("<-: back to files") {
            found_help_hunks = true;
        }
        if line.contains(">>") && line.contains("@@ hunk1 @@") {
            found_hunk_indicator = true;
        }
    }
    assert!(found_help_hunks, "Help text for Hunks mode not found");
    assert!(
        found_hunk_indicator,
        "Hunk focus indicator not found in Hunks mode"
    );
}

#[test]
fn test_split_view_renders_scrollbar() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let mut files = Vec::new();
    for i in 0..30 {
        files.push(FileDiff {
            path: format!("file-{}.txt", i),
            hunks: Vec::new(),
        });
    }

    state.split_state = Some(SplitState::new(files));

    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();
    let mut found_scrollbar = false;

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
        "Scrollbar symbols (↑ or ↓) not found in split view when items should fit"
    );
}

#[test]
fn test_split_view_scrollbar_at_bottom() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let mut files = Vec::new();
    for i in 0..100 {
        files.push(FileDiff {
            path: format!("file-{}.txt", i),
            hunks: Vec::new(),
        });
    }

    let mut split_state = SplitState::new(files);
    // Focus the last file
    for _ in 0..99 {
        split_state.next();
    }
    state.split_state = Some(split_state);

    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();

    // Check if the thumb is at the bottom.
    // The viewport is 20 lines. 3 are used for help and borders of chunks.
    // Chunk[0] height is 17. Inner height is 15.
    // We have 100 items. Offset should be 85.
    // Position 85 in total 100 with viewport 15 should be at the bottom.
    // So symbol '↓' should be at (or near) the bottom of chunk[0].

    let mut found_down_arrow_at_bottom = false;

    for x in 0..buffer.area.width {
        let symbol = buffer[(x, 15)].symbol(); // Last line before bottom border
        if symbol == "↓" {
            found_down_arrow_at_bottom = true;
        }
    }

    assert!(
        found_down_arrow_at_bottom,
        "Scrollbar bottom symbol '↓' not found at expected position"
    );
}

#[test]
fn test_ui_no_split_scrollbar_when_items_fit() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let file = FileDiff {
        path: "test.txt".to_string(),
        hunks: Vec::new(),
    };

    state.split_state = Some(SplitState::new(vec![file]));

    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();

    for y in 0..buffer.area.height {
        for x in 0..buffer.area.width {
            let symbol = buffer[(x, y)].symbol();
            assert!(
                symbol != "↑" && symbol != "↓",
                "Scrollbar symbols found in split view when items should fit"
            );
        }
    }
}

#[test]
fn test_split_view_no_hunks_in_files_mode() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let mut files = Vec::new();
    for i in 0..10 {
        files.push(FileDiff {
            path: format!("file-{:02}.txt", i),
            hunks: vec![Hunk {
                header: format!("hunk-{:02}", i),
                ..Default::default()
            }],
        });
    }

    let mut split_state = SplitState::new(files);
    split_state.mode = SplitViewMode::Files;
    // Focus the first file
    state.split_state = Some(split_state);

    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();

    let mut found_hunk = false;
    for y in 0..20 {
        let mut line = String::new();
        for x in 0..buffer.area.width {
            line.push(buffer[(x, y)].symbol().chars().next().unwrap_or(' '));
        }
        if line.contains("hunk-") {
            found_hunk = true;
            break;
        }
    }

    assert!(!found_hunk, "Hunks should not be shown in Files mode");
}

#[test]
fn test_split_interaction_deep_navigation() {
    let file1 = FileDiff {
        path: "a.txt".to_string(),
        hunks: vec![Hunk {
            header: "h1".to_string(),
            lines: vec![DiffLine {
                content: "l1\n".to_string(),
                ..Default::default()
            }],
            ..Default::default()
        }],
    };
    let file2 = FileDiff {
        path: "b.txt".to_string(),
        hunks: vec![
            Hunk {
                header: "h2".to_string(),
                lines: vec![DiffLine {
                    content: "l2\n".to_string(),
                    ..Default::default()
                }],
                ..Default::default()
            },
            Hunk {
                header: "h3".to_string(),
                lines: vec![DiffLine {
                    content: "l3\n".to_string(),
                    ..Default::default()
                }],
                ..Default::default()
            },
        ],
    };
    let file3 = FileDiff {
        path: "c.txt".to_string(),
        hunks: vec![Hunk {
            header: "h4".to_string(),
            ..Default::default()
        }],
    };

    let mut state = SplitState::new(vec![file1, file2, file3]);

    // 1. Initial state: File 0
    assert_eq!(state.mode, SplitViewMode::Files);
    assert_eq!(state.get_focused_file_idx(), Some(0));

    // 2. Down once: Move to second file (file index 1)
    state.next();
    assert_eq!(state.get_focused_file_idx(), Some(1));

    // 3. Right: enter second file's hunks
    state.enter_hunks();
    assert_eq!(state.mode, SplitViewMode::Hunks);
    assert_eq!(state.get_focused_file_idx(), Some(1));
    assert_eq!(state.get_focused_hunk_idx(), Some(0)); // First hunk of second file (h2)

    // 4. Down once: move to second chunk of that file (h3)
    state.next();
    assert_eq!(state.get_focused_file_idx(), Some(1));
    assert_eq!(state.get_focused_hunk_idx(), Some(1)); // Second hunk of second file (h3)

    // 5. Right: expand/enter second chunk's lines
    state.enter_lines();
    assert_eq!(state.mode, SplitViewMode::Lines);
    assert_eq!(state.get_focused_file_idx(), Some(1));
    assert_eq!(state.get_focused_hunk_idx(), Some(1));
    assert_eq!(state.get_focused_line_idx(), Some(0));

    // 6. Left: exit lines back to chunks
    state.exit_lines();
    assert_eq!(state.mode, SplitViewMode::Hunks);
    assert_eq!(
        state.get_focused_file_idx(),
        Some(1),
        "Should stay on file 1"
    );
    assert_eq!(
        state.get_focused_hunk_idx(),
        Some(1),
        "Should stay on hunk 1"
    );
}

#[test]
fn test_split_page_up_out_of_hunk() {
    let mut files = Vec::new();
    for i in 0..3 {
        files.push(FileDiff {
            path: format!("file-{}.txt", i),
            hunks: vec![
                Hunk {
                    header: format!("f{}h0", i),
                    ..Default::default()
                },
                Hunk {
                    header: format!("f{}h1", i),
                    ..Default::default()
                },
            ],
        });
    }

    let mut state = SplitState::new(files);

    // 1. Enter hunks mode on file 1
    state.next(); // file 1
    state.enter_hunks();
    assert_eq!(state.mode, SplitViewMode::Hunks);
    assert_eq!(state.get_focused_file_idx(), Some(1));
    assert_eq!(state.get_focused_hunk_idx(), Some(0));

    // 2. Move to second hunk of file 1
    state.next();
    assert_eq!(state.get_focused_hunk_idx(), Some(1));

    // 3. Page up with small height (1). Should move to first hunk.
    state.page_up(1);
    assert_eq!(state.get_focused_file_idx(), Some(1));
    assert_eq!(state.get_focused_hunk_idx(), Some(0));

    // 4. Page up again. In Hunks mode, we are constrained to the current file.
    // Index 0 is file0 header, index 1 is file1 header, index 2 is f1h0.
    // In Hunks mode, we cannot move to the file header of the current file because
    // prev() (and now page_up) stops if the previous item is a FileHeader.
    state.page_up(1);
    assert_eq!(state.get_focused_file_idx(), Some(1));
    assert_eq!(state.get_focused_hunk_idx(), Some(0)); // Still on first hunk

    // 5. Page up again. Should stay at the first hunk of the current file.
    state.page_up(1);
    assert_eq!(state.get_focused_file_idx(), Some(1));
    assert_eq!(state.get_focused_hunk_idx(), Some(0));

    // 6. Test Files mode: page up should cross file boundaries
    state.exit_hunks();
    state.next(); // file 2
    assert_eq!(state.get_focused_file_idx(), Some(2));
    state.page_up(2);
    assert_eq!(state.get_focused_file_idx(), Some(0));
}

#[test]
fn test_split_view_scrollbar_200_files() {
    let backend = TestBackend::new(80, 20);
    let mut terminal = Terminal::new(backend).unwrap();
    let mut state = AppState::default();

    let mut files = Vec::new();
    for i in 0..200 {
        files.push(FileDiff {
            path: format!("file-{:03}.txt", i),
            hunks: Vec::new(),
        });
    }

    let mut split_state = SplitState::new(files);
    // Focus the last file (index 199)
    for _ in 0..199 {
        split_state.next();
    }
    state.split_state = Some(split_state);

    terminal
        .draw(|f| ui::draw_split_view(f, &mut state))
        .unwrap();
    let buffer = terminal.backend().buffer();

    // chunks[0] height is 17 (0 to 16).
    // Scrollbar is in chunks[0].inner(vertical: 1), so height 15 (1 to 15).
    // Thumb should be at the very bottom, just above or at the '↓' symbol.

    let mut thumb_y = None;
    for y in 1..16 {
        if buffer[(79, y)].symbol() == "█" {
            thumb_y = Some(y);
        }
    }

    assert!(thumb_y.is_some(), "Scrollbar thumb '█' not found");
    // In a 15-height scrollbar (including arrows), bottom thumb should be at y=14
    assert_eq!(
        thumb_y.unwrap(),
        14,
        "Thumb should be at the bottom (y=14) for 200 items, but found at y={}",
        thumb_y.unwrap()
    );
}

// end of file
