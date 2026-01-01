use git2::Oid;
use gitui::engine::{
    BranchInfo, BranchIntent, HistoryContext, Operation, RepositorySnapshot, apply_move,
    calculate_plan, flatten_branches, is_descendant,
};
use gitui::state::{AppState, ConflictCheckState, Effect, Msg, SidebarState};
use gitui::testing::MockGit;
use std::collections::HashMap;

fn make_branch(name: &str, oid: Oid, parent: Option<&str>) -> BranchInfo {
    BranchInfo {
        name: name.to_string(),
        oid,
        original_parent: parent.map(|s| s.to_string()),
        ..Default::default()
    }
}

#[test]
fn test_is_descendant() {
    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_f1 = Oid::from_bytes(&[2; 20]).unwrap();
    let oid_f2 = Oid::from_bytes(&[3; 20]).unwrap();
    let branches = vec![
        make_branch("master", oid_m, None),
        make_branch("feat1", oid_f1, Some("master")),
        make_branch("feat2", oid_f2, Some("feat1")),
    ];
    let intents = HashMap::new();

    assert!(is_descendant(&branches, &intents, "feat1", "master"));
    assert!(is_descendant(&branches, &intents, "feat2", "master"));
    assert!(is_descendant(&branches, &intents, "feat2", "feat1"));
    assert!(!is_descendant(&branches, &intents, "master", "feat1"));
    assert!(!is_descendant(&branches, &intents, "feat1", "feat2"));
}

#[test]
fn test_tree_flattening() {
    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_f1 = Oid::from_bytes(&[2; 20]).unwrap();
    let oid_f2 = Oid::from_bytes(&[3; 20]).unwrap();
    let branches = vec![
        BranchInfo {
            name: "master".to_string(),
            oid: oid_m,
            children: vec!["feat1".to_string()],
            ..Default::default()
        },
        BranchInfo {
            name: "feat1".to_string(),
            oid: oid_f1,
            original_parent: Some("master".to_string()),
            children: vec!["feat2".to_string()],
            ..Default::default()
        },
        BranchInfo {
            name: "feat2".to_string(),
            oid: oid_f2,
            original_parent: Some("feat1".to_string()),
            ..Default::default()
        },
    ];
    let intents = HashMap::new();

    let flattened = flatten_branches(&branches, &intents, &HistoryContext::new(), false).unwrap();
    assert_eq!(flattened.len(), 3);
    assert_eq!(flattened[0].0, "master");
    assert_eq!(flattened[1].0, "feat1");
    assert_eq!(flattened[2].0, "feat2");
    assert_eq!(flattened[2].1, 2); // depth
}

#[test]
fn test_tree_with_missing_parent() {
    let oid = Oid::from_bytes(&[0; 20]).unwrap();
    let branches = vec![BranchInfo {
        name: "master".to_string(),
        oid,
        original_parent: Some("origin/master".to_string()),
        ..Default::default()
    }];
    let intents = HashMap::new();

    let flattened = flatten_branches(&branches, &intents, &HistoryContext::new(), false).unwrap();
    assert_eq!(flattened.len(), 1);
    assert_eq!(flattened[0].0, "master");
}

#[test]
fn test_swap_branches_high_level() {
    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_t = Oid::from_bytes(&[2; 20]).unwrap();
    let oid_g = Oid::from_bytes(&[3; 20]).unwrap();
    let oid_r = Oid::from_bytes(&[4; 20]).unwrap();
    let branches = vec![
        BranchInfo {
            name: "master".to_string(),
            oid: oid_m,
            children: vec!["toxav-msi".to_string()],
            ..Default::default()
        },
        BranchInfo {
            name: "toxav-msi".to_string(),
            oid: oid_t,
            original_parent: Some("master".to_string()),
            children: vec!["groups-perf".to_string()],
            ..Default::default()
        },
        BranchInfo {
            name: "groups-perf".to_string(),
            oid: oid_g,
            original_parent: Some("toxav-msi".to_string()),
            children: vec!["rtp-hbo".to_string()],
            ..Default::default()
        },
        BranchInfo {
            name: "rtp-hbo".to_string(),
            oid: oid_r,
            original_parent: Some("groups-perf".to_string()),
            ..Default::default()
        },
    ];

    let _mock_git = MockGit::new(branches.clone());
    let mut intents = HashMap::new();

    // Move rtp-hbo to be child of toxav-msi
    apply_move(&mut intents, "rtp-hbo", Some("toxav-msi".to_string())).unwrap();
    assert_eq!(
        intents
            .get("rtp-hbo")
            .unwrap()
            .parent
            .as_ref()
            .and_then(|o| o.as_deref()),
        Some("toxav-msi")
    );

    // Move groups-perf to be child of rtp-hbo
    apply_move(&mut intents, "groups-perf", Some("rtp-hbo".to_string())).unwrap();
    assert_eq!(
        intents
            .get("groups-perf")
            .unwrap()
            .parent
            .as_ref()
            .and_then(|o| o.as_deref()),
        Some("rtp-hbo")
    );
}

#[test]
fn test_calculate_plan_rebase() {
    use Operation;
    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_f1 = Oid::from_bytes(&[2; 20]).unwrap();
    let oid_f2 = Oid::from_bytes(&[3; 20]).unwrap();
    let branches = vec![
        BranchInfo {
            name: "master".to_string(),
            oid: oid_m,
            children: vec!["feat1".to_string()],
            ..Default::default()
        },
        BranchInfo {
            name: "feat1".to_string(),
            oid: oid_f1,
            original_parent: Some("master".to_string()),
            children: vec!["feat2".to_string()],
            ..Default::default()
        },
        BranchInfo {
            name: "feat2".to_string(),
            oid: oid_f2,
            original_parent: Some("feat1".to_string()),
            ..Default::default()
        },
    ];

    let mut intents = HashMap::new();
    // Move feat2 to be child of master.
    intents.insert(
        "feat2".to_string(),
        BranchIntent {
            parent: Some(Some("master".to_string())),
            ..Default::default()
        },
    );

    let snapshot = RepositorySnapshot {
        branches,
        history: HistoryContext::new(),
        is_dirty: false,
    };

    let plan = calculate_plan(&snapshot, &intents).unwrap();
    assert_eq!(plan.len(), 1);
    if let Operation::Rebase {
        upstream: _,
        branch,
        onto,
        predicted_conflict: _,
    } = &plan[0]
    {
        assert_eq!(branch, "feat2");
        assert_eq!(onto, "master");
    } else {
        panic!("Expected rebase operation");
    }
}

#[test]
fn test_transitive_rebase_planning() {
    use Operation;
    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_f1 = Oid::from_bytes(&[2; 20]).unwrap();
    let oid_f2 = Oid::from_bytes(&[3; 20]).unwrap();
    let branches = vec![
        BranchInfo {
            name: "master".to_string(),
            oid: oid_m,
            children: vec!["feat1".to_string()],
            ..Default::default()
        },
        BranchInfo {
            name: "feat1".to_string(),
            oid: oid_f1,
            original_parent: Some("master".to_string()),
            children: vec!["feat2".to_string()],
            ..Default::default()
        },
        BranchInfo {
            name: "feat2".to_string(),
            oid: oid_f2,
            original_parent: Some("feat1".to_string()),
            ..Default::default()
        },
    ];

    let mut intents = HashMap::new();
    // Move feat1 to be root (effectively a rebase onto nothing, or another base)
    // For this test, let's just mark feat1 for reset.
    intents.insert(
        "feat1".to_string(),
        BranchIntent {
            pending_reset: true,
            ..Default::default()
        },
    );

    let snapshot = RepositorySnapshot {
        branches,
        history: HistoryContext::new(),
        is_dirty: false,
    };

    let plan = calculate_plan(&snapshot, &intents).unwrap();

    // Expect:
    // 1. Reset feat1 (due to pending_reset and no upstream)
    // 2. Rebase feat2 (because its parent feat1 was rebased/reset)
    assert!(
        plan.iter()
            .any(|op| matches!(op, Operation::Reset { branch, .. } if branch == "feat1"))
    );
    assert!(plan.iter().any(
        |op| matches!(op, Operation::Rebase { upstream: _, branch, .. } if branch == "feat2")
    ));
}

#[test]
fn test_noop_filtering_reset() {
    let oid = Oid::from_bytes(&[1; 20]).unwrap();
    let mut history = HistoryContext::new();
    history
        .oid_to_visible_branch
        .insert(oid, "master".to_string());

    let branches = vec![BranchInfo {
        name: "master".to_string(),
        oid,
        upstream_oid: Some(oid), // Already in sync
        upstream: Some("origin/master".to_string()),
        ..Default::default()
    }];
    let mut intents = HashMap::new();
    intents.insert(
        "master".to_string(),
        BranchIntent {
            pending_reset: true,
            ..Default::default()
        },
    );

    let snapshot = RepositorySnapshot {
        branches,
        history,
        is_dirty: false,
    };

    let plan = calculate_plan(&snapshot, &intents).unwrap();
    assert!(
        plan.is_empty(),
        "Reset should be filtered out as no-op when OIDs match"
    );
}

#[test]
fn test_noop_filtering_structural_move() {
    let oid_a = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_b = Oid::from_bytes(&[2; 20]).unwrap();

    let mut history = HistoryContext::new();
    // B is already a child of A
    history.oid_to_ancestor.insert(oid_b, Some(oid_a));
    history.oid_to_visible_branch.insert(oid_a, "A".to_string());
    history.oid_to_visible_branch.insert(oid_b, "B".to_string());

    let branches = vec![
        BranchInfo {
            name: "A".to_string(),
            oid: oid_a,
            children: vec!["B".to_string()],
            ..Default::default()
        },
        BranchInfo {
            name: "B".to_string(),
            oid: oid_b,
            original_parent: None, // It was a root before
            ..Default::default()
        },
    ];
    let mut intents = HashMap::new();
    intents.insert(
        "B".to_string(),
        BranchIntent {
            parent: Some(Some("A".to_string())),
            ..Default::default()
        },
    );

    let snapshot = RepositorySnapshot {
        branches,
        history,
        is_dirty: false,
    };

    let plan = calculate_plan(&snapshot, &intents).unwrap();
    assert!(
        plan.is_empty(),
        "Structural move should be filtered out as no-op if topological relationship already exists"
    );
}

#[test]
fn test_dirty_filtering() {
    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_f1 = Oid::from_bytes(&[2; 20]).unwrap();
    let branches = vec![
        BranchInfo {
            name: "master".to_string(),
            oid: oid_m,
            children: vec!["feat1".to_string()],
            ..Default::default()
        },
        BranchInfo {
            name: "feat1".to_string(),
            oid: oid_f1,
            original_parent: Some("master".to_string()),
            ..Default::default()
        },
    ];

    let mut intents = HashMap::new();
    intents.insert(
        "feat1".to_string(),
        BranchIntent {
            parent: None,
            pending_push: true,
            ..Default::default()
        },
    );
    intents.insert(
        "master".to_string(),
        BranchIntent {
            pending_delete: true,
            ..Default::default()
        },
    );

    let snapshot = RepositorySnapshot {
        branches,
        history: HistoryContext::new(),
        is_dirty: true,
    };

    // When dirty: rebase should be filtered out, but push and delete should remain
    let plan = calculate_plan(&snapshot, &intents).unwrap();
    assert_eq!(plan.len(), 2);
    assert!(plan.iter().any(|op| matches!(op, Operation::Push { .. })));
    assert!(plan.iter().any(|op| matches!(op, Operation::Delete { .. })));
    assert!(
        !plan
            .iter()
            .any(|op| matches!(op, Operation::Rebase { upstream: _, .. }))
    );
}

#[test]
fn test_is_better_name() {
    use gitui::engine::is_better_name;

    assert!(is_better_name("master", "feat1"));
    assert!(!is_better_name("feat1", "master"));
    assert!(is_better_name("main", "feat1"));
    assert!(!is_better_name("feat1", "main"));
    assert!(is_better_name("a-feat", "z-feat"));
    assert!(!is_better_name("z-feat", "a-feat"));
    assert!(!is_better_name("same", "same"));

    // master is better than main
    assert!(is_better_name("master", "main"));
    assert!(!is_better_name("main", "master"));
}

#[test]
fn test_operation_commands() {
    use Operation;

    let op = Operation::Rebase {
        upstream: None,
        branch: "feat".to_string(),
        onto: "master".to_string(),
        predicted_conflict: None,
    };
    assert_eq!(
        op.commands(),
        vec![vec![
            "rebase".to_string(),
            "master".to_string(),
            "feat".to_string()
        ]]
    );
    assert_eq!(format!("{}", op), "git rebase master feat");

    let op = Operation::Reset {
        branch: "feat".to_string(),
    };
    assert_eq!(
        op.commands(),
        vec![
            vec!["checkout".to_string(), "feat".to_string()],
            vec![
                "reset".to_string(),
                "--hard".to_string(),
                "@{u}".to_string()
            ],
        ]
    );
    assert_eq!(
        format!("{}", op),
        "git checkout feat && git reset --hard @{u}"
    );

    let op = Operation::Localize {
        branch: "origin/feat".to_string(),
    };
    assert_eq!(
        op.commands(),
        vec![vec![
            "checkout".to_string(),
            "-B".to_string(),
            "feat".to_string(),
            "--track".to_string(),
            "origin/feat".to_string(),
        ]]
    );
}

#[test]
fn test_app_state_multiline_prompt_navigation() {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use gitui::state::{AppMode, PromptAction, PromptFocus, PromptState};

    let mut state = AppState {
        mode: AppMode::Prompt,
        prompt: Some(PromptState {
            title: "Test".to_string(),
            value: "line1\nline2\nline3".to_string(),
            cursor_position: 11, // End of "line2" (index of second \n)
            action: PromptAction::SplitPartMessage,
            focus: PromptFocus::Input,
        }),
        ..Default::default()
    };

    // 1. Move UP to line 1
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Up,
        KeyModifiers::NONE,
    )));
    // Cursor was at end of line2 (pos 11), should move to end of line1 (pos 5)
    assert_eq!(state.prompt.as_ref().unwrap().cursor_position, 5);

    // 2. Move DOWN to line 2
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Down,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.prompt.as_ref().unwrap().cursor_position, 11);

    // 3. Move DOWN to line 3
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Down,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.prompt.as_ref().unwrap().cursor_position, 17); // End of line 3 (last char)

    // 4. Test Enter for newline
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Enter,
        KeyModifiers::NONE,
    )));
    assert_eq!(
        state.prompt.as_ref().unwrap().value,
        "line1\nline2\nline3\n"
    );
    assert_eq!(state.prompt.as_ref().unwrap().cursor_position, 18);

    // 5. Test Tab navigation
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Tab,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.prompt.as_ref().unwrap().focus, PromptFocus::Ok);

    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Tab,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.prompt.as_ref().unwrap().focus, PromptFocus::Cancel);

    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Tab,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.prompt.as_ref().unwrap().focus, PromptFocus::Input);

    // 6. Test BackTab
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::BackTab,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.prompt.as_ref().unwrap().focus, PromptFocus::Cancel);
}

#[test]
fn test_app_state_log_loading_logic() {
    use std::time::Instant;

    let mut state = AppState::default();

    // Initial state: not loading (no selection time yet)
    assert!(!state.is_log_loading());

    // Selection changed: sets debouncing state
    state.sidebar = SidebarState::Debouncing {
        branch: "master".to_string(),
        since: Instant::now(),
    };

    // Should NOT be loading yet (waiting for debounce)
    assert!(!state.is_log_loading());

    // Request sent: state becomes Loading
    state.sidebar = SidebarState::Loading {
        branch: "master".to_string(),
    };
    assert!(state.is_log_loading());

    // Result received: state becomes Ready
    state.sidebar = SidebarState::Ready {
        branch: "master".to_string(),
        commits: vec![gitui::engine::CommitInfo {
            id: "abc1234".to_string(),
            summary: "commit 1".to_string(),
            author: "Author".to_string(),
        }],
    };

    // Should not be loading anymore
    assert!(!state.is_log_loading());
}

#[test]
fn test_app_state_log_fetch_debounce() {
    use std::time::{Duration, Instant};

    let mut state = AppState {
        flattened_tree: vec![("master".to_string(), 0)],
        ..Default::default()
    };

    // 1. Initial state: nothing to fetch
    assert!(state.sidebar == SidebarState::Idle);

    // 2. Selection changed: last_selection_time set
    state.sidebar = SidebarState::Debouncing {
        branch: "master".to_string(),
        since: Instant::now(),
    };

    // 3. Tick immediately: too soon, no effect returned
    assert!(state.update(Msg::Tick).is_empty());

    // 4. Wait for debounce (500ms)
    state.sidebar = SidebarState::Debouncing {
        branch: "master".to_string(),
        since: Instant::now() - Duration::from_millis(550),
    };

    // 5. First call: triggers fetch, returns effect, updates state to Loading
    let effects = state.update(Msg::Tick);
    assert!(
        effects
            .iter()
            .any(|e| matches!(e, Effect::FetchCommitLog { .. }))
    );
    assert!(state.is_log_loading());

    // 6. Second call: already fetching, returns nothing
    assert!(state.update(Msg::Tick).is_empty());
}

#[test]
fn test_app_state_update_branches_loaded() {
    let mut state = AppState::default();
    let oid = Oid::from_bytes(&[1; 20]).unwrap();
    let branches = vec![BranchInfo {
        name: "master".to_string(),
        oid,
        ..Default::default()
    }];

    let history = HistoryContext::new();
    let msg = Msg::BranchesLoaded(Ok((branches, history, false)));

    let effects = state.update(msg);

    // Selection should be at 0, Sidebar should be Debouncing
    assert_eq!(state.branches.len(), 1);
    assert_eq!(state.list_state.selected(), Some(0));
    assert!(matches!(state.sidebar, SidebarState::Debouncing { .. }));
    // No immediate effects expected on load (wait for tick)
    assert!(effects.is_empty());
}

#[test]
fn test_app_state_keyboard_navigation() {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    let mut state = AppState {
        flattened_tree: vec![
            ("a".to_string(), 0),
            ("b".to_string(), 0),
            ("c".to_string(), 0),
        ],
        ..Default::default()
    };

    // Down
    let key = KeyEvent::new(KeyCode::Char('j'), KeyModifiers::NONE);
    state.update(Msg::KeyPressed(key));
    assert_eq!(state.list_state.selected(), Some(1));
    assert!(matches!(state.sidebar, SidebarState::Debouncing { ref branch, .. } if branch == "b"));

    // Up
    let key = KeyEvent::new(KeyCode::Char('k'), KeyModifiers::NONE);
    state.update(Msg::KeyPressed(key));
    assert_eq!(state.list_state.selected(), Some(0));
    assert!(matches!(state.sidebar, SidebarState::Debouncing { ref branch, .. } if branch == "a"));
}

#[test]
fn test_app_state_conflict_checked_flow() {
    let mut state = AppState {
        branches: vec![BranchInfo {
            name: "feat".to_string(),
            oid: Oid::from_bytes(&[1; 20]).unwrap(),
            ..Default::default()
        }],
        grabbed_branch: Some("feat".to_string()),
        conflict_check: ConflictCheckState::Checking {
            branch: "feat".to_string(),
            onto: "master".to_string(),
        },
        ..Default::default()
    };

    state.update(Msg::ConflictChecked(
        "feat".to_string(),
        "master".to_string(),
        Ok(true),
    ));

    assert_eq!(state.conflict_check, ConflictCheckState::Idle);
    assert_eq!(
        state
            .conflict_cache
            .get(&("feat".to_string(), "master".to_string())),
        Some(&true)
    );
    assert!(state.get_intent("feat").has_conflict);
}

#[test]
fn test_app_state_prediction_flow() {
    let mut state = AppState::default();
    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_f = Oid::from_bytes(&[2; 20]).unwrap();
    state.branches = vec![
        BranchInfo {
            name: "master".to_string(),
            oid: oid_m,
            ..Default::default()
        },
        BranchInfo {
            name: "feat".to_string(),
            oid: oid_f,
            original_parent: None,
            ..Default::default()
        },
    ];
    state.mutate_intent("feat", |i| {
        i.parent = Some(Some("master".to_string()));
    });
    state.flattened_tree = vec![("master".to_string(), 0), ("feat".to_string(), 1)];

    // 1. Enter preview mode
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    let key_v = KeyEvent::new(KeyCode::Char('v'), KeyModifiers::NONE);
    let effects = state.update(Msg::KeyPressed(key_v));

    assert!(state.show_preview);
    assert!(state.is_predicting_conflicts);
    assert!(
        effects
            .iter()
            .any(|e| matches!(e, Effect::PredictConflicts { .. }))
    );

    // 2. Receive prediction result
    let predicted_plan = vec![Operation::Rebase {
        upstream: None,
        branch: "feat".to_string(),
        onto: "master".to_string(),
        predicted_conflict: Some(true),
    }];
    state.update(Msg::ConflictsPredicted(predicted_plan.clone()));

    assert!(!state.is_predicting_conflicts);
    assert_eq!(state.plan.as_ref(), Some(&predicted_plan));
}

#[test]
fn test_app_state_commit_effect() {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    let mut state = AppState::default();
    let key = KeyEvent::new(KeyCode::Char('c'), KeyModifiers::NONE);
    let effects = state.update(Msg::KeyPressed(key));

    assert!(
        effects
            .iter()
            .any(|e| matches!(e, Effect::ApplyAndQuit(_, _)))
    );
}

#[test]
fn test_quit_effect() {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

    let mut state = AppState::default();

    // 1. Initial state: empty branches, so calculate_plan is empty. 'q' should quit immediately.
    let key = KeyEvent::new(KeyCode::Char('q'), KeyModifiers::NONE);
    let effects = state.update(Msg::KeyPressed(key));
    assert!(effects.iter().any(|e| matches!(e, Effect::Quit)));

    // 2. Modified state: calculate_plan will NOT be empty because feat's parent changed.
    // 'q' should show confirmation.
    state.is_dirty = false; // reset to be sure
    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_f = Oid::from_bytes(&[2; 20]).unwrap();
    state.branches = vec![
        BranchInfo {
            name: "master".to_string(),
            oid: oid_m,
            ..Default::default()
        },
        BranchInfo {
            name: "feat".to_string(),
            oid: oid_f,
            original_parent: None, // This makes it NOT empty
            ..Default::default()
        },
    ];
    state.mutate_intent("feat", |i| {
        i.parent = Some(Some("master".to_string()));
    });
    state.flattened_tree = vec![("master".to_string(), 0), ("feat".to_string(), 1)];

    let effects = state.update(Msg::KeyPressed(key));
    assert!(
        effects.is_empty(),
        "Expected empty effects, but got {:?}",
        effects
    );
    assert!(state.show_quit_confirmation);

    // 3. Confirming quit: 'y' or 'q' should quit
    let key_y = KeyEvent::new(KeyCode::Char('y'), KeyModifiers::NONE);
    let effects = state.update(Msg::KeyPressed(key_y));
    assert!(effects.iter().any(|e| matches!(e, Effect::Quit)));

    // 4. In preview mode: 'q' should exit preview, not quit app
    state.show_quit_confirmation = false;
    state.show_preview = true;
    let effects = state.update(Msg::KeyPressed(key));
    assert!(effects.is_empty());
    assert!(!state.show_preview);
}

#[test]
fn test_calculate_plan_submit() {
    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_f = Oid::from_bytes(&[2; 20]).unwrap();
    let branches = vec![
        BranchInfo {
            name: "master".to_string(),
            oid: oid_m,
            upstream_oid: Some(oid_m),
            upstream: Some("origin/master".to_string()),
            ..Default::default()
        },
        BranchInfo {
            name: "feat".to_string(),
            oid: oid_f,
            original_parent: Some("master".to_string()),
            ahead: 1,
            ..Default::default()
        },
        BranchInfo {
            name: "desc".to_string(),
            oid: Oid::from_bytes(&[3; 20]).unwrap(),
            original_parent: Some("feat".to_string()),
            ahead: 1,
            ..Default::default()
        },
    ];
    let mut intents = HashMap::new();
    intents.insert(
        "feat".to_string(),
        BranchIntent {
            pending_submit: true,
            ..Default::default()
        },
    );

    let mut history = HistoryContext::new();
    history.oid_to_ancestor.insert(oid_f, Some(oid_m));
    history.oid_to_ancestor.insert(oid_m, None);
    history
        .oid_to_visible_branch
        .insert(oid_m, "master".to_string());
    history
        .oid_to_visible_branch
        .insert(oid_f, "feat".to_string());

    let snapshot = RepositorySnapshot {
        branches,
        history,
        is_dirty: false,
    };

    let plan = calculate_plan(&snapshot, &intents).unwrap();

    // Expected:
    // 1. Submit feat
    // 2. Rebase desc onto feat (actually feat is now master, but parent remains feat in topology)
    // 3. Push desc

    assert!(
        plan.iter()
            .any(|op| matches!(op, Operation::Submit { branch, .. } if branch == "feat"))
    );
    assert!(
        !plan.iter().any(
            |op| matches!(op, Operation::Rebase { upstream: _, branch, .. } if branch == "desc")
        ),
        "Descendant should NOT be rebased if the parent was submitted (stays at same OID)"
    );
    assert!(
        plan.iter()
            .any(|op| matches!(op, Operation::Delete { branch } if branch == "feat")),
        "Submitted branch should be deleted after landing"
    );
}

#[test]
fn test_predict_conflicts_simulation() {
    use gitui::engine::predict_conflicts;

    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_f = Oid::from_bytes(&[2; 20]).unwrap();
    let branches = vec![
        BranchInfo {
            name: "master".to_string(),
            oid: oid_m,
            ..Default::default()
        },
        BranchInfo {
            name: "feat".to_string(),
            oid: oid_f,
            ..Default::default()
        },
    ];

    let mut plan = vec![
        Operation::Submit {
            branch: "feat".to_string(),
            target: "master".to_string(),
        },
        Operation::Rebase {
            upstream: None,
            branch: "other".to_string(),
            onto: "master".to_string(),
            predicted_conflict: None,
        },
    ];

    // Add "other" to branches so it's found
    let mut branches_with_other = branches.clone();
    let oid_o = Oid::from_bytes(&[3; 20]).unwrap();
    branches_with_other.push(BranchInfo {
        name: "other".to_string(),
        oid: oid_o,
        ..Default::default()
    });

    let mock_git = MockGit::new(branches_with_other.clone());
    predict_conflicts(&mut plan, &mock_git, &branches_with_other, None);

    // Verify that the rebase check used the OID of "feat" (the new master), not the old master OID
    let calls = mock_git.check_conflict_between_calls.lock().unwrap();
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0], (oid_o, oid_f)); // (branch_oid, onto_oid)
}

#[test]
fn test_calculate_plan_amend_and_rebase_subtree() {
    use Operation;
    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_f1 = Oid::from_bytes(&[2; 20]).unwrap();
    let oid_f2 = Oid::from_bytes(&[3; 20]).unwrap();
    let branches = vec![
        BranchInfo {
            name: "master".to_string(),
            oid: oid_m,
            ..Default::default()
        },
        BranchInfo {
            name: "feat1".to_string(),
            oid: oid_f1,
            original_parent: Some("master".to_string()),
            ..Default::default()
        },
        BranchInfo {
            name: "feat2".to_string(),
            oid: oid_f2,
            original_parent: Some("feat1".to_string()),
            ..Default::default()
        },
    ];

    let mut intents = HashMap::new();
    intents.insert(
        "feat1".to_string(),
        BranchIntent {
            pending_amend: true,
            ..Default::default()
        },
    );

    let snapshot = RepositorySnapshot {
        branches,
        history: HistoryContext::new(),
        is_dirty: true,
    };

    // Test with is_dirty = true
    let plan = calculate_plan(&snapshot, &intents).unwrap();

    // 1. Amend feat1 (even if dirty, because it's pending_amend)
    // 2. Rebase feat2 onto feat1 (because feat1 was amended, and effectively_dirty becomes false)
    assert!(matches!(plan[0], Operation::Amend { .. }));
    match &plan[1] {
        Operation::Rebase {
            branch,
            onto,
            upstream: _,
            predicted_conflict: _,
        } => {
            assert_eq!(branch, "feat2");
            assert_eq!(onto, "feat1");
        }
        _ => panic!("Expected rebase operation"),
    }
}

#[test]
fn test_amend_operation_commands() {
    use Operation;
    let op = Operation::Amend { message: None };
    let cmds = op.commands();
    assert_eq!(cmds.len(), 1);
    assert_eq!(cmds[0], vec!["commit", "--amend", "--no-edit", "-a"]);
    assert_eq!(format!("{}", op), "git commit --amend --no-edit -a");

    let op_msg = Operation::Amend {
        message: Some("new msg".to_string()),
    };
    let cmds_msg = op_msg.commands();
    assert_eq!(cmds_msg.len(), 1);
    assert_eq!(
        cmds_msg[0],
        vec!["commit", "--amend", "-m", "new msg", "-a"]
    );
}

#[test]
fn test_split_mode_cancellation() {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    let mut state = AppState::default();

    // 1. Enter split mode (simulated)
    state.mode = gitui::state::AppMode::Split;
    state.is_loading = true;
    state.split_state = None;

    // 2. Press 'q' to cancel
    let key_q = KeyEvent::new(KeyCode::Char('q'), KeyModifiers::NONE);
    state.update(Msg::KeyPressed(key_q));

    // 3. Verify state
    assert_eq!(state.mode, gitui::state::AppMode::Tree);
    assert!(!state.is_loading);
}

#[test]
fn test_calculate_plan_upstream_mismatch() {
    use git2::Oid;
    use gitui::engine::types::{BranchInfo, BranchIntent, Operation, RepositorySnapshot};
    use gitui::engine::{HistoryContext, calculate_plan};
    use std::collections::HashMap;

    let oid_m = Oid::from_bytes(&[1; 20]).unwrap();
    let oid_f1 = Oid::from_bytes(&[2; 20]).unwrap();
    let oid_f2 = Oid::from_bytes(&[3; 20]).unwrap();

    let branches = vec![
        BranchInfo {
            name: "master".to_string(),
            oid: oid_m,
            ..Default::default()
        },
        BranchInfo {
            name: "feat1".to_string(),
            oid: oid_f1,
            original_parent: Some("master".to_string()),
            ..Default::default()
        },
        BranchInfo {
            name: "feat2".to_string(),
            oid: oid_f2,
            original_parent: Some("feat1".to_string()),
            heuristic_parent: Some("feat1".to_string()),
            heuristic_upstream_oid: Some(oid_f1), // Heuristic says it belongs on feat1 head
            ..Default::default()
        },
    ];

    let mut intents = HashMap::new();
    // Converge feat2 to feat1 (names match original_parent)
    intents.insert(
        "feat2".to_string(),
        BranchIntent {
            parent: Some(Some("feat1".to_string())),
            ..Default::default()
        },
    );

    let mut history = HistoryContext::new();
    // feat2 is currently on master, but heuristic says it belongs on feat1 (at oid_f1)
    history.oid_to_ancestor.insert(oid_f2, Some(oid_m));

    let snapshot = RepositorySnapshot {
        branches,
        history,
        is_dirty: false,
    };

    let plan = calculate_plan(&snapshot, &intents).unwrap();

    // It should plan a rebase because heuristic upstream (oid_f1) differs from current topo parent (oid_m).
    assert!(
        plan.iter().any(|op| matches!(op, Operation::Rebase { branch, onto, .. } if branch == "feat2" && onto == "feat1")),
        "Should plan rebase because heuristic upstream differs from current parent. Plan: {:?}", plan
    );
}

#[test]
fn test_rebase_command_generation() {
    use gitui::engine::types::Operation;

    let op_simple = Operation::Rebase {
        branch: "my-branch".to_string(),
        onto: "master".to_string(),
        upstream: None,
        predicted_conflict: None,
    };
    let cmds_simple = op_simple.commands();
    assert_eq!(cmds_simple.len(), 1);
    assert_eq!(cmds_simple[0], vec!["rebase", "master", "my-branch"]);

    let op_3arg = Operation::Rebase {
        branch: "my-branch".to_string(),
        onto: "master".to_string(),
        upstream: Some("old-parent-oid".to_string()),
        predicted_conflict: None,
    };
    let cmds_3arg = op_3arg.commands();
    assert_eq!(cmds_3arg.len(), 1);
    assert_eq!(
        cmds_3arg[0],
        vec!["rebase", "--onto", "master", "old-parent-oid", "my-branch"]
    );
}

#[test]
fn test_input_utf8_handling() {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    use gitui::state::{AppMode, PromptAction, PromptFocus, PromptState};

    let mut state = AppState {
        mode: AppMode::Prompt,
        prompt: Some(PromptState {
            title: "Test".to_string(),
            value: "".to_string(),
            cursor_position: 0,
            action: PromptAction::RenameBranch,
            focus: PromptFocus::Input,
        }),
        ..Default::default()
    };

    // 1. Insert '€' (3 bytes)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('€'),
        KeyModifiers::NONE,
    )));

    // Check cursor position moved by 3
    assert_eq!(state.prompt.as_ref().unwrap().cursor_position, 3);
    assert_eq!(state.prompt.as_ref().unwrap().value, "€");

    // 2. Insert 'a' (1 byte)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('a'),
        KeyModifiers::NONE,
    )));
    assert_eq!(state.prompt.as_ref().unwrap().cursor_position, 4);
    assert_eq!(state.prompt.as_ref().unwrap().value, "€a");

    // 3. Move Left (should jump over 'a' (1 byte) to pos 3)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Left,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.prompt.as_ref().unwrap().cursor_position, 3);

    // 4. Move Left (should jump over '€' (3 bytes) to pos 0)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Left,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.prompt.as_ref().unwrap().cursor_position, 0);

    // 5. Move Right (jump over '€' to pos 3)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Right,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.prompt.as_ref().unwrap().cursor_position, 3);

    // 6. Delete (Backspace) '€' (from pos 3)
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Backspace,
        KeyModifiers::NONE,
    )));
    assert_eq!(state.prompt.as_ref().unwrap().cursor_position, 0);
    assert_eq!(state.prompt.as_ref().unwrap().value, "a");
}
