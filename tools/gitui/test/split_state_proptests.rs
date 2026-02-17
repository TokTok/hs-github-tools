use gitui::diff_utils::{DiffLine, FileDiff, Hunk, LineType};
use gitui::split_state::{SplitState, SplitViewMode};
use proptest::prelude::*;

fn arb_file_diff(max_hunks: usize, max_lines: usize) -> impl Strategy<Value = FileDiff> {
    (
        ".*", // Path
        prop::collection::vec(arb_hunk(max_lines), 1..=max_hunks),
    )
        .prop_map(|(path, hunks)| FileDiff { path, hunks })
}

fn arb_hunk(max_lines: usize) -> impl Strategy<Value = Hunk> {
    prop::collection::vec(arb_diff_line(), 0..=max_lines).prop_map(|lines| Hunk {
        header: "@@ hunk @@".to_string(),
        lines,
        ..Default::default()
    })
}

fn arb_diff_line() -> impl Strategy<Value = DiffLine> {
    prop::sample::select(vec![
        LineType::Addition,
        LineType::Deletion,
        LineType::Context,
    ])
    .prop_map(|line_type| DiffLine {
        content: "line\n".to_string(),
        line_type,
        ..Default::default()
    })
}

fn arb_split_state(
    max_files: usize,
    max_hunks: usize,
    max_lines: usize,
) -> impl Strategy<Value = SplitState> {
    prop::collection::vec(arb_file_diff(max_hunks, max_lines), 1..=max_files)
        .prop_map(SplitState::new)
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(100))]

    #[test]
    fn test_selection_persistence_across_modes(
        mut state in arb_split_state(5, 5, 5),
        toggle_indices in prop::collection::vec(0..10usize, 1..5)
    ) {
        // 1. Initial selection in Files mode
        for &idx in &toggle_indices {
            state.selected_view_idx = idx % state.rendered_items.len();
            state.toggle_selection();
        }
        let selection_files = state.current_selection.clone();

        // 2. Switch to Hunks mode
        state.mode = SplitViewMode::Hunks;
        state.rebuild_view();
        prop_assert_eq!(&state.current_selection, &selection_files, "Selection changed when switching to Hunks mode");

        // 3. Toggle more in Hunks mode
        if !state.rendered_items.is_empty() {
            for &idx in &toggle_indices {
                state.selected_view_idx = idx % state.rendered_items.len();
                state.toggle_selection();
            }
        }
        let selection_hunks = state.current_selection.clone();

        // 4. Switch to Lines mode
        state.mode = SplitViewMode::Lines;
        state.rebuild_view();
        prop_assert_eq!(&state.current_selection, &selection_hunks, "Selection changed when switching to Lines mode");

        // 5. Switch back to Files mode
        state.mode = SplitViewMode::Files;
        state.rebuild_view();
        prop_assert_eq!(&state.current_selection, &selection_hunks, "Selection changed when switching back to Files mode");
    }

    #[test]
    fn test_file_selection_idempotency(
        mut state in arb_split_state(3, 5, 0)
    ) {
        let initial_selection = state.current_selection.clone();

        // Select first file
        state.selected_view_idx = 0;
        state.toggle_selection();

        // If it was empty, it should now have all hunks of file 0
        // If it was full, it should now be empty

        // Toggle again
        state.toggle_selection();

        prop_assert_eq!(state.current_selection, initial_selection, "Double toggle of file header should be idempotent");
    }
}
