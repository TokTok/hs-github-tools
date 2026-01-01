use git2::Oid;
use gitui::engine::{
    BranchInfo, BranchIntent, HistoryContext, Operation, RepositorySnapshot, calculate_plan,
};
use gitui::testing::mock_oid;
use proptest::prelude::*;
use std::collections::{HashMap, HashSet};

fn make_branch(name: &str, oid: Oid, parent: Option<&str>) -> BranchInfo {
    BranchInfo {
        name: name.to_string(),
        oid,
        original_parent: parent.map(|s| s.to_string()),
        ..Default::default()
    }
}

/// Strategy for a linear chain of branches.
/// Returns (branches, history)
fn arb_linear_chain(n: usize) -> impl Strategy<Value = (Vec<BranchInfo>, HistoryContext)> {
    Just(n).prop_map(|n| {
        let mut branches = Vec::new();
        let mut history = HistoryContext::new();

        // Root: master
        branches.push(BranchInfo {
            name: "master".to_string(),
            oid: mock_oid(0),
            ..Default::default()
        });
        history
            .oid_to_visible_branch
            .insert(mock_oid(0), "master".to_string());
        history.oid_to_ancestor.insert(mock_oid(0), None);

        for i in 1..=n {
            let name = format!("feat{}", i);
            let oid = mock_oid(i as u8);
            let parent_name = if i == 1 {
                "master".to_string()
            } else {
                format!("feat{}", i - 1)
            };
            let parent_oid = mock_oid((i - 1) as u8);

            branches.push(BranchInfo {
                name: name.clone(),
                oid,
                original_parent: Some(parent_name.clone()),
                ..Default::default()
            });

            history.oid_to_visible_branch.insert(oid, name);
            history.oid_to_ancestor.insert(oid, Some(parent_oid));
        }

        (branches, history)
    })
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(50))]

    #[test]
    fn test_plan_no_op_identity(
        data in arb_linear_chain(10)
    ) {
        let (branches, history) = data;
        let intents = HashMap::new();
        let snapshot = RepositorySnapshot { branches, history, is_dirty: false };
        let plan = calculate_plan(&snapshot, &intents).unwrap();
        prop_assert!(plan.is_empty(), "Plan should be empty when no changes are made. Got: {:?}", plan);
    }

    #[test]
    fn test_plan_transitive_rebase_completeness(
        data in arb_linear_chain(5)
    ) {
        let (branches, history) = data;
        let mut intents = HashMap::new();
        // Mark the first feature branch for amend
        intents.insert("feat1".to_string(), BranchIntent {
            pending_amend: true,
            ..Default::default()
        });

        let snapshot = RepositorySnapshot { branches, history, is_dirty: false };
        let plan = calculate_plan(&snapshot, &intents).unwrap();

        // Property: feat1 is amended, and all its descendants (feat2..5) must be rebased
        prop_assert!(plan.iter().any(|op| matches!(op, Operation::Amend { .. })), "feat1 should be amended");

        let expected_rebased = vec!["feat2", "feat3", "feat4", "feat5"];
        let actual_rebased: HashSet<_> = plan.iter().filter_map(|op| {
            if let Operation::Rebase { branch, .. } = op {
                Some(branch.as_str())
            } else {
                None
            }
        }).collect();

        for name in expected_rebased {
            prop_assert!(actual_rebased.contains(name), "Branch '{}' was not rebased despite its ancestor being amended", name);
        }
    }

    #[test]
    fn test_plan_topological_order(
        data in arb_linear_chain(5)
    ) {
        let (mut branches, history) = data;
        // Move feat1 to master2
        branches.push(BranchInfo {
            name: "master2".to_string(),
            oid: mock_oid(100),
            ..Default::default()
        });

        let mut intents = HashMap::new();
        intents.insert("feat1".to_string(), BranchIntent {
            parent: Some(Some("master2".to_string())),
            ..Default::default()
        });

        let snapshot = RepositorySnapshot { branches, history, is_dirty: false };
        let plan = calculate_plan(&snapshot, &intents).unwrap();

        // Property: For any two rebases, if A is an ancestor of B, Rebase(A) must come before Rebase(B)
        let rebase_indices: std::collections::HashMap<_, _> = plan.iter().enumerate().filter_map(|(i, op)| {
            if let Operation::Rebase { branch, .. } = op {
                Some((branch.as_str(), i))
            } else {
                None
            }
        }).collect();

        for i in 1..5 {
            let parent = format!("feat{}", i);
            let child = format!("feat{}", i + 1);

            if let (Some(&p_idx), Some(&c_idx)) = (rebase_indices.get(parent.as_str()), rebase_indices.get(child.as_str())) {
                prop_assert!(p_idx < c_idx, "Parent rebase of '{}' (idx {}) must come before child rebase of '{}' (idx {})", parent, p_idx, child, c_idx);
            }
        }
    }

    #[test]
    fn test_plan_parallel_subtree_rebases(
        _ in 0..1u32
    ) {
        let mut branches = Vec::new();
        let mut history = HistoryContext::new();

        let oid_m = mock_oid(0);
        let oid_f1 = mock_oid(1);
        let oid_f1c = mock_oid(2);
        let oid_f2 = mock_oid(3);
        let oid_f2c = mock_oid(4);
        let oid_o1 = mock_oid(10);
        let oid_o2 = mock_oid(20);

        branches.push(make_branch("master", oid_m, None));
        branches.push(make_branch("feat1", oid_f1, Some("master")));
        branches.push(make_branch("feat1_child", oid_f1c, Some("feat1")));
        branches.push(make_branch("feat2", oid_f2, Some("master")));
        branches.push(make_branch("feat2_child", oid_f2c, Some("feat2")));
        branches.push(make_branch("other1", oid_o1, None));
        branches.push(make_branch("other2", oid_o2, None));

        history.oid_to_visible_branch.insert(oid_m, "master".to_string());
        history.oid_to_visible_branch.insert(oid_f1, "feat1".to_string());
        history.oid_to_visible_branch.insert(oid_f1c, "feat1_child".to_string());
        history.oid_to_visible_branch.insert(oid_f2, "feat2".to_string());
        history.oid_to_visible_branch.insert(oid_f2c, "feat2_child".to_string());
        history.oid_to_visible_branch.insert(oid_o1, "other1".to_string());
        history.oid_to_visible_branch.insert(oid_o2, "other2".to_string());

        history.oid_to_ancestor.insert(oid_f1, Some(oid_m));
        history.oid_to_ancestor.insert(oid_f1c, Some(oid_f1));
        history.oid_to_ancestor.insert(oid_f2, Some(oid_m));
        history.oid_to_ancestor.insert(oid_f2c, Some(oid_f2));

        // Move feat1 to other1, feat2 to other2
        let mut intents = HashMap::new();
        intents.insert("feat1".to_string(), BranchIntent {
            parent: Some(Some("other1".to_string())),
            ..Default::default()
        });
        intents.insert("feat2".to_string(), BranchIntent {
            parent: Some(Some("other2".to_string())),
            ..Default::default()
        });

        let snapshot = RepositorySnapshot { branches, history, is_dirty: false };
        let plan = calculate_plan(&snapshot, &intents).unwrap();

        let rebased: HashSet<_> = plan.iter().filter_map(|op| {
            if let Operation::Rebase { branch, .. } = op { Some(branch.as_str()) } else { None }
        }).collect();

        prop_assert!(rebased.contains("feat1"));
        prop_assert!(rebased.contains("feat1_child"));
        prop_assert!(rebased.contains("feat2"));
        prop_assert!(rebased.contains("feat2_child"));
    }
}

// end of file
