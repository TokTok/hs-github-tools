use gitui::engine::{
    BranchInfo, BranchIntent, HistoryContext, Operation, RepositorySnapshot, calculate_plan,
};
use gitui::testing::mock_oid;
use std::collections::HashMap;

struct TestRepo {
    branches: HashMap<String, BranchInfo>,
    history: HistoryContext,
    intents: HashMap<String, BranchIntent>,
    is_dirty: bool,
}

impl TestRepo {
    fn new() -> Self {
        Self {
            branches: HashMap::new(),
            history: HistoryContext::new(),
            intents: HashMap::new(),
            is_dirty: false,
        }
    }

    fn branch(mut self, name: &str, oid_val: u8) -> Self {
        let oid = mock_oid(oid_val);
        let mut info = BranchInfo {
            name: name.to_string(),
            oid,
            is_local: !name.starts_with("origin/"),
            is_remote: name.starts_with("origin/"),
            ..Default::default()
        };
        if name == "master" || name == "main" {
            info.is_local = true;
        }
        self.branches.insert(name.to_string(), info);
        self.history
            .oid_to_visible_branch
            .insert(oid, name.to_string());
        self
    }

    fn parent(mut self, child: &str, parent: &str) -> Self {
        let p_oid = self.branches.get(parent).map(|b| b.oid);
        let c_oid = self.branches.get(child).map(|b| b.oid).unwrap();

        if let Some(mut b) = self.branches.remove(child) {
            b.original_parent = Some(parent.to_string());
            self.branches.insert(child.to_string(), b);
        }

        self.history.oid_to_ancestor.insert(c_oid, p_oid);
        self
    }

    fn heuristic(mut self, branch: &str, h_parent: &str, h_upstream_oid_val: u8) -> Self {
        if let Some(mut b) = self.branches.remove(branch) {
            b.heuristic_parent = Some(h_parent.to_string());
            b.heuristic_upstream_oid = Some(mock_oid(h_upstream_oid_val));
            self.branches.insert(branch.to_string(), b);
        }
        self
    }

    fn intent_move(mut self, branch: &str, new_parent: Option<&str>) -> Self {
        let is_remote = self
            .branches
            .get(branch)
            .map(|b| b.is_remote)
            .unwrap_or(false);
        let intent = self.intents.entry(branch.to_string()).or_default();
        intent.parent = Some(new_parent.map(|s| s.to_string()));
        if is_remote {
            intent.pending_localize = true;
        }
        self
    }

    fn intent_converge(mut self, branch: &str) -> Self {
        let h_parent = self
            .branches
            .get(branch)
            .and_then(|b| b.heuristic_parent.clone());
        let is_remote = self
            .branches
            .get(branch)
            .map(|b| b.is_remote)
            .unwrap_or(false);

        let intent = self.intents.entry(branch.to_string()).or_default();
        intent.parent = Some(h_parent);
        if is_remote {
            intent.pending_localize = true;
        }
        self
    }

    fn plan(&self) -> anyhow::Result<Vec<Operation>> {
        let snapshot = RepositorySnapshot {
            branches: self.branches.values().cloned().collect(),
            history: self.history.clone(),
            is_dirty: self.is_dirty,
        };
        calculate_plan(&snapshot, &self.intents)
    }
}

#[test]
fn test_plan_simple_rebase() {
    let plan = TestRepo::new()
        .branch("master", 1)
        .branch("feat", 2)
        .parent("feat", "master")
        .branch("new-base", 3)
        .intent_move("feat", Some("new-base"))
        .plan()
        .unwrap();

    assert!(plan.iter().any(|op| matches!(op, Operation::Rebase { branch, onto, .. } if branch == "feat" && onto == "new-base")));
}

#[test]
fn test_plan_transitive_rebase() {
    // A -> B -> C
    // Move A to master. Both B and C should rebase.
    let plan = TestRepo::new()
        .branch("master", 1)
        .branch("A", 2)
        .parent("A", "master")
        .branch("B", 3)
        .parent("B", "A")
        .branch("C", 4)
        .parent("C", "B")
        .branch("new-root", 5)
        .intent_move("A", Some("new-root"))
        .plan()
        .unwrap();

    let rebased_branches: Vec<_> = plan
        .iter()
        .filter_map(|op| match op {
            Operation::Rebase { branch, .. } => Some(branch.as_str()),
            _ => None,
        })
        .collect();

    assert!(rebased_branches.contains(&"A"));
    assert!(rebased_branches.contains(&"B"));
    assert!(rebased_branches.contains(&"C"));
}

#[test]
fn test_plan_converge_no_op_when_already_there() {
    // master (1) -> feat (2) -> subfeat (3)
    // subfeat heuristic says it belongs on feat (2)
    // The branch is already at the target location, so the plan should be empty.
    let plan = TestRepo::new()
        .branch("master", 1)
        .branch("feat", 2)
        .parent("feat", "master")
        .branch("subfeat", 3)
        .parent("subfeat", "feat")
        .heuristic("subfeat", "feat", 2)
        .intent_converge("subfeat")
        .plan()
        .unwrap();

    assert!(plan.is_empty(), "Should be empty plan, but got: {:?}", plan);
}

#[test]
fn test_plan_converge_cycle_prevention_error() {
    // master (1)
    //   owner (2) -> head summary matches hacks (3)
    //     hacks (3) -> child of owner (2)
    // Alphabetical tie-break makes hacks the heuristic representative.
    // owner heuristic says it should be under hacks (3).
    // hacks is already under owner (2).
    // Moving owner under hacks would be a cycle.
    // build_topology should return an Err.

    let res = TestRepo::new()
        .branch("master", 1)
        .branch("owner", 2)
        .parent("owner", "master")
        .branch("hacks", 3)
        .parent("hacks", "owner")
        .heuristic("owner", "hacks", 3)
        .intent_converge("owner")
        .plan();

    assert!(res.is_err());
    assert!(format!("{:?}", res.unwrap_err()).contains("cycle"));
}

#[test]
fn test_plan_rebase_to_different_heuristic_upstream() {
    // master (1)
    //   feat (2)
    //   subfeat (3) -> currently on master (1), but summary matches feat (2)
    let plan = TestRepo::new()
        .branch("master", 1)
        .branch("feat", 2)
        .parent("feat", "master")
        .branch("subfeat", 3)
        .parent("subfeat", "master")
        .heuristic("subfeat", "feat", 2)
        .intent_converge("subfeat")
        .plan()
        .unwrap();

    assert!(plan.iter().any(|op| matches!(op, Operation::Rebase { branch, onto, .. } if branch == "subfeat" && onto == "feat")));
}

#[test]
fn test_plan_localization_on_move() {
    // Moving a remote branch should trigger localization
    let plan = TestRepo::new()
        .branch("master", 1)
        .branch("origin/feat", 2)
        .parent("origin/feat", "master")
        .intent_move("origin/feat", Some("master")) // move it (even to same place)
        .plan()
        .unwrap();

    assert!(
        plan.iter()
            .any(|op| matches!(op, Operation::Localize { branch } if branch == "origin/feat"))
    );
}

#[test]
fn test_plan_no_rebase_after_delete_on_submit() {
    // master (1) -> A (2) -> B (3)
    // Submitting B should delete A and B, and master should be updated.
    // There should NOT be a Rebase(A) onto master.
    let mut repo = TestRepo::new()
        .branch("master", 1)
        .branch("A", 2)
        .parent("A", "master")
        .branch("B", 3)
        .parent("B", "A");

    // Mock that A is local and B is submittable
    if let Some(b) = repo.branches.get_mut("B") {
        b.upstream = Some("origin/B".to_string());
        b.is_ahead = true;
    }

    repo.intents.insert(
        "B".to_string(),
        BranchIntent {
            pending_submit: true,
            ..Default::default()
        },
    );

    let plan = repo.plan().unwrap();

    let deletes: Vec<_> = plan
        .iter()
        .filter_map(|op| match op {
            Operation::Delete { branch } => Some(branch.as_str()),
            _ => None,
        })
        .collect();

    let rebases: Vec<_> = plan
        .iter()
        .filter_map(|op| match op {
            Operation::Rebase { branch, .. } => Some(branch.as_str()),
            _ => None,
        })
        .collect();

    assert!(deletes.contains(&"A"), "A should be deleted");
    assert!(deletes.contains(&"B"), "B should be deleted");
    assert!(
        !rebases.contains(&"A"),
        "A should NOT be rebased after deletion. Plan: {:?}",
        plan
    );
}

#[test]
fn test_plan_rebase_upstream_set() {
    // master (1) -> feat (2)
    // new-base (3)
    // Moving feat to new-base.
    // Rebase(feat) onto new-base should have upstream=1 (master).
    let plan = TestRepo::new()
        .branch("master", 1)
        .branch("feat", 2)
        .parent("feat", "master")
        .branch("new-base", 3)
        .intent_move("feat", Some("new-base"))
        .plan()
        .unwrap();

    let rebase_op = plan
        .iter()
        .find(|op| matches!(op, Operation::Rebase { branch, .. } if branch == "feat"))
        .unwrap();
    if let Operation::Rebase { upstream, .. } = rebase_op {
        let expected = mock_oid(1).to_string();
        assert_eq!(upstream.as_deref(), Some(expected.as_str()));
    } else {
        panic!("Expected rebase operation");
    }
}

#[test]
fn test_plan_chain_rebase_upstreams() {
    let plan = TestRepo::new()
        .branch("master", 1)
        .branch("A", 2)
        .parent("A", "master")
        .branch("B", 3)
        .parent("B", "A")
        .branch("new-base", 4)
        .intent_move("A", Some("new-base"))
        .plan()
        .unwrap();

    let a_rebase = plan
        .iter()
        .find(|op| matches!(op, Operation::Rebase { branch, .. } if branch == "A"))
        .unwrap();
    if let Operation::Rebase { onto, upstream, .. } = a_rebase {
        assert_eq!(onto, "new-base");
        let expected = mock_oid(1).to_string();
        assert_eq!(upstream.as_deref(), Some(expected.as_str()));
    }

    let b_rebase = plan
        .iter()
        .find(|op| matches!(op, Operation::Rebase { branch, .. } if branch == "B"))
        .unwrap();
    if let Operation::Rebase { onto, upstream, .. } = b_rebase {
        assert_eq!(onto, "A");
        let expected = mock_oid(2).to_string();
        assert_eq!(upstream.as_deref(), Some(expected.as_str()));
    }
}
