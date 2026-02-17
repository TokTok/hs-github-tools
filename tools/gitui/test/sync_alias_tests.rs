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
            is_local: !name.starts_with("origin/") && !name.starts_with("upstream/"),
            is_remote: name.starts_with("origin/") || name.starts_with("upstream/"),
            ..Default::default()
        };
        if name == "master" || name == "main" {
            info.is_local = true;
            info.is_remote = false;
        }
        self.branches.insert(name.to_string(), info);
        self.history
            .oid_to_visible_branch
            .insert(oid, name.to_string());
        self
    }

    fn alias(mut self, branch: &str, alias: &str) -> Self {
        if let Some(mut b) = self.branches.remove(branch) {
            b.aliases.push(alias.to_string());
            self.branches.insert(branch.to_string(), b);
        }
        self
    }

    fn intent_sync(mut self, branch: &str) -> Self {
        let intent = self.intents.entry(branch.to_string()).or_default();
        intent.pending_reset = true;
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
fn test_sync_master_with_upstream_alias() {
    // master is at OID 1
    // settings-load is at OID 2
    // upstream/master is an alias for settings-load (so it's also at OID 2)
    // --sync master should result in a sync operation from upstream/master
    let plan = TestRepo::new()
        .branch("master", 1)
        .branch("settings-load", 2)
        .alias("settings-load", "upstream/master")
        .intent_sync("master")
        .plan()
        .unwrap();

    let sync_ops: Vec<_> = plan
        .iter()
        .filter_map(|op| match op {
            Operation::Sync { branch, onto, .. } => Some((branch.as_str(), onto.as_str())),
            _ => None,
        })
        .collect();

    assert_eq!(sync_ops, vec![("master", "upstream/master")]);
}
