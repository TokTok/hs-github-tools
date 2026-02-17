use gitui::engine::{
    BranchIntent, Git, Operation, RealGit, RepositorySnapshot, calculate_plan, predict_conflicts,
};
use gitui::testing::{run_git, setup_repo};
use std::collections::HashMap;

#[test]
fn test_sync_conflict_prediction_uses_new_master() {
    let (dir, _repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create a file in initial commit
    let path_conflict = dir.path().join("conflict.txt");
    std::fs::write(&path_conflict, "initial\n").unwrap();
    run_git(path_str, &["add", "conflict.txt"]);
    run_git(path_str, &["commit", "-m", "Add conflict.txt"]);

    // 2. Setup 'upstream' remote
    let upstream_dir = tempfile::tempdir().unwrap();
    run_git(upstream_dir.path().to_str().unwrap(), &["init", "--bare"]);
    run_git(
        path_str,
        &[
            "remote",
            "add",
            "upstream",
            upstream_dir.path().to_str().unwrap(),
        ],
    );

    // 3. Create a conflict in upstream/master
    // Current master has "initial" in conflict.txt.
    // upstream/master will have "upstream change" in conflict.txt.
    std::fs::write(&path_conflict, "upstream change\n").unwrap();
    run_git(path_str, &["add", "conflict.txt"]);
    run_git(path_str, &["commit", "-m", "Upstream change"]);
    run_git(
        path_str,
        &["push", "upstream", &format!("{}:{}", master, master)],
    );

    // 4. Reset local master back to "initial" state (one commit back)
    run_git(path_str, &["reset", "--hard", "HEAD^"]);

    // 5. Create a feature branch that ALSO modifies conflict.txt (conflicting with upstream)
    // but it DOES NOT conflict with current local master.
    run_git(path_str, &["checkout", "-b", "feat", &master]);
    std::fs::write(&path_conflict, "local change\n").unwrap();
    run_git(path_str, &["add", "conflict.txt"]);
    run_git(path_str, &["commit", "-m", "Local change"]);

    // 6. Set master to track origin (simulated fork)
    let origin_dir = tempfile::tempdir().unwrap();
    run_git(origin_dir.path().to_str().unwrap(), &["init", "--bare"]);
    run_git(path_str, &["checkout", &master]);
    run_git(
        path_str,
        &[
            "remote",
            "add",
            "origin",
            origin_dir.path().to_str().unwrap(),
        ],
    );
    run_git(
        path_str,
        &[
            "push",
            "--force",
            "origin",
            &format!("{}:{}", master, master),
        ],
    );
    run_git(
        path_str,
        &[
            "branch",
            &format!("--set-upstream-to=origin/{}", master),
            &master,
        ],
    );

    // Fetch upstream to have the conflicting commit locally
    run_git(path_str, &["fetch", "upstream"]);

    // 7. Initialize Git and Snapshot
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let is_dirty = git.is_dirty().unwrap();

    let mut intents = HashMap::new();
    intents.insert(
        master.clone(),
        BranchIntent {
            pending_reset: true,
            ..Default::default()
        },
    );

    let snapshot = RepositorySnapshot {
        branches,
        history,
        is_dirty,
    };

    // 8. Calculate plan and predict conflicts
    let mut plan = calculate_plan(&snapshot, &intents).unwrap();
    predict_conflicts(&mut plan, &git, &snapshot.branches, None);

    // 9. Verify that 'feat' rebase is marked as CONFLICT
    let feat_rebase = plan.iter().find(|op| {
        if let Operation::Rebase { branch, .. } = op {
            branch == "feat"
        } else {
            false
        }
    });

    if feat_rebase.is_none() {
        panic!("Rebase operation for 'feat' not found. Plan: {:?}", plan);
    }

    if let Some(Operation::Rebase {
        predicted_conflict, ..
    }) = feat_rebase
    {
        assert_eq!(
            *predicted_conflict,
            Some(true),
            "Rebase of 'feat' should be predicted as CONFLICT because of upstream changes in master. Plan: {:?}",
            plan
        );
    }
}
