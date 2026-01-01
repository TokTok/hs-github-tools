use gitui::engine::{BranchIntent, Git, RealGit};
use gitui::testing::{create_commit, run_git, setup_repo};
use std::collections::HashMap;

#[test]
fn test_integration_sync_master_from_upstream() {
    let (dir, repo, mut master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    if master != "master" && master != "main" {
        run_git(path_str, &["branch", "-m", &master, "master"]);
        master = "master".to_string();
    }

    // 1. Setup 'upstream' remote
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

    // 2. Setup 'origin' remote
    let origin_dir = tempfile::tempdir().unwrap();
    run_git(origin_dir.path().to_str().unwrap(), &["init", "--bare"]);
    run_git(
        path_str,
        &[
            "remote",
            "add",
            "origin",
            origin_dir.path().to_str().unwrap(),
        ],
    );

    // 3. Make 'upstream/master' ahead
    create_commit(&repo, "upstream.txt", "upstream", "Upstream commit");
    let upstream_oid = run_git(path_str, &["rev-parse", "HEAD"]);
    run_git(
        path_str,
        &["push", "upstream", &format!("{}:{}", master, master)],
    );

    // Reset local master back to initial commit
    run_git(path_str, &["reset", "--hard", "HEAD^"]);
    let initial_oid = run_git(path_str, &["rev-parse", "HEAD"]);
    assert_ne!(initial_oid, upstream_oid);

    // 4. Setup local master to track origin/master (simulating a fork)
    run_git(
        path_str,
        &["push", "origin", &format!("{}:{}", master, master)],
    );
    run_git(
        path_str,
        &[
            "branch",
            &format!("--set-upstream-to=origin/{}", master),
            &master,
        ],
    );

    // 5. Create feat1 off the current master
    run_git(path_str, &["checkout", "-b", "feat1", &master]);
    create_commit(&repo, "feat1.txt", "feat1", "Add feat1");
    run_git(path_str, &["checkout", &master]);

    // Fetch upstream so it exists in our repo
    run_git(path_str, &["fetch", "upstream"]);

    // 6. Run gitui engine
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    // 7. Mark master for reset
    let mut intents = HashMap::new();
    intents.insert(
        master.clone(),
        BranchIntent {
            pending_reset: true,
            ..Default::default()
        },
    );

    // 8. Execute plan
    gitui::execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 9. Verify master is updated to upstream/master's OID
    let master_oid = run_git(path_str, &["rev-parse", &master]);
    assert_eq!(master_oid, upstream_oid);

    // 10. Verify origin/master is also updated (since Sync pushes to origin)
    run_git(path_str, &["fetch", "origin"]);
    let origin_oid = run_git(path_str, &["rev-parse", &format!("origin/{}", master)]);
    assert_eq!(origin_oid, upstream_oid);

    // 11. Verify feat1 was rebased onto new master
    let feat1_merge_base = run_git(path_str, &["merge-base", "feat1", &master]);
    assert_eq!(feat1_merge_base, master_oid);
}
