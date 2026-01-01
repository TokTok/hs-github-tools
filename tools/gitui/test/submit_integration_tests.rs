use gitui::engine::{BranchIntent, Git, RealGit};
use gitui::execute_plan;
use gitui::testing::{create_commit, run_git, setup_repo};
use std::collections::HashMap;

#[test]
fn test_integration_submit_readiness() {
    let (dir, repo, initial_branch) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // Ensure we have a branch named "master"
    let master = if initial_branch != "master" {
        run_git(path_str, &["branch", "-m", &initial_branch, "master"]);
        "master".to_string()
    } else {
        initial_branch
    };

    let base_oid = run_git(path_str, &["rev-parse", &master]);

    // 1. Setup a remote
    let remote_dir = tempfile::tempdir().unwrap();
    run_git(remote_dir.path().to_str().unwrap(), &["init", "--bare"]);
    run_git(
        path_str,
        &[
            "remote",
            "add",
            "origin",
            remote_dir.path().to_str().unwrap(),
        ],
    );

    // 2. feat-ready
    run_git(path_str, &["checkout", "-b", "feat-ready", &base_oid]);
    create_commit(&repo, "ready.txt", "ready", "Commit ready");
    run_git(path_str, &["push", "origin", "feat-ready"]);
    run_git(
        path_str,
        &[
            "branch",
            "--set-upstream-to=origin/feat-ready",
            "feat-ready",
        ],
    );
    run_git(path_str, &["fetch", "origin"]);

    // 3. feat-not-pushed
    run_git(path_str, &["checkout", "-b", "feat-not-pushed", &base_oid]);
    create_commit(&repo, "no-push.txt", "no-push", "Commit no-push");

    // 4. feat-out-of-sync
    run_git(path_str, &["checkout", "-b", "feat-out-of-sync", &base_oid]);
    create_commit(&repo, "oos.txt", "oos", "Commit oos");
    run_git(path_str, &["push", "origin", "feat-out-of-sync"]);
    run_git(
        path_str,
        &[
            "branch",
            "--set-upstream-to=origin/feat-out-of-sync",
            "feat-out-of-sync",
        ],
    );
    run_git(path_str, &["fetch", "origin"]);
    create_commit(&repo, "oos2.txt", "oos2", "Commit oos2");

    {
        let git = RealGit::new(path_str).unwrap();
        let (branches, _) = git.get_branches(None).unwrap();
        let get_b = |name: &str| {
            branches
                .iter()
                .find(|b| b.name == name)
                .expect("branch not found")
        };
        let is_ready = |name: &str| {
            let b = get_b(name);
            b.can_submit()
        };

        assert!(get_b("feat-ready").is_ahead);
        assert!(is_ready("feat-ready"));
        assert!(!is_ready("feat-not-pushed"));
        assert!(!is_ready("feat-out-of-sync"));
    }

    // 5. feat-merged
    run_git(path_str, &["checkout", "-b", "feat-merged", &base_oid]);
    create_commit(&repo, "merged.txt", "merged", "Commit merged");
    run_git(path_str, &["checkout", &master]);
    run_git(path_str, &["merge", "feat-merged"]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, _) = git.get_branches(None).unwrap();
    let get_b = |name: &str| {
        branches
            .iter()
            .find(|b| b.name == name)
            .expect("branch not found")
    };
    let is_ready = |name: &str| {
        let b = get_b(name);
        b.can_submit()
    };

    assert!(!get_b("feat-ready").is_ahead);
    assert!(!is_ready("feat-ready"));

    let fm = get_b("feat-merged");
    assert!(fm.is_merged);
    assert!(!is_ready("feat-merged"));
}

#[test]
fn test_integration_complex_submit_execution() {
    let (dir, repo, initial_branch) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    let master = if initial_branch != "master" {
        run_git(path_str, &["branch", "-m", &initial_branch, "master"]);
        "master".to_string()
    } else {
        initial_branch
    };

    let base_oid = run_git(path_str, &["rev-parse", "HEAD"]);

    // 1. Setup remotes
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

    // 2. Setup tree
    run_git(path_str, &["checkout", "-b", "feat-a", &base_oid]);
    create_commit(&repo, "a.txt", "a", "Commit A");
    run_git(path_str, &["checkout", "-b", "feat-b"]);
    create_commit(&repo, "b.txt", "b", "Commit B");
    run_git(path_str, &["push", "origin", "feat-b"]);
    run_git(
        path_str,
        &["branch", "--set-upstream-to=origin/feat-b", "feat-b"],
    );
    run_git(path_str, &["checkout", "-b", "feat-c"]);
    create_commit(&repo, "c.txt", "c", "Commit C");
    run_git(path_str, &["checkout", "-b", "feat-d", &base_oid]);
    create_commit(&repo, "d.txt", "d", "Commit D");

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    // 3. Mark feat-b for submit
    {
        let _b = branches.iter().find(|b| b.name == "feat-b").unwrap();
        intents.insert(
            "feat-b".to_string(),
            BranchIntent {
                pending_submit: true,
                ..Default::default()
            },
        );
    }

    // 4. Execute plan
    execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 5. Verify results
    let master_oid = run_git(path_str, &["rev-parse", &master]);
    let origin_feat_b = run_git(path_str, &["ls-remote", "origin", "feat-b"]);
    assert!(origin_feat_b.is_empty());
    let upstream_master_oid = run_git(path_str, &["ls-remote", "upstream", &master]);
    assert!(upstream_master_oid.contains(&master_oid));

    let merge_base_c = run_git(path_str, &["merge-base", "feat-c", &master]);
    assert_eq!(merge_base_c, master_oid);
    let merge_base_d = run_git(path_str, &["merge-base", "feat-d", &master]);
    assert_eq!(merge_base_d, master_oid);
}
