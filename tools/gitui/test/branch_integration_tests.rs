use gitui::engine::{BranchIntent, Git, RealGit};
use gitui::execute_plan;
use gitui::state::AppState;
use gitui::testing::{create_commit, run_git, setup_repo};
use std::collections::HashMap;

#[test]
fn test_integration_push() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup a "remote"
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

    // 2. Create a branch and a commit
    create_commit(&repo, "push.txt", "push me", "Push commit");
    repo.branch(
        "push-branch",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 3. Mark for push
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();
    intents.insert(
        "push-branch".to_string(),
        BranchIntent {
            pending_push: true,
            ..Default::default()
        },
    );

    execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 4. Verify it was pushed to remote
    let output = run_git(path_str, &["ls-remote", "origin", "push-branch"]);
    assert!(!output.is_empty(), "Branch should exist on remote");
}

#[test]
fn test_integration_delete() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create a branch
    create_commit(&repo, "delete.txt", "delete me", "Delete commit");
    repo.branch(
        "delete-branch",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 2. Merge it into master so it's safe to delete with -d
    run_git(path_str, &["checkout", "main"]); // assuming main/master
    run_git(path_str, &["merge", "delete-branch"]);

    // 3. Verify is_merged is true
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    {
        let b = branches.iter().find(|b| b.name == "delete-branch").unwrap();
        assert!(b.is_merged, "Branch should be marked as merged");
    }

    // 4. Mark for delete
    let mut intents = HashMap::new();
    intents.insert(
        "delete-branch".to_string(),
        BranchIntent {
            pending_delete: true,
            ..Default::default()
        },
    );

    execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 4. Verify it's gone
    let output = std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["rev-parse", "--verify", "delete-branch"])
        .status()
        .unwrap();
    assert!(!output.success(), "Branch should be deleted");
}

#[test]
fn test_integration_origin_remote_filtering() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup two remotes
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

    let other_dir = tempfile::tempdir().unwrap();
    run_git(other_dir.path().to_str().unwrap(), &["init", "--bare"]);
    run_git(
        path_str,
        &["remote", "add", "other", other_dir.path().to_str().unwrap()],
    );

    // 2. Create commits and push to all
    create_commit(&repo, "o.txt", "origin content", "Push to origin");
    run_git(path_str, &["push", "origin", "HEAD:origin-branch"]);

    create_commit(&repo, "u.txt", "upstream content", "Push to upstream");
    run_git(path_str, &["push", "upstream", "HEAD:upstream-branch"]);

    create_commit(&repo, "x.txt", "other content", "Push to other");
    run_git(path_str, &["push", "other", "HEAD:other-branch"]);

    // 3. Fetch so they appear as remote branches
    run_git(path_str, &["fetch", "--all"]);

    let git = RealGit::new(path_str).unwrap();

    // 4. Test with show_remote = true
    let (branches, _history) = git.get_branches(None).unwrap();

    // Should find origin/origin-branch
    assert!(
        branches.iter().any(|b| b.name == "origin/origin-branch"),
        "origin/origin-branch should be visible"
    );

    // Should find upstream/upstream-branch
    assert!(
        branches
            .iter()
            .any(|b| b.name == "upstream/upstream-branch"),
        "upstream/upstream-branch should be visible"
    );

    // Should NOT find other/other-branch
    assert!(
        !branches.iter().any(|b| b.name == "other/other-branch"),
        "other/other-branch should NOT be visible"
    );
}

#[test]
fn test_integration_branch_aliasing() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

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

    // 2. Create a commit on master and push it
    create_commit(&repo, "alias.txt", "alias content", "Alias commit");
    run_git(
        path_str,
        &["push", "origin", &format!("{}:{}", master, master)],
    );

    // 3. Fetch so origin/master exists at the same commit as master
    run_git(path_str, &["fetch", "origin"]);

    // 4. Get branches with show_remote = true
    let git = RealGit::new(path_str).unwrap();
    let (branches, _history) = git.get_branches(None).unwrap();

    // 5. Verify that master has origin/master as an alias
    let master_info = branches
        .iter()
        .find(|b| b.name == master)
        .expect("master branch not found");
    assert!(
        master_info.aliases.contains(&format!("origin/{}", master)),
        "origin/master should be an alias of master"
    );
}

#[test]
fn test_integration_multiple_aliases_sorting() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup origin remote
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

    // 2. Create a commit and push to multiple branch names on origin
    create_commit(&repo, "sort.txt", "content", "Sort commit");
    let aliases = ["z-alias", "a-alias", master.as_str()];
    for alias in aliases {
        run_git(path_str, &["push", "origin", &format!("HEAD:{}", alias)]);
    }
    run_git(path_str, &["fetch", "origin"]);

    // 3. Get branches
    let git = RealGit::new(path_str).unwrap();
    let (branches, _history) = git.get_branches(None).unwrap();

    // 4. Verify sorting (a-alias, master, z-alias) - prefixed with origin/
    let master_info = branches.iter().find(|b| b.name == master).unwrap();
    let expected_aliases = vec![
        format!("origin/a-alias"),
        format!("origin/{}", master),
        format!("origin/z-alias"),
    ];
    assert_eq!(master_info.aliases, expected_aliases);
}

#[test]
fn test_integration_local_branches_at_same_commit() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create another local branch at the same commit
    repo.branch(
        "other-local",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 2. Get branches
    let git = RealGit::new(path_str).unwrap();
    let (branches, _history) = git.get_branches(None).unwrap();

    // 3. Verify that 'other-local' is NOT an alias but its own branch
    let master_info = branches.iter().find(|b| b.name == master).unwrap();
    assert!(
        master_info.aliases.is_empty(),
        "master should not have local aliases"
    );

    let other_info = branches
        .iter()
        .find(|b| b.name == "other-local")
        .expect("other-local branch not found");
    assert_eq!(
        other_info.original_parent.as_deref(),
        Some(master.as_str()),
        "other-local should be a child of master"
    );
}

#[test]
fn test_integration_remote_only_branch_visibility() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

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

    // 2. Create a branch on remote only
    run_git(path_str, &["checkout", &master]);
    run_git(path_str, &["checkout", "-b", "remote-only"]);
    create_commit(&repo, "remote.txt", "content", "Remote commit");
    run_git(path_str, &["push", "origin", "remote-only"]);
    run_git(path_str, &["checkout", &master]);
    run_git(path_str, &["branch", "-D", "remote-only"]);
    run_git(path_str, &["fetch", "origin"]);

    // 3. Initialize AppState with show_remote = true
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        show_remote: true,
        ..AppState::default()
    };
    state.refresh_tree(None);

    // 4. Verify origin/remote-only is visible
    assert!(
        state
            .flattened_tree
            .iter()
            .any(|(n, _)| n == "origin/remote-only"),
        "origin/remote-only should be visible when show_remote is true"
    );

    // 5. Toggle show_remote = false
    state.show_remote = false;
    state.refresh_tree(None);

    // 6. Verify origin/remote-only is NOT visible
    assert!(
        !state
            .flattened_tree
            .iter()
            .any(|(n, _)| n == "origin/remote-only"),
        "origin/remote-only should NOT be visible when show_remote is false"
    );
}

#[test]
fn test_integration_alias_parent_resolution() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup origin/master alias
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
    run_git(
        path_str,
        &["push", "origin", &format!("{}:{}", master, master)],
    );
    run_git(path_str, &["fetch", "origin"]);

    // 2. Create feature branch from master
    create_commit(&repo, "f.txt", "f", "Feature commit");
    repo.branch(
        "feature",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 3. Get branches - origin/master will be an alias of master
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        show_remote: false,
        ..AppState::default()
    };

    // 4. Simulate a move where parent is set to the alias name
    state.mutate_intent("feature", |i| {
        i.parent = Some(Some(format!("origin/{}", master)));
    });

    // 5. Refresh tree
    state.refresh_tree(None);
    let flattened = &state.flattened_tree;

    // 6. Verify feature is still a child (depth 1) of master (depth 0), not a root
    let master_depth = flattened
        .iter()
        .find(|(n, _)| n == &master)
        .map(|(_, d)| *d)
        .expect("master not found");
    let feature_depth = flattened
        .iter()
        .find(|(n, _)| n == "feature")
        .map(|(_, d)| *d)
        .expect("feature not found");

    assert_eq!(master_depth, 0);
    assert_eq!(
        feature_depth, 1,
        "Feature should be a child of master even if parented to its alias"
    );
}

#[test]
fn test_integration_localize_existing_branch() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

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

    // 2. Create 'feat' on remote
    run_git(path_str, &["checkout", "-b", "feat"]);
    create_commit(&repo, "feat.txt", "remote content", "Remote commit");
    run_git(path_str, &["push", "origin", "feat"]);
    run_git(path_str, &["checkout", &master]);

    // 3. Create a local 'feat' at a DIFFERENT commit (off master)
    create_commit(&repo, "local_feat.txt", "local content", "Local commit");
    run_git(path_str, &["branch", "-f", "feat", "HEAD"]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    // 4. Find origin/feat and mark for localization
    intents.insert(
        "origin/feat".to_string(),
        BranchIntent {
            pending_localize: true,
            ..Default::default()
        },
    );

    // 5. Execute plan (should use checkout -B)
    execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 6. Verify local 'feat' now matches 'origin/feat'
    let feat_oid = run_git(path_str, &["rev-parse", "feat"]);
    let origin_feat_oid = run_git(path_str, &["rev-parse", "origin/feat"]);
    assert_eq!(
        feat_oid, origin_feat_oid,
        "Local 'feat' should match 'origin/feat'"
    );

    // Verify it's tracking
    let upstream = run_git(path_str, &["config", "branch.feat.merge"]);
    assert_eq!(upstream, "refs/heads/feat");
    let remote = run_git(path_str, &["config", "branch.feat.remote"]);
    assert_eq!(remote, "origin");
}
