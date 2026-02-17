use git2::BranchType;
use gitui::engine::{BranchIntent, Git, RealGit};
use gitui::execute_rebases;
use gitui::testing::{create_commit, run_git, setup_repo};
use std::collections::HashMap;
use std::process::Command;

#[test]
fn test_integration_simple_rebase() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();
    let initial_oid = repo.head().unwrap().target().unwrap();

    // Create feat1 from initial commit
    create_commit(&repo, "f1.txt", "f1 content", "Add feat1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // Reset master to initial commit and create new-base from there
    repo.set_head_detached(initial_oid).unwrap();
    Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["reset", "--hard", &initial_oid.to_string()])
        .status()
        .unwrap();
    repo.branch(&master, &repo.find_commit(initial_oid).unwrap(), true)
        .unwrap();
    repo.set_head(&format!("refs/heads/{}", master)).unwrap();

    create_commit(&repo, "nb.txt", "nb content", "Add new-base");
    repo.branch(
        "new-base",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    intents.insert(
        "feat1".to_string(),
        BranchIntent {
            parent: Some(Some("new-base".to_string())),
            ..Default::default()
        },
    );

    execute_rebases(&git, &branches, &intents, &history, path_str).unwrap();

    // After rebase, feat1 should have new-base as its parent in history
    let _ = RealGit::new(path_str).unwrap().get_branches(None).unwrap();

    let output = Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["merge-base", "feat1", "new-base"])
        .output()
        .unwrap();
    let merge_base = String::from_utf8(output.stdout).unwrap().trim().to_string();
    let nb_oid = repo
        .find_branch("new-base", BranchType::Local)
        .unwrap()
        .get()
        .target()
        .unwrap()
        .to_string();

    assert_eq!(merge_base, nb_oid, "feat1 should be rebased onto new-base");
}

#[test]
fn test_integration_recursive_rebase() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();
    let initial_oid = repo.head().unwrap().target().unwrap();

    // Create feat1 -> feat2 from initial commit
    create_commit(&repo, "f1.txt", "f1 content", "Add feat1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["checkout", "feat1"])
        .status()
        .unwrap();
    create_commit(&repo, "f2.txt", "f2 content", "Add feat2");
    repo.branch(
        "feat2",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // Reset master to initial commit and create new-base
    repo.set_head_detached(initial_oid).unwrap();
    Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["reset", "--hard", &initial_oid.to_string()])
        .status()
        .unwrap();
    repo.branch(&master, &repo.find_commit(initial_oid).unwrap(), true)
        .unwrap();
    repo.set_head(&format!("refs/heads/{}", master)).unwrap();

    create_commit(&repo, "nb.txt", "nb content", "Add new-base");
    repo.branch(
        "new-base",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    intents.insert(
        "feat1".to_string(),
        BranchIntent {
            parent: Some(Some("new-base".to_string())),
            ..Default::default()
        },
    );

    execute_rebases(&git, &branches, &intents, &history, path_str).unwrap();

    // Verify both rebased
    let nb_oid = repo
        .find_branch("new-base", BranchType::Local)
        .unwrap()
        .get()
        .target()
        .unwrap()
        .to_string();

    let output1 = Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["merge-base", "feat1", "new-base"])
        .output()
        .unwrap();
    assert_eq!(String::from_utf8(output1.stdout).unwrap().trim(), nb_oid);

    let output2 = Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["merge-base", "feat2", "new-base"])
        .output()
        .unwrap();
    assert_eq!(String::from_utf8(output2.stdout).unwrap().trim(), nb_oid);
}

#[test]
fn test_integration_reset_rebase() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create feat1
    create_commit(&repo, "f1.txt", "f1", "Add f1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 2. Create feat2 on top of feat1
    run_git(path_str, &["checkout", "feat1"]);
    create_commit(&repo, "f2.txt", "f2", "Add f2");
    repo.branch(
        "feat2",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 3. Create an "upstream" branch for feat1
    run_git(path_str, &["checkout", &master]);
    let initial_oid = run_git(path_str, &["rev-parse", "HEAD"]);
    create_commit(&repo, "u1.txt", "u1", "Add u1");
    let upstream_commit = repo.head().unwrap().peel_to_commit().unwrap();
    repo.branch("feat1-upstream", &upstream_commit, false)
        .unwrap();
    let upstream_oid = upstream_commit.id().to_string();

    // Reset master back to initial so feat1's parent remains master.
    run_git(path_str, &["reset", "--hard", &initial_oid]);

    run_git(
        path_str,
        &["branch", "--set-upstream-to=feat1-upstream", "feat1"],
    );

    // 4. Mark feat1 for reset in our engine
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    {
        let feat1 = branches
            .iter()
            .find(|b| b.name == "feat1")
            .expect("feat1 not found");
        // Ensure feat1 has master as parent
        assert_eq!(feat1.original_parent.as_ref().unwrap(), &master);
        intents.insert(
            "feat1".to_string(),
            BranchIntent {
                pending_reset: true,
                ..Default::default()
            },
        );
    }

    gitui::execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // Verify feat1 now has upstream as ancestor
    let merge_base = run_git(path_str, &["merge-base", "feat1", "feat1-upstream"]);
    assert_eq!(merge_base, upstream_oid);

    // Verify feat2 is now child of the new feat1
    let feat1_oid = run_git(path_str, &["rev-parse", "feat1"]);
    let merge_base2 = run_git(path_str, &["merge-base", "feat2", "feat1"]);
    assert_eq!(merge_base2, feat1_oid);
}

#[test]
fn test_integration_sync_root_rebase() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create feat1 on top of master
    create_commit(&repo, "f1.txt", "f1", "Add f1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 2. Create an upstream for master that is ahead
    let initial_oid = run_git(path_str, &["rev-parse", "HEAD"]);
    run_git(path_str, &["checkout", "--detach", &initial_oid]);

    create_commit(&repo, "m2.txt", "m2", "Add m2");
    let upstream_commit = repo.head().unwrap().peel_to_commit().unwrap();
    repo.branch("master-upstream", &upstream_commit, false)
        .unwrap();
    let upstream_oid = upstream_commit.id().to_string();

    // 3. Set master's upstream and go back to master
    run_git(path_str, &["checkout", &master]);
    run_git(
        path_str,
        &["branch", "--set-upstream-to=master-upstream", &master],
    );

    // 4. Mark master for reset/sync
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();
    intents.insert(
        master.clone(),
        BranchIntent {
            pending_reset: true,
            ..Default::default()
        },
    );

    gitui::execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 5. Verify master moved to upstream OID
    let master_oid = run_git(path_str, &["rev-parse", &master]);
    assert_eq!(master_oid, upstream_oid);

    // 6. Verify feat1 was rebased onto new master
    let merge_base = run_git(path_str, &["merge-base", "feat1", &master]);
    assert_eq!(merge_base, master_oid);
}

#[test]
fn test_integration_rebase_remote_branch() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup a remote and a branch on it
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

    let initial_oid = run_git(path_str, &["rev-parse", "HEAD"]);

    // Make master ahead of initial commit
    create_commit(&repo, "master.txt", "master", "Master ahead");

    // Create 'feat' on remote, off initial commit (not master)
    run_git(path_str, &["checkout", "--detach", &initial_oid]);
    run_git(path_str, &["checkout", "-b", "feat"]);
    create_commit(&repo, "feat.txt", "feat", "Add feat");
    run_git(path_str, &["push", "origin", "feat"]);
    run_git(path_str, &["checkout", &master]);
    run_git(path_str, &["branch", "-D", "feat"]);
    run_git(path_str, &["fetch", "origin"]);

    // 2. Get branches with show_remote = true
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    // 3. Move origin/feat to master
    let mut intents = HashMap::new();
    {
        let feat = branches
            .iter()
            .find(|b| b.name == "origin/feat")
            .expect("origin/feat not found");
        assert_ne!(feat.original_parent.as_deref(), Some(master.as_str()));
        intents.insert(
            "origin/feat".to_string(),
            BranchIntent {
                parent: Some(Some(master.clone())),
                ..Default::default()
            },
        );
    }

    // 4. Execute plan
    gitui::execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 5. Verify local branch 'feat' exists and is rebased onto master
    let master_oid = run_git(path_str, &["rev-parse", &master]);
    let merge_base = run_git(path_str, &["merge-base", "feat", &master]);

    assert_eq!(
        merge_base, master_oid,
        "Local branch 'feat' should be rebased onto master"
    );

    // Verify we are not on a detached HEAD
    let head_shorthand = run_git(path_str, &["rev-parse", "--abbrev-ref", "HEAD"]);
    assert_ne!(head_shorthand, "HEAD", "Should not be on a detached HEAD");
}
