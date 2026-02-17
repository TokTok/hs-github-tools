use git2::BranchType;
use gitui::engine::{BranchIntent, Git, Intent, RealGit, build_topology, flatten_branches};
use gitui::testing::{create_commit, run_git, setup_repo};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

#[test]
fn test_integration_large_complex_tree() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // Create a deep chain
    let mut last_oid = repo.head().unwrap().target().unwrap();
    for i in 1..=20 {
        let branch_name = format!("chain-{}", i);
        repo.set_head_detached(last_oid).unwrap();
        last_oid = create_commit(
            &repo,
            &format!("f{}.txt", i),
            "content",
            &format!("commit {}", i),
        );
        repo.branch(&branch_name, &repo.find_commit(last_oid).unwrap(), false)
            .unwrap();
    }

    // Create many side branches at different levels
    for i in (1..=20).step_by(2) {
        let parent_branch = format!("chain-{}", i);
        let parent_oid = repo
            .find_branch(&parent_branch, BranchType::Local)
            .unwrap()
            .get()
            .target()
            .unwrap();
        for j in 1..=5 {
            let branch_name = format!("side-{}-{}", i, j);
            repo.set_head_detached(parent_oid).unwrap();
            let side_oid = create_commit(
                &repo,
                &format!("s{}-{}.txt", i, j),
                "content",
                &format!("side commit {}-{}", i, j),
            );
            repo.branch(&branch_name, &repo.find_commit(side_oid).unwrap(), false)
                .unwrap();
        }
    }

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let intents = HashMap::new();

    assert!(branches.len() >= 70);
    let flattened = flatten_branches(&branches, &intents, &history, false).unwrap();
    assert_eq!(flattened.len(), branches.len());

    let chain_20_depth = flattened
        .iter()
        .find(|(n, _)| n == "chain-20")
        .map(|(_, d)| *d)
        .unwrap();
    assert_eq!(chain_20_depth, 20);
}

#[test]
fn test_integration_commit_bridge() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    let c1 = create_commit(&repo, "1.txt", "1", "c1");
    let c2 = create_commit(&repo, "2.txt", "2", "c2");

    repo.branch("feature", &repo.find_commit(c2).unwrap(), false)
        .unwrap();

    repo.set_head_detached(c1).unwrap();
    repo.branch(&master, &repo.find_commit(c1).unwrap(), true)
        .unwrap();
    repo.set_head(&format!("refs/heads/{}", master)).unwrap();

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    intents.insert(
        "feature".to_string(),
        BranchIntent {
            parent: Some(Some(master.clone())),
            ..Default::default()
        },
    );

    let flattened = flatten_branches(&branches, &intents, &history, false).unwrap();
    let feature_depth = flattened
        .iter()
        .find(|(n, _)| n == "feature")
        .map(|(_, d)| *d)
        .unwrap();
    assert_eq!(feature_depth, 1);
}

#[test]
fn test_integration_cycle_prevention_on_real_repo() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    create_commit(&repo, "f1.txt", "1", "c1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    run_git(path_str, &["checkout", "feat1"]);
    create_commit(&repo, "f2.txt", "2", "c2");
    repo.branch(
        "feat2",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let intents = HashMap::new();

    let mut topo = build_topology(&branches, &intents, &history, false).unwrap();
    let res = topo.set_parent(&master, Some("feat2"), None, Intent::Structural);

    assert!(res.is_err());
    assert!(res.unwrap_err().to_string().contains("cycle"));
}

#[test]
fn test_integration_is_dirty_ignores_submodules() {
    let (dir, _repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    let sub_dir = tempfile::tempdir().unwrap();
    run_git(sub_dir.path().to_str().unwrap(), &["init"]);
    File::create(sub_dir.path().join("sub.txt"))
        .unwrap()
        .write_all(b"sub content")
        .unwrap();
    run_git(sub_dir.path().to_str().unwrap(), &["add", "sub.txt"]);
    run_git(
        sub_dir.path().to_str().unwrap(),
        &["commit", "-m", "Sub commit"],
    );

    std::process::Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args([
            "-c",
            "protocol.file.allow=always",
            "submodule",
            "add",
            sub_dir.path().to_str().unwrap(),
            "mysub",
        ])
        .status()
        .unwrap();

    run_git(path_str, &["commit", "-m", "Add submodule"]);

    let git = RealGit::new(path_str).unwrap();
    assert!(!git.is_dirty().unwrap());

    File::create(dir.path().join("mysub").join("sub.txt"))
        .unwrap()
        .write_all(b"modified sub content")
        .unwrap();

    assert!(!git.is_dirty().unwrap());

    File::create(dir.path().join("dirty.txt"))
        .unwrap()
        .write_all(b"dirty")
        .unwrap();
    run_git(path_str, &["add", "dirty.txt"]);
    assert!(git.is_dirty().unwrap());
}

#[test]
fn test_integration_commit_log_stopping() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    run_git(path_str, &["checkout", "--detach"]);

    let c1 = create_commit(&repo, "1.txt", "1", "Commit 1");
    repo.branch("feat1", &repo.find_commit(c1).unwrap(), false)
        .unwrap();

    let _c2 = create_commit(&repo, "2.txt", "2", "Commit 2");
    let c3 = create_commit(&repo, "3.txt", "3", "Commit 3");
    repo.branch("feat2", &repo.find_commit(c3).unwrap(), false)
        .unwrap();

    let git = RealGit::new(path_str).unwrap();
    let logs = git.get_commit_log("feat2").unwrap();

    assert_eq!(logs.len(), 2);
    assert!(logs[0].summary.contains("Commit 3"));
    assert!(logs[1].summary.contains("Commit 2"));
    assert!(!logs.iter().any(|l| l.summary.contains("Commit 1")));

    let logs1 = git.get_commit_log("feat1").unwrap();
    assert_eq!(logs1.len(), 1);
    assert!(logs1[0].summary.contains("Commit 1"));
}

#[test]
fn test_integration_commit_log_stack_uniqueness() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    run_git(path_str, &["checkout", "--detach"]);

    let c1 = create_commit(&repo, "f1.txt", "1", "Commit 1 (feat1)");
    repo.branch("feat1", &repo.find_commit(c1).unwrap(), false)
        .unwrap();

    let c2 = create_commit(&repo, "f2.txt", "2", "Commit 2 (feat2)");
    repo.branch("feat2", &repo.find_commit(c2).unwrap(), false)
        .unwrap();

    let master_oid = repo
        .find_branch(&master, BranchType::Local)
        .unwrap()
        .get()
        .target()
        .unwrap();
    run_git(path_str, &["checkout", "--detach", &master_oid.to_string()]);
    let s1 = create_commit(&repo, "s1.txt", "s", "Side commit 1");
    repo.branch("side1", &repo.find_commit(s1).unwrap(), false)
        .unwrap();

    let git = RealGit::new(path_str).unwrap();

    let logs_f1 = git.get_commit_log("feat1").unwrap();
    assert_eq!(logs_f1.len(), 1);
    assert!(logs_f1[0].summary.contains("Commit 1 (feat1)"));

    let logs_f2 = git.get_commit_log("feat2").unwrap();
    assert_eq!(logs_f2.len(), 1);
    assert!(logs_f2[0].summary.contains("Commit 2 (feat2)"));

    repo.branch("feat-empty", &repo.find_commit(c2).unwrap(), false)
        .unwrap();
    let logs_empty = git.get_commit_log("feat-empty").unwrap();
    assert_eq!(logs_empty.len(), 0);
}

#[test]
fn test_get_branches_progress() {
    let (dir, _repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();
    let git = RealGit::new(path_str).unwrap();

    let progress_calls = std::sync::Mutex::new(Vec::new());
    let progress = |msg: String, pct: f64| {
        progress_calls.lock().unwrap().push((msg, pct));
    };

    let _ = git.get_branches(Some(&progress)).unwrap();

    let calls = progress_calls.lock().unwrap();
    assert!(!calls.is_empty());
    assert!(calls.iter().any(|(m, _)| m.contains("Collecting")));
}
