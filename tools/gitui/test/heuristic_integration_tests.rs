use gitui::engine::{BranchIntent, Git, RealGit};
use gitui::execute_plan;
use gitui::testing::{create_commit, run_git, setup_repo};
use std::collections::HashMap;

#[test]
fn test_integration_diverged_branch_detection() {
    let (dir, repo, _main) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // Detach HEAD so main doesn't move
    repo.set_head_detached(repo.head().unwrap().target().unwrap())
        .unwrap();

    // 1. Create a base branch 'feat1'
    create_commit(&repo, "f1.txt", "f1 content", "feat1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 2. Create 'feat2' based on 'feat1'
    run_git(path_str, &["checkout", "feat1"]);
    create_commit(&repo, "f2.txt", "f2 content", "Add feat2");
    repo.branch(
        "feat2",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 3. Rebase 'feat1' so 'feat2' becomes diverged
    run_git(path_str, &["checkout", "feat1"]);
    std::fs::write(dir.path().join("f1.txt"), "f1 amended").unwrap();
    run_git(path_str, &["commit", "--amend", "-m", "feat1", "-a"]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, _) = git.get_branches(None).unwrap();

    let feat2 = branches
        .iter()
        .find(|b| b.name == "feat2")
        .expect("feat2 not found");

    assert_eq!(feat2.heuristic_parent, Some("feat1".to_string()));
}

#[test]
fn test_integration_converge_operation() {
    let (dir, repo, _main) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // Detach HEAD so main doesn't move
    repo.set_head_detached(repo.head().unwrap().target().unwrap())
        .unwrap();

    // 1. Create 'feat1'
    create_commit(&repo, "f1.txt", "f1", "feat1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 2. Create 'feat2' dependent on 'feat1'
    run_git(path_str, &["checkout", "feat1"]);
    create_commit(&repo, "f2.txt", "f2", "feat2");
    repo.branch(
        "feat2",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 3. Rewrite 'feat1'
    run_git(path_str, &["checkout", "feat1"]);
    std::fs::write(dir.path().join("f1.txt"), "f1 amended").unwrap();
    run_git(path_str, &["commit", "--amend", "-m", "feat1", "-a"]);
    let new_feat1_oid = run_git(path_str, &["rev-parse", "feat1"]);

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    {
        let feat2 = branches.iter().find(|b| b.name == "feat2").unwrap();
        intents.insert(
            "feat2".to_string(),
            BranchIntent {
                parent: Some(feat2.heuristic_parent.clone()),
                ..Default::default()
            },
        );
    }

    // 4. Execute plan
    execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 5. Verify feat2 is now on top of new feat1
    let merge_base = run_git(path_str, &["merge-base", "feat2", "feat1"]);
    assert_eq!(
        merge_base, new_feat1_oid,
        "feat2 should be rebased onto new feat1"
    );
}
