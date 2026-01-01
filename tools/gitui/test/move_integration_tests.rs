use gitui::engine::{BranchIntent, Git, RealGit, apply_move};
use gitui::execute_plan;
use gitui::testing::{create_commit, run_git, setup_repo};
use std::collections::HashMap;

#[test]
fn test_integration_move_and_execute() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // Create feat1
    create_commit(&repo, "f1.txt", "f1", "Add f1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // Create feat2 from master (sibling of feat1)
    run_git(path_str, &["checkout", &master]);
    create_commit(&repo, "f2.txt", "f2", "Add f2");
    repo.branch(
        "feat2",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    // Move feat2 to be child of feat1
    intents.insert(
        "feat2".to_string(),
        BranchIntent {
            parent: Some(Some("feat1".to_string())),
            ..Default::default()
        },
    );

    execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // Verify feat1 is now ancestor of feat2
    let feat1_oid = run_git(path_str, &["rev-parse", "feat1"]);
    let merge_base = run_git(path_str, &["merge-base", "feat2", "feat1"]);
    assert_eq!(merge_base, feat1_oid);
}

#[test]
fn test_integration_complex_move() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();
    let initial_oid = run_git(path_str, &["rev-parse", "HEAD"]);

    // 1. Setup feat1 -> feat2
    run_git(path_str, &["checkout", &initial_oid]);
    create_commit(&repo, "f1.txt", "f1", "Add f1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    run_git(path_str, &["checkout", "feat1"]);
    create_commit(&repo, "f2.txt", "f2", "Add f2");
    repo.branch(
        "feat2",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 2. Setup target-base
    run_git(path_str, &["checkout", &initial_oid]);
    create_commit(&repo, "t.txt", "t", "Add target");
    repo.branch(
        "target-base",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 3. Move feat1 (and thus feat2) to target-base
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    apply_move(&mut intents, "feat1", Some("target-base".to_string())).unwrap();

    execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 4. Verify feat1 is on target-base
    let target_oid = run_git(path_str, &["rev-parse", "target-base"]);
    assert_eq!(
        run_git(path_str, &["merge-base", "feat1", "target-base"]),
        target_oid
    );

    // 5. Verify feat2 is on feat1
    let feat1_oid = run_git(path_str, &["rev-parse", "feat1"]);
    assert_eq!(
        run_git(path_str, &["merge-base", "feat2", "feat1"]),
        feat1_oid
    );
}

#[test]
fn test_integration_reparent_across_subtrees() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();
    let initial_oid = run_git(path_str, &["rev-parse", "HEAD"]);

    // 1. feat1 -> feat2
    run_git(path_str, &["checkout", &initial_oid]);
    create_commit(&repo, "f1.txt", "f1", "Add f1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    run_git(path_str, &["checkout", "feat1"]);
    create_commit(&repo, "f2.txt", "f2", "Add f2");
    repo.branch(
        "feat2",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 2. feat3 (sibling of feat1)
    run_git(path_str, &["checkout", &initial_oid]);
    create_commit(&repo, "f3.txt", "f3", "Add f3");
    repo.branch(
        "feat3",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 3. Move feat2 to be child of feat3
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    apply_move(&mut intents, "feat2", Some("feat3".to_string())).unwrap();

    execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 4. Verify feat2 is now child of feat3
    let feat3_oid = run_git(path_str, &["rev-parse", "feat3"]);
    assert_eq!(
        run_git(path_str, &["merge-base", "feat2", "feat3"]),
        feat3_oid
    );

    // 5. Verify feat1 remains as it was
    let master_oid = run_git(path_str, &["rev-parse", &master]);
    assert_eq!(
        run_git(path_str, &["merge-base", "feat1", &master]),
        master_oid
    );
}
