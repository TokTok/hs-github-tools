use gitui::engine::{BranchIntent, Git, RealGit};
use gitui::execute_plan;
use gitui::testing::{create_commit, run_git, setup_repo};
use std::collections::HashMap;
use std::process::Command;

#[test]
fn test_integration_amend_branch_with_children() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup toxav-bench -> test-framework
    run_git(path_str, &["checkout", "-b", "toxav-bench"]);
    create_commit(&repo, "toxav.txt", "toxav content", "Add toxav-bench");

    run_git(path_str, &["checkout", "-b", "test-framework"]);
    create_commit(&repo, "test.txt", "test content", "Add test-framework");

    // 2. Go back to toxav-bench and add some staged changes
    run_git(path_str, &["checkout", "toxav-bench"]);
    let toxav_path = dir.path().join("toxav.txt");
    std::fs::write(&toxav_path, "toxav amended content").unwrap();
    run_git(path_str, &["add", "toxav.txt"]);

    // 3. Mark toxav-bench for amend
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    {
        let _toxav = branches
            .iter()
            .find(|b| b.name == "toxav-bench")
            .expect("toxav-bench not found");
        intents.insert(
            "toxav-bench".to_string(),
            BranchIntent {
                pending_amend: true,
                ..Default::default()
            },
        );
    }

    // 4. Execute plan
    execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 5. Verify toxav-bench is amended
    let content = std::fs::read_to_string(&toxav_path).unwrap();
    assert_eq!(content, "toxav amended content");

    // 6. Verify test-framework is rebased onto the new toxav-bench
    let toxav_oid = run_git(path_str, &["rev-parse", "toxav-bench"]);
    let merge_base = run_git(path_str, &["merge-base", "test-framework", "toxav-bench"]);
    assert_eq!(
        merge_base, toxav_oid,
        "test-framework should be rebased onto amended toxav-bench"
    );
}

#[test]
fn test_integration_amend_with_message() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create a feature branch with a commit
    create_commit(&repo, "feat.txt", "content", "Old message");
    repo.branch(
        "feat",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();
    Command::new("git")
        .arg("-C")
        .arg(path_str)
        .args(["checkout", "feat"])
        .status()
        .unwrap();

    // 2. Setup RealGit and load branches
    let git = RealGit::new(path_str).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();
    let mut intents = HashMap::new();

    // 3. Mark feat for amend with message
    intents.insert(
        "feat".to_string(),
        BranchIntent {
            pending_amend: true,
            pending_amend_message: Some("New message".to_string()),
            ..Default::default()
        },
    );

    // 4. Execute plan
    execute_plan(&git, &branches, &intents, &history, path_str).unwrap();

    // 5. Verify commit message changed
    let head = repo.head().unwrap();
    let commit = head.peel_to_commit().unwrap();
    assert_eq!(commit.message().unwrap(), "New message\n");

    // 6. Verify content is same (we didn't change content, just message)
    let path = dir.path().join("feat.txt");
    let content = std::fs::read_to_string(path).unwrap();
    assert_eq!(content, "content");
}
