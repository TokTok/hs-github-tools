use gitui::engine::{BranchInfo, Git, RealGit};
use gitui::testing::{create_commit, run_git, setup_repo};

// Replicate the logic from runtime.rs in a testable way
fn check_post_execution_switch(
    git: &dyn Git,
    initial_branch: &str,
    branches_to_execute: &[BranchInfo],
    intents: &std::collections::HashMap<String, gitui::engine::BranchIntent>,
) -> String {
    let current = git.get_current_branch().unwrap();

    // Check if initial branch exists
    let exists = git
        .run_command(&[
            "show-ref".to_string(),
            "--verify".to_string(),
            "--quiet".to_string(),
            format!("refs/heads/{}", initial_branch),
        ])
        .unwrap_or(false);

    if exists {
        git.checkout(initial_branch).unwrap();
        return initial_branch.to_string();
    }

    // Fallback logic
    let mut children: Vec<String> = branches_to_execute
        .iter()
        .filter(|b| {
            let intent = intents.get(&b.name);
            let effective_parent = match intent.and_then(|i| i.parent.as_ref()) {
                Some(p) => p.clone(),
                None => b.original_parent.clone(),
            };
            effective_parent.as_ref() == Some(&initial_branch.to_string())
        })
        .map(|b| b.name.clone())
        .collect();
    children.sort();

    let branch_exists = |name: &str| -> bool {
        git.run_command(&[
            "show-ref".to_string(),
            "--verify".to_string(),
            "--quiet".to_string(),
            format!("refs/heads/{}", name),
        ])
        .unwrap_or(false)
    };

    let mut target = None;
    for child in children {
        if branch_exists(&child) {
            target = Some(child);
            break;
        }
    }

    if target.is_none() {
        if branch_exists("master") {
            target = Some("master".to_string());
        } else if branch_exists("main") {
            target = Some("main".to_string());
        }
    }

    if let Some(t) = target
        && t != current
    {
        git.checkout(&t).unwrap();
        return t;
    }
    current
}

#[test]
fn test_post_submit_branch_switching() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup tree: master -> feat1 -> feat2
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

    // Simulate State before execution
    let branches_to_execute = vec![
        BranchInfo {
            name: "feat1".to_string(),
            original_parent: Some(master.clone()),
            ..Default::default()
        },
        BranchInfo {
            name: "feat2".to_string(),
            original_parent: Some("feat1".to_string()),
            ..Default::default()
        },
    ];
    let intents = std::collections::HashMap::new();

    // Case 1: Initial branch 'feat1' exists (normal case)
    run_git(path_str, &["checkout", &master]); // Currently on master
    let result = check_post_execution_switch(&git, "feat1", &branches_to_execute, &intents);
    assert_eq!(result, "feat1", "Should restore existing initial branch");

    // Case 2: Initial branch 'feat1' is deleted (simulating submit), switch to child 'feat2'
    run_git(path_str, &["checkout", &master]);
    run_git(path_str, &["branch", "-D", "feat1"]);
    let result = check_post_execution_switch(&git, "feat1", &branches_to_execute, &intents);
    assert_eq!(
        result, "feat2",
        "Should switch to child branch feat2 when feat1 is deleted"
    );

    // Case 3: Both 'feat1' and 'feat2' deleted, fallback to master
    run_git(path_str, &["checkout", &master]);
    // feat1 already deleted
    run_git(path_str, &["branch", "-D", "feat2"]);
    let result = check_post_execution_switch(&git, "feat1", &branches_to_execute, &intents);
    assert_eq!(
        result, master,
        "Should fallback to master when children also deleted"
    );
}

// end of file
