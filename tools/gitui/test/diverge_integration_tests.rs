use gitui::testing::{create_commit, run_git, setup_repo};

#[test]
fn test_divergence_detection_and_convergence_plan() {
    let (tdir, repo, _main_branch) = setup_repo();
    let path_str = tdir.path().to_str().unwrap();

    // 1. Create Branch A with "Commit 1"
    run_git(path_str, &["checkout", "-b", "branch-a"]);
    create_commit(&repo, "file1.txt", "content1", "Commit 1");

    // 2. Create Branch B from A with "Commit 2"
    run_git(path_str, &["checkout", "-b", "branch-b"]);
    create_commit(&repo, "file2.txt", "content2", "Commit 2");

    // 3. Amend Branch A (change content, keep summary)
    run_git(path_str, &["checkout", "branch-a"]);
    let file1_path = tdir.path().join("file1.txt");
    std::fs::write(&file1_path, "content1 updated").unwrap();
    run_git(path_str, &["add", "file1.txt"]);
    run_git(path_str, &["commit", "--amend", "--no-edit"]);

    // 4. Verify Branch B is detected as diverged from A
    let mut buf = Vec::new();
    gitui::print_tree_to(tdir.path(), false, &mut buf).expect("Failed to print tree");
    let output_str = String::from_utf8(buf).expect("Output not UTF-8");

    println!("Tree Output:\n{}", output_str);
    assert!(output_str.contains("branch-b"));
    assert!(output_str.contains("[DIVERGED]"));

    // 5. Verify --converge B suggests the correct rebase command
    let mut buf = Vec::new();
    gitui::print_converge_plan_to(tdir.path(), "branch-b", &mut buf)
        .expect("Failed to print converge plan");
    let plan_str = String::from_utf8(buf).expect("Plan output not UTF-8");

    println!("Plan Output:\n{}", plan_str);
    assert!(plan_str.contains("Plan to converge branch-b:"));
    assert!(plan_str.contains("git rebase --onto branch-a"));
    assert!(plan_str.contains("branch-b"));

    // Verify that it uses the 3-argument rebase (with upstream OID)
    // The output should look like: git rebase --onto branch-a <oid> branch-b
    let parts: Vec<&str> = plan_str
        .lines()
        .find(|line| line.contains("git rebase --onto"))
        .unwrap()
        .split_whitespace()
        .collect();

    // git, rebase, --onto, branch-a, <oid>, branch-b [, [CONFLICT]/[CLEAN]]
    assert!(
        parts.len() == 6 || parts.len() == 7,
        "Rebase command should have 6 or 7 parts"
    );
    assert_eq!(parts[0], "git");
    assert_eq!(parts[1], "rebase");
    assert_eq!(parts[2], "--onto");
    assert_eq!(parts[3], "branch-a");
    assert_eq!(parts[5], "branch-b");
    // parts[4] is the OID
    assert!(parts[4].len() >= 7, "Fifth part should be a commit OID");
}

#[test]
fn test_divergence_with_remote_branch() {
    let (tdir, repo, _main_branch) = setup_repo();
    let path_str = tdir.path().to_str().unwrap();

    // 1. Create Branch A with "Commit 1"
    run_git(path_str, &["checkout", "-b", "branch-a"]);
    create_commit(&repo, "file1.txt", "content1", "Commit 1");

    // 2. Create a "remote" branch origin/branch-a at the same OID
    let oid = repo.head().unwrap().target().unwrap();
    run_git(
        path_str,
        &[
            "update-ref",
            "refs/remotes/origin/branch-a",
            &oid.to_string(),
        ],
    );

    // 3. Create Branch B from A with "Commit 2"
    run_git(path_str, &["checkout", "-b", "branch-b"]);
    create_commit(&repo, "file2.txt", "content2", "Commit 2");

    // 4. Amend Branch A (change content, keep summary)
    run_git(path_str, &["checkout", "branch-a"]);
    let file1_path = tdir.path().join("file1.txt");
    std::fs::write(&file1_path, "content1 updated").unwrap();
    run_git(path_str, &["add", "file1.txt"]);
    run_git(path_str, &["commit", "--amend", "--no-edit"]);

    // 5. Verify Branch B is detected as diverged from A
    let mut buf = Vec::new();
    gitui::print_tree_to(tdir.path(), true, &mut buf).expect("Failed to print tree");
    let output_str = String::from_utf8(buf).expect("Output not UTF-8");

    println!("Tree Output:\n{}", output_str);
    assert!(output_str.contains("branch-b"));

    // This is what fails for the user!
    assert!(
        output_str.contains("[DIVERGED]"),
        "Should show [DIVERGED] even if origin/branch-a exists at old OID"
    );

    // 6. Verify --converge B works
    let mut buf = Vec::new();
    gitui::print_converge_plan_to(tdir.path(), "branch-b", &mut buf)
        .expect("Failed to print converge plan");
    let plan_str = String::from_utf8(buf).expect("Plan output not UTF-8");
    assert!(plan_str.contains("Plan to converge branch-b:"));
}
