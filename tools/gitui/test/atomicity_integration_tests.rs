use git2::{Signature, Time};
use gitui::engine::{Git, RealGit, SplitData, SplitPartData};
use gitui::testing::setup_repo;
use std::collections::HashSet;

#[test]
fn test_split_partial_failure_leaves_part_branches() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup a commit with multiple hunks
    let path_a = dir.path().join("a.txt");
    let mut initial_content = String::new();
    for i in 1..=50 {
        initial_content.push_str(&format!("line {}\n", i));
    }
    std::fs::write(&path_a, &initial_content).unwrap();
    {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("a.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Base", &tree, &[&parent])
            .unwrap();
    }

    let mut new_content = initial_content.clone();
    new_content.replace_range(0..7, "line 1 modified\n"); // Hunk 1
    new_content.push_str("line 51 added\n"); // Hunk 2
    std::fs::write(&path_a, &new_content).unwrap();
    let feat_oid = {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("a.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686001, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(
            Some("refs/heads/feat"),
            &sig,
            &sig,
            "Big",
            &tree,
            &[&parent],
        )
        .unwrap()
    };

    let git = RealGit::new(path_str).unwrap();

    // 2. Part 1: Valid. Part 2: INVALID branch name (empty or invalid chars)
    let mut sel1 = HashSet::new();
    sel1.insert(("a.txt".to_string(), 0)); // first hunk
    let mut sel2 = HashSet::new();
    sel2.insert(("a.txt".to_string(), 1)); // second hunk

    let split_data = SplitData {
        parts: vec![
            SplitPartData {
                name: "valid-part".to_string(),
                commit_message: "Msg1".to_string(),
                selected_hunks: sel1,
            },
            SplitPartData {
                name: "invalid branch name".to_string(), // Space is invalid in branch names
                commit_message: "Msg2".to_string(),
                selected_hunks: sel2,
            },
        ],
    };

    let res = git.split_branch("feat", &master, &split_data);

    // 3. Verify it failed
    if let Err(ref e) = res {
        println!("Split failed as expected: {}", e);
    }
    assert!(
        res.is_err(),
        "Split should have failed due to invalid branch name"
    );

    // 4. Verify that "valid-part" WAS created (partial success/no atomicity)
    assert!(
        repo.find_branch("valid-part", git2::BranchType::Local)
            .is_ok(),
        "valid-part branch should exist even though the whole operation failed"
    );

    // 5. Verify that "feat" still points to the original commit
    let feat_branch = repo.find_branch("feat", git2::BranchType::Local).unwrap();
    assert_eq!(
        feat_branch.get().target().unwrap(),
        feat_oid,
        "Original feat branch should NOT have moved if the operation failed midway"
    );
}
