use git2::Signature;
use gitui::engine::transaction::Transaction;
use gitui::testing::setup_repo;
use std::collections::HashSet;
use std::fs::File;
use std::io::Write;

#[test]
fn test_transaction_projection() {
    let (tmp_dir, repo, _branch) = setup_repo();
    let file_path = tmp_dir.path().join("test.txt");

    // 1. Initial commit (parent)
    let mut initial_content = String::new();
    for i in 1..=20 {
        initial_content.push_str(&format!("line{}\n", i));
    }
    File::create(&file_path)
        .unwrap()
        .write_all(initial_content.as_bytes())
        .unwrap();
    let mut index = repo.index().unwrap();
    index.add_path(std::path::Path::new("test.txt")).unwrap();
    let oid = index.write_tree().unwrap();
    let signature = Signature::now("Test User", "test@example.com").unwrap();
    let parent_tree = repo.find_tree(oid).unwrap();
    let parent_commit = repo.head().unwrap().peel_to_commit().unwrap();
    repo.commit(
        Some("HEAD"),
        &signature,
        &signature,
        "Initial commit",
        &parent_tree,
        &[&parent_commit],
    )
    .unwrap();
    repo.branch(
        "master",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        true,
    )
    .unwrap();

    // 2. Feature commit (1 commit ahead)
    let mut feat_content = String::new();
    for i in 1..=20 {
        if i == 5 {
            feat_content.push_str("line5 changed\n");
        } else if i == 15 {
            feat_content.push_str("line15 changed\n");
        } else {
            feat_content.push_str(&format!("line{}\n", i));
        }
    }
    File::create(&file_path)
        .unwrap()
        .write_all(feat_content.as_bytes())
        .unwrap();
    index.add_path(std::path::Path::new("test.txt")).unwrap();
    let oid = index.write_tree().unwrap();
    let feat_tree = repo.find_tree(oid).unwrap();
    let parent_commit = repo.head().unwrap().peel_to_commit().unwrap();
    repo.commit(
        Some("HEAD"),
        &signature,
        &signature,
        "Feat commit",
        &feat_tree,
        &[&parent_commit],
    )
    .unwrap();
    repo.branch(
        "feat",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        true,
    )
    .unwrap();

    // 3. Setup transaction to split "feat"
    let mut tx = Transaction::new("feat".to_string(), "master".to_string());

    // Get diff
    let diff = repo
        .diff_tree_to_tree(Some(&parent_tree), Some(&feat_tree), None)
        .unwrap();

    // Select only first hunk (line 5 change)
    let mut selected = HashSet::new();
    selected.insert(("test.txt".to_string(), 0));
    tx.selected_hunks = selected;

    tx.calculate_projected_oids(&repo, &diff).unwrap();

    assert!(tx.projected_first_tree_oid.is_some());
    assert!(tx.projected_remainder_tree_oid.is_some());

    let first_tree = repo
        .find_tree(tx.projected_first_tree_oid.unwrap())
        .unwrap();
    let obj = first_tree
        .get_path(std::path::Path::new("test.txt"))
        .unwrap()
        .to_object(&repo)
        .unwrap();
    let blob = obj.as_blob().unwrap();
    let content = std::str::from_utf8(blob.content()).unwrap();
    println!("Projected content:\n{}", content);

    // Should have line 5 changed, but line 15 original
    assert!(content.contains("line5 changed"));
    assert!(content.contains("line15\n"));
    assert!(!content.contains("line15 changed"));

    // Remainder tree should be the tip tree
    assert_eq!(tx.projected_remainder_tree_oid.unwrap(), feat_tree.id());
}
