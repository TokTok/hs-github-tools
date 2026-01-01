use git2::Signature;
use gitui::patch_utils::apply_selected_hunks_to_tree;
use gitui::testing::setup_repo;
use std::collections::HashSet;
use std::fs::File;
use std::io::Write;

#[test]
fn test_apply_selected_hunks() {
    let (tmp_dir, repo, _branch) = setup_repo();
    let file_path = tmp_dir.path().join("test.txt");

    // 1. Initial commit
    let mut content = String::new();
    for i in 1..=20 {
        content.push_str(&format!("line{}\n", i));
    }
    File::create(&file_path)
        .unwrap()
        .write_all(content.as_bytes())
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

    // 2. Modify in two places
    let mut new_content = String::new();
    for i in 1..=20 {
        if i == 5 {
            new_content.push_str("line5 changed\n");
        } else if i == 15 {
            new_content.push_str("line15 changed\n");
        } else {
            new_content.push_str(&format!("line{}\n", i));
        }
    }
    File::create(&file_path)
        .unwrap()
        .write_all(new_content.as_bytes())
        .unwrap();

    // 3. Get diff
    let diff = repo
        .diff_tree_to_workdir_with_index(Some(&parent_tree), None)
        .unwrap();

    // 4. Select ONLY the first hunk (line 5)
    let mut selected = HashSet::new();
    selected.insert(("test.txt".to_string(), 0));

    let patch_data = gitui::patch_utils::create_filtered_diff(&diff, &selected).unwrap();
    println!("Patch data:\n{}", std::str::from_utf8(&patch_data).unwrap());
    let new_tree_oid = apply_selected_hunks_to_tree(&repo, &parent_tree, &diff, &selected).unwrap();
    let new_tree = repo.find_tree(new_tree_oid).unwrap();

    // 5. Verify the new tree has line 5 changed but NOT line 15
    let obj = new_tree
        .get_path(std::path::Path::new("test.txt"))
        .unwrap()
        .to_object(&repo)
        .unwrap();
    let blob = obj.as_blob().unwrap();
    let content = std::str::from_utf8(blob.content()).unwrap();

    assert!(content.contains("line5 changed"));
    assert!(content.contains("line15\n"));
    assert!(!content.contains("line15 changed"));
}

// end of file
