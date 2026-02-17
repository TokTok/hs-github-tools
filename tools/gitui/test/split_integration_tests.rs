use git2::{BranchType, Signature, Time};
use gitui::engine::{Git, RealGit};
use gitui::testing::setup_repo;
use std::collections::HashSet;

#[test]
fn test_integration_split_branch_on_disk() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create a 1-commit branch that modifies two files
    // Initial files on master
    let path_a = dir.path().join("a.txt");
    std::fs::write(&path_a, "original a\n").unwrap();
    let path_b = dir.path().join("b.txt");
    std::fs::write(&path_b, "original b\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("a.txt")).unwrap();
        index.add_path(std::path::Path::new("b.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Base commit", &tree, &[&parent])
            .unwrap();
    }

    // Now create the "feat" branch with changes to both
    std::fs::write(&path_a, "modified a\n").unwrap();
    std::fs::write(&path_b, "modified b\n").unwrap();

    let feat_oid = {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("a.txt")).unwrap();
        index.add_path(std::path::Path::new("b.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686001, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Giant commit", &tree, &[&parent])
            .unwrap()
    };

    // feat now points to Giant commit, master also points to Giant commit because we committed to HEAD.
    // Move master back to Base commit.
    let giant_commit = repo.find_commit(feat_oid).unwrap();
    let base_commit = giant_commit.parents().next().unwrap();

    repo.set_head_detached(feat_oid).unwrap();
    repo.branch("feat", &giant_commit, true).unwrap();

    // Reset master to base_commit
    repo.branch(&master, &base_commit, true).unwrap();

    let git = RealGit::new(path_str).unwrap();

    // 2. Split "feat" by selecting only a.txt change
    let mut selected = HashSet::new();
    selected.insert(("a.txt".to_string(), 0));

    let split_data = gitui::engine::SplitData {
        parts: vec![gitui::engine::SplitPartData {
            name: "feat-part1".to_string(),
            commit_message: "Split from feat: first part".to_string(),
            selected_hunks: selected,
        }],
    };

    git.split_branch("feat", &master, &split_data).unwrap();

    // 3. Verify results
    // We expect: master -> feat-part1 -> feat

    let feat_branch = repo.find_branch("feat", BranchType::Local).unwrap();
    let feat_part1_branch = repo.find_branch("feat-part1", BranchType::Local).unwrap();

    let feat_commit = feat_branch.get().peel_to_commit().unwrap();
    let feat_part1_commit = feat_part1_branch.get().peel_to_commit().unwrap();
    let master_commit = repo
        .find_branch(&master, BranchType::Local)
        .unwrap()
        .get()
        .peel_to_commit()
        .unwrap();

    // Parentage
    assert_eq!(
        feat_commit.parents().next().unwrap().id(),
        feat_part1_commit.id()
    );
    assert_eq!(
        feat_part1_commit.parents().next().unwrap().id(),
        master_commit.id()
    );

    // Content of feat-part1 (should have modified a but original b)
    let tree1 = feat_part1_commit.tree().unwrap();
    let obj_a1 = tree1
        .get_path(std::path::Path::new("a.txt"))
        .unwrap()
        .to_object(&repo)
        .unwrap();
    let blob_a1 = obj_a1.as_blob().unwrap();
    let obj_b1 = tree1
        .get_path(std::path::Path::new("b.txt"))
        .unwrap()
        .to_object(&repo)
        .unwrap();
    let blob_b1 = obj_b1.as_blob().unwrap();
    assert_eq!(
        std::str::from_utf8(blob_a1.content()).unwrap(),
        "modified a\n"
    );
    assert_eq!(
        std::str::from_utf8(blob_b1.content()).unwrap(),
        "original b\n"
    );

    // Content of feat (should have both modified)
    let tree2 = feat_commit.tree().unwrap();
    let obj_a2 = tree2
        .get_path(std::path::Path::new("a.txt"))
        .unwrap()
        .to_object(&repo)
        .unwrap();
    let blob_a2 = obj_a2.as_blob().unwrap();
    let obj_b2 = tree2
        .get_path(std::path::Path::new("b.txt"))
        .unwrap()
        .to_object(&repo)
        .unwrap();
    let blob_b2 = obj_b2.as_blob().unwrap();
    assert_eq!(
        std::str::from_utf8(blob_a2.content()).unwrap(),
        "modified a\n"
    );
    assert_eq!(
        std::str::from_utf8(blob_b2.content()).unwrap(),
        "modified b\n"
    );
}

#[test]
fn test_integration_split_branch_3way() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create a commit that modifies three things
    let path_a = dir.path().join("a.txt");
    let path_b = dir.path().join("b.txt");
    let path_c = dir.path().join("c.txt");
    std::fs::write(&path_a, "a\n").unwrap();
    std::fs::write(&path_b, "b\n").unwrap();
    std::fs::write(&path_c, "c\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("a.txt")).unwrap();
        index.add_path(std::path::Path::new("b.txt")).unwrap();
        index.add_path(std::path::Path::new("c.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Base", &tree, &[&parent])
            .unwrap();
    }

    std::fs::write(&path_a, "A\n").unwrap();
    std::fs::write(&path_b, "B\n").unwrap();
    std::fs::write(&path_c, "C\n").unwrap();

    let feat_oid = {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("a.txt")).unwrap();
        index.add_path(std::path::Path::new("b.txt")).unwrap();
        index.add_path(std::path::Path::new("c.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686001, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Big", &tree, &[&parent])
            .unwrap()
    };

    let giant_commit = repo.find_commit(feat_oid).unwrap();
    let base_commit = giant_commit.parents().next().unwrap();
    repo.set_head_detached(feat_oid).unwrap();
    repo.branch("feat", &giant_commit, true).unwrap();
    repo.branch(&master, &base_commit, true).unwrap();

    let git = RealGit::new(path_str).unwrap();

    // 2. Split into 3 parts
    let mut sel1 = HashSet::new();
    sel1.insert(("a.txt".to_string(), 0));
    let mut sel2 = HashSet::new();
    sel2.insert(("b.txt".to_string(), 0));

    let split_data = gitui::engine::SplitData {
        parts: vec![
            gitui::engine::SplitPartData {
                name: "part1".to_string(),
                commit_message: "Msg1".to_string(),
                selected_hunks: sel1,
            },
            gitui::engine::SplitPartData {
                name: "part2".to_string(),
                commit_message: "Msg2".to_string(),
                selected_hunks: sel2,
            },
        ],
    };

    git.split_branch("feat", &master, &split_data).unwrap();

    // 3. Verify: master -> part1 -> part2 -> feat
    let feat_branch = repo.find_branch("feat", BranchType::Local).unwrap();
    let p2_branch = repo.find_branch("part2", BranchType::Local).unwrap();
    let p1_branch = repo.find_branch("part1", BranchType::Local).unwrap();

    let feat_c = feat_branch.get().peel_to_commit().unwrap();
    let p2_c = p2_branch.get().peel_to_commit().unwrap();
    let p1_c = p1_branch.get().peel_to_commit().unwrap();

    assert_eq!(feat_c.parents().next().unwrap().id(), p2_c.id());
    assert_eq!(p2_c.parents().next().unwrap().id(), p1_c.id());
    assert_eq!(p1_c.parents().next().unwrap().id(), base_commit.id());

    // Check content of part1 (A, b, c)
    let t1 = p1_c.tree().unwrap();
    assert_eq!(
        std::str::from_utf8(
            t1.get_path(std::path::Path::new("a.txt"))
                .unwrap()
                .to_object(&repo)
                .unwrap()
                .as_blob()
                .unwrap()
                .content()
        )
        .unwrap(),
        "A\n"
    );
    assert_eq!(
        std::str::from_utf8(
            t1.get_path(std::path::Path::new("b.txt"))
                .unwrap()
                .to_object(&repo)
                .unwrap()
                .as_blob()
                .unwrap()
                .content()
        )
        .unwrap(),
        "b\n"
    );

    // Check content of part2 (A, B, c)
    let t2 = p2_c.tree().unwrap();
    assert_eq!(
        std::str::from_utf8(
            t2.get_path(std::path::Path::new("a.txt"))
                .unwrap()
                .to_object(&repo)
                .unwrap()
                .as_blob()
                .unwrap()
                .content()
        )
        .unwrap(),
        "A\n"
    );
    assert_eq!(
        std::str::from_utf8(
            t2.get_path(std::path::Path::new("b.txt"))
                .unwrap()
                .to_object(&repo)
                .unwrap()
                .as_blob()
                .unwrap()
                .content()
        )
        .unwrap(),
        "B\n"
    );
    assert_eq!(
        std::str::from_utf8(
            t2.get_path(std::path::Path::new("c.txt"))
                .unwrap()
                .to_object(&repo)
                .unwrap()
                .as_blob()
                .unwrap()
                .content()
        )
        .unwrap(),
        "c\n"
    );

    // Check content of feat (A, B, C)
    let tf = feat_c.tree().unwrap();
    assert_eq!(
        std::str::from_utf8(
            tf.get_path(std::path::Path::new("c.txt"))
                .unwrap()
                .to_object(&repo)
                .unwrap()
                .as_blob()
                .unwrap()
                .content()
        )
        .unwrap(),
        "C\n"
    );
}

#[test]
fn test_integration_split_current_branch() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // Setup master -> feat (Giant commit)
    let path_a = dir.path().join("a.txt");
    std::fs::write(&path_a, "original a\n").unwrap();
    let path_b = dir.path().join("b.txt");
    std::fs::write(&path_b, "original b\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("a.txt")).unwrap();
        index.add_path(std::path::Path::new("b.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Base commit", &tree, &[&parent])
            .unwrap();
    }

    std::fs::write(&path_a, "modified a\n").unwrap();
    std::fs::write(&path_b, "modified b\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("a.txt")).unwrap();
        index.add_path(std::path::Path::new("b.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686001, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(
            Some("refs/heads/feat"),
            &sig,
            &sig,
            "Giant commit",
            &tree,
            &[&parent],
        )
        .unwrap();
    }

    // Checkout 'feat' branch
    repo.set_head("refs/heads/feat").unwrap();
    repo.checkout_head(Some(git2::build::CheckoutBuilder::new().force()))
        .unwrap();

    let git = RealGit::new(path_str).unwrap();

    let mut selected = HashSet::new();
    selected.insert(("a.txt".to_string(), 0));

    let split_data = gitui::engine::SplitData {
        parts: vec![gitui::engine::SplitPartData {
            name: "feat-part1".to_string(),
            commit_message: "Split from feat: first part".to_string(),
            selected_hunks: selected,
        }],
    };

    // This used to fail with "cannot force update branch ... as it is the current HEAD"
    git.split_branch("feat", &master, &split_data)
        .expect("Should allow splitting the current branch");

    // Verify we are still on feat
    let head = repo.head().unwrap();
    assert_eq!(head.shorthand(), Some("feat"));
}

#[test]
fn test_integration_split_with_rename() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Initial file
    let path_old = dir.path().join("old.txt");
    std::fs::write(&path_old, "line 1\nline 2\nline 3\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("old.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Base", &tree, &[&parent])
            .unwrap();
    }

    // 2. Rename and modify
    // Note: Repository operations are used to ensure it's a "proper" rename in Git's eyes if possible,
    // though Git usually detects renames heuristically.
    std::fs::remove_file(&path_old).unwrap();
    let path_new = dir.path().join("new.txt");
    std::fs::write(&path_new, "line 1\nline 2 modified\nline 3\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index.remove_path(std::path::Path::new("old.txt")).unwrap();
        index.add_path(std::path::Path::new("new.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686001, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(
            Some("refs/heads/feat"),
            &sig,
            &sig,
            "Rename commit",
            &tree,
            &[&parent],
        )
        .unwrap();
    }

    let git = RealGit::new(path_str).unwrap();

    // 3. Attempt to split.
    // Since renames aren't explicitly handled as "Rename" operations in our Hunk-based UI,
    // they show up as a deletion hunk and an addition hunk (unless find_similar is used).
    // In our current implementation, we just see "new.txt" as an addition.
    let mut selected = HashSet::new();
    selected.insert(("new.txt".to_string(), 0));

    let split_data = gitui::engine::SplitData {
        parts: vec![gitui::engine::SplitPartData {
            name: "feat-part1".to_string(),
            commit_message: "Split rename".to_string(),
            selected_hunks: selected,
        }],
    };

    git.split_branch("feat", &master, &split_data).unwrap();

    // 4. Verify results
    let part1_branch = repo.find_branch("feat-part1", BranchType::Local).unwrap();
    let part1_tree = part1_branch.get().peel_to_tree().unwrap();

    // Check if new.txt exists
    assert!(part1_tree.get_path(std::path::Path::new("new.txt")).is_ok());

    // With rename detection, selecting the hunk in new.txt should also include the rename metadata,
    // meaning old.txt should be GONE in the new tree.
    assert!(
        part1_tree
            .get_path(std::path::Path::new("old.txt"))
            .is_err(),
        "old.txt should NOT be present because it was renamed to new.txt"
    );
}

#[test]
fn test_integration_split_pure_rename() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Initial file
    let path_old = dir.path().join("old.txt");
    std::fs::write(&path_old, "line 1\nline 2\nline 3\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("old.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Base", &tree, &[&parent])
            .unwrap();
    }

    // 2. Pure Rename (no modification)
    std::fs::remove_file(&path_old).unwrap();
    let path_new = dir.path().join("new.txt");
    std::fs::write(&path_new, "line 1\nline 2\nline 3\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index.remove_path(std::path::Path::new("old.txt")).unwrap();
        index.add_path(std::path::Path::new("new.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686001, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(
            Some("refs/heads/feat"),
            &sig,
            &sig,
            "Rename commit",
            &tree,
            &[&parent],
        )
        .unwrap();
    }

    let git = RealGit::new(path_str).unwrap();

    // 3. Attempt to split.
    // For a pure rename, there are NO hunks.
    // We expect that selecting hunk 0 (or the file itself) should work.
    let mut selected = HashSet::new();
    selected.insert(("new.txt".to_string(), 0));

    let split_data = gitui::engine::SplitData {
        parts: vec![gitui::engine::SplitPartData {
            name: "feat-part1".to_string(),
            commit_message: "Split pure rename".to_string(),
            selected_hunks: selected,
        }],
    };

    git.split_branch("feat", &master, &split_data).unwrap();

    // 4. Verify results
    let part1_branch = repo.find_branch("feat-part1", BranchType::Local).unwrap();
    let part1_tree = part1_branch.get().peel_to_tree().unwrap();

    assert!(part1_tree.get_path(std::path::Path::new("new.txt")).is_ok());
    assert!(
        part1_tree
            .get_path(std::path::Path::new("old.txt"))
            .is_err(),
        "old.txt should NOT be present in the split part"
    );
}

#[test]
fn test_integration_split_rename_with_edit() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Initial file
    let path_old = dir.path().join("file.txt");
    let mut initial_content = String::new();
    for i in 1..=30 {
        initial_content.push_str(&format!("line {}\n", i));
    }
    std::fs::write(&path_old, &initial_content).unwrap();

    {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("file.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Base", &tree, &[&parent])
            .unwrap();
    }

    // 2. Rename and Edit (two separate hunks)
    std::fs::remove_file(&path_old).unwrap();
    let path_new = dir.path().join("renamed.txt");

    let mut content = String::new();
    content.push_str("line 1 modified\n");
    for i in 2..=30 {
        content.push_str(&format!("line {}\n", i));
    }
    content.push_str("line 31 added\n");
    std::fs::write(&path_new, &content).unwrap();

    {
        let mut index = repo.index().unwrap();
        index.remove_path(std::path::Path::new("file.txt")).unwrap();
        index.add_path(std::path::Path::new("renamed.txt")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686001, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(
            Some("refs/heads/feat"),
            &sig,
            &sig,
            "Rename and edit",
            &tree,
            &[&parent],
        )
        .unwrap();
    }

    let git = RealGit::new(path_str).unwrap();

    // 3. Split: Pick ONLY the first hunk (line 1 modification)
    let mut selected = HashSet::new();
    selected.insert(("renamed.txt".to_string(), 0));

    let split_data = gitui::engine::SplitData {
        parts: vec![gitui::engine::SplitPartData {
            name: "feat-part1".to_string(),
            commit_message: "Split rename + first edit".to_string(),
            selected_hunks: selected,
        }],
    };

    git.split_branch("feat", &master, &split_data).unwrap();

    // 4. Verify results
    let part1_branch = repo.find_branch("feat-part1", BranchType::Local).unwrap();
    let part1_tree = part1_branch.get().peel_to_tree().unwrap();

    // Should be renamed to renamed.txt
    assert!(
        part1_tree
            .get_path(std::path::Path::new("renamed.txt"))
            .is_ok()
    );
    assert!(
        part1_tree
            .get_path(std::path::Path::new("file.txt"))
            .is_err()
    );

    let obj = part1_tree
        .get_path(std::path::Path::new("renamed.txt"))
        .unwrap()
        .to_object(&repo)
        .unwrap();
    let content = std::str::from_utf8(obj.as_blob().unwrap().content()).unwrap();

    assert!(content.contains("line 1 modified"));
    assert!(
        !content.contains("line 31 added"),
        "Should not have the second hunk yet"
    );

    // Remainder branch should have everything
    let feat_branch = repo.find_branch("feat", BranchType::Local).unwrap();
    let feat_tree = feat_branch.get().peel_to_tree().unwrap();
    let obj_final = feat_tree
        .get_path(std::path::Path::new("renamed.txt"))
        .unwrap()
        .to_object(&repo)
        .unwrap();
    let content_final = std::str::from_utf8(obj_final.as_blob().unwrap().content()).unwrap();
    assert!(content_final.contains("line 31 added"));
}

#[test]
fn test_integration_split_with_mode_change() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Initial file
    let path = dir.path().join("script.sh");
    std::fs::write(&path, "echo hello\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("script.sh")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Base", &tree, &[&parent])
            .unwrap();
    }

    // 2. Chmod +x and modify
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&path, perms).unwrap();
    }
    // If not unix, this might not work as expected but git2 might still track it if forced.

    std::fs::write(&path, "echo hello world\n").unwrap();

    {
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("script.sh")).unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686001, 0)).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(
            Some("refs/heads/feat"),
            &sig,
            &sig,
            "Chmod commit",
            &tree,
            &[&parent],
        )
        .unwrap();
    }

    let git = RealGit::new(path_str).unwrap();

    let mut selected = HashSet::new();
    selected.insert(("script.sh".to_string(), 0));

    let split_data = gitui::engine::SplitData {
        parts: vec![gitui::engine::SplitPartData {
            name: "feat-part1".to_string(),
            commit_message: "Split mode change".to_string(),
            selected_hunks: selected,
        }],
    };

    git.split_branch("feat", &master, &split_data).unwrap();

    // 3. Verify results
    let part1_branch = repo.find_branch("feat-part1", BranchType::Local).unwrap();
    let part1_tree = part1_branch.get().peel_to_tree().unwrap();
    let entry = part1_tree.get_name("script.sh").unwrap();

    #[cfg(unix)]
    assert_eq!(entry.filemode(), 0o100755, "File mode should be executable");
}
