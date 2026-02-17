use git2::Signature;
use gitui::diff_utils::{LineType, parse_diff};
use gitui::testing::setup_repo;
use std::fs::File;
use std::io::Write;

#[test]
fn test_parse_diff_single_file() {
    let (tmp_dir, repo, _branch) = setup_repo();
    let file_path = tmp_dir.path().join("test.txt");

    // 1. Initial commit
    {
        let mut file = File::create(&file_path).unwrap();
        file.write_all(b"line1\nline2\nline3\n").unwrap();
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("test.txt")).unwrap();
        let oid = index.write_tree().unwrap();
        let signature = Signature::now("Test User", "test@example.com").unwrap();
        let tree = repo.find_tree(oid).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(
            Some("HEAD"),
            &signature,
            &signature,
            "Initial commit",
            &tree,
            &[&parent],
        )
        .unwrap();
    }

    // 2. Modify file
    {
        let mut file = File::create(&file_path).unwrap();
        file.write_all(b"line1\nline2 changed\nline3\nline4\n")
            .unwrap();
    }

    // 3. Get diff
    let head = repo.head().unwrap().peel_to_tree().unwrap();
    let diff = repo
        .diff_tree_to_workdir_with_index(Some(&head), None)
        .unwrap();

    let file_diffs = parse_diff(&diff).unwrap();
    assert_eq!(file_diffs.len(), 1);
    let f = &file_diffs[0];
    assert_eq!(f.path, "test.txt");
    assert_eq!(f.hunks.len(), 1);
    let h = &f.hunks[0];

    // line1 (context)
    // -line2
    // +line2 changed
    // line3 (context)
    // +line4
    assert_eq!(h.lines.len(), 5);
    assert_eq!(h.lines[0].line_type, LineType::Context);
    assert_eq!(h.lines[1].line_type, LineType::Deletion);
    assert_eq!(h.lines[2].line_type, LineType::Addition);
    assert_eq!(h.lines[3].line_type, LineType::Context);
    assert_eq!(h.lines[4].line_type, LineType::Addition);
}

#[test]
fn test_parse_diff_multiple_hunks() {
    let (tmp_dir, repo, _branch) = setup_repo();
    let file_path = tmp_dir.path().join("test.txt");

    // 1. Initial commit with many lines
    {
        let mut file = File::create(&file_path).unwrap();
        let mut content = String::new();
        for i in 1..=100 {
            content.push_str(&format!("line{}\n", i));
        }
        file.write_all(content.as_bytes()).unwrap();
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new("test.txt")).unwrap();
        let oid = index.write_tree().unwrap();
        let signature = Signature::now("Test User", "test@example.com").unwrap();
        let tree = repo.find_tree(oid).unwrap();
        let parent = repo.head().unwrap().peel_to_commit().unwrap();
        repo.commit(
            Some("HEAD"),
            &signature,
            &signature,
            "Initial commit",
            &tree,
            &[&parent],
        )
        .unwrap();
    }

    // 2. Modify file in two distant places
    {
        let mut file = File::create(&file_path).unwrap();
        let mut content = String::new();
        for i in 1..=100 {
            if i == 5 {
                content.push_str("line5 changed\n");
            } else if i == 95 {
                content.push_str("line95 changed\n");
            } else {
                content.push_str(&format!("line{}\n", i));
            }
        }
        file.write_all(content.as_bytes()).unwrap();
    }

    // 3. Get diff
    let head = repo.head().unwrap().peel_to_tree().unwrap();
    let diff = repo
        .diff_tree_to_workdir_with_index(Some(&head), None)
        .unwrap();

    let file_diffs = parse_diff(&diff).unwrap();
    assert_eq!(file_diffs.len(), 1);
    let f = &file_diffs[0];
    assert_eq!(f.hunks.len(), 2);

    assert!(f.hunks[0].header.contains("@@ -2,7 +2,7 @@"));
    assert!(f.hunks[1].header.contains("@@ -92,7 +92,7 @@"));
}

// end of file
