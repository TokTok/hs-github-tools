use gitui::testing::{create_commit, run_git, setup_repo};
use gitui::{print_converge_plan_to, print_submit_plan_to, print_tree_to};
use insta::assert_snapshot;
use std::path::PathBuf;

fn strip_ansi(s: &str) -> String {
    let re = regex::Regex::new(r"\x1b\[[0-9;]*[a-zA-Z]").unwrap();
    re.replace_all(s, "").to_string()
}

pub fn configure_insta() -> insta::Settings {
    let mut settings = insta::Settings::clone_current();
    let path = if let Ok(workspace_dir) = std::env::var("BUILD_WORKSPACE_DIRECTORY") {
        let mut p = PathBuf::from(workspace_dir);
        p.push("hs-github-tools");
        p.push("tools");
        p.push("gitui");
        p.push("test");
        p.push("snapshots");
        p
    } else {
        let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
        let mut p = std::env::current_dir().unwrap();
        p.push(manifest_dir);
        p.push("test");
        p.push("snapshots");
        p
    };
    settings.set_snapshot_path(path);
    settings
}

#[test]
fn test_cli_print_tree_snapshot() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // Setup a small tree
    // master
    //   feat1
    //     feat1-child
    //   feat2
    create_commit(&repo, "f1.txt", "1", "feat1 commit");
    run_git(path_str, &["branch", "feat1"]);

    run_git(path_str, &["checkout", "feat1"]);
    create_commit(&repo, "f1c.txt", "1c", "feat1-child commit");
    run_git(path_str, &["branch", "feat1-child"]);

    run_git(path_str, &["checkout", &master]);
    create_commit(&repo, "f2.txt", "2", "feat2 commit");
    run_git(path_str, &["branch", "feat2"]);

    // Currently on master
    let mut buffer = Vec::new();
    print_tree_to(path_str, false, &mut buffer).unwrap();

    let output = String::from_utf8(buffer).unwrap();
    let clean_output = strip_ansi(&output);

    let settings = configure_insta();
    settings.bind(|| {
        assert_snapshot!(clean_output);
    });
}

#[test]
fn test_cli_print_diverged_tree_snapshot() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // Detach HEAD so main doesn't move
    repo.set_head_detached(repo.head().unwrap().target().unwrap())
        .unwrap();

    // 1. feat1 at C1
    let oid1 = create_commit(&repo, "f1.txt", "1", "feat1");
    repo.branch("feat1", &repo.find_commit(oid1).unwrap(), false)
        .unwrap();

    // 2. feat2 at C2 (parent C1)
    repo.set_head_detached(oid1).unwrap();
    let oid2 = create_commit(&repo, "f2.txt", "2", "feat2");
    repo.branch("feat2", &repo.find_commit(oid2).unwrap(), false)
        .unwrap();

    // 3. Amend feat1 (C1 -> C1')
    run_git(path_str, &["checkout", "feat1"]);
    std::fs::write(dir.path().join("f1.txt"), "1 amended").unwrap();
    run_git(path_str, &["add", "f1.txt"]);
    run_git(path_str, &["commit", "--amend", "-m", "feat1", "-a"]);

    run_git(path_str, &["checkout", &master]);

    let mut buffer = Vec::new();
    print_tree_to(path_str, false, &mut buffer).unwrap();

    let output = String::from_utf8(buffer).unwrap();
    let clean_output = strip_ansi(&output);

    let settings = configure_insta();
    settings.bind(|| {
        assert_snapshot!(clean_output);
    });
}

#[test]
fn test_cli_print_submit_plan_snapshot() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Setup a "remote"
    let remote_dir = tempfile::tempdir().unwrap();
    run_git(remote_dir.path().to_str().unwrap(), &["init", "--bare"]);
    run_git(
        path_str,
        &[
            "remote",
            "add",
            "origin",
            remote_dir.path().to_str().unwrap(),
        ],
    );
    run_git(
        path_str,
        &[
            "remote",
            "add",
            "upstream",
            remote_dir.path().to_str().unwrap(),
        ],
    );

    // 2. Setup master tracking origin/master
    run_git(
        path_str,
        &["push", "origin", &format!("{}:{}", master, master)],
    );
    run_git(
        path_str,
        &[
            "branch",
            "--set-upstream-to",
            &format!("origin/{}", master),
            &master,
        ],
    );

    // 3. Setup feat branch ready to submit
    run_git(path_str, &["checkout", "-b", "feat"]);
    create_commit(&repo, "f.txt", "f", "Feat commit");
    run_git(path_str, &["push", "origin", "feat"]);
    run_git(
        path_str,
        &["branch", "--set-upstream-to", "origin/feat", "feat"],
    );
    run_git(path_str, &["fetch", "origin"]);

    let mut buffer = Vec::new();
    print_submit_plan_to(path_str, "feat", &mut buffer).unwrap();

    let output = String::from_utf8(buffer).unwrap();
    let clean_output = strip_ansi(&output);

    let settings = configure_insta();
    settings.bind(|| {
        assert_snapshot!(clean_output);
    });
}

#[test]
fn test_cli_print_converge_plan_snapshot() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // Detach HEAD so main doesn't move
    repo.set_head_detached(repo.head().unwrap().target().unwrap())
        .unwrap();

    // 1. Setup feat1
    let oid1 = create_commit(&repo, "f1.txt", "1", "feat1");
    repo.branch("feat1", &repo.find_commit(oid1).unwrap(), false)
        .unwrap();

    // 2. Setup feat2 on top of feat1
    repo.set_head_detached(oid1).unwrap();
    let oid2 = create_commit(&repo, "f2.txt", "2", "feat2");
    repo.branch("feat2", &repo.find_commit(oid2).unwrap(), false)
        .unwrap();

    // 3. Amend feat1
    run_git(path_str, &["checkout", "feat1"]);
    std::fs::write(dir.path().join("f1.txt"), "1 updated").unwrap();
    run_git(path_str, &["add", "f1.txt"]);
    run_git(path_str, &["commit", "--amend", "-m", "feat1", "-a"]);

    let mut buffer = Vec::new();
    print_converge_plan_to(path_str, "feat2", &mut buffer).unwrap();

    let output = String::from_utf8(buffer).unwrap();
    let clean_output = strip_ansi(&output);

    let settings = configure_insta();
    settings.bind(|| {
        assert_snapshot!(clean_output);
    });
}

#[test]
fn test_cli_cascading_divergence() {
    let (dir, repo, master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    repo.set_head_detached(repo.head().unwrap().target().unwrap())
        .unwrap();

    // 1. master -> A -> B -> C
    let oid_a = create_commit(&repo, "a.txt", "a", "branch-a");
    repo.branch("branch-a", &repo.find_commit(oid_a).unwrap(), false)
        .unwrap();

    repo.set_head_detached(oid_a).unwrap();
    let oid_b = create_commit(&repo, "b.txt", "b", "branch-b");
    repo.branch("branch-b", &repo.find_commit(oid_b).unwrap(), false)
        .unwrap();

    repo.set_head_detached(oid_b).unwrap();
    let oid_c = create_commit(&repo, "c.txt", "c", "branch-c");
    repo.branch("branch-c", &repo.find_commit(oid_c).unwrap(), false)
        .unwrap();

    // 2. Amend A
    run_git(path_str, &["checkout", "branch-a"]);
    std::fs::write(dir.path().join("a.txt"), "amended").unwrap();
    run_git(path_str, &["add", "a.txt"]);
    run_git(path_str, &["commit", "--amend", "-m", "branch-a", "-a"]);

    run_git(path_str, &["checkout", &master]);

    let mut buffer = Vec::new();
    print_tree_to(path_str, false, &mut buffer).unwrap();

    let output = String::from_utf8(buffer).unwrap();
    let clean_output = strip_ansi(&output);

    // branch-b should be [DIVERGED] because its parent A has moved
    assert!(clean_output.contains("branch-b [DIVERGED]"));
    // branch-c should NOT be [DIVERGED] because its parent B hasn't moved relative to it
    assert!(!clean_output.contains("branch-c [DIVERGED]"));

    let settings = configure_insta();
    settings.bind(|| {
        assert_snapshot!(clean_output);
    });
}
