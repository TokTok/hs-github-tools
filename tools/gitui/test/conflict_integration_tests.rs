use gitui::engine::RealGit;
use gitui::testing::{create_commit, run_git, setup_repo};

#[test]
fn test_integration_conflict_detection() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 0. Base commit with a.txt
    let base_oid = create_commit(&repo, "a.txt", "base content", "Add a.txt to master");

    // 1. Create feat1 from base
    run_git(path_str, &["checkout", &base_oid.to_string()]);
    create_commit(&repo, "a.txt", "content v1", "Modify a.txt in feat1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 2. Create feat2 from base (CONFLICT with feat1)
    run_git(path_str, &["checkout", &base_oid.to_string()]);
    create_commit(&repo, "a.txt", "content v2", "Modify a.txt in feat2");
    repo.branch(
        "feat2",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 3. Create feat3 from base (NO CONFLICT with feat1)
    run_git(path_str, &["checkout", &base_oid.to_string()]);
    create_commit(&repo, "b.txt", "other content", "Add b.txt in feat3");
    repo.branch(
        "feat3",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    let git = RealGit::new(path_str).unwrap();

    // feat1 vs feat2 (Conflict)
    assert!(
        gitui::engine::Git::check_conflict(&git, "feat1", "feat2", None).unwrap(),
        "feat1 and feat2 should conflict"
    );

    // feat1 vs feat3 (No conflict)
    assert!(
        !gitui::engine::Git::check_conflict(&git, "feat1", "feat3", None).unwrap(),
        "feat1 and feat3 should NOT conflict"
    );
}

#[test]
fn test_integration_remote_conflict_detection() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Base commit with a.txt
    let base_oid = create_commit(&repo, "a.txt", "base content", "Add a.txt to master");

    // 2. Create local feat1 from base
    run_git(path_str, &["checkout", &base_oid.to_string()]);
    create_commit(&repo, "a.txt", "content v1", "Modify a.txt in feat1");
    repo.branch(
        "feat1",
        &repo.head().unwrap().peel_to_commit().unwrap(),
        false,
    )
    .unwrap();

    // 3. Create remote branch 'origin/feat2' from base (CONFLICT with feat1)
    run_git(path_str, &["checkout", &base_oid.to_string()]);
    create_commit(&repo, "a.txt", "content v2", "Modify a.txt in feat2");

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
    run_git(path_str, &["push", "origin", "HEAD:refs/heads/feat2"]);
    run_git(path_str, &["fetch", "origin"]);

    let git = RealGit::new(path_str).unwrap();

    // 4. Verify conflict detection
    assert!(
        gitui::engine::Git::check_conflict(&git, "feat1", "origin/feat2", None).unwrap(),
        "feat1 and origin/feat2 should conflict"
    );
}

#[test]
fn test_integration_predict_conflict_hunks() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Create a base file with multiple lines
    let base_content = "line 1\nline 2\nline 3\nline 4\n";
    let base_oid = create_commit(&repo, "file.txt", base_content, "Base commit");

    // 2. Branch A: change line 1
    run_git(path_str, &["checkout", &base_oid.to_string()]);
    create_commit(
        &repo,
        "file.txt",
        "CHANGE A\nline 2\nline 3\nline 4\n",
        "Branch A",
    );
    let oid_a = repo.head().unwrap().target().unwrap();

    // 3. Branch B: change line 4 (Clean merge with A)
    run_git(path_str, &["checkout", &base_oid.to_string()]);
    create_commit(
        &repo,
        "file.txt",
        "line 1\nline 2\nline 3\nCHANGE B\n",
        "Branch B",
    );
    let oid_b = repo.head().unwrap().target().unwrap();

    // 4. Branch C: change line 1 (Conflict with A)
    run_git(path_str, &["checkout", &base_oid.to_string()]);
    create_commit(
        &repo,
        "file.txt",
        "CONFLICT C\nline 2\nline 3\nline 4\n",
        "Branch C",
    );
    let oid_c = repo.head().unwrap().target().unwrap();

    let git = RealGit::new(path_str).unwrap();

    // A vs B: Clean
    assert!(
        !gitui::engine::Git::check_conflict_between(&git, oid_a, oid_b, None).unwrap(),
        "A and B should be a clean merge (different hunks)"
    );

    // A vs C: Conflict
    assert!(
        gitui::engine::Git::check_conflict_between(&git, oid_a, oid_c, None).unwrap(),
        "A and C should conflict (same hunk)"
    );
}

#[test]
fn test_rebase_conflict_prediction_needs_base() {
    let (dir, repo, _master) = setup_repo();
    let path_str = dir.path().to_str().unwrap();

    // 1. Base
    let base_oid = create_commit(&repo, "f.txt", "line 1\n", "Base");

    // 2. Branch 'upgrade': modifies line 1
    run_git(path_str, &["checkout", &base_oid.to_string()]);
    let c1_oid = create_commit(&repo, "f.txt", "line 1 updated\n", "C1");
    repo.branch("upgrade", &repo.find_commit(c1_oid).unwrap(), false)
        .unwrap();

    // 3. Branch 'testing':
    // It was based on the same tree as C1, but a different commit (C1 prime)
    run_git(path_str, &["checkout", &base_oid.to_string()]);
    let c1_prime_oid = create_commit(&repo, "f.txt", "line 1 updated\n", "C1 prime");

    // Move HEAD to c1_prime_oid to create C2 on top of it
    run_git(path_str, &["checkout", &c1_prime_oid.to_string()]);
    let c2_oid = create_commit(&repo, "f.txt", "line 1 updated\nline 2\n", "C2");
    repo.branch("testing", &repo.find_commit(c2_oid).unwrap(), false)
        .unwrap();

    let git = RealGit::new(path_str).unwrap();

    // Currently, check_conflict_between finds 'Base' as common ancestor.
    // merge(upgrade, testing, base=Base) will conflict because both modified line 1.
    // However, if we rebase 'testing' onto 'upgrade', we only care about the change
    // introduced in 'testing' (adding 'line 2') relative to its parent 'c1_prime'.

    // This is what happens now: it auto-detects 'Base' and finds a conflict.
    assert!(
        gitui::engine::Git::check_conflict_between(&git, c2_oid, c1_oid, None).unwrap(),
        "Should conflict with auto-detected base (Base)"
    );

    // If we specify c1_prime_oid as base, it should be clean.
    assert!(
        !gitui::engine::Git::check_conflict_between(&git, c2_oid, c1_oid, Some(c1_prime_oid))
            .unwrap(),
        "Should NOT conflict if we specify the correct base (c1_prime_oid)"
    );
}
