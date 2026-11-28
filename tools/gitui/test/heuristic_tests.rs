use gitui::engine::{Git, RealGit};
use gitui::state::{AppState, Msg};
use gitui::testing::{create_commit, run_git, setup_repo};

#[test]
fn test_heuristic_parent_parsing() {
    let (dir, repo, main) = setup_repo();
    let path = dir.path();

    // Detach HEAD so 'main' doesn't move
    repo.set_head_detached(repo.head().unwrap().target().unwrap())
        .unwrap();

    // Create branches foo and bar first so they are in the summary map
    let oid_foo = create_commit(&repo, "foo.txt", "foo", "foo");
    repo.branch("foo", &repo.find_commit(oid_foo).unwrap(), false)
        .unwrap();
    let oid_bar = create_commit(&repo, "bar.txt", "bar", "bar");
    repo.branch("bar", &repo.find_commit(oid_bar).unwrap(), false)
        .unwrap();

    repo.set_head_detached(
        repo.find_branch(&main, git2::BranchType::Local)
            .unwrap()
            .get()
            .target()
            .unwrap(),
    )
    .unwrap();

    // 1. Create a branch whose history contains a commit with summary "foo"
    let _oid_pre_a = create_commit(&repo, "pre_a.txt", "pre_a", "foo");
    let oid_a = create_commit(&repo, "a.txt", "a", "Feature A");
    repo.branch("feat-a", &repo.find_commit(oid_a).unwrap(), false)
        .unwrap();

    // 2. Create another branch whose history contains a commit with summary "bar"
    // Reset to main's OID first
    let main_oid = repo
        .find_branch(&main, git2::BranchType::Local)
        .unwrap()
        .get()
        .target()
        .unwrap();
    repo.set_head_detached(main_oid).unwrap();
    let _oid_pre_b = create_commit(&repo, "pre_b.txt", "pre_b", "bar");
    let oid_b = create_commit(&repo, "b.txt", "b", "Feature B");
    repo.branch("feat-b", &repo.find_commit(oid_b).unwrap(), false)
        .unwrap();

    // Now amend foo and bar so that feat-a and feat-b are diverged
    run_git(path, &["checkout", "foo"]);
    create_commit(&repo, "foo2.txt", "foo2", "foo");
    run_git(path, &["checkout", "bar"]);
    create_commit(&repo, "bar2.txt", "bar2", "bar");

    let git = RealGit::new(path).unwrap();
    let (branches, _) = git.get_branches(None).unwrap();

    let a = branches
        .iter()
        .find(|b| b.name == "feat-a")
        .expect("feat-a missing");
    assert_eq!(a.heuristic_parent.as_deref(), Some("foo"));

    let b = branches
        .iter()
        .find(|b| b.name == "feat-b")
        .expect("feat-b missing");
    assert_eq!(b.heuristic_parent.as_deref(), Some("bar"));
}

#[test]
fn test_heuristic_sync_interaction() {
    let (dir, repo, main) = setup_repo();
    let path = dir.path();
    let main_oid = repo.head().unwrap().target().unwrap();

    // Detach HEAD so 'main' stays at initial commit
    repo.set_head_detached(main_oid).unwrap();

    // 1. Setup main -> feat1 -> feat2
    let oid1 = create_commit(&repo, "f1.txt", "1", "Add feat1");
    repo.branch("feat1", &repo.find_commit(oid1).unwrap(), false)
        .unwrap();

    let oid2 = create_commit(&repo, "f2.txt", "2", "Add feat2");
    repo.branch("feat2", &repo.find_commit(oid2).unwrap(), false)
        .unwrap();

    // 2. Amend feat1 (break the link)
    run_git(path, &["checkout", "-f", "feat1"]);
    std::fs::write(path.join("f1.txt"), "1 amended").unwrap();
    run_git(path, &["add", "f1.txt"]);
    run_git(path, &["commit", "--amend", "--no-edit"]);

    let git = RealGit::new(path).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        ..AppState::default()
    };
    state.refresh_tree(None);

    // The broken topology: feat2 is under main, as its parent OID (old feat1) is no branch head.
    let feat2_info = state
        .branches
        .iter()
        .find(|b| b.name == "feat2")
        .expect("feat2 missing");
    assert_eq!(
        state.get_effective_parent("feat2").as_ref().unwrap(),
        &main,
        "feat2 should have fallen back to main"
    );
    assert_eq!(feat2_info.heuristic_parent.as_deref(), Some("feat1"));

    // 3. Simulate pressing 'u' on feat2 to sync it back to feat1
    let feat2_pos = state
        .flattened_tree
        .iter()
        .position(|(n, _)| n == "feat2")
        .expect("feat2 missing from tree");
    state.list_state.select(Some(feat2_pos));

    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('u'),
        KeyModifiers::NONE,
    )));

    // 4. Verify feat2 is now parented to feat1
    assert_eq!(
        state.get_effective_parent("feat2").as_deref(),
        Some("feat1"),
        "feat2 should be synced back to feat1"
    );

    // Verify tree refresh
    state.refresh_tree(None);
    let feat2_flat_pos = state
        .flattened_tree
        .iter()
        .position(|(n, _)| n == "feat2")
        .unwrap();
    let feat1_flat_pos = state
        .flattened_tree
        .iter()
        .position(|(n, _)| n == "feat1")
        .unwrap();
    assert!(
        feat2_flat_pos > feat1_flat_pos,
        "feat2 should come after feat1"
    );
    assert_eq!(
        state.flattened_tree[feat2_flat_pos].1,
        state.flattened_tree[feat1_flat_pos].1 + 1
    );
}

#[test]
fn test_heuristic_sync_with_cycle_prevention() {
    let (dir, repo, main) = setup_repo();
    let path = dir.path();
    let main_oid = repo.head().unwrap().target().unwrap();

    repo.set_head_detached(main_oid).unwrap();

    // Create A -> B
    let oid_a = create_commit(&repo, "a.txt", "a", "A");
    repo.branch("A", &repo.find_commit(oid_a).unwrap(), false)
        .unwrap();

    let oid_b = create_commit(&repo, "b.txt", "b", "B");
    repo.branch("B", &repo.find_commit(oid_b).unwrap(), false)
        .unwrap();

    let git = RealGit::new(path).unwrap();

    // We use the amended message to get the heuristic parent B into A.
    run_git(path, &["checkout", "-f", "A"]);
    run_git(path, &["commit", "--amend", "-m", "B"]); // Set summary to "B" to match branch B

    // Reload branches
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        ..AppState::default()
    };
    state.refresh_tree(None);

    // For a cycle, B.parent must be A. B's parent OID (old A) is gone, so B walks
    // up to main: force it back to A.
    state.mutate_intent("B", |i| {
        i.parent = Some(Some("A".to_string()));
    });
    state.refresh_tree(None);

    let a_pos = state
        .flattened_tree
        .iter()
        .position(|(n, _)| n == "A")
        .expect("A missing");
    state.list_state.select(Some(a_pos));

    // A has heuristic parent B.
    // B has parent A.
    // Pressing 'u' on A tries to set A.parent = B. CYCLE!

    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('u'),
        KeyModifiers::NONE,
    )));

    // Verify it DID NOT sync
    assert_ne!(
        state.get_effective_parent("A").as_deref(),
        Some("B"),
        "Should not have synced A to B because B is its child"
    );

    // Verify it remains parented to its topological parent (main)
    assert_eq!(
        state.get_effective_parent("A").as_deref(),
        Some(main.as_str()),
        "A should have remained child of main"
    );
}

#[test]
fn test_heuristic_sync_by_commit_summary() {
    let (dir, repo, main) = setup_repo();
    let path = dir.path();
    let main_oid = repo.head().unwrap().target().unwrap();

    repo.set_head_detached(main_oid).unwrap();

    // 1. Setup main -> feat1 (parent) -> feat2 (child)
    let summary1 = "feat1 unique summary";
    let oid1 = create_commit(&repo, "f1.txt", "1", summary1);
    repo.branch("feat1", &repo.find_commit(oid1).unwrap(), false)
        .unwrap();

    let oid2 = create_commit(&repo, "f2.txt", "2", "feat2 summary");
    repo.branch("feat2", &repo.find_commit(oid2).unwrap(), false)
        .unwrap();

    // 2. Amend feat1 (change OID, keep summary)
    run_git(path, &["checkout", "-f", "feat1"]);
    std::fs::write(path.join("f1.txt"), "1 amended").unwrap();
    run_git(path, &["add", "f1.txt"]);
    run_git(path, &["commit", "--amend", "-m", summary1]);

    // feat2's parent is the old feat1 commit, no longer a branch head; feat1 is a
    // new commit with the same summary.

    let git = RealGit::new(path).unwrap();
    let (branches, history) = git.get_branches(None).unwrap();

    let mut state = AppState {
        branches,
        history,
        ..AppState::default()
    };
    state.refresh_tree(None);

    // Initial state: feat2 should be under main (since its parent OID is lost)
    assert_eq!(
        state.get_effective_parent("feat2").as_ref().unwrap(),
        &main,
        "feat2 should have fallen back to main"
    );

    // feat1 is the heuristic parent: feat2's history has a commit with feat1's tip summary.
    let feat2_info = state
        .branches
        .iter()
        .find(|b| b.name == "feat2")
        .expect("feat2 missing");
    assert_eq!(
        feat2_info.heuristic_parent.as_deref(),
        Some("feat1"),
        "Should have found feat1 via commit summary matching"
    );

    // 3. Simulate pressing 'u' on feat2
    let feat2_pos = state
        .flattened_tree
        .iter()
        .position(|(n, _)| n == "feat2")
        .expect("feat2 missing from tree");
    state.list_state.select(Some(feat2_pos));

    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    state.update(Msg::KeyPressed(KeyEvent::new(
        KeyCode::Char('u'),
        KeyModifiers::NONE,
    )));

    // 4. Verify feat2 is now parented to feat1
    assert_eq!(
        state.get_effective_parent("feat2").as_deref(),
        Some("feat1"),
        "feat2 should have synced back to feat1 via summary identity"
    );
}

#[test]
fn test_heuristic_ambiguous_summary() {
    let (dir, repo, _main) = setup_repo();
    let path = dir.path();

    // 1. Create a base branch with a unique summary
    let summary = "Unique Shared Summary";
    let oid_base = create_commit(&repo, "base.txt", "base", "Base Commit");
    repo.branch("base-branch", &repo.find_commit(oid_base).unwrap(), false)
        .unwrap();

    repo.set_head_detached(oid_base).unwrap();
    let oid1 = create_commit(&repo, "1.txt", "1", summary);
    repo.branch("feat1", &repo.find_commit(oid1).unwrap(), false)
        .unwrap();

    let oid2 = create_commit(&repo, "2.txt", "2", summary);
    repo.branch("feat2", &repo.find_commit(oid2).unwrap(), false)
        .unwrap();

    // 2. Create a child branch whose history contains oid1
    repo.set_head_detached(oid1).unwrap();
    let oid3 = create_commit(&repo, "3.txt", "3", "feat3 head");
    repo.branch("feat3", &repo.find_commit(oid3).unwrap(), false)
        .unwrap();

    // 3. Amend feat1 and feat2 so they point to new commits but keep the same summaries
    run_git(path, &["checkout", "feat1"]);
    create_commit(&repo, "1a.txt", "1a", summary);

    run_git(path, &["checkout", "feat2"]);
    create_commit(&repo, "2a.txt", "2a", summary);

    let git = RealGit::new(path).unwrap();
    let (branches, _) = git.get_branches(None).unwrap();
    let feat3 = branches.iter().find(|b| b.name == "feat3").unwrap();

    // feat3 is diverged: its parent oid1 is no tip, but has the summary of feat1's
    // and feat2's tips.
    assert!(
        feat3.heuristic_parent.is_some(),
        "Should have found a heuristic parent for feat3. Found: {:?}",
        feat3.heuristic_parent
    );
    let hp = feat3.heuristic_parent.as_ref().unwrap();
    assert!(
        hp == "feat1" || hp == "feat2",
        "Heuristic parent should be feat1 or feat2, got: {}",
        hp
    );
}

#[test]
fn test_heuristic_ambiguous_summary_prefers_shared_history() {
    let (dir, repo, main) = setup_repo();
    let path = dir.path();
    let main_oid = repo
        .find_branch(&main, git2::BranchType::Local)
        .unwrap()
        .get()
        .target()
        .unwrap();

    // base: three commits; child on top of it.
    repo.set_head_detached(main_oid).unwrap();
    let oid_one = create_commit(&repo, "1.txt", "1", "base one");
    let oid_two = create_commit(&repo, "2.txt", "2", "base two");
    let oid_three = create_commit(&repo, "3.txt", "3", "base three");
    repo.branch("base", &repo.find_commit(oid_three).unwrap(), false)
        .unwrap();
    let oid_child = create_commit(&repo, "c.txt", "c", "child head");
    repo.branch("child", &repo.find_commit(oid_child).unwrap(), false)
        .unwrap();

    // aside: the same tip summary as base, straight on main, and a name that
    // sorts first.
    repo.set_head_detached(main_oid).unwrap();
    let oid_aside = create_commit(&repo, "a.txt", "a", "base three");
    repo.branch("aside", &repo.find_commit(oid_aside).unwrap(), false)
        .unwrap();

    // base is rewritten without its first commit: child's history has the
    // old tip, and two of base's commits, but one of aside's.
    run_git(path, &["checkout", "-q", "base"]);
    run_git(path, &["reset", "--hard", "-q", &main_oid.to_string()]);
    run_git(path, &["cherry-pick", &oid_two.to_string()]);
    run_git(path, &["cherry-pick", &oid_three.to_string()]);
    let _ = oid_one;

    let git = RealGit::new(path).unwrap();
    let (branches, _) = git.get_branches(None).unwrap();
    let child = branches.iter().find(|b| b.name == "child").unwrap();
    assert_eq!(child.heuristic_parent.as_deref(), Some("base"));
    assert_eq!(child.heuristic_upstream_oid, Some(oid_three));
}

#[test]
fn test_heuristic_summary_match_in_history() {
    let (dir, repo, _main) = setup_repo();
    let path = dir.path();

    // 1. Create a branch "parent-br"
    let oid_p = create_commit(&repo, "p.txt", "p", "Parent Summary");
    repo.branch("parent-br", &repo.find_commit(oid_p).unwrap(), false)
        .unwrap();

    // 2. Commit with same summary in history of another branch
    repo.set_head_detached(
        repo.find_branch("main", git2::BranchType::Local)
            .unwrap()
            .get()
            .target()
            .unwrap(),
    )
    .unwrap();
    let _oid_a = create_commit(&repo, "a.txt", "a", "Parent Summary");

    // 3. Commit without matching summary on top
    let oid_b = create_commit(&repo, "b.txt", "b", "Feature B");
    repo.branch("feat-b", &repo.find_commit(oid_b).unwrap(), false)
        .unwrap();

    // Amend parent-br to make it diverged
    run_git(path, &["checkout", "parent-br"]);
    create_commit(&repo, "p2.txt", "p2", "Parent Summary");

    let git = RealGit::new(path).unwrap();
    let (branches, _) = git.get_branches(None).unwrap();
    let b = branches.iter().find(|b| b.name == "feat-b").unwrap();

    // Summary match in history should be found
    assert_eq!(b.heuristic_parent.as_deref(), Some("parent-br"));
}

#[test]
fn test_heuristic_precedence_local_vs_remote() {
    let (dir, repo, main) = setup_repo();
    let path = dir.path();

    // Detach HEAD so master doesn't move
    repo.set_head_detached(repo.head().unwrap().target().unwrap())
        .unwrap();

    let shared_summary = "Shared Summary";

    // 1. Create remote branch 'origin/feat' at OID 1
    let oid1 = create_commit(&repo, "r.txt", "remote", shared_summary);
    repo.branch("origin/feat", &repo.find_commit(oid1).unwrap(), false)
        .unwrap();

    // 2. Create local branch 'feat' at OID 2 (different OID, same summary)
    repo.set_head_detached(
        repo.find_branch(&main, git2::BranchType::Local)
            .unwrap()
            .get()
            .target()
            .unwrap(),
    )
    .unwrap();
    let oid2 = create_commit(&repo, "l.txt", "local", shared_summary);
    repo.branch("feat", &repo.find_commit(oid2).unwrap(), false)
        .unwrap();

    // 3. Create 'child' branch whose history contains a commit with 'shared_summary'
    repo.set_head_detached(oid1).unwrap(); // based on remote for test diversity
    let oid_c = create_commit(&repo, "c.txt", "child", "Child Commit");
    repo.branch("child", &repo.find_commit(oid_c).unwrap(), false)
        .unwrap();

    // Now amend origin/feat and feat to diverge
    run_git(path, &["checkout", "origin/feat"]);
    create_commit(&repo, "r2.txt", "r2", shared_summary);
    run_git(path, &["checkout", "feat"]);
    create_commit(&repo, "l2.txt", "l2", shared_summary);

    let git = RealGit::new(path).unwrap();
    // show_remote, so origin/feat is in the summary map.
    let (branches, _) = git.get_branches(None).unwrap();

    let child = branches
        .iter()
        .find(|b| b.name == "child")
        .expect("child missing");

    // The local 'feat' wins the summary collision.
    assert_eq!(
        child.heuristic_parent.as_deref(),
        Some("feat"),
        "Local branch should take precedence over remote for heuristic parent"
    );
}

#[test]
fn test_heuristic_diverged_at_head() {
    let (dir, repo, main) = setup_repo();
    let path = dir.path();

    // Detach HEAD so master doesn't move
    repo.set_head_detached(repo.head().unwrap().target().unwrap())
        .unwrap();

    let shared_summary = "Diverged Summary";

    // 1. Create branch 'feat-a' at OID 1
    let oid1 = create_commit(&repo, "a.txt", "a", shared_summary);
    repo.branch("feat-a", &repo.find_commit(oid1).unwrap(), false)
        .unwrap();

    // 2. Create branch 'feat-b' at OID 2 (different OID, same summary), also from master
    repo.set_head_detached(
        repo.find_branch(&main, git2::BranchType::Local)
            .unwrap()
            .get()
            .target()
            .unwrap(),
    )
    .unwrap();
    let oid2 = create_commit(&repo, "b.txt", "b", shared_summary);
    repo.branch("feat-b", &repo.find_commit(oid2).unwrap(), false)
        .unwrap();

    let git = RealGit::new(path).unwrap();
    let (branches, _) = git.get_branches(None).unwrap();

    let b = branches
        .iter()
        .find(|b| b.name == "feat-b")
        .expect("feat-b missing");

    // Even if it's just one commit, it should find feat-a (or vice-versa)
    assert!(
        b.heuristic_parent.is_some(),
        "Should find a heuristic parent even if it's the tip commit"
    );
    let hp = b.heuristic_parent.as_ref().unwrap();
    assert!(
        hp == "feat-a" || hp == "main",
        "Heuristic parent should be feat-a (or main if summaries collide)"
    );
    // The summaries differ, so the heuristic must pick feat-a deterministically.
    assert_eq!(hp, "feat-a");
}
