//! Rebases leave the working tree alone, except for the branch checked out
//! here and conflicts the user resolves there.

use gitui::engine::{ConflictMode, Operation};
use gitui::testing::{run_git, run_git_with_env, setup_repo};
use gitui::{OpResult, RepoState, execute_op_with, repo_status};
use tempfile::TempDir;

struct Repo {
    _dir: TempDir,
    path: String,
    main: String,
}

impl Repo {
    fn new() -> Self {
        let (dir, _repo, main) = setup_repo();
        let path = dir.path().to_str().unwrap().to_string();
        Self {
            _dir: dir,
            path,
            main,
        }
    }

    fn git(&self, args: &[&str]) -> String {
        run_git(&self.path, args)
    }

    fn commit(&self, file: &str, content: &str, msg: &str) {
        std::fs::write(format!("{}/{}", self.path, file), content).unwrap();
        self.git(&["add", file]);
        // Fixed dates: a copy keeps the author date, and gets a new committer date.
        run_git_with_env(
            &self.path,
            &["commit", "-q", "-m", msg],
            vec![
                ("GIT_AUTHOR_DATE", "1700000000 +0130"),
                ("GIT_COMMITTER_DATE", "1700000000 +0130"),
            ],
        );
    }

    /// `feat` with f.txt on main, then m.txt on main.
    fn feat_behind_main(&self) {
        self.commit("base.txt", "base\n", "Base");
        self.git(&["checkout", "-q", "-b", "feat"]);
        self.commit("f.txt", "f\n", "Add f\n\nWith a body.");
        self.git(&["checkout", "-q", &self.main]);
        self.commit("m.txt", "m\n", "Add m");
    }

    fn rebase(&self, branch: &str, mode: ConflictMode) -> OpResult {
        let op = Operation::Rebase {
            branch: branch.to_string(),
            onto: self.main.clone(),
            upstream: None,
            predicted_conflict: None,
        };
        execute_op_with(&self.path, &op, mode).unwrap()
    }

    fn read(&self, file: &str) -> Option<String> {
        std::fs::read_to_string(format!("{}/{}", self.path, file)).ok()
    }
}

#[test]
fn rebase_leaves_the_checkout_alone() {
    let r = Repo::new();
    r.feat_behind_main();
    r.git(&["checkout", "-q", "-b", "other"]);
    std::fs::write(format!("{}/base.txt", r.path), "uncommitted\n").unwrap();
    let before = r.git(&["log", "-1", "--format=%an %ae %ad%n%B", "feat"]);

    let res = r.rebase("feat", ConflictMode::Leave);

    assert!(matches!(res, OpResult::Ok { .. }), "{:?}", res);
    assert_eq!(r.git(&["rev-parse", "--abbrev-ref", "HEAD"]), "other");
    assert_eq!(r.read("base.txt").as_deref(), Some("uncommitted\n"));
    assert_eq!(
        r.git(&["rev-parse", "feat^"]),
        r.git(&["rev-parse", &r.main])
    );
    assert_eq!(r.git(&["show", "feat:f.txt"]), "f");
    assert_eq!(
        r.git(&["log", "-1", "--format=%an %ae %ad%n%B", "feat"]),
        before
    );
}

#[test]
fn checked_out_branch_follows_its_rebased_tip() {
    let r = Repo::new();
    r.feat_behind_main();
    r.git(&["checkout", "-q", "feat"]);

    let res = r.rebase("feat", ConflictMode::Leave);

    assert!(matches!(res, OpResult::Ok { .. }), "{:?}", res);
    assert_eq!(r.git(&["rev-parse", "--abbrev-ref", "HEAD"]), "feat");
    assert_eq!(r.read("m.txt").as_deref(), Some("m\n"));
    assert_eq!(r.git(&["status", "--porcelain"]), "");
}

fn conflicting(r: &Repo) {
    r.commit("c.txt", "base\n", "Base");
    r.git(&["checkout", "-q", "-b", "feat"]);
    r.commit("c.txt", "feat\n", "Feat");
    r.git(&["checkout", "-q", &r.main]);
    r.commit("c.txt", "main\n", "Main");
}

#[test]
fn reported_conflict_moves_nothing() {
    let r = Repo::new();
    conflicting(&r);
    let feat = r.git(&["rev-parse", "feat"]);

    let res = r.rebase("feat", ConflictMode::Report);

    match res {
        OpResult::Conflict {
            conflicted_paths,
            rebase_in_progress: false,
            ..
        } => assert_eq!(conflicted_paths, vec!["c.txt".to_string()]),
        res => panic!("expected a conflict, got {:?}", res),
    }
    assert_eq!(r.git(&["rev-parse", "feat"]), feat);
    assert_eq!(r.git(&["rev-parse", "--abbrev-ref", "HEAD"]), r.main);
    assert!(matches!(
        repo_status(&r.path).unwrap().state,
        RepoState::Clean
    ));
}

#[test]
fn conflict_left_for_the_user_rebases_in_the_checkout() {
    let r = Repo::new();
    conflicting(&r);

    let res = r.rebase("feat", ConflictMode::Leave);

    assert!(
        matches!(
            res,
            OpResult::Conflict {
                rebase_in_progress: true,
                ..
            }
        ),
        "{:?}",
        res
    );
}

#[test]
fn checked_out_branch_follows_with_a_separate_git_dir() {
    let r = Repo::new();
    r.feat_behind_main();
    r.git(&["checkout", "-q", "feat"]);
    let gitdir = tempfile::tempdir().unwrap();
    let moved = gitdir.path().join("feat.git");
    r.git(&["init", "-q", "--separate-git-dir", moved.to_str().unwrap()]);

    let res = r.rebase("feat", ConflictMode::Leave);

    assert!(matches!(res, OpResult::Ok { .. }), "{:?}", res);
    assert_eq!(r.git(&["rev-parse", "--abbrev-ref", "HEAD"]), "feat");
    assert_eq!(r.read("m.txt").as_deref(), Some("m\n"));
    assert_eq!(r.git(&["status", "--porcelain"]), "");
}

#[test]
fn branch_in_another_worktree_is_not_moved() {
    let r = Repo::new();
    r.feat_behind_main();
    let other = tempfile::tempdir().unwrap();
    let wt = other.path().join("wt");
    r.git(&["worktree", "add", "-q", wt.to_str().unwrap(), "feat"]);
    let feat = r.git(&["rev-parse", "feat"]);

    let res = r.rebase("feat", ConflictMode::Leave);

    match res {
        OpResult::Error { message, .. } => assert!(message.contains("checked out"), "{}", message),
        res => panic!("expected an error, got {:?}", res),
    }
    assert_eq!(r.git(&["rev-parse", "feat"]), feat);
}

#[test]
fn commits_already_on_onto_are_dropped() {
    let r = Repo::new();
    r.feat_behind_main();
    r.git(&["cherry-pick", "feat"]);

    let res = r.rebase("feat", ConflictMode::Leave);

    assert!(matches!(res, OpResult::Ok { .. }), "{:?}", res);
    assert_eq!(
        r.git(&["rev-parse", "feat"]),
        r.git(&["rev-parse", &r.main])
    );
}

#[test]
fn commits_already_in_place_are_kept() {
    let r = Repo::new();
    r.feat_behind_main();
    r.git(&["checkout", "-q", "-b", "on-main"]);
    r.commit("o.txt", "o\n", "On main");
    let before = r.git(&["rev-parse", "on-main"]);

    let res = r.rebase("on-main", ConflictMode::Leave);

    assert!(matches!(res, OpResult::Ok { .. }), "{:?}", res);
    assert_eq!(r.git(&["rev-parse", "on-main"]), before);
}

#[test]
fn copies_are_signed_when_commits_are() {
    let r = Repo::new();
    let key = format!("{}/.git/signing-key", r.path);
    let keygen = std::process::Command::new("ssh-keygen")
        .args(["-q", "-t", "ed25519", "-N", "", "-f", &key])
        .status()
        .unwrap();
    assert!(keygen.success());
    r.git(&["config", "gpg.format", "ssh"]);
    r.git(&["config", "user.signingKey", &key]);
    r.git(&["config", "commit.gpgSign", "true"]);
    r.feat_behind_main();

    let res = r.rebase("feat", ConflictMode::Leave);

    assert!(matches!(res, OpResult::Ok { .. }), "{:?}", res);
    assert!(r.git(&["cat-file", "commit", "feat"]).contains("gpgsig"));
}
