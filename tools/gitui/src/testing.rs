use git2::{Repository, Signature, Time};
use std::fs::File;
use std::io::Write;
use std::process::Command;
use std::sync::{Arc, Mutex};
use tempfile::TempDir;

use crate::diff_utils::FileDiff;
use crate::engine::{BranchInfo, CommitInfo, Git, HistoryContext, SplitData};

pub fn setup_repo() -> (TempDir, Repository, String) {
    let dir = tempfile::tempdir().unwrap();
    let repo = Repository::init(dir.path()).unwrap();
    {
        let mut config = repo.config().unwrap();
        config.set_str("user.name", "Test").unwrap();
        config.set_str("user.email", "test@example.com").unwrap();
    }

    let branch_name = {
        // Use a fixed signature for deterministic OIDs in tests
        let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
        let mut index = repo.index().unwrap();
        let id = index.write_tree().unwrap();
        let tree = repo.find_tree(id).unwrap();
        repo.commit(Some("HEAD"), &sig, &sig, "Initial commit", &tree, &[])
            .unwrap();

        let head = repo.head().unwrap();
        head.shorthand().unwrap().to_string()
    };

    (dir, repo, branch_name)
}

pub fn create_commit(repo: &Repository, filename: &str, content: &str, msg: &str) -> git2::Oid {
    let path = repo.workdir().unwrap().join(filename);
    File::create(path)
        .unwrap()
        .write_all(content.as_bytes())
        .unwrap();

    Command::new("git")
        .arg("-C")
        .arg(repo.workdir().unwrap())
        .args(["add", filename])
        .status()
        .unwrap();

    let mut index = repo.index().unwrap();
    index.read(false).unwrap();
    let id = index.write_tree().unwrap();
    let tree = repo.find_tree(id).unwrap();

    let sig = Signature::new("Test", "test@example.com", &Time::new(1735686000, 0)).unwrap();
    let parent = repo.head().unwrap().peel_to_commit().unwrap();
    let oid = repo
        .commit(Some("HEAD"), &sig, &sig, msg, &tree, &[&parent])
        .unwrap();

    Command::new("git")
        .arg("-C")
        .arg(repo.workdir().unwrap())
        .args(["reset", "--hard", "HEAD"])
        .status()
        .unwrap();
    oid
}

pub fn run_git<P: AsRef<std::path::Path>>(path: P, args: &[&str]) -> String {
    run_git_with_env(path, args, vec![])
}

pub fn run_git_with_env<P: AsRef<std::path::Path>>(
    path: P,
    args: &[&str],
    env: Vec<(&str, &str)>,
) -> String {
    let mut cmd = Command::new("git");
    cmd.arg("-C")
        .arg(path.as_ref())
        .args(args)
        .env_remove("GIT_DIR")
        .env_remove("GIT_WORK_TREE");

    for (key, value) in env {
        cmd.env(key, value);
    }

    let output = cmd.output().expect("Failed to execute git command");

    if !output.status.success() {
        panic!(
            "git command failed: git -C {} {:?}\nStdout: {}\nStderr: {}",
            path.as_ref().display(),
            args,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    String::from_utf8(output.stdout).unwrap().trim().to_string()
}

pub fn mock_oid(b: u8) -> git2::Oid {
    let mut bytes = [0u8; 20];
    bytes[0] = b;
    git2::Oid::from_bytes(&bytes).unwrap()
}

pub struct MockGit {
    pub branches: Arc<Mutex<Vec<BranchInfo>>>,
    pub current_branch: String,
    pub rebase_calls: Arc<Mutex<Vec<(String, String)>>>,
    pub check_conflict_between_calls: Arc<Mutex<Vec<(git2::Oid, git2::Oid)>>>,
}

impl MockGit {
    pub fn new(branches: Vec<BranchInfo>) -> Self {
        Self {
            branches: Arc::new(Mutex::new(branches)),
            current_branch: "master".to_string(),
            rebase_calls: Arc::new(Mutex::new(Vec::new())),
            check_conflict_between_calls: Arc::new(Mutex::new(Vec::new())),
        }
    }
}

impl Git for MockGit {
    fn find_parent_for_oid(
        &self,
        _oid: git2::Oid,
        _exclude_name: &str,
        _branches: &std::collections::HashMap<String, git2::Oid>,
    ) -> Option<String> {
        None
    }

    fn get_branches(
        &self,
        _progress: Option<&dyn Fn(String, f64)>,
    ) -> anyhow::Result<(Vec<BranchInfo>, HistoryContext)> {
        Ok((self.branches.lock().unwrap().clone(), HistoryContext::new()))
    }

    fn get_current_branch(&self) -> anyhow::Result<String> {
        Ok(self.current_branch.clone())
    }

    fn check_conflict(&self, _branch_name: &str, _new_parent_name: &str) -> anyhow::Result<bool> {
        Ok(false)
    }

    fn check_conflict_between(&self, oid_a: git2::Oid, oid_b: git2::Oid) -> anyhow::Result<bool> {
        self.check_conflict_between_calls
            .lock()
            .unwrap()
            .push((oid_a, oid_b));
        Ok(false)
    }

    fn is_descendant(&self, _ancestor: git2::Oid, _descendant: git2::Oid) -> anyhow::Result<bool> {
        Ok(true)
    }

    fn rebase(&self, branch: &str, onto: &str) -> anyhow::Result<bool> {
        self.rebase_calls
            .lock()
            .unwrap()
            .push((branch.to_string(), onto.to_string()));
        Ok(true)
    }

    fn checkout(&self, _branch: &str) -> anyhow::Result<()> {
        Ok(())
    }

    fn push(&self, _branch: &str) -> anyhow::Result<bool> {
        Ok(true)
    }

    fn delete_branch(&self, _branch: &str) -> anyhow::Result<bool> {
        Ok(true)
    }

    fn run_command(&self, args: &[String]) -> anyhow::Result<bool> {
        if args[0] == "rebase" {
            self.rebase_calls
                .lock()
                .unwrap()
                .push((args[2].clone(), args[1].clone()));
        }
        Ok(true)
    }

    fn is_dirty(&self) -> anyhow::Result<bool> {
        Ok(false)
    }

    fn reset_to_upstream(&self, _branch: &str) -> anyhow::Result<bool> {
        Ok(true)
    }

    fn get_commit_log(&self, _branch: &str) -> anyhow::Result<Vec<CommitInfo>> {
        Ok(vec![CommitInfo {
            id: "abc1234".to_string(),
            summary: "Mock commit".to_string(),
            author: "Author".to_string(),
        }])
    }

    fn get_diff(&self, _branch: &str, _parent: &str) -> anyhow::Result<Vec<FileDiff>> {
        Ok(Vec::new())
    }

    fn split_branch(&self, _branch: &str, _parent: &str, _data: &SplitData) -> anyhow::Result<()> {
        Ok(())
    }
}
