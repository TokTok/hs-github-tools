//! Moving branches without a checkout.
//!
//! A rebase replays each commit with `git merge-tree` and `git commit-tree`,
//! then moves the branch with `git update-ref`. Only a branch checked out here
//! takes the working tree along, with `git reset --keep`.

use crate::engine::git::RealGit;
use std::collections::HashSet;
use std::ffi::OsStr;
use std::io::Write;
use std::os::unix::ffi::OsStrExt;
use std::process::{Output, Stdio};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RebaseOutcome {
    Done,
    /// A commit didn't apply. Nothing moved.
    Conflict {
        paths: Vec<String>,
    },
}

enum Checkout {
    Nowhere,
    Here,
    Elsewhere(String),
}

impl RealGit {
    fn git_output(&self, args: &[&str]) -> anyhow::Result<Output> {
        Ok(self.git_cmd().args(args).stdin(Stdio::null()).output()?)
    }

    fn git_ok(&self, args: &[&str]) -> anyhow::Result<String> {
        let out = self.git_output(args)?;
        anyhow::ensure!(
            out.status.success(),
            "git {} failed: {}",
            args.join(" "),
            String::from_utf8_lossy(&out.stderr).trim()
        );
        Ok(String::from_utf8(out.stdout)?.trim().to_string())
    }

    fn commit_oid(&self, rev: &str) -> anyhow::Result<String> {
        self.git_ok(&["rev-parse", "--verify", &format!("{rev}^{{commit}}")])
    }

    fn tree_oid(&self, rev: &str) -> anyhow::Result<String> {
        self.git_ok(&["rev-parse", "--verify", &format!("{rev}^{{tree}}")])
    }

    fn branch_tip(&self, branch: &str) -> anyhow::Result<Option<String>> {
        let out = self.git_output(&[
            "rev-parse",
            "--verify",
            "--quiet",
            &format!("refs/heads/{branch}"),
        ])?;
        Ok(out
            .status
            .success()
            .then(|| String::from_utf8_lossy(&out.stdout).trim().to_string()))
    }

    fn checkout_of(&self, branch: &str) -> anyhow::Result<Checkout> {
        let name = format!("refs/heads/{branch}");
        // By HEAD, not by path: with a separate git dir (submodules), the
        // worktree list names the main worktree by its git dir.
        if self.git_ok(&["symbolic-ref", "-q", "HEAD"]).ok().as_deref() == Some(name.as_str()) {
            return Ok(Checkout::Here);
        }
        let list = self.git_ok(&["worktree", "list", "--porcelain"])?;
        let wanted = format!("branch {name}");
        let mut worktree = None;
        for line in list.lines() {
            if let Some(path) = line.strip_prefix("worktree ") {
                worktree = Some(path);
            } else if line == wanted
                && let Some(path) = worktree
            {
                return Ok(Checkout::Elsewhere(path.to_string()));
            }
        }
        Ok(Checkout::Nowhere)
    }

    /// Points `branch` at `new`. With `old`, only if it's still there.
    fn set_branch(
        &self,
        branch: &str,
        new: &str,
        old: Option<&str>,
        why: &str,
    ) -> anyhow::Result<()> {
        if let Some(old) = old {
            anyhow::ensure!(
                self.branch_tip(branch)?.as_deref() == Some(old),
                "{branch} moved while gitui worked on it"
            );
        }
        match self.checkout_of(branch)? {
            Checkout::Elsewhere(path) => anyhow::bail!("{branch} is checked out in {path}"),
            // The files follow the branch. Changes that would be lost stop it.
            Checkout::Here => self.git_ok(&["reset", "-q", "--keep", new]).map(drop),
            Checkout::Nowhere => {
                let name = format!("refs/heads/{branch}");
                let mut args = vec!["update-ref", "-m", why, name.as_str(), new];
                args.extend(old);
                self.git_ok(&args).map(drop)
            }
        }
    }

    /// Moves `branch` to `to`, creating it if needed.
    pub(crate) fn move_branch_to(
        &self,
        branch: &str,
        to: &str,
        fast_forward_only: bool,
    ) -> anyhow::Result<()> {
        let new = self.commit_oid(to)?;
        let old = self.branch_tip(branch)?;
        if fast_forward_only && let Some(old) = &old {
            let ff = self
                .git_cmd()
                .args(["merge-base", "--is-ancestor", old, &new])
                .status()?;
            anyhow::ensure!(ff.success(), "{branch} can't fast-forward to {to}");
        }
        if old.as_deref() == Some(new.as_str()) {
            return Ok(());
        }
        self.set_branch(
            branch,
            &new,
            old.as_deref(),
            &format!("gitui: {branch} to {to}"),
        )
    }

    pub(crate) fn delete_merged_branch(
        &self,
        branch: &str,
        merged_into: &str,
    ) -> anyhow::Result<()> {
        let merged = self
            .git_cmd()
            .args(["merge-base", "--is-ancestor", branch, merged_into])
            .status()?;
        anyhow::ensure!(merged.success(), "{branch} isn't merged into {merged_into}");
        match self.checkout_of(branch)? {
            Checkout::Elsewhere(path) => anyhow::bail!("{branch} is checked out in {path}"),
            // The branch goes away, so this checkout goes where it landed.
            Checkout::Here => self.git_ok(&["checkout", "-q", merged_into]).map(drop)?,
            Checkout::Nowhere => {}
        }
        self.git_ok(&["branch", "-q", "-D", branch]).map(drop)
    }

    /// `git rebase [--onto ONTO UPSTREAM | ONTO] BRANCH`, without a checkout.
    ///
    /// As in git rebase: merges are left out, commits already on `onto` are
    /// dropped, and so are commits that become empty.
    pub(crate) fn replay_branch(
        &self,
        branch: &str,
        onto: &str,
        upstream: Option<&str>,
    ) -> anyhow::Result<RebaseOutcome> {
        let old = self.commit_oid(&format!("refs/heads/{branch}"))?;
        let onto = self.commit_oid(onto)?;
        let upstream = upstream.unwrap_or(&onto);

        let not_on_onto: HashSet<String> = self
            .git_ok(&[
                "rev-list",
                "--no-merges",
                "--cherry-pick",
                "--right-only",
                &format!("{onto}...{old}"),
            ])?
            .lines()
            .map(str::to_owned)
            .collect();
        let commits = self.git_ok(&[
            "rev-list",
            "--reverse",
            "--no-merges",
            &format!("{upstream}..{old}"),
        ])?;

        // commit-tree is plumbing: it signs only when asked, unlike git rebase.
        let sign = self
            .git_output(&["config", "--type=bool", "--get", "commit.gpgSign"])?
            .stdout
            .starts_with(b"true");
        let mut tip = onto;
        for commit in commits.lines().filter(|c| not_on_onto.contains(*c)) {
            if self.first_parent(commit)?.as_deref() == Some(tip.as_str()) {
                // Already in place: git rebase keeps it as it is.
                tip = commit.to_string();
                continue;
            }
            let base = format!("{commit}^");
            let out = self.git_output(&[
                "merge-tree",
                "--write-tree",
                "--name-only",
                "--no-messages",
                "--merge-base",
                &base,
                &tip,
                commit,
            ])?;
            let stdout = String::from_utf8(out.stdout)?;
            let mut lines = stdout.lines();
            let tree = lines.next().unwrap_or_default().to_string();
            match out.status.code() {
                Some(0) => {}
                Some(1) => {
                    let paths = lines.take_while(|l| !l.is_empty()).map(str::to_owned);
                    return Ok(RebaseOutcome::Conflict {
                        paths: paths.collect(),
                    });
                }
                _ => anyhow::bail!(
                    "git merge-tree failed on {commit}: {}",
                    String::from_utf8_lossy(&out.stderr).trim()
                ),
            }
            if tree == self.tree_oid(&tip)? && self.tree_oid(commit)? != self.tree_oid(&base)? {
                continue;
            }
            tip = self.commit_like(commit, &tree, &tip, sign)?;
        }

        if tip != old {
            self.set_branch(branch, &tip, Some(&old), &format!("gitui: rebase {branch}"))?;
        }
        Ok(RebaseOutcome::Done)
    }

    fn first_parent(&self, commit: &str) -> anyhow::Result<Option<String>> {
        let repo = self.repo()?;
        let c = repo.find_commit(git2::Oid::from_str(commit)?)?;
        Ok(c.parent_ids().next().map(|p| p.to_string()))
    }

    /// A copy of `commit` with `tree` on `parent`: same author and message.
    fn commit_like(
        &self,
        commit: &str,
        tree: &str,
        parent: &str,
        sign: bool,
    ) -> anyhow::Result<String> {
        let (name, email, date, message) = {
            let repo = self.repo()?;
            let c = repo.find_commit(git2::Oid::from_str(commit)?)?;
            let author = c.author();
            let when = author.when();
            let offset = when.offset_minutes();
            let date = format!(
                "{} {}{:02}{:02}",
                when.seconds(),
                if offset < 0 { '-' } else { '+' },
                offset.abs() / 60,
                offset.abs() % 60
            );
            (
                author.name_bytes().to_vec(),
                author.email_bytes().to_vec(),
                date,
                c.message_raw_bytes().to_vec(),
            )
        };
        let mut child = self
            .git_cmd()
            .args(["commit-tree", tree, "-p", parent, "-F", "-"])
            .args(sign.then_some("-S"))
            .env("GIT_AUTHOR_NAME", OsStr::from_bytes(&name))
            .env("GIT_AUTHOR_EMAIL", OsStr::from_bytes(&email))
            .env("GIT_AUTHOR_DATE", date)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        child
            .stdin
            .take()
            .ok_or_else(|| anyhow::anyhow!("no stdin for git commit-tree"))?
            .write_all(&message)?;
        let out = child.wait_with_output()?;
        anyhow::ensure!(
            out.status.success(),
            "git commit-tree failed for {commit}: {}",
            String::from_utf8_lossy(&out.stderr).trim()
        );
        Ok(String::from_utf8(out.stdout)?.trim().to_string())
    }
}
