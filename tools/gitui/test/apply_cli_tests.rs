//! CLI-surface tests for `gitui --apply`. Each test shells out to the built
//! binary (path injected via the `GITUI_BIN` env var by Bazel) and asserts on
//! exit codes, stdout/stderr, and parsed JSON output.
//!
//! Companion to `apply_plan_integration_tests.rs` (which covers the library
//! functions); these tests lock in the argv parsing, exit-code mapping, and
//! JSON wiring that lives in `main.rs`.

use std::path::PathBuf;
use std::process::{Command, Output};
use tempfile::TempDir;

use gitui::OpResult;
use gitui::testing::{run_git, setup_repo};

// ============================================================================
// Binary locator
// ============================================================================

fn gitui_bin() -> PathBuf {
    let raw = std::env::var("GITUI_BIN").expect(
        "GITUI_BIN env var must point to the built gitui binary \
         (set via Bazel `data = [\":gitui\"]` + `env = {\"GITUI_BIN\": \"$(rootpath :gitui)\"}`)",
    );
    PathBuf::from(raw)
}

fn run_gitui(args: &[&str]) -> Output {
    Command::new(gitui_bin())
        .args(args)
        .output()
        .expect("failed to spawn gitui binary")
}

// ============================================================================
// Fixture (a copy of apply_plan_integration_tests.rs's, kept out of the library API)
// ============================================================================

struct Fixture {
    _main_dir: TempDir,
    _origin_dir: TempDir,
    _upstream_dir: TempDir,
    main_branch: String,
    path: String,
}

fn setup_with_remotes() -> Fixture {
    let (main_dir, _repo, branch_name) = setup_repo();
    let origin_dir = tempfile::tempdir().unwrap();
    let upstream_dir = tempfile::tempdir().unwrap();
    let path = main_dir.path().to_str().unwrap().to_string();

    run_git(origin_dir.path(), &["init", "--bare", "-q"]);
    run_git(upstream_dir.path(), &["init", "--bare", "-q"]);
    run_git(
        &path,
        &[
            "remote",
            "add",
            "origin",
            origin_dir.path().to_str().unwrap(),
        ],
    );
    run_git(
        &path,
        &[
            "remote",
            "add",
            "upstream",
            upstream_dir.path().to_str().unwrap(),
        ],
    );
    run_git(
        &path,
        &["push", "-q", "origin", &format!("HEAD:{}", branch_name)],
    );
    run_git(
        &path,
        &["push", "-q", "upstream", &format!("HEAD:{}", branch_name)],
    );
    run_git(
        &path,
        &[
            "branch",
            &format!("--set-upstream-to=origin/{}", branch_name),
            &branch_name,
        ],
    );

    Fixture {
        _main_dir: main_dir,
        _origin_dir: origin_dir,
        _upstream_dir: upstream_dir,
        main_branch: branch_name,
        path,
    }
}

fn commit_file(path: &str, file: &str, content: &str, msg: &str) {
    std::fs::write(format!("{}/{}", path, file), content).unwrap();
    run_git(path, &["add", file]);
    run_git(path, &["commit", "-q", "-m", msg]);
}

fn advance_upstream(fx: &Fixture, file: &str, content: &str, msg: &str) {
    run_git(&fx.path, &["checkout", "-q", &fx.main_branch]);
    let pre = run_git(&fx.path, &["rev-parse", "HEAD"]);
    commit_file(&fx.path, file, content, msg);
    run_git(
        &fx.path,
        &[
            "push",
            "-q",
            "upstream",
            &format!("HEAD:{}", fx.main_branch),
        ],
    );
    run_git(&fx.path, &["reset", "--hard", "-q", &pre]);
}

fn stack_branch(fx: &Fixture, name: &str, parent: &str, file: &str, content: &str) {
    run_git(&fx.path, &["checkout", "-q", parent]);
    run_git(&fx.path, &["checkout", "-q", "-b", name]);
    commit_file(&fx.path, file, content, &format!("commit on {}", name));
}

fn setup_midstack_conflict(fx: &Fixture) {
    run_git(&fx.path, &["checkout", "-q", &fx.main_branch]);
    commit_file(&fx.path, "conflict.txt", "v0\n", "Add conflict.txt v0");
    run_git(
        &fx.path,
        &["push", "-q", "origin", &format!("HEAD:{}", fx.main_branch)],
    );
    run_git(
        &fx.path,
        &[
            "push",
            "-q",
            "upstream",
            &format!("HEAD:{}", fx.main_branch),
        ],
    );
    stack_branch(fx, "A", &fx.main_branch, "a.txt", "A content");
    run_git(&fx.path, &["checkout", "-q", "-b", "B"]);
    commit_file(
        &fx.path,
        "conflict.txt",
        "v-local\n",
        "B changes conflict.txt",
    );
    run_git(&fx.path, &["checkout", "-q", "-b", "C"]);
    commit_file(&fx.path, "c.txt", "C content", "C unrelated");
    advance_upstream(
        fx,
        "conflict.txt",
        "v-upstream\n",
        "upstream changes conflict.txt",
    );
}

fn lossy(bytes: &[u8]) -> String {
    String::from_utf8_lossy(bytes).into_owned()
}

// ============================================================================
// Tests
// ============================================================================

#[test]
fn cli_apply_sync_exit_zero_on_clean_cascade() {
    let fx = setup_with_remotes();
    stack_branch(&fx, "A", &fx.main_branch, "a.txt", "A");
    advance_upstream(&fx, "u.txt", "u", "upstream commit");

    let out = run_gitui(&["--path", &fx.path, "--apply", "--sync", &fx.main_branch]);

    assert!(
        out.status.success(),
        "expected success exit, got {:?}\nstdout: {}\nstderr: {}",
        out.status.code(),
        lossy(&out.stdout),
        lossy(&out.stderr)
    );
}

#[test]
fn cli_apply_sync_exit_nonzero_on_conflict() {
    let fx = setup_with_remotes();
    setup_midstack_conflict(&fx);

    let out = run_gitui(&["--path", &fx.path, "--apply", "--sync", &fx.main_branch]);

    assert!(
        !out.status.success(),
        "expected non-zero exit on conflict, got success\nstdout: {}\nstderr: {}",
        lossy(&out.stdout),
        lossy(&out.stderr)
    );
}

#[test]
fn cli_apply_refuses_when_mid_rebase() {
    let fx = setup_with_remotes();
    setup_midstack_conflict(&fx);

    // Drive into mid-rebase via the first invocation (will conflict on B).
    let _ = run_gitui(&["--path", &fx.path, "--apply", "--sync", &fx.main_branch]);

    // The second invocation must refuse before running any op.
    let out = run_gitui(&["--path", &fx.path, "--apply", "--sync", &fx.main_branch]);

    assert!(!out.status.success(), "expected refusal exit code");
    let combined = format!("{}{}", lossy(&out.stdout), lossy(&out.stderr));
    assert!(
        combined.contains("refusing") || combined.contains("in-progress"),
        "expected refusal message, got: stdout={}\nstderr={}",
        lossy(&out.stdout),
        lossy(&out.stderr)
    );
}

#[test]
fn cli_apply_sync_json_round_trips() {
    let fx = setup_with_remotes();
    stack_branch(&fx, "A", &fx.main_branch, "a.txt", "A");
    advance_upstream(&fx, "u.txt", "u", "upstream commit");

    let out = run_gitui(&[
        "--path",
        &fx.path,
        "--apply",
        "--sync",
        &fx.main_branch,
        "--json",
    ]);

    assert!(
        out.status.success(),
        "expected success, got {:?}\nstderr: {}",
        out.status.code(),
        lossy(&out.stderr)
    );

    let parsed: Vec<OpResult> = serde_json::from_slice(&out.stdout).unwrap_or_else(|e| {
        panic!(
            "stdout must be valid JSON Vec<OpResult>; parse error: {}\nstdout: {}",
            e,
            lossy(&out.stdout)
        )
    });

    assert!(!parsed.is_empty(), "expected non-empty results");
    for r in &parsed {
        assert!(
            matches!(r, OpResult::Ok { .. }),
            "expected all Ok results, got: {:?}",
            r
        );
    }
}

#[test]
fn cli_apply_sync_human_output_mentions_affected_branches() {
    let fx = setup_with_remotes();
    stack_branch(&fx, "A", &fx.main_branch, "a.txt", "A");
    advance_upstream(&fx, "u.txt", "u", "upstream commit");

    let out = run_gitui(&["--path", &fx.path, "--apply", "--sync", &fx.main_branch]);

    assert!(
        out.status.success(),
        "expected success: {}",
        lossy(&out.stderr)
    );

    let combined = format!("{}{}", lossy(&out.stdout), lossy(&out.stderr));
    assert!(
        combined.contains(&fx.main_branch),
        "human output should mention main branch '{}'; got: {}",
        fx.main_branch,
        combined
    );
    assert!(
        combined.contains('A'),
        "human output should mention branch 'A'; got: {}",
        combined
    );
}

#[test]
fn cli_apply_warns_on_un_anchored_branches() {
    // When a branch has no detectable parent, --apply --sync should print a
    // warning to stderr but still proceed with the anchored branches.
    let fx = setup_with_remotes();
    stack_branch(&fx, "Normal", &fx.main_branch, "n.txt", "normal");

    run_git(&fx.path, &["checkout", "-q", "--orphan", "Loner"]);
    let _ = std::process::Command::new("git")
        .arg("-C")
        .arg(&fx.path)
        .args(["rm", "-rf", "--cached", "."])
        .status();
    let _ = std::process::Command::new("git")
        .arg("-C")
        .arg(&fx.path)
        .args(["clean", "-fdx"])
        .status();
    std::fs::write(format!("{}/loner.txt", fx.path), "lonely").unwrap();
    run_git(&fx.path, &["add", "loner.txt"]);
    run_git(
        &fx.path,
        &["commit", "-q", "-m", "Loner orphan commit unique xyz"],
    );

    advance_upstream(&fx, "u.txt", "u", "upstream commit");
    run_git(&fx.path, &["checkout", "-q", &fx.main_branch]);
    let _ = std::process::Command::new("git")
        .arg("-C")
        .arg(&fx.path)
        .args(["clean", "-fdx"])
        .status();

    let out = run_gitui(&["--path", &fx.path, "--apply", "--sync", &fx.main_branch]);

    let stderr = lossy(&out.stderr);
    assert!(
        stderr.contains("Loner") && stderr.contains("skipped"),
        "expected stderr to warn about Loner skip; got stderr: {}",
        stderr
    );
    // The anchored cascade should still succeed.
    assert!(
        out.status.success(),
        "anchored branches should still apply cleanly; stderr: {}",
        stderr
    );
}

#[test]
fn cli_apply_sync_json_parity_with_library_on_conflict() {
    let fx = setup_with_remotes();
    setup_midstack_conflict(&fx);

    let out = run_gitui(&[
        "--path",
        &fx.path,
        "--apply",
        "--sync",
        &fx.main_branch,
        "--json",
    ]);

    assert!(!out.status.success(), "expected non-zero exit on conflict");

    let parsed: Vec<OpResult> = serde_json::from_slice(&out.stdout).unwrap_or_else(|e| {
        panic!(
            "stdout must be valid JSON even on conflict; parse error: {}\nstdout: {}",
            e,
            lossy(&out.stdout)
        )
    });

    let last = parsed.last().expect("at least one result");
    match last {
        OpResult::Conflict {
            branch,
            conflicted_paths,
            rebase_in_progress,
        } => {
            assert_eq!(branch, "B", "expected halt on B, got {}", branch);
            assert!(*rebase_in_progress, "rebase should be in progress");
            assert!(
                conflicted_paths.iter().any(|p| p == "conflict.txt"),
                "expected conflict.txt in paths, got {:?}",
                conflicted_paths
            );
        }
        other => panic!("expected Conflict as last result, got {:?}", other),
    }

    for r in &parsed[..parsed.len() - 1] {
        assert!(
            matches!(r, OpResult::Ok { .. }),
            "earlier op should be Ok: {:?}",
            r
        );
    }

    // C should not have been attempted.
    assert!(
        !parsed.iter().any(|r| r.branch() == "C"),
        "C should not appear in results; got: {:?}",
        parsed
    );
}

fn setup_submittable_branch(fx: &Fixture, name: &str) -> String {
    stack_branch(fx, name, &fx.main_branch, &format!("{}.txt", name), name);
    run_git(&fx.path, &["push", "-q", "origin", name]);
    run_git(
        &fx.path,
        &[
            "branch",
            &format!("--set-upstream-to=origin/{}", name),
            name,
        ],
    );
    run_git(&fx.path, &["rev-parse", name])
}

#[test]
fn cli_apply_submit_executes_and_exits_zero() {
    let fx = setup_with_remotes();
    let feat_tip = setup_submittable_branch(&fx, "feat");

    let out = run_gitui(&["--path", &fx.path, "--apply", "--submit", "feat"]);

    assert!(
        out.status.success(),
        "expected success exit, got {:?}\nstdout: {}\nstderr: {}",
        out.status.code(),
        lossy(&out.stdout),
        lossy(&out.stderr)
    );

    let locals = run_git(&fx.path, &["branch", "--format=%(refname:short)"]);
    assert!(
        !locals.lines().any(|b| b == "feat"),
        "feat should be deleted locally; branches: {}",
        locals
    );

    let upstream_tip = run_git(
        &fx.path,
        &["rev-parse", &format!("upstream/{}", fx.main_branch)],
    );
    assert_eq!(
        upstream_tip, feat_tip,
        "upstream/{} must be at feat's tip after submit",
        fx.main_branch
    );
}

#[test]
fn cli_apply_submit_json_outputs_op_results() {
    let fx = setup_with_remotes();
    setup_submittable_branch(&fx, "feat");

    let out = run_gitui(&["--path", &fx.path, "--apply", "--submit", "feat", "--json"]);

    assert!(out.status.success(), "stderr: {}", lossy(&out.stderr));

    let parsed: Vec<OpResult> = serde_json::from_slice(&out.stdout).unwrap_or_else(|e| {
        panic!(
            "stdout must be JSON; parse error: {}\nstdout: {}",
            e,
            lossy(&out.stdout)
        )
    });
    assert!(!parsed.is_empty(), "expected at least one OpResult");
    for r in &parsed {
        assert!(matches!(r, OpResult::Ok { .. }), "expected Ok, got {:?}", r);
    }
}
