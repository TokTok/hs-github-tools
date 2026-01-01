# Git Stack Manager (gitui)

A simple Git TUI for managing branch stacks and complex branch trees.

## Overview

This tool is designed to simplify the management of "stacked" branches, where
multiple feature branches are built on top of each other. It provides a visual
representation of the branch hierarchy and allows for easy restructuring of
entire subtrees.

## Key Features

- **Visual Branch Tree:** Automatically detects and displays the relationship
  between local and remote branches.
- **Interactive Move:** "Grab" a branch and move it to a new parent. The tool
  handles the rebase of the entire subtree.
- **Predictive Conflict Detection:** Highlights potential merge conflicts
  _while_ you are moving a branch, before any action is taken.
- **Heuristic Repair (`u`):** Automatically detects when a branch has drifted
  from its true parent (e.g. after a remote rebase) and allows you to
  "converge" it back with a single keypress.
- **Split Branch (`x`):** Interactively decompose a single commit into
  multiple sequential branches by selecting specific hunks.
- **Remote Visibility:** Toggle between local-only and tracking views for
  `origin` and `upstream` remotes.
- **Submit Workflow:** Plan and execute branch submissions to `upstream` with
  automatic sync to `origin`.
- **Localize Remotes:** Easily create local tracking branches from remote
  branches by simply moving them in the tree.
- **No Checkouts:** Rebases and branch moves don't touch the working tree.
  Only the branch checked out here takes its files along, and a rebase that
  conflicts is done in the working tree for you to resolve.
- **Branch Management:** Directly push (`p`), delete (`d`), reset (`r`),
  rename (`R`), or amend (`m`/`M`) branches from the TUI.
- **GitHub PR Display:** When the `gh` CLI is available and authenticated,
  open pull requests are annotated next to each branch in the tree (cyan
  `#123`, or grey `#123 (draft)` for drafts). Degrades silently when `gh`
  is missing, unauthenticated, or the remote is not on GitHub.

## Shortcuts

### Navigation

- `j` / `Down`: Move selection down.
- `k` / `Up`: Move selection up.
- `a`: Toggle showing remote branches from `origin` and `upstream`.

### Manipulation

- `Space`: Grab or drop a branch. While grabbed, use `j`/`k` to select a new
  parent, or `h` to move to root.
- `p`: Toggle pending push (for local branches with ahead commits).
- `s`: Toggle pending submit (push to `upstream`, delete from `origin`, merge
  to `master`).
- `x`: Enter Split Branch mode (only available if 1 commit ahead of parent).
- `u`: Converge diverged branch (move to heuristic parent).
- `d`: Toggle pending delete.
- `r`: Toggle pending reset to upstream (or rebase onto upstream if
  ahead/behind).
- `m`: Toggle pending amend (amends current staged changes into the selected
  branch).
- `M`: Toggle pending amend with message update.
- `R`: Rename the selected branch.
- `f`: Toggle pending localize (for remote branches).

### Execution

- `v`: Enter Preview mode to see planned operations and predicted conflicts.
- `c`: Execute all pending operations.
- `Esc`: Cancel current grab or quit the current mode.
- `q`: Quit.

## CLI Usage

```bash
# Start the TUI in the current directory
gitui

# Start in a specific directory
gitui --path /path/to/repo

# Print the current tree and exit
gitui --tree

# Print the tree including remote branches
gitui --tree --all

# Show the submission plan for a branch and exit
gitui --submit branch-name

# Show the plan to fix a diverged branch (converge)
gitui --converge branch-name

# Show the plan to sync a branch with its upstream
gitui --sync branch-name

# Run that plan, rebasing every branch stacked on it. A branch that conflicts
# is left as it was and the branches that don't sit on it are still rebased.
gitui --sync branch-name --apply --keep-going

# Report current repo state (rebase-in-progress, conflicted paths, etc.)
gitui --status

# Execute a single rebase operation
gitui --rebase branch onto [upstream]

# Emit machine-readable JSON instead of human-readable output
# (combine with --tree, --submit, --converge, --sync, --status, or --rebase)
gitui --status --json
```

## Development

This tool is built with:

- **Language:** Rust
- **UI:** [Ratatui](https://ratatui.rs/)
- **Git Engine:** [git2-rs](https://github.com/rust-lang/git2-rs)
- **Build System:** Bazel
- **Optional runtime dependency:** [`gh`](https://cli.github.com/) — used
  for PR display in the tree view. Absence is handled gracefully.

### Building

```bash
bazel build //hs-github-tools/tools/gitui:gitui
```

### Testing

```bash
bazel test //hs-github-tools/tools/gitui/...
```
