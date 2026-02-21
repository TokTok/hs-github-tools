use git2::Oid;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct BranchIntent {
    pub parent: Option<Option<String>>,
    pub pending_push: bool,
    pub pending_reset: bool,
    pub pending_delete: bool,
    pub pending_localize: bool,
    pub pending_submit: bool,
    pub pending_amend: bool,
    pub pending_amend_message: Option<String>,
    pub pending_rename: Option<String>,
    pub pending_split: Option<SplitData>,
    pub has_conflict: bool,
}

#[derive(Debug, Clone)]
pub struct BranchInfo {
    pub name: String,
    pub oid: Oid,
    pub upstream_oid: Option<Oid>,
    pub original_parent: Option<String>,
    pub children: Vec<String>,
    pub ahead: usize,
    pub behind: usize,
    pub parent_ahead: usize,
    pub parent_behind: usize,
    pub upstream: Option<String>,
    pub is_merged: bool,
    pub is_ahead: bool,
    pub is_remote: bool,
    pub is_local: bool,
    pub aliases: Vec<String>,
    pub heuristic_parent: Option<String>,
    pub heuristic_upstream_oid: Option<Oid>,
}

pub struct RepositorySnapshot {
    pub branches: Vec<BranchInfo>,
    pub history: crate::topology::HistoryContext,
    pub is_dirty: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SplitPartData {
    pub name: String,
    pub commit_message: String,
    pub selected_hunks: HashSet<(String, usize)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SplitData {
    pub parts: Vec<SplitPartData>,
}

impl BranchInfo {
    pub fn can_submit(&self) -> bool {
        self.is_local
            && self.is_ahead
            && !self.is_merged
            && self.upstream.is_some()
            && self.ahead == 0
            && self.behind == 0
    }
}

impl Default for BranchInfo {
    fn default() -> Self {
        Self {
            name: String::new(),
            oid: Oid::from_bytes(&[0; 20]).unwrap(),
            upstream_oid: None,
            original_parent: None,
            children: Vec::new(),
            ahead: 0,
            behind: 0,
            parent_ahead: 0,
            parent_behind: 0,
            upstream: None,
            is_merged: false,
            is_ahead: false,
            is_remote: false,
            is_local: true,
            aliases: Vec::new(),
            heuristic_parent: None,
            heuristic_upstream_oid: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CommitInfo {
    pub id: String,
    pub summary: String,
    pub author: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operation {
    Reset {
        branch: String,
    },
    Rebase {
        branch: String,
        onto: String,
        upstream: Option<String>,
        predicted_conflict: Option<bool>,
    },
    Push {
        branch: String,
    },
    Submit {
        branch: String,
        target: String,
    },
    Delete {
        branch: String,
    },
    Localize {
        branch: String,
    },
    Amend {
        message: Option<String>,
    },
    Rename {
        branch: String,
        new_name: String,
    },
    Sync {
        branch: String,
        onto: String,
        predicted_conflict: Option<bool>,
    },
    Split {
        branch: String,
        parent: String,
        data: SplitData,
    },
}

impl Operation {
    pub fn commands(&self) -> Vec<Vec<String>> {
        match self {
            Operation::Reset { branch } => vec![
                vec!["checkout".to_string(), branch.clone()],
                vec![
                    "reset".to_string(),
                    "--hard".to_string(),
                    "@{u}".to_string(),
                ],
            ],
            Operation::Rebase {
                branch,
                onto,
                upstream,
                predicted_conflict: _,
            } => {
                if let Some(u) = upstream {
                    vec![vec![
                        "rebase".to_string(),
                        "--onto".to_string(),
                        onto.clone(),
                        u.clone(),
                        branch.clone(),
                    ]]
                } else {
                    vec![vec!["rebase".to_string(), onto.clone(), branch.clone()]]
                }
            }
            Operation::Push { branch } => vec![vec![
                "push".to_string(),
                "--force-with-lease".to_string(),
                "origin".to_string(),
                branch.clone(),
            ]],
            Operation::Submit { branch, target } => vec![
                vec![
                    "push".to_string(),
                    "upstream".to_string(),
                    format!("{}:{}", branch, target),
                ],
                vec![
                    "push".to_string(),
                    "origin".to_string(),
                    "--delete".to_string(),
                    branch.clone(),
                ],
                vec!["checkout".to_string(), target.clone()],
                vec!["merge".to_string(), "--ff-only".to_string(), branch.clone()],
                vec!["push".to_string(), "origin".to_string(), target.clone()],
            ],
            Operation::Sync {
                branch,
                onto,
                predicted_conflict: _,
            } => vec![
                vec!["checkout".to_string(), branch.clone()],
                vec!["merge".to_string(), "--ff-only".to_string(), onto.clone()],
                vec!["push".to_string(), "origin".to_string(), branch.clone()],
            ],
            Operation::Delete { branch } => {
                vec![vec!["branch".to_string(), "-d".to_string(), branch.clone()]]
            }
            Operation::Localize { branch } => {
                let local_name = get_local_name(branch);
                vec![vec![
                    "checkout".to_string(),
                    "-B".to_string(),
                    local_name.to_string(),
                    "--track".to_string(),
                    branch.clone(),
                ]]
            }
            Operation::Amend { message } => {
                let mut cmd = vec!["commit".to_string(), "--amend".to_string()];
                if let Some(msg) = message {
                    cmd.push("-m".to_string());
                    cmd.push(msg.clone());
                } else {
                    cmd.push("--no-edit".to_string());
                }
                cmd.push("-a".to_string());
                vec![cmd]
            }
            Operation::Rename { branch, new_name } => {
                vec![vec![
                    "branch".to_string(),
                    "-m".to_string(),
                    branch.clone(),
                    new_name.clone(),
                ]]
            }
            Operation::Split {
                branch: _,
                parent: _,
                data: _,
            } => {
                vec![vec!["[SPLIT OPERATION]".to_string()]]
            }
        }
    }
}

pub fn get_local_name(branch_name: &str) -> &str {
    branch_name
        .split_once('/')
        .map(|(_, suffix)| suffix)
        .unwrap_or(branch_name)
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Operation::Split { branch, data, .. } = self {
            let names: Vec<_> = data.parts.iter().map(|p| p.name.as_str()).collect();
            return write!(f, "git split {} (creating {})", branch, names.join(", "));
        }

        let cmds = self.commands();
        for (i, cmd) in cmds.iter().enumerate() {
            if i > 0 {
                write!(f, " && ")?;
            }
            write!(f, "git {}", cmd.join(" "))?;
        }
        Ok(())
    }
}
