use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use std::process::Command;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PrInfo {
    pub number: u64,
    pub url: String,
    #[serde(rename = "isDraft")]
    pub is_draft: bool,
    pub state: String,
    pub title: String,
    #[serde(rename = "headRefName")]
    pub head_ref: String,
}

/// Fetch open pull requests for the repository at `path`, keyed by `headRefName`.
///
/// Empty on any failure (`gh` missing, no auth, no GitHub remote, network down):
/// a missing key means "no known PR", not an error.
pub fn fetch_open_prs<P: AsRef<Path>>(path: P) -> HashMap<String, PrInfo> {
    let output = Command::new("gh")
        .arg("pr")
        .arg("list")
        .arg("--limit")
        .arg("200")
        .arg("--state")
        .arg("open")
        .arg("--json")
        .arg("number,title,state,isDraft,headRefName,url")
        .current_dir(path.as_ref())
        .output();

    let output = match output {
        Ok(o) if o.status.success() => o,
        _ => return HashMap::new(),
    };

    let prs: Vec<PrInfo> = match serde_json::from_slice(&output.stdout) {
        Ok(v) => v,
        Err(_) => return HashMap::new(),
    };

    let mut by_head = HashMap::new();
    for pr in prs {
        by_head.entry(pr.head_ref.clone()).or_insert(pr);
    }
    by_head
}
