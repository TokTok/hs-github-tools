use crate::patch_utils::apply_selected_hunks_to_tree;
use git2::{Diff, Oid, Repository};
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct Transaction {
    pub original_branch: String,
    pub parent_branch: String,
    pub selected_hunks: HashSet<(String, usize)>,
    /// The OID that the new "first" commit's tree will have.
    pub projected_first_tree_oid: Option<Oid>,
    /// The OID that the new "second" (remainder) commit's tree will have.
    pub projected_remainder_tree_oid: Option<Oid>,
}

impl Transaction {
    pub fn new(original_branch: String, parent_branch: String) -> Self {
        Self {
            original_branch,
            parent_branch,
            selected_hunks: HashSet::new(),
            projected_first_tree_oid: None,
            projected_remainder_tree_oid: None,
        }
    }

    pub fn calculate_projected_oids(
        &mut self,
        repo: &Repository,
        diff: &Diff,
    ) -> anyhow::Result<()> {
        let parent_obj = repo.revparse_single(&self.parent_branch)?;
        let parent_commit = parent_obj.peel_to_commit()?;
        let base_tree = parent_commit.tree()?;

        // 1. First commit tree (selected hunks)
        let first_tree_oid =
            apply_selected_hunks_to_tree(repo, &base_tree, diff, &self.selected_hunks)?;
        self.projected_first_tree_oid = Some(first_tree_oid);

        // 2. Remainder commit tree (all hunks)
        // For a 1-commit branch, the remainder tree is just the original branch's tree.
        let original_obj = repo.revparse_single(&self.original_branch)?;
        let original_commit = original_obj.peel_to_commit()?;
        self.projected_remainder_tree_oid = Some(original_commit.tree_id());

        Ok(())
    }
}
