use crate::topology::{Intent, VirtualTopology};
use git2::Oid;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct VirtualBranch {
    pub name: String,
    pub oid: Option<Oid>,
    pub parent: Option<String>,
}

#[derive(Default)]
pub struct VirtualLayer {
    /// Branches that are entirely virtual (don't exist in git).
    pub virtual_branches: HashMap<String, VirtualBranch>,
    /// Real branches that should be hidden from the UI (e.g. because they are being replaced).
    pub hidden_branches: Vec<String>,
}

impl VirtualLayer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_virtual_branch(&mut self, name: String, parent: Option<String>, oid: Option<Oid>) {
        self.virtual_branches
            .insert(name.clone(), VirtualBranch { name, oid, parent });
    }

    pub fn hide_branch(&mut self, name: &str) {
        if !self.hidden_branches.contains(&name.to_string()) {
            self.hidden_branches.push(name.to_string());
        }
    }

    pub fn is_hidden(&self, name: &str) -> bool {
        self.hidden_branches.contains(&name.to_string())
    }

    pub fn is_virtual(&self, name: &str) -> bool {
        self.virtual_branches.contains_key(name)
    }

    pub fn apply(&self, topo: &mut VirtualTopology) {
        for name in &self.hidden_branches {
            if !self.virtual_branches.contains_key(name) {
                topo.remove_branch(name);
            }
        }
        // First pass: add all branches
        for (name, vb) in &self.virtual_branches {
            let oid = vb.oid.unwrap_or(Oid::from_bytes(&[0; 20]).unwrap());
            topo.add_branch(name, oid);
        }
        // Second pass: set parents
        for (name, vb) in &self.virtual_branches {
            if let Some(parent) = &vb.parent {
                let _ = topo.set_parent(name, Some(parent), None, Intent::Implicit);
            }
        }
    }
}
