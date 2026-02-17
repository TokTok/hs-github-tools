use git2::Oid;
use petgraph::Direction;
use petgraph::graph::NodeIndex;
use petgraph::stable_graph::StableGraph;
use petgraph::visit::EdgeRef;
use std::collections::{HashMap, HashSet};

pub mod virtual_layer;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TopologyNode {
    /// A branch pointer.
    Branch { name: String, oid: Oid },
    /// A raw commit in history that is NOT a branch head we are tracking.
    Commit { oid: Oid },
}

impl TopologyNode {
    pub fn oid(&self) -> Oid {
        match self {
            TopologyNode::Branch { oid, .. } => *oid,
            TopologyNode::Commit { oid } => *oid,
        }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            TopologyNode::Branch { name, .. } => Some(name),
            TopologyNode::Commit { .. } => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Intent {
    /// User explicitly moved this branch to a new stack.
    Structural,
    /// User is syncing/resetting the branch, but it should stay in its current visual stack.
    Synchronizing,
    /// A child following its parent.
    Implicit,
    /// Parent inferred from heuristics (e.g. commit message).
    Heuristic,
}

#[derive(Debug, Clone, Default)]
pub struct HistoryContext {
    /// Maps an OID to its nearest visible ancestor OID.
    pub oid_to_ancestor: HashMap<Oid, Option<Oid>>,
    /// Maps an OID to the "best" visible branch name at that commit.
    pub oid_to_visible_branch: HashMap<Oid, String>,
}

impl HistoryContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Resolves a parent for a given OID by walking up the compressed history
    /// until it finds a visible branch name that is NOT the excluded name.
    pub fn resolve_visible_parent(&self, oid: Oid, exclude_name: Option<&str>) -> Option<String> {
        let mut current_oid = oid;
        let mut visited = HashSet::new();
        while visited.insert(current_oid) {
            if let Some(name) = self.oid_to_visible_branch.get(&current_oid)
                && Some(name.as_str()) != exclude_name
            {
                return Some(name.clone());
            }
            match self.oid_to_ancestor.get(&current_oid) {
                Some(Some(parent_oid)) => current_oid = *parent_oid,
                _ => break,
            }
        }
        None
    }
}

pub struct VirtualTopology {
    pub graph: StableGraph<TopologyNode, Intent>,
    /// Maps branch names to their unique node in the graph.
    pub branches: HashMap<String, NodeIndex>,
    /// Maps OIDs to their "canonical" commit node (if it's not a branch).
    commits: HashMap<Oid, NodeIndex>,
    /// Visual memory: stores the last seen order of branches for stable rendering.
    visual_memory: Vec<String>,
}

impl VirtualTopology {
    pub fn new() -> Self {
        Self {
            graph: StableGraph::new(),
            branches: HashMap::new(),
            commits: HashMap::new(),
            visual_memory: Vec::new(),
        }
    }

    pub fn set_visual_memory(&mut self, order: Vec<String>) {
        self.visual_memory = order;
    }

    pub fn add_branch(&mut self, name: &str, oid: Oid) -> NodeIndex {
        if let Some(&idx) = self.branches.get(name) {
            if let TopologyNode::Branch { oid: old_oid, .. } = &mut self.graph[idx] {
                *old_oid = oid;
            }
            return idx;
        }
        let idx = self.graph.add_node(TopologyNode::Branch {
            name: name.to_string(),
            oid,
        });
        self.branches.insert(name.to_string(), idx);
        idx
    }

    pub fn add_commit(&mut self, oid: Oid) -> NodeIndex {
        if let Some(&idx) = self.commits.get(&oid) {
            return idx;
        }
        let idx = self.graph.add_node(TopologyNode::Commit { oid });
        self.commits.insert(oid, idx);
        idx
    }

    pub fn add_edge(&mut self, child_oid: Oid, parent_oid: Oid) {
        let child_idx = self.add_commit(child_oid);
        let parent_idx = self.add_commit(parent_oid);
        self.graph
            .update_edge(child_idx, parent_idx, Intent::Implicit);
    }

    pub fn add_commit_parent(&mut self, oid: Oid, parent_name: &str) -> anyhow::Result<()> {
        let child_idx = self.add_commit(oid);
        let parent_idx = self
            .branches
            .get(parent_name)
            .copied()
            .ok_or_else(|| anyhow::anyhow!("Branch {} not found", parent_name))?;

        if petgraph::algo::has_path_connecting(&self.graph, parent_idx, child_idx, None) {
            anyhow::bail!("Cycle detected");
        }

        self.graph
            .update_edge(child_idx, parent_idx, Intent::Implicit);
        Ok(())
    }

    pub fn set_parent(
        &mut self,
        branch_name: &str,
        parent_name: Option<&str>,
        parent_oid: Option<Oid>,
        intent: Intent,
    ) -> anyhow::Result<()> {
        let branch_idx = self
            .branches
            .get(branch_name)
            .copied()
            .ok_or_else(|| anyhow::anyhow!("Branch {} not found", branch_name))?;

        let parent_idx = if let Some(p_name) = parent_name {
            self.branches
                .get(p_name)
                .copied()
                .ok_or_else(|| anyhow::anyhow!("Parent branch {} not found", p_name))?
        } else if let Some(p_oid) = parent_oid {
            self.add_commit(p_oid)
        } else {
            let edges: Vec<_> = self
                .graph
                .edges_directed(branch_idx, Direction::Outgoing)
                .map(|e| e.id())
                .collect();
            for e in edges {
                self.graph.remove_edge(e);
            }
            return Ok(());
        };

        if branch_idx == parent_idx {
            return Ok(());
        }

        if petgraph::algo::has_path_connecting(&self.graph, parent_idx, branch_idx, None) {
            anyhow::bail!(
                "Cannot set parent of {} to {} as it would create a cycle",
                branch_name,
                parent_name.unwrap_or("commit")
            );
        }

        let edges: Vec<_> = self
            .graph
            .edges_directed(branch_idx, Direction::Outgoing)
            .map(|e| e.id())
            .collect();
        for e in edges {
            self.graph.remove_edge(e);
        }

        self.graph.add_edge(branch_idx, parent_idx, intent);
        Ok(())
    }

    pub fn get_parents(&self, branch_name: &str) -> Vec<TopologyNode> {
        if let Some(&idx) = self.branches.get(branch_name) {
            self.graph
                .neighbors_directed(idx, Direction::Outgoing)
                .map(|idx| self.graph[idx].clone())
                .collect()
        } else {
            Vec::new()
        }
    }

    pub fn get_children(&self, branch_name: &str) -> Vec<String> {
        if let Some(&idx) = self.branches.get(branch_name) {
            let mut children: Vec<_> = self
                .graph
                .neighbors_directed(idx, Direction::Incoming)
                .filter_map(|idx| {
                    if let TopologyNode::Branch { name, .. } = &self.graph[idx] {
                        Some(name.clone())
                    } else {
                        None
                    }
                })
                .collect();
            children.sort();
            children
        } else {
            Vec::new()
        }
    }

    pub fn get_branch_oid(&self, name: &str) -> Option<Oid> {
        self.branches.get(name).map(|&idx| self.graph[idx].oid())
    }

    pub fn remove_branch(&mut self, name: &str) {
        if let Some(idx) = self.branches.remove(name) {
            self.graph.remove_node(idx);
        }
    }

    pub fn list_roots(&self) -> Vec<String> {
        let mut roots = Vec::new();
        for &idx in self.branches.values() {
            if self
                .graph
                .neighbors_directed(idx, Direction::Outgoing)
                .next()
                .is_none()
                && let TopologyNode::Branch { name, .. } = &self.graph[idx]
            {
                roots.push(name.clone());
            }
        }
        roots.sort();
        roots
    }

    pub fn list_all_branches(&self) -> Vec<String> {
        let mut names: Vec<_> = self.branches.keys().cloned().collect();
        names.sort();
        names
    }

    pub fn flatten(&self) -> Vec<(String, usize)> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();

        // 1. Identify "Effective Roots" for the TUI.
        // If a branch has a parent via Synchronizing intent, we might want to treat it as a child
        // of its original visual parent even if the graph says it's now parented to an alias.
        // But the VirtualTopology already handles alias resolution in build_topology.

        // We use the graph as source of truth for relationships, but use visual_memory for tie-breaking.

        let mut roots: Vec<_> = self
            .graph
            .node_indices()
            .filter(|&idx| {
                self.graph
                    .neighbors_directed(idx, Direction::Outgoing)
                    .next()
                    .is_none()
            })
            .collect();

        // Sort roots using visual_memory then name/oid
        roots.sort_by(|&a, &b| self.compare_nodes(a, b));

        for root_idx in roots {
            self.flatten_recursive(root_idx, 0, &mut result, &mut visited);
        }

        result
    }

    fn flatten_recursive(
        &self,
        node_idx: NodeIndex,
        depth: usize,
        result: &mut Vec<(String, usize)>,
        visited: &mut HashSet<NodeIndex>,
    ) {
        if visited.contains(&node_idx) {
            return;
        }
        visited.insert(node_idx);

        let mut next_depth = depth;
        if let TopologyNode::Branch { name, .. } = &self.graph[node_idx] {
            result.push((name.clone(), depth));
            next_depth = depth + 1;
        }

        let mut children: Vec<_> = self
            .graph
            .neighbors_directed(node_idx, Direction::Incoming)
            .collect();
        children.sort_by(|&a, &b| self.compare_nodes(a, b));

        for child_idx in children {
            self.flatten_recursive(child_idx, next_depth, result, visited);
        }
    }

    fn compare_nodes(&self, a: NodeIndex, b: NodeIndex) -> std::cmp::Ordering {
        let node_a = &self.graph[a];
        let node_b = &self.graph[b];

        let name_a = node_a.name();
        let name_b = node_b.name();

        match (name_a, name_b) {
            (Some(na), Some(nb)) => {
                // Primary: alphabetical sorting
                let res = na.cmp(nb);
                if res != std::cmp::Ordering::Equal {
                    return res;
                }

                // Tie-breaker: Use visual memory if available
                let pos_a = self.visual_memory.iter().position(|n| n == na);
                let pos_b = self.visual_memory.iter().position(|n| n == nb);

                match (pos_a, pos_b) {
                    (Some(pa), Some(pb)) => pa.cmp(&pb),
                    (Some(_), None) => std::cmp::Ordering::Less,
                    (None, Some(_)) => std::cmp::Ordering::Greater,
                    (None, None) => std::cmp::Ordering::Equal,
                }
            }
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => node_a.oid().to_string().cmp(&node_b.oid().to_string()),
        }
    }
}

impl Default for VirtualTopology {
    fn default() -> Self {
        Self::new()
    }
}
