pub mod executor;
pub mod git;
pub mod planner;
pub mod topology;
pub mod transaction;
pub mod types;

pub use executor::{Executor, ShellExecutor};
pub use git::{Git, RealGit, is_better_name};
pub use planner::{calculate_plan, predict_conflicts};
pub use topology::{apply_move, build_topology, flatten_branches, is_descendant};
pub use types::*;

// Re-exports from other modules for convenience
pub use crate::topology::{HistoryContext, Intent, VirtualTopology};
