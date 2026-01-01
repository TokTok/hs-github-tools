pub mod executor;
pub mod git;
pub mod github;
pub mod planner;
pub mod refs;
pub mod topology;
pub mod transaction;
pub mod types;

pub use executor::{ConflictMode, Executor, RebaseConflict, ShellExecutor};
pub use git::{Git, RealGit, is_better_name};
pub use github::PrInfo;
pub use planner::{calculate_plan, predict_conflicts};
pub use refs::RebaseOutcome;
pub use topology::{apply_move, build_topology, flatten_branches, is_descendant};
pub use types::*;

pub use crate::topology::{HistoryContext, Intent, VirtualTopology};
