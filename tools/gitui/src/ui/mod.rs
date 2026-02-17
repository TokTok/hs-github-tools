pub mod common;
pub mod main_view;
pub mod preview_view;
pub mod prompt_view;
pub mod split_view;

pub use main_view::draw_main;
pub use preview_view::draw_preview;
pub use prompt_view::draw_prompt;
pub use split_view::draw_split_view;

pub const SPINNERS: &[&str] = &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
