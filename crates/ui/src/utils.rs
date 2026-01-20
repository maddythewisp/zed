//! UI-related utilities

use gpui::App;
use theme::ActiveTheme;

mod apca_contrast;
mod color_contrast;
mod corner_solver;
mod format_distance;
mod search_input;

pub use apca_contrast::*;
pub use color_contrast::*;
pub use corner_solver::{CornerSolver, inner_corner_radius};
pub use format_distance::*;
pub use search_input::*;

/// Returns true if the current theme is light or vibrant light.
pub fn is_light(cx: &mut App) -> bool {
    cx.theme().appearance.is_light()
}
