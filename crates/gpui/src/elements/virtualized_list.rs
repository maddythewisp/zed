//! Shared infrastructure for virtualized lists (List and UniformList).
//!
//! This module provides:
//! - Common fiber-backed item rendering
//! - Shared item fiber management

use crate::{
    AnyElement, App, AvailableSpace, ContentMask, GlobalElementId, Pixels, Point, Size, Window,
    taffy::layout_bounds,
};
use collections::FxHashMap;
use std::ops::Range;

/// Manages fiber IDs for virtualized list items.
///
/// This provides stable identity for items across frames, enabling
/// the fiber tree to efficiently reconcile and cache item state.
#[derive(Debug)]
pub struct ItemFiberManager {
    /// Map from item index to fiber ID.
    item_fibers: FxHashMap<usize, GlobalElementId>,
}

impl ItemFiberManager {
    pub fn new() -> Self {
        Self {
            item_fibers: FxHashMap::default(),
        }
    }

    /// Get or create a fiber ID for an item at the given index.
    pub fn get_or_create(&mut self, index: usize, window: &mut Window) -> GlobalElementId {
        if let Some(id) = self.item_fibers.get(&index).copied() {
            if window.element_fiber_exists(&id) {
                return id;
            }
        }

        let id = window.fiber.tree.create_placeholder_fiber();
        self.item_fibers.insert(index, id);
        id
    }

    /// Update indices after a splice operation.
    /// Items in old_range are removed, and items after are shifted by delta.
    pub fn splice(&mut self, old_range: Range<usize>, new_count: usize) {
        if self.item_fibers.is_empty() {
            return;
        }

        let removed = old_range.end.saturating_sub(old_range.start);
        let delta = new_count as isize - removed as isize;

        let mut next = FxHashMap::default();
        for (index, id) in self.item_fibers.drain() {
            if index < old_range.start {
                next.insert(index, id);
            } else if index >= old_range.end {
                let shifted = (index as isize + delta) as usize;
                next.insert(shifted, id);
            }
        }
        self.item_fibers = next;
    }

    /// Clear all fiber mappings.
    pub fn clear(&mut self) {
        self.item_fibers.clear();
    }
}

impl Default for ItemFiberManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Layout result for a single item.
pub struct ItemLayout {
    /// Index of the item in the list.
    pub index: usize,
    /// Fiber ID for the item.
    pub fiber_id: GlobalElementId,
    /// Computed size of the item.
    pub size: Size<Pixels>,
}

/// Layout an item using the fiber-based pipeline.
///
/// This reconciles the element into a fiber, installs retained nodes,
/// computes layout via Taffy, and returns the computed size.
pub fn layout_item_fiber(
    fiber_id: GlobalElementId,
    element: &mut AnyElement,
    available_space: Size<AvailableSpace>,
    window: &mut Window,
    cx: &mut App,
) -> Size<Pixels> {
    // Expand wrapper elements BEFORE reconciliation.
    element.expand_wrappers(window, cx);

    // Reconcile the element into the fiber tree
    window.fiber.tree.reconcile(&fiber_id, element, true);

    // Install retained nodes
    window
        .fibers()
        .cache_fiber_payloads_overlay(&fiber_id, element, cx);

    window.prepare_detached_root_for_layout(fiber_id, cx);

    // Compute layout
    window.compute_layout_for_fiber(fiber_id, available_space, cx);

    // Get the computed size
    let scale_factor = window.scale_factor();
    let mut cache = FxHashMap::default();
    let bounds = layout_bounds(window, &fiber_id, scale_factor, &mut cache);
    bounds.size
}

/// Recursively clear cached prepaint/paint state for a fiber and all its descendants.
///
/// This is necessary for item fibers that exist outside the normal frame lifecycle.
/// They persist via ItemFiberManager but the line layout cache is swapped each frame,
/// so their cached state references line layouts that no longer exist.
fn clear_cached_state_recursive(fiber_id: GlobalElementId, window: &mut Window) {
    // Collect children first to avoid borrow issues
    let children: Vec<GlobalElementId> = window.fiber.tree.children(&fiber_id).collect();

    // Clear this fiber's cached state
    if let Some(cache) = window.fiber.tree.paint_cache.get_mut(fiber_id.into()) {
        cache.prepaint_state = None;
        cache.paint_list = None;
    }

    // Recursively clear children
    for child_id in children {
        clear_cached_state_recursive(child_id, window);
    }
}

/// Prepaint an item at the given origin with content mask.
///
/// This clears any cached prepaint state first to prevent stale cache replay,
/// since item fibers exist outside the normal frame lifecycle (they persist
/// via ItemFiberManager but the line layout cache is swapped each frame).
pub fn prepaint_item_fiber(
    fiber_id: GlobalElementId,
    origin: Point<Pixels>,
    content_mask: ContentMask<Pixels>,
    window: &mut Window,
    cx: &mut App,
) {
    // Clear cached prepaint/paint state for the entire subtree to prevent replay
    // of stale line layout indices.
    clear_cached_state_recursive(fiber_id, window);

    let mut prepaint_cx = crate::window::context::PrepaintCx::new(window);
    prepaint_cx.with_content_mask(Some(content_mask), |window| {
        let mut prepaint_cx = crate::window::context::PrepaintCx::new(window);
        prepaint_cx.with_absolute_element_offset(origin, |window| {
            window
                .fibers()
                .prepaint_fiber_tree_internal(fiber_id, cx, true);
        });
    });
}

/// Paint multiple item fibers with a shared content mask.
pub fn paint_item_fibers(
    items: &[ItemLayout],
    content_mask: ContentMask<Pixels>,
    window: &mut Window,
    cx: &mut App,
) {
    let mut paint_cx = crate::window::context::PaintCx::new(window);
    paint_cx.with_content_mask(Some(content_mask), |window| {
        for item in items {
            window
                .fibers()
                .paint_fiber_tree_internal(item.fiber_id, cx, false);
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_item_fiber_manager_splice() {
        use crate::FiberTree;

        // Create a real FiberTree to get valid fiber IDs
        let mut tree = FiberTree::new();
        let fiber_0 = tree.create_placeholder_fiber();
        let fiber_1 = tree.create_placeholder_fiber();
        let fiber_2 = tree.create_placeholder_fiber();
        let fiber_3 = tree.create_placeholder_fiber();
        let fiber_4 = tree.create_placeholder_fiber();

        let mut manager = ItemFiberManager::new();
        manager.item_fibers.insert(0, fiber_0);
        manager.item_fibers.insert(1, fiber_1);
        manager.item_fibers.insert(2, fiber_2);
        manager.item_fibers.insert(3, fiber_3);
        manager.item_fibers.insert(4, fiber_4);

        // splice(1..3, 1) means:
        // - Remove items at indices 1 and 2 (2 items)
        // - Insert 1 new item in their place
        // - delta = 1 - 2 = -1
        // - Items at index >= 3 shift by -1
        //
        // Before: [0, 1, 2, 3, 4]
        // After:  [0, (new), 3->2, 4->3]
        manager.splice(1..3, 1);

        // Index 0 unchanged
        assert!(manager.item_fibers.contains_key(&0));
        assert_eq!(manager.item_fibers.get(&0), Some(&fiber_0));

        // Index 1 was in removed range - fiber gone, new item not tracked
        assert!(!manager.item_fibers.contains_key(&1));

        // Index 2 now has fiber that was at index 3
        assert!(manager.item_fibers.contains_key(&2));
        assert_eq!(manager.item_fibers.get(&2), Some(&fiber_3));

        // Index 3 now has fiber that was at index 4
        assert!(manager.item_fibers.contains_key(&3));
        assert_eq!(manager.item_fibers.get(&3), Some(&fiber_4));

        // Index 4 no longer exists (shifted to 3)
        assert!(!manager.item_fibers.contains_key(&4));
    }
}
