//! A list element that can be used to render a large number of differently sized elements
//! efficiently. Clients of this API need to ensure that elements outside of the scrolled
//! area do not change their height for this element to function correctly. If your elements
//! do change height, notify the list element via [`ListState::splice`] or [`ListState::reset`].
//! In order to minimize re-renders, this element's state is stored intrusively
//! on your own views, so that your code can coordinate directly with the list element's cached state.
//!
//! If all of your elements are the same height, see [`crate::UniformList`] for a simpler API

use gpui_macros::Element;

use super::virtualized_list::{
    ItemFiberManager, ItemLayout, layout_item_fiber, paint_item_fibers, prepaint_item_fiber,
};
use crate::render_node::{
    CallbackSlot, LayoutCtx, LayoutFrame, PaintCtx, PaintFrame, PrepaintCtx, PrepaintFrame,
    RenderNode, UpdateResult,
};
use crate::taffy::ToTaffy;
use crate::{
    AnyElement, App, AvailableSpace, Bounds, ContentMask, DispatchPhase, Display, Edges, EntityId,
    ElementImpl, FocusHandle, Hitbox, HitboxBehavior, IntoElement, Overflow,
    Pixels, Point, ScrollDelta, ScrollWheelEvent, Size, Style, StyleRefinement, Styled,
    Window, point, px, size,
};
use collections::VecDeque;
use refineable::Refineable as _;
use std::{any::TypeId, cell::RefCell, ops::Range, rc::Rc};
use sum_tree::{Bias, Dimensions, SumTree};

type RenderItemFn = dyn FnMut(usize, &mut Window, &mut App) -> AnyElement + 'static;

/// Construct a new list element
pub fn list(
    state: ListState,
    render_item: impl FnMut(usize, &mut Window, &mut App) -> AnyElement + 'static,
) -> List {
    List {
        state,
        render_item: Box::new(render_item),
        style: StyleRefinement::default(),
        sizing_behavior: ListSizingBehavior::default(),
    }
}

/// A list element
#[derive(Element)]
#[element(crate = crate)]
pub struct List {
    state: ListState,
    render_item: Box<RenderItemFn>,
    style: StyleRefinement,
    sizing_behavior: ListSizingBehavior,
}

impl List {
    /// Set the sizing behavior for the list.
    pub fn with_sizing_behavior(mut self, behavior: ListSizingBehavior) -> Self {
        self.sizing_behavior = behavior;
        self
    }
}

/// The list state that views must hold on behalf of the list element.
#[derive(Clone)]
pub struct ListState(Rc<RefCell<StateInner>>);

impl std::fmt::Debug for ListState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ListState")
    }
}

struct StateInner {
    last_layout_bounds: Option<Bounds<Pixels>>,
    last_padding: Option<Edges<Pixels>>,
    items: SumTree<ListItem>,
    item_fibers: ItemFiberManager,
    logical_scroll_top: Option<ListOffset>,
    alignment: ListAlignment,
    overdraw: Pixels,
    reset: bool,
    #[allow(clippy::type_complexity)]
    scroll_handler: Option<Box<dyn FnMut(&ListScrollEvent, &mut Window, &mut App)>>,
    scrollbar_drag_start_height: Option<Pixels>,
    measuring_behavior: ListMeasuringBehavior,
    pending_scroll: Option<PendingScrollFraction>,
}

/// Keeps track of a fractional scroll position within an item for restoration
/// after remeasurement.
struct PendingScrollFraction {
    /// The index of the item to scroll within.
    item_ix: usize,
    /// Fractional offset (0.0 to 1.0) within the item's height.
    fraction: f32,
}

/// Whether the list is scrolling from top to bottom or bottom to top.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ListAlignment {
    /// The list is scrolling from top to bottom, like most lists.
    Top,
    /// The list is scrolling from bottom to top, like a chat log.
    Bottom,
}

/// A scroll event that has been converted to be in terms of the list's items.
pub struct ListScrollEvent {
    /// The range of items currently visible in the list, after applying the scroll event.
    pub visible_range: Range<usize>,

    /// The number of items that are currently visible in the list, after applying the scroll event.
    pub count: usize,

    /// Whether the list has been scrolled.
    pub is_scrolled: bool,
}

/// The sizing behavior to apply during layout.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ListSizingBehavior {
    /// The list should calculate its size based on the size of its items.
    Infer,
    /// The list should not calculate a fixed size.
    #[default]
    Auto,
}

/// The measuring behavior to apply during layout.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ListMeasuringBehavior {
    /// Measure all items in the list.
    /// Note: This can be expensive for the first frame in a large list.
    Measure(bool),
    /// Only measure visible items
    #[default]
    Visible,
}

impl ListMeasuringBehavior {
    fn reset(&mut self) {
        match self {
            ListMeasuringBehavior::Measure(has_measured) => *has_measured = false,
            ListMeasuringBehavior::Visible => {}
        }
    }
}

/// The horizontal sizing behavior to apply during layout.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ListHorizontalSizingBehavior {
    /// List items' width can never exceed the width of the list.
    #[default]
    FitList,
    /// List items' width may go over the width of the list, if any item is wider.
    Unconstrained,
}

struct LayoutItemsResponse {
    _max_item_width: Pixels,
    scroll_top: ListOffset,
    item_layouts: VecDeque<ItemLayout>,
}

/// Retained render node for List elements.
///
/// ListNode owns all List-specific state and implements the scope-based
/// prepaint/paint lifecycle. The render callback is deposited each frame
/// via CallbackSlot.
pub(crate) struct ListNode {
    /// The shared list state (also owned by user code).
    pub state: ListState,
    /// Render callback deposited by the element each frame.
    pub render_item: CallbackSlot<RenderItemFn>,
    /// Styling configuration.
    pub style: StyleRefinement,
    /// Sizing behavior.
    pub sizing_behavior: ListSizingBehavior,
    /// Cached item layouts for paint phase.
    cached_item_layouts: VecDeque<ItemLayout>,
    /// Cached content mask for paint phase.
    cached_content_mask: Option<ContentMask<Pixels>>,
    /// Cached scroll top for paint phase.
    cached_scroll_top: ListOffset,
    /// Cached hitbox for paint phase.
    cached_hitbox: Option<Hitbox>,
}

impl ListNode {
    /// Create a new ListNode.
    pub fn new(
        state: ListState,
        style: StyleRefinement,
        sizing_behavior: ListSizingBehavior,
    ) -> Self {
        Self {
            state,
            render_item: CallbackSlot::new(),
            style,
            sizing_behavior,
            cached_item_layouts: VecDeque::new(),
            cached_content_mask: None,
            cached_scroll_top: ListOffset::default(),
            cached_hitbox: None,
        }
    }
}

impl RenderNode for ListNode {
    fn taffy_style(&self, rem_size: Pixels, scale_factor: f32) -> taffy::style::Style {
        let mut style = Style::default();
        style.overflow.y = Overflow::Scroll;
        style.refine(&self.style);
        style.to_taffy(rem_size, scale_factor)
    }

    fn compute_intrinsic_size(
        &mut self,
        _ctx: &mut crate::SizingCtx,
    ) -> crate::IntrinsicSizeResult {
        crate::IntrinsicSizeResult {
            size: crate::IntrinsicSize::default(),
            input: crate::SizingInput::default(),
        }
    }

    fn layout_begin(&mut self, ctx: &mut LayoutCtx) -> LayoutFrame {
        let mut frame = LayoutFrame {
            handled: true,
            ..Default::default()
        };

        // Push text style refinement for child text measurement
        let mut style = Style::default();
        style.refine(&self.style);
        if let Some(text_style) = style.text_style() {
            ctx.window.text_style_stack.push(text_style.clone());
            frame.pushed_text_style = true;
        }

        frame
    }

    fn layout_end(&mut self, ctx: &mut LayoutCtx, frame: LayoutFrame) {
        if frame.pushed_text_style {
            ctx.window.text_style_stack.pop();
        }
    }

    fn prepaint_begin(&mut self, ctx: &mut PrepaintCtx) -> PrepaintFrame {
        use crate::window::context::PrepaintCx;

        let mut frame = PrepaintFrame {
            handled: true,
            skip_children: true, // We manage items ourselves
            ..Default::default()
        };

        // Clear cached state from previous frame
        self.cached_item_layouts.clear();
        self.cached_content_mask = None;
        self.cached_scroll_top = ListOffset::default();
        self.cached_hitbox = None;

        // Compute style
        let mut style = Style::default();
        style.overflow.y = Overflow::Scroll;
        style.refine(&self.style);

        // Skip if display: none
        if style.display == Display::None {
            return frame;
        }

        // Create hitbox
        let hitbox =
            ctx.window
                .insert_hitbox_with_fiber(ctx.bounds, HitboxBehavior::Normal, ctx.fiber_id);
        frame.hitbox = Some(hitbox.clone());
        self.cached_hitbox = Some(hitbox);

        // Push text style
        if let Some(text_style) = style.text_style() {
            ctx.window.text_style_stack.push(text_style.clone());
            frame.pushed_text_style = true;
        }

        // Push content mask for scrolling
        if let Some(mask) = style.overflow_mask(ctx.bounds, ctx.window.rem_size()) {
            let world_mask = ctx.window.transform_mask_to_world(mask);
            let intersected = world_mask.intersect(&PrepaintCx::new(ctx.window).content_mask());
            ctx.window.content_mask_stack.push(intersected);
            frame.pushed_content_mask = true;
        }

        // Borrow state and perform prepaint
        let state = &mut *self.state.0.borrow_mut();
        state.reset = false;

        // If the width of the list has changed, invalidate all cached item heights
        if state
            .last_layout_bounds
            .is_none_or(|last_bounds| last_bounds.size.width != ctx.bounds.size.width)
        {
            let new_items = SumTree::from_iter(
                state.items.iter().map(|item| ListItem::Unmeasured {
                    focus_handle: item.focus_handle(),
                }),
                (),
            );
            state.items = new_items;
        }

        let padding = style
            .padding
            .to_pixels(ctx.bounds.size.into(), ctx.window.rem_size());

        // Run the prepaint logic with the render callback
        let layout = self.render_item.with_mut(|render_item| {
            match state.prepaint_items(ctx.bounds, padding, true, render_item, ctx.window, ctx.cx) {
                Ok(layout) => layout,
                Err(autoscroll_request) => {
                    state.logical_scroll_top = Some(autoscroll_request);
                    state
                        .prepaint_items(ctx.bounds, padding, false, render_item, ctx.window, ctx.cx)
                        .unwrap()
                }
            }
        });

        if let Some(layout) = layout {
            state.last_layout_bounds = Some(ctx.bounds);
            state.last_padding = Some(padding);
            self.cached_scroll_top = layout.scroll_top;
            self.cached_item_layouts = layout.item_layouts;
            self.cached_content_mask = Some(ContentMask { bounds: ctx.bounds });
        }

        frame
    }

    fn prepaint_end(&mut self, ctx: &mut PrepaintCtx, frame: PrepaintFrame) {
        if frame.pushed_content_mask {
            ctx.window.content_mask_stack.pop();
        }
        if frame.pushed_text_style {
            ctx.window.text_style_stack.pop();
        }
    }

    fn paint_begin(&mut self, ctx: &mut PaintCtx) -> PaintFrame {
        let mut frame = PaintFrame {
            handled: true,
            skip_children: true, // We manage items ourselves
            ..Default::default()
        };

        // Paint items
        if let Some(content_mask) = &self.cached_content_mask {
            let items = self.cached_item_layouts.make_contiguous();
            paint_item_fibers(items, content_mask.clone(), ctx.window, ctx.cx);
        }

        // Register scroll handler using cached hitbox from prepaint
        if let Some(hitbox) = &self.cached_hitbox {
            let list_state = self.state.clone();
            let height = ctx.bounds.size.height;
            let scroll_top = self.cached_scroll_top;
            let current_view = ctx.window.current_view();
            let hitbox_id = hitbox.id;

            let mut accumulated_scroll_delta = ScrollDelta::default();
            #[allow(deprecated)] // List uses on_mouse_event for scroll coalescing
            ctx.window
                .on_mouse_event(move |event: &ScrollWheelEvent, phase, window, cx| {
                    if phase == DispatchPhase::Bubble
                        && window.hitbox_should_handle_scroll(hitbox_id)
                    {
                        accumulated_scroll_delta = accumulated_scroll_delta.coalesce(event.delta);
                        let pixel_delta = accumulated_scroll_delta.pixel_delta(px(20.));
                        list_state.0.borrow_mut().scroll(
                            &scroll_top,
                            height,
                            pixel_delta,
                            current_view,
                            window,
                            cx,
                        );
                    }
                });
        }

        frame
    }

    fn paint_end(&mut self, _ctx: &mut PaintCtx, _frame: PaintFrame) {
        // No stacks to pop
    }
}

#[derive(Clone)]
enum ListItem {
    Unmeasured {
        focus_handle: Option<FocusHandle>,
    },
    Measured {
        size: Size<Pixels>,
        focus_handle: Option<FocusHandle>,
    },
}

impl ListItem {
    fn size(&self) -> Option<Size<Pixels>> {
        if let ListItem::Measured { size, .. } = self {
            Some(*size)
        } else {
            None
        }
    }

    fn focus_handle(&self) -> Option<FocusHandle> {
        match self {
            ListItem::Unmeasured { focus_handle } | ListItem::Measured { focus_handle, .. } => {
                focus_handle.clone()
            }
        }
    }

    fn contains_focused(&self, window: &Window, cx: &App) -> bool {
        match self {
            ListItem::Unmeasured { focus_handle } | ListItem::Measured { focus_handle, .. } => {
                focus_handle
                    .as_ref()
                    .is_some_and(|handle| handle.contains_focused(window, cx))
            }
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ListItemSummary {
    count: usize,
    rendered_count: usize,
    unrendered_count: usize,
    height: Pixels,
    has_focus_handles: bool,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
struct Count(usize);

#[derive(Clone, Debug, Default)]
struct Height(Pixels);

impl ListState {
    /// Construct a new list state, for storage on a view.
    ///
    /// The overdraw parameter controls how much extra space is rendered
    /// above and below the visible area. Elements within this area will
    /// be measured even though they are not visible. This can help ensure
    /// that the list doesn't flicker or pop in when scrolling.
    pub fn new(item_count: usize, alignment: ListAlignment, overdraw: Pixels) -> Self {
        let this = Self(Rc::new(RefCell::new(StateInner {
            last_layout_bounds: None,
            last_padding: None,
            items: SumTree::default(),
            item_fibers: ItemFiberManager::new(),
            logical_scroll_top: None,
            alignment,
            overdraw,
            scroll_handler: None,
            reset: false,
            scrollbar_drag_start_height: None,
            measuring_behavior: ListMeasuringBehavior::default(),
            pending_scroll: None,
        })));
        this.splice(0..0, item_count);
        this
    }

    /// Set the list to measure all items in the list in the first layout phase.
    ///
    /// This is useful for ensuring that the scrollbar size is correct instead of based on only rendered elements.
    pub fn measure_all(self) -> Self {
        self.0.borrow_mut().measuring_behavior = ListMeasuringBehavior::Measure(false);
        self
    }

    /// Reset this instantiation of the list state.
    ///
    /// Note that this will cause scroll events to be dropped until the next paint.
    pub fn reset(&self, element_count: usize) {
        let old_count = {
            let state = &mut *self.0.borrow_mut();
            state.reset = true;
            state.measuring_behavior.reset();
            state.logical_scroll_top = None;
            state.scrollbar_drag_start_height = None;
            state.item_fibers.clear();
            state.items.summary().count
        };

        self.splice(0..old_count, element_count);
    }

    /// Remeasure all items while preserving proportional scroll position.
    ///
    /// Use this when item heights may have changed (e.g., font size changes)
    /// but the number and identity of items remains the same.
    pub fn remeasure(&self) {
        let state = &mut *self.0.borrow_mut();

        let new_items = state.items.iter().map(|item| ListItem::Unmeasured {
            focus_handle: item.focus_handle(),
        });

        // If there's a `logical_scroll_top`, we need to keep track of it as a
        // `PendingScrollFraction`, so we can later preserve that scroll
        // position proportionally to the item, in case the item's height
        // changes.
        if let Some(scroll_top) = state.logical_scroll_top {
            let mut cursor = state.items.cursor::<Count>(());
            cursor.seek(&Count(scroll_top.item_ix), Bias::Right);

            if let Some(item) = cursor.item() {
                if let Some(size) = item.size() {
                    let fraction = if size.height.0 > 0.0 {
                        (scroll_top.offset_in_item.0 / size.height.0).clamp(0.0, 1.0)
                    } else {
                        0.0
                    };

                    state.pending_scroll = Some(PendingScrollFraction {
                        item_ix: scroll_top.item_ix,
                        fraction,
                    });
                }
            }
        }

        state.items = SumTree::from_iter(new_items, ());
        state.measuring_behavior.reset();
    }

    /// The number of items in this list.
    pub fn item_count(&self) -> usize {
        self.0.borrow().items.summary().count
    }

    /// Inform the list state that the items in `old_range` have been replaced
    /// by `count` new items that must be recalculated.
    pub fn splice(&self, old_range: Range<usize>, count: usize) {
        self.splice_focusable(old_range, (0..count).map(|_| None))
    }

    /// Register with the list state that the items in `old_range` have been replaced
    /// by new items. As opposed to [`Self::splice`], this method allows an iterator of optional focus handles
    /// to be supplied to properly integrate with items in the list that can be focused. If a focused item
    /// is scrolled out of view, the list will continue to render it to allow keyboard interaction.
    pub fn splice_focusable(
        &self,
        old_range: Range<usize>,
        focus_handles: impl IntoIterator<Item = Option<FocusHandle>>,
    ) {
        let state = &mut *self.0.borrow_mut();

        let mut old_items = state.items.cursor::<Count>(());
        let mut new_items = old_items.slice(&Count(old_range.start), Bias::Right);
        old_items.seek_forward(&Count(old_range.end), Bias::Right);

        let mut spliced_count = 0;
        new_items.extend(
            focus_handles.into_iter().map(|focus_handle| {
                spliced_count += 1;
                ListItem::Unmeasured { focus_handle }
            }),
            (),
        );
        new_items.append(old_items.suffix(), ());
        drop(old_items);
        state.items = new_items;

        if let Some(ListOffset {
            item_ix,
            offset_in_item,
        }) = state.logical_scroll_top.as_mut()
        {
            if old_range.contains(item_ix) {
                *item_ix = old_range.start;
                *offset_in_item = px(0.);
            } else if old_range.end <= *item_ix {
                *item_ix = *item_ix - (old_range.end - old_range.start) + spliced_count;
            }
        }

        // Update fiber ID mappings after splice
        state.item_fibers.splice(old_range, spliced_count);
    }

    /// Set a handler that will be called when the list is scrolled.
    pub fn set_scroll_handler(
        &self,
        handler: impl FnMut(&ListScrollEvent, &mut Window, &mut App) + 'static,
    ) {
        self.0.borrow_mut().scroll_handler = Some(Box::new(handler))
    }

    /// Get the current scroll offset, in terms of the list's items.
    pub fn logical_scroll_top(&self) -> ListOffset {
        self.0.borrow().logical_scroll_top()
    }

    /// Scroll the list by the given offset
    pub fn scroll_by(&self, distance: Pixels) {
        if distance == px(0.) {
            return;
        }

        let current_offset = self.logical_scroll_top();
        let state = &mut *self.0.borrow_mut();
        let mut cursor = state.items.cursor::<ListItemSummary>(());
        cursor.seek(&Count(current_offset.item_ix), Bias::Right);

        let start_pixel_offset = cursor.start().height + current_offset.offset_in_item;
        let new_pixel_offset = (start_pixel_offset + distance).max(px(0.));
        if new_pixel_offset > start_pixel_offset {
            cursor.seek_forward(&Height(new_pixel_offset), Bias::Right);
        } else {
            cursor.seek(&Height(new_pixel_offset), Bias::Right);
        }

        state.logical_scroll_top = Some(ListOffset {
            item_ix: cursor.start().count,
            offset_in_item: new_pixel_offset - cursor.start().height,
        });
    }

    /// Scroll the list to the given offset
    pub fn scroll_to(&self, mut scroll_top: ListOffset) {
        let state = &mut *self.0.borrow_mut();
        let item_count = state.items.summary().count;
        if scroll_top.item_ix >= item_count {
            scroll_top.item_ix = item_count;
            scroll_top.offset_in_item = px(0.);
        }

        state.logical_scroll_top = Some(scroll_top);
    }

    /// Scroll the list to the given item, such that the item is fully visible.
    pub fn scroll_to_reveal_item(&self, ix: usize) {
        let state = &mut *self.0.borrow_mut();

        let mut scroll_top = state.logical_scroll_top();
        let height = state
            .last_layout_bounds
            .map_or(px(0.), |bounds| bounds.size.height);
        let padding = state.last_padding.unwrap_or_default();

        if ix <= scroll_top.item_ix {
            scroll_top.item_ix = ix;
            scroll_top.offset_in_item = px(0.);
        } else {
            let mut cursor = state.items.cursor::<ListItemSummary>(());
            cursor.seek(&Count(ix + 1), Bias::Right);
            let bottom = cursor.start().height + padding.top;
            let goal_top = px(0.).max(bottom - height + padding.bottom);

            cursor.seek(&Height(goal_top), Bias::Left);
            let start_ix = cursor.start().count;
            let start_item_top = cursor.start().height;

            if start_ix >= scroll_top.item_ix {
                scroll_top.item_ix = start_ix;
                scroll_top.offset_in_item = goal_top - start_item_top;
            }
        }

        state.logical_scroll_top = Some(scroll_top);
    }

    /// Get the bounds for the given item in window coordinates, if it's
    /// been rendered.
    pub fn bounds_for_item(&self, ix: usize) -> Option<Bounds<Pixels>> {
        let state = &*self.0.borrow();

        let bounds = state.last_layout_bounds.unwrap_or_default();
        let scroll_top = state.logical_scroll_top();
        if ix < scroll_top.item_ix {
            return None;
        }

        let mut cursor = state.items.cursor::<Dimensions<Count, Height>>(());
        cursor.seek(&Count(scroll_top.item_ix), Bias::Right);

        let scroll_top = cursor.start().1.0 + scroll_top.offset_in_item;

        cursor.seek_forward(&Count(ix), Bias::Right);
        if let Some(&ListItem::Measured { size, .. }) = cursor.item() {
            let &Dimensions(Count(count), Height(top), _) = cursor.start();
            if count == ix {
                let top = bounds.top() + top - scroll_top;
                return Some(Bounds::from_corners(
                    point(bounds.left(), top),
                    point(bounds.right(), top + size.height),
                ));
            }
        }
        None
    }

    /// Call this method when the user starts dragging the scrollbar.
    ///
    /// This will prevent the height reported to the scrollbar from changing during the drag
    /// as items in the overdraw get measured, and help offset scroll position changes accordingly.
    pub fn scrollbar_drag_started(&self) {
        let mut state = self.0.borrow_mut();
        state.scrollbar_drag_start_height = Some(state.items.summary().height);
    }

    /// Called when the user stops dragging the scrollbar.
    ///
    /// See `scrollbar_drag_started`.
    pub fn scrollbar_drag_ended(&self) {
        self.0.borrow_mut().scrollbar_drag_start_height.take();
    }

    /// Set the offset from the scrollbar
    pub fn set_offset_from_scrollbar(&self, point: Point<Pixels>) {
        self.0.borrow_mut().set_offset_from_scrollbar(point);
    }

    /// Returns the maximum scroll offset according to the items we have measured.
    /// This value remains constant while dragging to prevent the scrollbar from moving away unexpectedly.
    pub fn max_offset_for_scrollbar(&self) -> Size<Pixels> {
        let state = self.0.borrow();
        let bounds = state.last_layout_bounds.unwrap_or_default();

        let height = state
            .scrollbar_drag_start_height
            .unwrap_or_else(|| state.items.summary().height);

        Size::new(Pixels::ZERO, Pixels::ZERO.max(height - bounds.size.height))
    }

    /// Returns the current scroll offset adjusted for the scrollbar
    pub fn scroll_px_offset_for_scrollbar(&self) -> Point<Pixels> {
        let state = &self.0.borrow();
        let logical_scroll_top = state.logical_scroll_top();

        let mut cursor = state.items.cursor::<ListItemSummary>(());
        let summary: ListItemSummary =
            cursor.summary(&Count(logical_scroll_top.item_ix), Bias::Right);
        let content_height = state.items.summary().height;
        let drag_offset =
            // if dragging the scrollbar, we want to offset the point if the height changed
            content_height - state.scrollbar_drag_start_height.unwrap_or(content_height);
        let offset = summary.height + logical_scroll_top.offset_in_item - drag_offset;

        Point::new(px(0.), -offset)
    }

    /// Return the bounds of the viewport in pixels.
    pub fn viewport_bounds(&self) -> Bounds<Pixels> {
        self.0.borrow().last_layout_bounds.unwrap_or_default()
    }
}

impl StateInner {
    fn visible_range(&self, height: Pixels, scroll_top: &ListOffset) -> Range<usize> {
        let mut cursor = self.items.cursor::<ListItemSummary>(());
        cursor.seek(&Count(scroll_top.item_ix), Bias::Right);
        let start_y = cursor.start().height + scroll_top.offset_in_item;
        cursor.seek_forward(&Height(start_y + height), Bias::Left);
        scroll_top.item_ix..cursor.start().count + 1
    }

    fn scroll(
        &mut self,
        scroll_top: &ListOffset,
        height: Pixels,
        delta: Point<Pixels>,
        current_view: EntityId,
        window: &mut Window,
        cx: &mut App,
    ) {
        // Drop scroll events after a reset, since we can't calculate
        // the new logical scroll top without the item heights
        if self.reset {
            return;
        }

        let padding = self.last_padding.unwrap_or_default();
        let scroll_max =
            (self.items.summary().height + padding.top + padding.bottom - height).max(px(0.));
        let new_scroll_top = (self.scroll_top(scroll_top) - delta.y)
            .max(px(0.))
            .min(scroll_max);

        if self.alignment == ListAlignment::Bottom && new_scroll_top == scroll_max {
            self.logical_scroll_top = None;
        } else {
            let (start, ..) =
                self.items
                    .find::<ListItemSummary, _>((), &Height(new_scroll_top), Bias::Right);
            let item_ix = start.count;
            let offset_in_item = new_scroll_top - start.height;
            self.logical_scroll_top = Some(ListOffset {
                item_ix,
                offset_in_item,
            });
        }

        if self.scroll_handler.is_some() {
            let visible_range = self.visible_range(height, scroll_top);
            self.scroll_handler.as_mut().unwrap()(
                &ListScrollEvent {
                    visible_range,
                    count: self.items.summary().count,
                    is_scrolled: self.logical_scroll_top.is_some(),
                },
                window,
                cx,
            );
        }

        cx.notify(current_view);
    }

    fn logical_scroll_top(&self) -> ListOffset {
        self.logical_scroll_top
            .unwrap_or_else(|| match self.alignment {
                ListAlignment::Top => ListOffset {
                    item_ix: 0,
                    offset_in_item: px(0.),
                },
                ListAlignment::Bottom => ListOffset {
                    item_ix: self.items.summary().count,
                    offset_in_item: px(0.),
                },
            })
    }

    fn scroll_top(&self, logical_scroll_top: &ListOffset) -> Pixels {
        let (start, ..) = self.items.find::<ListItemSummary, _>(
            (),
            &Count(logical_scroll_top.item_ix),
            Bias::Right,
        );
        start.height + logical_scroll_top.offset_in_item
    }

    fn layout_all_items(
        &mut self,
        available_width: Pixels,
        render_item: &mut RenderItemFn,
        window: &mut Window,
        cx: &mut App,
    ) {
        match &mut self.measuring_behavior {
            ListMeasuringBehavior::Visible => {
                return;
            }
            ListMeasuringBehavior::Measure(has_measured) => {
                if *has_measured {
                    return;
                }
                *has_measured = true;
            }
        }

        let available_item_space = size(
            AvailableSpace::Definite(available_width),
            AvailableSpace::MinContent,
        );

        let mut measured_items = Vec::default();

        let items: Vec<ListItem> = self.items.iter().cloned().collect();
        for (ix, item) in items.into_iter().enumerate() {
            let size = match item.size() {
                Some(size) => size,
                None => {
                    let mut element = render_item(ix, window, cx);
                    let fiber_id = self.item_fibers.get_or_create(ix, window);
                    layout_item_fiber(fiber_id, &mut element, available_item_space, window, cx)
                }
            };

            measured_items.push(ListItem::Measured {
                size,
                focus_handle: item.focus_handle(),
            });
        }

        self.items = SumTree::from_iter(measured_items, ());
    }

    fn layout_items(
        &mut self,
        available_width: Option<Pixels>,
        available_height: Pixels,
        padding: &Edges<Pixels>,
        render_item: &mut RenderItemFn,
        window: &mut Window,
        cx: &mut App,
    ) -> LayoutItemsResponse {
        let old_items = self.items.clone();
        let mut measured_items = VecDeque::new();
        let mut item_layouts = VecDeque::new();
        let mut rendered_height = padding.top;
        let mut max_item_width = px(0.);
        let mut scroll_top = self.logical_scroll_top();
        let mut rendered_focused_item = false;

        let available_item_space = size(
            available_width.map_or(AvailableSpace::MinContent, |width| {
                AvailableSpace::Definite(width)
            }),
            AvailableSpace::MinContent,
        );

        let mut cursor = old_items.cursor::<Count>(());

        // Render items after the scroll top, including those in the trailing overdraw
        cursor.seek(&Count(scroll_top.item_ix), Bias::Right);
        for (ix, item) in cursor.by_ref().enumerate() {
            let visible_height = rendered_height - scroll_top.offset_in_item;
            if visible_height >= available_height + self.overdraw {
                break;
            }

            // Use the previously cached height and focus handle if available
            let mut size = item.size();

            // If we're within the visible area or the height wasn't cached, render and measure the item's element
            if visible_height < available_height || size.is_none() {
                let item_index = scroll_top.item_ix + ix;
                let mut element = render_item(item_index, window, cx);
                let fiber_id = self.item_fibers.get_or_create(item_index, window);
                let element_size =
                    layout_item_fiber(fiber_id, &mut element, available_item_space, window, cx);
                size = Some(element_size);

                // If there's a pending scroll adjustment for the scroll-top
                // item, apply it, ensuring proportional scroll position is
                // maintained after re-measuring.
                if ix == 0 {
                    if let Some(pending_scroll) = self.pending_scroll.take() {
                        if pending_scroll.item_ix == scroll_top.item_ix {
                            scroll_top.offset_in_item =
                                Pixels(pending_scroll.fraction * element_size.height.0);
                            self.logical_scroll_top = Some(scroll_top);
                        }
                    }
                }

                if visible_height < available_height {
                    item_layouts.push_back(ItemLayout {
                        index: item_index,
                        fiber_id,
                        size: element_size,
                    });
                    if item.contains_focused(window, cx) {
                        rendered_focused_item = true;
                    }
                }
            }

            let size = size.unwrap();
            rendered_height += size.height;
            max_item_width = max_item_width.max(size.width);
            measured_items.push_back(ListItem::Measured {
                size,
                focus_handle: item.focus_handle(),
            });
        }
        rendered_height += padding.bottom;

        // Prepare to start walking upward from the item at the scroll top.
        cursor.seek(&Count(scroll_top.item_ix), Bias::Right);

        // If the rendered items do not fill the visible region, then adjust
        // the scroll top upward.
        if rendered_height - scroll_top.offset_in_item < available_height {
            while rendered_height < available_height {
                cursor.prev();
                if let Some(item) = cursor.item() {
                    let item_index = cursor.start().0;
                    let mut element = render_item(item_index, window, cx);
                    let fiber_id = self.item_fibers.get_or_create(item_index, window);
                    let element_size =
                        layout_item_fiber(fiber_id, &mut element, available_item_space, window, cx);
                    let focus_handle = item.focus_handle();
                    rendered_height += element_size.height;
                    measured_items.push_front(ListItem::Measured {
                        size: element_size,
                        focus_handle,
                    });
                    item_layouts.push_front(ItemLayout {
                        index: item_index,
                        fiber_id,
                        size: element_size,
                    });
                    if item.contains_focused(window, cx) {
                        rendered_focused_item = true;
                    }
                } else {
                    break;
                }
            }

            scroll_top = ListOffset {
                item_ix: cursor.start().0,
                offset_in_item: rendered_height - available_height,
            };

            match self.alignment {
                ListAlignment::Top => {
                    scroll_top.offset_in_item = scroll_top.offset_in_item.max(px(0.));
                    self.logical_scroll_top = Some(scroll_top);
                }
                ListAlignment::Bottom => {
                    scroll_top = ListOffset {
                        item_ix: cursor.start().0,
                        offset_in_item: rendered_height - available_height,
                    };
                    self.logical_scroll_top = None;
                }
            };
        }

        // Measure items in the leading overdraw
        let mut leading_overdraw = scroll_top.offset_in_item;
        while leading_overdraw < self.overdraw {
            cursor.prev();
            if let Some(item) = cursor.item() {
                let size = if let ListItem::Measured { size, .. } = item {
                    *size
                } else {
                    let item_index = cursor.start().0;
                    let mut element = render_item(item_index, window, cx);
                    let fiber_id = self.item_fibers.get_or_create(item_index, window);
                    layout_item_fiber(fiber_id, &mut element, available_item_space, window, cx)
                };

                leading_overdraw += size.height;
                measured_items.push_front(ListItem::Measured {
                    size,
                    focus_handle: item.focus_handle(),
                });
            } else {
                break;
            }
        }

        let measured_range = cursor.start().0..(cursor.start().0 + measured_items.len());
        let mut cursor = old_items.cursor::<Count>(());
        let mut new_items = cursor.slice(&Count(measured_range.start), Bias::Right);
        new_items.extend(measured_items, ());
        cursor.seek(&Count(measured_range.end), Bias::Right);
        new_items.append(cursor.suffix(), ());
        self.items = new_items;

        // If none of the visible items are focused, check if an off-screen item is focused
        // and include it to be rendered after the visible items so keyboard interaction continues
        // to work for it.
        if !rendered_focused_item {
            let focused_index = {
                let mut cursor = self
                    .items
                    .filter::<_, Count>((), |summary| summary.has_focus_handles);
                cursor.next();
                let mut found = None;
                while let Some(item) = cursor.item() {
                    if item.contains_focused(window, cx) {
                        found = Some(cursor.start().0);
                        break;
                    }
                    cursor.next();
                }
                found
            };

            if let Some(item_index) = focused_index {
                let mut element = render_item(item_index, window, cx);
                let fiber_id = self.item_fibers.get_or_create(item_index, window);
                let element_size =
                    layout_item_fiber(fiber_id, &mut element, available_item_space, window, cx);
                item_layouts.push_back(ItemLayout {
                    index: item_index,
                    fiber_id,
                    size: element_size,
                });
            }
        }

        LayoutItemsResponse {
            _max_item_width: max_item_width,
            scroll_top,
            item_layouts,
        }
    }

    fn prepaint_items(
        &mut self,
        bounds: Bounds<Pixels>,
        padding: Edges<Pixels>,
        autoscroll: bool,
        render_item: &mut RenderItemFn,
        window: &mut Window,
        cx: &mut App,
    ) -> Result<LayoutItemsResponse, ListOffset> {
        window.transact(|window| {
            match self.measuring_behavior {
                ListMeasuringBehavior::Measure(has_measured) if !has_measured => {
                    self.layout_all_items(bounds.size.width, render_item, window, cx);
                }
                _ => {}
            }

            let mut layout_response = self.layout_items(
                Some(bounds.size.width),
                bounds.size.height,
                &padding,
                render_item,
                window,
                cx,
            );

            // Avoid honoring autoscroll requests from elements other than our children.
            window.take_autoscroll();

            // Only paint the visible items, if there is actually any space for them (taking padding into account)
            if bounds.size.height > padding.top + padding.bottom {
                let mut item_origin = bounds.origin + Point::new(px(0.), padding.top);
                item_origin.y -= layout_response.scroll_top.offset_in_item;
                for item in &layout_response.item_layouts {
                    prepaint_item_fiber(
                        item.fiber_id,
                        item_origin,
                        ContentMask { bounds },
                        window,
                        cx,
                    );

                    if let Some(autoscroll_bounds) = window.take_autoscroll()
                        && autoscroll
                    {
                        if autoscroll_bounds.top() < bounds.top() {
                            return Err(ListOffset {
                                item_ix: item.index,
                                offset_in_item: autoscroll_bounds.top() - item_origin.y,
                            });
                        } else if autoscroll_bounds.bottom() > bounds.bottom() {
                            let old_items = self.items.clone();
                            let mut cursor = old_items.cursor::<Count>(());
                            cursor.seek(&Count(item.index), Bias::Right);
                            let mut height = bounds.size.height - padding.top - padding.bottom;

                            // Account for the height of the element down until the autoscroll bottom.
                            height -= autoscroll_bounds.bottom() - item_origin.y;

                            // Keep decreasing the scroll top until we fill all the available space.
                            while height > Pixels::ZERO {
                                cursor.prev();
                                let item_index = cursor.start().0;
                                let item_size = cursor.item().and_then(|item| item.size());
                                let item_computed_size = match item_size {
                                    Some(size) => size,
                                    None => {
                                        let mut item_element = render_item(item_index, window, cx);
                                        let item_available_size = size(
                                            bounds.size.width.into(),
                                            AvailableSpace::MinContent,
                                        );
                                        let fiber_id =
                                            self.item_fibers.get_or_create(item_index, window);
                                        layout_item_fiber(
                                            fiber_id,
                                            &mut item_element,
                                            item_available_size,
                                            window,
                                            cx,
                                        )
                                    }
                                };
                                height -= item_computed_size.height;
                            }

                            return Err(ListOffset {
                                item_ix: cursor.start().0,
                                offset_in_item: if height < Pixels::ZERO {
                                    -height
                                } else {
                                    Pixels::ZERO
                                },
                            });
                        }
                    }

                    item_origin.y += item.size.height;
                }
            } else {
                layout_response.item_layouts.clear();
            }

            Ok(layout_response)
        })
    }

    // Scrollbar support

    fn set_offset_from_scrollbar(&mut self, point: Point<Pixels>) {
        let Some(bounds) = self.last_layout_bounds else {
            return;
        };
        let height = bounds.size.height;

        let padding = self.last_padding.unwrap_or_default();
        let content_height = self.items.summary().height;
        let scroll_max = (content_height + padding.top + padding.bottom - height).max(px(0.));
        let drag_offset =
            // if dragging the scrollbar, we want to offset the point if the height changed
            content_height - self.scrollbar_drag_start_height.unwrap_or(content_height);
        let new_scroll_top = (point.y - drag_offset).abs().max(px(0.)).min(scroll_max);

        if self.alignment == ListAlignment::Bottom && new_scroll_top == scroll_max {
            self.logical_scroll_top = None;
        } else {
            let (start, _, _) =
                self.items
                    .find::<ListItemSummary, _>((), &Height(new_scroll_top), Bias::Right);

            let item_ix = start.count;
            let offset_in_item = new_scroll_top - start.height;
            self.logical_scroll_top = Some(ListOffset {
                item_ix,
                offset_in_item,
            });
        }
    }
}

impl std::fmt::Debug for ListItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unmeasured { .. } => write!(f, "Unrendered"),
            Self::Measured { size, .. } => f.debug_struct("Rendered").field("size", size).finish(),
        }
    }
}

/// An offset into the list's items, in terms of the item index and the number
/// of pixels off the top left of the item.
#[derive(Debug, Clone, Copy, Default)]
pub struct ListOffset {
    /// The index of an item in the list
    pub item_ix: usize,
    /// The number of pixels to offset from the item index.
    pub offset_in_item: Pixels,
}

impl ElementImpl for List {
    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        let mut node = ListNode::new(self.state.clone(), self.style.clone(), self.sizing_behavior);

        // Deposit the render callback into the node's CallbackSlot
        let render_item = std::mem::replace(
            &mut self.render_item,
            Box::new(|_, _, _| crate::Empty.into_any_element()),
        );
        node.render_item.deposit(render_item);

        Some(Box::new(node))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<ListNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let node = node.as_any_mut().downcast_mut::<ListNode>()?;

        // Deposit the render callback into the node's CallbackSlot
        let render_item = std::mem::replace(
            &mut self.render_item,
            Box::new(|_, _, _| crate::Empty.into_any_element()),
        );
        node.render_item.deposit(render_item);

        // Update configuration
        node.style = self.style.clone();
        node.sizing_behavior = self.sizing_behavior;

        // The ListState is shared, so no need to update it

        Some(UpdateResult::UNCHANGED)
    }
}

impl IntoElement for List {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl Styled for List {
    fn style(&mut self) -> &mut StyleRefinement {
        &mut self.style
    }
}

impl sum_tree::Item for ListItem {
    type Summary = ListItemSummary;

    fn summary(&self, _: ()) -> Self::Summary {
        match self {
            ListItem::Unmeasured { focus_handle } => ListItemSummary {
                count: 1,
                rendered_count: 0,
                unrendered_count: 1,
                height: px(0.),
                has_focus_handles: focus_handle.is_some(),
            },
            ListItem::Measured {
                size, focus_handle, ..
            } => ListItemSummary {
                count: 1,
                rendered_count: 1,
                unrendered_count: 0,
                height: size.height,
                has_focus_handles: focus_handle.is_some(),
            },
        }
    }
}

impl sum_tree::ContextLessSummary for ListItemSummary {
    fn zero() -> Self {
        Default::default()
    }

    fn add_summary(&mut self, summary: &Self) {
        self.count += summary.count;
        self.rendered_count += summary.rendered_count;
        self.unrendered_count += summary.unrendered_count;
        self.height += summary.height;
        self.has_focus_handles |= summary.has_focus_handles;
    }
}

impl<'a> sum_tree::Dimension<'a, ListItemSummary> for Count {
    fn zero(_cx: ()) -> Self {
        Default::default()
    }

    fn add_summary(&mut self, summary: &'a ListItemSummary, _: ()) {
        self.0 += summary.count;
    }
}

impl<'a> sum_tree::Dimension<'a, ListItemSummary> for Height {
    fn zero(_cx: ()) -> Self {
        Default::default()
    }

    fn add_summary(&mut self, summary: &'a ListItemSummary, _: ()) {
        self.0 += summary.height;
    }
}

impl sum_tree::SeekTarget<'_, ListItemSummary, ListItemSummary> for Count {
    fn cmp(&self, other: &ListItemSummary, _: ()) -> std::cmp::Ordering {
        self.0.partial_cmp(&other.count).unwrap()
    }
}

impl sum_tree::SeekTarget<'_, ListItemSummary, ListItemSummary> for Height {
    fn cmp(&self, other: &ListItemSummary, _: ()) -> std::cmp::Ordering {
        self.0.partial_cmp(&other.height).unwrap()
    }
}

#[cfg(test)]
mod test {

    use gpui::{ScrollDelta, ScrollWheelEvent};
    use std::cell::Cell;
    use std::rc::Rc;

    use crate::{
        self as gpui, AppContext, Context, IntoElement, ListState, Render, Styled, TestAppContext,
        Window, div, list, point, px, size,
    };

    #[gpui::test]
    fn test_reset_after_paint_before_scroll(cx: &mut TestAppContext) {
        let cx = cx.add_empty_window();

        let state = ListState::new(5, crate::ListAlignment::Top, px(10.));

        // Ensure that the list is scrolled to the top
        state.scroll_to(gpui::ListOffset {
            item_ix: 0,
            offset_in_item: px(0.0),
        });

        struct TestView(ListState);
        impl Render for TestView {
            fn render(&mut self, _: &mut Window, _: &mut Context<Self>) -> impl IntoElement {
                list(self.0.clone(), |_, _, _| {
                    div().h(px(10.)).w_full().into_any_element()
                })
                .w_full()
                .h_full()
            }
        }

        // Paint
        cx.draw(point(px(0.), px(0.)), size(px(100.), px(20.)), |_, cx| {
            cx.new(|_| TestView(state.clone()))
        });

        // Reset
        state.reset(5);

        // And then receive a scroll event _before_ the next paint
        cx.simulate_event(ScrollWheelEvent {
            position: point(px(1.), px(1.)),
            delta: ScrollDelta::Pixels(point(px(0.), px(-500.))),
            ..Default::default()
        });

        // Scroll position should stay at the top of the list
        assert_eq!(state.logical_scroll_top().item_ix, 0);
        assert_eq!(state.logical_scroll_top().offset_in_item, px(0.));
    }

    #[gpui::test]
    fn test_scroll_by_positive_and_negative_distance(cx: &mut TestAppContext) {
        let cx = cx.add_empty_window();

        let state = ListState::new(5, crate::ListAlignment::Top, px(10.));

        struct TestView(ListState);
        impl Render for TestView {
            fn render(&mut self, _: &mut Window, _: &mut Context<Self>) -> impl IntoElement {
                list(self.0.clone(), |_, _, _| {
                    div().h(px(20.)).w_full().into_any_element()
                })
                .w_full()
                .h_full()
            }
        }

        // Paint
        cx.draw(point(px(0.), px(0.)), size(px(100.), px(100.)), |_, cx| {
            cx.new(|_| TestView(state.clone()))
        });

        // Test positive distance: start at item 1, move down 30px
        state.scroll_by(px(30.));

        // Should move to item 2
        let offset = state.logical_scroll_top();
        assert_eq!(offset.item_ix, 1);
        assert_eq!(offset.offset_in_item, px(10.));

        // Test negative distance: start at item 2, move up 30px
        state.scroll_by(px(-30.));

        // Should move back to item 1
        let offset = state.logical_scroll_top();
        assert_eq!(offset.item_ix, 0);
        assert_eq!(offset.offset_in_item, px(0.));

        // Test zero distance
        state.scroll_by(px(0.));
        let offset = state.logical_scroll_top();
        assert_eq!(offset.item_ix, 0);
        assert_eq!(offset.offset_in_item, px(0.));
    }

    #[gpui::test]
    fn test_remeasure(cx: &mut TestAppContext) {
        let cx = cx.add_empty_window();

        // Create a list with 10 items, each 100px tall. We'll keep a reference
        // to the item height so we can later change the height and assert how
        // `ListState` handles it.
        let item_height = Rc::new(Cell::new(100usize));
        let state = ListState::new(10, crate::ListAlignment::Top, px(10.));

        struct TestView {
            state: ListState,
            item_height: Rc<Cell<usize>>,
        }

        impl Render for TestView {
            fn render(&mut self, _: &mut Window, _: &mut Context<Self>) -> impl IntoElement {
                let height = self.item_height.get();
                list(self.state.clone(), move |_, _, _| {
                    div().h(px(height as f32)).w_full().into_any_element()
                })
                .w_full()
                .h_full()
            }
        }

        let state_clone = state.clone();
        let item_height_clone = item_height.clone();
        let view = cx.update(|_, cx| {
            cx.new(|_| TestView {
                state: state_clone,
                item_height: item_height_clone,
            })
        });

        // Simulate scrolling 40px inside the element with index 2. Since the
        // original item height is 100px, this equates to 40% inside the item.
        state.scroll_to(gpui::ListOffset {
            item_ix: 2,
            offset_in_item: px(40.),
        });

        cx.draw(point(px(0.), px(0.)), size(px(100.), px(200.)), |_, _| {
            view.clone()
        });

        let offset = state.logical_scroll_top();
        assert_eq!(offset.item_ix, 2);
        assert_eq!(offset.offset_in_item, px(40.));

        // Update the `item_height` to be 50px instead of 100px so we can assert
        // that the scroll position is proportionally preserved, that is,
        // instead of 40px from the top of item 2, it should be 20px, since the
        // item's height has been halved.
        item_height.set(50);
        state.remeasure();

        cx.draw(point(px(0.), px(0.)), size(px(100.), px(200.)), |_, _| view);

        let offset = state.logical_scroll_top();
        assert_eq!(offset.item_ix, 2);
        assert_eq!(offset.offset_in_item, px(20.));
    }
}
