//! A scrollable list of elements with uniform height, optimized for large lists.
//!
//! UniformList renders only the visible subset of items, using a single measured
//! height for all items. This makes it very efficient for large lists where all
//! items have the same height.
//!
//! Internally, UniformList uses the same fiber-backed rendering pipeline as List,
//! with a simpler height strategy (uniform vs variable).

use gpui_macros::Element;

use super::virtualized_list::{
    ItemFiberManager, ItemLayout, layout_item_fiber, paint_item_fibers, prepaint_item_fiber,
};
use crate::{
    AnyElement, App, AvailableSpace, Bounds, ContentMask, ElementId, Entity,
    ElementImpl, InteractiveElement, Interactivity, IntoElement, IsZero, ListSizingBehavior,
    Overflow, Pixels, Point, ScrollHandle, Size, Style, StyleRefinement, Styled, Window,
    point, px, size,
};
use refineable::Refineable;
use smallvec::SmallVec;
use std::{any::TypeId, cell::RefCell, cmp, ops::Range, rc::Rc, usize};

use super::ListHorizontalSizingBehavior;
use crate::Display;
use crate::render_node::{
    CallbackSlot, LayoutCtx, LayoutFrame, PaintCtx, PaintFrame, PrepaintCtx, PrepaintFrame,
    RenderNode, UpdateResult,
};
use crate::taffy::ToTaffy;

/// uniform_list provides lazy rendering for a set of items that are of uniform height.
/// When rendered into a container with overflow-y: hidden and a fixed (or max) height,
/// uniform_list will only render the visible subset of items.
#[track_caller]
pub fn uniform_list<R>(
    id: impl Into<ElementId>,
    item_count: usize,
    f: impl 'static + Fn(Range<usize>, &mut Window, &mut App) -> Vec<R>,
) -> UniformList
where
    R: IntoElement,
{
    let id = id.into();
    let mut base_style = StyleRefinement::default();
    base_style.overflow.y = Some(Overflow::Scroll);

    let render_range = move |range: Range<usize>, window: &mut Window, cx: &mut App| {
        f(range, window, cx)
            .into_iter()
            .map(|component| component.into_any_element())
            .collect()
    };

    UniformList {
        item_count,
        item_to_measure_index: 0,
        render_items: Some(Box::new(render_range)),
        decorations: Vec::new(),
        interactivity: Interactivity {
            element_id: Some(id),
            base_style: Box::new(base_style),
            ..Interactivity::new()
        },
        scroll_handle: None,
        sizing_behavior: ListSizingBehavior::default(),
        horizontal_sizing_behavior: ListHorizontalSizingBehavior::default(),
    }
}

/// A list element for efficiently laying out and displaying a list of uniform-height elements.
#[derive(Element)]
#[element(crate = crate)]
pub struct UniformList {
    item_count: usize,
    item_to_measure_index: usize,
    render_items: Option<Box<UniformListRenderCallback>>,
    decorations: Vec<Box<dyn UniformListDecoration>>,
    interactivity: Interactivity,
    scroll_handle: Option<UniformListScrollHandle>,
    sizing_behavior: ListSizingBehavior,
    horizontal_sizing_behavior: ListHorizontalSizingBehavior,
}

/// A handle for controlling the scroll position of a uniform list.
/// This should be stored in your view and passed to the uniform_list on each frame.
#[derive(Clone, Debug, Default)]
pub struct UniformListScrollHandle(pub Rc<RefCell<UniformListScrollState>>);

/// Where to place the element scrolled to.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScrollStrategy {
    /// Place the element at the top of the list's viewport.
    Top,
    /// Attempt to place the element in the middle of the list's viewport.
    /// May not be possible if there's not enough list items above the item scrolled to:
    /// in this case, the element will be placed at the closest possible position.
    Center,
    /// Attempt to place the element at the bottom of the list's viewport.
    /// May not be possible if there's not enough list items above the item scrolled to:
    /// in this case, the element will be placed at the closest possible position.
    Bottom,
    /// If the element is not visible attempt to place it at:
    /// - The top of the list's viewport if the target element is above currently visible elements.
    /// - The bottom of the list's viewport if the target element is above currently visible elements.
    Nearest,
}

#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct DeferredScrollToItem {
    /// The item index to scroll to
    pub item_index: usize,
    /// The scroll strategy to use
    pub strategy: ScrollStrategy,
    /// The offset in number of items
    pub offset: usize,
    pub scroll_strict: bool,
}

#[derive(Clone, Debug, Default)]
#[allow(missing_docs)]
pub struct UniformListScrollState {
    pub base_handle: ScrollHandle,
    pub deferred_scroll_to_item: Option<DeferredScrollToItem>,
    /// Size of the item, captured during last layout.
    pub last_item_size: Option<ItemSize>,
    /// Whether the list was vertically flipped during last layout.
    pub y_flipped: bool,
    /// Fiber manager for item identity across frames.
    #[allow(clippy::type_complexity)]
    pub(crate) item_fibers: Option<Rc<RefCell<ItemFiberManager>>>,
    /// Fiber manager for decoration identity across frames.
    #[allow(clippy::type_complexity)]
    pub(crate) decoration_fibers: Option<Rc<RefCell<ItemFiberManager>>>,
}

#[derive(Copy, Clone, Debug, Default)]
/// The size of the item and its contents.
pub struct ItemSize {
    /// The size of the item.
    pub item: Size<Pixels>,
    /// The size of the item's contents, which may be larger than the item itself,
    /// if the item was bounded by a parent element.
    pub contents: Size<Pixels>,
}

/// Render callback type for UniformList.
pub type UniformListRenderCallback =
    dyn for<'a> Fn(Range<usize>, &'a mut Window, &'a mut App) -> SmallVec<[AnyElement; 64]>;

/// Retained render node for UniformList elements.
///
/// UniformListNode owns all UniformList-specific state and implements the
/// scope-based prepaint/paint lifecycle. The render callback is deposited
/// each frame via CallbackSlot, enabling the element to be ephemeral while
/// the node retains state across frames.
pub(crate) struct UniformListNode {
    /// Interactivity state for scroll handling.
    pub interactivity: Interactivity,

    /// Render callback deposited by the element each frame.
    pub render_items: CallbackSlot<UniformListRenderCallback>,

    /// Configuration from element.
    pub item_count: usize,
    pub item_to_measure_index: usize,
    pub sizing_behavior: ListSizingBehavior,
    pub horizontal_sizing_behavior: ListHorizontalSizingBehavior,

    /// Scroll handle for external control.
    scroll_handle: Option<UniformListScrollHandle>,

    /// Whether the list is vertically flipped.
    y_flipped: bool,

    /// Cached item layouts for paint phase.
    cached_item_layouts: SmallVec<[ItemLayout; 32]>,
    /// Cached decoration layouts for paint phase.
    cached_decoration_layouts: SmallVec<[ItemLayout; 2]>,
    /// Cached content mask for paint phase.
    cached_content_mask: Option<ContentMask<Pixels>>,
}

impl UniformListNode {
    /// Create a new UniformListNode.
    pub fn new(
        interactivity: Interactivity,
        item_count: usize,
        item_to_measure_index: usize,
        sizing_behavior: ListSizingBehavior,
        horizontal_sizing_behavior: ListHorizontalSizingBehavior,
        scroll_handle: Option<UniformListScrollHandle>,
    ) -> Self {
        Self {
            interactivity,
            render_items: CallbackSlot::new(),
            item_count,
            item_to_measure_index,
            sizing_behavior,
            horizontal_sizing_behavior,
            scroll_handle,
            y_flipped: false,
            cached_item_layouts: SmallVec::new(),
            cached_decoration_layouts: SmallVec::new(),
            cached_content_mask: None,
        }
    }

    /// Measure an item to get the uniform item size.
    fn measure_item(
        &self,
        list_width: Option<Pixels>,
        window: &mut Window,
        cx: &mut App,
    ) -> Size<Pixels> {
        if self.item_count == 0 {
            return Size::default();
        }

        let item_ix = cmp::min(self.item_to_measure_index, self.item_count - 1);

        // Use the deposited callback to render the item for measurement
        let items = self
            .render_items
            .with(|render| render(item_ix..item_ix + 1, window, cx));

        let Some(items) = items else {
            return Size::default();
        };

        let Some(mut item_to_measure) = items.into_iter().next() else {
            return Size::default();
        };

        let available_space = size(
            list_width.map_or(AvailableSpace::MinContent, |width| {
                AvailableSpace::Definite(width)
            }),
            AvailableSpace::MaxContent,
        );

        let mut measured_size =
            window.measure_element_via_fibers(&mut item_to_measure, available_space, cx);

        if measured_size.height.is_zero() {
            if let Some(style) = item_to_measure.cached_style() {
                let mut resolved = Style::default();
                resolved.refine(style);
                if let crate::Length::Definite(height) = resolved.size.height {
                    measured_size.height =
                        height.to_pixels(crate::AbsoluteLength::Pixels(px(0.0)), window.rem_size());
                }
            }
        }

        measured_size
    }

    /// Compute visible range based on scroll offset and item height.
    fn compute_visible_range(
        &self,
        scroll_offset: Point<Pixels>,
        item_height: Pixels,
        padded_bounds: Bounds<Pixels>,
        padding_top: Pixels,
    ) -> Range<usize> {
        let first_visible_element_ix =
            (-(scroll_offset.y + padding_top) / item_height).floor() as usize;
        let last_visible_element_ix =
            ((-scroll_offset.y + padded_bounds.size.height) / item_height).ceil() as usize;

        first_visible_element_ix..cmp::min(last_visible_element_ix, self.item_count)
    }

    /// Get the fiber managers from scroll handle or create new ones.
    fn get_fiber_managers(&self) -> (Rc<RefCell<ItemFiberManager>>, Rc<RefCell<ItemFiberManager>>) {
        let item_fibers = self
            .scroll_handle
            .as_ref()
            .and_then(|h| h.0.borrow().item_fibers.clone())
            .unwrap_or_else(|| Rc::new(RefCell::new(ItemFiberManager::new())));
        let decoration_fibers = self
            .scroll_handle
            .as_ref()
            .and_then(|h| h.0.borrow().decoration_fibers.clone())
            .unwrap_or_else(|| Rc::new(RefCell::new(ItemFiberManager::new())));

        (item_fibers, decoration_fibers)
    }
}

impl RenderNode for UniformListNode {
    fn taffy_style(&self, rem_size: Pixels, scale_factor: f32) -> taffy::style::Style {
        let mut style = Style::default();
        style.refine(&self.interactivity.base_style);
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
        style.refine(&self.interactivity.base_style);
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
            skip_children: true, // We manage children (items) ourselves
            ..Default::default()
        };

        // Clear cached state from previous frame
        self.cached_item_layouts.clear();
        self.cached_decoration_layouts.clear();
        self.cached_content_mask = None;

        // Compute style
        let style =
            self.interactivity
                .compute_style_with_fiber(ctx.fiber_id, None, ctx.window, ctx.cx);

        // Skip if display: none
        if style.display == Display::None {
            return frame;
        }

        let border = style.border_widths.to_pixels(ctx.window.rem_size());
        let padding = style
            .padding
            .to_pixels(ctx.bounds.size.into(), ctx.window.rem_size());

        let padded_bounds = Bounds::from_corners(
            ctx.bounds.origin + point(border.left + padding.left, border.top + padding.top),
            ctx.bounds.bottom_right()
                - point(border.right + padding.right, border.bottom + padding.bottom),
        );

        let can_scroll_horizontally = matches!(
            self.horizontal_sizing_behavior,
            ListHorizontalSizingBehavior::Unconstrained
        );

        // Measure item to get uniform height
        let item_size = self.measure_item(None, ctx.window, ctx.cx);
        let item_height = item_size.height;

        let content_width = if can_scroll_horizontally {
            padded_bounds.size.width.max(item_size.width)
        } else {
            padded_bounds.size.width
        };
        let content_size = Size {
            width: content_width,
            height: item_height * self.item_count,
        };

        // Get scroll offset and handle deferred scroll
        let shared_scroll_offset = self.interactivity.scroll_offset.clone();
        let mut scroll_offset = shared_scroll_offset
            .as_ref()
            .map(|o| *o.borrow())
            .unwrap_or_default();

        // Update scroll handle state and get deferred scroll
        let shared_scroll_to_item = self.scroll_handle.as_mut().and_then(|handle| {
            let mut handle = handle.0.borrow_mut();
            handle.last_item_size = Some(ItemSize {
                item: padded_bounds.size,
                contents: content_size,
            });
            handle.deferred_scroll_to_item.take()
        });

        // Get y_flipped state
        self.y_flipped = self
            .scroll_handle
            .as_ref()
            .map(|h| h.0.borrow().y_flipped)
            .unwrap_or(false);

        // Prepare prepaint to set up hitbox
        let prepaint = self.interactivity.prepare_prepaint(
            ctx.fiber_id,
            ctx.inspector_id.as_ref(),
            ctx.bounds,
            content_size,
            ctx.window,
            ctx.cx,
        );

        // Push text style
        if let Some(text_style) = prepaint.style.text_style() {
            ctx.window.text_style_stack.push(text_style.clone());
            frame.pushed_text_style = true;
        }

        // Push content mask
        if let Some(mask) = prepaint
            .style
            .overflow_mask(ctx.bounds, ctx.window.rem_size())
        {
            let world_mask = ctx.window.transform_mask_to_world(mask);
            let intersected = world_mask.intersect(&PrepaintCx::new(ctx.window).content_mask());
            ctx.window.content_mask_stack.push(intersected);
            frame.pushed_content_mask = true;
        }

        // Process items if we have any
        if self.item_count > 0 {
            let content_height = item_height * self.item_count;
            let content_mask = ContentMask { bounds: ctx.bounds };
            self.cached_content_mask = Some(content_mask.clone());

            // Handle deferred scroll to item
            if let Some(DeferredScrollToItem {
                mut item_index,
                mut strategy,
                offset,
                scroll_strict,
            }) = shared_scroll_to_item
            {
                if self.y_flipped {
                    item_index = self.item_count.saturating_sub(item_index + 1);
                }
                let list_height = padded_bounds.size.height;
                let item_top = item_height * item_index;
                let item_bottom = item_top + item_height;
                let scroll_top = -scroll_offset.y;
                let offset_pixels = item_height * offset;

                // Is the selected item above/below currently visible items
                let is_above = item_top < scroll_top + offset_pixels;
                let is_below = item_bottom > scroll_top + list_height;

                if scroll_strict || is_above || is_below {
                    if strategy == ScrollStrategy::Nearest {
                        if is_above {
                            strategy = ScrollStrategy::Top;
                        } else if is_below {
                            strategy = ScrollStrategy::Bottom;
                        }
                    }

                    let max_scroll_offset = (content_height - list_height).max(Pixels::ZERO);
                    match strategy {
                        ScrollStrategy::Top => {
                            scroll_offset.y =
                                -(item_top - offset_pixels).clamp(Pixels::ZERO, max_scroll_offset);
                        }
                        ScrollStrategy::Center => {
                            let item_center = item_top + item_height / 2.0;
                            let viewport_height = list_height - offset_pixels;
                            let viewport_center = offset_pixels + viewport_height / 2.0;
                            let target_scroll_top = item_center - viewport_center;
                            scroll_offset.y =
                                -target_scroll_top.clamp(Pixels::ZERO, max_scroll_offset);
                        }
                        ScrollStrategy::Bottom => {
                            scroll_offset.y =
                                -(item_bottom - list_height).clamp(Pixels::ZERO, max_scroll_offset);
                        }
                        ScrollStrategy::Nearest => {
                            // Nearest, but the item is visible -> no scroll is required
                        }
                    }

                    // Update the shared scroll offset
                    if let Some(ref shared_offset) = shared_scroll_offset {
                        *shared_offset.borrow_mut() = scroll_offset;
                    }
                }
            }

            // Compute visible range
            let visible_range =
                self.compute_visible_range(scroll_offset, item_height, padded_bounds, padding.top);

            // Render items using deposited callback
            let items = if self.y_flipped {
                let flipped_range = self.item_count.saturating_sub(visible_range.end)
                    ..self.item_count.saturating_sub(visible_range.start);
                let mut items = self
                    .render_items
                    .with(|render| render(flipped_range, ctx.window, ctx.cx))
                    .unwrap_or_default();
                items.reverse();
                items
            } else {
                self.render_items
                    .with(|render| render(visible_range.clone(), ctx.window, ctx.cx))
                    .unwrap_or_default()
            };

            let (item_fibers, _decoration_fibers) = self.get_fiber_managers();

            // Layout and prepaint items
            for (mut item, ix) in items.into_iter().zip(visible_range) {
                let item_origin =
                    padded_bounds.origin + scroll_offset + point(Pixels::ZERO, item_height * ix);

                let available_width = if can_scroll_horizontally {
                    padded_bounds.size.width + scroll_offset.x.abs()
                } else {
                    padded_bounds.size.width
                };
                let available_space = size(
                    AvailableSpace::Definite(available_width),
                    AvailableSpace::Definite(item_height),
                );

                let fiber_id = item_fibers.borrow_mut().get_or_create(ix, ctx.window);
                let item_size =
                    layout_item_fiber(fiber_id, &mut item, available_space, ctx.window, ctx.cx);
                prepaint_item_fiber(
                    fiber_id,
                    item_origin,
                    content_mask.clone(),
                    ctx.window,
                    ctx.cx,
                );

                self.cached_item_layouts.push(ItemLayout {
                    index: ix,
                    fiber_id,
                    size: item_size,
                });
            }
        }

        frame
    }

    fn prepaint_end(&mut self, ctx: &mut PrepaintCtx, frame: PrepaintFrame) {
        // Pop pushed context in reverse order
        if frame.pushed_content_mask {
            ctx.window.content_mask_stack.pop();
        }
        if frame.pushed_text_style {
            ctx.window.text_style_stack.pop();
        }
    }

    fn paint_begin(&mut self, ctx: &mut PaintCtx) -> PaintFrame {
        use crate::window::context::PaintCx;

        let mut frame = PaintFrame {
            handled: true,
            skip_children: true, // We manage children ourselves
            ..Default::default()
        };

        // Get hitbox from window
        let hitbox = ctx.window.resolve_hitbox(&ctx.fiber_id);

        // Call prepare_paint
        let Some(paint) = self.interactivity.prepare_paint(
            ctx.fiber_id,
            ctx.bounds,
            hitbox.as_ref(),
            ctx.window,
            ctx.cx,
        ) else {
            return frame;
        };

        // Apply opacity
        if let Some(opacity) = paint.style.opacity {
            frame.previous_opacity = Some(ctx.window.element_opacity);
            ctx.window.element_opacity *= opacity;
        }

        // Push text style
        if let Some(text_style) = paint.style.text_style() {
            ctx.window.text_style_stack.push(text_style.clone());
            frame.pushed_text_style = true;
        }

        // Push content mask
        if let Some(mask) = paint.style.overflow_mask(ctx.bounds, ctx.window.rem_size()) {
            let world_mask = ctx.window.transform_mask_to_world(mask);
            let intersected = world_mask.intersect(&PaintCx::new(ctx.window).content_mask());
            ctx.window.content_mask_stack.push(intersected);
            frame.pushed_content_mask = true;
        }

        // Paint background
        paint
            .style
            .paint_before_children(ctx.bounds, ctx.window, ctx.cx);

        // Handle cursor style
        if let Some(hitbox) = hitbox.as_ref() {
            if let Some(drag) = ctx.cx.active_drag.as_ref() {
                if let Some(mouse_cursor) = drag.cursor_style {
                    ctx.window.set_window_cursor_style(mouse_cursor);
                }
            } else if let Some(mouse_cursor) = paint.style.mouse_cursor {
                ctx.window.set_cursor_style(mouse_cursor, hitbox);
            }
        }

        // Paint items
        if let Some(content_mask) = self.cached_content_mask.clone() {
            paint_item_fibers(
                &self.cached_item_layouts,
                content_mask.clone(),
                ctx.window,
                ctx.cx,
            );
            paint_item_fibers(
                &self.cached_decoration_layouts,
                content_mask,
                ctx.window,
                ctx.cx,
            );
        }

        frame
    }

    fn paint_end(&mut self, ctx: &mut PaintCtx, frame: PaintFrame) {
        // Pop pushed context in reverse order
        if frame.pushed_content_mask {
            ctx.window.content_mask_stack.pop();
        }
        if frame.pushed_text_style {
            ctx.window.text_style_stack.pop();
        }
        if let Some(previous_opacity) = frame.previous_opacity {
            ctx.window.element_opacity = previous_opacity;
        }
    }

    fn interactivity(&self) -> Option<&Interactivity> {
        Some(&self.interactivity)
    }
}

impl UniformListScrollHandle {
    /// Create a new scroll handle to bind to a uniform list.
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(UniformListScrollState {
            base_handle: ScrollHandle::new(),
            deferred_scroll_to_item: None,
            last_item_size: None,
            y_flipped: false,
            item_fibers: Some(Rc::new(RefCell::new(ItemFiberManager::new()))),
            decoration_fibers: Some(Rc::new(RefCell::new(ItemFiberManager::new()))),
        })))
    }

    /// Scroll the list so that the given item index is visible.
    ///
    /// This uses non-strict scrolling: if the item is already fully visible, no scrolling occurs.
    /// If the item is out of view, it scrolls the minimum amount to bring it into view according
    /// to the strategy.
    pub fn scroll_to_item(&self, ix: usize, strategy: ScrollStrategy) {
        self.0.borrow_mut().deferred_scroll_to_item = Some(DeferredScrollToItem {
            item_index: ix,
            strategy,
            offset: 0,
            scroll_strict: false,
        });
    }

    /// Scroll the list so that the given item index is at scroll strategy position.
    ///
    /// This uses strict scrolling: the item will always be scrolled to match the strategy position,
    /// even if it's already visible. Use this when you need precise positioning.
    pub fn scroll_to_item_strict(&self, ix: usize, strategy: ScrollStrategy) {
        self.0.borrow_mut().deferred_scroll_to_item = Some(DeferredScrollToItem {
            item_index: ix,
            strategy,
            offset: 0,
            scroll_strict: true,
        });
    }

    /// Scroll the list to the given item index with an offset in number of items.
    ///
    /// This uses non-strict scrolling: if the item is already visible within the offset region,
    /// no scrolling occurs.
    ///
    /// The offset parameter shrinks the effective viewport by the specified number of items
    /// from the corresponding edge, then applies the scroll strategy within that reduced viewport:
    /// - `ScrollStrategy::Top`: Shrinks from top, positions item at the new top
    /// - `ScrollStrategy::Center`: Shrinks from top, centers item in the reduced viewport
    /// - `ScrollStrategy::Bottom`: Shrinks from bottom, positions item at the new bottom
    pub fn scroll_to_item_with_offset(&self, ix: usize, strategy: ScrollStrategy, offset: usize) {
        self.0.borrow_mut().deferred_scroll_to_item = Some(DeferredScrollToItem {
            item_index: ix,
            strategy,
            offset,
            scroll_strict: false,
        });
    }

    /// Scroll the list so that the given item index is at the exact scroll strategy position with an offset.
    ///
    /// This uses strict scrolling: the item will always be scrolled to match the strategy position,
    /// even if it's already visible.
    ///
    /// The offset parameter shrinks the effective viewport by the specified number of items
    /// from the corresponding edge, then applies the scroll strategy within that reduced viewport:
    /// - `ScrollStrategy::Top`: Shrinks from top, positions item at the new top
    /// - `ScrollStrategy::Center`: Shrinks from top, centers item in the reduced viewport
    /// - `ScrollStrategy::Bottom`: Shrinks from bottom, positions item at the new bottom
    pub fn scroll_to_item_strict_with_offset(
        &self,
        ix: usize,
        strategy: ScrollStrategy,
        offset: usize,
    ) {
        self.0.borrow_mut().deferred_scroll_to_item = Some(DeferredScrollToItem {
            item_index: ix,
            strategy,
            offset,
            scroll_strict: true,
        });
    }

    /// Check if the list is flipped vertically.
    pub fn y_flipped(&self) -> bool {
        self.0.borrow().y_flipped
    }

    /// Get the index of the topmost visible child.
    #[cfg(any(test, feature = "test-support"))]
    pub fn logical_scroll_top_index(&self) -> usize {
        let this = self.0.borrow();
        this.deferred_scroll_to_item
            .as_ref()
            .map(|deferred| deferred.item_index)
            .unwrap_or_else(|| this.base_handle.logical_scroll_top().0)
    }

    /// Checks if the list can be scrolled vertically.
    pub fn is_scrollable(&self) -> bool {
        if let Some(size) = self.0.borrow().last_item_size {
            size.contents.height > size.item.height
        } else {
            false
        }
    }

    /// Scroll to the bottom of the list.
    pub fn scroll_to_bottom(&self) {
        self.scroll_to_item(usize::MAX, ScrollStrategy::Bottom);
    }
}

impl Styled for UniformList {
    fn style(&mut self) -> &mut StyleRefinement {
        &mut self.interactivity.base_style
    }
}

impl ElementImpl for UniformList {
    fn id(&self) -> Option<ElementId> {
        self.interactivity.element_id.clone()
    }

    fn source_location(&self) -> Option<&'static std::panic::Location<'static>> {
        self.interactivity.source_location()
    }

    fn cached_style(&self) -> Option<&StyleRefinement> {
        Some(&self.interactivity.base_style)
    }

    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        let mut node = UniformListNode::new(
            std::mem::take(&mut self.interactivity),
            self.item_count,
            self.item_to_measure_index,
            self.sizing_behavior,
            self.horizontal_sizing_behavior,
            self.scroll_handle.take(),
        );

        // Deposit the callback immediately
        if let Some(render_items) = self.render_items.take() {
            node.render_items.deposit(render_items);
        }

        // Set up scroll_offset from tracked_scroll_handle
        // This is normally done in request_layout, but we need it before then
        if let Some(scroll_handle) = &node.interactivity.tracked_scroll_handle {
            node.interactivity.scroll_offset = Some(scroll_handle.offset_rc());
        }

        Some(Box::new(node))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<UniformListNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let node = node.as_any_mut().downcast_mut::<UniformListNode>()?;

        // Deposit the render callback into the node's CallbackSlot
        if let Some(render_items) = self.render_items.take() {
            node.render_items.deposit(render_items);
        }

        // Update configuration
        node.item_count = self.item_count;
        node.item_to_measure_index = self.item_to_measure_index;
        node.sizing_behavior = self.sizing_behavior;
        node.horizontal_sizing_behavior = self.horizontal_sizing_behavior;

        // Update interactivity
        node.interactivity = std::mem::take(&mut self.interactivity);

        // Update scroll handle
        if let Some(scroll_handle) = self.scroll_handle.take() {
            node.scroll_handle = Some(scroll_handle);
        }

        // Set up scroll_offset from tracked_scroll_handle if not already present
        // This is normally done in request_layout, but we need it before then
        if node.interactivity.scroll_offset.is_none() {
            if let Some(scroll_handle) = &node.interactivity.tracked_scroll_handle {
                node.interactivity.scroll_offset = Some(scroll_handle.offset_rc());
            }
        }

        // Check if there's a deferred scroll that needs prepaint to run
        let needs_prepaint = node
            .scroll_handle
            .as_ref()
            .map(|handle| handle.0.borrow().deferred_scroll_to_item.is_some())
            .unwrap_or(false);

        if needs_prepaint {
            Some(UpdateResult::PAINT_ONLY)
        } else {
            Some(UpdateResult::UNCHANGED)
        }
    }
}

impl IntoElement for UniformList {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

/// A decoration for a [`UniformList`]. This can be used for various things,
/// such as rendering indent guides, or other visual effects.
pub trait UniformListDecoration {
    /// Compute the decoration element, given the visible range of list items,
    /// the bounds of the list, and the height of each item.
    fn compute(
        &self,
        visible_range: Range<usize>,
        bounds: Bounds<Pixels>,
        scroll_offset: Point<Pixels>,
        item_height: Pixels,
        item_count: usize,
        window: &mut Window,
        cx: &mut App,
    ) -> AnyElement;
}

impl<T: UniformListDecoration + 'static> UniformListDecoration for Entity<T> {
    fn compute(
        &self,
        visible_range: Range<usize>,
        bounds: Bounds<Pixels>,
        scroll_offset: Point<Pixels>,
        item_height: Pixels,
        item_count: usize,
        window: &mut Window,
        cx: &mut App,
    ) -> AnyElement {
        self.update(cx, |inner, cx| {
            inner.compute(
                visible_range,
                bounds,
                scroll_offset,
                item_height,
                item_count,
                window,
                cx,
            )
        })
    }
}

impl UniformList {
    /// Selects a specific list item for measurement.
    pub fn with_width_from_item(mut self, item_index: Option<usize>) -> Self {
        self.item_to_measure_index = item_index.unwrap_or(0);
        self
    }

    /// Sets the sizing behavior, similar to the `List` element.
    pub fn with_sizing_behavior(mut self, behavior: ListSizingBehavior) -> Self {
        self.sizing_behavior = behavior;
        self
    }

    /// Sets the horizontal sizing behavior, controlling the way list items laid out horizontally.
    /// With [`ListHorizontalSizingBehavior::Unconstrained`] behavior, every item and the list itself will
    /// have the size of the widest item and lay out pushing the `end_slot` to the right end.
    pub fn with_horizontal_sizing_behavior(
        mut self,
        behavior: ListHorizontalSizingBehavior,
    ) -> Self {
        self.horizontal_sizing_behavior = behavior;
        match behavior {
            ListHorizontalSizingBehavior::FitList => {
                self.interactivity.base_style.overflow.x = None;
            }
            ListHorizontalSizingBehavior::Unconstrained => {
                self.interactivity.base_style.overflow.x = Some(Overflow::Scroll);
            }
        }
        self
    }

    /// Adds a decoration element to the list.
    pub fn with_decoration(mut self, decoration: impl UniformListDecoration + 'static) -> Self {
        self.decorations.push(Box::new(decoration));
        self
    }

    /// Track and render scroll state of this list with reference to the given scroll handle.
    pub fn track_scroll(mut self, handle: &UniformListScrollHandle) -> Self {
        self.interactivity.tracked_scroll_handle = Some(handle.0.borrow().base_handle.clone());
        self.scroll_handle = Some(handle.clone());
        self
    }

    /// Sets whether the list is flipped vertically, such that item 0 appears at the bottom.
    pub fn y_flipped(mut self, y_flipped: bool) -> Self {
        if let Some(ref scroll_handle) = self.scroll_handle {
            let mut scroll_state = scroll_handle.0.borrow_mut();
            let mut base_handle = &scroll_state.base_handle;
            let offset = base_handle.offset();
            match scroll_state.last_item_size {
                Some(last_size) if scroll_state.y_flipped != y_flipped => {
                    let new_y_offset =
                        -(offset.y + last_size.contents.height - last_size.item.height);
                    base_handle.set_offset(point(offset.x, new_y_offset));
                    scroll_state.y_flipped = y_flipped;
                }
                // Handle case where list is initially flipped.
                None if y_flipped => {
                    base_handle.set_offset(point(offset.x, Pixels::MIN));
                    scroll_state.y_flipped = y_flipped;
                }
                _ => {}
            }
        }
        self
    }
}

impl InteractiveElement for UniformList {
    fn interactivity(&mut self) -> &mut crate::Interactivity {
        &mut self.interactivity
    }
}

#[cfg(test)]
mod test {
    use crate::TestAppContext;

    #[gpui::test]
    fn test_scroll_strategy_nearest(cx: &mut TestAppContext) {
        use crate::{
            Context, FocusHandle, ScrollStrategy, UniformListScrollHandle, Window, div, prelude::*,
            px, uniform_list,
        };
        use std::ops::Range;

        actions!(example, [SelectNext, SelectPrev]);

        struct TestView {
            index: usize,
            length: usize,
            scroll_handle: UniformListScrollHandle,
            focus_handle: FocusHandle,
            visible_range: Range<usize>,
        }

        impl TestView {
            pub fn select_next(
                &mut self,
                _: &SelectNext,
                window: &mut Window,
                _: &mut Context<Self>,
            ) {
                if self.index + 1 == self.length {
                    self.index = 0
                } else {
                    self.index += 1;
                }
                self.scroll_handle
                    .scroll_to_item(self.index, ScrollStrategy::Nearest);
                window.refresh();
            }

            pub fn select_previous(
                &mut self,
                _: &SelectPrev,
                window: &mut Window,
                _: &mut Context<Self>,
            ) {
                if self.index == 0 {
                    self.index = self.length - 1
                } else {
                    self.index -= 1;
                }
                self.scroll_handle
                    .scroll_to_item(self.index, ScrollStrategy::Nearest);
                window.refresh();
            }
        }

        impl Render for TestView {
            fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
                div()
                    .id("list-example")
                    .track_focus(&self.focus_handle)
                    .on_action(cx.listener(Self::select_next))
                    .on_action(cx.listener(Self::select_previous))
                    .size_full()
                    .child(
                        uniform_list(
                            "entries",
                            self.length,
                            cx.processor(|this, range: Range<usize>, _window, _cx| {
                                this.visible_range = range.clone();
                                range
                                    .map(|ix| div().id(ix).h(px(20.0)).child(format!("Item {ix}")))
                                    .collect()
                            }),
                        )
                        .track_scroll(&self.scroll_handle)
                        .h(px(200.0)),
                    )
            }
        }

        let (view, cx) = cx.add_window_view(|window, cx| {
            let focus_handle = cx.focus_handle();
            window.focus(&focus_handle, cx);
            TestView {
                scroll_handle: UniformListScrollHandle::new(),
                index: 0,
                focus_handle,
                length: 47,
                visible_range: 0..0,
            }
        });

        // 10 out of 47 items are visible

        // First 9 times selecting next item does not scroll
        for ix in 1..10 {
            cx.dispatch_action(SelectNext);
            view.read_with(cx, |view, _| {
                assert_eq!(view.index, ix);
                assert_eq!(view.visible_range, 0..10);
            })
        }

        // Now each time the list scrolls down by 1
        for ix in 10..47 {
            cx.dispatch_action(SelectNext);
            view.read_with(cx, |view, _| {
                assert_eq!(view.index, ix);
                assert_eq!(view.visible_range, ix - 9..ix + 1);
            })
        }

        // After the last item we move back to the start
        cx.dispatch_action(SelectNext);
        view.read_with(cx, |view, _| {
            assert_eq!(view.index, 0);
            assert_eq!(view.visible_range, 0..10);
        });

        // Return to the last element
        cx.dispatch_action(SelectPrev);
        view.read_with(cx, |view, _| {
            assert_eq!(view.index, 46);
            assert_eq!(view.visible_range, 37..47);
        });

        // First 9 times selecting previous does not scroll
        for ix in (37..46).rev() {
            cx.dispatch_action(SelectPrev);
            view.read_with(cx, |view, _| {
                assert_eq!(view.index, ix);
                assert_eq!(view.visible_range, 37..47);
            })
        }

        // Now each time the list scrolls up by 1
        for ix in (0..37).rev() {
            cx.dispatch_action(SelectPrev);
            view.read_with(cx, |view, _| {
                assert_eq!(view.index, ix);
                assert_eq!(view.visible_range, ix..ix + 10);
            })
        }
    }
}
