//! Retained render nodes for incremental rendering.
//!
//! This module defines the `RenderNode` trait and associated types for the
//! retained layer of GPUI's two-layer rendering architecture. RenderNodes
//! persist in the fiber tree and own all element-specific state and caches.
//!
//! # Architecture
//!
//! Elements (descriptors) are ephemeral - created each frame via `render()`.
//! RenderNodes are persistent - they live in the fiber tree and are updated
//! by descriptors during reconciliation.
//!
//! This separation allows:
//! - O(changed) incremental rendering via dirty tracking
//! - State persistence (scroll position, hover state, etc.) across frames
//! - Generic fiber traversal without element-type knowledge

use crate::{
    AnyElement, AnyTooltip, App, AvailableSpace, Bounds, ClickEvent, ContentMask, CursorStyle,
    DispatchPhase, FocusHandle, GlobalElementId, Hitbox, HitboxBehavior, InputHandler,
    InspectorElementId, Interactivity, IntrinsicSize, IntrinsicSizeResult, KeyContext,
    MouseDownEvent, MouseMoveEvent, MouseUpEvent, Pixels, ScrollWheelEvent, SharedString, Size,
    SizeQuery, SizingCtx, SizingInput, TooltipId, Window,
};
use smallvec::SmallVec;
use std::any::{Any, TypeId};
use std::rc::Rc;
use taffy::style::Style as TaffyStyle;

/// A minimal, blanket-implemented downcasting capability.
///
/// This avoids repeating `as_any` / `as_any_mut` boilerplate on every node type,
/// while still enabling descriptor-side node updates that need to downcast.
pub trait AsAny: Any {
    /// Return a reference to self as `&dyn Any` for downcasting.
    fn as_any(&self) -> &dyn Any;
    /// Return a mutable reference to self as `&mut dyn Any` for downcasting.
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T: Any> AsAny for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

/// Context provided to RenderNode during layout setup.
///
/// This context is passed to `layout_begin` and `layout_end` hooks,
/// allowing nodes to push inherited context (text style, image cache)
/// and compute their Taffy style with access to rem_size and scale_factor.
pub struct LayoutCtx<'a> {
    /// The fiber's global element ID.
    pub fiber_id: GlobalElementId,
    /// The current rem size for unit conversion.
    pub rem_size: Pixels,
    /// The current scale factor for unit conversion.
    pub scale_factor: f32,
    /// Window access.
    pub window: &'a mut Window,
    /// App context.
    pub cx: &'a mut App,
}

/// Frame state returned by `layout_begin`.
///
/// This struct tracks what context was pushed during layout_begin
/// so that layout_end can pop it in reverse order.
#[derive(Clone, Default)]
pub struct LayoutFrame {
    /// Whether the node fully handled the layout phase.
    /// When true, the legacy VKind-specific layout code is skipped,
    /// and the fiber's taffy_style is set from node.taffy_style(rem_size, scale_factor).
    pub handled: bool,
    /// Whether a text style was pushed.
    pub pushed_text_style: bool,
    /// Whether an image cache was pushed.
    pub pushed_image_cache: bool,
    /// Whether a view boundary was pushed.
    pub pushed_view_boundary: bool,
    /// Whether a rem size override was pushed.
    pub pushed_rem_size: bool,
}

impl LayoutFrame {
    /// Create a LayoutFrame indicating the node handled layout.
    ///
    /// Use this for fiber-native nodes that fully handle their own layout.
    pub fn handled() -> Self {
        Self {
            handled: true,
            ..Default::default()
        }
    }
}

/// Result of updating a render node from a descriptor.
///
/// Returned by `Element::update_render_node` to indicate what changed.
/// The fiber system uses this to mark appropriate dirty flags.
#[derive(Clone, Copy, Debug, Default)]
pub struct UpdateResult {
    /// Whether layout-affecting properties changed.
    pub layout_changed: bool,
    /// Whether paint-affecting properties changed (requires prepaint + paint to re-run).
    pub paint_changed: bool,
}

impl UpdateResult {
    /// No changes occurred.
    pub const UNCHANGED: UpdateResult = UpdateResult {
        layout_changed: false,
        paint_changed: false,
    };

    /// Only paint changed.
    pub const PAINT_ONLY: UpdateResult = UpdateResult {
        layout_changed: false,
        paint_changed: true,
    };

    /// Layout changed (implies paint also changed).
    pub const LAYOUT_CHANGED: UpdateResult = UpdateResult {
        layout_changed: true,
        paint_changed: true,
    };

    /// Check if any change occurred.
    pub fn any_change(&self) -> bool {
        self.layout_changed || self.paint_changed
    }
}

/// A declarative child slot owned by a render node.
///
/// Conditional slots allow render nodes to express optional children that are driven by the node's
/// internal state (e.g. loading/error UI). Slots are reconciled into the main fiber tree so they
/// participate in normal layout and paint passes.
pub struct ConditionalSlot {
    /// Position index for this slot (0, 1, 2...).
    /// Used for positional matching within node_children.
    pub slot_index: u32,
    /// Whether the slot is currently active.
    pub active: bool,
    /// Factory invoked to construct the element when the slot is active.
    ///
    /// The factory is only called when the slot is active.
    pub element_factory: Option<Box<dyn FnOnce() -> AnyElement>>,
}

impl ConditionalSlot {
    /// Create an active slot with an element factory.
    pub fn active(slot_index: u32, element_factory: impl FnOnce() -> AnyElement + 'static) -> Self {
        Self {
            slot_index,
            active: true,
            element_factory: Some(Box::new(element_factory)),
        }
    }

    /// Create an inactive slot.
    pub fn inactive(slot_index: u32) -> Self {
        Self {
            slot_index,
            active: false,
            element_factory: None,
        }
    }
}

/// Context provided to RenderNode during prepaint.
pub struct PrepaintCtx<'a> {
    /// The fiber's global element ID.
    pub fiber_id: GlobalElementId,
    /// The computed bounds for this fiber.
    pub bounds: Bounds<Pixels>,
    /// Pre-computed bounds for each child fiber.
    /// Used by Anchored nodes to compute positioning offsets.
    pub child_bounds: Vec<Bounds<Pixels>>,
    /// Inspector element ID, if inspector is active.
    pub inspector_id: Option<InspectorElementId>,
    /// Window access.
    pub window: &'a mut Window,
    /// App context.
    pub cx: &'a mut App,
}

/// Context provided to RenderNode during paint.
pub struct PaintCtx<'a> {
    /// The fiber's global element ID.
    pub fiber_id: GlobalElementId,
    /// The computed bounds for this fiber.
    pub bounds: Bounds<Pixels>,
    /// Pre-computed bounds for each child fiber.
    /// Used by Anchored nodes to compute positioning offsets.
    pub child_bounds: Vec<Bounds<Pixels>>,
    /// Inspector element ID, if inspector is active.
    pub inspector_id: Option<InspectorElementId>,
    /// Window access.
    pub window: &'a mut Window,
    /// App context.
    pub cx: &'a mut App,
}

impl PrepaintCtx<'_> {
    /// Insert a hitbox for this fiber.
    ///
    /// Hitboxes define interactive regions that receive mouse events.
    /// The returned `Hitbox` should be stored by the render node and passed
    /// to event registration methods during `paint_begin`.
    pub fn insert_hitbox(&mut self, bounds: Bounds<Pixels>, behavior: HitboxBehavior) -> Hitbox {
        self.window
            .insert_hitbox_with_fiber(bounds, behavior, self.fiber_id)
    }

    /// Push a content mask for clipping.
    ///
    /// Content masks define clipping regions for child rendering. The mask will be
    /// intersected with any existing masks in the stack.
    ///
    /// **Important**: After calling this, you must set `frame.pushed_content_mask = true`
    /// in your `PrepaintFrame` return value so the fiber system knows to pop the mask
    /// during `prepaint_end`.
    ///
    /// # Example
    ///
    /// ```ignore
    /// fn prepaint_begin(&mut self, ctx: &mut PrepaintCtx) -> PrepaintFrame {
    ///     ctx.push_content_mask(ContentMask { bounds: ctx.bounds });
    ///     PrepaintFrame {
    ///         handled: true,
    ///         pushed_content_mask: true,
    ///         ..Default::default()
    ///     }
    /// }
    /// ```
    pub fn push_content_mask(&mut self, mask: ContentMask<Pixels>) {
        let world_mask = self.window.transform_mask_to_world(mask);
        let intersected = world_mask.intersect(&self.window.content_mask());
        self.window.content_mask_stack.push(intersected);
    }

    /// Get the current content mask.
    ///
    /// Returns the intersection of all content masks currently on the stack,
    /// or a mask covering the entire viewport if the stack is empty.
    pub fn content_mask(&self) -> ContentMask<Pixels> {
        self.window.content_mask()
    }
}

impl PaintCtx<'_> {
    /// Register a mouse down listener for this fiber.
    ///
    /// This is the efficient typed listener path - no runtime type checking is performed.
    /// The listener receives the event, dispatch phase, hitbox (from dispatch), window, and app.
    ///
    /// The hitbox passed to the listener is determined by the event dispatch system based on
    /// which hitbox was hit. Ensure you've registered a hitbox during prepaint using
    /// `PrepaintCtx::insert_hitbox`.
    pub fn on_mouse_down(
        &mut self,
        listener: impl Fn(&MouseDownEvent, DispatchPhase, &Hitbox, &mut Window, &mut App) + 'static,
    ) {
        if let Some(effects) = self.window.register_fiber_effects(&self.fiber_id) {
            effects.mouse_down_listeners.push(Rc::new(listener));
        }
    }

    /// Register a mouse up listener for this fiber.
    ///
    /// This is the efficient typed listener path - no runtime type checking is performed.
    pub fn on_mouse_up(
        &mut self,
        listener: impl Fn(&MouseUpEvent, DispatchPhase, &Hitbox, &mut Window, &mut App) + 'static,
    ) {
        if let Some(effects) = self.window.register_fiber_effects(&self.fiber_id) {
            effects.mouse_up_listeners.push(Rc::new(listener));
        }
    }

    /// Register a mouse move listener for this fiber.
    ///
    /// This is the efficient typed listener path - no runtime type checking is performed.
    pub fn on_mouse_move(
        &mut self,
        listener: impl Fn(&MouseMoveEvent, DispatchPhase, &Hitbox, &mut Window, &mut App) + 'static,
    ) {
        if let Some(effects) = self.window.register_fiber_effects(&self.fiber_id) {
            effects.mouse_move_listeners.push(Rc::new(listener));
        }
    }

    /// Register a scroll wheel listener for this fiber.
    ///
    /// This is the efficient typed listener path - no runtime type checking is performed.
    pub fn on_scroll_wheel(
        &mut self,
        listener: impl Fn(&ScrollWheelEvent, DispatchPhase, &Hitbox, &mut Window, &mut App) + 'static,
    ) {
        if let Some(effects) = self.window.register_fiber_effects(&self.fiber_id) {
            effects.scroll_wheel_listeners.push(Rc::new(listener));
        }
    }

    /// Register a click listener for this fiber.
    ///
    /// Click events are synthesized from mouse down/up sequences.
    /// This is the efficient typed listener path - no runtime type checking is performed.
    pub fn on_click(
        &mut self,
        listener: impl Fn(&ClickEvent, &mut Window, &mut App) + 'static,
    ) {
        if let Some(effects) = self.window.register_fiber_effects(&self.fiber_id) {
            effects.click_listeners.push(Rc::new(listener));
        }
    }

    /// Set the cursor style for this fiber's hitbox.
    ///
    /// The cursor will be displayed when the mouse is over the fiber's hitbox.
    pub fn set_cursor_style(&mut self, style: CursorStyle) {
        if let Some(effects) = self.window.register_fiber_effects(&self.fiber_id) {
            effects.cursor_style = Some(style);
        }
    }

    /// Set a tooltip for this fiber.
    ///
    /// The tooltip will be displayed when the mouse hovers over the fiber's hitbox.
    /// Returns a `TooltipId` that can be used to identify this tooltip.
    pub fn set_tooltip(&mut self, tooltip: AnyTooltip) -> TooltipId {
        self.window
            .with_fiber_cx_for(self.fiber_id, |cx| cx.set_tooltip(tooltip))
    }

    /// Register a focus handle for this fiber.
    ///
    /// This associates the focus handle with the fiber, allowing the focus system
    /// to track which fiber owns the focus handle for proper dispatch ordering.
    pub fn register_focus_handle(&mut self, handle: &FocusHandle) {
        self.window
            .with_fiber_cx_for(self.fiber_id, |cx| cx.set_focus_handle(handle));
    }

    /// Register a key context for this fiber.
    ///
    /// Key contexts are used to scope keybindings. When this fiber is in the focus
    /// path, its key context will be included in action dispatch.
    pub fn register_key_context(&mut self, context: KeyContext) {
        self.window
            .with_fiber_cx_for(self.fiber_id, |cx| cx.set_key_context(context));
    }

    /// Register an input handler for this fiber.
    ///
    /// This associates the input handler with the focus handle and bounds,
    /// allowing text input and IME events to be routed to this element.
    pub fn register_input_handler(
        &mut self,
        focus_handle: &FocusHandle,
        input_handler: impl InputHandler,
    ) {
        self.window
            .with_fiber_cx_for(self.fiber_id, |cx| {
                cx.handle_input(focus_handle, input_handler, self.cx)
            });
    }

    /// Push a content mask for clipping during paint.
    ///
    /// **Important**: After calling this, you must set `frame.pushed_content_mask = true`
    /// in your `PaintFrame` return value so the fiber system knows to pop the mask
    /// during `paint_end`.
    pub fn push_content_mask(&mut self, mask: ContentMask<Pixels>) {
        let world_mask = self.window.transform_mask_to_world(mask);
        let intersected = world_mask.intersect(&self.window.content_mask());
        self.window.content_mask_stack.push(intersected);
    }

    /// Get the current content mask.
    pub fn content_mask(&self) -> ContentMask<Pixels> {
        self.window.content_mask()
    }
}

/// Frame state returned by `prepaint_begin`.
///
/// This struct tracks what context was pushed during prepaint_begin
/// so that prepaint_end can pop it in reverse order. It also tracks
/// whether children should be skipped (e.g., for display: none).
#[derive(Clone, Default)]
pub struct PrepaintFrame {
    /// Whether the node fully handled the prepaint phase.
    /// When true, the legacy VKind-specific prepaint code is skipped.
    pub handled: bool,

    /// Skip processing children (e.g., display: none).
    pub skip_children: bool,

    /// Whether a text style was pushed.
    pub pushed_text_style: bool,
    /// Whether a content mask was pushed.
    pub pushed_content_mask: bool,
    /// Whether an element offset was pushed.
    pub pushed_element_offset: bool,
    /// Whether a transform context was pushed.
    pub pushed_transform: bool,
    /// Whether an image cache was pushed.
    pub pushed_image_cache: bool,
    /// Whether opacity was pushed.
    pub pushed_opacity: bool,
    /// Whether a tab group was pushed.
    pub pushed_tab_group: bool,
    /// Whether a rem size override was pushed.
    pub pushed_rem_size: bool,

    /// Hitbox registered during prepaint.
    pub hitbox: Option<Hitbox>,
}

impl PrepaintFrame {
    /// Create a PrepaintFrame indicating the node handled prepaint.
    ///
    /// Use this for fiber-native nodes that fully handle their own prepaint.
    pub fn handled() -> Self {
        Self {
            handled: true,
            ..Default::default()
        }
    }
}

/// Frame state returned by `paint_begin`.
///
/// Similar to PrepaintFrame, this tracks pushed context for proper cleanup
/// in paint_end.
#[derive(Clone, Default)]
pub struct PaintFrame {
    /// Whether the node fully handled the paint phase.
    /// When true, the legacy VKind-specific paint code is skipped.
    pub handled: bool,

    /// Skip processing children.
    pub skip_children: bool,

    /// Whether a text style was pushed.
    pub pushed_text_style: bool,
    /// Whether a content mask was pushed.
    pub pushed_content_mask: bool,
    /// Whether an element offset was pushed.
    pub pushed_element_offset: bool,
    /// Whether a transform context was pushed.
    pub pushed_transform: bool,
    /// Whether an image cache was pushed.
    pub pushed_image_cache: bool,
    /// Previous opacity value to restore (if opacity was changed).
    pub previous_opacity: Option<f32>,
    /// Whether a tab group was pushed.
    pub pushed_tab_group: bool,
    /// Whether a rem size override was pushed.
    pub pushed_rem_size: bool,

    /// Group hitbox context pushed during paint (if any).
    pub pushed_group_hitbox: Option<SharedString>,
}

impl PaintFrame {
    /// Create a PaintFrame indicating the node handled paint.
    ///
    /// Use this for fiber-native nodes that fully handle their own paint.
    pub fn handled() -> Self {
        Self {
            handled: true,
            ..Default::default()
        }
    }
}

/// A retained render node that persists in the fiber tree.
///
/// RenderNodes own all element-specific state and implement the
/// scope-based prepaint/paint lifecycle. They are created by elements
/// during reconciliation and updated on subsequent frames.
///
/// # Scope Discipline
///
/// Wrapper nodes (like Div) must use the begin/end pattern to ensure
/// children are painted within the correct context (masks, offsets, etc.):
///
/// ```ignore
/// fn prepaint_begin(&mut self, ctx: &mut PrepaintCtx) -> PrepaintFrame {
///     // Push context: text style, masks, offsets, etc.
///     PrepaintFrame { pushed_text_style: true, ... }
/// }
///
/// // Traversal calls children's prepaint here
///
/// fn prepaint_end(&mut self, ctx: &mut PrepaintCtx, frame: PrepaintFrame) {
///     // Pop context in reverse order
/// }
/// ```
pub trait RenderNode: 'static + AsAny {
    /// Return a debug-friendly type name for this render node.
    ///
    /// Used in diagnostic logging to identify what kind of element a fiber represents.
    fn debug_type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    /// Return the Taffy style for layout computation.
    ///
    /// Called by the layout traversal to get this node's layout constraints.
    /// The rem_size and scale_factor are provided for unit conversion.
    ///
    /// Defaults to `TaffyStyle::default()`. Override for elements with styling.
    fn taffy_style(&self, _rem_size: Pixels, _scale_factor: f32) -> TaffyStyle {
        TaffyStyle::default()
    }

    /// Whether this node needs per-child bounds to be computed and provided via `PrepaintCtx` and
    /// `PaintCtx`.
    ///
    /// Defaults to `true` to preserve legacy behavior. Nodes that never inspect `ctx.child_bounds`
    /// should override this to `false` so the fiber renderer can avoid O(n) child-bound collection
    /// work for large containers.
    fn needs_child_bounds(&self) -> bool {
        true
    }

    /// Begin layout scope.
    ///
    /// Called during layout setup traversal, before processing children.
    /// Use this to:
    /// - Push inherited context (text style, image cache)
    /// - Prepare any state needed for layout
    ///
    /// Returns a LayoutFrame indicating what context was pushed.
    fn layout_begin(&mut self, _ctx: &mut LayoutCtx) -> LayoutFrame {
        LayoutFrame::default()
    }

    /// End layout scope.
    ///
    /// Called after processing children. Pop any context that was pushed
    /// in layout_begin (in reverse order).
    fn layout_end(&mut self, _ctx: &mut LayoutCtx, _frame: LayoutFrame) {}

    /// Measure leaf content (text, images). Called during layout.
    ///
    /// Return `Some(size)` for leaf nodes that measure their own content.
    /// Return `None` for container nodes that don't have intrinsic size.
    fn measure(
        &mut self,
        _known: Size<Option<Pixels>>,
        _available: Size<AvailableSpace>,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<Size<Pixels>> {
        None
    }

    /// Compute this element's intrinsic size.
    ///
    /// Called during the intrinsic sizing pass. Must return the element's
    /// min-content and max-content sizes based on its current content and styles.
    ///
    /// The returned `SizingInput` is used to determine cache validity - if it matches the cached
    /// input, the cached size can be reused.
    ///
    /// Defaults to returning zero intrinsic size. Override for elements that have
    /// content-based sizing (images, text, etc.).
    fn compute_intrinsic_size(&mut self, _ctx: &mut SizingCtx) -> IntrinsicSizeResult {
        IntrinsicSizeResult {
            size: IntrinsicSize::default(),
            input: SizingInput::default(),
        }
    }

    /// Whether this node's intrinsic size can be cached and used to answer Taffy measure queries.
    ///
    /// Nodes that require `measure()` to run with the exact Taffy constraints (e.g. because it has
    /// side effects or performs nested fiber layout) should return `false` to force the slow path.
    fn uses_intrinsic_sizing_cache(&self) -> bool {
        true
    }

    /// Answer a size query using cached intrinsic size.
    ///
    /// Called during layout when taffy needs to measure this element.
    /// Should use the cached intrinsic size computed in `compute_intrinsic_size`.
    ///
    /// For most elements, this just returns min_content or max_content.
    /// For elements with height-for-width (like wrapping text), this may
    /// need additional computation.
    fn resolve_size_query(
        &mut self,
        query: SizeQuery,
        cached: &IntrinsicSize,
        _ctx: &mut SizingCtx,
    ) -> Size<Pixels> {
        match query {
            SizeQuery::MinContent => cached.min_content,
            SizeQuery::MaxContent => cached.max_content,
            SizeQuery::ForWidth(width) => Size {
                width: width.min(cached.max_content.width),
                height: cached.max_content.height,
            },
            SizeQuery::ForHeight(height) => Size {
                width: cached.max_content.width,
                height: height.min(cached.max_content.height),
            },
            SizeQuery::Definite(size) => Size {
                width: size.width.min(cached.max_content.width),
                height: size.height.min(cached.max_content.height),
            },
        }
    }

    /// Check if this element is a "layout boundary".
    ///
    /// Layout boundaries have fixed intrinsic size regardless of children.
    /// Child size changes don't propagate past a layout boundary.
    fn is_layout_boundary(&self) -> bool {
        false
    }

    /// Begin prepaint scope.
    ///
    /// Called before processing children. Push any context (text style,
    /// content mask, element offset, etc.) and return a frame indicating
    /// what was pushed and whether to skip children.
    ///
    /// The default implementation returns `PrepaintFrame::default()` with
    /// `handled: false`, which triggers legacy fallback behavior. Fiber-native
    /// nodes should override this and return `PrepaintFrame::handled()` or
    /// `PrepaintFrame { handled: true, ..Default::default() }`.
    fn prepaint_begin(&mut self, _ctx: &mut PrepaintCtx) -> PrepaintFrame {
        PrepaintFrame::default()
    }

    /// End prepaint scope.
    ///
    /// Called after processing children. Pop any context that was pushed
    /// in prepaint_begin (in reverse order).
    ///
    /// The default implementation does nothing.
    fn prepaint_end(&mut self, _ctx: &mut PrepaintCtx, _frame: PrepaintFrame) {}

    /// Begin paint scope.
    ///
    /// Called before processing children. Paint background/shadows, push
    /// any context, and return a frame indicating what was pushed.
    ///
    /// The default implementation returns `PaintFrame::default()` with
    /// `handled: false`, which triggers legacy fallback behavior. Fiber-native
    /// nodes should override this and return `PaintFrame::handled()` or
    /// `PaintFrame { handled: true, ..Default::default() }`.
    fn paint_begin(&mut self, _ctx: &mut PaintCtx) -> PaintFrame {
        PaintFrame::default()
    }

    /// End paint scope.
    ///
    /// Called after processing children. Paint borders/foreground, pop
    /// any context that was pushed in paint_begin.
    ///
    /// The default implementation does nothing.
    fn paint_end(&mut self, _ctx: &mut PaintCtx, _frame: PaintFrame) {}

    /// Whether this node requires an "after" scene segment.
    ///
    /// When `true`, the fiber renderer will ensure an additional persistent scene segment exists
    /// for this fiber and will invoke `paint_end` with that segment as the active segment. This
    /// allows any primitives emitted by `paint_end` to be ordered after the node's children in the
    /// final scene (e.g. borders drawn over children).
    ///
    /// Most leaf nodes should return `false` (the default).
    fn needs_after_segment(&self) -> bool {
        false
    }

    /// Returns conditional child slots driven by this node's internal state.
    ///
    /// Slots are reconciled into the fiber tree during layout so they participate in normal
    /// layout/prepaint/paint passes.
    fn conditional_slots(&mut self, _fiber_id: GlobalElementId) -> SmallVec<[ConditionalSlot; 4]> {
        SmallVec::new()
    }

    /// Return the focus handle if this node is focusable.
    ///
    /// This is a node capability that allows generic code to query focus
    /// handles without downcasting to specific node types.
    fn focus_handle(&self) -> Option<FocusHandle> {
        self.interactivity()
            .and_then(|interactivity| interactivity.tracked_focus_handle.clone())
    }

    /// Return the interactivity state if this node has it.
    ///
    /// This is a node capability that allows generic code to query
    /// interactivity metadata (hover styles, group membership, etc.)
    /// without downcasting to specific node types.
    fn interactivity(&self) -> Option<&Interactivity> {
        None
    }

    /// The concrete node type id.
    ///
    /// Used by reconciliation to decide whether an existing node can be
    /// updated in-place, or whether it must be replaced.
    fn node_type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
}

// Intentionally no downcasting API: retained nodes are meant to be interacted
// with through explicit trait capabilities rather than type-specific matches.

/// A null node that does nothing.
///
/// Used as a placeholder when no real node is needed.
#[allow(dead_code)]
pub struct NullNode;

impl RenderNode for NullNode {}

/// A legacy node for elements that don't support retained rendering.
///
/// This node is always considered dirty and doesn't participate in
/// incremental caching. It's used as a fallback for third-party elements
/// that don't implement the retained node API.
#[allow(dead_code)]
pub struct LegacyNode;

impl RenderNode for LegacyNode {}

/// Render node for the Empty element.
///
/// This node renders nothing and takes up no space (display: none).
pub struct EmptyNode;

impl RenderNode for EmptyNode {
    fn taffy_style(&self, _rem_size: Pixels, _scale_factor: f32) -> taffy::Style {
        taffy::Style {
            display: taffy::Display::None,
            ..Default::default()
        }
    }
}

/// A slot for sharing callbacks between ephemeral elements and retained nodes.
///
/// Elements (ephemeral) own callbacks like render functions, but nodes (retained)
/// need to invoke them during prepaint/paint. CallbackSlot bridges this gap:
///
/// 1. Element deposits its callback during `update_render_node`
/// 2. Node uses the callback during prepaint/paint via `with()`
/// 3. Callback is available for the duration of the frame
///
/// # Example
///
/// ```ignore
/// struct MyNode {
///     render_callback: CallbackSlot<dyn Fn(usize) -> AnyElement>,
/// }
///
/// impl Element for MyElement {
///     fn update_render_node(&mut self, node: &mut dyn RenderNode, ...) -> Option<UpdateResult> {
///         let node = node.as_any_mut().downcast_mut::<MyNode>()?;
///         node.render_callback.deposit(std::mem::take(&mut self.render_fn));
///         Some(UpdateResult::UNCHANGED)
///     }
/// }
///
/// impl RenderNode for MyNode {
///     fn prepaint_begin(&mut self, ctx: &mut PrepaintCtx) -> PrepaintFrame {
///         self.render_callback.with(|render| {
///             let element = render(0);
///             // use element...
///         });
///         PrepaintFrame { handled: true, ..Default::default() }
///     }
/// }
/// ```
pub struct CallbackSlot<F: ?Sized> {
    callback: std::cell::RefCell<Option<Box<F>>>,
}

impl<F: ?Sized> CallbackSlot<F> {
    /// Create an empty callback slot.
    pub fn new() -> Self {
        Self {
            callback: std::cell::RefCell::new(None),
        }
    }

    /// Deposit a callback into the slot.
    ///
    /// This takes ownership of the callback. The previous callback (if any)
    /// is replaced.
    pub fn deposit(&self, f: Box<F>) {
        *self.callback.borrow_mut() = Some(f);
    }

    /// Borrow the callback for use.
    ///
    /// Returns `None` if no callback has been deposited.
    /// The callback remains in the slot after this call.
    pub fn with<R>(&self, f: impl FnOnce(&F) -> R) -> Option<R> {
        self.callback.borrow().as_ref().map(|cb| f(cb.as_ref()))
    }

    /// Borrow the callback mutably for use.
    ///
    /// Returns `None` if no callback has been deposited.
    /// Use this for `FnMut` callbacks that need mutable access.
    pub fn with_mut<R>(&self, f: impl FnOnce(&mut F) -> R) -> Option<R> {
        self.callback.borrow_mut().as_mut().map(|cb| f(cb.as_mut()))
    }

    /// Check if a callback is currently deposited.
    pub fn has_callback(&self) -> bool {
        self.callback.borrow().is_some()
    }

    /// Take the callback out of the slot, leaving it empty.
    ///
    /// This is useful if the element needs to reclaim its callback.
    pub fn take(&self) -> Option<Box<F>> {
        self.callback.borrow_mut().take()
    }

    /// Clear the slot without returning the callback.
    #[allow(dead_code)]
    pub fn clear(&self) {
        *self.callback.borrow_mut() = None;
    }
}

impl<F: ?Sized> Default for CallbackSlot<F> {
    fn default() -> Self {
        Self::new()
    }
}

impl<F: ?Sized> std::fmt::Debug for CallbackSlot<F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CallbackSlot")
            .field("has_callback", &self.has_callback())
            .finish()
    }
}
