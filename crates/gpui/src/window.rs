#[cfg(any(feature = "inspector", debug_assertions))]
use crate::Inspector;
use crate::taffy::TaffyLayoutEngine;
use crate::fiber::StateStore;
use crate::{
    Action, AnyDrag, AnyElement, AnyImageCache, AnyTooltip, AnyView, App, AppContext, Asset,
    AsyncWindowContext, AvailableSpace, Background, BorderStyle, Bounds, BoxShadow, Capslock,
    ConstraintsBucket, Context, Corners, CursorStyle, Decorations, DevicePixels, DirtyFlags,
    DisplayId, Edges, Effect, Entity, EntityId, EventEmitter, FiberEffects, FileDropEvent, FontId,
    Global, GlobalElementId, GlyphId, GpuSpecs, Hsla, InputHandler, IsZero, KeyBinding, KeyContext,
    KeyDispatcher, KeyDownEvent, KeyEvent, Keystroke, KeystrokeEvent, LayoutId, MeasurementCache,
    MeasurementKey, Modifiers, ModifiersChangedEvent, MonochromeSprite, MouseButton, MouseEvent,
    MouseMoveEvent, MouseUpEvent, Path, Pixels, PlatformAtlas, PlatformDisplay, PlatformInput,
    PlatformWindow, Point, PolychromeSprite, Priority, PromptButton, PromptLevel, Quad, Render,
    RenderGlyphParams, RenderImage, RenderImageParams, RenderSvgParams, Replay, ResizeEdge,
    SMOOTH_SVG_SCALE_FACTOR, SUBPIXEL_VARIANTS_X, SUBPIXEL_VARIANTS_Y, ScaledPixels, Scene,
    SceneSegmentPool, Shadow, SharedString, Size, StrikethroughStyle, Style, SubpixelSprite,
    SubscriberSet, Subscription, SystemWindowTab, SystemWindowTabController, Task, TextRenderingMode,
    TextStyle, TextStyleRefinement, Transform2D, TransformId, TransformationMatrix, Underline,
    UnderlineStyle, WindowAppearance, WindowBackgroundAppearance, WindowBounds, WindowControls,
    WindowDecorations, WindowOptions, WindowParams, WindowTextSystem, div, point, prelude::*, px,
    rems, size, transparent_black,
};
use anyhow::{Context as _, Result, anyhow};
use collections::{FxHashMap, FxHashSet, FxHasher};
#[cfg(target_os = "macos")]
use core_video::pixel_buffer::CVPixelBuffer;
use derive_more::{Deref, DerefMut};
use futures::FutureExt;
use futures::channel::oneshot;
use parking_lot::RwLock;
use raw_window_handle::{HandleError, HasDisplayHandle, HasWindowHandle};
use refineable::Refineable;
use slotmap::{DefaultKey, SlotMap};
use smallvec::SmallVec;
use std::{
    any::{Any, TypeId},
    borrow::Cow,
    cell::{Cell, RefCell},
    cmp,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem,
    ops::DerefMut,
    rc::Rc,
    sync::{
        Arc, Weak,
        atomic::{AtomicUsize, Ordering::SeqCst},
    },
    time::{Duration, Instant},
};
use taffy::tree::NodeId;
use util::{ResultExt, measure};
use uuid::Uuid;

pub(crate) mod context;
mod prompts;

use crate::fiber::FiberRuntime;
#[cfg(debug_assertions)]
use crate::fiber::debug_assert_active_list_matches_map;
pub(crate) use crate::fiber::{FiberRuntimeHandle, FiberRuntimeHandleRef, has_mouse_effects};
use crate::util::atomic_incr_if_not_zero;
pub use prompts::*;

pub(crate) const DEFAULT_WINDOW_SIZE: Size<Pixels> = size(px(1536.), px(864.));

/// A 6:5 aspect ratio minimum window size to be used for functional,
/// additional-to-main-Zed windows, like the settings and rules library windows.
pub const DEFAULT_ADDITIONAL_WINDOW_SIZE: Size<Pixels> = Size {
    width: Pixels(900.),
    height: Pixels(750.),
};

/// Represents the two different phases when dispatching events.
#[derive(Default, Copy, Clone, Debug, Eq, PartialEq)]
pub enum DispatchPhase {
    /// After the capture phase comes the bubble phase, in which mouse event listeners are
    /// invoked front to back and keyboard event listeners are invoked from the focused element
    /// to the root of the element tree. This is the phase you'll most commonly want to use when
    /// registering event listeners.
    #[default]
    Bubble,
    /// During the initial capture phase, mouse event listeners are invoked back to front, and keyboard
    /// listeners are invoked from the root of the tree downward toward the focused element. This phase
    /// is used for special purposes such as clearing the "pressed" state for click events. If
    /// you stop event propagation during this phase, you need to know what you're doing. Handlers
    /// outside of the immediate region may rely on detecting non-local events during this phase.
    Capture,
}

impl DispatchPhase {
    /// Returns true if this represents the "bubble" phase.
    #[inline]
    pub fn bubble(self) -> bool {
        self == DispatchPhase::Bubble
    }

    /// Returns true if this represents the "capture" phase.
    #[inline]
    pub fn capture(self) -> bool {
        self == DispatchPhase::Capture
    }
}

struct WindowInvalidatorInner {
    pub dirty: bool,
    pub draw_phase: DrawPhase,
    pub dirty_views: FxHashSet<EntityId>,
}

#[derive(Clone)]
pub(crate) struct WindowInvalidator {
    inner: Rc<RefCell<WindowInvalidatorInner>>,
}

impl WindowInvalidator {
    pub fn new() -> Self {
        WindowInvalidator {
            inner: Rc::new(RefCell::new(WindowInvalidatorInner {
                dirty: true,
                draw_phase: DrawPhase::None,
                dirty_views: FxHashSet::default(),
            })),
        }
    }

    pub fn invalidate_view(&self, entity: EntityId, cx: &mut App) -> bool {
        let mut inner = self.inner.borrow_mut();
        inner.dirty_views.insert(entity);
        inner.dirty = true;
        if matches!(inner.draw_phase, DrawPhase::None | DrawPhase::Event) {
            cx.push_effect(Effect::Notify { emitter: entity });
            true
        } else {
            false
        }
    }

    pub fn mark_dirty(&self, entity: EntityId, cx: &mut App) -> bool {
        let mut inner = self.inner.borrow_mut();
        inner.dirty = true;
        if matches!(inner.draw_phase, DrawPhase::None | DrawPhase::Event) {
            cx.push_effect(Effect::Notify { emitter: entity });
            true
        } else {
            false
        }
    }

    pub fn is_dirty(&self) -> bool {
        self.inner.borrow().dirty
    }

    pub fn set_dirty(&self, dirty: bool) {
        self.inner.borrow_mut().dirty = dirty
    }

    pub fn set_phase(&self, phase: DrawPhase) {
        self.inner.borrow_mut().draw_phase = phase
    }

    pub fn phase(&self) -> DrawPhase {
        self.inner.borrow().draw_phase
    }

    pub fn take_views(&self) -> FxHashSet<EntityId> {
        mem::take(&mut self.inner.borrow_mut().dirty_views)
    }

    pub fn replace_views(&self, views: FxHashSet<EntityId>) {
        self.inner.borrow_mut().dirty_views = views;
    }

    pub fn not_drawing(&self) -> bool {
        matches!(
            self.inner.borrow().draw_phase,
            DrawPhase::None | DrawPhase::Event
        )
    }

    #[track_caller]
    pub fn debug_assert_paint(&self) {
        debug_assert!(
            matches!(self.inner.borrow().draw_phase, DrawPhase::Paint),
            "this method can only be called during paint"
        );
    }

    #[track_caller]
    pub fn debug_assert_layout_or_prepaint(&self) {
        debug_assert!(
            matches!(
                self.inner.borrow().draw_phase,
                DrawPhase::Layout | DrawPhase::Prepaint
            ),
            "this method can only be called during layout or prepaint"
        );
    }

    #[track_caller]
    pub fn debug_assert_prepaint_or_paint(&self) {
        debug_assert!(
            matches!(
                self.inner.borrow().draw_phase,
                DrawPhase::Paint | DrawPhase::Prepaint
            ),
            "this method can only be called during prepaint or paint"
        );
    }

    #[track_caller]
    pub fn debug_assert_layout_or_prepaint_or_paint(&self) {
        debug_assert!(
            matches!(
                self.inner.borrow().draw_phase,
                DrawPhase::Layout | DrawPhase::Prepaint | DrawPhase::Paint
            ),
            "this method can only be called during layout, prepaint, or paint"
        );
    }

    #[track_caller]
    pub fn debug_assert_reconcile_or_layout_or_prepaint_or_paint(&self) {
        debug_assert!(
            matches!(
                self.inner.borrow().draw_phase,
                DrawPhase::Reconcile | DrawPhase::Layout | DrawPhase::Prepaint | DrawPhase::Paint
            ),
            "this method can only be called during reconcile, layout, prepaint, or paint"
        );
    }
}

type AnyObserver = Box<dyn FnMut(&mut Window, &mut App) -> bool + 'static>;

#[derive(Default)]
pub(crate) struct GlobalIdScopeState {
    pub(crate) children: FxHashMap<ElementId, GlobalElementId>,
}

pub(crate) type AnyWindowFocusListener =
    Box<dyn FnMut(&WindowFocusEvent, &mut Window, &mut App) -> bool + 'static>;

pub(crate) struct WindowFocusEvent {
    pub(crate) previous_focus_path: SmallVec<[FocusId; 8]>,
    pub(crate) current_focus_path: SmallVec<[FocusId; 8]>,
}

impl WindowFocusEvent {
    pub fn is_focus_in(&self, focus_id: FocusId) -> bool {
        !self.previous_focus_path.contains(&focus_id) && self.current_focus_path.contains(&focus_id)
    }

    pub fn is_focus_out(&self, focus_id: FocusId) -> bool {
        self.previous_focus_path.contains(&focus_id) && !self.current_focus_path.contains(&focus_id)
    }
}

fn snapshot_hitboxes_into_map(
    fiber_tree: &crate::fiber::FiberTree,
    map: &mut FxHashMap<HitboxId, HitboxSnapshot>,
) {
    map.clear();
    let Some(root) = fiber_tree.root else {
        return;
    };
    let mut stack: Vec<GlobalElementId> = vec![root];
    while let Some(fiber_id) = stack.pop() {
        let children: SmallVec<[GlobalElementId; 8]> = fiber_tree.children(&fiber_id).collect();
        for child in children {
            stack.push(child);
        }
        let Some(data) = fiber_tree
            .hitbox_state
            .get(fiber_id.into())
            .and_then(|state| state.hitbox.as_ref())
        else {
            continue;
        };

        map.insert(
            fiber_id.into(),
            HitboxSnapshot {
                transform_id: data.transform_id,
                bounds: data.bounds,
                content_mask: data.content_mask.clone(),
                behavior: data.behavior,
            },
        );
    }
}

fn resolve_hitbox_for_hit_test(
    window: &Window,
    fiber_id: &GlobalElementId,
) -> Option<HitboxSnapshot> {
    let data = window
        .fiber
        .tree
        .hitbox_state
        .get((*fiber_id).into())
        .and_then(|state| state.hitbox.as_ref())?;

    Some(HitboxSnapshot {
        transform_id: data.transform_id,
        bounds: data.bounds,
        content_mask: data.content_mask.clone(),
        behavior: data.behavior,
    })
}

/// This is provided when subscribing for `Context::on_focus_out` events.
pub struct FocusOutEvent {
    /// A weak focus handle representing what was blurred.
    pub blurred: WeakFocusHandle,
}

slotmap::new_key_type! {
    /// A globally unique identifier for a focusable element.
    pub struct FocusId;
}

pub(crate) type FocusMap = RwLock<SlotMap<FocusId, FocusRef>>;
pub(crate) struct FocusRef {
    pub(crate) ref_count: AtomicUsize,
    pub(crate) tab_index: isize,
    pub(crate) tab_stop: bool,
}

impl FocusId {
    /// Obtains whether the element associated with this handle is currently focused.
    pub fn is_focused(&self, window: &Window) -> bool {
        window.focus == Some(*self)
    }

    /// Obtains whether the element associated with this handle contains the focused
    /// element or is itself focused.
    pub fn contains_focused(&self, window: &Window, cx: &App) -> bool {
        window
            .focused(cx)
            .is_some_and(|focused| self.contains(focused.id, window))
    }

    /// Obtains whether the element associated with this handle is contained within the
    /// focused element or is itself focused.
    pub fn within_focused(&self, window: &Window, cx: &App) -> bool {
        let focused = window.focused(cx);
        focused.is_some_and(|focused| focused.id.contains(*self, window))
    }

    /// Obtains whether this handle contains the given handle in the most recently rendered frame.
    pub(crate) fn contains(&self, other: Self, window: &Window) -> bool {
        window.focus_contains(*self, other)
    }
}

/// A handle which can be used to track and manipulate the focused element in a window.
pub struct FocusHandle {
    pub(crate) id: FocusId,
    handles: Arc<FocusMap>,
    /// The index of this element in the tab order.
    pub tab_index: isize,
    /// Whether this element can be focused by tab navigation.
    pub tab_stop: bool,
}

impl std::fmt::Debug for FocusHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("FocusHandle({:?})", self.id))
    }
}

impl FocusHandle {
    pub(crate) fn new(handles: &Arc<FocusMap>) -> Self {
        let id = handles.write().insert(FocusRef {
            ref_count: AtomicUsize::new(1),
            tab_index: 0,
            tab_stop: false,
        });

        Self {
            id,
            tab_index: 0,
            tab_stop: false,
            handles: handles.clone(),
        }
    }

    pub(crate) fn for_id(id: FocusId, handles: &Arc<FocusMap>) -> Option<Self> {
        let lock = handles.read();
        let focus = lock.get(id)?;
        if atomic_incr_if_not_zero(&focus.ref_count) == 0 {
            return None;
        }
        Some(Self {
            id,
            tab_index: focus.tab_index,
            tab_stop: focus.tab_stop,
            handles: handles.clone(),
        })
    }

    /// Sets the tab index of the element associated with this handle.
    pub fn tab_index(mut self, index: isize) -> Self {
        self.tab_index = index;
        if let Some(focus) = self.handles.write().get_mut(self.id) {
            focus.tab_index = index;
        }
        self
    }

    /// Sets whether the element associated with this handle is a tab stop.
    ///
    /// When `false`, the element will not be included in the tab order.
    pub fn tab_stop(mut self, tab_stop: bool) -> Self {
        self.tab_stop = tab_stop;
        if let Some(focus) = self.handles.write().get_mut(self.id) {
            focus.tab_stop = tab_stop;
        }
        self
    }

    /// Converts this focus handle into a weak variant, which does not prevent it from being released.
    pub fn downgrade(&self) -> WeakFocusHandle {
        WeakFocusHandle {
            id: self.id,
            handles: Arc::downgrade(&self.handles),
        }
    }

    /// Moves the focus to the element associated with this handle.
    pub fn focus(&self, window: &mut Window, cx: &mut App) {
        window.focus(self, cx)
    }

    /// Obtains whether the element associated with this handle is currently focused.
    pub fn is_focused(&self, window: &Window) -> bool {
        self.id.is_focused(window)
    }

    /// Obtains whether the element associated with this handle contains the focused
    /// element or is itself focused.
    pub fn contains_focused(&self, window: &Window, cx: &App) -> bool {
        self.id.contains_focused(window, cx)
    }

    /// Obtains whether the element associated with this handle is contained within the
    /// focused element or is itself focused.
    pub fn within_focused(&self, window: &Window, cx: &mut App) -> bool {
        self.id.within_focused(window, cx)
    }

    /// Obtains whether this handle contains the given handle in the most recently rendered frame.
    pub fn contains(&self, other: &Self, window: &Window) -> bool {
        self.id.contains(other.id, window)
    }

    /// Dispatch an action on the element that rendered this focus handle
    pub fn dispatch_action(&self, action: &dyn Action, window: &mut Window, cx: &mut App) {
        if let Some(fiber_id) = window.fiber.tree.focusable_fibers.get(&self.id).copied() {
            window.dispatch_action_on_node(fiber_id, action, cx)
        }
    }
}

impl Clone for FocusHandle {
    fn clone(&self) -> Self {
        Self::for_id(self.id, &self.handles).unwrap()
    }
}

impl PartialEq for FocusHandle {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for FocusHandle {}

impl Drop for FocusHandle {
    fn drop(&mut self) {
        self.handles
            .read()
            .get(self.id)
            .unwrap()
            .ref_count
            .fetch_sub(1, SeqCst);
    }
}

/// A weak reference to a focus handle.
#[derive(Clone, Debug)]
pub struct WeakFocusHandle {
    pub(crate) id: FocusId,
    pub(crate) handles: Weak<FocusMap>,
}

impl WeakFocusHandle {
    /// Attempts to upgrade the [WeakFocusHandle] to a [FocusHandle].
    pub fn upgrade(&self) -> Option<FocusHandle> {
        let handles = self.handles.upgrade()?;
        FocusHandle::for_id(self.id, &handles)
    }
}

impl PartialEq for WeakFocusHandle {
    fn eq(&self, other: &WeakFocusHandle) -> bool {
        self.id == other.id
    }
}

impl Eq for WeakFocusHandle {}

impl PartialEq<FocusHandle> for WeakFocusHandle {
    fn eq(&self, other: &FocusHandle) -> bool {
        self.id == other.id
    }
}

impl PartialEq<WeakFocusHandle> for FocusHandle {
    fn eq(&self, other: &WeakFocusHandle) -> bool {
        self.id == other.id
    }
}

/// Focusable allows users of your view to easily
/// focus it (using window.focus_view(cx, view))
pub trait Focusable: 'static {
    /// Returns the focus handle associated with this view.
    fn focus_handle(&self, cx: &App) -> FocusHandle;
}

impl<V: Focusable> Focusable for Entity<V> {
    fn focus_handle(&self, cx: &App) -> FocusHandle {
        self.read(cx).focus_handle(cx)
    }
}

/// ManagedView is a view (like a Modal, Popover, Menu, etc.)
/// where the lifecycle of the view is handled by another view.
pub trait ManagedView: Focusable + EventEmitter<DismissEvent> + Render {}

impl<M: Focusable + EventEmitter<DismissEvent> + Render> ManagedView for M {}

/// Emitted by implementers of [`ManagedView`] to indicate the view should be dismissed, such as when a view is presented as a modal.
pub struct DismissEvent;

type FrameCallback = Box<dyn FnOnce(&mut Window, &mut App)>;

#[derive(Clone)]
pub(crate) struct CursorStyleRequest {
    pub(crate) hitbox_id: Option<HitboxId>,
    pub(crate) style: CursorStyle,
}

#[derive(Default, Eq, PartialEq)]
pub(crate) struct HitTest {
    pub(crate) ids: SmallVec<[HitboxId; 8]>,
    pub(crate) hover_hitbox_count: usize,
}

#[derive(Clone, Debug)]
pub(crate) struct HitboxSnapshot {
    pub(crate) transform_id: TransformId,
    pub(crate) bounds: Bounds<Pixels>,
    pub(crate) content_mask: ContentMask<Pixels>,
    pub(crate) behavior: HitboxBehavior,
}

#[derive(Clone, Copy, Debug)]
struct TransformStackFrame {
    id: TransformId,
    offset: Point<Pixels>,
}

/// Manages the current transform context during prepaint and paint.
pub(crate) struct TransformStack {
    frames: Vec<TransformStackFrame>,
}

impl Default for TransformStack {
    fn default() -> Self {
        Self::new()
    }
}

impl TransformStack {
    pub(crate) fn new() -> Self {
        Self {
            frames: vec![TransformStackFrame {
                id: TransformId::ROOT,
                offset: Point::default(),
            }],
        }
    }

    pub(crate) fn depth(&self) -> usize {
        self.frames.len()
    }

    pub(crate) fn truncate(&mut self, depth: usize) {
        self.frames.truncate(depth.max(1));
    }

    pub(crate) fn set_local_offset(&mut self, offset: Point<Pixels>) {
        if let Some(frame) = self.frames.last_mut() {
            frame.offset = offset;
        }
    }

    /// Get the current transform ID.
    pub(crate) fn current(&self) -> TransformId {
        self.frames
            .last()
            .map(|frame| frame.id)
            .unwrap_or(TransformId::ROOT)
    }

    /// Get the current offset within this transform context.
    pub(crate) fn local_offset(&self) -> Point<Pixels> {
        self.frames
            .last()
            .map(|frame| frame.offset)
            .unwrap_or_default()
    }

    /// Push a simple offset (accumulates into the current frame's offset).
    pub(crate) fn push_offset(&mut self, offset: Point<Pixels>) {
        if let Some(frame) = self.frames.last_mut() {
            frame.offset.x += offset.x;
            frame.offset.y += offset.y;
        }
    }

    /// Pop a simple offset.
    pub(crate) fn pop_offset(&mut self, offset: Point<Pixels>) {
        if let Some(frame) = self.frames.last_mut() {
            frame.offset.x -= offset.x;
            frame.offset.y -= offset.y;
        }
    }

    /// Push an existing transform context.
    ///
    /// This resets the local offset for the child context to 0, so that primitives can be stored
    /// in the transform's local coordinate space.
    pub(crate) fn push_existing_transform(&mut self, id: TransformId) {
        self.frames.push(TransformStackFrame {
            id,
            offset: Point::default(),
        });
    }

    /// Pop the current transform context.
    pub(crate) fn pop_transform(&mut self) {
        if self.frames.len() > 1 {
            self.frames.pop();
        }
    }
}

/// A type of window control area that corresponds to the platform window.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum WindowControlArea {
    /// An area that allows dragging of the platform window.
    Drag,
    /// An area that allows closing of the platform window.
    Close,
    /// An area that allows maximizing of the platform window.
    Max,
    /// An area that allows minimizing of the platform window.
    Min,
}

/// An identifier for a [Hitbox] which also includes [HitboxBehavior].
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct HitboxId(NodeId);

impl HitboxId {
    /// Checks if the hitbox with this ID is currently hovered.
    pub fn is_hovered(self, window: &Window) -> bool {
        window.hitbox_is_hovered(self)
    }

    /// Checks if the hitbox contains the mouse and should handle scroll events.
    pub fn should_handle_scroll(self, window: &Window) -> bool {
        window.hitbox_should_handle_scroll(self)
    }
}

impl std::ops::Deref for HitboxId {
    type Target = NodeId;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<NodeId> for HitboxId {
    fn from(id: NodeId) -> Self {
        Self(id)
    }
}

impl From<HitboxId> for NodeId {
    fn from(id: HitboxId) -> Self {
        id.0
    }
}

impl From<HitboxId> for DefaultKey {
    fn from(id: HitboxId) -> Self {
        id.0.into()
    }
}

/// A rectangular region that potentially blocks hitboxes inserted prior.
/// See [Window::insert_hitbox] for more details.
#[derive(Clone, Debug, Deref)]
pub struct Hitbox {
    /// A unique identifier for the hitbox.
    pub id: HitboxId,
    /// The bounds of the hitbox.
    #[deref]
    pub bounds: Bounds<Pixels>,
    /// The content mask when the hitbox was inserted.
    pub content_mask: ContentMask<Pixels>,
    /// Flags that specify hitbox behavior.
    pub behavior: HitboxBehavior,
}

impl Hitbox {
    /// Checks if the hitbox is currently hovered. Except when handling `ScrollWheelEvent`, this is
    /// typically what you want when determining whether to handle mouse events or paint hover
    /// styles.
    ///
    /// This can return `false` even when the hitbox contains the mouse, if a hitbox in front of
    /// this sets `HitboxBehavior::BlockMouse` (`InteractiveElement::occlude`) or
    /// `HitboxBehavior::BlockMouseExceptScroll` (`InteractiveElement::block_mouse_except_scroll`).
    ///
    /// Handling of `ScrollWheelEvent` should typically use `should_handle_scroll` instead.
    /// Concretely, this is due to use-cases like overlays that cause the elements under to be
    /// non-interactive while still allowing scrolling. More abstractly, this is because
    /// `is_hovered` is about element interactions directly under the mouse - mouse moves, clicks,
    /// hover styling, etc. In contrast, scrolling is about finding the current outer scrollable
    /// container.
    pub fn is_hovered(&self, window: &Window) -> bool {
        self.id.is_hovered(window)
    }

    /// Checks if the hitbox contains the mouse and should handle scroll events. Typically this
    /// should only be used when handling `ScrollWheelEvent`, and otherwise `is_hovered` should be
    /// used. See the documentation of `Hitbox::is_hovered` for details about this distinction.
    ///
    /// This can return `false` even when the hitbox contains the mouse, if a hitbox in front of
    /// this sets `HitboxBehavior::BlockMouse` (`InteractiveElement::occlude`).
    pub fn should_handle_scroll(&self, window: &Window) -> bool {
        self.id.should_handle_scroll(window)
    }
}

/// How the hitbox affects mouse behavior.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum HitboxBehavior {
    /// Normal hitbox mouse behavior, doesn't affect mouse handling for other hitboxes.
    #[default]
    Normal,

    /// All hitboxes behind this hitbox will be ignored and so will have `hitbox.is_hovered() ==
    /// false` and `hitbox.should_handle_scroll() == false`. Typically for elements this causes
    /// skipping of all mouse events, hover styles, and tooltips. This flag is set by
    /// [`InteractiveElement::occlude`].
    ///
    /// For mouse handlers that check those hitboxes, this behaves the same as registering a
    /// bubble-phase handler for every mouse event type:
    ///
    /// ```ignore
    /// window.on_mouse_event(move |_: &EveryMouseEventTypeHere, phase, window, cx| {
    ///     if phase == DispatchPhase::Capture && hitbox.is_hovered(window) {
    ///         cx.stop_propagation();
    ///     }
    /// })
    /// ```
    ///
    /// This has effects beyond event handling - any use of hitbox checking, such as hover
    /// styles and tooltips. These other behaviors are the main point of this mechanism. An
    /// alternative might be to not affect mouse event handling - but this would allow
    /// inconsistent UI where clicks and moves interact with elements that are not considered to
    /// be hovered.
    BlockMouse,

    /// All hitboxes behind this hitbox will have `hitbox.is_hovered() == false`, even when
    /// `hitbox.should_handle_scroll() == true`. Typically for elements this causes all mouse
    /// interaction except scroll events to be ignored - see the documentation of
    /// [`Hitbox::is_hovered`] for details. This flag is set by
    /// [`InteractiveElement::block_mouse_except_scroll`].
    ///
    /// For mouse handlers that check those hitboxes, this behaves the same as registering a
    /// bubble-phase handler for every mouse event type **except** `ScrollWheelEvent`:
    ///
    /// ```ignore
    /// window.on_mouse_event(move |_: &EveryMouseEventTypeExceptScroll, phase, window, cx| {
    ///     if phase == DispatchPhase::Bubble && hitbox.should_handle_scroll(window) {
    ///         cx.stop_propagation();
    ///     }
    /// })
    /// ```
    ///
    /// See the documentation of [`Hitbox::is_hovered`] for details of why `ScrollWheelEvent` is
    /// handled differently than other mouse events. If also blocking these scroll events is
    /// desired, then a `cx.stop_propagation()` handler like the one above can be used.
    ///
    /// This has effects beyond event handling - this affects any use of `is_hovered`, such as
    /// hover styles and tooltips. These other behaviors are the main point of this mechanism.
    /// An alternative might be to not affect mouse event handling - but this would allow
    /// inconsistent UI where clicks and moves interact with elements that are not considered to
    /// be hovered.
    BlockMouseExceptScroll,
}

/// An identifier for a tooltip.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct TooltipId(usize);

impl TooltipId {
    pub(crate) fn next(&mut self) -> TooltipId {
        let id = self.0;
        self.0 = self.0.wrapping_add(1);
        TooltipId(id)
    }

    /// Checks if the tooltip is currently hovered.
    pub fn is_hovered(&self, window: &Window) -> bool {
        window
            .tooltip_bounds
            .as_ref()
            .is_some_and(|tooltip_bounds| {
                tooltip_bounds.id == *self
                    && tooltip_bounds.bounds.contains(&window.mouse_position())
            })
    }
}

pub(crate) struct TooltipBounds {
    id: TooltipId,
    bounds: Bounds<Pixels>,
}

/// Tracks an active fiber-backed overlay (tooltip, prompt, or drag) for painting.
#[derive(Clone, Copy)]
pub(crate) struct ActiveOverlay {
    /// The fiber root for this overlay.
    pub(crate) fiber_id: GlobalElementId,
    /// The absolute offset to paint at.
    pub(crate) offset: Point<Pixels>,
    /// The view context for painting.
    pub(crate) view_id: EntityId,
}

#[derive(Clone)]
pub(crate) struct TooltipRequest {
    pub(crate) id: TooltipId,
    pub(crate) tooltip: AnyTooltip,
}

pub(crate) struct DeferredDraw {
    pub(crate) current_view: EntityId,
    pub(crate) priority: usize,
    pub(crate) text_style_stack: Vec<TextStyleRefinement>,
    pub(crate) rem_size: Pixels,
    pub(crate) element: Option<AnyElement>,
    pub(crate) fiber_id: Option<GlobalElementId>,
    pub(crate) reference_fiber: Option<GlobalElementId>,
    pub(crate) local_offset: Point<Pixels>,
    /// Whether this deferred draw needs its own layout pass.
    ///
    /// `false` for deferred drawing of an already-laid-out fiber subtree (e.g. `deferred(...)`).
    /// `true` for detached overlay trees created via `Window::defer_draw`.
    pub(crate) requires_layout: bool,
    pub(crate) layout_constraints: Size<AvailableSpace>,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct HitboxesSnapshotEpoch {
    structure_epoch: u64,
    hitbox_epoch: u64,
}

pub(crate) struct Frame {
    pub(crate) focus_path: SmallVec<[FocusId; 8]>,
    pub(crate) window_active: bool,
    pub(crate) scene: Scene,
    pub(crate) hitboxes: FxHashMap<HitboxId, HitboxSnapshot>,
    hitboxes_epoch: Option<HitboxesSnapshotEpoch>,
    pub(crate) window_control_hitboxes: Vec<(WindowControlArea, Hitbox)>,
    #[cfg(any(test, feature = "test-support"))]
    pub(crate) debug_bounds: FxHashMap<String, Bounds<Pixels>>,
    #[cfg(any(feature = "inspector", debug_assertions))]
    pub(crate) next_inspector_instance_ids: FxHashMap<Rc<crate::InspectorElementPath>, usize>,
    #[cfg(any(feature = "inspector", debug_assertions))]
    pub(crate) inspector_hitboxes: FxHashMap<HitboxId, crate::InspectorElementId>,
}

impl Frame {
    pub(crate) fn new() -> Self {
        Frame {
            focus_path: SmallVec::new(),
            window_active: false,
            scene: Scene::default(),
            hitboxes: FxHashMap::default(),
            hitboxes_epoch: None,
            window_control_hitboxes: Vec::new(),

            #[cfg(any(test, feature = "test-support"))]
            debug_bounds: FxHashMap::default(),

            #[cfg(any(feature = "inspector", debug_assertions))]
            next_inspector_instance_ids: FxHashMap::default(),

            #[cfg(any(feature = "inspector", debug_assertions))]
            inspector_hitboxes: FxHashMap::default(),
        }
    }

    pub(crate) fn clear(&mut self) {
        self.scene.clear_transient();
        self.window_control_hitboxes.clear();
        self.focus_path.clear();

        #[cfg(any(feature = "inspector", debug_assertions))]
        {
            self.next_inspector_instance_ids.clear();
            self.inspector_hitboxes.clear();
        }
    }

    pub(crate) fn hit_test(&self, window: &Window, position: Point<Pixels>) -> HitTest {
        let mut set_hover_hitbox_count = false;
        let mut hit_test = HitTest::default();

        let viewport = window.viewport_size();
        if position.x < px(0.)
            || position.y < px(0.)
            || position.x >= viewport.width
            || position.y >= viewport.height
        {
            return hit_test;
        }

        let scale_factor = window.scale_factor();
        let transforms = &window.segment_pool.transforms;
        let world_scaled = Point::new(
            ScaledPixels(position.x.0 * scale_factor),
            ScaledPixels(position.y.0 * scale_factor),
        );

        // Returns true if we should stop (BlockMouse behavior hit)
        let mut push_hitbox = |hit_test: &mut HitTest,
                               set_hover_hitbox_count: &mut bool,
                               hitbox_id: HitboxId,
                               hitbox: &HitboxSnapshot| {
            if !hitbox.content_mask.bounds.contains(&position) {
                return false;
            }

            let local_scaled =
                transforms.world_to_local_no_cache(hitbox.transform_id, world_scaled);
            let local_point = Point::new(
                Pixels(local_scaled.x.0 / scale_factor),
                Pixels(local_scaled.y.0 / scale_factor),
            );

            if hitbox.bounds.contains(&local_point) {
                hit_test.ids.push(hitbox_id);
                if !*set_hover_hitbox_count
                    && hitbox.behavior == HitboxBehavior::BlockMouseExceptScroll
                {
                    hit_test.hover_hitbox_count = hit_test.ids.len();
                    *set_hover_hitbox_count = true;
                }
                if hitbox.behavior == HitboxBehavior::BlockMouse {
                    return true;
                }
            }
            false
        };

        let should_visit_subtree = |fiber_id: GlobalElementId| {
            window
                .fiber
                .tree
                .hitbox_state
                .get(fiber_id.into())
                .and_then(|state| state.hitbox_subtree_bounds)
                .is_some_and(|subtree| {
                    let local_scaled =
                        transforms.world_to_local_no_cache(subtree.transform_id, world_scaled);
                    let local_point = Point::new(
                        Pixels(local_scaled.x.0 / scale_factor),
                        Pixels(local_scaled.y.0 / scale_factor),
                    );
                    subtree.bounds.contains(&local_point)
                })
        };

        // Defered fibers (via `deferred(...)`) paint after all non-deferred content, regardless of
        // their position in the tree. Hit-testing must mirror this paint order so overlays remain
        // interactive even when declared before the content they cover.
        let mut deferred_roots: Vec<(GlobalElementId, usize)> = window
            .fiber
            .active_deferred_draws
            .members
            .iter()
            .filter_map(|fiber_id| {
                window
                    .fiber
                    .tree
                    .deferred_priorities
                    .get((*fiber_id).into())
                    .map(|&priority| (*fiber_id, priority))
            })
            .collect();
        deferred_roots.sort_by_key(|(_, priority)| *priority);

        let mut process_hitbox =
            |fiber_id: GlobalElementId,
             hit_test: &mut HitTest,
             set_hover_hitbox_count: &mut bool| {
                if let Some(hitbox) = self.hitboxes.get(&fiber_id.into()) {
                    push_hitbox(hit_test, set_hover_hitbox_count, fiber_id.into(), hitbox)
                } else if let Some(hitbox) = resolve_hitbox_for_hit_test(window, &fiber_id) {
                    push_hitbox(hit_test, set_hover_hitbox_count, fiber_id.into(), &hitbox)
                } else {
                    false
                }
            };

        // Process deferred roots first (topmost-first): higher priority is painted later (on top).
        'outer: for (deferred_root, _priority) in deferred_roots.iter().rev().copied() {
            let mut stack: Vec<(GlobalElementId, bool)> = vec![(deferred_root, true)];
            while let Some((fiber_id, entering)) = stack.pop() {
                if entering {
                    if !should_visit_subtree(fiber_id) {
                        continue;
                    }
                    stack.push((fiber_id, false));
                    for child_id in window.fiber.tree.children_slice(&fiber_id) {
                        stack.push((*child_id, true));
                    }
                } else if process_hitbox(fiber_id, &mut hit_test, &mut set_hover_hitbox_count) {
                    break 'outer;
                }
            }
        }

        // Then process the main tree, skipping deferred subtrees (they were handled above).
        if let Some(root) = window.fiber.tree.root {
            let mut stack: Vec<(GlobalElementId, bool)> = vec![(root, true)];
            while let Some((fiber_id, entering)) = stack.pop() {
                if entering {
                    if !should_visit_subtree(fiber_id) {
                        continue;
                    }
                    if window
                        .fiber
                        .tree
                        .deferred_priorities
                        .contains_key(fiber_id.into())
                    {
                        continue;
                    }
                    stack.push((fiber_id, false));
                    for child_id in window.fiber.tree.children_slice(&fiber_id) {
                        stack.push((*child_id, true));
                    }
                } else if process_hitbox(fiber_id, &mut hit_test, &mut set_hover_hitbox_count) {
                    break;
                }
            }
        }

        if !set_hover_hitbox_count {
            hit_test.hover_hitbox_count = hit_test.ids.len();
        }

        hit_test
    }

    pub(crate) fn focus_path(&self) -> &SmallVec<[FocusId; 8]> {
        &self.focus_path
    }

    pub(crate) fn finish(
        &mut self,
        segment_pool: &mut SceneSegmentPool,
    ) -> crate::scene::SceneFinishStats {
        self.scene.finish(segment_pool)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum InputModality {
    Mouse,
    Keyboard,
}

/// Diagnostic counters for the most recently completed frame.
///
/// Enable in release builds via the `diagnostics` feature.
#[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct FrameDiagnostics {
    /// Frame number for which these counters were recorded.
    pub frame_number: u64,
    /// `FiberTree` structure epoch at frame start.
    pub structure_epoch: u64,
    /// `FiberTree` hitbox epoch at frame end.
    pub hitbox_epoch: u64,
    /// Number of fibers that executed a prepaint this frame (not replayed).
    pub prepaint_fibers: usize,
    /// Number of subtrees whose prepaint state was replayed.
    pub prepaint_replayed_subtrees: usize,
    /// Number of fibers that executed paint this frame (not replayed).
    pub paint_fibers: usize,
    /// Number of subtrees whose paint output was replayed.
    pub paint_replayed_subtrees: usize,
    /// Whether the scene segment order was rebuilt.
    pub segment_order_rebuilt: bool,
    /// Scene segment order length when rebuilt.
    pub scene_segment_order_len: usize,
    /// Whether the hitbox snapshot map was rebuilt.
    pub hitboxes_snapshot_rebuilt: bool,
    /// Hitbox count in the snapshot map.
    pub hitboxes_in_snapshot: usize,
    /// Total allocated segments in the segment pool.
    pub total_pool_segments: usize,
    /// Segments mutated in the current scene mutation epoch.
    pub mutated_pool_segments: usize,
    /// Whether the transient segment was mutated.
    pub transient_segment_mutated: bool,
    /// Total path primitives in the scene.
    pub paths: usize,
    /// Total shadow primitives in the scene.
    pub shadows: usize,
    /// Total quad primitives in the scene.
    pub quads: usize,
    /// Total underline primitives in the scene.
    pub underlines: usize,
    /// Total monochrome sprite primitives in the scene.
    pub monochrome_sprites: usize,
    /// Total subpixel sprite primitives in the scene.
    pub subpixel_sprites: usize,
    /// Total polychrome sprite primitives in the scene.
    pub polychrome_sprites: usize,
    /// Total surface primitives in the scene.
    pub surfaces: usize,
    /// Estimated bytes uploaded for instance buffers if the entire scene is re-uploaded.
    pub estimated_instance_upload_bytes: usize,
    /// Number of fibers that had layout computed (cache miss).
    pub layout_fibers: usize,
    /// Time spent in the reconcile phase.
    pub reconcile_time: std::time::Duration,
    /// Time spent in the intrinsic sizing phase.
    pub intrinsic_sizing_time: std::time::Duration,
    /// Time spent in the layout phase.
    pub layout_time: std::time::Duration,
    /// Time spent in the prepaint phase.
    pub prepaint_time: std::time::Duration,
    /// Time spent in the paint phase.
    pub paint_time: std::time::Duration,
    /// Time spent in end-of-frame cleanup (clearing work flags, descendant tracking, etc.).
    pub cleanup_time: std::time::Duration,
    /// Total frame time.
    pub total_time: std::time::Duration,
}

/// Holds the state for a specific window.
pub struct Window {
    pub(crate) handle: AnyWindowHandle,
    pub(crate) invalidator: WindowInvalidator,
    pub(crate) removed: bool,
    pub(crate) platform_window: Box<dyn PlatformWindow>,
    display_id: Option<DisplayId>,
    sprite_atlas: Arc<dyn PlatformAtlas>,
    text_system: Arc<WindowTextSystem>,
    pub(crate) layout_engine: TaffyLayoutEngine,
    key_dispatch: KeyDispatcher,
    text_rendering_mode: Rc<Cell<TextRenderingMode>>,
    rem_size: Pixels,
    /// The stack of override values for the window's rem size.
    ///
    /// This is used by `with_rem_size` to allow rendering an element tree with
    /// a given rem size.
    pub(crate) rem_size_override_stack: SmallVec<[Pixels; 8]>,
    pub(crate) viewport_size: Size<Pixels>,
    pub(crate) fiber: FiberRuntime,
    pub(crate) root: Option<AnyView>,
    pub(crate) text_style_stack: Vec<TextStyleRefinement>,
    pub(crate) rendered_entity_stack: Vec<EntityId>,
    pub(crate) transform_stack: TransformStack,
    scroll_transforms: FxHashMap<GlobalElementId, TransformId>,
    pub(crate) element_opacity: f32,
    pub(crate) content_mask_stack: Vec<ContentMask<Pixels>>,
    scene_culling_disabled_depth: usize,
    pub(crate) requested_autoscroll: Option<Bounds<Pixels>>,
    pub(crate) image_cache_stack: Vec<AnyImageCache>,
    pub(crate) rendered_frame: Frame,
    pub(crate) next_frame: Frame,
    /// Shared storage for fiber scene segments. Persists across frame swaps so that
    /// segment IDs allocated during paint remain valid when frames are swapped.
    pub(crate) segment_pool: SceneSegmentPool,
    pub(crate) next_tooltip_id: TooltipId,
    pub(crate) tooltip_bounds: Option<TooltipBounds>,
    /// Active overlay state for fiber-backed overlays (tooltip/prompt/drag).
    /// Stores the offset used during prepaint for use during paint.
    pub(crate) active_overlay: Option<ActiveOverlay>,
    next_frame_callbacks: Rc<RefCell<Vec<FrameCallback>>>,
    render_layers: FxHashMap<ElementId, RenderLayerRegistration>,
    next_render_layer_seq: usize,
    global_overlay_owner_fiber: Option<GlobalElementId>,
    measurement_generation: u64,
    measurement_cache: MeasurementCache<MeasurementKey>,
    pending_view_accesses: FxHashMap<GlobalElementId, FxHashSet<EntityId>>,
    focus_listeners: SubscriberSet<(), AnyWindowFocusListener>,
    pub(crate) focus_lost_listeners: SubscriberSet<(), AnyObserver>,
    default_prevented: bool,
    mouse_position: Point<Pixels>,
    pub(crate) mouse_hit_test: HitTest,
    pending_mouse_hit_test_refresh: bool,
    modifiers: Modifiers,
    capslock: Capslock,
    scale_factor: f32,
    pub(crate) bounds_observers: SubscriberSet<(), AnyObserver>,
    appearance: WindowAppearance,
    pub(crate) appearance_observers: SubscriberSet<(), AnyObserver>,
    active: Rc<Cell<bool>>,
    hovered: Rc<Cell<bool>>,
    pub(crate) needs_present: Rc<Cell<bool>>,
    /// Tracks recent input event timestamps to determine if input is arriving at a high rate.
    /// Used to selectively enable VRR optimization only when input rate exceeds 60fps.
    pub(crate) input_rate_tracker: Rc<RefCell<InputRateTracker>>,
    #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
    pub(crate) frame_diagnostics: FrameDiagnostics,
    #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
    completed_frame_diagnostics: FrameDiagnostics,
    last_input_modality: InputModality,
    pub(crate) refreshing: bool,
    measurement_depth: u32,
    /// Tracks whether a full reconciliation (structure change) occurred during the current frame.
    /// Used to determine whether to update the entity invalidator mapping.
    /// Only full reconciliation should update the mapping, as incremental rendering only
    /// accesses a subset of entities and would incorrectly remove others.
    full_reconciliation_this_frame: bool,
    pub(crate) activation_observers: SubscriberSet<(), AnyObserver>,
    pub(crate) focus: Option<FocusId>,
    focus_before_deactivation: Option<FocusId>,
    focus_enabled: bool,
    pending_input: Option<PendingInput>,
    pending_modifier: ModifierState,
    pub(crate) pending_input_observers: SubscriberSet<(), AnyObserver>,
    prompt: Option<RenderablePromptHandle>,
    pub(crate) client_inset: Option<Pixels>,
    #[cfg(any(feature = "inspector", debug_assertions))]
    inspector: Option<Entity<Inspector>>,
}

type RenderLayerBuilder = Arc<dyn Fn(&mut Window, &mut App) -> AnyElement + 'static>;

#[derive(Clone)]
struct RenderLayerRegistration {
    order: i32,
    seq: usize,
    build: RenderLayerBuilder,
}

#[derive(Clone, Debug, Default)]
struct ModifierState {
    modifiers: Modifiers,
    saw_keystroke: bool,
}

/// Tracks input event timestamps to determine if input is arriving at a high rate.
/// Used for selective VRR (Variable Refresh Rate) optimization.
#[derive(Clone, Debug)]
pub(crate) struct InputRateTracker {
    timestamps: Vec<Instant>,
    window: Duration,
    inputs_per_second: u32,
    sustain_until: Instant,
    sustain_duration: Duration,
}

impl Default for InputRateTracker {
    fn default() -> Self {
        Self {
            timestamps: Vec::new(),
            window: Duration::from_millis(100),
            inputs_per_second: 60,
            sustain_until: Instant::now(),
            sustain_duration: Duration::from_secs(1),
        }
    }
}

impl InputRateTracker {
    pub fn record_input(&mut self) {
        let now = Instant::now();
        self.timestamps.push(now);
        self.prune_old_timestamps(now);

        let min_events = self.inputs_per_second as u128 * self.window.as_millis() / 1000;
        if self.timestamps.len() as u128 >= min_events {
            self.sustain_until = now + self.sustain_duration;
        }
    }

    pub fn is_high_rate(&self) -> bool {
        Instant::now() < self.sustain_until
    }

    fn prune_old_timestamps(&mut self, now: Instant) {
        self.timestamps
            .retain(|&t| now.duration_since(t) <= self.window);
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum DrawPhase {
    None,
    Reconcile,
    Layout,
    Prepaint,
    Paint,
    Focus,
    Event,
}

#[derive(Default, Debug)]
struct PendingInput {
    keystrokes: SmallVec<[Keystroke; 1]>,
    focus: Option<FocusId>,
    timer: Option<Task<()>>,
    needs_timeout: bool,
}

fn default_bounds(display_id: Option<DisplayId>, cx: &mut App) -> WindowBounds {
    // TODO, BUG: if you open a window with the currently active window
    // on the stack, this will erroneously fallback to `None`
    //
    // TODO these should be the initial window bounds not considering maximized/fullscreen
    let active_window_bounds = cx
        .active_window()
        .and_then(|w| w.update(cx, |_, window, _| window.window_bounds()).ok());

    const CASCADE_OFFSET: f32 = 25.0;

    let display = display_id
        .map(|id| cx.find_display(id))
        .unwrap_or_else(|| cx.primary_display());

    let default_placement = || Bounds::new(point(px(0.), px(0.)), DEFAULT_WINDOW_SIZE);

    // Use visible_bounds to exclude taskbar/dock areas
    let display_bounds = display
        .as_ref()
        .map(|d| d.visible_bounds())
        .unwrap_or_else(default_placement);

    let (
        Bounds {
            origin: base_origin,
            size: base_size,
        },
        window_bounds_ctor,
    ): (_, fn(Bounds<Pixels>) -> WindowBounds) = match active_window_bounds {
        Some(bounds) => match bounds {
            WindowBounds::Windowed(bounds) => (bounds, WindowBounds::Windowed),
            WindowBounds::Maximized(bounds) => (bounds, WindowBounds::Maximized),
            WindowBounds::Fullscreen(bounds) => (bounds, WindowBounds::Fullscreen),
        },
        None => (
            display
                .as_ref()
                .map(|d| d.default_bounds())
                .unwrap_or_else(default_placement),
            WindowBounds::Windowed,
        ),
    };

    let cascade_offset = point(px(CASCADE_OFFSET), px(CASCADE_OFFSET));
    let proposed_origin = base_origin + cascade_offset;
    let proposed_bounds = Bounds::new(proposed_origin, base_size);

    let display_right = display_bounds.origin.x + display_bounds.size.width;
    let display_bottom = display_bounds.origin.y + display_bounds.size.height;
    let window_right = proposed_bounds.origin.x + proposed_bounds.size.width;
    let window_bottom = proposed_bounds.origin.y + proposed_bounds.size.height;

    let fits_horizontally = window_right <= display_right;
    let fits_vertically = window_bottom <= display_bottom;

    let final_origin = match (fits_horizontally, fits_vertically) {
        (true, true) => proposed_origin,
        (false, true) => point(display_bounds.origin.x, base_origin.y),
        (true, false) => point(base_origin.x, display_bounds.origin.y),
        (false, false) => display_bounds.origin,
    };
    window_bounds_ctor(Bounds::new(final_origin, base_size))
}

struct IntrinsicSizingOptions {
    track_processed_islands: bool,
    convergence_root: Option<GlobalElementId>,
    force_compute: bool,
}

#[derive(Clone, Copy, Debug)]
/// Tracks what was pushed by [`Window::push_fiber_scroll_transform`], so it can be popped later.
pub struct FiberScrollTransformScope {
    pushed_unculled_scene: bool,
    pushed_transform: bool,
}

impl FiberScrollTransformScope {
    /// A scope that indicates nothing was pushed.
    pub const NONE: Self = Self {
        pushed_unculled_scene: false,
        pushed_transform: false,
    };
}

impl Window {
    pub(crate) fn effects_suppressed(&self) -> bool {
        self.measurement_depth > 0
    }

    pub(crate) fn with_render_node_mut<R>(
        &mut self,
        fiber_id: GlobalElementId,
        f: impl FnOnce(&mut dyn crate::RenderNode, &mut Window) -> R,
    ) -> Option<R> {
        let mut render_node = self.fiber.tree.render_nodes.remove(fiber_id.into())?;
        let result = f(&mut *render_node, self);
        self.fiber
            .tree
            .render_nodes
            .insert(fiber_id.into(), render_node);
        Some(result)
    }

    pub(crate) fn prepare_detached_root_for_layout(
        &mut self,
        root: GlobalElementId,
        cx: &mut App,
    ) {
        self.compute_intrinsic_sizes_for_detached_root(root, cx);
        crate::taffy::TaffyLayoutEngine::setup_taffy_from_fibers(self, root, cx);
    }

    fn update_platform_input_handler(&mut self, cx: &App) {
        if !self.invalidator.not_drawing() {
            return;
        }
        if let Some(input_handler) = self.fibers().latest_input_handler(cx) {
            self.platform_window.set_input_handler(input_handler);
        } else {
            let _ = self.platform_window.take_input_handler();
        }
    }
    pub(crate) fn fibers(&mut self) -> FiberRuntimeHandle<'_> {
        FiberRuntimeHandle { window: self }
    }

    pub(crate) fn fibers_ref(&self) -> FiberRuntimeHandleRef<'_> {
        FiberRuntimeHandleRef { window: self }
    }

    /// Returns true if the hitbox is currently hovered.
    pub fn hitbox_is_hovered(&self, hitbox_id: HitboxId) -> bool {
        self.mouse_hit_test
            .ids
            .iter()
            .take(self.mouse_hit_test.hover_hitbox_count)
            .any(|id| *id == hitbox_id)
    }

    /// Returns true if the hitbox should handle scroll events.
    pub fn hitbox_should_handle_scroll(&self, hitbox_id: HitboxId) -> bool {
        self.mouse_hit_test.ids.contains(&hitbox_id)
    }

    fn resolve_hitbox_bounds_world(&self, data: &crate::fiber::HitboxData) -> Bounds<Pixels> {
        if data.transform_id.is_root() {
            return data.bounds;
        }

        let scale_factor = self.scale_factor();
        let local_scaled = data.bounds.scale(scale_factor);
        let world = self
            .segment_pool
            .transforms
            .get_world_no_cache(data.transform_id);
        let origin_scaled = world.apply(local_scaled.origin);
        let size_scaled = Size::new(
            ScaledPixels(local_scaled.size.width.0 * world.scale),
            ScaledPixels(local_scaled.size.height.0 * world.scale),
        );

        Bounds::new(
            Point::new(
                Pixels(origin_scaled.x.0 / scale_factor),
                Pixels(origin_scaled.y.0 / scale_factor),
            ),
            Size::new(
                Pixels(size_scaled.width.0 / scale_factor),
                Pixels(size_scaled.height.0 / scale_factor),
            ),
        )
    }

    pub(crate) fn resolve_hitbox(&self, fiber_id: &GlobalElementId) -> Option<Hitbox> {
        self.invalidator.debug_assert_prepaint_or_paint();
        let data = self
            .fiber
            .tree
            .hitbox_state
            .get((*fiber_id).into())
            .and_then(|state| state.hitbox.as_ref())?;

        let bounds = self.resolve_hitbox_bounds_world(data);

        Some(Hitbox {
            id: (*fiber_id).into(),
            bounds,
            content_mask: data.content_mask.clone(),
            behavior: data.behavior,
        })
    }

    pub(crate) fn resolve_hitbox_for_event(&self, fiber_id: &GlobalElementId) -> Option<Hitbox> {
        let data = self
            .fiber
            .tree
            .hitbox_state
            .get((*fiber_id).into())
            .and_then(|state| state.hitbox.as_ref())?;

        Some(Hitbox {
            id: (*fiber_id).into(),
            bounds: self.resolve_hitbox_bounds_world(data),
            content_mask: data.content_mask.clone(),
            behavior: data.behavior,
        })
    }

    pub(crate) fn get_fiber_effects(&self, fiber_id: &GlobalElementId) -> Option<&FiberEffects> {
        self.fiber.tree.effects.get((*fiber_id).into())
    }

    /// Inserts a hitbox associated with a fiber.
    pub fn insert_hitbox_with_fiber(
        &mut self,
        bounds: Bounds<Pixels>,
        behavior: HitboxBehavior,
        fiber_id: GlobalElementId,
    ) -> Hitbox {
        self.fibers()
            .insert_hitbox_with_fiber(bounds, behavior, fiber_id)
    }

    pub(crate) fn register_fiber_effects(
        &mut self,
        fiber_id: &GlobalElementId,
    ) -> Option<&mut FiberEffects> {
        let entry = self.fiber.tree.effects.entry((*fiber_id).into())?;
        let effects = entry.or_insert_with(FiberEffects::new);
        Some(effects)
    }

    pub(crate) fn update_active_mouse_listeners(&mut self, fiber_id: &GlobalElementId) {
        let effects = self.fiber.tree.effects.get((*fiber_id).into());
        // Get interactivity from render node
        let interactivity = self
            .fiber
            .tree
            .render_nodes
            .get((*fiber_id).into())
            .and_then(|node| node.interactivity());
        if has_mouse_effects(interactivity, effects) {
            self.fiber.active_mouse_listeners.insert(*fiber_id);
        } else {
            self.fiber.active_mouse_listeners.remove(fiber_id);
        }
    }

    pub(crate) fn clear_fiber_mouse_effects(&mut self, fiber_id: &GlobalElementId) {
        if let Some(effects) = self.fiber.tree.effects.get_mut((*fiber_id).into()) {
            effects.click_listeners.clear();
            effects.any_mouse_listeners.clear();
            effects.mouse_down_listeners.clear();
            effects.mouse_up_listeners.clear();
            effects.mouse_move_listeners.clear();
            effects.mouse_pressure_listeners.clear();
            effects.scroll_wheel_listeners.clear();
            effects.drag_listener = None;
            effects.drop_listeners.clear();
            effects.can_drop_predicate = None;
            effects.hover_listener = None;
            effects.tooltip = None;
            effects.cursor_style = None;
        }
        self.fiber.active_mouse_listeners.remove(fiber_id);
    }

    pub(crate) fn new(
        handle: AnyWindowHandle,
        options: WindowOptions,
        cx: &mut App,
    ) -> Result<Self> {
        let WindowOptions {
            window_bounds,
            titlebar,
            focus,
            show,
            kind,
            is_movable,
            is_resizable,
            is_minimizable,
            display_id,
            window_background,
            app_id,
            window_min_size,
            window_decorations,
            #[cfg_attr(not(target_os = "macos"), allow(unused_variables))]
            tabbing_identifier,
        } = options;

        let window_bounds = window_bounds.unwrap_or_else(|| default_bounds(display_id, cx));
        let mut platform_window = cx.platform.open_window(
            handle,
            WindowParams {
                bounds: window_bounds.get_bounds(),
                titlebar,
                kind,
                is_movable,
                is_resizable,
                is_minimizable,
                focus,
                show,
                display_id,
                window_min_size,
                #[cfg(target_os = "macos")]
                tabbing_identifier,
            },
        )?;

        let tab_bar_visible = platform_window.tab_bar_visible();
        SystemWindowTabController::init_visible(cx, tab_bar_visible);
        if let Some(tabs) = platform_window.tabbed_windows() {
            SystemWindowTabController::add_tab(cx, handle.window_id(), tabs);
        }

        let display_id = platform_window.display().map(|display| display.id());
        let sprite_atlas = platform_window.sprite_atlas();
        let mouse_position = platform_window.mouse_position();
        let modifiers = platform_window.modifiers();
        let capslock = platform_window.capslock();
        let content_size = platform_window.content_size();
        let scale_factor = platform_window.scale_factor();
        let appearance = platform_window.appearance();
        let text_system = Arc::new(WindowTextSystem::new(cx.text_system().clone()));
        let invalidator = WindowInvalidator::new();
        let active = Rc::new(Cell::new(platform_window.is_active()));
        let hovered = Rc::new(Cell::new(platform_window.is_hovered()));
        let needs_present = Rc::new(Cell::new(false));
        let next_frame_callbacks: Rc<RefCell<Vec<FrameCallback>>> = Default::default();
        let input_rate_tracker = Rc::new(RefCell::new(InputRateTracker::default()));

        platform_window
            .request_decorations(window_decorations.unwrap_or(WindowDecorations::Server));
        platform_window.set_background_appearance(window_background);

        match window_bounds {
            WindowBounds::Fullscreen(_) => platform_window.toggle_fullscreen(),
            WindowBounds::Maximized(_) => platform_window.zoom(),
            WindowBounds::Windowed(_) => {}
        }

        platform_window.on_close(Box::new({
            let window_id = handle.window_id();
            let mut cx = cx.to_async();
            move || {
                let _ = handle.update(&mut cx, |_, window, _| window.remove_window());
                let _ = cx.update(|cx| {
                    SystemWindowTabController::remove_tab(cx, window_id);
                });
            }
        }));
        platform_window.on_request_frame(Box::new({
            let mut cx = cx.to_async();
            let invalidator = invalidator.clone();
            let active = active.clone();
            let needs_present = needs_present.clone();
            let next_frame_callbacks = next_frame_callbacks.clone();
            let input_rate_tracker = input_rate_tracker.clone();
            move |request_frame_options| {
                let next_frame_callbacks = next_frame_callbacks.take();
                if !next_frame_callbacks.is_empty() {
                    handle
                        .update(&mut cx, |_, window, cx| {
                            for callback in next_frame_callbacks {
                                callback(window, cx);
                            }
                        })
                        .log_err();
                }

                // Keep presenting if input was recently arriving at a high rate (>= 60fps).
                // Once high-rate input is detected, we sustain presentation for 1 second
                // to prevent display underclocking during active input.
                let needs_present = request_frame_options.require_presentation
                    || needs_present.get()
                    || (active.get() && input_rate_tracker.borrow_mut().is_high_rate());

                if invalidator.is_dirty() || request_frame_options.force_render {
                    measure("frame duration", || {
                        handle
                            .update(&mut cx, |_, window, cx| {
                                window.draw(cx);
                                window.present();
                            })
                            .log_err();
                    })
                } else if needs_present {
                    handle
                        .update(&mut cx, |_, window, _| window.present())
                        .log_err();
                }

                handle
                    .update(&mut cx, |_, window, _| {
                        window.complete_frame();
                    })
                    .log_err();
            }
        }));
        platform_window.on_resize(Box::new({
            let mut cx = cx.to_async();
            move |_, _| {
                handle
                    .update(&mut cx, |_, window, cx| window.bounds_changed(cx))
                    .log_err();
            }
        }));
        platform_window.on_moved(Box::new({
            let mut cx = cx.to_async();
            move || {
                handle
                    .update(&mut cx, |_, window, cx| window.bounds_changed(cx))
                    .log_err();
            }
        }));
        platform_window.on_appearance_changed(Box::new({
            let mut cx = cx.to_async();
            move || {
                handle
                    .update(&mut cx, |_, window, cx| window.appearance_changed(cx))
                    .log_err();
            }
        }));
        platform_window.on_active_status_change(Box::new({
            let mut cx = cx.to_async();
            move |active| {
                handle
                    .update(&mut cx, |_, window, cx| {
                        if active {
                            if let Some(focus_id) = window.focus_before_deactivation.take() {
                                window.focus = Some(focus_id);
                            }
                        } else {
                            window.focus_before_deactivation = window.focus.take();
                        }

                        window.active.set(active);
                        window.modifiers = window.platform_window.modifiers();
                        window.capslock = window.platform_window.capslock();
                        window
                            .activation_observers
                            .clone()
                            .retain(&(), |callback| callback(window, cx));

                        window.bounds_changed(cx);
                        window.refresh();

                        SystemWindowTabController::update_last_active(cx, window.handle.id);
                    })
                    .log_err();
            }
        }));
        platform_window.on_hover_status_change(Box::new({
            let mut cx = cx.to_async();
            move |active| {
                handle
                    .update(&mut cx, |_, window, _| {
                        window.hovered.set(active);
                        window.refresh();
                    })
                    .log_err();
            }
        }));
        platform_window.on_input({
            let mut cx = cx.to_async();
            Box::new(move |event| {
                handle
                    .update(&mut cx, |_, window, cx| window.dispatch_event(event, cx))
                    .log_err()
                    .unwrap_or(DispatchEventResult::default())
            })
        });
        platform_window.on_hit_test_window_control({
            let mut cx = cx.to_async();
            Box::new(move || {
                handle
                    .update(&mut cx, |_, window, _cx| {
                        for (area, hitbox) in &window.rendered_frame.window_control_hitboxes {
                            if window.mouse_hit_test.ids.contains(&hitbox.id) {
                                return Some(*area);
                            }
                        }
                        None
                    })
                    .log_err()
                    .unwrap_or(None)
            })
        });
        platform_window.on_move_tab_to_new_window({
            let mut cx = cx.to_async();
            Box::new(move || {
                handle
                    .update(&mut cx, |_, _window, cx| {
                        SystemWindowTabController::move_tab_to_new_window(cx, handle.window_id());
                    })
                    .log_err();
            })
        });
        platform_window.on_merge_all_windows({
            let mut cx = cx.to_async();
            Box::new(move || {
                handle
                    .update(&mut cx, |_, _window, cx| {
                        SystemWindowTabController::merge_all_windows(cx, handle.window_id());
                    })
                    .log_err();
            })
        });
        platform_window.on_select_next_tab({
            let mut cx = cx.to_async();
            Box::new(move || {
                handle
                    .update(&mut cx, |_, _window, cx| {
                        SystemWindowTabController::select_next_tab(cx, handle.window_id());
                    })
                    .log_err();
            })
        });
        platform_window.on_select_previous_tab({
            let mut cx = cx.to_async();
            Box::new(move || {
                handle
                    .update(&mut cx, |_, _window, cx| {
                        SystemWindowTabController::select_previous_tab(cx, handle.window_id())
                    })
                    .log_err();
            })
        });
        platform_window.on_toggle_tab_bar({
            let mut cx = cx.to_async();
            Box::new(move || {
                handle
                    .update(&mut cx, |_, window, cx| {
                        let tab_bar_visible = window.platform_window.tab_bar_visible();
                        SystemWindowTabController::set_visible(cx, tab_bar_visible);
                    })
                    .log_err();
            })
        });

        if let Some(app_id) = app_id {
            platform_window.set_app_id(&app_id);
        }

        platform_window.map_window().unwrap();

        Ok(Window {
            handle,
            invalidator,
            removed: false,
            platform_window,
            display_id,
            sprite_atlas,
            text_system,
            layout_engine: TaffyLayoutEngine::new(),
            key_dispatch: KeyDispatcher::new(cx.keymap.clone()),
            text_rendering_mode: cx.text_rendering_mode.clone(),
            rem_size: px(16.),
            rem_size_override_stack: SmallVec::new(),
            viewport_size: content_size,
            fiber: FiberRuntime::new(),
            root: None,
            text_style_stack: Vec::new(),
            rendered_entity_stack: Vec::new(),
            transform_stack: TransformStack::new(),
            scroll_transforms: FxHashMap::default(),
            content_mask_stack: Vec::new(),
            scene_culling_disabled_depth: 0,
            element_opacity: 1.0,
            requested_autoscroll: None,
            rendered_frame: Frame::new(),
            next_frame: Frame::new(),
            segment_pool: SceneSegmentPool::default(),
            next_frame_callbacks,
            next_tooltip_id: TooltipId::default(),
            tooltip_bounds: None,
            active_overlay: None,
            render_layers: FxHashMap::default(),
            next_render_layer_seq: 0,
            global_overlay_owner_fiber: None,
            measurement_generation: 0,
            measurement_cache: MeasurementCache::with_hysteresis(
                crate::HysteresisPolicy::Pixels(px(0.5)),
            ),
            pending_view_accesses: FxHashMap::default(),
            focus_listeners: SubscriberSet::new(),
            focus_lost_listeners: SubscriberSet::new(),
            default_prevented: true,
            mouse_position,
            mouse_hit_test: HitTest::default(),
            pending_mouse_hit_test_refresh: false,
            modifiers,
            capslock,
            scale_factor,
            bounds_observers: SubscriberSet::new(),
            appearance,
            appearance_observers: SubscriberSet::new(),
            active,
            hovered,
            needs_present,
            input_rate_tracker,
            #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
            frame_diagnostics: FrameDiagnostics::default(),
            #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
            completed_frame_diagnostics: FrameDiagnostics::default(),
            last_input_modality: InputModality::Mouse,
            refreshing: false,
            measurement_depth: 0,
            full_reconciliation_this_frame: false,
            activation_observers: SubscriberSet::new(),
            focus: None,
            focus_before_deactivation: None,
            focus_enabled: true,
            pending_input: None,
            pending_modifier: ModifierState::default(),
            pending_input_observers: SubscriberSet::new(),
            prompt: None,
            client_inset: None,
            image_cache_stack: Vec::new(),
            #[cfg(any(feature = "inspector", debug_assertions))]
            inspector: None,
        })
    }

    pub(crate) fn new_focus_listener(
        &self,
        value: AnyWindowFocusListener,
    ) -> (Subscription, impl FnOnce() + use<>) {
        self.focus_listeners.insert((), value)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct DispatchEventResult {
    pub propagate: bool,
    pub default_prevented: bool,
}

/// Indicates which region of the window is visible. Content falling outside of this mask will not be
/// rendered. Currently, only rectangular content masks are supported, but we give the mask its own type
/// to leave room to support more complex shapes in the future.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[repr(C)]
pub struct ContentMask<P: Clone + Debug + Default + PartialEq> {
    /// The bounds
    pub bounds: Bounds<P>,
}

impl ContentMask<Pixels> {
    /// Scale the content mask's pixel units by the given scaling factor.
    pub fn scale(&self, factor: f32) -> ContentMask<ScaledPixels> {
        ContentMask {
            bounds: self.bounds.scale(factor),
        }
    }

    /// Intersect the content mask with the given content mask.
    pub fn intersect(&self, other: &Self) -> Self {
        let bounds = self.bounds.intersect(&other.bounds);
        ContentMask { bounds }
    }
}

impl Window {
    pub(crate) fn mark_view_dirty(&mut self, view_id: EntityId) {
        self.fibers().mark_view_dirty(view_id);
    }

    /// Registers a callback to be invoked when the window appearance changes.
    pub fn observe_window_appearance(
        &self,
        mut callback: impl FnMut(&mut Window, &mut App) + 'static,
    ) -> Subscription {
        let (subscription, activate) = self.appearance_observers.insert(
            (),
            Box::new(move |window, cx| {
                callback(window, cx);
                true
            }),
        );
        activate();
        subscription
    }

    /// Replaces the root entity of the window with a new one.
    pub fn replace_root<E>(
        &mut self,
        cx: &mut App,
        build_view: impl FnOnce(&mut Window, &mut Context<E>) -> E,
    ) -> Entity<E>
    where
        E: 'static + Render,
    {
        let view = cx.new(|cx| build_view(self, cx));
        self.root = Some(view.clone().into());
        self.ensure_view_root_fiber(view.entity_id());
        self.refresh();
        view
    }

    /// Returns the root entity of the window, if it has one.
    pub fn root<E>(&self) -> Option<Option<Entity<E>>>
    where
        E: 'static + Render,
    {
        self.root
            .as_ref()
            .map(|view| view.clone().downcast::<E>().ok())
    }

    /// Obtain a handle to the window that belongs to this context.
    pub fn window_handle(&self) -> AnyWindowHandle {
        self.handle
    }

    /// Mark the window as dirty, scheduling it to be redrawn on the next frame.
    pub fn refresh(&mut self) {
        if self.invalidator.not_drawing() {
            self.refreshing = true;
            self.invalidator.set_dirty(true);
        }
    }

    /// Schedule a redraw without forcing a full refresh/reconciliation path.
    ///
    /// This is useful for paint-only invalidation driven by runtime state (hover, scroll offset,
    /// etc.) in the retained fiber architecture.
    pub(crate) fn request_redraw(&mut self) {
        if self.invalidator.not_drawing() {
            self.invalidator.set_dirty(true);
        }
    }

    /// Mark a specific fiber as needing paint and schedule a redraw.
    pub(crate) fn invalidate_fiber_paint(&mut self, fiber_id: GlobalElementId) {
        self.fiber
            .tree
            .mark_dirty(&fiber_id, DirtyFlags::NEEDS_PAINT);
        self.request_redraw();
    }

    /// Mark a specific fiber as needing prepaint/paint and schedule a redraw.
    ///
    /// This is a lower-level escape hatch intended for retained/fiber-based components that need
    /// to update visuals based on runtime state without re-running full view reconciliation.
    pub fn invalidate_fiber_paint_public(&mut self, fiber_id: GlobalElementId) {
        self.invalidate_fiber_paint(fiber_id);
    }

    /// Mark a specific fiber as having a transform-only change and schedule a redraw.
    pub(crate) fn invalidate_fiber_transform(&mut self, fiber_id: GlobalElementId) {
        self.fiber
            .tree
            .mark_dirty(&fiber_id, DirtyFlags::TRANSFORM_CHANGED);
        self.request_redraw();
    }

    pub(crate) fn ensure_scroll_transform(
        &mut self,
        fiber_id: GlobalElementId,
        parent: TransformId,
        scroll_offset: Point<Pixels>,
    ) -> TransformId {
        let scale_factor = self.scale_factor();
        let offset = Point::new(
            ScaledPixels(scroll_offset.x.0 * scale_factor),
            ScaledPixels(scroll_offset.y.0 * scale_factor),
        );
        let transform = Transform2D {
            offset,
            scale: 1.0,
            parent,
        };

        if let Some(id) = self.scroll_transforms.get(&fiber_id).copied() {
            self.segment_pool.transforms.insert(id, transform);
            id
        } else {
            let id = self.segment_pool.transforms.push(transform);
            self.scroll_transforms.insert(fiber_id, id);
            id
        }
    }

    /// Push an unculling scope and a persistent scroll transform for the given fiber.
    ///
    /// This is intended for custom scroll containers implemented outside of `gpui`.
    pub fn push_fiber_scroll_transform(
        &mut self,
        fiber_id: GlobalElementId,
        scroll_offset: Point<Pixels>,
    ) -> FiberScrollTransformScope {
        self.push_unculled_scene();

        let parent = self.transform_stack.current();
        let transform_id = self.ensure_scroll_transform(fiber_id, parent, scroll_offset);
        self.transform_stack.push_existing_transform(transform_id);

        FiberScrollTransformScope {
            pushed_unculled_scene: true,
            pushed_transform: true,
        }
    }

    /// Pop a scope previously returned by [`Window::push_fiber_scroll_transform`].
    pub fn pop_fiber_scroll_transform(&mut self, scope: FiberScrollTransformScope) {
        if scope.pushed_transform {
            self.transform_stack.pop_transform();
        }
        if scope.pushed_unculled_scene {
            self.pop_unculled_scene();
        }
    }

    /// Mark a scroll container as having a transform-only change and update its persistent scroll
    /// transform in O(1).
    ///
    /// This is intended for custom scroll containers implemented outside of `gpui`.
    pub fn invalidate_scroll_transform(
        &mut self,
        fiber_id: GlobalElementId,
        scroll_offset: Point<Pixels>,
        cx: &mut App,
    ) {
        self.invalidate_fiber_scroll(fiber_id, scroll_offset, cx);
    }

    /// Mark a scroll container as having a transform-only change and update its scroll transform in
    /// O(1).
    pub(crate) fn invalidate_fiber_scroll(
        &mut self,
        fiber_id: GlobalElementId,
        scroll_offset: Point<Pixels>,
        cx: &mut App,
    ) {
        self.invalidate_fiber_transform(fiber_id);

        if let Some(transform_id) = self.scroll_transforms.get(&fiber_id).copied() {
            let scale_factor = self.scale_factor();
            let scaled = Point::new(
                ScaledPixels(scroll_offset.x.0 * scale_factor),
                ScaledPixels(scroll_offset.y.0 * scale_factor),
            );
            self.segment_pool
                .transforms
                .update_offset(transform_id, scaled);
        }

        self.pending_mouse_hit_test_refresh = true;
        self.apply_pending_mouse_hit_test_refresh(cx);
    }

    pub(crate) fn apply_pending_mouse_hit_test_refresh(&mut self, cx: &mut App) {
        if !self.pending_mouse_hit_test_refresh {
            return;
        }
        self.pending_mouse_hit_test_refresh = false;

        let previous_hit_test = HitTest {
            ids: self.mouse_hit_test.ids.clone(),
            hover_hitbox_count: self.mouse_hit_test.hover_hitbox_count,
        };

        let current_hit_test = self.rendered_frame.hit_test(self, self.mouse_position);
        if current_hit_test == self.mouse_hit_test {
            return;
        }

        self.mouse_hit_test = current_hit_test;
        self.reset_cursor_style(cx);
        self.update_hover_states_for_hit_test_change(&previous_hit_test, cx);
    }

    fn update_hover_states_for_hit_test_change(
        &mut self,
        previous_hit_test: &HitTest,
        cx: &mut App,
    ) {
        let current_hit_test = &self.mouse_hit_test;

        let mut target_hitboxes: SmallVec<[HitboxId; 16]> = SmallVec::new();
        let mut seen: FxHashSet<HitboxId> = FxHashSet::default();

        for id in current_hit_test
            .ids
            .iter()
            .take(current_hit_test.hover_hitbox_count)
            .copied()
        {
            if seen.insert(id) {
                target_hitboxes.push(id);
            }
        }
        for id in previous_hit_test
            .ids
            .iter()
            .take(previous_hit_test.hover_hitbox_count)
            .copied()
        {
            if seen.insert(id) {
                target_hitboxes.push(id);
            }
        }

        for hitbox_id in target_hitboxes {
            let fiber_id: GlobalElementId = hitbox_id.into();
            if self.fiber.tree.get(&fiber_id).is_none() {
                continue;
            }

            let (has_hover_style, group_hover) = {
                let Some(interactivity) = self
                    .fiber
                    .tree
                    .render_nodes
                    .get(fiber_id.into())
                    .and_then(|node| node.interactivity())
                else {
                    continue;
                };
                (
                    interactivity.hover_style.is_some(),
                    interactivity
                        .group_hover_style
                        .as_ref()
                        .map(|group_hover| group_hover.group.clone()),
                )
            };

            if has_hover_style {
                let _ = self.with_element_state_in_event::<crate::InteractiveElementState, _>(
                    &fiber_id,
                    |element_state, window| {
                        let mut element_state = element_state.unwrap_or_default();
                        let hover_state = element_state
                            .hover_state
                            .get_or_insert_with(Default::default)
                            .clone();
                        let hovered = window.hitbox_is_hovered(hitbox_id);
                        let mut hover_state = hover_state.borrow_mut();
                        if hovered != hover_state.element {
                            hover_state.element = hovered;
                            drop(hover_state);
                            window.invalidate_fiber_paint(fiber_id);
                        }
                        ((), element_state)
                    },
                );
            }

            if let Some(group) = group_hover {
                if let Some(group_hitbox_id) = crate::GroupHitboxes::get(&group, cx) {
                    let _ = self.with_element_state_in_event::<crate::InteractiveElementState, _>(
                        &fiber_id,
                        |element_state, window| {
                            let mut element_state = element_state.unwrap_or_default();
                            let hover_state = element_state
                                .hover_state
                                .get_or_insert_with(Default::default)
                                .clone();
                            let group_hovered = window.hitbox_is_hovered(group_hitbox_id);
                            let mut hover_state = hover_state.borrow_mut();
                            if group_hovered != hover_state.group {
                                hover_state.group = group_hovered;
                                drop(hover_state);
                                window.invalidate_fiber_paint(fiber_id);
                            }
                            ((), element_state)
                        },
                    );
                }
            }
        }
    }

    /// Close this window.
    pub fn remove_window(&mut self) {
        self.removed = true;
    }

    /// Obtain the currently focused [`FocusHandle`]. If no elements are focused, returns `None`.
    pub fn focused(&self, cx: &App) -> Option<FocusHandle> {
        self.focus
            .and_then(|id| FocusHandle::for_id(id, &cx.focus_handles))
    }

    fn focus_contains(&self, parent: FocusId, child: FocusId) -> bool {
        self.fibers_ref().focus_contains(parent, child)
    }

    /// Move focus to the element associated with the given [`FocusHandle`].
    pub fn focus(&mut self, handle: &FocusHandle, cx: &mut App) {
        if !self.focus_enabled || self.focus == Some(handle.id) {
            return;
        }

        self.focus = Some(handle.id);
        self.clear_pending_keystrokes();
        self.update_platform_input_handler(cx);

        // Avoid re-entrant entity updates by deferring observer notifications to the end of the
        // current effect cycle, and only for this window.
        let window_handle = self.handle;
        cx.defer(move |cx| {
            window_handle
                .update(cx, |_, window, cx| {
                    window.pending_input_changed(cx);
                })
                .ok();
        });

        self.refresh();
    }

    /// Remove focus from all elements within this context's window.
    pub fn blur(&mut self) {
        if !self.focus_enabled {
            return;
        }

        self.focus = None;
        if self.invalidator.not_drawing() {
            let _ = self.platform_window.take_input_handler();
        }
        self.refresh();
    }

    /// Blur the window and don't allow anything in it to be focused again.
    pub fn disable_focus(&mut self) {
        self.blur();
        self.focus_enabled = false;
    }

    /// Move focus to next tab stop.
    pub fn focus_next(&mut self, cx: &mut App) {
        if !self.focus_enabled {
            return;
        }

        if let Some(handle) = self.fibers_ref().next_tab_stop(self.focus.as_ref()) {
            self.focus(&handle, cx)
        }
    }

    /// Move focus to previous tab stop.
    pub fn focus_prev(&mut self, cx: &mut App) {
        if !self.focus_enabled {
            return;
        }

        if let Some(handle) = self.fibers_ref().prev_tab_stop(self.focus.as_ref()) {
            self.focus(&handle, cx)
        }
    }

    /// Accessor for the text system.
    pub fn text_system(&self) -> &Arc<WindowTextSystem> {
        &self.text_system
    }

    /// The current text style. Which is composed of all the style refinements provided to `with_text_style`.
    pub fn text_style(&self) -> TextStyle {
        let mut style = TextStyle::default();
        for refinement in &self.text_style_stack {
            style.refine(refinement);
        }
        style
    }

    /// Check if the platform window is maximized.
    ///
    /// On some platforms (namely Windows) this is different than the bounds being the size of the display
    pub fn is_maximized(&self) -> bool {
        self.platform_window.is_maximized()
    }

    /// request a certain window decoration (Wayland)
    pub fn request_decorations(&self, decorations: WindowDecorations) {
        self.platform_window.request_decorations(decorations);
    }

    /// Start a window resize operation (Wayland)
    pub fn start_window_resize(&self, edge: ResizeEdge) {
        self.platform_window.start_window_resize(edge);
    }

    /// Return the `WindowBounds` to indicate that how a window should be opened
    /// after it has been closed
    pub fn window_bounds(&self) -> WindowBounds {
        self.platform_window.window_bounds()
    }

    /// Return the `WindowBounds` excluding insets (Wayland and X11)
    pub fn inner_window_bounds(&self) -> WindowBounds {
        self.platform_window.inner_window_bounds()
    }

    /// Dispatch the given action on the currently focused element.
    pub fn dispatch_action(&mut self, action: Box<dyn Action>, cx: &mut App) {
        let focus_id = self.focused(cx).map(|handle| handle.id);

        let window = self.handle;
        cx.defer(move |cx| {
            window
                .update(cx, |_, window, cx| {
                    let node_id = window.focus_node_id_in_rendered_frame(focus_id);
                    window.dispatch_action_on_node(node_id, action.as_ref(), cx);
                })
                .log_err();
        })
    }

    pub(crate) fn dispatch_keystroke_observers(
        &mut self,
        event: &dyn Any,
        action: Option<Box<dyn Action>>,
        context_stack: Vec<KeyContext>,
        cx: &mut App,
    ) {
        let Some(key_down_event) = event.downcast_ref::<KeyDownEvent>() else {
            return;
        };

        cx.keystroke_observers.clone().retain(&(), move |callback| {
            (callback)(
                &KeystrokeEvent {
                    keystroke: key_down_event.keystroke.clone(),
                    action: action.as_ref().map(|action| action.boxed_clone()),
                    context_stack: context_stack.clone(),
                },
                self,
                cx,
            )
        });
    }

    pub(crate) fn dispatch_keystroke_interceptors(
        &mut self,
        event: &dyn Any,
        context_stack: Vec<KeyContext>,
        cx: &mut App,
    ) {
        let Some(key_down_event) = event.downcast_ref::<KeyDownEvent>() else {
            return;
        };

        cx.keystroke_interceptors
            .clone()
            .retain(&(), move |callback| {
                (callback)(
                    &KeystrokeEvent {
                        keystroke: key_down_event.keystroke.clone(),
                        action: None,
                        context_stack: context_stack.clone(),
                    },
                    self,
                    cx,
                )
            });
    }

    /// Schedules the given function to be run at the end of the current effect cycle, allowing entities
    /// that are currently on the stack to be returned to the app.
    pub fn defer(&self, cx: &mut App, f: impl FnOnce(&mut Window, &mut App) + 'static) {
        let handle = self.handle;
        cx.defer(move |cx| {
            handle.update(cx, |_, window, cx| f(window, cx)).ok();
        });
    }

    /// Subscribe to events emitted by a entity.
    /// The entity to which you're subscribing must implement the [`EventEmitter`] trait.
    /// The callback will be invoked a handle to the emitting entity, the event, and a window context for the current window.
    pub fn observe<T: 'static>(
        &mut self,
        observed: &Entity<T>,
        cx: &mut App,
        mut on_notify: impl FnMut(Entity<T>, &mut Window, &mut App) + 'static,
    ) -> Subscription {
        let entity_id = observed.entity_id();
        let observed = observed.downgrade();
        let window_handle = self.handle;
        cx.new_observer(
            entity_id,
            Box::new(move |cx| {
                window_handle
                    .update(cx, |_, window, cx| {
                        if let Some(handle) = observed.upgrade() {
                            on_notify(handle, window, cx);
                            true
                        } else {
                            false
                        }
                    })
                    .unwrap_or(false)
            }),
        )
    }

    /// Subscribe to events emitted by a entity.
    /// The entity to which you're subscribing must implement the [`EventEmitter`] trait.
    /// The callback will be invoked a handle to the emitting entity, the event, and a window context for the current window.
    pub fn subscribe<Emitter, Evt>(
        &mut self,
        entity: &Entity<Emitter>,
        cx: &mut App,
        mut on_event: impl FnMut(Entity<Emitter>, &Evt, &mut Window, &mut App) + 'static,
    ) -> Subscription
    where
        Emitter: EventEmitter<Evt>,
        Evt: 'static,
    {
        let entity_id = entity.entity_id();
        let handle = entity.downgrade();
        let window_handle = self.handle;
        cx.new_subscription(
            entity_id,
            (
                TypeId::of::<Evt>(),
                Box::new(move |event, cx| {
                    window_handle
                        .update(cx, |_, window, cx| {
                            if let Some(entity) = handle.upgrade() {
                                let event = event.downcast_ref().expect("invalid event type");
                                on_event(entity, event, window, cx);
                                true
                            } else {
                                false
                            }
                        })
                        .unwrap_or(false)
                }),
            ),
        )
    }

    /// Register a callback to be invoked when the given `Entity` is released.
    pub fn observe_release<T>(
        &self,
        entity: &Entity<T>,
        cx: &mut App,
        mut on_release: impl FnOnce(&mut T, &mut Window, &mut App) + 'static,
    ) -> Subscription
    where
        T: 'static,
    {
        let entity_id = entity.entity_id();
        let window_handle = self.handle;
        let (subscription, activate) = cx.release_listeners.insert(
            entity_id,
            Box::new(move |entity, cx| {
                let entity = entity.downcast_mut().expect("invalid entity type");
                let _ = window_handle.update(cx, |_, window, cx| on_release(entity, window, cx));
            }),
        );
        activate();
        subscription
    }

    /// Creates an [`AsyncWindowContext`], which has a static lifetime and can be held across
    /// await points in async code.
    pub fn to_async(&self, cx: &App) -> AsyncWindowContext {
        AsyncWindowContext::new_context(cx.to_async(), self.handle)
    }

    /// Schedule the given closure to be run directly after the current frame is rendered.
    pub fn on_next_frame(&self, callback: impl FnOnce(&mut Window, &mut App) + 'static) {
        RefCell::borrow_mut(&self.next_frame_callbacks).push(Box::new(callback));
    }

    /// Schedule a frame to be drawn on the next animation frame.
    ///
    /// This is useful for elements that need to animate continuously, such as a video player or an animated GIF.
    /// It will cause the window to redraw on the next frame, even if no other changes have occurred.
    ///
    /// If called from within a view, it will notify that view on the next frame. Otherwise, it will refresh the entire window.
    pub fn request_animation_frame(&self) {
        // Get the current view without phase assertion - this can be called during reconciliation
        // when views are being rendered (expand_view_fibers phase)
        let entity = if let Some(id) = self.rendered_entity_stack.last().copied() {
            id
        } else {
            self.root.as_ref().map(|root| root.entity_id()).expect(
                "Window::request_animation_frame called with no rendered view and no root view",
            )
        };
        self.on_next_frame(move |_, cx| cx.notify(entity));
    }

    /// Spawn the future returned by the given closure on the application thread pool.
    /// The closure is provided a handle to the current window and an `AsyncWindowContext` for
    /// use within your future.
    #[track_caller]
    pub fn spawn<AsyncFn, R>(&self, cx: &App, f: AsyncFn) -> Task<R>
    where
        R: 'static,
        AsyncFn: AsyncFnOnce(&mut AsyncWindowContext) -> R + 'static,
    {
        let handle = self.handle;
        cx.spawn(async move |app| {
            let mut async_window_cx = AsyncWindowContext::new_context(app.clone(), handle);
            f(&mut async_window_cx).await
        })
    }

    /// Spawn the future returned by the given closure on the application thread
    /// pool, with the given priority. The closure is provided a handle to the
    /// current window and an `AsyncWindowContext` for use within your future.
    #[track_caller]
    pub fn spawn_with_priority<AsyncFn, R>(
        &self,
        priority: Priority,
        cx: &App,
        f: AsyncFn,
    ) -> Task<R>
    where
        R: 'static,
        AsyncFn: AsyncFnOnce(&mut AsyncWindowContext) -> R + 'static,
    {
        let handle = self.handle;
        cx.spawn_with_priority(priority, async move |app| {
            let mut async_window_cx = AsyncWindowContext::new_context(app.clone(), handle);
            f(&mut async_window_cx).await
        })
    }

    fn bounds_changed(&mut self, cx: &mut App) {
        self.scale_factor = self.platform_window.scale_factor();
        self.viewport_size = self.platform_window.content_size();
        self.display_id = self.platform_window.display().map(|display| display.id());
        self.measurement_generation = self.measurement_generation.wrapping_add(1);
        self.measurement_cache
            .evict_stale_generations(self.measurement_generation);

        self.refresh();

        self.bounds_observers
            .clone()
            .retain(&(), |callback| callback(self, cx));
    }

    /// Returns the bounds of the current window in the global coordinate space, which could span across multiple displays.
    pub fn bounds(&self) -> Bounds<Pixels> {
        self.platform_window.bounds()
    }

    /// Renders the current frame's scene to a texture and returns the pixel data as an RGBA image.
    /// This does not present the frame to screen - useful for visual testing where we want
    /// to capture what would be rendered without displaying it or requiring the window to be visible.
    #[cfg(any(test, feature = "test-support"))]
    pub fn render_to_image(&self) -> anyhow::Result<image::RgbaImage> {
        self.platform_window
            .render_to_image(&self.rendered_frame.scene, &self.segment_pool)
    }

    /// Set the content size of the window.
    pub fn resize(&mut self, size: Size<Pixels>) {
        self.platform_window.resize(size);
    }

    /// Returns whether or not the window is currently fullscreen
    pub fn is_fullscreen(&self) -> bool {
        self.platform_window.is_fullscreen()
    }

    pub(crate) fn appearance_changed(&mut self, cx: &mut App) {
        self.appearance = self.platform_window.appearance();

        self.appearance_observers
            .clone()
            .retain(&(), |callback| callback(self, cx));
    }

    /// Returns the appearance of the current window.
    pub fn appearance(&self) -> WindowAppearance {
        self.appearance
    }

    /// Returns the size of the drawable area within the window.
    pub fn viewport_size(&self) -> Size<Pixels> {
        self.viewport_size
    }

    /// Returns whether this window is focused by the operating system (receiving key events).
    pub fn is_window_active(&self) -> bool {
        self.active.get()
    }

    /// Returns whether this window is considered to be the window
    /// that currently owns the mouse cursor.
    /// On mac, this is equivalent to `is_window_active`.
    pub fn is_window_hovered(&self) -> bool {
        if cfg!(any(
            target_os = "windows",
            target_os = "linux",
            target_os = "freebsd"
        )) {
            self.hovered.get()
        } else {
            self.is_window_active()
        }
    }

    /// Toggle zoom on the window.
    pub fn zoom_window(&self) {
        self.platform_window.zoom();
    }

    /// Opens the native title bar context menu, useful when implementing client side decorations (Wayland and X11)
    pub fn show_window_menu(&self, position: Point<Pixels>) {
        self.platform_window.show_window_menu(position)
    }

    /// Handle window movement for Linux and macOS.
    /// Tells the compositor to take control of window movement (Wayland and X11)
    ///
    /// Events may not be received during a move operation.
    pub fn start_window_move(&self) {
        self.platform_window.start_window_move()
    }

    /// When using client side decorations, set this to the width of the invisible decorations (Wayland and X11)
    pub fn set_client_inset(&mut self, inset: Pixels) {
        self.client_inset = Some(inset);
        self.platform_window.set_client_inset(inset);
    }

    /// Returns the client_inset value by [`Self::set_client_inset`].
    pub fn client_inset(&self) -> Option<Pixels> {
        self.client_inset
    }

    /// Returns whether the title bar window controls need to be rendered by the application (Wayland and X11)
    pub fn window_decorations(&self) -> Decorations {
        self.platform_window.window_decorations()
    }

    /// Returns which window controls are currently visible (Wayland)
    pub fn window_controls(&self) -> WindowControls {
        self.platform_window.window_controls()
    }

    /// Updates the window's title at the platform level.
    pub fn set_window_title(&mut self, title: &str) {
        self.platform_window.set_title(title);
    }

    /// Sets the application identifier.
    pub fn set_app_id(&mut self, app_id: &str) {
        self.platform_window.set_app_id(app_id);
    }

    /// Sets the window background appearance.
    pub fn set_background_appearance(&self, background_appearance: WindowBackgroundAppearance) {
        self.platform_window
            .set_background_appearance(background_appearance);
    }

    /// Mark the window as dirty at the platform level.
    pub fn set_window_edited(&mut self, edited: bool) {
        self.platform_window.set_edited(edited);
    }

    /// Determine the display on which the window is visible.
    pub fn display(&self, cx: &App) -> Option<Rc<dyn PlatformDisplay>> {
        cx.platform
            .displays()
            .into_iter()
            .find(|display| Some(display.id()) == self.display_id)
    }

    /// Show the platform character palette.
    pub fn show_character_palette(&self) {
        self.platform_window.show_character_palette();
    }

    /// The scale factor of the display associated with the window. For example, it could
    /// return 2.0 for a "retina" display, indicating that each logical pixel should actually
    /// be rendered as two pixels on screen.
    pub fn scale_factor(&self) -> f32 {
        self.scale_factor
    }

    /// The size of an em for the base font of the application. Adjusting this value allows the
    /// UI to scale, just like zooming a web page.
    pub fn rem_size(&self) -> Pixels {
        self.rem_size_override_stack
            .last()
            .copied()
            .unwrap_or(self.rem_size)
    }

    /// Sets the size of an em for the base font of the application. Adjusting this value allows the
    /// UI to scale, just like zooming a web page.
    pub fn set_rem_size(&mut self, rem_size: impl Into<Pixels>) {
        self.rem_size = rem_size.into();
        self.measurement_generation = self.measurement_generation.wrapping_add(1);
        self.measurement_cache
            .evict_stale_generations(self.measurement_generation);
    }

    /// Returns the current measurement cache generation.
    pub fn measurement_generation(&self) -> u64 {
        self.measurement_generation
    }

    /// Look up a cached measured size for the given key and constraints.
    pub fn measured_size_for(
        &self,
        key: MeasurementKey,
        constraints: Size<AvailableSpace>,
    ) -> Option<Size<Pixels>> {
        let bucket = ConstraintsBucket::from_available_space_default(constraints);
        self.measurement_cache
            .get_stale_ok(key, bucket, self.measurement_generation)
            .map(|(size, _generation)| size)
    }

    /// Check if there is a cached measured size for the given key and constraints.
    pub fn has_measured_size_for(
        &self,
        key: MeasurementKey,
        constraints: Size<AvailableSpace>,
    ) -> bool {
        self.measured_size_for(key, constraints).is_some()
    }

    /// Record a measured size for the given key and constraints. Returns true if the size changed.
    pub fn record_measured_size_for(
        &mut self,
        key: MeasurementKey,
        constraints: Size<AvailableSpace>,
        size: Size<Pixels>,
    ) -> bool {
        let bucket = ConstraintsBucket::from_available_space_default(constraints);
        let changed = self
            .measurement_cache
            .insert(key, bucket, self.measurement_generation, size);
        if changed {
            self.measurement_cache
                .evict_stale_generations(self.measurement_generation);
        }
        changed
    }

    /// Wrap an element with measurement caching using an auto-generated key.
    #[track_caller]
    pub fn with_cached_measurement(
        &mut self,
        element: AnyElement,
        constraints: Size<AvailableSpace>,
        default_size: Size<Pixels>,
    ) -> (AnyElement, Size<Pixels>) {
        let key = MeasurementKey::for_callsite(0);
        self.with_cached_measurement_keyed(key, element, constraints, default_size)
    }

    /// Wrap an element with measurement caching using a custom key.
    pub fn with_cached_measurement_keyed(
        &mut self,
        key: MeasurementKey,
        element: AnyElement,
        constraints: Size<AvailableSpace>,
        default_size: Size<Pixels>,
    ) -> (AnyElement, Size<Pixels>) {
        let size = self
            .measured_size_for(key, constraints)
            .unwrap_or(default_size);

        let element = div()
            .child(element)
            .on_layout(move |bounds, window, _cx| {
                if window.record_measured_size_for(key, constraints, bounds.size) {
                    window.refresh();
                }
            })
            .into_any();

        (element, size)
    }

    /// Acquire a globally unique identifier for the given ElementId.
    /// Only valid for the duration of the provided closure.
    pub fn with_global_id<R>(
        &mut self,
        element_id: ElementId,
        f: impl FnOnce(&GlobalElementId, &mut Self) -> R,
    ) -> R {
        self.invalidator
            .debug_assert_reconcile_or_layout_or_prepaint_or_paint();

        let parent_id = self.ensure_fiber_for_current_id();
        let mut reused = true;
        let scoped_id =
            self.with_element_state(&parent_id, |state: Option<GlobalIdScopeState>, window| {
                let mut state = state.unwrap_or_default();

                let scoped_id = match state.children.get(&element_id).copied() {
                    Some(existing_id) if window.fiber.tree.get(&existing_id).is_some() => {
                        existing_id
                    }
                    _ => {
                        reused = false;
                        let new_id = window.fiber.tree.create_state_only_child(parent_id);
                        if let Some(fiber) = window.fiber.tree.get_mut(&new_id) {
                            fiber.key = Some(element_id.clone());
                            fiber.child_count = 0;
                        }

                        state.children.insert(element_id.clone(), new_id);
                        new_id
                    }
                };

                (scoped_id, state)
            });

        let stack = &self.fiber.fiber_id_stack;
        let len = stack.len();
        let preview_len = len.min(16);
        let start = len.saturating_sub(preview_len);
        let preview = &stack[start..];
        log::trace!(
            "[WITH_GLOBAL_ID] element_id={:?} parent_id={:?} scoped_id={:?} reused={} fiber_stack=len={} tail={:?}",
            element_id,
            parent_id,
            scoped_id,
            reused,
            len,
            preview
        );

        self.with_element_id_stack(&scoped_id, |window| f(&scoped_id, window))
    }

    /// Executes the provided function with the specified rem size.
    ///
    /// This method must only be called as part of element layout or drawing.
    // This function is called in a highly recursive manner in editor
    // prepainting, make sure its inlined to reduce the stack burden
    #[inline]
    pub fn with_rem_size<F, R>(&mut self, rem_size: Option<impl Into<Pixels>>, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.invalidator.debug_assert_layout_or_prepaint_or_paint();

        if let Some(rem_size) = rem_size {
            self.rem_size_override_stack.push(rem_size.into());
            let result = f(self);
            self.rem_size_override_stack.pop();
            result
        } else {
            f(self)
        }
    }

    /// The line height associated with the current text style.
    pub fn line_height(&self) -> Pixels {
        self.text_style().line_height_in_pixels(self.rem_size())
    }

    /// Call to prevent the default action of an event. Currently only used to prevent
    /// parent elements from becoming focused on mouse down.
    pub fn prevent_default(&mut self) {
        self.default_prevented = true;
    }

    /// Obtain whether default has been prevented for the event currently being dispatched.
    pub fn default_prevented(&self) -> bool {
        self.default_prevented
    }

    /// Register a window-global render layer.
    ///
    /// Render layers are invoked once per window per frame, after the root view
    /// has been prepainted (so they can rely on layout-bound state) and before
    /// hit-testing is finalized.
    ///
    /// Layers are painted after the root view and before deferred draws,
    /// prompts, and tooltips. Ordering between layers is controlled by `order`
    /// (lower first). Ties are broken by first-registration order.
    pub fn register_render_layer<F>(&mut self, key: impl Into<ElementId>, order: i32, build: F)
    where
        F: Fn(&mut Window, &mut App) -> AnyElement + 'static,
    {
        let key = key.into();
        let build: RenderLayerBuilder = Arc::new(build);

        if let Some(registration) = self.render_layers.get_mut(&key) {
            registration.order = order;
            registration.build = build;
            return;
        }

        let seq = self.next_render_layer_seq;
        self.next_render_layer_seq = self.next_render_layer_seq.saturating_add(1);
        self.render_layers
            .insert(key, RenderLayerRegistration { order, seq, build });
    }

    /// Unregister a render layer by key.
    pub fn unregister_render_layer(&mut self, key: &ElementId) {
        self.render_layers.remove(key);
    }

    /// Returns true if the given render layer key is registered.
    pub fn has_render_layer(&self, key: &ElementId) -> bool {
        self.render_layers.contains_key(key)
    }

    /// Determine whether the given action is available along the dispatch path to the currently focused element.
    pub fn is_action_available(&self, action: &dyn Action, cx: &App) -> bool {
        let node_id =
            self.focus_node_id_in_rendered_frame(self.focused(cx).map(|handle| handle.id));
        self.fibers_ref()
            .is_action_available_for_node(action, node_id)
    }

    /// Determine whether the given action is available along the dispatch path to the given focus_handle.
    pub fn is_action_available_in(&self, action: &dyn Action, focus_handle: &FocusHandle) -> bool {
        let node_id = self.focus_node_id_in_rendered_frame(Some(focus_handle.id));
        self.fibers_ref()
            .is_action_available_for_node(action, node_id)
    }

    /// The position of the mouse relative to the window.
    pub fn mouse_position(&self) -> Point<Pixels> {
        self.mouse_position
    }

    /// Hit-test the rendered frame at the given window position.
    ///
    /// Returns hitbox IDs from topmost to bottommost.
    pub fn hit_test_ids(&self, position: Point<Pixels>) -> SmallVec<[HitboxId; 8]> {
        self.rendered_frame.hit_test(self, position).ids
    }

    /// Returns diagnostic counters for the most recently completed frame.
    #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
    #[allow(clippy::misnamed_getters)]
    pub fn frame_diagnostics(&self) -> FrameDiagnostics {
        self.completed_frame_diagnostics
    }

    /// The current state of the keyboard's modifiers
    pub fn modifiers(&self) -> Modifiers {
        self.modifiers
    }

    /// Returns true if the last input event was keyboard-based (key press, tab navigation, etc.)
    /// This is used for focus-visible styling to show focus indicators only for keyboard navigation.
    pub fn last_input_was_keyboard(&self) -> bool {
        self.last_input_modality == InputModality::Keyboard
    }

    /// The current state of the keyboard's capslock
    pub fn capslock(&self) -> Capslock {
        self.capslock
    }

    fn complete_frame(&self) {
        self.platform_window.completed_frame();
    }

    /// Produces a new frame and assigns it to `rendered_frame`. To actually show
    /// the contents of the new [`Scene`], use [`Self::present`].
    #[profiling::function]
    pub fn draw(&mut self, cx: &mut App) {
        self.prepare_frame(cx);
        if !cx.mode.skip_drawing() {
            self.draw_roots(cx);
        }
        for segment_id in self.fiber.tree.take_removed_scene_segments() {
            self.segment_pool.remove_segment(segment_id);
        }
        self.rebuild_scene_segment_order_if_needed();
        self.cleanup_removed_fibers();
        self.fibers().rebuild_collection_ordering();
        #[cfg(debug_assertions)]
        self.debug_assert_incremental_collections();
        self.finalize_frame(cx);
        self.fiber.log_frame_diagnostics();
    }

    fn prepare_frame(&mut self, cx: &mut App) {
        self.invalidate_entities();
        cx.entities.clear_accessed();
        self.full_reconciliation_this_frame = false;
        debug_assert!(self.rendered_entity_stack.is_empty());
        self.invalidator.set_dirty(false);
        self.requested_autoscroll = None;
        self.fiber.layout_bounds_cache.clear();
        self.fiber.fiber_id_stack.clear();
        self.fiber.hitbox_stack.clear();
        #[cfg(any(test, feature = "test-support"))]
        self.next_frame
            .debug_bounds
            .clone_from(&self.rendered_frame.debug_bounds);

        self.next_frame.scene.begin_frame();

        // Restore the previously-used input handler.
        self.platform_window.take_input_handler();
    }

    fn finalize_frame(&mut self, cx: &mut App) {
        self.next_frame.window_active = self.active.get();

        // Register requested input handler with the platform window.
        if let Some(input_handler) = self.fibers().latest_input_handler(cx) {
            self.platform_window.set_input_handler(input_handler);
        }

        self.text_system().finish_frame();
        let scene_finish_stats = {
            let fiber_tree = &self.fiber.tree;
            let hitboxes_epoch = HitboxesSnapshotEpoch {
                structure_epoch: fiber_tree.structure_epoch,
                hitbox_epoch: fiber_tree.hitbox_epoch(),
            };
            if self.next_frame.hitboxes_epoch != Some(hitboxes_epoch) {
                snapshot_hitboxes_into_map(fiber_tree, &mut self.next_frame.hitboxes);
                self.next_frame.hitboxes_epoch = Some(hitboxes_epoch);
                #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
                {
                    self.frame_diagnostics.hitboxes_snapshot_rebuilt = true;
                }
            }
            #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
            {
                self.frame_diagnostics.hitboxes_in_snapshot = self.next_frame.hitboxes.len();
                self.frame_diagnostics.hitbox_epoch = hitboxes_epoch.hitbox_epoch;
            }

            self.next_frame.finish(&mut self.segment_pool)
        };
        #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
        {
            self.frame_diagnostics.total_pool_segments = scene_finish_stats.total_pool_segments;
            self.frame_diagnostics.mutated_pool_segments = scene_finish_stats.mutated_pool_segments;
            self.frame_diagnostics.transient_segment_mutated = scene_finish_stats.transient_mutated;

            let scene = &self.next_frame.scene;
            let pool = &self.segment_pool;
            self.frame_diagnostics.paths = scene.paths_len(pool);
            self.frame_diagnostics.shadows = scene.shadows_len(pool);
            self.frame_diagnostics.quads = scene.quads_len(pool);
            self.frame_diagnostics.underlines = scene.underlines_len(pool);
            self.frame_diagnostics.monochrome_sprites = scene.monochrome_sprites_len(pool);
            self.frame_diagnostics.subpixel_sprites = scene.subpixel_sprites_len(pool);
            self.frame_diagnostics.polychrome_sprites = scene.polychrome_sprites_len(pool);
            self.frame_diagnostics.surfaces = scene.surfaces_len(pool);

            use std::mem::size_of;
            self.frame_diagnostics.estimated_instance_upload_bytes = self
                .frame_diagnostics
                .shadows
                .saturating_mul(
                    size_of::<crate::scene::Shadow>() + size_of::<TransformationMatrix>(),
                )
                .saturating_add(self.frame_diagnostics.quads.saturating_mul(
                    size_of::<crate::scene::Quad>() + size_of::<TransformationMatrix>(),
                ))
                .saturating_add(self.frame_diagnostics.underlines.saturating_mul(
                    size_of::<crate::scene::Underline>() + size_of::<TransformationMatrix>(),
                ))
                .saturating_add(
                    self.frame_diagnostics
                        .monochrome_sprites
                        .saturating_mul(size_of::<crate::scene::MonochromeSprite>()),
                )
                .saturating_add(
                    self.frame_diagnostics
                        .subpixel_sprites
                        .saturating_mul(size_of::<crate::scene::SubpixelSprite>()),
                )
                .saturating_add(self.frame_diagnostics.polychrome_sprites.saturating_mul(
                    size_of::<crate::scene::PolychromeSprite>() + size_of::<TransformationMatrix>(),
                ))
                .saturating_add(
                    self.frame_diagnostics
                        .paths
                        .saturating_mul(size_of::<crate::Path<ScaledPixels>>()),
                )
                .saturating_add(
                    self.frame_diagnostics
                        .surfaces
                        .saturating_mul(size_of::<crate::scene::PaintSurface>()),
                );
        }
        let _ = scene_finish_stats;
        self.next_frame.focus_path = self.fibers_ref().focus_path_for(self.focus);

        self.invalidator.set_phase(DrawPhase::Focus);
        let previous_focus_path = self.rendered_frame.focus_path().clone();
        let previous_window_active = self.rendered_frame.window_active;
        mem::swap(&mut self.rendered_frame, &mut self.next_frame);
        self.next_frame.clear();
        let current_focus_path = self.rendered_frame.focus_path().clone();
        let current_window_active = self.rendered_frame.window_active;

        if previous_focus_path != current_focus_path
            || previous_window_active != current_window_active
        {
            self.dispatch_focus_change_events(
                previous_focus_path,
                current_focus_path,
                previous_window_active,
                current_window_active,
                cx,
            );
        }

        debug_assert!(self.rendered_entity_stack.is_empty());
        // Only update the entity invalidator mapping during full reconciliation.
        // During incremental rendering (Branch 2), only some views are rendered,
        // so accessed_entities only contains a subset. Using that to update the
        // mapping would incorrectly remove entities from views that weren't rendered.
        if self.full_reconciliation_this_frame {
            self.record_entities_accessed(cx);
        }
        self.reset_cursor_style(cx);
        self.refreshing = false;
        self.invalidator.set_phase(DrawPhase::None);
        self.needs_present.set(true);

        #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
        {
            self.completed_frame_diagnostics = self.frame_diagnostics;
        }
    }

    /// Dispatch any pending focus change events without performing a full draw cycle.
    ///
    /// This is useful when you need focus change listeners to be notified immediately,
    /// such as between programmatic keystrokes where focus may change and subsequent
    /// keystrokes depend on the new focus state (e.g., vim mode).
    ///
    /// In the fiber architecture, the focus path can be computed at any time from the
    /// persistent fiber tree, enabling this lightweight focus event dispatch without
    /// requiring a full layout/paint cycle.
    pub fn dispatch_pending_focus_events(&mut self, cx: &mut App) {
        let current_focus_path = self.fibers_ref().focus_path_for(self.focus);
        let previous_focus_path = self.rendered_frame.focus_path.clone();

        if previous_focus_path != current_focus_path {
            self.dispatch_focus_change_events(
                previous_focus_path,
                current_focus_path.clone(),
                self.rendered_frame.window_active,
                self.rendered_frame.window_active,
                cx,
            );
            self.rendered_frame.focus_path = current_focus_path;
        }
    }

    fn dispatch_focus_change_events(
        &mut self,
        previous_focus_path: SmallVec<[FocusId; 8]>,
        current_focus_path: SmallVec<[FocusId; 8]>,
        previous_window_active: bool,
        current_window_active: bool,
        cx: &mut App,
    ) {
        if !previous_focus_path.is_empty() && current_focus_path.is_empty() {
            self.focus_lost_listeners
                .clone()
                .retain(&(), |listener| listener(self, cx));
        }

        let event = WindowFocusEvent {
            previous_focus_path: if previous_window_active {
                previous_focus_path
            } else {
                Default::default()
            },
            current_focus_path: if current_window_active {
                current_focus_path
            } else {
                Default::default()
            },
        };
        self.focus_listeners
            .clone()
            .retain(&(), |listener| listener(&event, self, cx));
    }

    #[cfg(any(test, feature = "test-support"))]
    pub(crate) fn snapshot_hitboxes_into_rendered_frame(&mut self) {
        let fiber_tree = &self.fiber.tree;
        snapshot_hitboxes_into_map(fiber_tree, &mut self.rendered_frame.hitboxes);
        self.rendered_frame.hitboxes_epoch = Some(HitboxesSnapshotEpoch {
            structure_epoch: fiber_tree.structure_epoch,
            hitbox_epoch: fiber_tree.hitbox_epoch(),
        });
    }

    fn record_entities_accessed(&mut self, cx: &mut App) {
        let mut entities_ref = cx.entities.accessed_entities.borrow_mut();
        let mut entities = mem::take(entities_ref.deref_mut());
        drop(entities_ref);
        let handle = self.handle;
        cx.record_entities_accessed(
            handle,
            // Try moving window invalidator into the Window
            self.invalidator.clone(),
            &entities,
        );
        let mut entities_ref = cx.entities.accessed_entities.borrow_mut();
        mem::swap(&mut entities, entities_ref.deref_mut());
    }

    #[cfg(debug_assertions)]
    fn debug_assert_incremental_collections(&self) {
        debug_assert_active_list_matches_map(
            "tooltips",
            &self.fiber.active_tooltips,
            &self.fiber.tree.tooltips,
        );
        debug_assert_active_list_matches_map(
            "cursor_styles",
            &self.fiber.active_cursor_styles,
            &self.fiber.tree.cursor_styles,
        );
        debug_assert_active_list_matches_map(
            "deferred_draws",
            &self.fiber.active_deferred_draws,
            &self.fiber.tree.deferred_draws,
        );
        debug_assert_active_list_matches_map(
            "input_handlers",
            &self.fiber.active_input_handlers,
            &self.fiber.tree.input_handlers,
        );
        for fiber_id in self.fiber.active_mouse_listeners.members.iter() {
            let effects = self.fiber.tree.effects.get((*fiber_id).into());
            // Get interactivity from render node
            let interactivity = self
                .fiber
                .tree
                .render_nodes
                .get((*fiber_id).into())
                .and_then(|node| node.interactivity());
            debug_assert!(
                has_mouse_effects(interactivity, effects),
                "active mouse list contains fiber without mouse effects: {fiber_id:?}"
            );
        }
        for (key, focus_ids) in self.fiber.tree.tab_stops.iter() {
            let fiber_id = GlobalElementId::from(key);
            for focus_id in focus_ids.iter() {
                debug_assert!(
                    self.fiber.rendered_tab_stops.contains(focus_id),
                    "tab stop {focus_id:?} missing from rendered map for fiber {fiber_id:?}"
                );
            }
        }
    }

    fn invalidate_entities(&mut self) {
        let mut views = self.invalidator.take_views();
        for entity in views.drain() {
            self.mark_view_dirty(entity);
        }
        self.invalidator.replace_views(views);
    }

    #[profiling::function]
    fn present(&self) {
        self.platform_window
            .draw(&self.rendered_frame.scene, &self.segment_pool);
        self.needs_present.set(false);
        profiling::finish_frame!();
    }

    const RENDER_LAYER_DEFERRED_DRAW_PRIORITY_BASE: usize = 1000;
    const INSPECTOR_DEFERRED_DRAW_PRIORITY: usize = 2000;

    fn global_overlay_owner_fiber(&mut self) -> GlobalElementId {
        if let Some(fiber_id) = self.global_overlay_owner_fiber {
            return fiber_id;
        }

        let fiber_id = self.fiber.tree.create_placeholder_fiber();
        self.global_overlay_owner_fiber = Some(fiber_id);
        fiber_id
    }

    fn reset_global_overlay_owner_deferred_draw_state(&mut self, fiber_id: GlobalElementId) {
        self.fiber.tree.deferred_draws.remove(fiber_id.into());
        self.fiber.active_deferred_draws.remove(&fiber_id);

        if let Some(groups) = self
            .fiber
            .tree
            .deferred_draw_overlay_groups
            .get_mut(fiber_id.into())
        {
            for group in groups.values_mut() {
                group.next_index = 0;
            }
        }
    }

    #[track_caller]
    fn enqueue_render_layers_and_inspector(&mut self, root_size: Size<Pixels>, cx: &mut App) {
        let owner_fiber = self.global_overlay_owner_fiber();
        self.reset_global_overlay_owner_deferred_draw_state(owner_fiber);

        let callsite = core::panic::Location::caller();
        self.with_root_view_context(|window| {
            if !window.render_layers.is_empty() {
                let mut layers: Vec<(ElementId, i32, usize, RenderLayerBuilder)> = window
                    .render_layers
                    .iter()
                    .map(|(key, reg)| (key.clone(), reg.order, reg.seq, reg.build.clone()))
                    .collect();
                layers.sort_by_key(|(_, order, seq, _)| (*order, *seq));

                for (index, (key, _order, _seq, build)) in layers.into_iter().enumerate() {
                    let mut hasher = FxHasher::default();
                    key.hash(&mut hasher);
                    let group_key = hasher.finish() as usize;

                    let element = (&*build)(window, cx);
                    window.fibers().defer_draw_for_fiber(
                        owner_fiber,
                        element,
                        Point::default(),
                        Self::RENDER_LAYER_DEFERRED_DRAW_PRIORITY_BASE + index,
                        root_size.into(),
                        group_key,
                        callsite,
                    );
                }
            }

            #[cfg(any(feature = "inspector", debug_assertions))]
            if let Some(inspector) = window.inspector.as_ref().cloned() {
                let element = AnyView::from(inspector).into_any_element();
                let inspector_width: Pixels = rems(30.0).to_pixels(window.rem_size());
                let available_space = size(inspector_width, window.viewport_size.height).into();
                let origin = point(window.viewport_size.width - inspector_width, px(0.0));
                window.fibers().defer_draw_for_fiber(
                    owner_fiber,
                    element,
                    origin,
                    Self::INSPECTOR_DEFERRED_DRAW_PRIORITY,
                    available_space,
                    0,
                    callsite,
                );
            }
        });
    }

    fn with_root_view_context<R>(&mut self, f: impl FnOnce(&mut Window) -> R) -> R {
        // Many elements expect to run under a rendering view context (e.g. image caches
        // consult `Window::current_view()`), so ensure a view ID is present.
        if let Some(root_view_id) = self.root.as_ref().map(|v| v.entity_id()) {
            self.with_rendered_view(root_view_id, f)
        } else {
            f(self)
        }
    }

    fn rebuild_scene_segment_order_if_needed(&mut self) {
        let structure_epoch = self.fiber.tree.structure_epoch;
        if self.next_frame.scene.segment_order_epoch() == structure_epoch {
            return;
        }
        let order = self
            .fiber
            .tree
            .root
            .map(|root| self.fiber.tree.scene_segment_order(root))
            .unwrap_or_default();
        #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
        {
            self.frame_diagnostics.segment_order_rebuilt = true;
            self.frame_diagnostics.scene_segment_order_len = order.len();
        }
        self.next_frame
            .scene
            .set_segment_order(order, structure_epoch);
    }

    fn cleanup_removed_fibers(&mut self) {
        let removed_tab_stops = self.fiber.tree.take_removed_tab_stops();
        for (owner_id, focus_id) in removed_tab_stops {
            self.fiber
                .rendered_tab_stops
                .remove_if_owned_by(&focus_id, owner_id);
        }
        for fiber_id in self.fiber.tree.take_removed_fibers() {
            self.fiber.active_tooltips.remove(&fiber_id);
            self.fiber.active_cursor_styles.remove(&fiber_id);
            self.fiber.active_deferred_draws.remove(&fiber_id);
            self.fiber.active_input_handlers.remove(&fiber_id);
            self.fiber.active_mouse_listeners.remove(&fiber_id);
            if let Some(transform_id) = self.scroll_transforms.remove(&fiber_id) {
                self.segment_pool.transforms.remove(transform_id);
            }
        }
    }

    fn finalize_dirty_flags(&mut self) {
        TaffyLayoutEngine::finalize_dirty_flags(self);
    }

    /// Reconcile the fiber tree against the current view state.
    ///
    /// This is the single entry point for all structure changes and node updates.
    /// After this call, the fiber tree is stable and ready for layout/prepaint/paint.
    ///
    /// Returns `(structure_changed, root_fiber)` where `structure_changed` is true
    /// when a full reconciliation occurred (Branch 3).
    fn reconcile_frame(
        &mut self,
        root_size: Size<Pixels>,
        cx: &mut App,
    ) -> (bool, GlobalElementId) {
        self.invalidator.set_phase(DrawPhase::Reconcile);

        // Check for viewport change
        let viewport_changed = self
            .layout_engine
            .last_layout_viewport_size
            .is_none_or(|size| size != root_size);

        // Branch 1: Root is completely clean with cached output - skip reconciliation
        if !self.refreshing
            && !viewport_changed
            && self.fiber.tree.root.as_ref().is_some_and(|root_id| {
                self.fiber.tree.get(root_id).is_some_and(|_| {
                    self.fiber.tree.dirty_flags(root_id).is_subtree_clean()
                        && self.fiber.tree.has_cached_output(root_id)
                })
            })
        {
            let root_fiber = self.fiber.tree.root.unwrap();
            return (false, root_fiber);
        }

        // Branch 2: Root exists but has dirty views - expand views only
        if !self.refreshing && !viewport_changed && self.fiber.tree.root.is_some() {
            let root_fiber = self.fiber.tree.root.unwrap();

            // Check if the root fiber itself is dirty and needs re-rendering.
            // The root fiber doesn't have view_data, so expand_view_fibers skips it.
            // We need to handle root view re-rendering here.
            let root_needs_rerender = self.fiber.tree.dirty_flags(&root_fiber).needs_work();

            if root_needs_rerender {
                let root_view = self.root.as_ref().unwrap().clone();
                let (mut root_element_tree, root_accessed_entities) =
                    cx.detect_accessed_entities(|cx| {
                        self.with_element_id_stack(&root_fiber, |window| {
                            window.with_rendered_view(root_view.entity_id(), |window| {
                                root_view.render_element(window, cx)
                            })
                        })
                    });

                self.hydrate_view_children(&mut root_element_tree);
                self.record_pending_view_accesses(&root_fiber, root_accessed_entities);

                // Reconcile the new element tree into the existing fiber structure
                self.fiber
                    .tree
                    .reconcile_wrapper(&root_fiber, &root_element_tree, false);

                // Update view_roots mapping for any new nested views
                self.map_view_roots_from_element(&root_fiber, &root_element_tree, &mut Vec::new());

                self.cache_fiber_payloads(&root_fiber, &mut root_element_tree, cx);
            }

            self.expand_view_fibers(root_fiber, cx);

            return (false, root_fiber);
        }

        // Branch 3: Full reconciliation (first render, refreshing, or viewport changed)
        let root_view = self.root.as_ref().unwrap().clone();
        let root_fiber_scope = self
            .fiber
            .tree
            .root
            .filter(|fiber_id| self.fiber.tree.get(fiber_id).is_some())
            .or_else(|| {
                self.fiber
                    .tree
                    .get_view_root(root_view.entity_id())
                    .filter(|fiber_id| self.fiber.tree.get(fiber_id).is_some())
            })
            .unwrap_or_else(|| self.ensure_view_root_fiber(root_view.entity_id()));
        let (mut root_element_tree, root_accessed_entities) = cx.detect_accessed_entities(|cx| {
            self.with_element_id_stack(&root_fiber_scope, |window| {
                window.with_rendered_view(root_view.entity_id(), |window| {
                    root_view.render_element(window, cx)
                })
            })
        });

        self.hydrate_view_children(&mut root_element_tree);

        // Get or create root fiber.
        //
        // Prefer the existing `FiberTree.root` over `view_roots` because the window root view is
        // rendered directly (not as an `AnyView` element), so the `view_roots` map is not always
        // populated for it. Using `FiberTree.root` ensures state (e.g. scroll offsets) survives
        // full refreshes triggered by transient UI events like mouse down/up.
        let root_fiber = self
            .fiber
            .tree
            .root
            .filter(|fiber_id| self.fiber.tree.get(fiber_id).is_some())
            .or_else(|| {
                self.fiber
                    .tree
                    .get_view_root(root_view.entity_id())
                    .filter(|fiber_id| self.fiber.tree.get(fiber_id).is_some())
            })
            .unwrap_or_else(|| self.fiber.tree.create_fiber_for(&root_element_tree));
        let root_fiber = self.fiber.tree.ensure_root(&root_element_tree, root_fiber);

        // Slow path: disable bailout to force full reconciliation with fresh constraints
        self.fiber
            .tree
            .reconcile_wrapper(&root_fiber, &root_element_tree, false);
        self.fiber.tree.view_roots.clear();
        self.map_view_roots_from_element(&root_fiber, &root_element_tree, &mut Vec::new());
        self.fiber
            .tree
            .set_view_root(root_view.entity_id(), root_fiber);

        self.record_pending_view_accesses(&root_fiber, root_accessed_entities);

        // Update last viewport size if it changed
        if viewport_changed {
            // Mark the root fiber as needing layout so the layout pass can track which fibers
            // changed bounds under the new viewport constraints. This enables correct
            // SIZE_CHANGED/POSITION_CHANGED propagation and prevents stale prepaint/paint replay
            // (e.g. cached line layouts) across resizes.
            //
            // Also mark the root as needing paint so that we do a full-tree repaint on the first
            // frame under a new viewport. This avoids replaying cached primitives that may depend
            // on viewport-relative state (content masks, pixel alignment, platform surfaces) when
            // the drawable size changes.
            self.fiber.tree.mark_dirty(
                &root_fiber,
                DirtyFlags::NEEDS_LAYOUT | DirtyFlags::NEEDS_PAINT,
            );
            self.layout_engine.last_layout_viewport_size = Some(root_size);
        }

        self.cache_fiber_payloads(&root_fiber, &mut root_element_tree, cx);
        self.expand_view_fibers(root_fiber, cx);

        (true, root_fiber)
    }

    fn draw_roots(&mut self, cx: &mut App) {
        let frame_start = std::time::Instant::now();
        self.tooltip_bounds.take();
        self.pending_view_accesses.clear();
        self.fiber.frame_number = self.fiber.frame_number.wrapping_add(1);
        self.fiber.tree.begin_frame(self.fiber.frame_number);
        #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
        {
            self.frame_diagnostics = FrameDiagnostics {
                frame_number: self.fiber.frame_number,
                structure_epoch: self.fiber.tree.structure_epoch,
                ..Default::default()
            };
        }

        let _inspector_width: Pixels = rems(30.0).to_pixels(self.rem_size());
        let root_size = {
            #[cfg(any(feature = "inspector", debug_assertions))]
            {
                if self.inspector.is_some() {
                    let mut size = self.viewport_size;
                    size.width = (size.width - _inspector_width).max(px(0.0));
                    size
                } else {
                    self.viewport_size
                }
            }
            #[cfg(not(any(feature = "inspector", debug_assertions)))]
            {
                self.viewport_size
            }
        };

        // Phase 1: Reconcile (single entry point for all structure changes and node updates)
        // reconcile_frame sets DrawPhase::Reconcile internally
        let reconcile_start = std::time::Instant::now();
        let (structure_changed, root_fiber) = self.reconcile_frame(root_size, cx);
        self.full_reconciliation_this_frame = structure_changed;
        #[cfg(any(test, feature = "test-support"))]
        if structure_changed {
            self.next_frame.debug_bounds.clear();
        }
        let reconcile_time = reconcile_start.elapsed();

        // Phase 2: Intrinsic sizing
        self.invalidator.set_phase(DrawPhase::Layout);
        let sizing_start = std::time::Instant::now();
        let any_size_changed = self.compute_intrinsic_sizes(root_fiber, cx);
        let sizing_time = sizing_start.elapsed();

        let dirty_islands = self.fiber.tree.collect_dirty_layout_islands();

        // Layout is needed if:
        // - Any island has explicit layout dirtiness (position/style/structure), or
        // - Intrinsic sizing determined any intrinsic sizes actually changed.
        let needs_layout = any_size_changed || !dirty_islands.is_empty();

        // Phase 3: Layout (if needed)
        let layout_start = std::time::Instant::now();
        let layout_fibers = if needs_layout {
            self.invalidator.set_phase(DrawPhase::Layout);
            self.compute_layout_islands(root_fiber, root_size.into(), &dirty_islands, cx)
        } else {
            0
        };

        if needs_layout {
            self.finalize_dirty_flags();
        }
        let layout_time = layout_start.elapsed();

        // Phase 4: Prepaint
        let prepaint_start = std::time::Instant::now();
        self.invalidator.set_phase(DrawPhase::Prepaint);
        self.with_absolute_element_offset(Point::default(), |window| {
            context::PrepaintCx::new(window).prepaint_fiber_tree(root_fiber, cx)
        });
        self.enqueue_render_layers_and_inspector(root_size, cx);

        let mut deferred_draws = {
            let mut prepaint_cx = context::PrepaintCx::new(self);
            prepaint_cx.collect_deferred_draw_keys()
        };
        deferred_draws.sort_by_key(|draw| (draw.priority, draw.sequence));
        {
            let mut prepaint_cx = context::PrepaintCx::new(self);
            prepaint_cx.prepaint_deferred_draws(&deferred_draws, cx);
        }

        // Clear active overlay from previous frame.
        self.active_overlay = None;

        // Prepaint overlays in priority order: prompt > drag > tooltip.
        // Only one can be active at a time. All use fiber-backed rendering.
        let has_overlay = self.prepaint_prompt(root_size, cx)
            || self.prepaint_active_drag(cx)
            || self.prepaint_tooltip(cx);

        self.mouse_hit_test = self.next_frame.hit_test(self, self.mouse_position);
        let prepaint_time = prepaint_start.elapsed();

        // Dispatch queued on_layout events after all prepaint passes.
        self.dispatch_layout_events(cx);

        // Phase 5: Paint
        let paint_start = std::time::Instant::now();
        self.fiber.tree.ensure_preorder_indices();
        self.invalidator.set_phase(DrawPhase::Paint);
        context::PaintCx::new(self).paint_fiber_tree(root_fiber, cx);

        {
            let mut paint_cx = context::PaintCx::new(self);
            paint_cx.paint_deferred_draws(&deferred_draws, cx);
        }

        if has_overlay {
            // Paint fiber-backed overlay (prompt, drag, or tooltip).
            self.paint_overlay(cx);
        }

        #[cfg(any(feature = "inspector", debug_assertions))]
        self.paint_inspector_hitbox(cx);
        let paint_time = paint_start.elapsed();

	        // Phase 6: Cleanup
	        // Clear work flags and properly recompute HAS_DIRTY_DESCENDANT.
	        // This ensures fibers are in a clean state for the next frame's caching decisions.
	        let cleanup_start = std::time::Instant::now();
	        self.fiber.tree.log_dirty_touched_summary_if_enabled();
	        self.fiber.tree.end_of_frame_cleanup();
	        let cleanup_time = cleanup_start.elapsed();
	        let total_time = frame_start.elapsed();

        #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
        {
            self.frame_diagnostics.layout_fibers = layout_fibers;
            self.frame_diagnostics.reconcile_time = reconcile_time;
            self.frame_diagnostics.intrinsic_sizing_time = sizing_time;
            self.frame_diagnostics.layout_time = layout_time;
            self.frame_diagnostics.prepaint_time = prepaint_time;
            self.frame_diagnostics.paint_time = paint_time;
            self.frame_diagnostics.cleanup_time = cleanup_time;
            self.frame_diagnostics.total_time = total_time;
        }
        #[cfg(not(any(debug_assertions, feature = "diagnostics", feature = "test-support")))]
        let _ = layout_fibers;
    }

    /// Phase 2: Compute intrinsic sizes for dirty elements.
    ///
    /// Returns true if any fiber's computed intrinsic size changed.
    fn compute_intrinsic_sizes(&mut self, root_fiber: GlobalElementId, cx: &mut App) -> bool {
        let _ = root_fiber;
        self.compute_intrinsic_sizes_for_islands(
            cx,
            |_, _| true,
            true,
            None,
            false,
        )
    }

    fn compute_intrinsic_sizes_for_islands<F>(
        &mut self,
        cx: &mut App,
        mut include_island: F,
        track_processed_islands: bool,
        detached_root: Option<GlobalElementId>,
        force_compute: bool,
    ) -> bool
    where
        F: FnMut(&crate::FiberTree, GlobalElementId) -> bool,
    {
        let rem_size = self.rem_size();
        let scale_factor = self.scale_factor();
        let mut any_changed = false;
        let options = IntrinsicSizingOptions {
            track_processed_islands,
            convergence_root: detached_root,
            force_compute,
        };

	        #[derive(Clone, Copy)]
	        struct StackState {
	            text_style_len: usize,
	            image_cache_len: usize,
	            rendered_entity_len: usize,
	        }

	        impl StackState {
	            fn capture(window: &Window) -> Self {
	                Self {
	                    text_style_len: window.text_style_stack.len(),
	                    image_cache_len: window.image_cache_stack.len(),
	                    rendered_entity_len: window.rendered_entity_stack.len(),
	                }
	            }

	            fn restore(self, window: &mut Window) {
	                window.text_style_stack.truncate(self.text_style_len);
	                window.image_cache_stack.truncate(self.image_cache_len);
	                window
	                    .rendered_entity_stack
	                    .truncate(self.rendered_entity_len);
	            }
	        }

	        struct Frame {
	            fiber_id: GlobalElementId,
	            dirty: DirtyFlags,
	            stack_state: StackState,
	            node_frame: Option<crate::LayoutFrame>,
	        }

        if options.track_processed_islands {
            self.fiber.tree.sizing_pass_processed_islands.clear();
        }

	        let mut processed_islands: collections::FxHashSet<GlobalElementId> =
	            collections::FxHashSet::default();
	        let mut iterations = 0usize;

	        loop {
            iterations += 1;
            if iterations >= 32 {
                if let Some(root) = options.convergence_root {
                    log::error!(
                        "detached intrinsic sizing did not converge for root {:?}",
                        root
                    );
                } else {
	                    log::error!("conditional slot reconciliation did not converge");
	                }
	                break;
            }

            self.fiber.tree.rebuild_layout_islands_if_needed();
            let island_roots: Vec<GlobalElementId> =
                self.fiber.tree.layout_island_roots().to_vec();

            let dirty_layout_islands = self.fiber.tree.collect_dirty_layout_islands();
            let dirty_sizing_islands = self.fiber.tree.collect_dirty_sizing_islands();

            let mut islands_to_process: collections::FxHashSet<GlobalElementId> = if options
                .force_compute
            {
                island_roots
                    .iter()
                    .copied()
                    .filter(|island_root| include_island(&self.fiber.tree, *island_root))
                    .collect()
            } else {
                let mut islands_to_process = dirty_layout_islands.clone();
                islands_to_process.extend(dirty_sizing_islands);
                islands_to_process.retain(|island_root| include_island(&self.fiber.tree, *island_root));
                islands_to_process
            };
            islands_to_process.retain(|island_root| !processed_islands.contains(island_root));
            if islands_to_process.is_empty() {
                break;
            }

            for island_root in island_roots.into_iter().rev() {
                if !islands_to_process.contains(&island_root) {
                    continue;
                }
	                if self.fiber.tree.get(&island_root).is_none() {
	                    continue;
	                }

                processed_islands.insert(island_root);
                if options.track_processed_islands {
                    self.fiber
                        .tree
                        .sizing_pass_processed_islands
                        .insert(island_root);
                }

	                let mut stack: Vec<(GlobalElementId, bool)> = vec![(island_root, true)];
	                let mut frame_stack: Vec<Frame> = Vec::new();

	                while let Some((fiber_id, entering)) = stack.pop() {
	                    if entering {
	                        let view_id = {
	                            let Some(fiber) = self.fiber.tree.get(&fiber_id) else {
	                                continue;
	                            };
	                            self.fiber_view_id(&fiber_id, fiber)
	                        };

	                        let dirty = self.fiber.tree.dirty_flags(&fiber_id);
	                        let stack_state = StackState::capture(self);
	                        let structure_epoch_before = self.fiber.tree.structure_epoch;
	                        let mut node_frame: Option<crate::LayoutFrame> = None;

	                        if let Some(view_id) = view_id {
	                            self.rendered_entity_stack.push(view_id);
	                        }

	                        let _ = self.with_render_node_mut(fiber_id, |node, window| {
	                            let mut layout_ctx = crate::LayoutCtx {
	                                fiber_id,
	                                rem_size,
	                                scale_factor,
	                                window,
	                                cx,
	                            };
	                            let frame = node.layout_begin(&mut layout_ctx);

	                            let slots = node.conditional_slots(fiber_id);
	                            let had_node_children = window
	                                .fiber
	                                .tree
	                                .node_children
	                                .get(fiber_id.into())
	                                .is_some_and(|children| !children.is_empty());
	                            if !slots.is_empty() || had_node_children {
	                                let structure_epoch_before = window.fiber.tree.structure_epoch;
	                                window.reconcile_conditional_slots(fiber_id, slots, cx);
	                                if window.fiber.tree.structure_epoch != structure_epoch_before {
	                                    window.fiber.tree.rebuild_layout_islands_if_needed();
	                                }
	                            }

	                            node_frame = Some(frame);
	                        });

	                        if self.fiber.tree.structure_epoch != structure_epoch_before {
	                            self.fiber.tree.rebuild_layout_islands_if_needed();
	                        }

                        frame_stack.push(Frame {
                            fiber_id,
                            dirty,
	                            stack_state,
	                            node_frame,
	                        });
	                        stack.push((fiber_id, false));

	                        let children: SmallVec<[GlobalElementId; 8]> =
	                            self.fiber.tree.children(&fiber_id).collect();
	                        for child_id in children.into_iter().rev() {
	                            if self.fiber.tree.outer_island_root_for(child_id) == island_root {
	                                stack.push((child_id, true));
	                            }
	                        }
	                    } else {
	                        let Some(frame) = frame_stack.pop() else {
	                            continue;
                        };
                        debug_assert_eq!(frame.fiber_id, fiber_id);

                        if (options.force_compute || frame.dirty.needs_sizing())
                            && self.compute_fiber_intrinsic_size(
                                fiber_id,
                                rem_size,
                                scale_factor,
                                cx,
	                            )
	                        {
	                            any_changed = true;
                            self.fiber.tree.mark_intrinsic_size_changed(&fiber_id);
                        }

                        if frame.dirty.has_sizing_work() {
                            self.fiber.tree.clear_sizing_flags(&fiber_id);
                        }

                        if let Some(node_frame) = frame.node_frame {
                            let _ = self.with_render_node_mut(fiber_id, |node, window| {
                                let mut layout_ctx = crate::LayoutCtx {
                                    fiber_id,
                                    rem_size,
                                    scale_factor,
                                    window,
                                    cx,
                                };
                                node.layout_end(&mut layout_ctx, node_frame);
                            });
                        }

                        frame.stack_state.restore(self);
                    }
                }
            }
	        }

	        any_changed
	    }

	    fn reconcile_conditional_slots(
	        &mut self,
	        parent_id: GlobalElementId,
	        slots: SmallVec<[crate::ConditionalSlot; 4]>,
        cx: &mut App,
    ) {
        let parent_key: DefaultKey = parent_id.into();
        let old_node_children = self
            .fiber
            .tree
            .node_children
            .get(parent_key)
            .cloned()
            .unwrap_or_default();

        let existing_children: SmallVec<[GlobalElementId; 8]> =
            SmallVec::from_slice(self.fiber.tree.children_slice(&parent_id));

        let mut descriptor_children: SmallVec<[GlobalElementId; 8]> = SmallVec::new();
        for child_id in &existing_children {
            if !old_node_children.contains(child_id) {
                descriptor_children.push(*child_id);
            }
        }

        let mut new_node_children: SmallVec<[GlobalElementId; 4]> = SmallVec::new();
        for slot in slots {
            if !slot.active {
                continue;
            }

            let existing_child_id = old_node_children.iter().copied().find(|candidate_id| {
                self.fiber
                    .tree
                    .get(candidate_id)
                    .is_some_and(|fiber| fiber.slot_index == Some(slot.slot_index))
                    && !new_node_children.contains(candidate_id)
            });

            let Some(element_factory) = slot.element_factory else {
                if let Some(child_id) = existing_child_id {
                    self.fiber.tree.set_parent(&child_id, &parent_id);
                    new_node_children.push(child_id);
                }
                continue;
            };

            let child_id = existing_child_id.unwrap_or_else(|| {
                let child_id = self.fiber.tree.create_placeholder_fiber();
                if let Some(fiber) = self.fiber.tree.get_mut(&child_id) {
                    fiber.slot_index = Some(slot.slot_index);
                }
                child_id
            });

            self.fiber.tree.set_parent(&child_id, &parent_id);

            let mut element = element_factory();
            element.expand_wrappers(self, cx);
            self.fiber.tree.reconcile_wrapper(&child_id, &element, true);
            self.fibers()
                .cache_fiber_payloads_overlay(&child_id, &mut element, cx);

            new_node_children.push(child_id);
        }

        for child_id in old_node_children.iter() {
            if new_node_children.contains(child_id) {
                continue;
            }
            if self.fiber.tree.get(child_id).is_some() {
                self.fiber.tree.remove(child_id);
            }
        }

        if let Some(stored) = self.fiber.tree.node_children.get_mut(parent_key) {
            *stored = new_node_children.clone();
        } else {
            self.fiber
                .tree
                .node_children
                .insert(parent_key, new_node_children.clone());
        }

        let mut merged_children = descriptor_children;
        merged_children.extend(new_node_children);

        if existing_children != merged_children {
            self.fiber
                .tree
                .relink_children_in_order(&parent_id, &merged_children);
            self.fiber.tree.clear_taffy_cache_upwards(&parent_id);
            self.fiber.tree.mark_dirty(
                &parent_id,
                DirtyFlags::STRUCTURE_CHANGED | DirtyFlags::NEEDS_LAYOUT | DirtyFlags::NEEDS_PAINT,
            );
        }
    }

    fn compute_layout_islands(
        &mut self,
        root_fiber: GlobalElementId,
        root_space: Size<AvailableSpace>,
        dirty_islands: &collections::FxHashSet<GlobalElementId>,
        cx: &mut App,
    ) -> usize {
        self.fiber.tree.rebuild_layout_islands_if_needed();

        // Update taffy styles and run layout for dirty islands only.
        self.layout_engine.fibers_layout_changed.clear();
        let island_roots: Vec<GlobalElementId> = self.fiber.tree.layout_island_roots().to_vec();

        let mut layout_calls = 0;
        let scale_factor = self.scale_factor();

        for island_root in &island_roots {
            if !dirty_islands.contains(island_root) {
                continue;
            }

            let available_space = if *island_root == root_fiber {
                root_space
            } else if self.fiber.tree.parent(island_root).is_some() {
                // For internal islands (layout boundaries), use the island root's *current* bounds
                // from the already-computed outer layout. Using `tree.bounds` here is incorrect
                // because it is only updated after the full island pass, which can leave inner
                // islands laid out with stale (or missing) constraints.
                let layout = self.fiber.tree.final_layout_for_bounds(*island_root);
                Size {
                    width: AvailableSpace::Definite(Pixels(layout.size.width / scale_factor)),
                    height: AvailableSpace::Definite(Pixels(layout.size.height / scale_factor)),
                }
            } else if let Some(bounds) = self.fiber.tree.bounds.get((*island_root).into()).copied()
            {
                Size {
                    width: AvailableSpace::Definite(bounds.size.width),
                    height: AvailableSpace::Definite(bounds.size.height),
                }
            } else {
                // Fallback for first layout of detached roots. Use min-size constraints.
                AvailableSpace::min_size()
            };

            TaffyLayoutEngine::setup_taffy_from_fibers(self, *island_root, cx);
            layout_calls += self.compute_layout_for_fiber(*island_root, available_space, cx);
        }

        layout_calls
    }

    /// Compute intrinsic sizes and reconcile conditional slots for a detached root.
    ///
    /// This is used for test/overlay subtrees that perform their own layout pass (outside the main
    /// window frame pipeline). It ensures retained nodes can materialize conditional-slot children
    /// and all fibers in the detached subtree have cached intrinsic sizes before taffy layout runs.
    pub(crate) fn compute_intrinsic_sizes_for_detached_root(
        &mut self,
        root: GlobalElementId,
        cx: &mut App,
    ) {
	        fn is_descendant_of(
	            tree: &crate::FiberTree,
	            mut fiber_id: GlobalElementId,
	            root: GlobalElementId,
	        ) -> bool {
	            loop {
	                if fiber_id == root {
	                    return true;
	                }
	                let Some(parent_id) = tree.parent(&fiber_id) else {
	                    return false;
	                };
	                fiber_id = parent_id;
	            }
	        }
        let _ = self.compute_intrinsic_sizes_for_islands(
            cx,
            |tree, island_root| is_descendant_of(tree, island_root, root),
            false,
            Some(root),
            false,
        );
    }

    fn compute_fiber_intrinsic_size(
        &mut self,
        fiber_id: GlobalElementId,
        rem_size: Pixels,
        scale_factor: f32,
        cx: &mut App,
    ) -> bool {
        let slot_key: DefaultKey = fiber_id.into();

        let mut render_node = self.fiber.tree.render_nodes.remove(slot_key);
        if render_node
            .as_ref()
            .is_some_and(|node| !node.uses_intrinsic_sizing_cache())
        {
            log::trace!(
                "clear_intrinsic_size {:?}: render_node doesn't use intrinsic sizing cache",
                fiber_id
            );
            if let Some(layout_state) = self.fiber.tree.layout_state.get_mut(slot_key) {
                layout_state.intrinsic_size = None;
            }
            if let Some(node) = render_node {
                self.fiber.tree.render_nodes.insert(slot_key, node);
            }
            return false;
        }

        let result = if let Some(ref mut node) = render_node {
            let mut ctx = crate::SizingCtx {
                fiber_id,
                window: self,
                cx,
                rem_size,
                scale_factor,
            };
            Some(node.compute_intrinsic_size(&mut ctx))
        } else {
            None
        };

        if let Some(node) = render_node {
            self.fiber.tree.render_nodes.insert(slot_key, node);
        }

        let Some(result) = result else {
            return false;
        };

        let cached = self.fiber.tree.get_intrinsic_size(&fiber_id);
        let changed = cached.map(|c| c.size != result.size).unwrap_or(true);

        self.fiber.tree.set_intrinsic_size(&fiber_id, result.size);

        changed
    }

    /// Paint the active fiber-backed overlay (tooltip, prompt, or drag).
    fn paint_overlay(&mut self, cx: &mut App) {
        let Some(overlay) = self.active_overlay else {
            return;
        };

        self.with_rendered_view(overlay.view_id, |window| {
            let mut paint_cx = context::PaintCx::new(window);
            paint_cx.with_absolute_element_offset(overlay.offset, |window| {
                window
                    .fibers()
                    .paint_fiber_tree_internal(overlay.fiber_id, cx, true)
            });
        });
    }

    /// Dispatch queued on_layout events to their listeners.
    ///
    /// Called after all prepaint passes, before paint. Events are queued during prepaint
    /// when a fiber's bounds change and it has a layout listener.
    fn dispatch_layout_events(&mut self, cx: &mut App) {
        let events = std::mem::take(&mut self.fiber.pending_layout_events);
        for (fiber_id, bounds) in events {
            let fiber_key = fiber_id.into();
            if let Some(mut listener) = self.fiber.tree.layout_listeners.remove(fiber_key) {
                // If the listener panics, we still want to restore it into the map so that
                // debugging does not lose the registration.
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    listener(bounds, self, cx);
                }));
                self.fiber.tree.layout_listeners.insert(fiber_key, listener);
                if let Err(payload) = result {
                    std::panic::resume_unwind(payload);
                }
            }
        }
    }

    fn prepaint_tooltip(&mut self, cx: &mut App) -> bool {
        let tooltip_requests = self.fibers().collect_tooltip_requests();
        for tooltip_request in tooltip_requests.into_iter().rev() {
            let mut element = tooltip_request.tooltip.view.clone().into_any();
            let mouse_position = tooltip_request.tooltip.mouse_position;
            let current_view = self.current_view();

            // Get or create the tooltip fiber root.
            let fiber_id = if let Some(existing) = self.fiber.tooltip_overlay_root {
                existing
            } else {
                let new_root = self.fiber.tree.create_placeholder_fiber();
                self.fiber.tooltip_overlay_root = Some(new_root);
                new_root
            };

            // Expand wrapper elements BEFORE reconciliation.
            element.expand_wrappers(self, cx);

            // Reconcile the tooltip element into the fiber.
            self.fiber.tree.reconcile(&fiber_id, &element, true);

            // Install retained nodes.
            self.fibers()
                .cache_fiber_payloads_overlay(&fiber_id, &mut element, cx);

            // Layout the tooltip using min-size constraints.
            self.prepare_detached_root_for_layout(fiber_id, cx);
            self.compute_layout_for_fiber(fiber_id, AvailableSpace::min_size(), cx);

            // Get the computed size from the fiber bounds.
            let tooltip_size = self
                .fiber
                .tree
                .bounds
                .get(fiber_id.into())
                .map(|b| b.size)
                .unwrap_or_default();

            // Position the tooltip.
            let mut tooltip_bounds =
                Bounds::new(mouse_position + point(px(1.), px(1.)), tooltip_size);
            let window_bounds = Bounds {
                origin: Point::default(),
                size: self.viewport_size(),
            };

            if tooltip_bounds.right() > window_bounds.right() {
                let new_x = mouse_position.x - tooltip_bounds.size.width - px(1.);
                if new_x >= Pixels::ZERO {
                    tooltip_bounds.origin.x = new_x;
                } else {
                    tooltip_bounds.origin.x = cmp::max(
                        Pixels::ZERO,
                        tooltip_bounds.origin.x - tooltip_bounds.right() - window_bounds.right(),
                    );
                }
            }

            if tooltip_bounds.bottom() > window_bounds.bottom() {
                let new_y = mouse_position.y - tooltip_bounds.size.height - px(1.);
                if new_y >= Pixels::ZERO {
                    tooltip_bounds.origin.y = new_y;
                } else {
                    tooltip_bounds.origin.y = cmp::max(
                        Pixels::ZERO,
                        tooltip_bounds.origin.y - tooltip_bounds.bottom() - window_bounds.bottom(),
                    );
                }
            }

            // Check visibility.
            let is_visible =
                (tooltip_request.tooltip.check_visible_and_update)(tooltip_bounds, self, cx);
            if !is_visible {
                continue;
            }

            // Prepaint the fiber tree at the computed offset.
            self.with_rendered_view(current_view, |window| {
                let mut prepaint_cx = context::PrepaintCx::new(window);
                prepaint_cx.with_absolute_element_offset(tooltip_bounds.origin, |window| {
                    window
                        .fibers()
                        .prepaint_fiber_tree_internal(fiber_id, cx, true)
                });
            });

            // Store state for painting.
            self.tooltip_bounds = Some(TooltipBounds {
                id: tooltip_request.id,
                bounds: tooltip_bounds,
            });
            self.active_overlay = Some(ActiveOverlay {
                fiber_id,
                offset: tooltip_bounds.origin,
                view_id: current_view,
            });
            return true;
        }
        false
    }

    /// Prepaint the prompt overlay using the fiber-backed pipeline.
    /// Returns true if a prompt was prepainted.
    fn prepaint_prompt(&mut self, root_size: Size<Pixels>, cx: &mut App) -> bool {
        let Some(prompt) = self.prompt.take() else {
            return false;
        };

        let mut element = prompt.view.any_view().into_any();
        let current_view = self.current_view();

        // Get or create the prompt fiber root.
        let fiber_id = if let Some(existing) = self.fiber.prompt_overlay_root {
            existing
        } else {
            let new_root = self.fiber.tree.create_placeholder_fiber();
            self.fiber.prompt_overlay_root = Some(new_root);
            new_root
        };

        // Expand wrapper elements BEFORE reconciliation.
        element.expand_wrappers(self, cx);

        // Reconcile the prompt element into the fiber.
        self.fiber.tree.reconcile(&fiber_id, &element, true);

        // Install retained nodes.
        self.fibers()
            .cache_fiber_payloads_overlay(&fiber_id, &mut element, cx);

        // Layout the prompt using root size constraints.
        self.prepare_detached_root_for_layout(fiber_id, cx);
        self.compute_layout_for_fiber(fiber_id, root_size.into(), cx);

        // Prepaint the fiber tree at the origin.
        self.with_rendered_view(current_view, |window| {
            let mut prepaint_cx = context::PrepaintCx::new(window);
            prepaint_cx.with_absolute_element_offset(Point::default(), |window| {
                window
                    .fibers()
                    .prepaint_fiber_tree_internal(fiber_id, cx, true)
            });
        });

        // Store state for painting.
        self.active_overlay = Some(ActiveOverlay {
            fiber_id,
            offset: Point::default(),
            view_id: current_view,
        });

        // Restore the prompt.
        self.prompt = Some(prompt);
        true
    }

    /// Prepaint the active drag overlay using the fiber-backed pipeline.
    /// Returns true if a drag was prepainted.
    fn prepaint_active_drag(&mut self, cx: &mut App) -> bool {
        let Some(active_drag) = cx.active_drag.take() else {
            return false;
        };

        let mut element = active_drag.view.clone().into_any();
        let offset = self.mouse_position() - active_drag.cursor_offset;
        let current_view = self.current_view();

        // Get or create the drag fiber root.
        let fiber_id = if let Some(existing) = self.fiber.drag_overlay_root {
            existing
        } else {
            let new_root = self.fiber.tree.create_placeholder_fiber();
            self.fiber.drag_overlay_root = Some(new_root);
            new_root
        };

        // Expand wrapper elements BEFORE reconciliation.
        element.expand_wrappers(self, cx);

        // Reconcile the drag element into the fiber.
        self.fiber.tree.reconcile(&fiber_id, &element, true);

        // Install retained nodes.
        self.fibers()
            .cache_fiber_payloads_overlay(&fiber_id, &mut element, cx);

        // Layout the drag using min-size constraints.
        self.prepare_detached_root_for_layout(fiber_id, cx);
        self.compute_layout_for_fiber(fiber_id, AvailableSpace::min_size(), cx);

        // Prepaint the fiber tree at the computed offset.
        self.with_rendered_view(current_view, |window| {
            let mut prepaint_cx = context::PrepaintCx::new(window);
            prepaint_cx.with_absolute_element_offset(offset, |window| {
                window
                    .fibers()
                    .prepaint_fiber_tree_internal(fiber_id, cx, true)
            });
        });

        // Store state for painting.
        self.active_overlay = Some(ActiveOverlay {
            fiber_id,
            offset,
            view_id: current_view,
        });

        // Restore the active drag.
        cx.active_drag = Some(active_drag);
        true
    }

    /// Push a text style onto the stack, and call a function with that style active.
    /// Use [`Window::text_style`] to get the current, combined text style. This method
    /// should only be called as part of element drawing.
    // This function is called in a highly recursive manner in editor
    // prepainting, make sure its inlined to reduce the stack burden
    #[inline]
    pub fn with_text_style<F, R>(&mut self, style: Option<TextStyleRefinement>, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.invalidator.debug_assert_prepaint_or_paint();
        if let Some(style) = style {
            self.text_style_stack.push(style);
            let result = f(self);
            self.text_style_stack.pop();
            result
        } else {
            f(self)
        }
    }

    /// Updates the cursor style at the platform level. This method should only be called
    /// during the paint phase of element drawing.
    pub fn set_cursor_style(&mut self, style: CursorStyle, hitbox: &Hitbox) {
        self.invalidator.debug_assert_paint();
        self.fibers().set_cursor_style(style, hitbox);
    }

    /// Updates the cursor style for the entire window at the platform level. A cursor
    /// style using this method will have precedence over any cursor style set using
    /// `set_cursor_style`. This method should only be called during the paint
    /// phase of element drawing.
    pub fn set_window_cursor_style(&mut self, style: CursorStyle) {
        self.invalidator.debug_assert_paint();
        self.with_fiber_cx(|fiber| fiber.set_window_cursor_style(style));
    }

    /// Sets a tooltip to be rendered for the upcoming frame. This method should only be called
    /// during the paint phase of element drawing.
    pub fn set_tooltip(&mut self, tooltip: AnyTooltip) -> TooltipId {
        self.invalidator.debug_assert_layout_or_prepaint();
        self.with_fiber_cx(|fiber| fiber.set_tooltip(tooltip))
    }

    /// Invoke the given function with the given content mask after intersecting it
    /// with the current mask. This method should only be called during element drawing.
    // This function is called in a highly recursive manner in editor
    // prepainting, make sure its inlined to reduce the stack burden
    #[inline]
    pub fn with_content_mask<R>(
        &mut self,
        mask: Option<ContentMask<Pixels>>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.invalidator.debug_assert_prepaint_or_paint();
        if let Some(mask) = mask {
            // Transform mask to world coordinates if inside a transform context
            let world_mask = self.transform_mask_to_world(mask);
            let intersected = world_mask.intersect(&self.content_mask());
            self.content_mask_stack.push(intersected);
            let result = f(self);
            self.content_mask_stack.pop();
            result
        } else {
            f(self)
        }
    }

    /// Updates the global element offset relative to the current offset. This is used to implement
    /// scrolling. This method should only be called during element drawing.
    pub fn with_element_offset<R>(
        &mut self,
        offset: Point<Pixels>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.invalidator.debug_assert_prepaint_or_paint();

        if offset.is_zero() {
            return f(self);
        };

        let abs_offset = self.element_offset() + offset;
        self.with_absolute_element_offset(abs_offset, f)
    }

    /// Updates the global element offset based on the given offset. This is used to implement
    /// drag handles and other manual painting of elements. This method should only be called during
    /// element drawing.
    pub fn with_absolute_element_offset<R>(
        &mut self,
        offset: Point<Pixels>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.invalidator.debug_assert_prepaint_or_paint();
        let current = self.element_offset();
        let delta = offset - current;
        self.transform_stack.push_offset(delta);
        let result = f(self);
        self.transform_stack.pop_offset(delta);
        result
    }

    pub(crate) fn push_unculled_scene(&mut self) {
        self.scene_culling_disabled_depth = self.scene_culling_disabled_depth.saturating_add(1);
    }

    pub(crate) fn pop_unculled_scene(&mut self) {
        self.scene_culling_disabled_depth = self.scene_culling_disabled_depth.saturating_sub(1);
    }

    pub(crate) fn should_cull_scene_primitives(&self) -> bool {
        self.scene_culling_disabled_depth == 0
    }

    /// Executes the given closure with an additional element opacity multiplier.
    ///
    /// This is used to implement inherited opacity for custom elements that paint directly
    /// via window APIs.
    ///
    /// This method should only be called during the prepaint or paint phase of element drawing.
    pub fn with_element_opacity<R>(
        &mut self,
        opacity: Option<f32>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.invalidator.debug_assert_prepaint_or_paint();

        let Some(opacity) = opacity else {
            return f(self);
        };

        let previous_opacity = self.element_opacity;
        self.element_opacity = previous_opacity * opacity;
        let result = f(self);
        self.element_opacity = previous_opacity;
        result
    }

    /// Perform prepaint on child elements in a "retryable" manner, so that any side effects
    /// of prepaints can be discarded before prepainting again. This is used to support autoscroll
    /// where we need to prepaint children to detect the autoscroll bounds, then adjust the
    /// element offset and prepaint again. See [`crate::List`] for an example. This method should only be
    /// called during the prepaint phase of element drawing.
    pub fn transact<T, U>(&mut self, f: impl FnOnce(&mut Self) -> Result<T, U>) -> Result<T, U> {
        self.invalidator.debug_assert_layout_or_prepaint();
        self.fibers().transact(f)
    }

    /// When you call this method during [`Element::prepaint`], containing elements will attempt to
    /// scroll to cause the specified bounds to become visible. When they decide to autoscroll, they will call
    /// [`Element::prepaint`] again with a new set of bounds. See [`crate::List`] for an example of an element
    /// that supports this method being called on the elements it contains. This method should only be
    /// called during the prepaint phase of element drawing.
    pub fn request_autoscroll(&mut self, bounds: Bounds<Pixels>) {
        self.invalidator.debug_assert_layout_or_prepaint();
        self.requested_autoscroll = Some(bounds);
    }

    /// This method can be called from a containing element such as [`crate::List`] to support the autoscroll behavior
    /// described in [`Self::request_autoscroll`].
    pub fn take_autoscroll(&mut self) -> Option<Bounds<Pixels>> {
        self.invalidator.debug_assert_layout_or_prepaint();
        self.requested_autoscroll.take()
    }

    /// Asynchronously load an asset, if the asset hasn't finished loading this will return None.
    /// Your view will be re-drawn once the asset has finished loading.
    ///
    /// Note that the multiple calls to this method will only result in one `Asset::load` call at a
    /// time.
    pub fn use_asset<A: Asset>(&mut self, source: &A::Source, cx: &mut App) -> Option<A::Output> {
        let (task, is_first) = cx.fetch_asset::<A>(source);
        task.clone().now_or_never().or_else(|| {
            if is_first {
                let entity_id = self.current_view();
                self.spawn(cx, {
                    let task = task.clone();
                    async move |cx| {
                        task.await;
                        cx.on_next_frame(move |_, cx| {
                            cx.notify(entity_id);
                        });
                    }
                })
                .detach();
            }

            None
        })
    }

    /// Asynchronously load an asset, if the asset hasn't finished loading or doesn't exist this will return None.
    /// Your view will not be re-drawn once the asset has finished loading.
    ///
    /// Note that the multiple calls to this method will only result in one `Asset::load` call at a
    /// time.
    pub fn get_asset<A: Asset>(&mut self, source: &A::Source, cx: &mut App) -> Option<A::Output> {
        let (task, _) = cx.fetch_asset::<A>(source);
        task.now_or_never()
    }
    /// Obtain the current element offset. This method should only be called during element drawing.
    pub fn element_offset(&self) -> Point<Pixels> {
        self.invalidator.debug_assert_prepaint_or_paint();
        self.transform_stack.local_offset()
    }

    /// Obtain the current element opacity. This method should only be called during the
    /// prepaint phase of element drawing.
    #[inline]
    pub(crate) fn element_opacity(&self) -> f32 {
        self.invalidator.debug_assert_prepaint_or_paint();
        self.element_opacity
    }

    /// Obtain the current content mask. This method should only be called during element drawing.
    pub fn content_mask(&self) -> ContentMask<Pixels> {
        self.invalidator.debug_assert_prepaint_or_paint();
        self.content_mask_stack
            .last()
            .cloned()
            .unwrap_or_else(|| ContentMask {
                bounds: Bounds {
                    origin: Point::default(),
                    size: self.viewport_size,
                },
            })
    }

    /// Transform a content mask from local coordinates to world coordinates.
    /// If we're inside a scroll transform context, the mask bounds need to be
    /// transformed so they match the coordinate space used by the shader for
    /// clipping comparisons.
    #[inline]
    pub(crate) fn transform_mask_to_world(&self, mask: ContentMask<Pixels>) -> ContentMask<Pixels> {
        let transform_id = self.transform_stack.current();
        if transform_id.is_root() {
            return mask;
        }

        let scale_factor = self.scale_factor();
        let world_transform = self
            .segment_pool
            .transforms
            .get_world_no_cache(transform_id);

        // Convert origin to ScaledPixels, apply transform, convert back to Pixels
        let origin_scaled = Point::new(
            ScaledPixels(mask.bounds.origin.x.0 * scale_factor),
            ScaledPixels(mask.bounds.origin.y.0 * scale_factor),
        );
        let world_origin = world_transform.apply(origin_scaled);

        // Scale the size by the transform's scale factor
        let world_size = Size {
            width: Pixels(mask.bounds.size.width.0 * world_transform.scale),
            height: Pixels(mask.bounds.size.height.0 * world_transform.scale),
        };

        ContentMask {
            bounds: Bounds {
                origin: Point::new(
                    Pixels(world_origin.x.0 / scale_factor),
                    Pixels(world_origin.y.0 / scale_factor),
                ),
                size: world_size,
            },
        }
    }

    /// Provide elements in the called function with a new namespace in which their identifiers must be unique.
    /// This can be used within a custom element to distinguish multiple sets of child elements.
    pub fn with_element_namespace<R>(
        &mut self,
        element_id: impl Into<ElementId>,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.with_global_id(element_id.into(), |_, window| f(window))
    }

    /// Use a piece of state that exists as long this element is being rendered in consecutive frames.
    pub fn use_keyed_state<S: 'static>(
        &mut self,
        key: impl Into<ElementId>,
        cx: &mut App,
        init: impl FnOnce(&mut Self, &mut Context<S>) -> S,
    ) -> Entity<S> {
        let current_view = self.current_view();
        self.with_global_id(key.into(), |global_id, window| {
            window.with_element_state(global_id, |state: Option<Entity<S>>, window| {
                if let Some(state) = state {
                    (state.clone(), state)
                } else {
                    let new_state = cx.new(|cx| init(window, cx));
                    cx.observe(&new_state, move |_, cx| {
                        cx.notify(current_view);
                    })
                    .detach();
                    (new_state.clone(), new_state)
                }
            })
        })
    }

    /// Immediately push an element ID onto the stack. Useful for simplifying IDs in lists
    pub fn with_id<R>(&mut self, id: impl Into<ElementId>, f: impl FnOnce(&mut Self) -> R) -> R {
        self.with_global_id(id.into(), |_, window| f(window))
    }

    /// Use a piece of state that exists as long this element is being rendered in consecutive frames, without needing to specify a key
    ///
    /// NOTE: This method uses the location of the caller to generate an ID for this state.
    ///       If this is not sufficient to identify your state (e.g. you're rendering a list item),
    ///       you can provide a custom ElementID using the `use_keyed_state` method.
    #[track_caller]
    pub fn use_state<S: 'static>(
        &mut self,
        cx: &mut App,
        init: impl FnOnce(&mut Self, &mut Context<S>) -> S,
    ) -> Entity<S> {
        self.use_keyed_state(
            ElementId::CodeLocation(*core::panic::Location::caller()),
            cx,
            init,
        )
    }

    /// Updates or initializes state for the given element id, stored on the fiber as long as it
    /// persists across frames. The state returned by the closure will be stored and reused the next
    /// time this fiber is drawn. This method should only be called as part of element drawing.
    ///
    pub fn with_element_state<S, R>(
        &mut self,
        global_id: &GlobalElementId,
        f: impl FnOnce(Option<S>, &mut Self) -> (R, S),
    ) -> R
    where
        S: 'static,
    {
        self.invalidator
            .debug_assert_reconcile_or_layout_or_prepaint_or_paint();
        self.with_element_state_inner(global_id, f)
    }

    pub(crate) fn with_element_state_in_event<S, R>(
        &mut self,
        global_id: &GlobalElementId,
        f: impl FnOnce(Option<S>, &mut Self) -> (R, S),
    ) -> R
    where
        S: 'static,
    {
        self.with_element_state_inner(global_id, f)
    }

    fn with_element_state_inner<S, R>(
        &mut self,
        global_id: &GlobalElementId,
        f: impl FnOnce(Option<S>, &mut Self) -> (R, S),
    ) -> R
    where
        S: 'static,
    {
        let fiber_key: DefaultKey = (*global_id).into();
        if !self.fiber.tree.fibers.contains_key(fiber_key) {
            let (result, _discarded) = f(None, self);
            return result;
        }

        let store: Rc<StateStore> = {
            let Some(entry) = self.fiber.tree.element_states.entry(fiber_key) else {
                let (result, _discarded) = f(None, self);
                return result;
            };
            entry.or_insert_with(|| Rc::new(StateStore::new())).clone()
        };

        store.with_mut::<S, R>(|state| f(state, self))
    }

    pub(crate) fn with_input_handler_mut<R>(
        &mut self,
        fiber_id: GlobalElementId,
        cx: &mut App,
        f: impl FnOnce(&mut dyn InputHandler, &mut Window, &mut App) -> R,
    ) -> Option<R> {
        let slot_key: DefaultKey = fiber_id.into();
        let mut handler = self.fiber.tree.input_handlers.remove(slot_key)?;
        let result = f(handler.as_mut(), self, cx);
        self.fiber.tree.input_handlers.insert(slot_key, handler);
        Some(result)
    }

    /// A variant of `with_element_state` that allows the element's id to be optional. This is a convenience
    /// method for elements where the element id may or may not be assigned. Prefer using `with_element_state`
    /// when the element is guaranteed to have an id.
    ///
    /// The first option means 'no ID provided'
    /// The second option means 'not yet initialized'
    pub fn with_optional_element_state<S, R>(
        &mut self,
        global_id: Option<&GlobalElementId>,
        f: impl FnOnce(Option<Option<S>>, &mut Self) -> (R, Option<S>),
    ) -> R
    where
        S: 'static,
    {
        self.invalidator
            .debug_assert_reconcile_or_layout_or_prepaint_or_paint();

        if let Some(global_id) = global_id {
            self.with_element_state(global_id, |state, window| {
                let (result, state) = f(Some(state), window);
                let state =
                    state.expect("you must return some state when you pass some element id");
                (result, state)
            })
        } else {
            let (result, state) = f(None, self);
            debug_assert!(
                state.is_none(),
                "you must not return an element state when passing None for the global id"
            );
            result
        }
    }

    /// Executes the given closure within the context of a tab group.
    #[inline]
    pub fn with_tab_group<R>(&mut self, index: Option<isize>, f: impl FnOnce(&mut Self) -> R) -> R {
        self.fibers().with_tab_group(index, f)
    }

    /// Begins a tab group scope. Must be paired with `end_tab_group`.
    /// This is useful for retained node implementations where children are painted
    /// between begin and end calls.
    pub fn begin_tab_group(&mut self, index: isize) {
        self.invalidator.debug_assert_paint();
        self.fiber.rendered_tab_stops.begin_group(index);
    }

    /// Ends a tab group scope started by `begin_tab_group`.
    pub fn end_tab_group(&mut self) {
        self.invalidator.debug_assert_paint();
        self.fiber.rendered_tab_stops.end_group();
    }

    /// Creates a fiber for a dynamically rendered element.
    /// This is used by virtualized lists and other elements that create children dynamically.
    /// Returns the global element ID that can be used with `with_element_context`.
    pub fn create_element_fiber(&mut self, element: &AnyElement) -> GlobalElementId {
        self.fiber.tree.create_fiber_for(element)
    }

    /// Checks if a fiber exists for the given element ID.
    /// This is useful for virtualized lists to check if an item's fiber is still valid.
    pub fn element_fiber_exists(&self, id: &GlobalElementId) -> bool {
        self.fiber.tree.get(id).is_some()
    }

    /// Removes a fiber for a dynamically rendered element.
    /// This should be called when a dynamic element is no longer needed.
    pub fn remove_element_fiber(&mut self, id: &GlobalElementId) {
        self.fiber.tree.remove(id);
    }

    /// Executes the given closure within the context of a specific element fiber.
    /// This sets up the element ID stack so that child elements are properly associated
    /// with the parent fiber.
    pub fn with_element_context<R>(
        &mut self,
        fiber_id: GlobalElementId,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.push_fiber_id(fiber_id);
        let result = f(self);
        self.pop_fiber_id();
        result
    }

    /// Measures an element's size using the fiber-backed layout pipeline.
    ///
    /// This is the preferred way to measure elements for sizing probes (e.g., virtualized
    /// lists measuring item heights). This goes through the retained fiber/node pipeline,
    /// ensuring that RenderNode::measure is used for
    /// leaf sizing and that layout context (text style, image cache) is properly inherited.
    ///
    /// The measurement is performed in a temporary fiber subtree that:
    /// - Does NOT affect focus state (focusable_fibers is not modified)
    /// - Does NOT register view roots
    /// - Does NOT run prepaint or paint
    /// - Is automatically cleaned up after measurement
    ///
    /// Returns the computed size of the element.
    pub(crate) fn measure_element_via_fibers(
        &mut self,
        element: &mut AnyElement,
        available_space: Size<AvailableSpace>,
        cx: &mut App,
    ) -> Size<Pixels> {
        // Create a temporary measurement root fiber.
        //
        // Use a placeholder fiber so we don't accidentally participate in keyed
        // view-root bookkeeping (`tree.view_roots`) during cleanup.
        let measure_root = self.fiber.tree.create_placeholder_fiber();

        // Expand wrapper elements BEFORE reconciliation.
        element.expand_wrappers(self, cx);
        element.expand_views_detached(self, cx);

        // Reconcile the element subtree into the measurement root
        self.fiber
            .tree
            .reconcile_wrapper(&measure_root, element, false);

        // Install render nodes using measurement-safe variant (no focus/view mutations)
        self.fibers()
            .cache_fiber_payloads_measurement(&measure_root, element, cx);

        self.prepare_detached_root_for_layout(measure_root, cx);

        // Scope layout engine state so we don't clobber the main frame's state
        let saved_fibers_layout_changed =
            std::mem::take(&mut self.layout_engine.fibers_layout_changed);
        let saved_pending_measure_calls =
            std::mem::take(&mut self.layout_engine.pending_measure_calls);

        // Compute layout
        self.compute_layout_for_fiber(measure_root, available_space, cx);

        // Read the computed size
        let layout_id = TaffyLayoutEngine::layout_id(&measure_root);
        let bounds = self.with_layout_engine(|layout_engine, window| {
            layout_engine.layout_bounds(window, layout_id)
        });

        // Restore layout engine state
        self.layout_engine.fibers_layout_changed = saved_fibers_layout_changed;
        self.layout_engine.pending_measure_calls = saved_pending_measure_calls;

        // Clean up the temporary measurement subtree
        self.fiber.tree.remove(&measure_root);

        bounds.size
    }

    /// Draws an element using the fiber-backed rendering pipeline.
    ///
    /// This is similar to `measure_element_via_fibers` but also performs prepaint and paint.
    /// It supports views by using the full reconciliation path with view expansion.
    ///
    /// Used by test utilities to render elements through the retained node pipeline.
    ///
    /// Returns the computed bounds of the rendered element.
    #[cfg(any(test, feature = "test-support"))]
    pub(crate) fn draw_element_via_fibers(
        &mut self,
        element: &mut AnyElement,
        origin: Point<Pixels>,
        available_space: Size<AvailableSpace>,
        cx: &mut App,
    ) -> Bounds<Pixels> {
        // Phase 1: Reconcile
        // Set the phase for reconciliation (required by debug assertions in cache_fiber_payloads and expand_view_fibers)
        self.invalidator.set_phase(DrawPhase::Reconcile);

        // Create a temporary root fiber for this draw call.
        let draw_root = self.fiber.tree.create_placeholder_fiber();

        // Reconcile the element subtree into the draw root
        self.fiber
            .tree
            .reconcile_wrapper(&draw_root, element, false);

        // Save the current structure epoch so transient draw fibers don't
        // force incremental collections to rebuild for the main rendered tree.
        let saved_structure_epoch = self.fiber.tree.structure_epoch;

        // Install render nodes. This registers view roots so expand_view_fibers can find them.
        self.fibers().cache_fiber_payloads(&draw_root, element, cx);
        self.expand_view_fibers(draw_root, cx);

        self.prepare_detached_root_for_layout(draw_root, cx);

        // Phase 2: Layout
        // Scope layout engine state so we don't clobber the main frame's state
        let saved_fibers_layout_changed =
            std::mem::take(&mut self.layout_engine.fibers_layout_changed);
        let saved_pending_measure_calls =
            std::mem::take(&mut self.layout_engine.pending_measure_calls);

        // Setup taffy styles from fibers (calls RenderNode::layout_begin/end)
        self.invalidator.set_phase(DrawPhase::Layout);

        // Compute layout
        self.compute_layout_for_fiber(draw_root, available_space, cx);

        // Read the computed bounds
        let layout_id = TaffyLayoutEngine::layout_id(&draw_root);
        let bounds = self.with_layout_engine(|layout_engine, window| {
            layout_engine.layout_bounds(window, layout_id)
        });

        // Prepaint at the specified origin
        self.invalidator.set_phase(DrawPhase::Prepaint);
        self.with_absolute_element_offset(origin, |window| {
            context::PrepaintCx::new(window).prepaint_fiber_tree(draw_root, cx)
        });

        // Ensure preorder indices are set before paint
        self.fiber.tree.ensure_preorder_indices();

        // Paint
        self.invalidator.set_phase(DrawPhase::Paint);
        context::PaintCx::new(self).paint_fiber_tree(draw_root, cx);

        // Snapshot hitboxes
        self.snapshot_hitboxes_into_rendered_frame();

        // Reset phase
        self.invalidator.set_phase(DrawPhase::None);

        // Restore layout engine state
        self.layout_engine.fibers_layout_changed = saved_fibers_layout_changed;
        self.layout_engine.pending_measure_calls = saved_pending_measure_calls;

        // Clean up the temporary draw subtree
        self.fiber.tree.remove(&draw_root);

        // Restore structure epoch to avoid perturbing the main tree's incremental
        // ordering/caching mechanisms.
        self.fiber.tree.structure_epoch = saved_structure_epoch;

        // Return bounds offset by origin
        Bounds {
            origin,
            size: bounds.size,
        }
    }

    /// Registers a focus handle as a tab stop for the current frame.
    ///
    /// This method should only be called during the paint phase of element drawing.
    pub fn register_tab_stop(&mut self, focus_handle: &FocusHandle, tab_index: isize) {
        self.invalidator.debug_assert_paint();
        self.with_fiber_cx(|fiber| fiber.register_tab_stop(focus_handle, tab_index));
    }

    /// Defers the drawing of the given element, scheduling it to be painted on top of the currently-drawn tree
    /// at a later time. The `priority` parameter determines the drawing order relative to other deferred elements,
    /// with higher values being drawn on top.
    ///
    /// This method should only be called as part of the prepaint phase of element drawing.
    #[track_caller]
    pub fn defer_draw(
        &mut self,
        element: AnyElement,
        absolute_offset: Point<Pixels>,
        priority: usize,
    ) {
        self.invalidator.debug_assert_layout_or_prepaint();
        let callsite = core::panic::Location::caller();
        self.with_fiber_cx(|fiber| fiber.defer_draw(element, absolute_offset, priority, callsite));
    }

    /// Creates a new painting layer for the specified bounds. A "layer" is a batch
    /// of geometry that are non-overlapping and have the same draw order. This is typically used
    /// for performance reasons. Bounds are used only to skip creating empty layers.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_layer<R>(&mut self, bounds: Bounds<Pixels>, f: impl FnOnce(&mut Self) -> R) -> R {
        self.invalidator.debug_assert_paint();

        let scale_factor = self.scale_factor();
        let content_mask = self.content_mask().scale(scale_factor);
        let local_bounds = bounds.scale(scale_factor);
        let world_transform = self
            .segment_pool
            .transforms
            .get_world_no_cache(self.transform_stack.current());
        let world_bounds = Bounds {
            origin: world_transform.apply(local_bounds.origin),
            size: Size {
                width: ScaledPixels(local_bounds.size.width.0 * world_transform.scale),
                height: ScaledPixels(local_bounds.size.height.0 * world_transform.scale),
            },
        };

        let clipped_bounds = world_bounds.intersect(&content_mask.bounds);
        let pushed = !clipped_bounds.is_empty();
        if pushed {
            self.next_frame.scene.push_layer(&mut self.segment_pool);
        }

        let result = f(self);

        if pushed {
            self.next_frame.scene.pop_layer(&mut self.segment_pool);
        }

        result
    }

    /// Paint one or more drop shadows into the scene for the next frame at the current z-index.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_shadows(
        &mut self,
        bounds: Bounds<Pixels>,
        corner_radii: Corners<Pixels>,
        shadows: &[BoxShadow],
    ) {
        self.paint_shadows_with_transform(
            bounds,
            corner_radii,
            shadows,
            TransformationMatrix::unit(),
        );
    }

    /// Paint one or more drop shadows with an explicit visual transform.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_shadows_with_transform(
        &mut self,
        bounds: Bounds<Pixels>,
        corner_radii: Corners<Pixels>,
        shadows: &[BoxShadow],
        transform: TransformationMatrix,
    ) {
        self.invalidator.debug_assert_paint();

        let scale_factor = self.scale_factor();
        let content_mask = self.content_mask();
        let opacity = self.element_opacity();
        let transform_index = self.transform_stack.current().as_u32();
        let transform = self.scale_transform_for_scene(transform);
        let cull = self.should_cull_scene_primitives();
        for shadow in shadows {
            let shadow_bounds = (bounds + shadow.offset).dilate(shadow.spread_radius);
            self.next_frame.scene.insert_primitive(
                &mut self.segment_pool,
                (
                    Shadow {
                        order: 0,
                        blur_radius: shadow.blur_radius.scale(scale_factor),
                        transform_index,
                        pad: 0,
                        bounds: shadow_bounds.scale(scale_factor),
                        content_mask: content_mask.scale(scale_factor),
                        corner_radii: corner_radii.scale(scale_factor),
                        color: shadow.color.opacity(opacity),
                    },
                    transform,
                ),
                cull,
            );
        }
    }

    /// Paint one or more quads into the scene for the next frame at the current stacking context.
    /// Quads are colored rectangular regions with an optional background, border, and corner radius.
    /// see [`fill`], [`outline`], and [`quad`] to construct this type.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    ///
    /// Note that the `quad.corner_radii` are allowed to exceed the bounds, creating sharp corners
    /// where the circular arcs meet. This will not display well when combined with dashed borders.
    /// Use `Corners::clamp_radii_for_quad_size` if the radii should fit within the bounds.
    pub fn paint_quad(&mut self, quad: PaintQuad) {
        self.paint_quad_with_transform(quad, TransformationMatrix::unit());
    }

    /// Paint one or more quads with an explicit visual transform.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_quad_with_transform(&mut self, quad: PaintQuad, transform: TransformationMatrix) {
        self.invalidator.debug_assert_paint();

        let scale_factor = self.scale_factor();
        let content_mask = self.content_mask();
        let opacity = self.element_opacity();
        let transform_index = self.transform_stack.current().as_u32();
        let transform = self.scale_transform_for_scene(transform);
        let cull = self.should_cull_scene_primitives();

        self.next_frame.scene.insert_primitive(
            &mut self.segment_pool,
            (
                Quad {
                    order: 0,
                    transform_index,
                    pad: 0,
                    bounds: quad.bounds.scale(scale_factor),
                    content_mask: content_mask.scale(scale_factor),
                    background: quad.background.opacity(opacity),
                    border_color: quad.border_color.opacity(opacity),
                    corner_radii: quad.corner_radii.scale(scale_factor),
                    border_widths: quad.border_widths.scale(scale_factor),
                    border_style: quad.border_style,
                },
                transform,
            ),
            cull,
        );
    }

    /// Paint the given `Path` into the scene for the next frame at the current z-index.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_path(&mut self, mut path: Path<Pixels>, color: impl Into<Background>) {
        self.invalidator.debug_assert_paint();

        path.transform_index = self.transform_stack.current().as_u32();
        let scale_factor = self.scale_factor();
        let content_mask = self.content_mask();
        let opacity = self.element_opacity();
        path.content_mask = content_mask;
        let color: Background = color.into();
        path.color = color.opacity(opacity);
        let cull = self.should_cull_scene_primitives();
        self.next_frame.scene.insert_primitive(
            &mut self.segment_pool,
            path.scale(scale_factor),
            cull,
        );
    }

    /// Paint an underline into the scene for the next frame at the current z-index.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_underline(
        &mut self,
        origin: Point<Pixels>,
        width: Pixels,
        style: &UnderlineStyle,
    ) {
        self.paint_underline_with_transform(origin, width, style, TransformationMatrix::unit());
    }

    /// Paint an underline with an explicit visual transform.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_underline_with_transform(
        &mut self,
        origin: Point<Pixels>,
        width: Pixels,
        style: &UnderlineStyle,
        transform: TransformationMatrix,
    ) {
        self.invalidator.debug_assert_paint();

        let scale_factor = self.scale_factor();
        let height = if style.wavy {
            style.thickness * 3.
        } else {
            style.thickness
        };
        let bounds = Bounds {
            origin,
            size: size(width, height),
        };
        let content_mask = self.content_mask();
        let element_opacity = self.element_opacity();
        let transform_index = self.transform_stack.current().as_u32();
        let transform = self.scale_transform_for_scene(transform);
        let cull = self.should_cull_scene_primitives();

        self.next_frame.scene.insert_primitive(
            &mut self.segment_pool,
            (
                Underline {
                    order: 0,
                    transform_index,
                    bounds: bounds.scale(scale_factor),
                    content_mask: content_mask.scale(scale_factor),
                    color: style.color.unwrap_or_default().opacity(element_opacity),
                    thickness: style.thickness.scale(scale_factor),
                    wavy: if style.wavy { 1 } else { 0 },
                },
                transform,
            ),
            cull,
        );
    }

    /// Paint a strikethrough into the scene for the next frame at the current z-index.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_strikethrough(
        &mut self,
        origin: Point<Pixels>,
        width: Pixels,
        style: &StrikethroughStyle,
    ) {
        self.paint_strikethrough_with_transform(origin, width, style, TransformationMatrix::unit());
    }

    /// Paint a strikethrough with an explicit visual transform.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_strikethrough_with_transform(
        &mut self,
        origin: Point<Pixels>,
        width: Pixels,
        style: &StrikethroughStyle,
        transform: TransformationMatrix,
    ) {
        self.invalidator.debug_assert_paint();

        let scale_factor = self.scale_factor();
        let height = style.thickness;
        let bounds = Bounds {
            origin,
            size: size(width, height),
        };
        let content_mask = self.content_mask();
        let opacity = self.element_opacity();
        let transform_index = self.transform_stack.current().as_u32();
        let transform = self.scale_transform_for_scene(transform);
        let cull = self.should_cull_scene_primitives();

        self.next_frame.scene.insert_primitive(
            &mut self.segment_pool,
            (
                Underline {
                    order: 0,
                    transform_index,
                    bounds: bounds.scale(scale_factor),
                    content_mask: content_mask.scale(scale_factor),
                    thickness: style.thickness.scale(scale_factor),
                    color: style.color.unwrap_or_default().opacity(opacity),
                    wavy: 0,
                },
                transform,
            ),
            cull,
        );
    }

    /// Paints a monochrome (non-emoji) glyph into the scene for the next frame at the current z-index.
    ///
    /// The y component of the origin is the baseline of the glyph.
    /// You should generally prefer to use the [`ShapedLine::paint`](crate::ShapedLine::paint) or
    /// [`WrappedLine::paint`](crate::WrappedLine::paint) methods in the [`TextSystem`](crate::TextSystem).
    /// This method is only useful if you need to paint a single glyph that has already been shaped.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_glyph(
        &mut self,
        origin: Point<Pixels>,
        font_id: FontId,
        glyph_id: GlyphId,
        font_size: Pixels,
        color: Hsla,
    ) -> Result<()> {
        self.paint_glyph_with_transform(
            origin,
            font_id,
            glyph_id,
            font_size,
            color,
            TransformationMatrix::unit(),
        )
    }

    /// Paints a monochrome glyph with an explicit visual transform.
    pub fn paint_glyph_with_transform(
        &mut self,
        origin: Point<Pixels>,
        font_id: FontId,
        glyph_id: GlyphId,
        font_size: Pixels,
        color: Hsla,
        transform: TransformationMatrix,
    ) -> Result<()> {
        self.invalidator.debug_assert_paint();

        let element_opacity = self.element_opacity();
        let scale_factor = self.scale_factor();
        let glyph_origin = origin.scale(scale_factor);
        let transform_index = self.transform_stack.current().as_u32();
        let transform = self.scale_transform_for_scene(transform);

        let subpixel_variant = Point {
            x: (glyph_origin.x.0.fract() * SUBPIXEL_VARIANTS_X as f32).floor() as u8,
            y: (glyph_origin.y.0.fract() * SUBPIXEL_VARIANTS_Y as f32).floor() as u8,
        };
        let subpixel_rendering = self.should_use_subpixel_rendering(font_id, font_size);
        let params = RenderGlyphParams {
            font_id,
            glyph_id,
            font_size,
            subpixel_variant,
            scale_factor,
            is_emoji: false,
            subpixel_rendering,
        };

        let raster_bounds = self.text_system().raster_bounds(&params)?;
        if !raster_bounds.is_zero() {
            let tile = self
                .sprite_atlas
                .get_or_insert_with(&params.clone().into(), &mut || {
                    let (size, bytes) = self.text_system().rasterize_glyph(&params)?;
                    Ok(Some((size, Cow::Owned(bytes))))
                })?
                .expect("Callback above only errors or returns Some");
            let bounds = Bounds {
                origin: glyph_origin.map(|px| px.floor()) + raster_bounds.origin.map(Into::into),
                size: tile.bounds.size.map(Into::into),
            };
            let content_mask = self.content_mask().scale(scale_factor);
            let cull = self.should_cull_scene_primitives();

            if subpixel_rendering {
                self.next_frame.scene.insert_primitive(
                    &mut self.segment_pool,
                    SubpixelSprite {
                        order: 0,
                        transform_index,
                        bounds,
                        content_mask,
                        color: color.opacity(element_opacity),
                        tile,
                        transformation: TransformationMatrix::unit(),
                    },
                    cull,
                );
            } else {
                self.next_frame.scene.insert_primitive(
                    &mut self.segment_pool,
                    MonochromeSprite {
                        order: 0,
                        transform_index,
                        bounds,
                        content_mask,
                        color: color.opacity(element_opacity),
                        tile,
                        transformation: transform,
                    },
                    cull,
                );
            }
        }
        Ok(())
    }

    fn should_use_subpixel_rendering(&self, font_id: FontId, font_size: Pixels) -> bool {
        if self.platform_window.background_appearance() != WindowBackgroundAppearance::Opaque {
            return false;
        }

        if !self.platform_window.is_subpixel_rendering_supported() {
            return false;
        }

        let mode = match self.text_rendering_mode.get() {
            TextRenderingMode::PlatformDefault => self
                .text_system()
                .recommended_rendering_mode(font_id, font_size),
            mode => mode,
        };

        mode == TextRenderingMode::Subpixel
    }

    /// Paints an emoji glyph into the scene for the next frame at the current z-index.
    ///
    /// The y component of the origin is the baseline of the glyph.
    /// You should generally prefer to use the [`ShapedLine::paint`](crate::ShapedLine::paint) or
    /// [`WrappedLine::paint`](crate::WrappedLine::paint) methods in the [`TextSystem`](crate::TextSystem).
    /// This method is only useful if you need to paint a single emoji that has already been shaped.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_emoji(
        &mut self,
        origin: Point<Pixels>,
        font_id: FontId,
        glyph_id: GlyphId,
        font_size: Pixels,
    ) -> Result<()> {
        self.paint_emoji_with_transform(
            origin,
            font_id,
            glyph_id,
            font_size,
            TransformationMatrix::unit(),
        )
    }

    /// Paints an emoji glyph with an explicit visual transform.
    pub fn paint_emoji_with_transform(
        &mut self,
        origin: Point<Pixels>,
        font_id: FontId,
        glyph_id: GlyphId,
        font_size: Pixels,
        transform: TransformationMatrix,
    ) -> Result<()> {
        self.invalidator.debug_assert_paint();

        let scale_factor = self.scale_factor();
        let glyph_origin = origin.scale(scale_factor);
        let transform_index = self.transform_stack.current().as_u32();
        let transform = self.scale_transform_for_scene(transform);
        let params = RenderGlyphParams {
            font_id,
            glyph_id,
            font_size,
            // We don't render emojis with subpixel variants.
            subpixel_variant: Default::default(),
            scale_factor,
            is_emoji: true,
            subpixel_rendering: false,
        };

        let raster_bounds = self.text_system().raster_bounds(&params)?;
        if !raster_bounds.is_zero() {
            let tile = self
                .sprite_atlas
                .get_or_insert_with(&params.clone().into(), &mut || {
                    let (size, bytes) = self.text_system().rasterize_glyph(&params)?;
                    Ok(Some((size, Cow::Owned(bytes))))
                })?
                .expect("Callback above only errors or returns Some");

            let bounds = Bounds {
                origin: glyph_origin.map(|px| px.floor()) + raster_bounds.origin.map(Into::into),
                size: tile.bounds.size.map(Into::into),
            };
            let content_mask = self.content_mask().scale(scale_factor);
            let opacity = self.element_opacity();
            let cull = self.should_cull_scene_primitives();

            self.next_frame.scene.insert_primitive(
                &mut self.segment_pool,
                (
                    PolychromeSprite {
                        order: 0,
                        transform_index,
                        grayscale: false,
                        bounds,
                        corner_radii: Default::default(),
                        content_mask,
                        tile,
                        opacity,
                    },
                    transform,
                ),
                cull,
            );
        }
        Ok(())
    }

    /// Paint a monochrome SVG into the scene for the next frame at the current stacking context.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_svg(
        &mut self,
        bounds: Bounds<Pixels>,
        path: SharedString,
        mut data: Option<&[u8]>,
        transformation: TransformationMatrix,
        color: Hsla,
        cx: &App,
    ) -> Result<()> {
        self.invalidator.debug_assert_paint();

        let element_opacity = self.element_opacity();
        let scale_factor = self.scale_factor();
        let transform_index = self.transform_stack.current().as_u32();

        let bounds = bounds.scale(scale_factor);
        let params = RenderSvgParams {
            path,
            size: bounds.size.map(|pixels| {
                DevicePixels::from((pixels.0 * SMOOTH_SVG_SCALE_FACTOR).ceil() as i32)
            }),
        };

        let Some(tile) =
            self.sprite_atlas
                .get_or_insert_with(&params.clone().into(), &mut || {
                    let Some((size, bytes)) = cx.svg_renderer.render_alpha_mask(&params, data)?
                    else {
                        return Ok(None);
                    };
                    Ok(Some((size, Cow::Owned(bytes))))
                })?
        else {
            return Ok(());
        };
        let content_mask = self.content_mask().scale(scale_factor);
        let svg_bounds = Bounds {
            origin: bounds.center()
                - Point::new(
                    ScaledPixels(tile.bounds.size.width.0 as f32 / SMOOTH_SVG_SCALE_FACTOR / 2.),
                    ScaledPixels(tile.bounds.size.height.0 as f32 / SMOOTH_SVG_SCALE_FACTOR / 2.),
                ),
            size: tile
                .bounds
                .size
                .map(|value| ScaledPixels(value.0 as f32 / SMOOTH_SVG_SCALE_FACTOR)),
        };
        let cull = self.should_cull_scene_primitives();

        self.next_frame.scene.insert_primitive(
            &mut self.segment_pool,
            MonochromeSprite {
                order: 0,
                transform_index,
                bounds: svg_bounds
                    .map_origin(|origin| origin.round())
                    .map_size(|size| size.ceil()),
                content_mask,
                color: color.opacity(element_opacity),
                tile,
                transformation,
            },
            cull,
        );

        Ok(())
    }

    /// Paint an image into the scene for the next frame at the current z-index.
    /// This method will panic if the frame_index is not valid
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn paint_image(
        &mut self,
        bounds: Bounds<Pixels>,
        corner_radii: Corners<Pixels>,
        data: Arc<RenderImage>,
        frame_index: usize,
        grayscale: bool,
    ) -> Result<()> {
        self.paint_image_with_transform(
            bounds,
            corner_radii,
            data,
            frame_index,
            grayscale,
            TransformationMatrix::unit(),
        )
    }

    /// Paint an image with an explicit visual transform.
    pub fn paint_image_with_transform(
        &mut self,
        bounds: Bounds<Pixels>,
        corner_radii: Corners<Pixels>,
        data: Arc<RenderImage>,
        frame_index: usize,
        grayscale: bool,
        transform: TransformationMatrix,
    ) -> Result<()> {
        self.invalidator.debug_assert_paint();

        let scale_factor = self.scale_factor();
        let bounds = bounds.scale(scale_factor);
        let params = RenderImageParams {
            image_id: data.id,
            frame_index,
        };

        let tile = self
            .sprite_atlas
            .get_or_insert_with(&params.into(), &mut || {
                Ok(Some((
                    data.size(frame_index),
                    Cow::Borrowed(
                        data.as_bytes(frame_index)
                            .expect("It's the caller's job to pass a valid frame index"),
                    ),
                )))
            })?
            .expect("Callback above only returns Some");
        let content_mask = self.content_mask().scale(scale_factor);
        let corner_radii = corner_radii.scale(scale_factor);
        let opacity = self.element_opacity();
        let transform_index = self.transform_stack.current().as_u32();
        let transform = self.scale_transform_for_scene(transform);
        let cull = self.should_cull_scene_primitives();

        self.next_frame.scene.insert_primitive(
            &mut self.segment_pool,
            (
                PolychromeSprite {
                    order: 0,
                    transform_index,
                    grayscale,
                    bounds: bounds
                        .map_origin(|origin| origin.floor())
                        .map_size(|size| size.ceil()),
                    content_mask,
                    corner_radii,
                    tile,
                    opacity,
                },
                transform,
            ),
            cull,
        );
        Ok(())
    }

    fn scale_transform_for_scene(&self, transform: TransformationMatrix) -> TransformationMatrix {
        if transform.is_unit() {
            return transform;
        }
        let scale_factor = self.scale_factor();
        let mut scaled = transform;
        scaled.translation[0] *= scale_factor;
        scaled.translation[1] *= scale_factor;
        scaled
    }

    /// Paint a surface into the scene for the next frame at the current z-index.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    #[cfg(target_os = "macos")]
    pub fn paint_surface(&mut self, bounds: Bounds<Pixels>, image_buffer: CVPixelBuffer) {
        use crate::PaintSurface;

        self.invalidator.debug_assert_paint();

        let scale_factor = self.scale_factor();
        let bounds = bounds.scale(scale_factor);
        let content_mask = self.content_mask().scale(scale_factor);
        let transform_index = self.transform_stack.current().as_u32();
        let cull = self.should_cull_scene_primitives();
        self.next_frame.scene.insert_primitive(
            &mut self.segment_pool,
            PaintSurface {
                order: 0,
                transform_index,
                bounds,
                content_mask,
                image_buffer,
            },
            cull,
        );
    }

    /// Removes an image from the sprite atlas.
    pub fn drop_image(&mut self, data: Arc<RenderImage>) -> Result<()> {
        for frame_index in 0..data.frame_count() {
            let params = RenderImageParams {
                image_id: data.id,
                frame_index,
            };

            self.sprite_atlas.remove(&params.clone().into());
        }

        Ok(())
    }

    pub(crate) fn push_fiber_id(&mut self, id: GlobalElementId) {
        self.fibers().push_fiber_id(id);
    }

    pub(crate) fn pop_fiber_id(&mut self) {
        self.fibers().pop_fiber_id();
    }

    pub(crate) fn current_fiber_id(&self) -> Option<GlobalElementId> {
        self.fibers_ref().current_fiber_id()
    }

    pub(crate) fn with_element_id_stack<R>(
        &mut self,
        fiber_id: &GlobalElementId,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        if self.current_fiber_id() == Some(*fiber_id) {
            return f(self);
        }
        self.push_fiber_id(*fiber_id);
        let result = f(self);
        self.pop_fiber_id();
        result
    }

    /// Ensure a fiber exists for the current fiber scope, creating one if necessary.
    pub(crate) fn ensure_fiber_for_current_id(&mut self) -> GlobalElementId {
        self.fibers().ensure_fiber_for_current_id()
    }

    /// Ensure a pending fiber exists for a view root outside of render traversal.
    pub(crate) fn ensure_view_root_fiber(&mut self, view_id: EntityId) -> GlobalElementId {
        self.fibers().ensure_view_root_fiber(view_id)
    }

    pub(crate) fn record_pending_view_accesses(
        &mut self,
        fiber_id: &GlobalElementId,
        accessed: FxHashSet<EntityId>,
    ) {
        if accessed.is_empty() {
            return;
        }
        self.pending_view_accesses
            .entry(*fiber_id)
            .or_insert_with(FxHashSet::default)
            .extend(accessed);
    }

    pub(crate) fn take_pending_view_accesses(
        &mut self,
        fiber_id: &GlobalElementId,
    ) -> Option<FxHashSet<EntityId>> {
        self.pending_view_accesses.remove(&fiber_id)
    }

    pub(crate) fn hydrate_view_children(&self, element: &mut AnyElement) {
        // Recursively hydrate children
        for child in element.children_mut() {
            self.hydrate_view_children(child);
        }
    }

    fn map_view_roots_from_element(
        &mut self,
        fiber_id: &GlobalElementId,
        element: &AnyElement,
        new_view_fibers: &mut Vec<GlobalElementId>,
    ) {
        self.fibers()
            .map_view_roots_from_element(fiber_id, element, new_view_fibers);
    }

    pub(crate) fn should_render_view_fiber(&self, fiber_id: &GlobalElementId) -> bool {
        self.fibers_ref().should_render_view_fiber(fiber_id)
    }

    fn expand_view_fibers(&mut self, root_fiber: GlobalElementId, cx: &mut App) {
        self.fibers().expand_view_fibers(root_fiber, cx);
    }

    pub(crate) fn fiber_view_id(
        &self,
        fiber_id: &GlobalElementId,
        fiber: &crate::Fiber,
    ) -> Option<EntityId> {
        fiber.view_id.or_else(|| {
            self.fiber
                .tree
                .view_state
                .get((*fiber_id).into())
                .and_then(|state| state.view_data.as_ref())
                .map(|view| view.view.entity_id())
        })
    }

    pub(crate) fn paint_svg_paths(
        &mut self,
        bounds: Bounds<Pixels>,
        svg_path: Option<&SharedString>,
        svg_external_path: Option<&SharedString>,
        svg_transformation: Option<crate::Transformation>,
        color: Hsla,
        cx: &mut App,
    ) {
        if bounds.size.is_zero() {
            return;
        }

        let transformation = svg_transformation
            .map(|transformation| transformation.into_matrix(bounds.center(), self.scale_factor()))
            .unwrap_or_default();

        if let Some(path) = svg_path {
            self.paint_svg(bounds, path.clone(), None, transformation, color, cx)
                .log_err();
            return;
        }

        let Some(path) = svg_external_path else {
            return;
        };
        let Some(bytes) = self
            .use_asset::<crate::elements::SvgAsset>(path, cx)
            .and_then(|asset| asset.log_err())
        else {
            return;
        };

        self.paint_svg(
            bounds,
            path.clone(),
            Some(&bytes),
            transformation,
            color,
            cx,
        )
        .log_err();
    }

    fn cache_fiber_payloads(
        &mut self,
        fiber_id: &GlobalElementId,
        element: &mut AnyElement,
        cx: &mut App,
    ) {
        self.fibers().cache_fiber_payloads(fiber_id, element, cx);
    }

    pub(crate) fn remove_rendered_tab_stops_for_fiber(
        &mut self,
        owner_id: GlobalElementId,
        focus_ids: impl IntoIterator<Item = FocusId>,
    ) {
        for focus_id in focus_ids {
            self.fiber
                .rendered_tab_stops
                .remove_if_owned_by(&focus_id, owner_id);
        }
    }

    pub(crate) fn layout_bounds_cached(
        &self,
        global_id: &GlobalElementId,
        scale_factor: f32,
        cache: &mut FxHashMap<GlobalElementId, Bounds<Pixels>>,
    ) -> Bounds<Pixels> {
        crate::taffy::layout_bounds(self, global_id, scale_factor, cache)
    }

    /// Temporarily take the layout engine out of self, use it via the closure, and restore it.
    /// This pattern is needed because layout engine methods require both `&mut self` on the engine
    /// and `&mut Window`.
    fn with_layout_engine<R>(
        &mut self,
        f: impl FnOnce(&mut TaffyLayoutEngine, &mut Self) -> R,
    ) -> R {
        let mut layout_engine =
            std::mem::replace(&mut self.layout_engine, TaffyLayoutEngine::new());
        let result = f(&mut layout_engine, self);
        self.layout_engine = layout_engine;
        result
    }

    /// Add a node to the layout tree for the current frame, using the current fiber scope.
    /// This method is called during [`Element::request_layout`] and enables any element
    /// to participate in layout. Children are implicit in the fiber tree.
    ///
    /// This method should only be called as part of the request_layout or prepaint phase
    /// of element drawing.
    #[must_use]
    pub fn request_layout(
        &mut self,
        style: Style,
        children: impl IntoIterator<Item = LayoutId>,
        cx: &mut App,
    ) -> LayoutId {
        let fiber_id = self.ensure_fiber_for_current_id();
        self.invalidator.debug_assert_layout_or_prepaint();
        let children: Vec<_> = children.into_iter().collect();
        self.with_layout_engine(|layout_engine, window| {
            layout_engine.request_layout(window, fiber_id, style, children, cx)
        })
    }

    /// Add a node to the layout tree for the current frame. Instead of taking a `Style` and children,
    /// this variant takes a function that is invoked during layout so you can use arbitrary logic to
    /// determine the element's size. One place this is used internally is when measuring text.
    ///
    /// The given closure is invoked at layout time with the known dimensions and available space and
    /// returns a `Size`.
    ///
    /// This method should only be called as part of the request_layout or prepaint phase of element drawing.
    /// For better performance with caching, use `request_measured_layout_cached` instead.
    pub fn request_measured_layout<F>(&mut self, style: Style, measure: F) -> LayoutId
    where
        F: Fn(Size<Option<Pixels>>, Size<AvailableSpace>, &mut Window, &mut App) -> Size<Pixels>
            + 'static,
    {
        let fiber_id = self.ensure_fiber_for_current_id();
        self.invalidator.debug_assert_layout_or_prepaint();
        self.with_layout_engine(|layout_engine, window| {
            layout_engine.request_measured_layout(window, fiber_id, style, measure)
        })
    }

    /// Request a measured layout with caching support.
    ///
    /// This method should only be called as part of the request_layout or prepaint phase of element drawing.
    pub fn request_measured_layout_cached<F>(
        &mut self,
        style: Style,
        content_hash: u64,
        measure: F,
    ) -> LayoutId
    where
        F: Fn(Size<Option<Pixels>>, Size<AvailableSpace>, &mut Window, &mut App) -> Size<Pixels>
            + 'static,
    {
        let fiber_id = self.ensure_fiber_for_current_id();
        self.invalidator.debug_assert_layout_or_prepaint();
        self.with_layout_engine(|layout_engine, window| {
            layout_engine.request_measured_layout_cached(
                window,
                fiber_id,
                style,
                content_hash,
                measure,
            )
        })
    }

    /// Compute the layout for the given id within the given available space.
    /// This method is called for its side effect, typically by the framework prior to painting.
    /// After calling it, you can request the bounds of the given layout node id or any descendant.
    ///
    /// This method should only be called as part of the prepaint phase of element drawing.
    pub fn compute_layout(
        &mut self,
        layout_id: LayoutId,
        available_space: Size<AvailableSpace>,
        cx: &mut App,
    ) -> usize {
        self.invalidator.debug_assert_layout_or_prepaint();
        self.with_layout_engine(|layout_engine, window| {
            layout_engine.compute_layout(window, layout_id, available_space, cx)
        })
    }

    pub(crate) fn compute_layout_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        available_space: Size<AvailableSpace>,
        cx: &mut App,
    ) -> usize {
        self.invalidator.debug_assert_layout_or_prepaint();
        self.with_layout_engine(|layout_engine, window| {
            layout_engine.compute_layout_for_fiber(window, fiber_id, available_space, cx)
        })
    }

    /// Obtain the bounds computed for the given LayoutId relative to the window. This method will usually be invoked by
    /// GPUI itself automatically in order to pass your element its `Bounds` automatically.
    ///
    /// This method should only be called as part of element drawing.
    pub fn layout_bounds(&mut self, layout_id: LayoutId) -> Bounds<Pixels> {
        self.invalidator.debug_assert_layout_or_prepaint();
        let mut bounds = self.with_layout_engine(|layout_engine, window| {
            layout_engine.layout_bounds(window, layout_id)
        });
        bounds.origin += self.transform_stack.local_offset();
        bounds
    }

    /// This method should be called during `prepaint`. You can use
    /// the returned [Hitbox] during `paint` or in an event handler
    /// to determine whether the inserted hitbox was the topmost.
    ///
    /// This method should only be called as part of the prepaint phase of element drawing.
    pub fn insert_hitbox(&mut self, bounds: Bounds<Pixels>, behavior: HitboxBehavior) -> Hitbox {
        self.invalidator.debug_assert_layout_or_prepaint();
        self.with_fiber_cx(|fiber| fiber.insert_hitbox(bounds, behavior))
    }

    /// Set a hitbox which will act as a control area of the platform window.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn insert_window_control_hitbox(&mut self, area: WindowControlArea, hitbox: Hitbox) {
        self.invalidator.debug_assert_paint();
        self.next_frame.window_control_hitboxes.push((area, hitbox));
    }

    /// Sets the key context for the current element. This context will be used to translate
    /// keybindings into actions.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn set_key_context(&mut self, context: KeyContext) {
        self.invalidator.debug_assert_paint();
        self.with_fiber_cx(|fiber| fiber.set_key_context(context));
    }

    /// Sets the focus handle for the current element. This handle will be used to manage focus state
    /// and keyboard event dispatch for the element.
    ///
    /// This method should only be called as part of the prepaint phase of element drawing.
    pub fn set_focus_handle(&mut self, focus_handle: &FocusHandle, cx: &App) {
        self.invalidator.debug_assert_layout_or_prepaint();
        let _ = cx;
        self.with_fiber_cx(|fiber| fiber.set_focus_handle(focus_handle));
    }

    /// Sets the focus handle for a specific element identified by its global element id.
    /// This is used when the element's focus handle needs to be registered with a specific fiber.
    ///
    /// This method should only be called as part of the prepaint phase of element drawing.
    pub fn set_focus_handle_for(&mut self, global_id: GlobalElementId, focus_handle: &FocusHandle) {
        self.invalidator.debug_assert_layout_or_prepaint();
        self.with_fiber_cx_for(global_id, |fiber| fiber.set_focus_handle(focus_handle));
    }

    /// Registers a focus handle as a tab stop for the current element.
    /// The focus handle should already have its tab stop configuration set.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn register_tab_stop_handle(&mut self, focus_handle: &FocusHandle) {
        self.invalidator.debug_assert_paint();
        self.with_fiber_cx(|fiber| fiber.register_tab_stop_handle(focus_handle));
    }

    /// Sets the view id for the current element, which will be used to manage view caching.
    ///
    /// This method should only be called as part of element prepaint. We plan on removing this
    /// method eventually when we solve some issues that require us to construct editor elements
    /// directly instead of always using editors via views.
    pub fn set_view_id(&mut self, view_id: EntityId) {
        self.invalidator.debug_assert_layout_or_prepaint();
        let _ = view_id;
    }

    /// Get the entity ID for the currently rendering view
    pub fn current_view(&self) -> EntityId {
        debug_assert!(
            matches!(
                self.invalidator.phase(),
                DrawPhase::Reconcile | DrawPhase::Layout | DrawPhase::Prepaint | DrawPhase::Paint
            ),
            "Window::current_view can only be called during reconciliation, layout, prepaint, or paint"
        );
        if let Some(id) = self.rendered_entity_stack.last().copied() {
            return id;
        }

        // Render layers and other out-of-tree rendering can legitimately run
        // outside a view's `Element` implementation. When that happens, fall
        // back to the window root view so subsystems like image caching can
        // still associate work with a view.
        self.root
            .as_ref()
            .map(|root| root.entity_id())
            .expect("Window::current_view called with no rendered view and no root view")
    }

    /// Execute `f` while treating `id` as the "current view".
    ///
    /// This is primarily intended for render layers and other out-of-tree
    /// rendering that needs a stable view identity for subsystems like image
    /// caching and view-local state.
    pub fn with_rendered_view<R>(&mut self, id: EntityId, f: impl FnOnce(&mut Self) -> R) -> R {
        self.rendered_entity_stack.push(id);
        let result = f(self);
        self.rendered_entity_stack.pop();
        result
    }

    /// Executes the provided function with the specified image cache.
    pub fn with_image_cache<F, R>(&mut self, image_cache: Option<AnyImageCache>, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        if let Some(image_cache) = image_cache {
            self.image_cache_stack.push(image_cache);
            let result = f(self);
            self.image_cache_stack.pop();
            result
        } else {
            f(self)
        }
    }

    /// Sets an input handler, such as [`ElementInputHandler`][element_input_handler], which interfaces with the
    /// platform to receive textual input with proper integration with concerns such
    /// as IME interactions. This handler will be active for the upcoming frame until the following frame is
    /// rendered.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    ///
    /// [element_input_handler]: crate::ElementInputHandler
    pub fn handle_input(
        &mut self,
        focus_handle: &FocusHandle,
        input_handler: impl InputHandler,
        cx: &App,
    ) {
        self.invalidator.debug_assert_paint();
        self.with_fiber_cx(|fiber| fiber.handle_input(focus_handle, input_handler, cx));
    }

    /// Register a mouse event listener on the window for the next frame. The type of event
    /// is determined by the first parameter of the given listener. When the next frame is rendered
    /// the listener will be cleared.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    ///
    /// # Deprecation
    ///
    /// For fiber-native elements implementing `RenderNode`, prefer using the typed listener
    /// methods on `PaintCtx` instead:
    /// - `ctx.on_mouse_down(|event, phase, hitbox, window, cx| ...)`
    /// - `ctx.on_mouse_up(|event, phase, hitbox, window, cx| ...)`
    /// - `ctx.on_mouse_move(|event, phase, hitbox, window, cx| ...)`
    /// - `ctx.on_scroll_wheel(|event, phase, hitbox, window, cx| ...)`
    /// - `ctx.on_click(|event, window, cx| ...)`
    ///
    /// These methods are more efficient as they avoid runtime type downcasting.
    ///
    /// Continue using `on_mouse_event` for:
    /// - Legacy elements that don't use `RenderNode`
    /// - Elements with multiple hitboxes requiring manual hit testing
    /// - Elements that need to listen for events outside their hitbox
    #[deprecated(
        since = "0.1.0",
        note = "For fiber-native elements, use PaintCtx::on_mouse_down/up/move or on_scroll_wheel instead. See method documentation for when to continue using this."
    )]
    pub fn on_mouse_event<Event: MouseEvent>(
        &mut self,
        mut listener: impl FnMut(&Event, DispatchPhase, &mut Window, &mut App) + 'static,
    ) {
        self.invalidator.debug_assert_prepaint_or_paint();
        self.with_fiber_cx(|fiber| fiber.on_mouse_event(listener));
    }

    /// Register a key event listener on this node for the next frame. The type of event
    /// is determined by the first parameter of the given listener. When the next frame is rendered
    /// the listener will be cleared.
    ///
    /// This is a fairly low-level method, so prefer using event handlers on elements unless you have
    /// a specific need to register a listener yourself.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn on_key_event<Event: KeyEvent>(
        &mut self,
        listener: impl Fn(&Event, DispatchPhase, &mut Window, &mut App) + 'static,
    ) {
        self.invalidator.debug_assert_paint();
        self.with_fiber_cx(|fiber| fiber.on_key_event(listener))
    }

    /// Register a modifiers changed event listener on the window for the next frame.
    ///
    /// This is a fairly low-level method, so prefer using event handlers on elements unless you have
    /// a specific need to register a global listener.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn on_modifiers_changed(
        &mut self,
        listener: impl Fn(&ModifiersChangedEvent, &mut Window, &mut App) + 'static,
    ) {
        self.invalidator.debug_assert_paint();
        self.with_fiber_cx(|fiber| fiber.on_modifiers_changed(listener))
    }

    /// Register a listener to be called when the given focus handle or one of its descendants receives focus.
    /// This does not fire if the given focus handle - or one of its descendants - was previously focused.
    /// Returns a subscription and persists until the subscription is dropped.
    pub fn on_focus_in(
        &mut self,
        handle: &FocusHandle,
        cx: &mut App,
        mut listener: impl FnMut(&mut Window, &mut App) + 'static,
    ) -> Subscription {
        let focus_id = handle.id;
        let (subscription, activate) =
            self.new_focus_listener(Box::new(move |event, window, cx| {
                if event.is_focus_in(focus_id) {
                    listener(window, cx);
                }
                true
            }));
        cx.defer(move |_| activate());
        subscription
    }

    /// Register a listener to be called when the given focus handle or one of its descendants loses focus.
    /// Returns a subscription and persists until the subscription is dropped.
    pub fn on_focus_out(
        &mut self,
        handle: &FocusHandle,
        cx: &mut App,
        mut listener: impl FnMut(FocusOutEvent, &mut Window, &mut App) + 'static,
    ) -> Subscription {
        let focus_id = handle.id;
        let (subscription, activate) =
            self.new_focus_listener(Box::new(move |event, window, cx| {
                if let Some(blurred_id) = event.previous_focus_path.last().copied()
                    && event.is_focus_out(focus_id)
                {
                    let event = FocusOutEvent {
                        blurred: WeakFocusHandle {
                            id: blurred_id,
                            handles: Arc::downgrade(&cx.focus_handles),
                        },
                    };
                    listener(event, window, cx)
                }
                true
            }));
        cx.defer(move |_| activate());
        subscription
    }

    pub(crate) fn reset_cursor_style(&mut self, cx: &mut App) {
        // Set the cursor only if we're the active window.
        if self.is_window_hovered() {
            let style = self
                .fibers()
                .cursor_style_for_frame()
                .unwrap_or(CursorStyle::Arrow);
            cx.platform.set_cursor_style(style);
        }
    }

    /// Dispatch a given keystroke as though the user had typed it.
    /// You can create a keystroke with Keystroke::parse("").
    pub fn dispatch_keystroke(&mut self, keystroke: Keystroke, cx: &mut App) -> bool {
        let keystroke = keystroke.with_simulated_ime();
        let prefer_character_input = keystroke.key_char.is_some();
        let result = self.dispatch_event(
            PlatformInput::KeyDown(KeyDownEvent {
                keystroke: keystroke.clone(),
                is_held: false,
                prefer_character_input,
            }),
            cx,
        );
        if !result.propagate {
            return true;
        }

        if let Some(input) = keystroke.key_char
            && let Some(mut input_handler) = self.platform_window.take_input_handler()
        {
            input_handler.dispatch_input(&input);
            self.platform_window.set_input_handler(input_handler);
            return true;
        }

        false
    }

    /// Return a key binding string for an action, to display in the UI. Uses the highest precedence
    /// binding for the action (last binding added to the keymap).
    pub fn keystroke_text_for(&self, action: &dyn Action) -> String {
        self.highest_precedence_binding_for_action(action)
            .map(|binding| {
                binding
                    .keystrokes()
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" ")
            })
            .unwrap_or_else(|| action.name().to_string())
    }

    /// Dispatch a mouse or keyboard event on the window.
    #[profiling::function]
    pub fn dispatch_event(&mut self, event: PlatformInput, cx: &mut App) -> DispatchEventResult {
        // Track whether this input was keyboard-based for focus-visible styling
        self.last_input_modality = match &event {
            PlatformInput::KeyDown(_) | PlatformInput::ModifiersChanged(_) => {
                InputModality::Keyboard
            }
            PlatformInput::MouseDown(e) if e.is_focusing() => InputModality::Mouse,
            _ => self.last_input_modality,
        };

        // Handlers may set this to false by calling `stop_propagation`.
        cx.propagate_event = true;
        // Handlers may set this to true by calling `prevent_default`.
        self.default_prevented = false;

        let event = match event {
            // Track the mouse position with our own state, since accessing the platform
            // API for the mouse position can only occur on the main thread.
            PlatformInput::MouseMove(mouse_move) => {
                self.mouse_position = mouse_move.position;
                self.modifiers = mouse_move.modifiers;
                PlatformInput::MouseMove(mouse_move)
            }
            PlatformInput::MouseDown(mouse_down) => {
                self.mouse_position = mouse_down.position;
                self.modifiers = mouse_down.modifiers;
                PlatformInput::MouseDown(mouse_down)
            }
            PlatformInput::MouseUp(mouse_up) => {
                self.mouse_position = mouse_up.position;
                self.modifiers = mouse_up.modifiers;
                PlatformInput::MouseUp(mouse_up)
            }
            PlatformInput::MousePressure(mouse_pressure) => {
                PlatformInput::MousePressure(mouse_pressure)
            }
            PlatformInput::MouseExited(mouse_exited) => {
                self.mouse_position = mouse_exited.position;
                self.modifiers = mouse_exited.modifiers;
                PlatformInput::MouseExited(mouse_exited)
            }
            PlatformInput::ModifiersChanged(modifiers_changed) => {
                self.modifiers = modifiers_changed.modifiers;
                self.capslock = modifiers_changed.capslock;
                PlatformInput::ModifiersChanged(modifiers_changed)
            }
            PlatformInput::ScrollWheel(scroll_wheel) => {
                self.mouse_position = scroll_wheel.position;
                self.modifiers = scroll_wheel.modifiers;
                PlatformInput::ScrollWheel(scroll_wheel)
            }
            // Translate dragging and dropping of external files from the operating system
            // to internal drag and drop events.
            PlatformInput::FileDrop(file_drop) => match file_drop {
                FileDropEvent::Entered { position, paths } => {
                    self.mouse_position = position;
                    if cx.active_drag.is_none() {
                        cx.active_drag = Some(AnyDrag {
                            value: Arc::new(paths.clone()),
                            view: cx.new(|_| paths).into(),
                            cursor_offset: position,
                            cursor_style: None,
                        });
                    }
                    PlatformInput::MouseMove(MouseMoveEvent {
                        position,
                        pressed_button: Some(MouseButton::Left),
                        modifiers: Modifiers::default(),
                    })
                }
                FileDropEvent::Pending { position } => {
                    self.mouse_position = position;
                    PlatformInput::MouseMove(MouseMoveEvent {
                        position,
                        pressed_button: Some(MouseButton::Left),
                        modifiers: Modifiers::default(),
                    })
                }
                FileDropEvent::Submit { position } => {
                    cx.activate(true);
                    self.mouse_position = position;
                    PlatformInput::MouseUp(MouseUpEvent {
                        button: MouseButton::Left,
                        position,
                        modifiers: Modifiers::default(),
                        click_count: 1,
                    })
                }
                FileDropEvent::Exited => {
                    cx.active_drag.take();
                    PlatformInput::FileDrop(FileDropEvent::Exited)
                }
            },
            PlatformInput::KeyDown(_) | PlatformInput::KeyUp(_) => event,
        };

        if let Some(any_mouse_event) = event.mouse_event() {
            self.dispatch_mouse_event(any_mouse_event, cx);
        } else if let Some(any_key_event) = event.keyboard_event() {
            self.dispatch_key_event(any_key_event, cx);
        }

        if self.invalidator.is_dirty() {
            self.input_rate_tracker.borrow_mut().record_input();
        }

        DispatchEventResult {
            propagate: cx.propagate_event,
            default_prevented: self.default_prevented,
        }
    }

    fn dispatch_mouse_event(&mut self, event: &dyn Any, cx: &mut App) {
        let _event_phase = context::EventPhaseScope::new(self.invalidator.clone());
        context::EventCx::new(self).dispatch_mouse_event(event, cx);
    }

    fn dispatch_key_event(&mut self, event: &dyn Any, cx: &mut App) {
        let event_phase = context::EventPhaseScope::new(self.invalidator.clone());
        if self.invalidator.is_dirty() {
            self.draw(cx);
            event_phase.reassert();
        }

        let mut node_id = self.focus_node_id_in_rendered_frame(self.focus);
        let mut context_stack = self.fibers_ref().context_stack_for_node(node_id);
        if context_stack.is_empty() && self.invalidator.not_drawing() {
            self.draw(cx);
            node_id = self.focus_node_id_in_rendered_frame(self.focus);
            context_stack = self.fibers_ref().context_stack_for_node(node_id);
            event_phase.reassert();
        }

        let mut keystroke: Option<Keystroke> = None;

        if let Some(event) = event.downcast_ref::<ModifiersChangedEvent>() {
            if event.modifiers.number_of_modifiers() == 0
                && self.pending_modifier.modifiers.number_of_modifiers() == 1
                && !self.pending_modifier.saw_keystroke
            {
                let key = match self.pending_modifier.modifiers {
                    modifiers if modifiers.shift => Some("shift"),
                    modifiers if modifiers.control => Some("control"),
                    modifiers if modifiers.alt => Some("alt"),
                    modifiers if modifiers.platform => Some("platform"),
                    modifiers if modifiers.function => Some("function"),
                    _ => None,
                };
                if let Some(key) = key {
                    keystroke = Some(Keystroke {
                        key: key.to_string(),
                        key_char: None,
                        modifiers: Modifiers::default(),
                    });
                }
            }

            if self.pending_modifier.modifiers.number_of_modifiers() == 0
                && event.modifiers.number_of_modifiers() == 1
            {
                self.pending_modifier.saw_keystroke = false
            }
            self.pending_modifier.modifiers = event.modifiers
        } else if let Some(key_down_event) = event.downcast_ref::<KeyDownEvent>() {
            self.pending_modifier.saw_keystroke = true;
            keystroke = Some(key_down_event.keystroke.clone());
        }

        let Some(keystroke) = keystroke else {
            self.finish_dispatch_key_event(event, node_id, context_stack, cx);
            return;
        };

        cx.propagate_event = true;
        self.dispatch_keystroke_interceptors(event, context_stack.clone(), cx);
        if !cx.propagate_event {
            self.finish_dispatch_key_event(event, node_id, context_stack, cx);
            return;
        }

        let mut currently_pending = self.pending_input.take().unwrap_or_default();
        if currently_pending.focus.is_some() && currently_pending.focus != self.focus {
            currently_pending = PendingInput::default();
        }
        let had_pending = !currently_pending.keystrokes.is_empty();

        let pending_keystrokes = currently_pending.keystrokes.clone();
        let match_result =
            self.key_dispatch
                .dispatch_key(pending_keystrokes, keystroke, &context_stack);

        if !match_result.to_replay.is_empty() {
            self.replay_pending_input(match_result.to_replay, cx);
            cx.propagate_event = true;
        }

        if !match_result.pending.is_empty() {
            currently_pending.timer.take();
            currently_pending.keystrokes = match_result.pending;
            currently_pending.focus = self.focus;

            let text_input_requires_timeout = event
                .downcast_ref::<KeyDownEvent>()
                .filter(|key_down| key_down.keystroke.key_char.is_some())
                .and_then(|_| self.platform_window.take_input_handler())
                .map_or(false, |mut input_handler| {
                    let accepts = input_handler.accepts_text_input();
                    self.platform_window.set_input_handler(input_handler);
                    accepts
                });

            currently_pending.needs_timeout |=
                match_result.pending_has_binding || text_input_requires_timeout;

            if currently_pending.needs_timeout {
                currently_pending.timer = Some(self.spawn(cx, async move |cx| {
                    cx.background_executor.timer(Duration::from_secs(1)).await;
                    cx.update(move |window, cx| {
                        let Some(currently_pending) = window
                            .pending_input
                            .take()
                            .filter(|pending| pending.focus == window.focus)
                        else {
                            return;
                        };

                        let node_id = window.focus_node_id_in_rendered_frame(window.focus);
                        let context_stack = window.fibers_ref().context_stack_for_node(node_id);

                        let to_replay = window
                            .key_dispatch
                            .flush_dispatch(currently_pending.keystrokes, &context_stack);

                        window.pending_input_changed(cx);
                        window.replay_pending_input(to_replay, cx)
                    })
                    .log_err();
                }));
            } else {
                currently_pending.timer = None;
            }
            self.pending_input = Some(currently_pending);
            self.pending_input_changed(cx);
            cx.propagate_event = false;
            return;
        }

        let prefer_character_input =
            event
                .downcast_ref::<KeyDownEvent>()
                .is_some_and(|key_down_event| {
                    key_down_event.prefer_character_input
                        && key_down_event.keystroke.key_char.is_some()
                });
        let skip_bindings = event
            .downcast_ref::<KeyDownEvent>()
            .filter(|key_down_event| key_down_event.prefer_character_input)
            .map(|_| {
                self.platform_window
                    .take_input_handler()
                    .map_or(false, |mut input_handler| {
                        let accepts = input_handler.accepts_text_input();
                        self.platform_window.set_input_handler(input_handler);
                        // If modifiers are not excessive (e.g. AltGr), and the input handler is accepting text input,
                        // we prefer the text input over bindings.
                        accepts
                    })
            })
            .unwrap_or(false);

        if (skip_bindings || prefer_character_input) && had_pending {
            self.pending_input = Some(currently_pending);
            self.pending_input_changed(cx);
            cx.propagate_event = false;
            return;
        }

        if !skip_bindings {
            for binding in match_result.bindings {
                self.dispatch_action_on_node(node_id, binding.action.as_ref(), cx);
                if !cx.propagate_event {
                    self.dispatch_keystroke_observers(
                        event,
                        Some(binding.action),
                        match_result.context_stack,
                        cx,
                    );
                    self.pending_input_changed(cx);
                    return;
                }
            }
        }

        self.finish_dispatch_key_event(event, node_id, match_result.context_stack, cx);
        self.pending_input_changed(cx);
    }

    fn finish_dispatch_key_event(
        &mut self,
        event: &dyn Any,
        node_id: GlobalElementId,
        context_stack: Vec<KeyContext>,
        cx: &mut App,
    ) {
        self.dispatch_key_down_up_event(event, node_id, cx);
        if !cx.propagate_event {
            return;
        }

        self.dispatch_modifiers_changed_event(event, node_id, cx);
        if !cx.propagate_event {
            return;
        }

        self.dispatch_keystroke_observers(event, None, context_stack, cx);
    }

    pub(crate) fn pending_input_changed(&mut self, cx: &mut App) {
        self.pending_input_observers
            .clone()
            .retain(&(), |callback| callback(self, cx));
    }

    fn dispatch_key_down_up_event(
        &mut self,
        event: &dyn Any,
        node_id: GlobalElementId,
        cx: &mut App,
    ) {
        self.fibers().dispatch_key_listeners(event, node_id, cx);
    }

    fn dispatch_modifiers_changed_event(
        &mut self,
        event: &dyn Any,
        node_id: GlobalElementId,
        cx: &mut App,
    ) {
        let Some(event) = event.downcast_ref::<ModifiersChangedEvent>() else {
            return;
        };
        self.fibers()
            .dispatch_modifiers_listeners(event, node_id, cx);
    }

    /// Determine whether a potential multi-stroke key binding is in progress on this window.
    pub fn has_pending_keystrokes(&self) -> bool {
        self.pending_input.is_some()
    }

    pub(crate) fn clear_pending_keystrokes(&mut self) {
        self.pending_input.take();
    }

    /// Returns the currently pending input keystrokes that might result in a multi-stroke key binding.
    pub fn pending_input_keystrokes(&self) -> Option<&[Keystroke]> {
        self.pending_input
            .as_ref()
            .map(|pending_input| pending_input.keystrokes.as_slice())
    }

    fn replay_pending_input(&mut self, replays: SmallVec<[Replay; 1]>, cx: &mut App) {
        let node_id = self.focus_node_id_in_rendered_frame(self.focus);
        'replay: for replay in replays {
            let event = KeyDownEvent {
                keystroke: replay.keystroke.clone(),
                is_held: false,
                prefer_character_input: true,
            };

            cx.propagate_event = true;
            for binding in replay.bindings {
                self.dispatch_action_on_node(node_id, binding.action.as_ref(), cx);
                if !cx.propagate_event {
                    self.dispatch_keystroke_observers(
                        &event,
                        Some(binding.action),
                        Vec::default(),
                        cx,
                    );
                    continue 'replay;
                }
            }

            self.dispatch_key_down_up_event(&event, node_id, cx);
            if !cx.propagate_event {
                continue 'replay;
            }
            if let Some(input) = replay.keystroke.key_char.as_ref().cloned()
                && let Some(mut input_handler) = self.platform_window.take_input_handler()
            {
                input_handler.dispatch_input(&input);
                self.platform_window.set_input_handler(input_handler)
            }
        }
    }

    fn focus_node_id_in_rendered_frame(&self, focus_id: Option<FocusId>) -> GlobalElementId {
        self.fibers_ref().focus_node_id_in_rendered_frame(focus_id)
    }

    fn dispatch_action_on_node(
        &mut self,
        node_id: GlobalElementId,
        action: &dyn Action,
        cx: &mut App,
    ) {
        // Capture phase for global actions.
        cx.propagate_event = true;
        if let Some(mut global_listeners) = cx
            .global_action_listeners
            .remove(&action.as_any().type_id())
        {
            for listener in &global_listeners {
                listener(action.as_any(), DispatchPhase::Capture, cx);
                if !cx.propagate_event {
                    break;
                }
            }

            global_listeners.extend(
                cx.global_action_listeners
                    .remove(&action.as_any().type_id())
                    .unwrap_or_default(),
            );

            cx.global_action_listeners
                .insert(action.as_any().type_id(), global_listeners);
        }

        if !cx.propagate_event {
            return;
        }

        if !self
            .fibers()
            .dispatch_window_action_listeners(action, node_id, cx)
        {
            return;
        }

        // Bubble phase for global actions.
        if let Some(mut global_listeners) = cx
            .global_action_listeners
            .remove(&action.as_any().type_id())
        {
            for listener in global_listeners.iter().rev() {
                cx.propagate_event = false; // Actions stop propagation by default during the bubble phase

                listener(action.as_any(), DispatchPhase::Bubble, cx);
                if !cx.propagate_event {
                    break;
                }
            }

            global_listeners.extend(
                cx.global_action_listeners
                    .remove(&action.as_any().type_id())
                    .unwrap_or_default(),
            );

            cx.global_action_listeners
                .insert(action.as_any().type_id(), global_listeners);
        }
    }

    /// Register the given handler to be invoked whenever the global of the given type
    /// is updated.
    pub fn observe_global<G: Global>(
        &mut self,
        cx: &mut App,
        f: impl Fn(&mut Window, &mut App) + 'static,
    ) -> Subscription {
        let window_handle = self.handle;
        let (subscription, activate) = cx.global_observers.insert(
            TypeId::of::<G>(),
            Box::new(move |cx| {
                window_handle
                    .update(cx, |_, window, cx| f(window, cx))
                    .is_ok()
            }),
        );
        cx.defer(move |_| activate());
        subscription
    }

    /// Focus the current window and bring it to the foreground at the platform level.
    pub fn activate_window(&self) {
        self.platform_window.activate();
    }

    /// Minimize the current window at the platform level.
    pub fn minimize_window(&self) {
        self.platform_window.minimize();
    }

    /// Toggle full screen status on the current window at the platform level.
    pub fn toggle_fullscreen(&self) {
        self.platform_window.toggle_fullscreen();
    }

    /// Updates the IME panel position suggestions for languages like japanese, chinese.
    pub fn invalidate_character_coordinates(&self) {
        self.on_next_frame(|window, _cx| {
            if let Some(mut input_handler) = window.platform_window.take_input_handler() {
                if let Some(bounds) = input_handler.selected_bounds() {
                    window.platform_window.update_ime_position(bounds);
                }
                window.platform_window.set_input_handler(input_handler);
            }
        });
    }

    /// Present a platform dialog.
    /// The provided message will be presented, along with buttons for each answer.
    /// When a button is clicked, the returned Receiver will receive the index of the clicked button.
    pub fn prompt<T>(
        &mut self,
        level: PromptLevel,
        message: &str,
        detail: Option<&str>,
        answers: &[T],
        cx: &mut App,
    ) -> oneshot::Receiver<usize>
    where
        T: Clone + Into<PromptButton>,
    {
        let prompt_builder = cx.prompt_builder.take();
        let Some(prompt_builder) = prompt_builder else {
            unreachable!("Re-entrant window prompting is not supported by GPUI");
        };

        let answers = answers
            .iter()
            .map(|answer| answer.clone().into())
            .collect::<Vec<_>>();

        let receiver = match &prompt_builder {
            PromptBuilder::Default => self
                .platform_window
                .prompt(level, message, detail, &answers)
                .unwrap_or_else(|| {
                    self.build_custom_prompt(&prompt_builder, level, message, detail, &answers, cx)
                }),
            PromptBuilder::Custom(_) => {
                self.build_custom_prompt(&prompt_builder, level, message, detail, &answers, cx)
            }
        };

        cx.prompt_builder = Some(prompt_builder);

        receiver
    }

    fn build_custom_prompt(
        &mut self,
        prompt_builder: &PromptBuilder,
        level: PromptLevel,
        message: &str,
        detail: Option<&str>,
        answers: &[PromptButton],
        cx: &mut App,
    ) -> oneshot::Receiver<usize> {
        let (sender, receiver) = oneshot::channel();
        let handle = PromptHandle::new(sender);
        let handle = (prompt_builder)(level, message, detail, answers, handle, self, cx);
        self.prompt = Some(handle);
        receiver
    }

    /// Returns the current context stack.
    pub fn context_stack(&self) -> Vec<KeyContext> {
        let node_id = self.focus_node_id_in_rendered_frame(self.focus);
        self.fibers_ref().context_stack_for_node(node_id)
    }

    /// Returns all available actions for the focused element.
    pub fn available_actions(&self, cx: &App) -> Vec<Box<dyn Action>> {
        let node_id = self.focus_node_id_in_rendered_frame(self.focus);
        let mut actions = Vec::<Box<dyn Action>>::new();
        let mut current = Some(node_id);
        while let Some(fiber_id) = current {
            if let Some(effects) = self.get_fiber_effects(&fiber_id) {
                for (action_type, _) in &effects.action_listeners {
                    if let Err(ix) =
                        actions.binary_search_by_key(action_type, |a| a.as_any().type_id())
                    {
                        // Intentionally silence these errors without logging.
                        // If an action cannot be built by default, it's not available.
                        let action = cx.actions.build_action_type(action_type).ok();
                        if let Some(action) = action {
                            actions.insert(ix, action);
                        }
                    }
                }
            }
            current = self.fibers_ref().parent_for(&fiber_id);
        }
        for action_type in cx.global_action_listeners.keys() {
            if let Err(ix) = actions.binary_search_by_key(action_type, |a| a.as_any().type_id()) {
                let action = cx.actions.build_action_type(action_type).ok();
                if let Some(action) = action {
                    actions.insert(ix, action);
                }
            }
        }
        actions
    }

    /// Returns key bindings that invoke an action on the currently focused element. Bindings are
    /// returned in the order they were added. For display, the last binding should take precedence.
    pub fn bindings_for_action(&self, action: &dyn Action) -> Vec<KeyBinding> {
        self.key_dispatch
            .bindings_for_action(action, &self.context_stack())
    }

    /// Returns the highest precedence key binding that invokes an action on the currently focused
    /// element. This is more efficient than getting the last result of `bindings_for_action`.
    pub fn highest_precedence_binding_for_action(&self, action: &dyn Action) -> Option<KeyBinding> {
        self.key_dispatch
            .highest_precedence_binding_for_action(action, &self.context_stack())
    }

    /// Returns the key bindings for an action in a context.
    pub fn bindings_for_action_in_context(
        &self,
        action: &dyn Action,
        context: KeyContext,
    ) -> Vec<KeyBinding> {
        self.key_dispatch.bindings_for_action(action, &[context])
    }

    /// Returns the highest precedence key binding for an action in a context. This is more
    /// efficient than getting the last result of `bindings_for_action_in_context`.
    pub fn highest_precedence_binding_for_action_in_context(
        &self,
        action: &dyn Action,
        context: KeyContext,
    ) -> Option<KeyBinding> {
        self.key_dispatch
            .highest_precedence_binding_for_action(action, &[context])
    }

    /// Returns any bindings that would invoke an action on the given focus handle if it were
    /// focused. Bindings are returned in the order they were added. For display, the last binding
    /// should take precedence.
    pub fn bindings_for_action_in(
        &self,
        action: &dyn Action,
        focus_handle: &FocusHandle,
    ) -> Vec<KeyBinding> {
        let Some(context_stack) = self.context_stack_for_focus_handle(focus_handle) else {
            return vec![];
        };
        self.key_dispatch
            .bindings_for_action(action, &context_stack)
    }

    /// Returns the highest precedence key binding that would invoke an action on the given focus
    /// handle if it were focused. This is more efficient than getting the last result of
    /// `bindings_for_action_in`.
    pub fn highest_precedence_binding_for_action_in(
        &self,
        action: &dyn Action,
        focus_handle: &FocusHandle,
    ) -> Option<KeyBinding> {
        let context_stack = self.context_stack_for_focus_handle(focus_handle)?;
        self.key_dispatch
            .highest_precedence_binding_for_action(action, &context_stack)
    }

    /// Find the bindings that can follow the current input sequence for the current context stack.
    pub fn possible_bindings_for_input(&self, input: &[Keystroke]) -> Vec<KeyBinding> {
        self.key_dispatch
            .possible_next_bindings_for_input(input, &self.context_stack())
    }

    fn context_stack_for_focus_handle(
        &self,
        focus_handle: &FocusHandle,
    ) -> Option<Vec<KeyContext>> {
        self.fibers_ref()
            .context_stack_for_focus_handle(focus_handle)
    }

    /// Returns a generic event listener that invokes the given listener with the view and context associated with the given view handle.
    pub fn listener_for<T: 'static, E>(
        &self,
        view: &Entity<T>,
        f: impl Fn(&mut T, &E, &mut Window, &mut Context<T>) + 'static,
    ) -> impl Fn(&E, &mut Window, &mut App) + 'static {
        let view = view.downgrade();
        move |e: &E, window: &mut Window, cx: &mut App| {
            view.update(cx, |view, cx| f(view, e, window, cx)).ok();
        }
    }

    /// Returns a generic handler that invokes the given handler with the view and context associated with the given view handle.
    pub fn handler_for<E: 'static, Callback: Fn(&mut E, &mut Window, &mut Context<E>) + 'static>(
        &self,
        entity: &Entity<E>,
        f: Callback,
    ) -> impl Fn(&mut Window, &mut App) + 'static {
        let entity = entity.downgrade();
        move |window: &mut Window, cx: &mut App| {
            entity.update(cx, |entity, cx| f(entity, window, cx)).ok();
        }
    }

    /// Register a callback that can interrupt the closing of the current window based the returned boolean.
    /// If the callback returns false, the window won't be closed.
    pub fn on_window_should_close(
        &self,
        cx: &App,
        f: impl Fn(&mut Window, &mut App) -> bool + 'static,
    ) {
        let mut cx = self.to_async(cx);
        self.platform_window.on_should_close(Box::new(move || {
            cx.update(|window, cx| f(window, cx)).unwrap_or(true)
        }))
    }

    /// Register an action listener on this node for the next frame. The type of action
    /// is determined by the first parameter of the given listener. When the next frame is rendered
    /// the listener will be cleared.
    ///
    /// This is a fairly low-level method, so prefer using action handlers on elements unless you have
    /// a specific need to register a listener yourself.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn on_action(
        &mut self,
        action_type: TypeId,
        listener: impl Fn(&dyn Any, DispatchPhase, &mut Window, &mut App) + 'static,
    ) {
        self.invalidator.debug_assert_paint();
        self.with_fiber_cx(|fiber| fiber.on_action(action_type, listener));
    }

    /// Register a capturing action listener on this node for the next frame if the condition is true.
    /// The type of action is determined by the first parameter of the given listener. When the next
    /// frame is rendered the listener will be cleared.
    ///
    /// This is a fairly low-level method, so prefer using action handlers on elements unless you have
    /// a specific need to register a listener yourself.
    ///
    /// This method should only be called as part of the paint phase of element drawing.
    pub fn on_action_when(
        &mut self,
        condition: bool,
        action_type: TypeId,
        listener: impl Fn(&dyn Any, DispatchPhase, &mut Window, &mut App) + 'static,
    ) {
        self.invalidator.debug_assert_paint();

        if condition {
            self.on_action(action_type, listener);
        }
    }

    /// Read information about the GPU backing this window.
    /// Currently returns None on Mac and Windows.
    pub fn gpu_specs(&self) -> Option<GpuSpecs> {
        self.platform_window.gpu_specs()
    }

    /// Perform titlebar double-click action.
    /// This is macOS specific.
    pub fn titlebar_double_click(&self) {
        self.platform_window.titlebar_double_click();
    }

    /// Gets the window's title at the platform level.
    /// This is macOS specific.
    pub fn window_title(&self) -> String {
        self.platform_window.get_title()
    }

    /// Returns a list of all tabbed windows and their titles.
    /// This is macOS specific.
    pub fn tabbed_windows(&self) -> Option<Vec<SystemWindowTab>> {
        self.platform_window.tabbed_windows()
    }

    /// Returns the tab bar visibility.
    /// This is macOS specific.
    pub fn tab_bar_visible(&self) -> bool {
        self.platform_window.tab_bar_visible()
    }

    /// Merges all open windows into a single tabbed window.
    /// This is macOS specific.
    pub fn merge_all_windows(&self) {
        self.platform_window.merge_all_windows()
    }

    /// Moves the tab to a new containing window.
    /// This is macOS specific.
    pub fn move_tab_to_new_window(&self) {
        self.platform_window.move_tab_to_new_window()
    }

    /// Shows or hides the window tab overview.
    /// This is macOS specific.
    pub fn toggle_window_tab_overview(&self) {
        self.platform_window.toggle_window_tab_overview()
    }

    /// Sets the tabbing identifier for the window.
    /// This is macOS specific.
    pub fn set_tabbing_identifier(&self, tabbing_identifier: Option<String>) {
        self.platform_window
            .set_tabbing_identifier(tabbing_identifier)
    }

    /// Toggles the inspector mode on this window.
    #[cfg(any(feature = "inspector", debug_assertions))]
    pub fn toggle_inspector(&mut self, cx: &mut App) {
        self.inspector = match self.inspector {
            None => Some(cx.new(|_| Inspector::new())),
            Some(_) => None,
        };
        self.refresh();
    }

    /// Returns true if the window is in inspector mode.
    pub fn is_inspector_picking(&self, _cx: &App) -> bool {
        #[cfg(any(feature = "inspector", debug_assertions))]
        {
            if let Some(inspector) = &self.inspector {
                return inspector.read(_cx).is_picking();
            }
        }
        false
    }

    /// Executes the provided function with mutable access to an inspector state.
    #[cfg(any(feature = "inspector", debug_assertions))]
    pub fn with_inspector_state<T: 'static, R>(
        &mut self,
        _inspector_id: Option<&crate::InspectorElementId>,
        cx: &mut App,
        f: impl FnOnce(&mut Option<T>, &mut Self) -> R,
    ) -> R {
        if let Some(inspector_id) = _inspector_id
            && let Some(inspector) = &self.inspector
        {
            let inspector = inspector.clone();
            let active_element_id = inspector.read(cx).active_element_id();
            if Some(inspector_id) == active_element_id {
                return inspector.update(cx, |inspector, _cx| {
                    inspector.with_active_element_state(self, f)
                });
            }
        }
        f(&mut None, self)
    }

    #[cfg(any(feature = "inspector", debug_assertions))]
    pub(crate) fn build_inspector_element_id(
        &mut self,
        path: crate::InspectorElementPath,
    ) -> crate::InspectorElementId {
        self.invalidator.debug_assert_layout_or_prepaint();
        let path = Rc::new(path);
        let next_instance_id = self
            .next_frame
            .next_inspector_instance_ids
            .entry(path.clone())
            .or_insert(0);
        let instance_id = *next_instance_id;
        *next_instance_id += 1;
        crate::InspectorElementId { path, instance_id }
    }

    /// Registers a hitbox that can be used for inspector picking mode, allowing users to select and
    /// inspect UI elements by clicking on them.
    #[cfg(any(feature = "inspector", debug_assertions))]
    pub fn insert_inspector_hitbox(
        &mut self,
        hitbox_id: HitboxId,
        inspector_id: Option<&crate::InspectorElementId>,
        cx: &App,
    ) {
        self.invalidator.debug_assert_prepaint_or_paint();
        if !self.is_inspector_picking(cx) {
            return;
        }
        if let Some(inspector_id) = inspector_id {
            self.next_frame
                .inspector_hitboxes
                .insert(hitbox_id, inspector_id.clone());
        }
    }

    #[cfg(any(feature = "inspector", debug_assertions))]
    fn paint_inspector_hitbox(&mut self, cx: &App) {
        if let Some(inspector) = self.inspector.as_ref() {
            let inspector = inspector.read(cx);
            if let Some((hitbox_id, _)) = self.hovered_inspector_hitbox(inspector, &self.next_frame)
                && let Some(hitbox) = self.resolve_hitbox(&hitbox_id)
            {
                self.paint_quad(crate::fill(hitbox.bounds, crate::rgba(0x61afef4d)));
            }
        }
    }

    #[cfg(any(feature = "inspector", debug_assertions))]
    pub(crate) fn handle_inspector_mouse_event(&mut self, event: &dyn Any, cx: &mut App) {
        let Some(inspector) = self.inspector.clone() else {
            return;
        };
        if event.downcast_ref::<MouseMoveEvent>().is_some() {
            inspector.update(cx, |inspector, _cx| {
                if let Some((_, inspector_id)) =
                    self.hovered_inspector_hitbox(inspector, &self.rendered_frame)
                {
                    inspector.hover(inspector_id, self);
                }
            });
        } else if event.downcast_ref::<crate::MouseDownEvent>().is_some() {
            inspector.update(cx, |inspector, _cx| {
                if let Some((_, inspector_id)) =
                    self.hovered_inspector_hitbox(inspector, &self.rendered_frame)
                {
                    inspector.select(inspector_id, self);
                }
            });
        } else if let Some(event) = event.downcast_ref::<crate::ScrollWheelEvent>() {
            // This should be kept in sync with SCROLL_LINES in x11 platform.
            const SCROLL_LINES: f32 = 3.0;
            const SCROLL_PIXELS_PER_LAYER: f32 = 36.0;
            let delta_y = event
                .delta
                .pixel_delta(px(SCROLL_PIXELS_PER_LAYER / SCROLL_LINES))
                .y;
            if let Some(inspector) = self.inspector.clone() {
                inspector.update(cx, |inspector, _cx| {
                    if let Some(depth) = inspector.pick_depth.as_mut() {
                        *depth += f32::from(delta_y) / SCROLL_PIXELS_PER_LAYER;
                        let max_depth = self.mouse_hit_test.ids.len() as f32 - 0.5;
                        if *depth < 0.0 {
                            *depth = 0.0;
                        } else if *depth > max_depth {
                            *depth = max_depth;
                        }
                        if let Some((_, inspector_id)) =
                            self.hovered_inspector_hitbox(inspector, &self.rendered_frame)
                        {
                            inspector.set_active_element_id(inspector_id, self);
                        }
                    }
                });
            }
        }
    }

    #[cfg(any(feature = "inspector", debug_assertions))]
    fn hovered_inspector_hitbox(
        &self,
        inspector: &Inspector,
        frame: &Frame,
    ) -> Option<(HitboxId, crate::InspectorElementId)> {
        if let Some(pick_depth) = inspector.pick_depth {
            let depth = (pick_depth as i64).try_into().unwrap_or(0);
            let max_skipped = self.mouse_hit_test.ids.len().saturating_sub(1);
            let skip_count = (depth as usize).min(max_skipped);
            for hitbox_id in self.mouse_hit_test.ids.iter().skip(skip_count) {
                if let Some(inspector_id) = frame.inspector_hitboxes.get(hitbox_id) {
                    return Some((*hitbox_id, inspector_id.clone()));
                }
            }
        }
        None
    }

    /// For testing: set the current modifier keys state.
    /// This does not generate any events.
    #[cfg(any(test, feature = "test-support"))]
    pub fn set_modifiers(&mut self, modifiers: Modifiers) {
        self.modifiers = modifiers;
    }

    /// For testing: simulate a mouse move event to the given position.
    /// This dispatches the event through the normal event handling path,
    /// which will trigger hover states and tooltips.
    #[cfg(any(test, feature = "test-support"))]
    pub fn simulate_mouse_move(&mut self, position: Point<Pixels>, cx: &mut App) {
        let event = PlatformInput::MouseMove(MouseMoveEvent {
            position,
            modifiers: self.modifiers,
            pressed_button: None,
        });
        let _ = self.dispatch_event(event, cx);
    }
}

// #[derive(Clone, Copy, Eq, PartialEq, Hash)]
slotmap::new_key_type! {
    /// A unique identifier for a window.
    pub struct WindowId;
}

impl WindowId {
    /// Converts this window ID to a `u64`.
    pub fn as_u64(&self) -> u64 {
        self.0.as_ffi()
    }
}

impl From<u64> for WindowId {
    fn from(value: u64) -> Self {
        WindowId(slotmap::KeyData::from_ffi(value))
    }
}

/// A handle to a window with a specific root view type.
/// Note that this does not keep the window alive on its own.
#[derive(Deref, DerefMut)]
pub struct WindowHandle<V> {
    #[deref]
    #[deref_mut]
    pub(crate) any_handle: AnyWindowHandle,
    state_type: PhantomData<fn(V) -> V>,
}

impl<V> Debug for WindowHandle<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WindowHandle")
            .field("any_handle", &self.any_handle.id.as_u64())
            .finish()
    }
}

impl<V: 'static + Render> WindowHandle<V> {
    /// Creates a new handle from a window ID.
    /// This does not check if the root type of the window is `V`.
    pub fn new(id: WindowId) -> Self {
        WindowHandle {
            any_handle: AnyWindowHandle {
                id,
                state_type: TypeId::of::<V>(),
            },
            state_type: PhantomData,
        }
    }

    /// Get the root view out of this window.
    ///
    /// This will fail if the window is closed or if the root view's type does not match `V`.
    #[cfg(any(test, feature = "test-support"))]
    pub fn root<C>(&self, cx: &mut C) -> Result<Entity<V>>
    where
        C: AppContext,
    {
        cx.update_window(self.any_handle, |root_view, _, _| {
            root_view
                .downcast::<V>()
                .map_err(|_| anyhow!("the type of the window's root view has changed"))
        })?
    }

    /// Updates the root view of this window.
    ///
    /// This will fail if the window has been closed or if the root view's type does not match
    pub fn update<C, R>(
        &self,
        cx: &mut C,
        update: impl FnOnce(&mut V, &mut Window, &mut Context<V>) -> R,
    ) -> Result<R>
    where
        C: AppContext,
    {
        cx.update_window(self.any_handle, |root_view, window, cx| {
            let view = root_view
                .downcast::<V>()
                .map_err(|_| anyhow!("the type of the window's root view has changed"))?;

            Ok(view.update(cx, |view, cx| update(view, window, cx)))
        })?
    }

    /// Read the root view out of this window.
    ///
    /// This will fail if the window is closed or if the root view's type does not match `V`.
    pub fn read<'a>(&self, cx: &'a App) -> Result<&'a V> {
        let x = cx
            .windows
            .get(self.id)
            .and_then(|window| {
                window
                    .as_deref()
                    .and_then(|window| window.root.clone())
                    .map(|root_view| root_view.downcast::<V>())
            })
            .context("window not found")?
            .map_err(|_| anyhow!("the type of the window's root view has changed"))?;

        Ok(x.read(cx))
    }

    /// Read the root view out of this window, with a callback
    ///
    /// This will fail if the window is closed or if the root view's type does not match `V`.
    pub fn read_with<C, R>(&self, cx: &C, read_with: impl FnOnce(&V, &App) -> R) -> Result<R>
    where
        C: AppContext,
    {
        cx.read_window(self, |root_view, cx| read_with(root_view.read(cx), cx))
    }

    /// Read the root view pointer off of this window.
    ///
    /// This will fail if the window is closed or if the root view's type does not match `V`.
    pub fn entity<C>(&self, cx: &C) -> Result<Entity<V>>
    where
        C: AppContext,
    {
        cx.read_window(self, |root_view, _cx| root_view)
    }

    /// Check if this window is 'active'.
    ///
    /// Will return `None` if the window is closed or currently
    /// borrowed.
    pub fn is_active(&self, cx: &mut App) -> Option<bool> {
        cx.update_window(self.any_handle, |_, window, _| window.is_window_active())
            .ok()
    }
}

impl<V> Copy for WindowHandle<V> {}

impl<V> Clone for WindowHandle<V> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<V> PartialEq for WindowHandle<V> {
    fn eq(&self, other: &Self) -> bool {
        self.any_handle == other.any_handle
    }
}

impl<V> Eq for WindowHandle<V> {}

impl<V> Hash for WindowHandle<V> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.any_handle.hash(state);
    }
}

impl<V: 'static> From<WindowHandle<V>> for AnyWindowHandle {
    fn from(val: WindowHandle<V>) -> Self {
        val.any_handle
    }
}

/// A handle to a window with any root view type, which can be downcast to a window with a specific root view type.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct AnyWindowHandle {
    pub(crate) id: WindowId,
    state_type: TypeId,
}

impl AnyWindowHandle {
    /// Get the ID of this window.
    pub fn window_id(&self) -> WindowId {
        self.id
    }

    /// Attempt to convert this handle to a window handle with a specific root view type.
    /// If the types do not match, this will return `None`.
    pub fn downcast<T: 'static>(&self) -> Option<WindowHandle<T>> {
        if TypeId::of::<T>() == self.state_type {
            Some(WindowHandle {
                any_handle: *self,
                state_type: PhantomData,
            })
        } else {
            None
        }
    }

    /// Updates the state of the root view of this window.
    ///
    /// This will fail if the window has been closed.
    pub fn update<C, R>(
        self,
        cx: &mut C,
        update: impl FnOnce(AnyView, &mut Window, &mut App) -> R,
    ) -> Result<R>
    where
        C: AppContext,
    {
        cx.update_window(self, update)
    }

    /// Read the state of the root view of this window.
    ///
    /// This will fail if the window has been closed.
    pub fn read<T, C, R>(self, cx: &C, read: impl FnOnce(Entity<T>, &App) -> R) -> Result<R>
    where
        C: AppContext,
        T: 'static,
    {
        let view = self
            .downcast::<T>()
            .context("the type of the window's root view has changed")?;

        cx.read_window(&view, read)
    }
}

impl HasWindowHandle for Window {
    fn window_handle(&self) -> Result<raw_window_handle::WindowHandle<'_>, HandleError> {
        self.platform_window.window_handle()
    }
}

impl HasDisplayHandle for Window {
    fn display_handle(
        &self,
    ) -> std::result::Result<raw_window_handle::DisplayHandle<'_>, HandleError> {
        self.platform_window.display_handle()
    }
}

/// An identifier for an [`Element`].
///
/// Can be constructed with a string, a number, or both, as well
/// as other internal representations.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ElementId {
    /// The ID of a View element
    View(EntityId),
    /// An integer ID.
    Integer(u64),
    /// A string based ID.
    Name(SharedString),
    /// A UUID.
    Uuid(Uuid),
    /// An ID that's equated with a focus handle.
    FocusHandle(FocusId),
    /// A combination of a name and an integer.
    NamedInteger(SharedString, u64),
    /// A path.
    Path(Arc<std::path::Path>),
    /// A code location.
    CodeLocation(core::panic::Location<'static>),
    /// A labeled child of an element.
    NamedChild(Arc<ElementId>, SharedString),
}

impl ElementId {
    /// Constructs an `ElementId::NamedInteger` from a name and `usize`.
    pub fn named_usize(name: impl Into<SharedString>, integer: usize) -> ElementId {
        Self::NamedInteger(name.into(), integer as u64)
    }
}

impl Display for ElementId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ElementId::View(entity_id) => write!(f, "view-{}", entity_id)?,
            ElementId::Integer(ix) => write!(f, "{}", ix)?,
            ElementId::Name(name) => write!(f, "{}", name)?,
            ElementId::FocusHandle(_) => write!(f, "FocusHandle")?,
            ElementId::NamedInteger(s, i) => write!(f, "{}-{}", s, i)?,
            ElementId::Uuid(uuid) => write!(f, "{}", uuid)?,
            ElementId::Path(path) => write!(f, "{}", path.display())?,
            ElementId::CodeLocation(location) => write!(f, "{}", location)?,
            ElementId::NamedChild(id, name) => write!(f, "{}-{}", id, name)?,
        }

        Ok(())
    }
}

impl TryInto<SharedString> for ElementId {
    type Error = anyhow::Error;

    fn try_into(self) -> anyhow::Result<SharedString> {
        if let ElementId::Name(name) = self {
            Ok(name)
        } else {
            anyhow::bail!("element id is not string")
        }
    }
}

impl From<usize> for ElementId {
    fn from(id: usize) -> Self {
        ElementId::Integer(id as u64)
    }
}

impl From<i32> for ElementId {
    fn from(id: i32) -> Self {
        Self::Integer(id as u64)
    }
}

impl From<SharedString> for ElementId {
    fn from(name: SharedString) -> Self {
        ElementId::Name(name)
    }
}

impl From<String> for ElementId {
    fn from(name: String) -> Self {
        ElementId::Name(name.into())
    }
}

impl From<Arc<str>> for ElementId {
    fn from(name: Arc<str>) -> Self {
        ElementId::Name(name.into())
    }
}

impl From<Arc<std::path::Path>> for ElementId {
    fn from(path: Arc<std::path::Path>) -> Self {
        ElementId::Path(path)
    }
}

impl From<&'static str> for ElementId {
    fn from(name: &'static str) -> Self {
        ElementId::Name(name.into())
    }
}

impl<'a> From<&'a FocusHandle> for ElementId {
    fn from(handle: &'a FocusHandle) -> Self {
        ElementId::FocusHandle(handle.id)
    }
}

impl From<(&'static str, EntityId)> for ElementId {
    fn from((name, id): (&'static str, EntityId)) -> Self {
        ElementId::NamedInteger(name.into(), id.as_u64())
    }
}

impl From<(&'static str, usize)> for ElementId {
    fn from((name, id): (&'static str, usize)) -> Self {
        ElementId::NamedInteger(name.into(), id as u64)
    }
}

impl From<(SharedString, usize)> for ElementId {
    fn from((name, id): (SharedString, usize)) -> Self {
        ElementId::NamedInteger(name, id as u64)
    }
}

impl From<(&'static str, u64)> for ElementId {
    fn from((name, id): (&'static str, u64)) -> Self {
        ElementId::NamedInteger(name.into(), id)
    }
}

impl From<Uuid> for ElementId {
    fn from(value: Uuid) -> Self {
        Self::Uuid(value)
    }
}

impl From<(&'static str, u32)> for ElementId {
    fn from((name, id): (&'static str, u32)) -> Self {
        ElementId::NamedInteger(name.into(), id.into())
    }
}

impl<T: Into<SharedString>> From<(ElementId, T)> for ElementId {
    fn from((id, name): (ElementId, T)) -> Self {
        ElementId::NamedChild(Arc::new(id), name.into())
    }
}

impl From<&'static core::panic::Location<'static>> for ElementId {
    fn from(location: &'static core::panic::Location<'static>) -> Self {
        ElementId::CodeLocation(*location)
    }
}

/// A rectangle to be rendered in the window at the given position and size.
/// Passed as an argument [`Window::paint_quad`].
#[derive(Clone)]
pub struct PaintQuad {
    /// The bounds of the quad within the window.
    pub bounds: Bounds<Pixels>,
    /// The radii of the quad's corners.
    pub corner_radii: Corners<Pixels>,
    /// The background color of the quad.
    pub background: Background,
    /// The widths of the quad's borders.
    pub border_widths: Edges<Pixels>,
    /// The color of the quad's borders.
    pub border_color: Hsla,
    /// The style of the quad's borders.
    pub border_style: BorderStyle,
}

impl PaintQuad {
    /// Sets the corner radii of the quad.
    pub fn corner_radii(self, corner_radii: impl Into<Corners<Pixels>>) -> Self {
        PaintQuad {
            corner_radii: corner_radii.into(),
            ..self
        }
    }

    /// Sets the border widths of the quad.
    pub fn border_widths(self, border_widths: impl Into<Edges<Pixels>>) -> Self {
        PaintQuad {
            border_widths: border_widths.into(),
            ..self
        }
    }

    /// Sets the border color of the quad.
    pub fn border_color(self, border_color: impl Into<Hsla>) -> Self {
        PaintQuad {
            border_color: border_color.into(),
            ..self
        }
    }

    /// Sets the background color of the quad.
    pub fn background(self, background: impl Into<Background>) -> Self {
        PaintQuad {
            background: background.into(),
            ..self
        }
    }
}

/// Creates a quad with the given parameters.
pub fn quad(
    bounds: Bounds<Pixels>,
    corner_radii: impl Into<Corners<Pixels>>,
    background: impl Into<Background>,
    border_widths: impl Into<Edges<Pixels>>,
    border_color: impl Into<Hsla>,
    border_style: BorderStyle,
) -> PaintQuad {
    PaintQuad {
        bounds,
        corner_radii: corner_radii.into(),
        background: background.into(),
        border_widths: border_widths.into(),
        border_color: border_color.into(),
        border_style,
    }
}

/// Creates a filled quad with the given bounds and background color.
pub fn fill(bounds: impl Into<Bounds<Pixels>>, background: impl Into<Background>) -> PaintQuad {
    PaintQuad {
        bounds: bounds.into(),
        corner_radii: (0.).into(),
        background: background.into(),
        border_widths: (0.).into(),
        border_color: transparent_black(),
        border_style: BorderStyle::default(),
    }
}

/// Creates a rectangle outline with the given bounds, border color, and a 1px border width
pub fn outline(
    bounds: impl Into<Bounds<Pixels>>,
    border_color: impl Into<Hsla>,
    border_style: BorderStyle,
) -> PaintQuad {
    PaintQuad {
        bounds: bounds.into(),
        corner_radii: (0.).into(),
        background: transparent_black().into(),
        border_widths: (1.).into(),
        border_color: border_color.into(),
        border_style,
    }
}

#[cfg(test)]
mod tests;
