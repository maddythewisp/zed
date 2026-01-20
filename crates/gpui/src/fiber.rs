use crate::elements::{
    ActionListener, CanDropPredicate, ClickListener, DragListener, DropListener, HoverListener,
    LayoutListener, ModifiersChangedListener, MouseDownListener, MouseMoveListener,
    MousePressureListener, MouseUpListener, ScrollWheelListener, TooltipBuilder,
};
#[cfg(debug_assertions)]
use crate::window::DrawPhase;
use crate::window::context::{PaintCx, PrepaintCx};
use crate::window::{CursorStyleRequest, DeferredDraw, HitTest, TooltipRequest};
use crate::{
    Action, AnchoredFitMode, AnchoredPositionMode, AnyDrag, AnyElement, AnyTooltip, AnyView, App,
    AvailableSpace, Bounds, ClickEvent, ContentMask, Corner, CursorStyle, DRAG_THRESHOLD,
    DispatchPhase, ElementClickedState, ElementId, EntityId, FocusHandle, FocusId, GlobalElementId,
    GroupHitboxes, Hitbox, HitboxBehavior, HitboxId, InputHandler, Interactivity, KeyContext,
    LineLayoutIndex, ModifiersChangedEvent, MouseButton, MouseClickEvent,
    MouseDownEvent, MouseMoveEvent, MouseUpEvent, Pixels, PlatformInputHandler, Point,
    ScaledPixels, SceneSegmentId, SharedString, Size, Style, StyleRefinement, TabStopMap,
    TooltipId, TransformId, ViewData, Window, clear_active_tooltip_if_not_hoverable,
    handle_tooltip_mouse_move,
};
use ::taffy::style::{AvailableSpace as TaffyAvailableSpace, Display, Style as TaffyStyle};
use ::taffy::tree::RunMode;
use ::taffy::{
    Cache, CacheTree, Layout, LayoutBlockContainer, LayoutFlexboxContainer, LayoutGridContainer,
    LayoutInput, LayoutOutput, LayoutPartialTree, NodeId, RoundTree, TraversePartialTree,
    TraverseTree, compute_block_layout, compute_cached_layout, compute_flexbox_layout,
    compute_grid_layout, compute_hidden_layout, compute_leaf_layout,
};
use bitflags::bitflags;
use collections::{FxHashMap, FxHashSet, FxHasher};
use slotmap::{DefaultKey, SecondaryMap, SlotMap};
use smallvec::SmallVec;
use std::any::{Any, TypeId};
#[cfg(test)]
use std::cell::Cell;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

#[cfg(test)]
thread_local! {
    static DUPLICATE_FIBER_ID_PUSH_COUNT: Cell<usize> = const { Cell::new(0) };
}

pub(crate) struct StateStore {
    cells: RefCell<FxHashMap<TypeId, Rc<RefCell<Box<dyn Any + 'static>>>>>,
}

impl StateStore {
    pub(crate) fn new() -> Self {
        Self {
            cells: RefCell::new(FxHashMap::default()),
        }
    }

    pub(crate) fn with_mut<S: 'static, R>(
        &self,
        f: impl FnOnce(Option<S>) -> (R, S),
    ) -> R {
        let type_id = TypeId::of::<S>();

        let (cell, is_new) = {
            let mut cells = self.cells.borrow_mut();
            let is_new = !cells.contains_key(&type_id);
            let cell = cells
                .entry(type_id)
                .or_insert_with(|| Rc::new(RefCell::new(Box::new(None::<S>) as Box<dyn Any>)))
                .clone();
            (cell, is_new)
        };

        let existing = {
            let mut guard = cell.borrow_mut();
            let state = guard
                .downcast_mut::<Option<S>>()
                .expect("invalid element state type");
            let value = state.take();
            if !is_new && value.is_none() {
                panic!(
                    "re-entrant with_element_state for type {:?}. Nested calls with the same state type are not allowed.",
                    std::any::type_name::<S>()
                );
            }
            value
        };

        let (result, state) = f(existing);

        {
            let mut guard = cell.borrow_mut();
            let stored = guard
                .downcast_mut::<Option<S>>()
                .expect("invalid element state type");
            *stored = Some(state);
        }

        result
    }
}

#[cfg(test)]
pub(crate) fn take_duplicate_fiber_id_push_count() -> usize {
    DUPLICATE_FIBER_ID_PUSH_COUNT.with(|count| {
        let value = count.get();
        count.set(0);
        value
    })
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct FiberSceneSegments {
    pub before: SceneSegmentId,
    pub after: Option<SceneSegmentId>,
}

/// Key event listener callback type.
pub type KeyListener = Rc<dyn Fn(&dyn Any, DispatchPhase, &mut Window, &mut App) + 'static>;

/// Mouse event listener callback type (window-level style).
pub type AnyMouseListener =
    Rc<RefCell<dyn FnMut(&dyn Any, DispatchPhase, &mut Window, &mut App) + 'static>>;

/// All event handlers for a single fiber.
#[allow(missing_docs)]
#[derive(Default)]
pub struct FiberEffects {
    pub click_listeners: Vec<ClickListener>,
    pub any_mouse_listeners: Vec<AnyMouseListener>,
    pub mouse_down_listeners: Vec<MouseDownListener>,
    pub mouse_up_listeners: Vec<MouseUpListener>,
    pub mouse_move_listeners: Vec<MouseMoveListener>,
    pub mouse_pressure_listeners: Vec<MousePressureListener>,
    pub scroll_wheel_listeners: Vec<ScrollWheelListener>,
    pub key_listeners: SmallVec<[KeyListener; 2]>,
    pub modifiers_changed_listeners: Vec<ModifiersChangedListener>,
    pub action_listeners: Vec<(TypeId, ActionListener)>,
    pub drag_listener: Option<(Arc<dyn Any>, DragListener)>,
    pub drop_listeners: Vec<(TypeId, DropListener)>,
    pub can_drop_predicate: Option<CanDropPredicate>,
    pub hover_listener: Option<HoverListener>,
    pub(crate) tooltip: Option<TooltipBuilder>,
    pub cursor_style: Option<CursorStyle>,
}

impl FiberEffects {
    /// Creates an empty FiberEffects with no handlers.
    pub fn new() -> Self {
        Self::default()
    }
}

/// A function that measures the size of a leaf node during layout.
/// Called with known dimensions (if any), available space, and the window/app context.
pub type MeasureFunc = Box<
    dyn FnMut(Size<Option<Pixels>>, Size<AvailableSpace>, &mut Window, &mut App) -> Size<Pixels>,
>;

/// Layout configuration for anchored fibers.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct AnchoredConfig {
    pub anchor_corner: Corner,
    pub fit_mode: AnchoredFitMode,
    pub anchor_position: Option<Point<Pixels>>,
    pub position_mode: AnchoredPositionMode,
    pub offset: Option<Point<Pixels>>,
}

bitflags! {
    /// Dirty flags for incremental updates.
    ///
    /// The key insight is separating SIZE changes from POSITION/PAINT changes:
    /// - Size changes propagate upward (parent's size may depend on child's)
    /// - Position changes propagate downward (children move with parent)
    /// - Paint changes don't propagate (only this element needs repaint)
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
    pub struct DirtyFlags: u16 {
        // === Intrinsic sizing flags ===

        /// This element's content changed - intrinsic size needs recomputation.
        /// Set when: text changes, image loads, children added/removed.
        const CONTENT_CHANGED = 1 << 0;

        /// This element's sizing-relevant styles changed.
        /// Set when: font-size, padding, border, min/max size change.
        const SIZING_STYLE_CHANGED = 1 << 1;

        /// A descendant has CONTENT_CHANGED or SIZING_STYLE_CHANGED.
        /// Used for efficient tree traversal during sizing pass.
        const HAS_DIRTY_SIZING_DESCENDANT = 1 << 2;

        // === Layout flags ===

        /// This element's computed intrinsic size changed.
        /// Set after sizing pass determines the size actually differs.
        const INTRINSIC_SIZE_CHANGED = 1 << 3;

        /// A descendant has INTRINSIC_SIZE_CHANGED.
        const HAS_SIZE_CHANGED_DESCENDANT = 1 << 4;

        /// Position-only styles changed (margin, align-self, etc.).
        /// These don't affect intrinsic size, only layout position.
        const POSITION_STYLE_CHANGED = 1 << 5;

        /// A descendant has layout work (NEEDS_LAYOUT).
        ///
        /// This enables fast root-level gating without scanning the entire tree.
        const HAS_LAYOUT_DIRTY_DESCENDANT = 1 << 11;

        // === Paint flags ===

        /// Paint-only styles changed (background, border-color, shadow, etc.).
        const PAINT_STYLE_CHANGED = 1 << 6;

        /// This element needs repaint.
        const NEEDS_PAINT = 1 << 7;

        /// A descendant needs repaint.
        const HAS_PAINT_DIRTY_DESCENDANT = 1 << 8;

        // === Structural flags ===

        /// Element identity changed (different element type at this position).
        const STRUCTURE_CHANGED = 1 << 9;

        // === Runtime flags ===

        /// Node's visual transform changed (e.g. scroll offset).
        ///
        /// Transform changes do not require prepaint and should not invalidate sizing/layout
        /// caches. They only affect painting (and cached scene translation).
        const TRANSFORM_CHANGED = 1 << 10;

        // === Computed flags ===

        /// Needs intrinsic size computation (CONTENT_CHANGED | SIZING_STYLE_CHANGED).
        const NEEDS_SIZING =
            Self::CONTENT_CHANGED.bits() | Self::SIZING_STYLE_CHANGED.bits();

        /// Needs layout (INTRINSIC_SIZE_CHANGED | SIZING_STYLE_CHANGED | POSITION_STYLE_CHANGED | STRUCTURE_CHANGED).
        const NEEDS_LAYOUT = Self::INTRINSIC_SIZE_CHANGED.bits()
            | Self::SIZING_STYLE_CHANGED.bits()
            | Self::POSITION_STYLE_CHANGED.bits()
            | Self::STRUCTURE_CHANGED.bits();

        /// Flags indicating this fiber itself needs work (excludes descendant-only flags).
        const WORK_FLAGS = Self::NEEDS_SIZING.bits()
            | Self::NEEDS_LAYOUT.bits()
            | Self::PAINT_STYLE_CHANGED.bits()
            | Self::NEEDS_PAINT.bits()
            | Self::TRANSFORM_CHANGED.bits();

        /// Flags that require prepaint.
        ///
        /// Prepaint must run when bounds-dependent state could change, which includes layout work
        /// and paint-style updates. Transform-only changes are excluded.
        const PREPAINT_FLAGS = Self::NEEDS_LAYOUT.bits()
            | Self::PAINT_STYLE_CHANGED.bits()
            | Self::NEEDS_PAINT.bits();

        /// Flags that require paint (includes transform-only changes).
        const PAINT_FLAGS = Self::PREPAINT_FLAGS.bits() | Self::TRANSFORM_CHANGED.bits();
    }
}

impl DirtyFlags {
    /// Alias for empty flags.
    pub const NONE: DirtyFlags = DirtyFlags::empty();

    /// Check if any flags are set.
    pub fn any(self) -> bool {
        !self.is_empty()
    }

    /// Check if this fiber itself needs work (excludes descendant-only flags).
    pub fn needs_work(self) -> bool {
        self.intersects(Self::WORK_FLAGS)
    }

    /// Check if this element needs intrinsic size computation.
    pub fn needs_sizing(self) -> bool {
        self.intersects(Self::NEEDS_SIZING)
    }

    /// Check if this element needs layout.
    pub fn needs_layout(self) -> bool {
        self.intersects(Self::NEEDS_LAYOUT)
    }

    /// Check if this element or descendants need sizing.
    pub fn has_sizing_work(self) -> bool {
        self.intersects(Self::NEEDS_SIZING | Self::HAS_DIRTY_SIZING_DESCENDANT)
    }

    /// Check if this element or descendants need layout.
    pub fn has_layout_work(self) -> bool {
        self.intersects(Self::NEEDS_LAYOUT | Self::HAS_LAYOUT_DIRTY_DESCENDANT)
    }

    /// Check if this fiber needs prepaint.
    pub fn needs_prepaint(self) -> bool {
        self.intersects(Self::PREPAINT_FLAGS)
    }

    /// Check if this fiber needs paint.
    pub fn needs_paint(self) -> bool {
        self.intersects(Self::PAINT_FLAGS)
    }

    /// Check if this subtree is completely clean (no flags at all).
    /// Use this for replay gating - requires ALL flags to be clear.
    /// This is stricter than !needs_work() which ignores descendant-only flags.
    pub fn is_subtree_clean(self) -> bool {
        self.is_empty()
    }

    /// Clear all flags (alias for remove with all bits).
    pub fn clear(&mut self) {
        *self = Self::empty();
    }
}

/// Report returned by `reconcile_frame` indicating what changed during reconciliation.
///
/// This provides visibility into the reconciliation phase for debugging, profiling,
/// and future tooling. The report summarizes structural changes, dirty flags, and
/// work performed during the reconcile pass.
/// A fiber node - the persistent unit of the UI tree.
/// Fibers cache expensive computed results across frames.
pub struct Fiber {
    pub(crate) kind: FiberKind,

    /// The reconciliation key for this fiber.
    /// Used to match elements across frames during reconciliation.
    /// Derived from Element::id().
    pub key: Option<ElementId>,

    /// If Some, this fiber represents a view boundary.
    /// View identity is tracked separately from the reconciliation key
    /// because views use EntityId (stable across renders) rather than
    /// ElementId (potentially recreated each frame).
    pub view_id: Option<EntityId>,

    /// For conditional slot children, the slot position index.
    /// Used for positional matching within node_children.
    pub slot_index: Option<u32>,

    /// Cached child count for structure-change detection.
    pub child_count: usize,

    /// Cached preorder index for ordering stable collections.
    pub(crate) preorder_index: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub(crate) enum FiberKind {
    #[default]
    Normal,
    /// Fiber created only for element state scoping, not layout participation.
    StateOnly,
    /// Intentional detached root (overlay, measurement, drag, etc).
    DetachedRoot,
}

#[derive(Default)]
pub(crate) struct FiberViewState {
    pub view_data: Option<ViewData>,
    pub view_descriptor_hash: u64,
    pub(crate) is_view_wrapper: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct HitboxSubtreeBounds {
    pub(crate) transform_id: TransformId,
    pub(crate) bounds: Bounds<Pixels>,
}

#[derive(Default)]
pub(crate) struct FiberHitboxState {
    pub hitbox: Option<HitboxData>,
    pub hitbox_subtree_bounds: Option<HitboxSubtreeBounds>,
}

#[derive(Default)]
pub(crate) struct FiberPaintCache {
    pub(crate) prepaint_state: Option<PrepaintState>,
    pub(crate) paint_list: Option<PaintList>,
    pub(crate) scene_segment_list: Option<Vec<SceneSegmentId>>,
}

#[derive(Default)]
pub(crate) struct FiberLayoutState {
    pub taffy_cache: collections::FxHashMap<GlobalElementId, Cache>,
    pub taffy_style: TaffyStyle,
    pub unrounded_layout: collections::FxHashMap<GlobalElementId, Layout>,
    pub final_layout: collections::FxHashMap<GlobalElementId, Layout>,
    /// Cached intrinsic size (separate from taffy_cache).
    pub intrinsic_size: Option<crate::CachedIntrinsicSize>,
}

impl FiberLayoutState {
    fn taffy_cache_for_island_mut(&mut self, island_root: GlobalElementId) -> &mut Cache {
        self.taffy_cache
            .entry(island_root)
            .or_insert_with(Cache::new)
    }

    fn taffy_cache_for_island(&self, island_root: GlobalElementId) -> Option<&Cache> {
        self.taffy_cache.get(&island_root)
    }

    fn unrounded_layout_for_island(&self, island_root: GlobalElementId) -> Layout {
        self.unrounded_layout
            .get(&island_root)
            .copied()
            .unwrap_or_else(Layout::new)
    }

    fn set_unrounded_layout_for_island(&mut self, island_root: GlobalElementId, layout: Layout) {
        self.unrounded_layout.insert(island_root, layout);
    }

    fn final_layout_for_island(&self, island_root: GlobalElementId) -> Layout {
        self.final_layout
            .get(&island_root)
            .copied()
            .unwrap_or_else(Layout::new)
    }

    fn set_final_layout_for_island(&mut self, island_root: GlobalElementId, layout: Layout) {
        self.final_layout.insert(island_root, layout);
    }
}

/// Cached measure data for a fiber.
pub struct FiberMeasureData {
    pub measure_func: MeasureFunc,
    pub measure_hash: Option<u64>,
}

impl Fiber {
    /// Create a new fiber with kind and key for keyed reconciliation.
    pub fn new(key: Option<ElementId>, child_count: usize) -> Self {
        Self {
            kind: FiberKind::Normal,
            key,
            view_id: None,
            slot_index: None,
            child_count,
            preorder_index: 0,
        }
    }

    /// Create a new fiber for a view.
    pub fn for_view(view_id: EntityId, child_count: usize) -> Self {
        Self {
            kind: FiberKind::Normal,
            key: None,
            view_id: Some(view_id),
            slot_index: None,
            child_count,
            preorder_index: 0,
        }
    }

    fn state_only() -> Self {
        Self {
            kind: FiberKind::StateOnly,
            key: None,
            view_id: None,
            slot_index: None,
            child_count: 0,
            preorder_index: 0,
        }
    }

    fn detached_root() -> Self {
        Self {
            kind: FiberKind::DetachedRoot,
            key: None,
            view_id: None,
            slot_index: None,
            child_count: 0,
            preorder_index: 0,
        }
    }
}

/// The fiber tree - stores all fibers and manages their relationships
pub struct FiberTree {
    /// Storage for all fibers
    pub fibers: SlotMap<DefaultKey, Fiber>,

    /// Dirty tracking flags for each fiber.
    pub dirty: SecondaryMap<DefaultKey, DirtyFlags>,

    /// Retained render nodes for fibers that support retained rendering.
    pub render_nodes: SecondaryMap<DefaultKey, Box<dyn crate::RenderNode>>,

    /// Children lists stored separately to reduce fiber size.
    pub children: SecondaryMap<DefaultKey, SmallVec<[GlobalElementId; 4]>>,

    /// Children that exist only for identity / scoped state, not layout or paint.
    pub(crate) scope_children: SecondaryMap<DefaultKey, SmallVec<[GlobalElementId; 4]>>,

    /// Node-managed children lists (e.g. conditional slots).
    ///
    /// These children are attached to a parent fiber by a retained render node rather than by the
    /// ephemeral descriptor tree, and must be preserved across descriptor reconciliation.
    pub(crate) node_children: SecondaryMap<DefaultKey, SmallVec<[GlobalElementId; 4]>>,

    /// Parent links stored separately to reduce fiber size.
    pub parents: SecondaryMap<DefaultKey, Option<GlobalElementId>>,

    /// Cached measure functions for fibers.
    pub measure_funcs: SecondaryMap<DefaultKey, FiberMeasureData>,

    /// Cached view/legacy payloads for fibers.
    pub view_state: SecondaryMap<DefaultKey, FiberViewState>,

    /// Cached paint output for fibers.
    pub paint_cache: SecondaryMap<DefaultKey, FiberPaintCache>,

    /// Cached hitbox state for fibers.
    pub hitbox_state: SecondaryMap<DefaultKey, FiberHitboxState>,

    /// Cached layout state for fibers.
    pub layout_state: SecondaryMap<DefaultKey, FiberLayoutState>,

    /// Cached style refinements for style-based diffing.
    pub(crate) cached_styles: SecondaryMap<DefaultKey, StyleRefinement>,

    /// Cached layout bounds for fibers.
    pub bounds: SecondaryMap<DefaultKey, Bounds<Pixels>>,

    /// Cached effects for fibers.
    pub effects: SecondaryMap<DefaultKey, FiberEffects>,

    /// Cached tooltip requests for fibers.
    pub tooltips: SecondaryMap<DefaultKey, SmallVec<[TooltipRequest; 1]>>,

    /// Cached cursor style requests for fibers.
    pub cursor_styles: SecondaryMap<DefaultKey, SmallVec<[CursorStyleRequest; 1]>>,

    /// Cached input handlers for fibers.
    pub input_handlers: SecondaryMap<DefaultKey, Box<dyn InputHandler>>,

    /// Cached deferred draws for fibers.
    pub deferred_draws: SecondaryMap<DefaultKey, SmallVec<[DeferredDraw; 1]>>,

    /// Retained overlay roots for `Window::defer_draw`, grouped by callsite.
    ///
    /// These overlay roots are detached from the main tree (not children of `root`) but
    /// still live in the same `FiberTree` so they can use retained nodes, incremental
    /// layout/prepaint/paint, and participate in hit testing.
    pub deferred_draw_overlay_groups:
        SecondaryMap<DefaultKey, FxHashMap<usize, DeferredDrawOverlayGroup>>,

    /// Cached tab stop ids for fibers.
    pub tab_stops: SecondaryMap<DefaultKey, SmallVec<[FocusId; 1]>>,

    /// Cached scene segments for fibers.
    pub scene_segments: SecondaryMap<DefaultKey, FiberSceneSegments>,

    /// Per-fiber element state storage by type.
    pub(crate) element_states: SecondaryMap<DefaultKey, Rc<StateStore>>,

    /// Cached key contexts for keyboard dispatch.
    pub key_contexts: SecondaryMap<DefaultKey, KeyContext>,

    /// Cached deferred draw priorities for Deferred fibers.
    pub deferred_priorities: SecondaryMap<DefaultKey, usize>,

    /// Cached focus IDs for focusable fibers.
    pub focus_ids: SecondaryMap<DefaultKey, FocusId>,

    /// Per-fiber layout listeners, extracted from Interactivity each reconcile.
    pub(crate) layout_listeners: SecondaryMap<DefaultKey, LayoutListener>,

    /// Per-fiber previous bounds for on_layout change detection.
    pub(crate) previous_bounds: SecondaryMap<DefaultKey, Bounds<Pixels>>,

    /// The root fiber of the tree
    pub root: Option<GlobalElementId>,

    /// Maps view entity IDs to their fiber roots
    pub view_roots: FxHashMap<EntityId, GlobalElementId>,

    /// Maps focus handles to fibers for focus traversal.
    pub focusable_fibers: FxHashMap<FocusId, GlobalElementId>,

    /// Current frame number for dirty tracking
    current_frame: u64,

    /// Layout context for measured nodes (set during layout computation)
    layout_context: Option<LayoutContext>,

    /// Per-frame counter for reconcile debug logging.
    reconcile_debug_state: ReconcileDebugState,

    /// Last structure epoch used to rebuild layout islands.
    layout_island_epoch: u64,
    /// For each fiber, the island root that governs its layout relative to its parent.
    ///
    /// Island roots are fibers whose descendants cannot affect their outer size (see
    /// `RenderNode::is_layout_boundary`). Children of an island root belong to a new island.
    outer_island_root: SecondaryMap<DefaultKey, GlobalElementId>,
    /// Roots of all layout islands in the main tree (including the main root).
    layout_island_roots: Vec<GlobalElementId>,

    /// Islands that were processed in the most recent sizing pass.
    /// Used for diagnostic logging when measure_child_size fails.
    pub(crate) sizing_pass_processed_islands: FxHashSet<GlobalElementId>,

    /// Whether any fibers were marked dirty this epoch.
    /// Used to skip `end_of_frame_cleanup` on clean frames.
    had_dirty_work: bool,
    /// Dirty slots touched in the current frame (for cheap end-of-frame cleanup).
    dirty_touched: Vec<DefaultKey>,
    /// Per-slot epoch used to dedupe `dirty_touched` entries without clearing the map.
    dirty_touched_epoch: SecondaryMap<DefaultKey, u64>,
    /// Fibers removed during reconciliation, for external cleanup.
    removed_fibers: Vec<GlobalElementId>,
    /// Scene segments removed alongside fibers.
    removed_scene_segments: Vec<SceneSegmentId>,
    /// Tab stops removed alongside fibers.
    removed_tab_stops: Vec<(GlobalElementId, FocusId)>,

    /// Scratch map reused during reconciliation to avoid reallocation.
    ///
    /// Reconciliation is recursive, so these must be pools (stacks) of scratch collections to avoid
    /// clobbering an in-progress parent reconciliation when reconciling a child subtree.
    keyed_children_scratch: Vec<FxHashMap<ElementId, Vec<GlobalElementId>>>,
    unkeyed_children_scratch: Vec<std::collections::VecDeque<GlobalElementId>>,
    used_existing_scratch: Vec<FxHashSet<GlobalElementId>>,
    /// Monotonic epoch tracking structural changes for ordered traversals.
    pub(crate) structure_epoch: u64,
    /// Monotonic epoch tracking changes in hitbox output.
    hitbox_epoch: u64,
    /// Last structure epoch used to assign preorder indices.
    preorder_epoch: u64,
}

struct LayoutContext {
    window: *mut Window,
    app: *mut App,
    scale_factor: f32,
    current_island_root: GlobalElementId,
    layout_calls: usize,
    layout_cache_debug: Option<LayoutCacheDebug>,
}

struct LayoutCacheDebug {
    misses_by_key: collections::FxHashMap<String, usize>,
}

#[derive(Debug)]
struct ReconcileDebugConfig {
    enabled: bool,
    filter: Option<String>,
    limit: usize,
}

static RECONCILE_DEBUG_CONFIG: LazyLock<ReconcileDebugConfig> = LazyLock::new(|| {
    let enabled = std::env::var_os("GPUI_DEBUG_RECONCILE").is_some();
    let filter = std::env::var("GPUI_DEBUG_RECONCILE_FILTER").ok().filter(|s| !s.is_empty());
    let limit = std::env::var("GPUI_DEBUG_RECONCILE_LIMIT")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or(500);
    ReconcileDebugConfig {
        enabled,
        filter,
        limit,
    }
});

#[derive(Debug)]
struct PlaceholderFiberDebugConfig {
    limit: usize,
}

static PLACEHOLDER_FIBER_DEBUG_CONFIG: LazyLock<Option<PlaceholderFiberDebugConfig>> =
    LazyLock::new(|| {
        if std::env::var_os("GPUI_DEBUG_PLACEHOLDER_FIBERS").is_none() {
            return None;
        }
        let limit = std::env::var("GPUI_DEBUG_PLACEHOLDER_FIBERS_LIMIT")
            .ok()
            .and_then(|v| v.parse::<usize>().ok())
            .unwrap_or(200);
        Some(PlaceholderFiberDebugConfig { limit })
    });

static PLACEHOLDER_FIBER_DEBUG_EMITTED: AtomicUsize = AtomicUsize::new(0);

static FRAME_DIAGNOSTICS_ENABLED: LazyLock<bool> = LazyLock::new(|| {
    std::env::var_os("GPUI_DEBUG_FRAME_DIAGNOSTICS").is_some()
});


#[derive(Clone, Copy, Debug, Default)]
struct ReconcileDebugState {
    last_frame: u64,
    emitted: usize,
}

impl Default for FiberTree {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct DeferredDrawOverlayGroup {
    /// Detached roots allocated for this callsite.
    pub(crate) roots: Vec<GlobalElementId>,
    /// Cursor used during a single prepaint pass to allocate/reuse slots.
    pub(crate) next_index: usize,
}

impl FiberTree {
    fn record_dirty_touch(&mut self, key: DefaultKey) {
        if self.current_frame == 0 {
            return;
        }
        if self
            .dirty_touched_epoch
            .get(key)
            .copied()
            .is_some_and(|epoch| epoch == self.current_frame)
        {
            return;
        }
        self.dirty_touched_epoch.insert(key, self.current_frame);
        self.dirty_touched.push(key);
    }

    /// Create a new empty fiber tree
    pub fn new() -> Self {
        Self {
            fibers: SlotMap::default(),
            dirty: SecondaryMap::new(),
            render_nodes: SecondaryMap::new(),
            children: SecondaryMap::new(),
            scope_children: SecondaryMap::new(),
            node_children: SecondaryMap::new(),
            parents: SecondaryMap::new(),
            measure_funcs: SecondaryMap::new(),
            view_state: SecondaryMap::new(),
            paint_cache: SecondaryMap::new(),
            hitbox_state: SecondaryMap::new(),
            layout_state: SecondaryMap::new(),
            cached_styles: SecondaryMap::new(),
            bounds: SecondaryMap::new(),
            effects: SecondaryMap::new(),
            tooltips: SecondaryMap::new(),
            cursor_styles: SecondaryMap::new(),
            input_handlers: SecondaryMap::new(),
            deferred_draws: SecondaryMap::new(),
            deferred_draw_overlay_groups: SecondaryMap::new(),
            tab_stops: SecondaryMap::new(),
            scene_segments: SecondaryMap::new(),
            element_states: SecondaryMap::new(),
            key_contexts: SecondaryMap::new(),
            deferred_priorities: SecondaryMap::new(),
            focus_ids: SecondaryMap::new(),
            layout_listeners: SecondaryMap::new(),
            previous_bounds: SecondaryMap::new(),
            root: None,
            view_roots: FxHashMap::default(),
            focusable_fibers: FxHashMap::default(),
            current_frame: 0,
            layout_context: None,
            reconcile_debug_state: ReconcileDebugState::default(),
            layout_island_epoch: u64::MAX,
            outer_island_root: SecondaryMap::new(),
            layout_island_roots: Vec::new(),
            sizing_pass_processed_islands: FxHashSet::default(),
            had_dirty_work: false,
            dirty_touched: Vec::new(),
            dirty_touched_epoch: SecondaryMap::new(),
            removed_fibers: Vec::new(),
            removed_scene_segments: Vec::new(),
            removed_tab_stops: Vec::new(),
            keyed_children_scratch: Vec::new(),
            unkeyed_children_scratch: Vec::new(),
            used_existing_scratch: Vec::new(),
            structure_epoch: 0,
            hitbox_epoch: 0,
            preorder_epoch: u64::MAX,
        }
    }

    /// Set the current frame number for dirty tracking.
    pub fn begin_frame(&mut self, frame_number: u64) {
        self.current_frame = frame_number;
        self.had_dirty_work = false;
        self.dirty_touched.clear();
    }

    /// Count fibers by kind for testing/diagnostics.
    #[cfg(any(test, feature = "test-support"))]
    #[allow(dead_code)]
    pub fn count_fibers_by_kind(&self) -> (usize, usize, usize) {
        let mut normal = 0;
        let mut state_only = 0;
        let mut detached_root = 0;
        for fiber in self.fibers.values() {
            match fiber.kind {
                FiberKind::Normal => normal += 1,
                FiberKind::StateOnly => state_only += 1,
                FiberKind::DetachedRoot => detached_root += 1,
            }
        }
        (normal, state_only, detached_root)
    }

    fn should_log_reconcile_for_label(&mut self, label: &str) -> bool {
        if !RECONCILE_DEBUG_CONFIG.enabled {
            return false;
        }

        if self.reconcile_debug_state.last_frame != self.current_frame {
            self.reconcile_debug_state.last_frame = self.current_frame;
            self.reconcile_debug_state.emitted = 0;
        }

        if RECONCILE_DEBUG_CONFIG.limit > 0
            && self.reconcile_debug_state.emitted >= RECONCILE_DEBUG_CONFIG.limit
        {
            return false;
        }

        if let Some(filter) = RECONCILE_DEBUG_CONFIG.filter.as_deref() {
            if !label.contains(filter) {
                return false;
            }
        }

        self.reconcile_debug_state.emitted += 1;
        true
    }

    fn log_reconcile_for_label(&mut self, label: &str, message: impl FnOnce() -> String) {
        if self.should_log_reconcile_for_label(label) {
            log::info!("{}", message());
        }
    }

    pub(crate) fn rebuild_layout_islands_if_needed(&mut self) {
        if self.layout_island_epoch == self.structure_epoch {
            return;
        }
        self.layout_island_epoch = self.structure_epoch;
        self.outer_island_root.clear();
        self.layout_island_roots.clear();

        // Compute layout islands across the entire forest of detached roots (any fiber with no
        // parent). This includes the main window root and any detached overlay roots.
        let mut forest_roots: Vec<GlobalElementId> = Vec::new();
        for (key, parent) in self.parents.iter() {
            if parent.is_some() || !self.fibers.contains_key(key) {
                continue;
            }
            if self
                .fibers
                .get(key)
                .is_some_and(|fiber| fiber.kind == FiberKind::StateOnly)
            {
                continue;
            }
            forest_roots.push(Self::id_for_key(key));
        }

        for root in forest_roots {
            if self.get(&root).is_none() {
                continue;
            }
            self.layout_island_roots.push(root);
            self.outer_island_root.insert(Self::key_for_id(root), root);

            let mut stack: Vec<(GlobalElementId, GlobalElementId)> = vec![(root, root)];
            while let Some((fiber_id, current_island_root)) = stack.pop() {
                let children: SmallVec<[GlobalElementId; 8]> = self.children(&fiber_id).collect();
                for child_id in children {
                    if self.get(&child_id).is_none() {
                        continue;
                    }

                    let child_key = Self::key_for_id(child_id);
                    self.outer_island_root
                        .insert(child_key, current_island_root);

                    let is_boundary = self
                        .render_nodes
                        .get(child_key)
                        .is_some_and(|node| node.is_layout_boundary());

                    if is_boundary {
                        self.layout_island_roots.push(child_id);
                        // Children of a boundary belong to the boundary's island.
                        stack.push((child_id, child_id));
                    } else {
                        stack.push((child_id, current_island_root));
                    }
                }
            }
        }
    }

    pub(crate) fn outer_island_root_for(&self, fiber_id: GlobalElementId) -> GlobalElementId {
        self.outer_island_root
            .get(Self::key_for_id(fiber_id))
            .copied()
            .unwrap_or(fiber_id)
    }

    pub(crate) fn final_layout_for_bounds(&self, fiber_id: GlobalElementId) -> Layout {
        let island_root = self.outer_island_root_for(fiber_id);
        self.layout_state
            .get(Self::key_for_id(fiber_id))
            .map(|state| state.final_layout_for_island(island_root))
            .unwrap_or_else(Layout::new)
    }

    pub(crate) fn layout_island_roots(&mut self) -> &[GlobalElementId] {
        self.rebuild_layout_islands_if_needed();
        &self.layout_island_roots
    }

    pub(crate) fn collect_dirty_layout_islands(&mut self) -> FxHashSet<GlobalElementId> {
        self.rebuild_layout_islands_if_needed();
        let mut result = FxHashSet::default();
        for (key, dirty) in self.dirty.iter() {
            if self
                .fibers
                .get(key)
                .is_some_and(|fiber| fiber.kind == FiberKind::StateOnly)
            {
                continue;
            }
            if !dirty.has_layout_work() {
                continue;
            }
            let fiber_id = Self::id_for_key(key);
            result.insert(self.outer_island_root_for(fiber_id));

            // Layout boundaries are island roots for their own nested layout subtree. When a
            // boundary fiber itself is dirty, we must also process its nested island; otherwise
            // the boundary island can be skipped even though it contains work.
            if self
                .render_nodes
                .get(Self::key_for_id(fiber_id))
                .is_some_and(|node| node.is_layout_boundary())
            {
                result.insert(fiber_id);
            }
        }
        result
    }

    pub(crate) fn collect_dirty_sizing_islands(&mut self) -> FxHashSet<GlobalElementId> {
        self.rebuild_layout_islands_if_needed();
        let mut result = FxHashSet::default();
        for (key, dirty) in self.dirty.iter() {
            if self
                .fibers
                .get(key)
                .is_some_and(|fiber| fiber.kind == FiberKind::StateOnly)
            {
                continue;
            }
            if !dirty.has_sizing_work() {
                continue;
            }
            let fiber_id = Self::id_for_key(key);
            result.insert(self.outer_island_root_for(fiber_id));

            // Like layout, sizing work for a layout-boundary fiber must include the boundary's
            // own island root so nested islands aren't missed.
            if self
                .render_nodes
                .get(Self::key_for_id(fiber_id))
                .is_some_and(|node| node.is_layout_boundary())
            {
                result.insert(fiber_id);
            }
        }
        result
    }

    fn bump_structure_epoch(&mut self) {
        self.structure_epoch = self.structure_epoch.wrapping_add(1);
    }

    pub(crate) fn bump_hitbox_epoch(&mut self) {
        self.hitbox_epoch = self.hitbox_epoch.wrapping_add(1);
    }

    pub(crate) fn hitbox_epoch(&self) -> u64 {
        self.hitbox_epoch
    }

    fn key_for_id(id: GlobalElementId) -> DefaultKey {
        id.into()
    }

    fn id_for_key(key: DefaultKey) -> GlobalElementId {
        NodeId::from(key)
    }

    fn insert_fiber(&mut self, fiber: Fiber) -> GlobalElementId {
        let slot_key = self.fibers.insert(fiber);
        self.dirty.insert(slot_key, DirtyFlags::all());
        self.record_dirty_touch(slot_key);
        self.had_dirty_work = true;
        self.children.insert(slot_key, SmallVec::new());
        self.scope_children.insert(slot_key, SmallVec::new());
        self.node_children.insert(slot_key, SmallVec::new());
        self.parents.insert(slot_key, None);
        self.view_state.insert(slot_key, FiberViewState::default());
        self.paint_cache
            .insert(slot_key, FiberPaintCache::default());
        self.hitbox_state
            .insert(slot_key, FiberHitboxState::default());
        self.layout_state
            .insert(slot_key, FiberLayoutState::default());
        let fiber_id = Self::id_for_key(slot_key);
        self.bump_structure_epoch();
        fiber_id
    }

    fn insert_state_only_fiber(&mut self, parent: Option<GlobalElementId>) -> GlobalElementId {
        #[cfg(debug_assertions)]
        if parent.is_none() {
            // `StateOnly` fibers must be attached via `scope_children`.
            log::warn!("creating detached StateOnly fiber");
        }
        let fiber = Fiber::state_only();
        let slot_key = self.fibers.insert(fiber);
        self.children.insert(slot_key, SmallVec::new());
        self.scope_children.insert(slot_key, SmallVec::new());
        self.node_children.insert(slot_key, SmallVec::new());
        self.parents.insert(slot_key, parent);
        self.view_state.insert(slot_key, FiberViewState::default());
        self.paint_cache
            .insert(slot_key, FiberPaintCache::default());
        self.hitbox_state
            .insert(slot_key, FiberHitboxState::default());
        self.layout_state
            .insert(slot_key, FiberLayoutState::default());
        let fiber_id = Self::id_for_key(slot_key);
        self.bump_structure_epoch();
        fiber_id
    }

    /// Create a new fiber and return its ID
    pub fn create_fiber_for(&mut self, descriptor: &AnyElement) -> GlobalElementId {
        let key = descriptor.element_id();
        let child_count = descriptor.child_count();
        let fiber = Fiber::new(key, child_count);
        self.insert_fiber(fiber)
    }

    /// Create a placeholder fiber for non-tree rendering contexts.
    #[track_caller]
    pub fn create_placeholder_fiber(&mut self) -> GlobalElementId {
        let fiber = Fiber::detached_root();
        let fiber_id = self.insert_fiber(fiber);
        if let Some(config) = PLACEHOLDER_FIBER_DEBUG_CONFIG.as_ref() {
            let emitted = PLACEHOLDER_FIBER_DEBUG_EMITTED.fetch_add(1, AtomicOrdering::Relaxed);
            if emitted < config.limit {
                let caller = std::panic::Location::caller();
                log::info!(
                    "[FIBER] create_placeholder_fiber: fiber_id={fiber_id:?} caller={}:{}",
                    caller.file(),
                    caller.line()
                );
            }
        }
        fiber_id
    }

    pub fn create_state_only_child(&mut self, parent: GlobalElementId) -> GlobalElementId {
        let child_id = self.insert_state_only_fiber(Some(parent));
        let parent_key = Self::key_for_id(parent);
        if let Some(list) = self.scope_children.get_mut(parent_key) {
            list.push(child_id);
        } else {
            self.scope_children.insert(parent_key, SmallVec::from_slice(&[child_id]));
        }
        child_id
    }

    /// Create or reuse a child fiber at the given index under the parent.
    /// Ensure a pending view fiber exists for the given view entity id.
    pub fn ensure_pending_view_fiber(&mut self, view_id: EntityId) -> GlobalElementId {
        if let Some(fiber_id) = self.view_roots.get(&view_id).copied() {
            if self.get(&fiber_id).is_some() {
                return fiber_id;
            }
        }
        let fiber = Fiber::for_view(view_id, 0);
        let fiber_id = self.insert_fiber(fiber);
        self.view_roots.insert(view_id, fiber_id);
        self.log_reconcile_for_label("View", || {
            format!(
                "[RECONCILE] ensure_pending_view_fiber: created view_id={} fiber_id={:?}",
                view_id, fiber_id
            )
        });
        fiber_id
    }

    /// Get a fiber by ID
    pub fn get(&self, id: &GlobalElementId) -> Option<&Fiber> {
        self.fibers.get(Self::key_for_id(*id))
    }

    /// Get a mutable fiber by ID
    pub fn get_mut(&mut self, id: &GlobalElementId) -> Option<&mut Fiber> {
        self.fibers.get_mut(Self::key_for_id(*id))
    }

    /// Get a fiber's current dirty flags.
    pub(crate) fn dirty_flags(&self, id: &GlobalElementId) -> DirtyFlags {
        self.dirty
            .get(Self::key_for_id(*id))
            .copied()
            .unwrap_or(DirtyFlags::NONE)
    }

    /// Overwrite a fiber's dirty flags.
    pub(crate) fn set_dirty_flags(&mut self, id: &GlobalElementId, dirty: DirtyFlags) {
        let key = Self::key_for_id(*id);
        self.dirty.insert(key, dirty);
        self.record_dirty_touch(key);
    }

    pub(crate) fn clear_dirty_flags(&mut self, id: &GlobalElementId) {
        if let Some(dirty) = self.dirty.get_mut(Self::key_for_id(*id)) {
            dirty.clear();
        }
    }

    /// Get the parent of a fiber, if any.
    pub fn parent(&self, id: &GlobalElementId) -> Option<GlobalElementId> {
        self.parents.get(Self::key_for_id(*id)).copied().flatten()
    }

    pub(crate) fn is_view_wrapper(&self, id: &GlobalElementId) -> bool {
        self.view_state
            .get(Self::key_for_id(*id))
            .is_some_and(|state| state.is_view_wrapper)
    }

    pub(crate) fn set_is_view_wrapper(
        &mut self,
        id: &GlobalElementId,
        is_view_wrapper: bool,
    ) -> bool {
        let slot_key = Self::key_for_id(*id);
        let Some(state) = self.view_state.get_mut(slot_key) else {
            return false;
        };
        if state.is_view_wrapper == is_view_wrapper {
            return false;
        }

        state.is_view_wrapper = is_view_wrapper;
        self.bump_structure_epoch();
        self.clear_taffy_cache_upwards(id);
        self.mark_content_changed(id);
        self.mark_dirty(id, DirtyFlags::STRUCTURE_CHANGED | DirtyFlags::NEEDS_PAINT);
        true
    }

    pub(crate) fn view_wrapper_child(&self, id: &GlobalElementId) -> Option<GlobalElementId> {
        if !self.is_view_wrapper(id) {
            return None;
        }

        let children = self.children_slice(id);
        #[cfg(debug_assertions)]
        debug_assert!(
            children.len() <= 1,
            "view wrapper fibers must have at most one child"
        );
        children.first().copied()
    }

    pub(crate) fn layout_parent(&self, id: &GlobalElementId) -> Option<GlobalElementId> {
        let mut current = self.parent(id);
        while let Some(parent_id) = current {
            if !self.is_view_wrapper(&parent_id) {
                return Some(parent_id);
            }
            current = self.parent(&parent_id);
        }
        None
    }

    /// Remove a fiber and all its descendants
    pub fn remove(&mut self, id: &GlobalElementId) {
        self.bump_structure_epoch();
        let parent = self.parent(id);
        if let Some(parent_id) = parent {
            let kind = self
                .fibers
                .get(Self::key_for_id(*id))
                .map(|fiber| fiber.kind);
            match kind {
                Some(FiberKind::StateOnly) => self.unlink_scope_child(&parent_id, id),
                Some(_) => self.unlink_child(&parent_id, id),
                None => {
                    let parent_key = Self::key_for_id(parent_id);
                    let is_scope_child = self
                        .scope_children
                        .get(parent_key)
                        .is_some_and(|children| children.contains(id));
                    if is_scope_child {
                        self.unlink_scope_child(&parent_id, id);
                    } else {
                        self.unlink_child(&parent_id, id);
                    }
                }
            }
        }
        let mut to_remove = vec![*id];

        while let Some(fiber_id) = to_remove.pop() {
            let slot_key = Self::key_for_id(fiber_id);
            let key_label = self
                .fibers
                .get(slot_key)
                .map(|fiber| format!("{:?}", fiber.key));
            let current_parent = self.parents.get(slot_key).copied().flatten();
            let current_parent_key = current_parent
                .and_then(|parent_id| self.fibers.get(Self::key_for_id(parent_id)))
                .map(|fiber| format!("{:?}", fiber.key));
            let children = self.remove_storage_for(slot_key);
            if let Some(fiber) = self.fibers.remove(slot_key) {
                if let Some(focus_id) = self.focus_ids.remove(slot_key) {
                    self.focusable_fibers.remove(&focus_id);
                }
                if let Some(view_id) = fiber.view_id {
                    self.view_roots.remove(&view_id);
                }
                self.removed_fibers.push(fiber_id);
                to_remove.extend(children.into_iter());

                if let Some(key_label) = key_label {
                    self.log_reconcile_for_label(&key_label, || {
                        let parent_id = current_parent
                            .map(|id| format!("{id:?}"))
                            .unwrap_or_else(|| "<none>".to_string());
                        let parent_key = current_parent_key
                            .clone()
                            .unwrap_or_else(|| "<none>".to_string());
                        format!(
                            "[RECONCILE] remove: fiber_id={fiber_id:?} key={key_label} parent={parent_id} parent_key={parent_key}"
                        )
                    });
                }
            }
        }
    }

    fn remove_storage_for(&mut self, slot_key: DefaultKey) -> SmallVec<[GlobalElementId; 4]> {
        self.dirty.remove(slot_key);
        self.render_nodes.remove(slot_key);
        let mut children = self.children.remove(slot_key).unwrap_or_default();
        if let Some(scope_children) = self.scope_children.remove(slot_key) {
            children.extend(scope_children);
        }
        self.node_children.remove(slot_key);
        self.parents.remove(slot_key);
        self.element_states.remove(slot_key);
        self.measure_funcs.remove(slot_key);
        self.view_state.remove(slot_key);
        self.paint_cache.remove(slot_key);
        self.hitbox_state.remove(slot_key);
        self.layout_state.remove(slot_key);
        self.cached_styles.remove(slot_key);
        self.bounds.remove(slot_key);
        self.effects.remove(slot_key);
        self.tooltips.remove(slot_key);
        self.cursor_styles.remove(slot_key);
        self.input_handlers.remove(slot_key);
        self.deferred_draws.remove(slot_key);
        self.deferred_draw_overlay_groups.remove(slot_key);
        let fiber_id = Self::id_for_key(slot_key);
        let tab_stops = self.remove_tab_stops_for_fiber(&fiber_id);
        self.removed_tab_stops
            .extend(tab_stops.into_iter().map(|focus_id| (fiber_id, focus_id)));
        if let Some(segments) = self.scene_segments.remove(slot_key) {
            self.removed_scene_segments.push(segments.before);
            if let Some(after) = segments.after {
                self.removed_scene_segments.push(after);
            }
        }

        self.layout_listeners.remove(slot_key);
        self.previous_bounds.remove(slot_key);
        children
    }

    pub(crate) fn scene_segments(&self, id: &GlobalElementId) -> Option<FiberSceneSegments> {
        let slot_key = Self::key_for_id(*id);
        self.scene_segments.get(slot_key).copied()
    }

    pub(crate) fn insert_scene_segments(
        &mut self,
        id: &GlobalElementId,
        segments: FiberSceneSegments,
    ) {
        let slot_key = Self::key_for_id(*id);
        let changed = self
            .scene_segments
            .get(slot_key)
            .map(|existing| existing.before != segments.before || existing.after != segments.after)
            .unwrap_or(true);
        if changed {
            self.scene_segments.insert(slot_key, segments);
            self.bump_structure_epoch();
        }
    }

    pub(crate) fn take_removed_scene_segments(&mut self) -> Vec<SceneSegmentId> {
        std::mem::take(&mut self.removed_scene_segments)
    }

    pub(crate) fn take_removed_tab_stops(&mut self) -> Vec<(GlobalElementId, FocusId)> {
        std::mem::take(&mut self.removed_tab_stops)
    }

    pub(crate) fn remove_tab_stops_for_fiber(&mut self, id: &GlobalElementId) -> Vec<FocusId> {
        let slot_key = Self::key_for_id(*id);
        self.tab_stops.remove(slot_key).unwrap_or_default().to_vec()
    }

    pub(crate) fn take_removed_fibers(&mut self) -> Vec<GlobalElementId> {
        std::mem::take(&mut self.removed_fibers)
    }

    /// Set the parent-child relationship between fibers.
    ///
    /// If the child already has a different parent, it is first unlinked from
    /// the old parent's children list. This is critical for correctness when
    /// fibers are reparented (e.g., when a View moves in the element tree and
    /// its fiber is reused via `view_roots` lookup).
    pub fn set_parent(&mut self, child_id: &GlobalElementId, parent_id: &GlobalElementId) {
        // Check if the child already has a different parent
        if let Some(old_parent_id) = self.parent(child_id) {
            if old_parent_id != *parent_id {
                // Unlink from old parent's children list before reparenting
                self.unlink_child(&old_parent_id, child_id);
            }
        }
        let child_key = Self::key_for_id(*child_id);
        if let Some(fiber) = self.fibers.get_mut(child_key) {
            if fiber.kind == FiberKind::DetachedRoot {
                fiber.kind = FiberKind::Normal;
            }
        }
        self.parents
            .insert(child_key, Some(*parent_id));
    }

    /// Mark a fiber as dirty and propagate flags to ancestors in a single walk.
    pub fn mark_dirty(&mut self, fiber_id: &GlobalElementId, flags: DirtyFlags) {
        let slot_key = Self::key_for_id(*fiber_id);
        let Some(current_dirty) = self.dirty.get(slot_key).copied() else {
            return;
        };

        // Calculate new flags that weren't already set.
        let new_flags = DirtyFlags(flags.0 & !current_dirty.0);
        if new_flags.any() {
            self.had_dirty_work = true;
        }
        self.set_dirty_flags(fiber_id, current_dirty | flags);

        if flags.needs_sizing() {
            self.propagate_sizing_dirty_ancestor(fiber_id);
        }
        if flags.needs_layout() {
            self.propagate_layout_dirty_ancestor(fiber_id);
        }
        if flags.needs_paint() || flags.contains(DirtyFlags::PAINT_STYLE_CHANGED) {
            self.propagate_paint_dirty_ancestor(fiber_id);
        }
    }

    /// Mark content changed on an element.
    /// This is the primary entry point for content mutations.
    pub fn mark_content_changed(&mut self, fiber_id: &GlobalElementId) {
        self.add_dirty_flags(fiber_id, DirtyFlags::CONTENT_CHANGED);
        self.propagate_sizing_dirty_ancestor(fiber_id);
    }

    /// Mark sizing-relevant style changed.
    pub fn mark_sizing_style_changed(&mut self, fiber_id: &GlobalElementId) {
        // Sizing-relevant style changes (padding, border widths, flex params, etc.) can affect
        // both intrinsic sizing and layout within this island, even when the outer size is fixed.
        self.add_dirty_flags(
            fiber_id,
            DirtyFlags::SIZING_STYLE_CHANGED | DirtyFlags::POSITION_STYLE_CHANGED,
        );
        self.propagate_sizing_dirty_ancestor(fiber_id);
        self.propagate_layout_dirty_ancestor(fiber_id);
    }

    /// Mark position-relevant style changed (doesn't affect intrinsic size).
    pub fn mark_position_style_changed(&mut self, fiber_id: &GlobalElementId) {
        self.add_dirty_flags(fiber_id, DirtyFlags::POSITION_STYLE_CHANGED);
        self.propagate_layout_dirty_ancestor(fiber_id);
    }

    /// Mark paint-only style changed.
    pub fn mark_paint_style_changed(&mut self, fiber_id: &GlobalElementId) {
        self.add_dirty_flags(
            fiber_id,
            DirtyFlags::PAINT_STYLE_CHANGED | DirtyFlags::NEEDS_PAINT,
        );
        self.propagate_paint_dirty_ancestor(fiber_id);
    }

    /// Called by sizing pass when intrinsic size computation shows size changed.
    pub fn mark_intrinsic_size_changed(&mut self, fiber_id: &GlobalElementId) {
        self.add_dirty_flags(fiber_id, DirtyFlags::INTRINSIC_SIZE_CHANGED);

        // Clear this element's cached taffy output.
        //
        // Note: We intentionally do *not* clear ancestor caches here.
        // Ancestors are marked with descendant flags so `CacheTree::cache_get` can
        // decide whether to reuse cached layout outputs or recompute.
        let slot_key = Self::key_for_id(*fiber_id);
        if let Some(layout_state) = self.layout_state.get_mut(slot_key) {
            layout_state.taffy_cache.clear();
        }

        // Mark ancestors so layout traversal reaches this subtree.
        let mut current = self.parent(fiber_id);
        while let Some(parent_id) = current {
            self.add_dirty_flags(&parent_id, DirtyFlags::HAS_SIZE_CHANGED_DESCENDANT);
            self.add_dirty_flags(&parent_id, DirtyFlags::HAS_LAYOUT_DIRTY_DESCENDANT);
            let slot_key = Self::key_for_id(parent_id);
            if self
                .render_nodes
                .get(slot_key)
                .is_some_and(|node| node.is_layout_boundary())
            {
                break;
            }
            current = self.parent(&parent_id);
        }
    }

    fn propagate_sizing_dirty_ancestor(&mut self, fiber_id: &GlobalElementId) {
        let mut current = self.parent(fiber_id);
        while let Some(parent_id) = current {
            let slot_key = Self::key_for_id(parent_id);
            let flags = self.dirty.get(slot_key).copied().unwrap_or_default();

            if flags.contains(DirtyFlags::HAS_DIRTY_SIZING_DESCENDANT) {
                break;
            }

            self.add_dirty_flags(&parent_id, DirtyFlags::HAS_DIRTY_SIZING_DESCENDANT);
            if self
                .render_nodes
                .get(slot_key)
                .is_some_and(|node| node.is_layout_boundary())
            {
                break;
            }
            current = self.parent(&parent_id);
        }
    }

    fn propagate_paint_dirty_ancestor(&mut self, fiber_id: &GlobalElementId) {
        let mut current = self.parent(fiber_id);
        while let Some(parent_id) = current {
            let slot_key = Self::key_for_id(parent_id);
            let flags = self.dirty.get(slot_key).copied().unwrap_or_default();

            if flags.contains(DirtyFlags::HAS_PAINT_DIRTY_DESCENDANT) {
                break;
            }

            self.add_dirty_flags(&parent_id, DirtyFlags::HAS_PAINT_DIRTY_DESCENDANT);
            current = self.parent(&parent_id);
        }
    }

    fn propagate_layout_dirty_ancestor(&mut self, fiber_id: &GlobalElementId) {
        let mut current = self.parent(fiber_id);
        while let Some(parent_id) = current {
            let slot_key = Self::key_for_id(parent_id);
            let flags = self.dirty.get(slot_key).copied().unwrap_or_default();

            if flags.contains(DirtyFlags::HAS_LAYOUT_DIRTY_DESCENDANT) {
                break;
            }

            self.add_dirty_flags(&parent_id, DirtyFlags::HAS_LAYOUT_DIRTY_DESCENDANT);
            if self
                .render_nodes
                .get(slot_key)
                .is_some_and(|node| node.is_layout_boundary())
            {
                break;
            }
            current = self.parent(&parent_id);
        }
    }

    fn add_dirty_flags(&mut self, fiber_id: &GlobalElementId, flags: DirtyFlags) {
        let slot_key = Self::key_for_id(*fiber_id);
        if let Some(current) = self.dirty.get_mut(slot_key) {
            let before = *current;
            *current |= flags;
            if *current != before {
                self.had_dirty_work = true;
                self.record_dirty_touch(slot_key);
            }
        } else {
            self.dirty.insert(slot_key, flags);
            self.had_dirty_work = true;
            self.record_dirty_touch(slot_key);
        }

        if flags.needs_layout() {
            self.propagate_layout_dirty_ancestor(fiber_id);
        }
    }

    /// Get cached intrinsic size for a fiber.
    pub fn get_intrinsic_size(
        &self,
        fiber_id: &GlobalElementId,
    ) -> Option<&crate::CachedIntrinsicSize> {
        let slot_key = Self::key_for_id(*fiber_id);
        self.layout_state.get(slot_key)?.intrinsic_size.as_ref()
    }

    /// Set cached intrinsic size for a fiber.
    pub fn set_intrinsic_size(&mut self, fiber_id: &GlobalElementId, size: crate::IntrinsicSize) {
        let slot_key = Self::key_for_id(*fiber_id);
        if let Some(layout_state) = self.layout_state.get_mut(slot_key) {
            log::trace!(
                "set_intrinsic_size {:?}: min_content={:?}, max_content={:?}",
                fiber_id,
                size.min_content,
                size.max_content
            );
            layout_state.intrinsic_size = Some(crate::CachedIntrinsicSize { size });
        }
    }

    /// Clear sizing-related dirty flags after sizing pass.
    pub fn clear_sizing_flags(&mut self, fiber_id: &GlobalElementId) {
        let slot_key = Self::key_for_id(*fiber_id);
        if let Some(flags) = self.dirty.get_mut(slot_key) {
            flags.remove(
                DirtyFlags::CONTENT_CHANGED
                    | DirtyFlags::SIZING_STYLE_CHANGED
                    | DirtyFlags::HAS_DIRTY_SIZING_DESCENDANT,
            );
        }
    }

    /// Iterate over children of a fiber (O(1) access via slice)
    pub fn children(
        &self,
        parent_id: &GlobalElementId,
    ) -> impl Iterator<Item = GlobalElementId> + '_ {
        self.children_slice(parent_id).iter().copied()
    }

    /// Get children as a slice for O(1) indexed access
    pub fn children_slice(&self, parent_id: &GlobalElementId) -> &[GlobalElementId] {
        self.children
            .get(Self::key_for_id(*parent_id))
            .map(|children| children.as_slice())
            .unwrap_or(&[])
    }

    /// Ensure each fiber has an up-to-date preorder index for ordering tasks.
    pub(crate) fn ensure_preorder_indices(&mut self) {
        if self.preorder_epoch == self.structure_epoch {
            return;
        }

        self.preorder_epoch = self.structure_epoch;
        let mut roots: Vec<GlobalElementId> = Vec::new();
        if let Some(root) = self.root {
            roots.push(root);
        }
        // Include detached trees (e.g. overlay roots for `Window::defer_draw`) so ordering
        // and cached-subtree replay remain correct for out-of-tree roots.
        for (key, _fiber) in self.fibers.iter() {
            let id = Self::id_for_key(key);
            if Some(id) == self.root {
                continue;
            }
            if self.parent(&id).is_none() {
                roots.push(id);
            }
        }

        let mut visited: FxHashSet<GlobalElementId> = FxHashSet::default();
        let mut index: u64 = 0;
        for root in roots {
            if !visited.insert(root) {
                continue;
            }
            let mut stack = vec![root];
            while let Some(fiber_id) = stack.pop() {
                if let Some(fiber) = self.get_mut(&fiber_id) {
                    fiber.preorder_index = index;
                    index = index.wrapping_add(1);
                }
                for child_id in self.children_slice(&fiber_id).iter().rev() {
                    let child_id = *child_id;
                    if visited.insert(child_id) {
                        stack.push(child_id);
                    }
                }
            }
        }
    }

    pub(crate) fn preorder_index(&self, fiber_id: &GlobalElementId) -> Option<u64> {
        self.get(fiber_id).map(|fiber| fiber.preorder_index)
    }

    pub(crate) fn scene_segment_order(&self, root: GlobalElementId) -> Vec<SceneSegmentId> {
        let mut order = Vec::with_capacity(self.fibers.len().saturating_mul(2));
        let mut deferred_fibers: Vec<(GlobalElementId, usize)> = Vec::new();
        // `scene_segment_list` is a cached *subtree* segment sequence. If we use it for a fiber,
        // we must not also emit segments for its descendants, or we will duplicate segments and
        // break ordering/performance. We still need to traverse descendants to discover deferred
        // fibers, so we support a "scan-only" mode that collects deferred roots without emitting.
        let mut stack = vec![(root, true, true)]; // (fiber_id, entering, emit_segments)

        while let Some((fiber_id, entering, emit_segments)) = stack.pop() {
            if self.get(&fiber_id).is_none() {
                continue;
            }

            // Skip deferred fibers during main traversal - they'll be appended at the end
            if let Some(&priority) = self.deferred_priorities.get(fiber_id.into()) {
                if entering {
                    deferred_fibers.push((fiber_id, priority));
                }
                continue;
            }

            if !emit_segments {
                if entering {
                    for child_id in self.children_slice(&fiber_id).iter().rev() {
                        stack.push((*child_id, true, false));
                    }
                }
                continue;
            }

            if entering {
                if let Some(list) = self
                    .paint_cache
                    .get(Self::key_for_id(fiber_id))
                    .and_then(|cache| cache.scene_segment_list.as_ref())
                {
                    order.extend(list.iter().copied());
                    // Still need to explore children to find deferred fibers,
                    // but skip the exit phase since segment_list already includes
                    // this fiber's before/after segments.
                    for child_id in self.children_slice(&fiber_id).iter().rev() {
                        stack.push((*child_id, true, false));
                    }
                    continue;
                }
                if let Some(segments) = self.scene_segments(&fiber_id) {
                    order.push(segments.before);
                }
                stack.push((fiber_id, false, true));
                for child_id in self.children_slice(&fiber_id).iter().rev() {
                    stack.push((*child_id, true, true));
                }
            } else {
                // Check if fiber has an "after" scene segment (used by Div/Svg for wrapping)
                if let Some(segments) = self.scene_segments(&fiber_id) {
                    if let Some(after) = segments.after {
                        order.push(after);
                    }
                }
            }
        }

        // Append deferred fibers at the end, sorted by priority (lower priority = paint first)
        deferred_fibers.sort_by_key(|(_, priority)| *priority);
        for (fiber_id, _priority) in &deferred_fibers {
            self.collect_fiber_segments(fiber_id, &mut order);
        }

        order
    }

    /// Collect all scene segments for a fiber subtree in tree order.
    fn collect_fiber_segments(&self, root: &GlobalElementId, order: &mut Vec<SceneSegmentId>) {
        let mut stack = vec![(*root, true)];

        while let Some((fiber_id, entering)) = stack.pop() {
            if self.get(&fiber_id).is_none() {
                continue;
            };

            if entering {
                if let Some(list) = self
                    .paint_cache
                    .get(Self::key_for_id(fiber_id))
                    .and_then(|cache| cache.scene_segment_list.as_ref())
                {
                    order.extend(list.iter().copied());
                    continue;
                }
                if let Some(segments) = self.scene_segments(&fiber_id) {
                    order.push(segments.before);
                }
                stack.push((fiber_id, false));
                for child_id in self.children_slice(&fiber_id).iter().rev() {
                    stack.push((*child_id, true));
                }
            } else {
                if let Some(segments) = self.scene_segments(&fiber_id) {
                    if let Some(after) = segments.after {
                        order.push(after);
                    }
                }
            }
        }
    }

    /// Returns true if any ancestor needs prepaint (excludes NEEDS_LAYOUT).
    pub fn has_prepaint_dirty_ancestor(&self, fiber_id: &GlobalElementId) -> bool {
        let mut current = self.parent(fiber_id);
        while let Some(parent_id) = current {
            if self.get(&parent_id).is_none() {
                break;
            };
            if self.dirty_flags(&parent_id).needs_prepaint() {
                return true;
            }
            current = self.parent(&parent_id);
        }
        false
    }

    /// Check if a fiber can replay its cached prepaint state.
    pub fn can_replay_prepaint(&self, fiber_id: &GlobalElementId) -> bool {
        self.can_replay_cached(fiber_id, false)
    }

    /// Check if a fiber can replay its cached paint list.
    pub fn can_replay_paint(&self, fiber_id: &GlobalElementId) -> bool {
        self.can_replay_cached(fiber_id, true)
    }

    fn can_replay_cached(&self, fiber_id: &GlobalElementId, check_paint: bool) -> bool {
        if self.get(fiber_id).is_none() {
            return false;
        };
        let dirty = self.dirty_flags(fiber_id);
        let needs_work = if check_paint {
            dirty.needs_paint()
        } else {
            dirty.needs_prepaint()
        };

        !needs_work
            && !dirty.contains(DirtyFlags::HAS_PAINT_DIRTY_DESCENDANT)
            && !self.has_prepaint_dirty_ancestor(fiber_id)
            && self.has_cached_output(fiber_id)
    }

    pub(crate) fn has_cached_output(&self, fiber_id: &GlobalElementId) -> bool {
        self.paint_cache
            .get(Self::key_for_id(*fiber_id))
            .is_some_and(|cache| cache.prepaint_state.is_some() && cache.paint_list.is_some())
    }

    /// Get the root fiber for a view entity
    pub fn get_view_root(&self, entity_id: EntityId) -> Option<GlobalElementId> {
        self.view_roots.get(&entity_id).cloned()
    }

    /// Set the root fiber for a view entity
    pub fn set_view_root(&mut self, entity_id: EntityId, fiber_id: GlobalElementId) {
        self.view_roots.insert(entity_id, fiber_id);
    }

    #[cfg(debug_assertions)]
    pub(crate) fn assert_no_detached_scopes(&self) {
        for (key, parent) in self.parents.iter() {
            if parent.is_none() {
                if let Some(fiber) = self.fibers.get(key) {
                    if fiber.kind == FiberKind::StateOnly {
                        panic!(
                            "INVARIANT VIOLATION: StateOnly fiber without parent: {:?}",
                            Self::id_for_key(key)
                        );
                    }
                }
            }
        }
    }

    /// End-of-frame cleanup: clears per-frame work flags and recomputes descendant flags.
    ///
    /// This should be called at the end of each frame after all rendering is complete.
    /// It clears per-frame work flags and properly recomputes `HAS_PAINT_DIRTY_DESCENDANT`
    /// bottom-up.
    ///
    /// On clean frames (no dirty work), this is a no-op since the tree state is unchanged.
    pub fn end_of_frame_cleanup(&mut self) {
        // Skip cleanup on clean frames - tree state is unchanged from previous frame.
        // Descendant flags are already accurate, and no work flags need clearing.
        if !self.had_dirty_work {
            #[cfg(debug_assertions)]
            self.assert_no_detached_scopes();
            return;
        }

        let clear_mask = DirtyFlags::WORK_FLAGS
            | DirtyFlags::HAS_DIRTY_SIZING_DESCENDANT
            | DirtyFlags::HAS_SIZE_CHANGED_DESCENDANT
            | DirtyFlags::HAS_LAYOUT_DIRTY_DESCENDANT
            | DirtyFlags::HAS_PAINT_DIRTY_DESCENDANT;

        // Clear only fibers that were touched this frame. All work/descendant flags are set via
        // `add_dirty_flags`/`set_dirty_flags`, which record touches, so this remains correct while
        // avoiding an O(all fibers) scan on most frames.
        for key in self.dirty_touched.drain(..) {
            if let Some(dirty) = self.dirty.get_mut(key) {
                dirty.remove(clear_mask);
            }
        }

        #[cfg(debug_assertions)]
        self.assert_no_detached_scopes();
    }

    pub(crate) fn log_dirty_touched_summary_if_enabled(&self) {
        let Some(value) = std::env::var_os("GPUI_DEBUG_DIRTY_SUMMARY") else {
            return;
        };
        let value = value.to_string_lossy();
        let interval: u64 = value.parse().ok().filter(|n: &u64| *n > 0).unwrap_or(1);
        if interval > 1 && !self.current_frame.is_multiple_of(interval) {
            return;
        }

        if self.dirty_touched.is_empty() {
            log::info!(
                "dirty summary: frame={} touched=0 (had_dirty_work={})",
                self.current_frame,
                self.had_dirty_work
            );
            return;
        }

        let mut entries: Vec<(DefaultKey, DirtyFlags, u32)> = self
            .dirty_touched
            .iter()
            .copied()
            .map(|key| {
                let flags = self.dirty.get(key).copied().unwrap_or_default();
                let bits = flags.bits().count_ones();
                (key, flags, bits)
            })
            .collect();
        entries.sort_by_key(|(_, flags, bits)| (std::cmp::Reverse(*bits), std::cmp::Reverse(flags.bits())));

        let mut rendered = String::new();
        for (i, (key, flags, _bits)) in entries.into_iter().take(25).enumerate() {
            if i > 0 {
                rendered.push_str(" | ");
            }
            let id = GlobalElementId::from(key);
            let label = self
                .fibers
                .get(key)
                .map(|fiber| format!("{:?}", fiber.key))
                .unwrap_or_else(|| "<missing>".to_string());
            rendered.push_str(&format!("{id:?}={label}:{flags:?}"));
        }

        log::info!(
            "dirty summary: frame={} touched={} (had_dirty_work={}) top=[{}]",
            self.current_frame,
            self.dirty_touched.len(),
            self.had_dirty_work,
            rendered
        );
    }

    pub(crate) fn set_layout_context(
        &mut self,
        window: *mut Window,
        app: *mut App,
        scale_factor: f32,
        current_island_root: GlobalElementId,
    ) {
        let layout_cache_debug = std::env::var_os("GPUI_DEBUG_LAYOUT_CACHE_SUMMARY")
            .is_some()
            .then_some(LayoutCacheDebug {
                misses_by_key: collections::FxHashMap::default(),
            });
        self.layout_context = Some(LayoutContext {
            window,
            app,
            scale_factor,
            current_island_root,
            layout_calls: 0,
            layout_cache_debug,
        });
    }

    pub fn layout_calls(&self) -> usize {
        self.layout_context
            .as_ref()
            .map_or(0, |ctx| ctx.layout_calls)
    }

    pub(crate) fn log_layout_cache_summary_if_enabled(&self) {
        let Some(ctx) = self.layout_context.as_ref() else {
            return;
        };
        let Some(debug) = ctx.layout_cache_debug.as_ref() else {
            return;
        };

        if ctx.layout_calls == 0 {
            return;
        }

        let mut top: Vec<(&str, usize)> = debug
            .misses_by_key
            .iter()
            .map(|(k, v)| (k.as_str(), *v))
            .collect();
        top.sort_by_key(|(_, v)| std::cmp::Reverse(*v));

        let mut rendered = String::new();
        for (i, (key, count)) in top.into_iter().take(15).enumerate() {
            if i > 0 {
                rendered.push_str(", ");
            }
            rendered.push_str(key);
            rendered.push('=');
            rendered.push_str(&count.to_string());
        }

        log::info!(
            "layout cache misses: island={:?} misses={} top=[{}]",
            ctx.current_island_root,
            ctx.layout_calls,
            rendered
        );
    }

    pub(crate) fn clear_layout_context(&mut self) {
        self.layout_context = None;
    }

    /// Clear the Taffy cache for a fiber and its ancestors.
    ///
    /// This forces the next layout pass to traverse into this subtree and recompute.
    pub(crate) fn clear_taffy_cache_upwards(&mut self, fiber_id: &GlobalElementId) {
        let mut current = Some(*fiber_id);
        while let Some(id) = current {
            let Some(layout_state) = self.layout_state.get_mut(Self::key_for_id(id)) else {
                break;
            };
            layout_state.taffy_cache.clear();
            current = self.parent(&id);
        }
    }

    /// Reconcile a descriptor against a fiber, returning whether it changed.
    /// Uses direct style diffing and structure comparison to determine dirty flags.
    /// When allow_bailout is false, unchanged subtrees are still reconciled.
    pub fn reconcile(
        &mut self,
        fiber_id: &GlobalElementId,
        descriptor: &AnyElement,
        allow_bailout: bool,
    ) -> bool {
        let mut view_changed = false;
        if let Some(view) = descriptor.as_any_view() {
            let entity_id = view.entity_id();
            let mut hasher = FxHasher::default();
            entity_id.hash(&mut hasher);
            let new_view_hash = hasher.finish();
            let slot_key = Self::key_for_id(*fiber_id);
            let old_view_hash = self
                .view_state
                .get(slot_key)
                .map(|state| state.view_descriptor_hash);
            if old_view_hash.is_some_and(|old| old != new_view_hash) {
                if let Some(view_state) = self.view_state.get_mut(slot_key) {
                    view_state.view_descriptor_hash = new_view_hash;
                }
                self.add_dirty_flags(fiber_id, DirtyFlags::STRUCTURE_CHANGED);
                self.mark_content_changed(fiber_id);
                self.add_dirty_flags(fiber_id, DirtyFlags::NEEDS_PAINT);
                view_changed = true;
            }
        }

        let reconciled_changed = self.reconcile_internal(fiber_id, descriptor, true, allow_bailout);
        view_changed || reconciled_changed
    }

    /// Reconcile a wrapper fiber against its expanded descriptor without changing kind.
    pub fn reconcile_wrapper(
        &mut self,
        fiber_id: &GlobalElementId,
        descriptor: &AnyElement,
        allow_bailout: bool,
    ) -> bool {
        self.reconcile_internal(fiber_id, descriptor, false, allow_bailout)
    }

    fn reconcile_internal(
        &mut self,
        fiber_id: &GlobalElementId,
        descriptor: &AnyElement,
        allow_kind_change: bool,
        allow_bailout: bool,
    ) -> bool {
        let new_child_count = descriptor.child_count();

        let slot_key = Self::key_for_id(*fiber_id);
        let existing_dirty = self.dirty_flags(fiber_id);

        let mut changed = false;
        let mut structure_epoch_changed = false;
        let has_render_node = self.render_nodes.get(slot_key).is_some();
        // Always check if descriptor is a View - needed to skip reconcile_children for Views
        let is_view_descriptor = descriptor.as_any_view().is_some();

        let (children_unchanged, kind_changed, child_count_changed, kind_change) = {
            let Some(fiber) = self.fibers.get_mut(slot_key) else {
                return false;
            };

            let children_unchanged = fiber.child_count == new_child_count;
            let mut kind_changed = false;
            let mut kind_change: Option<(Option<ElementId>, Option<ElementId>)> = None;
            if allow_kind_change {
                let new_key = descriptor.element_id();
                if fiber.key != new_key {
                    kind_change = Some((fiber.key.clone(), new_key.clone()));
                    fiber.key = new_key;
                    structure_epoch_changed = true;
                    kind_changed = true;
                }
                // Also update view_id if this is a view
                let new_view_id = descriptor.as_any_view().map(|v| v.entity_id());
                if fiber.view_id != new_view_id {
                    fiber.view_id = new_view_id;
                    structure_epoch_changed = true;
                    kind_changed = true;
                }
            }

            let mut child_count_changed = false;
            if fiber.child_count != new_child_count {
                fiber.child_count = new_child_count;
                child_count_changed = true;
            }
            (
                children_unchanged,
                kind_changed,
                child_count_changed,
                kind_change,
            )
        };

        if kind_changed {
            self.add_dirty_flags(fiber_id, DirtyFlags::STRUCTURE_CHANGED);
            self.mark_content_changed(fiber_id);
            self.add_dirty_flags(fiber_id, DirtyFlags::NEEDS_PAINT);
            changed = true;

            if !is_view_descriptor {
                if let Some(view_state) = self.view_state.get_mut(slot_key) {
                    view_state.view_data = None;
                    view_state.view_descriptor_hash = 0;
                }
            }
            self.deferred_priorities.remove(slot_key);

            if let Some((old_key, new_key)) = kind_change {
                let label = format!("{new_key:?}");
                self.log_reconcile_for_label(&label, || {
                    format!(
                        "[RECONCILE] kind_changed: fiber_id={fiber_id:?} {old_key:?} -> {new_key:?} has_render_node={has_render_node}"
                    )
                });
            }
        }

        // Style diffing - categorize by what the change affects.
        if let Some(new_style) = descriptor.style() {
            let mut should_cache_style = false;
            if let Some(cached) = self.cached_styles.get(slot_key) {
                if !new_style.sizing_eq(cached) {
                    self.mark_sizing_style_changed(fiber_id);
                    changed = true;
                    should_cache_style = true;
                } else if !new_style.position_eq(cached) {
                    self.mark_position_style_changed(fiber_id);
                    changed = true;
                    should_cache_style = true;
                } else if !new_style.paint_eq(cached) {
                    self.mark_paint_style_changed(fiber_id);
                    changed = true;
                    should_cache_style = true;
                }
            } else {
                self.mark_sizing_style_changed(fiber_id);
                changed = true;
                should_cache_style = true;
            }

            if should_cache_style {
                self.cached_styles.insert(slot_key, new_style.clone());
            }
        } else if self.cached_styles.remove(slot_key).is_some() {
            self.mark_sizing_style_changed(fiber_id);
            changed = true;
        }

        // Modifier diffing (e.g., z_index affects paint ordering).
        if self.deferred_priorities.get(slot_key).copied()
            != descriptor.modifiers().z_index
        {
            self.mark_dirty(fiber_id, DirtyFlags::NEEDS_PAINT);
            changed = true;
        }

        if child_count_changed {
            self.add_dirty_flags(fiber_id, DirtyFlags::STRUCTURE_CHANGED);
            self.mark_content_changed(fiber_id);
            self.add_dirty_flags(fiber_id, DirtyFlags::NEEDS_PAINT);
            changed = true;
        }

        if allow_bailout && !changed && children_unchanged && existing_dirty.is_subtree_clean() {
            log::trace!("RECONCILE_BAILOUT: fiber_id={:?} - skip subtree", fiber_id);
            return false;
        }
        if structure_epoch_changed {
            self.bump_structure_epoch();
        }

        let children = descriptor.children();
        // Skip reconcile_children for View descriptors - Views have empty descriptor children
        // because their real children come from rendering (handled in expand_view_fibers).
        // Calling reconcile_children with empty children would incorrectly remove existing children.
        if !is_view_descriptor {
            self.reconcile_children(fiber_id, children, allow_bailout);
        }

        changed
    }

    /// Reconcile children of a fiber against descriptor children.
    /// allow_bailout is passed through to child reconciliation.
    pub fn reconcile_children(
        &mut self,
        parent_id: &GlobalElementId,
        children: &[AnyElement],
        allow_bailout: bool,
    ) {
        let existing_children =
            SmallVec::<[GlobalElementId; 8]>::from_slice(self.children_slice(parent_id));
        let parent_key = Self::key_for_id(*parent_id);
        let parent_label = self
            .fibers
            .get(parent_key)
            .map(|fiber| format!("{:?}", fiber.key))
            .unwrap_or_else(|| "<missing>".to_string());

        // Preserve node-managed children (e.g. conditional slots) across descriptor reconciliation.
        // These children are reconciled separately and should never be removed just because the
        // descriptor tree doesn't include them.
        let mut existing_node_children = self
            .node_children
            .get(parent_key)
            .cloned()
            .unwrap_or_default();
        existing_node_children.retain(|child_id| self.get(child_id).is_some());
        if let Some(stored) = self.node_children.get_mut(parent_key) {
            *stored = existing_node_children.clone();
        }

        let mut existing_descriptor_children: SmallVec<[GlobalElementId; 8]> = SmallVec::new();
        for child_id in &existing_children {
            if !existing_node_children.contains(child_id) {
                existing_descriptor_children.push(*child_id);
            }
        }

        // Use pooled scratch collections to avoid allocation in hot path
        let mut keyed_children = self.keyed_children_scratch.pop().unwrap_or_default();
        keyed_children.clear();
        let mut unkeyed_children = self.unkeyed_children_scratch.pop().unwrap_or_default();
        unkeyed_children.clear();
        let mut used_existing = self.used_existing_scratch.pop().unwrap_or_default();
        used_existing.clear();

        {
            let fibers = &self.fibers;
            for child_id in &existing_descriptor_children {
                let Some(child) = fibers.get(Self::key_for_id(*child_id)) else {
                    continue;
                };
                if let Some(key) = &child.key {
                    keyed_children
                        .entry(key.clone())
                        .or_default()
                        .push(*child_id);
                } else {
                    unkeyed_children.push_back(*child_id);
                }
            }
        }
        let mut new_children: SmallVec<[GlobalElementId; 8]> =
            SmallVec::with_capacity(children.len() + existing_node_children.len());

        self.log_reconcile_for_label(&parent_label, || {
            format!(
                "[RECONCILE] reconcile_children: parent_id={parent_id:?} parent_key={parent_label} descriptor_children={} existing_children={} node_children={}",
                children.len(),
                existing_children.len(),
                existing_node_children.len(),
            )
        });

        for child_desc in children {
            let element_key = child_desc.element_id();
            let mut child_id = None;
            let child_label = format!("{element_key:?}");

            // Check for view match first - views match by entity_id via view_roots
            if let Some(view) = child_desc.as_any_view() {
                let view_id = view.entity_id();
                if let Some(existing_id) = self.view_roots.get(&view_id).copied() {
                    if self.get(&existing_id).is_some() && !used_existing.contains(&existing_id) {
                        child_id = Some(existing_id);
                        self.log_reconcile_for_label(&child_label, || {
                            format!(
                                "[RECONCILE] match(view_root): parent={parent_id:?} parent_key={parent_label} child_key={child_label} fiber_id={existing_id:?}"
                            )
                        });
                    }
                }
            }

            // Then check for keyed match
            if child_id.is_none() {
                if let Some(key) = &element_key {
                    if let Some(list) = keyed_children.get_mut(key) {
                        while let Some(candidate) = list.pop() {
                            if used_existing.insert(candidate) {
                                child_id = Some(candidate);
                                self.log_reconcile_for_label(&child_label, || {
                                    format!(
                                        "[RECONCILE] match(keyed): parent={parent_id:?} parent_key={parent_label} child_key={child_label} fiber_id={candidate:?}"
                                    )
                                });
                                break;
                            }
                        }
                    }
                }
            }

            if child_id.is_none() {
                while let Some(candidate) = unkeyed_children.pop_front() {
                    if used_existing.insert(candidate) {
                        child_id = Some(candidate);
                        self.log_reconcile_for_label(&child_label, || {
                            format!(
                                "[RECONCILE] match(unkeyed): parent={parent_id:?} parent_key={parent_label} child_key={child_label} fiber_id={candidate:?}"
                            )
                        });
                        break;
                    }
                }
            }

            let child_id = child_id.unwrap_or_else(|| {
                let new_id = self.create_fiber_for(child_desc);
                self.log_reconcile_for_label(&child_label, || {
                    format!(
                        "[RECONCILE] create: parent={parent_id:?} parent_key={parent_label} child_key={child_label} fiber_id={new_id:?}"
                    )
                });
                new_id
            });
            used_existing.insert(child_id);

            self.set_parent(&child_id, parent_id);
            self.reconcile(&child_id, child_desc, allow_bailout);
            new_children.push(child_id);
        }

        for child_id in &existing_node_children {
            if used_existing.insert(*child_id) {
                self.set_parent(child_id, parent_id);
                new_children.push(*child_id);
            }
        }

        if existing_children != new_children {
            // Remove children that weren't reused. used_existing contains all IDs
            // that are in new_children, so we can use it directly instead of
            // creating a new set.
            for existing_id in existing_children.iter() {
                if !used_existing.contains(existing_id) {
                    let removed_key = self
                        .get(existing_id)
                        .map(|fiber| format!("{:?}", fiber.key))
                        .unwrap_or_else(|| "<missing>".to_string());
                    let label = format!("{parent_label} {removed_key}");
                    self.log_reconcile_for_label(&label, || {
                        format!(
                            "[RECONCILE] remove_unmatched_child: parent={parent_id:?} parent_key={parent_label} child_id={existing_id:?} child_key={removed_key}"
                        )
                    });
                    self.remove(existing_id);
                }
            }
            self.log_reconcile_for_label(&parent_label, || {
                format!(
                    "[RECONCILE] relink_children: parent={parent_id:?} parent_key={parent_label} old_len={} new_len={}",
                    existing_children.len(),
                    new_children.len()
                )
            });
            self.relink_children_in_order(parent_id, &new_children);
            self.clear_taffy_cache_upwards(parent_id);
            self.mark_dirty(
                parent_id,
                DirtyFlags::STRUCTURE_CHANGED | DirtyFlags::NEEDS_LAYOUT | DirtyFlags::NEEDS_PAINT,
            );
        }

        // Return pooled collections for reuse
        self.keyed_children_scratch.push(keyed_children);
        self.unkeyed_children_scratch.push(unkeyed_children);
        self.used_existing_scratch.push(used_existing);
    }

    /// Relink children in a new order (Vec-based: O(n) copy)
    pub(crate) fn relink_children_in_order(
        &mut self,
        parent_id: &GlobalElementId,
        children: &[GlobalElementId],
    ) {
        self.bump_structure_epoch();
        for child_id in children {
            self.parents
                .insert(Self::key_for_id(*child_id), Some(*parent_id));
        }

        let parent_key = Self::key_for_id(*parent_id);
        if let Some(list) = self.children.get_mut(parent_key) {
            let old_children: Vec<_> = list.iter().copied().collect();
            log::trace!(
                "[FIBER_CHILDREN] relink_children_in_order: parent {:?}, old={:?}, new={:?}",
                parent_id,
                old_children,
                children
            );
            list.clear();
            list.extend(children.iter().copied());
        } else if !children.is_empty() {
            log::trace!(
                "[FIBER_CHILDREN] relink_children_in_order: parent {:?} (new entry), children={:?}",
                parent_id,
                children
            );
            self.children
                .insert(parent_key, children.iter().copied().collect());
        }
    }

    // === VNode-based reconciliation methods removed ===
    // The VNode system is unused; production uses AnyElement descriptor reconciliation.
    // See reconcile_children() for the production implementation.

    fn unlink_scope_child(&mut self, parent_id: &GlobalElementId, child_id: &GlobalElementId) {
        self.bump_structure_epoch();

        if let Some(list) = self.scope_children.get_mut(Self::key_for_id(*parent_id)) {
            list.retain(|id| id != child_id);
        }

        self.parents.insert(Self::key_for_id(*child_id), None);
    }

    /// Unlink a child from its parent (Vec-based: O(n) search + remove)
    fn unlink_child(&mut self, parent_id: &GlobalElementId, child_id: &GlobalElementId) {
        self.clear_taffy_cache_upwards(parent_id);

        self.bump_structure_epoch();
        self.mark_dirty(
            parent_id,
            DirtyFlags::STRUCTURE_CHANGED | DirtyFlags::NEEDS_LAYOUT | DirtyFlags::NEEDS_PAINT,
        );

        // Remove from parent's children Vec
        if let Some(list) = self.children.get_mut(Self::key_for_id(*parent_id)) {
            if let Some(pos) = list.iter().position(|id| id == child_id) {
                log::trace!(
                    "[FIBER_CHILDREN] unlink_child: removing {:?} from parent {:?} at position {}",
                    child_id,
                    parent_id,
                    pos
                );
                list.remove(pos);
            } else {
                log::trace!(
                    "[FIBER_CHILDREN] unlink_child: {:?} not found in parent {:?}'s children list",
                    child_id,
                    parent_id
                );
            }
        } else {
            log::trace!(
                "[FIBER_CHILDREN] unlink_child: parent {:?} has no children list (may have been removed)",
                parent_id
            );
        }

        // Clear the child's parent pointer
        self.parents.insert(Self::key_for_id(*child_id), None);
    }

    /// Ensure a root fiber exists, creating one if necessary.
    pub fn ensure_root(
        &mut self,
        descriptor: &AnyElement,
        root_id: GlobalElementId,
    ) -> GlobalElementId {
        if self.get(&root_id).is_none() {
            let new_root = self.create_fiber_for(descriptor);
            self.root = Some(new_root);
            self.bump_structure_epoch();
            return new_root;
        }

        if self.root != Some(root_id) {
            self.root = Some(root_id);
            self.bump_structure_epoch();
        }
        root_id
    }
}

impl TraversePartialTree for FiberTree {
    type ChildIter<'a>
        = smallvec::IntoIter<[NodeId; 8]>
    where
        Self: 'a;

    fn child_ids(&self, node_id: NodeId) -> Self::ChildIter<'_> {
        let current_island_root = self
            .layout_context
            .as_ref()
            .map(|ctx| ctx.current_island_root);

        let all_children: SmallVec<[NodeId; 8]> =
            self.children_slice(&node_id).iter().copied().collect();
        let mut children: SmallVec<[NodeId; 8]> = SmallVec::new();
        let mut excluded: SmallVec<[NodeId; 8]> = SmallVec::new();

        fn push_child(
            tree: &FiberTree,
            current_island_root: Option<GlobalElementId>,
            included: &mut SmallVec<[NodeId; 8]>,
            excluded: &mut SmallVec<[NodeId; 8]>,
            child_id: NodeId,
        ) {
            if tree.is_view_wrapper(&child_id) {
                for grandchild_id in tree.children_slice(&child_id).iter().copied() {
                    push_child(
                        tree,
                        current_island_root,
                        included,
                        excluded,
                        grandchild_id,
                    );
                }
                return;
            }

            if let Some(island_root) = current_island_root {
                let child_island = tree.outer_island_root_for(child_id);
                if child_island != island_root {
                    excluded.push(child_id);
                    return;
                }
            }

            included.push(child_id);
        }

        for child_id in all_children.iter().copied() {
            push_child(
                self,
                current_island_root,
                &mut children,
                &mut excluded,
                child_id,
            );
        }
        if !excluded.is_empty() {
            log::trace!(
                "[CHILD_IDS_EXCLUDED] parent={:?} current_island={:?} included={:?} excluded={:?}",
                node_id,
                current_island_root,
                children,
                excluded
            );
        }
        children.into_iter()
    }

    fn child_count(&self, node_id: NodeId) -> usize {
        self.child_ids(node_id).len()
    }

    fn get_child_id(&self, node_id: NodeId, index: usize) -> NodeId {
        self.child_ids(node_id)
            .nth(index)
            .unwrap_or_else(|| panic!("missing child {index} for node {node_id:?}"))
    }
}

impl TraverseTree for FiberTree {}

impl LayoutPartialTree for FiberTree {
    type CoreContainerStyle<'a>
        = &'a TaffyStyle
    where
        Self: 'a;

    type CustomIdent = String;

    fn get_core_container_style(&self, node_id: NodeId) -> Self::CoreContainerStyle<'_> {
        let fiber_id = node_id;
        let _fiber = self.get(&fiber_id).unwrap_or_else(|| {
                // Collect diagnostic information about what parent might contain this stale child
                let mut parent_info = String::new();
                for (key, children) in self.children.iter() {
                    if children.contains(&fiber_id) {
                        let parent_id = Self::id_for_key(key);
                        let parent_exists = self.fibers.contains_key(key);
                        let parent_key = if let Some(fiber) = self.fibers.get(key) {
                            format!("{:?}", fiber.key)
                        } else {
                            "N/A".to_string()
                        };
                        parent_info.push_str(&format!(
                            "\n  - Found in parent {:?} (exists={}, key={}), children={:?}",
                            parent_id, parent_exists, parent_key, children
                        ));
                    }
                }
                if parent_info.is_empty() {
                    parent_info = "\n  - Not found in any parent's children list".to_string();
                }
                panic!(
                    "missing fiber for node {node_id:?}\nDiagnostics:{parent_info}\nTotal fibers: {}, Total children entries: {}",
                    self.fibers.len(),
                    self.children.len()
                )
            });
        &self
            .layout_state
            .get(Self::key_for_id(fiber_id))
            .unwrap_or_else(|| panic!("missing layout state for node {node_id:?}"))
            .taffy_style
    }

    fn set_unrounded_layout(&mut self, node_id: NodeId, layout: &Layout) {
        let island_root = self
            .layout_context
            .as_ref()
            .map(|ctx| ctx.current_island_root)
            .unwrap_or(node_id);
        if let Some(layout_state) = self.layout_state.get_mut(Self::key_for_id(node_id)) {
            layout_state.set_unrounded_layout_for_island(island_root, *layout);
        }
    }

    fn compute_child_layout(&mut self, node_id: NodeId, inputs: LayoutInput) -> LayoutOutput {
        if inputs.run_mode == RunMode::PerformHiddenLayout {
            return compute_hidden_layout(self, node_id);
        }

        compute_cached_layout(self, node_id, inputs, |tree, node_id, inputs| {
            if let Some(ctx) = tree.layout_context.as_mut() {
                ctx.layout_calls += 1;
            }
            if std::env::var("GPUI_DEBUG_LAYOUT").is_ok() {
                let key = Self::key_for_id(node_id);
                if let Some(fiber) = tree.fibers.get(key) {
                    log::info!("layout cache miss: {:?}", fiber.key);
                }
            }
            if let Some(ctx) = tree.layout_context.as_mut()
                && let Some(debug) = ctx.layout_cache_debug.as_mut()
            {
                let key = Self::key_for_id(node_id);
                let label = tree
                    .fibers
                    .get(key)
                    .map(|fiber| format!("{:?}", fiber.key))
                    .unwrap_or_else(|| "<missing>".to_string());
                *debug.misses_by_key.entry(label).or_insert(0) += 1;
            }
            let (display_mode, has_children, child_count) = {
                let display_mode = tree.get_core_container_style(node_id).display;
                let count = tree.child_count(node_id);
                (display_mode, count > 0, count)
            };

            log::trace!(
                "[LAYOUT_CHILD] node_id={:?} display_mode={:?} child_count={} has_children={}",
                node_id,
                display_mode,
                child_count,
                has_children
            );

            match (display_mode, has_children) {
                (Display::None, _) => compute_hidden_layout(tree, node_id),
                (Display::Block, true) => compute_block_layout(tree, node_id, inputs),
                (Display::Flex, true) => compute_flexbox_layout(tree, node_id, inputs),
                (Display::Grid, true) => compute_grid_layout(tree, node_id, inputs),
                (_, false) => {
                    // Leaf node measurement.
                    // We use take/put for render nodes to avoid aliasing issues.
                    let (window_ptr, app_ptr, scale_factor) = {
                        let context = tree
                            .layout_context
                            .as_ref()
                            .expect("layout context is not set");
                        (context.window, context.app, context.scale_factor)
                    };

                    // Take the render node out so we can pass it to the measure closure
                    // without aliasing the tree. This allows node.measure() to call Window APIs.
                    let mut render_node = tree.render_nodes.remove(Self::key_for_id(node_id));

                    let style_ptr = {
                        let layout_state = tree
                            .layout_state
                            .get_mut(Self::key_for_id(node_id))
                            .unwrap_or_else(|| panic!("missing layout state for node {node_id:?}"));
                        &layout_state.taffy_style as *const TaffyStyle
                    };
                    let intrinsic_ptr = tree
                        .layout_state
                        .get(Self::key_for_id(node_id))
                        .and_then(|state| state.intrinsic_size.as_ref())
                        .map(|cached| cached as *const crate::CachedIntrinsicSize);
                    let measure_ptr = tree
                        .measure_funcs
                        .get_mut(Self::key_for_id(node_id))
                        .map(|data| &mut data.measure_func as *mut MeasureFunc);

                    let style = unsafe { &*style_ptr };
                    let measure_fn = |known_dimensions: ::taffy::geometry::Size<Option<f32>>,
                                      available_space: ::taffy::geometry::Size<
                        TaffyAvailableSpace,
                    >| {
                        let to_available_space = |space: TaffyAvailableSpace| match space {
                            TaffyAvailableSpace::Definite(value) => {
                                AvailableSpace::Definite(Pixels(value / scale_factor))
                            }
                            TaffyAvailableSpace::MinContent => AvailableSpace::MinContent,
                            TaffyAvailableSpace::MaxContent => AvailableSpace::MaxContent,
                        };

                        let known = Size {
                            width: known_dimensions
                                .width
                                .map(|value| Pixels(value / scale_factor)),
                            height: known_dimensions
                                .height
                                .map(|value| Pixels(value / scale_factor)),
                        };
                        let avail = Size {
                            width: to_available_space(available_space.width),
                            height: to_available_space(available_space.height),
                        };

                        // Fast path: resolve size from cached intrinsic size.
                        if let Some(intrinsic_ptr) = intrinsic_ptr {
                            let cached = unsafe { &*intrinsic_ptr };
                            let query = crate::SizeQuery::from_taffy(known, avail);

                            // SAFETY: window_ptr and app_ptr are valid for the duration of layout.
                            let window = unsafe { &mut *window_ptr };
                            let cx = unsafe { &mut *app_ptr };
                            let mut sizing_ctx = crate::SizingCtx {
                                fiber_id: node_id,
                                rem_size: window.rem_size(),
                                scale_factor,
                                window,
                                cx,
                            };

                            let size = if let Some(ref mut node) = render_node {
                                node.resolve_size_query(query, &cached.size, &mut sizing_ctx)
                            } else {
                                match query {
                                    crate::SizeQuery::MinContent => cached.size.min_content,
                                    _ => cached.size.max_content,
                                }
                            };

                            return ::taffy::geometry::Size {
                                width: size.width.0 * scale_factor,
                                height: size.height.0 * scale_factor,
                            };
                        }

                        // Slow path: no cached intrinsic size available yet.
                        // Fall back to render-node or legacy measurement.
                        if let Some(ref mut node) = render_node {
                            let measured =
                                node.measure(known, avail, unsafe { &mut *window_ptr }, unsafe {
                                    &mut *app_ptr
                                });
                            if let Some(size) = measured {
                                return ::taffy::geometry::Size {
                                    width: size.width.0 * scale_factor,
                                    height: size.height.0 * scale_factor,
                                };
                            }
                        }

                        let Some(measure_ptr) = measure_ptr else {
                            let has_render_node = render_node.is_some();

                            // Fiber-native nodes (with render nodes) must have cached intrinsic sizes.
                            if has_render_node {
                                let dirty = tree.dirty.get(Self::key_for_id(node_id)).copied();
                                let has_layout_state =
                                    tree.layout_state.contains_key(Self::key_for_id(node_id));

                                // Get element key info for debugging
                                let element_key = tree
                                    .fibers
                                    .get(Self::key_for_id(node_id))
                                    .map(|fiber| format!("{:?}", fiber.key))
                                    .unwrap_or_else(|| "<missing>".to_string());

                                // Get render node type if present
                                let render_node_type = render_node
                                    .as_ref()
                                    .map(|n| n.debug_type_name())
                                    .unwrap_or("<none>");

                                // Get the fiber's own island root
                                let fiber_island = tree
                                    .outer_island_root
                                    .get(Self::key_for_id(node_id))
                                    .copied();

                                // Get parent info
                                let parent_id =
                                    tree.parents.get(Self::key_for_id(node_id)).and_then(|p| *p);
                                let parent_key = parent_id.and_then(|pid| {
                                    tree.fibers
                                        .get(Self::key_for_id(pid))
                                        .map(|fiber| format!("{:?}", fiber.key))
                                });
                                let parent_render_node_type = parent_id.and_then(|pid| {
                                    tree.render_nodes
                                        .get(Self::key_for_id(pid))
                                        .map(|n| n.debug_type_name())
                                });

                                // Check if fiber has intrinsic size cached
                                let has_intrinsic_cache = tree
                                    .layout_state
                                    .get(Self::key_for_id(node_id))
                                    .is_some_and(|ls| ls.intrinsic_size.is_some());

                                // Check if the fiber's island was processed during sizing
                                let island_was_processed = fiber_island
                                    .map(|island| {
                                        tree.sizing_pass_processed_islands.contains(&island)
                                    })
                                    .unwrap_or(false);

                                panic!(
                                    "measure_child_size called without cached intrinsic size for {:?} \
                                    (element_key={}, render_node_type={}, has_render_node={}, has_layout_state={}, \
                                    has_intrinsic_cache={}, dirty={:?}, fiber_island={:?}, island_was_processed={}, \
                                    current_island={:?}, parent={:?}, parent_key={:?}, parent_render_node={:?})",
                                    node_id,
                                    element_key,
                                    render_node_type,
                                    has_render_node,
                                    has_layout_state,
                                    has_intrinsic_cache,
                                    dirty,
                                    fiber_island,
                                    island_was_processed,
                                    tree.layout_context
                                        .as_ref()
                                        .map(|ctx| ctx.current_island_root),
                                    parent_id,
                                    parent_key,
                                    parent_render_node_type,
                                );
                            }

                            // Leaf without a measure function - return 0x0 as fallback.
                            return ::taffy::geometry::Size::ZERO;
                        };

                        let measured = unsafe {
                            (&mut *measure_ptr)(known, avail, &mut *window_ptr, &mut *app_ptr)
                        };

                        ::taffy::geometry::Size {
                            width: measured.width.0 * scale_factor,
                            height: measured.height.0 * scale_factor,
                        }
                    };

                    let resolve_calc_value = |val, basis| tree.resolve_calc_value(val, basis);
                    let result = compute_leaf_layout(inputs, style, resolve_calc_value, measure_fn);

                    // Put the render node back
                    if let Some(node) = render_node {
                        tree.render_nodes.insert(Self::key_for_id(node_id), node);
                    }

                    result
                }
            }
        })
    }
}

impl CacheTree for FiberTree {
    fn cache_get(
        &self,
        node_id: NodeId,
        known_dimensions: ::taffy::geometry::Size<Option<f32>>,
        available_space: ::taffy::geometry::Size<TaffyAvailableSpace>,
        run_mode: RunMode,
    ) -> Option<LayoutOutput> {
        let island_root = self
            .layout_context
            .as_ref()
            .map(|ctx| ctx.current_island_root)?;
        self.layout_state
            .get(Self::key_for_id(node_id))?
            .taffy_cache_for_island(island_root)?
            .get(known_dimensions, available_space, run_mode)
    }

    fn cache_store(
        &mut self,
        node_id: NodeId,
        known_dimensions: ::taffy::geometry::Size<Option<f32>>,
        available_space: ::taffy::geometry::Size<TaffyAvailableSpace>,
        run_mode: RunMode,
        output: LayoutOutput,
    ) {
        let island_root = self
            .layout_context
            .as_ref()
            .map(|ctx| ctx.current_island_root)
            .unwrap_or(node_id);
        if let Some(layout_state) = self.layout_state.get_mut(Self::key_for_id(node_id)) {
            layout_state.taffy_cache_for_island_mut(island_root).store(
                known_dimensions,
                available_space,
                run_mode,
                output,
            );
        }
    }

    fn cache_clear(&mut self, node_id: NodeId) {
        let island_root = self
            .layout_context
            .as_ref()
            .map(|ctx| ctx.current_island_root)
            .unwrap_or(node_id);
        if let Some(layout_state) = self.layout_state.get_mut(Self::key_for_id(node_id)) {
            layout_state.taffy_cache_for_island_mut(island_root).clear();
        }
    }
}

impl LayoutBlockContainer for FiberTree {
    type BlockContainerStyle<'a>
        = &'a TaffyStyle
    where
        Self: 'a;
    type BlockItemStyle<'a>
        = &'a TaffyStyle
    where
        Self: 'a;

    fn get_block_container_style(&self, node_id: NodeId) -> Self::BlockContainerStyle<'_> {
        self.get_core_container_style(node_id)
    }

    fn get_block_child_style(&self, child_node_id: NodeId) -> Self::BlockItemStyle<'_> {
        self.get_core_container_style(child_node_id)
    }
}

impl LayoutFlexboxContainer for FiberTree {
    type FlexboxContainerStyle<'a>
        = &'a TaffyStyle
    where
        Self: 'a;
    type FlexboxItemStyle<'a>
        = &'a TaffyStyle
    where
        Self: 'a;

    fn get_flexbox_container_style(&self, node_id: NodeId) -> Self::FlexboxContainerStyle<'_> {
        self.get_core_container_style(node_id)
    }

    fn get_flexbox_child_style(&self, child_node_id: NodeId) -> Self::FlexboxItemStyle<'_> {
        self.get_core_container_style(child_node_id)
    }
}

impl LayoutGridContainer for FiberTree {
    type GridContainerStyle<'a>
        = &'a TaffyStyle
    where
        Self: 'a;
    type GridItemStyle<'a>
        = &'a TaffyStyle
    where
        Self: 'a;

    fn get_grid_container_style(&self, node_id: NodeId) -> Self::GridContainerStyle<'_> {
        self.get_core_container_style(node_id)
    }

    fn get_grid_child_style(&self, child_node_id: NodeId) -> Self::GridItemStyle<'_> {
        self.get_core_container_style(child_node_id)
    }
}

impl RoundTree for FiberTree {
    fn get_unrounded_layout(&self, node_id: NodeId) -> Layout {
        let island_root = self
            .layout_context
            .as_ref()
            .map(|ctx| ctx.current_island_root)
            .unwrap_or(node_id);
        self.layout_state
            .get(Self::key_for_id(node_id))
            .unwrap_or_else(|| panic!("missing layout state for node {node_id:?}"))
            .unrounded_layout_for_island(island_root)
    }

    fn set_final_layout(&mut self, node_id: NodeId, layout: &Layout) {
        let island_root = self
            .layout_context
            .as_ref()
            .map(|ctx| ctx.current_island_root)
            .unwrap_or(node_id);
        if let Some(layout_state) = self.layout_state.get_mut(Self::key_for_id(node_id)) {
            layout_state.set_final_layout_for_island(island_root, *layout);
        }
    }
}

pub(crate) struct ActiveFiberList {
    pub(crate) members: FxHashSet<GlobalElementId>,
    pub(crate) ordered: Vec<GlobalElementId>,
    dirty: bool,
    last_structure_epoch: u64,
}

impl ActiveFiberList {
    pub(crate) fn new() -> Self {
        Self {
            members: FxHashSet::default(),
            ordered: Vec::new(),
            dirty: true,
            last_structure_epoch: 0,
        }
    }

    pub(crate) fn insert(&mut self, fiber_id: GlobalElementId) {
        if self.members.insert(fiber_id) {
            self.dirty = true;
        }
    }

    pub(crate) fn remove(&mut self, fiber_id: &GlobalElementId) {
        if self.members.remove(fiber_id) {
            self.dirty = true;
        }
    }

    pub(crate) fn rebuild_if_needed(&mut self, tree: &mut FiberTree) {
        if !self.dirty && self.last_structure_epoch == tree.structure_epoch {
            return;
        }

        tree.ensure_preorder_indices();
        self.members.retain(|fiber_id| tree.get(fiber_id).is_some());

        self.ordered.clear();
        self.ordered.extend(self.members.iter().copied());
        self.ordered
            .sort_by_key(|fiber_id| tree.preorder_index(fiber_id).unwrap_or(u64::MAX));

        self.last_structure_epoch = tree.structure_epoch;
        self.dirty = false;
    }
}

pub(crate) fn has_mouse_effects(
    interactivity: Option<&Interactivity>,
    effects: Option<&FiberEffects>,
) -> bool {
    if effects.is_some_and(|effects| {
        !effects.any_mouse_listeners.is_empty()
            || !effects.mouse_down_listeners.is_empty()
            || !effects.mouse_up_listeners.is_empty()
            || !effects.mouse_move_listeners.is_empty()
            || !effects.mouse_pressure_listeners.is_empty()
            || !effects.scroll_wheel_listeners.is_empty()
            || !effects.click_listeners.is_empty()
            || effects.drag_listener.is_some()
            || !effects.drop_listeners.is_empty()
            || effects.can_drop_predicate.is_some()
            || effects.hover_listener.is_some()
            || effects.tooltip.is_some()
            || effects.cursor_style.is_some()
    }) {
        return true;
    }

    interactivity.is_some_and(|interactivity| {
        interactivity.hover_style.is_some()
            || interactivity.group_hover_style.is_some()
            || interactivity.active_style.is_some()
            || interactivity.group_active_style.is_some()
            || !interactivity.drag_over_styles.is_empty()
            || !interactivity.group_drag_over_styles.is_empty()
            || interactivity.tracked_focus_handle.is_some()
            || interactivity.base_style.mouse_cursor.is_some()
            || interactivity.tooltip_builder.is_some()
    })
}

#[cfg(debug_assertions)]
pub(crate) fn debug_assert_active_list_matches_map<T>(
    list_name: &str,
    list: &ActiveFiberList,
    map: &slotmap::SecondaryMap<DefaultKey, T>,
) {
    for fiber_id in list.members.iter() {
        debug_assert!(
            map.contains_key((*fiber_id).into()),
            "active list {list_name} contains stale fiber {fiber_id:?}"
        );
    }
    for (key, _) in map.iter() {
        let fiber_id = GlobalElementId::from(key);
        debug_assert!(
            list.members.contains(&fiber_id),
            "map {list_name} contains fiber {fiber_id:?} missing from active list"
        );
    }
}

pub(crate) fn path_to_root_smallvec(
    tree: &FiberTree,
    id: GlobalElementId,
) -> SmallVec<[GlobalElementId; 32]> {
    let mut path = SmallVec::new();
    let mut current = Some(id);
    while let Some(node_id) = current {
        path.push(node_id);
        current = tree.parent(&node_id);
    }
    path.reverse();
    path
}

pub(crate) struct FiberRuntime {
    pub(crate) tree: FiberTree,
    pub(crate) frame_number: u64,
    pub(crate) fiber_id_stack: Vec<GlobalElementId>,
    pub(crate) hitbox_stack: Vec<HitboxId>,
    pub(crate) rendered_tab_stops: TabStopMap,
    pub(crate) layout_bounds_cache: FxHashMap<GlobalElementId, Bounds<Pixels>>,
    pub(crate) active_tooltips: ActiveFiberList,
    pub(crate) active_cursor_styles: ActiveFiberList,
    pub(crate) active_deferred_draws: ActiveFiberList,
    pub(crate) active_input_handlers: ActiveFiberList,
    pub(crate) active_mouse_listeners: ActiveFiberList,
    /// Fiber root for the tooltip overlay (at most one tooltip is shown at a time).
    pub(crate) tooltip_overlay_root: Option<GlobalElementId>,
    /// Fiber root for the prompt overlay (modal dialog).
    pub(crate) prompt_overlay_root: Option<GlobalElementId>,
    /// Fiber root for the active drag overlay.
    pub(crate) drag_overlay_root: Option<GlobalElementId>,
    /// Current layout island root during setup phase.
    /// Set during setup_taffy_from_fibers to allow dynamically-created
    /// fiber children to be assigned to the correct layout island.
    pub(crate) setup_island_root: Option<GlobalElementId>,
    /// Queued layout events to dispatch after all prepaint passes.
    pub(crate) pending_layout_events: Vec<(GlobalElementId, Bounds<Pixels>)>,
}

impl FiberRuntime {
    pub(crate) fn new() -> Self {
        Self {
            tree: FiberTree::new(),
            frame_number: 0,
            fiber_id_stack: Vec::new(),
            hitbox_stack: Vec::new(),
            rendered_tab_stops: TabStopMap::default(),
            layout_bounds_cache: FxHashMap::default(),
            active_tooltips: ActiveFiberList::new(),
            active_cursor_styles: ActiveFiberList::new(),
            active_deferred_draws: ActiveFiberList::new(),
            active_input_handlers: ActiveFiberList::new(),
            active_mouse_listeners: ActiveFiberList::new(),
            tooltip_overlay_root: None,
            prompt_overlay_root: None,
            drag_overlay_root: None,
            setup_island_root: None,
            pending_layout_events: Vec::new(),
        }
    }

    pub(crate) fn log_frame_diagnostics(&self) {
        if !*FRAME_DIAGNOSTICS_ENABLED {
            return;
        }
        if !log::log_enabled!(log::Level::Info) {
            return;
        }

        let total_fibers = self.tree.fibers.len();
        let island_count = self.tree.layout_island_roots.len();

        let mut scope_children_total = 0usize;
        for children in self.tree.scope_children.values() {
            scope_children_total = scope_children_total.saturating_add(children.len());
        }

        let mut empty_normal = 0usize;
        let mut empty_scope = 0usize;
        let mut empty_detached_root = 0usize;
        let mut real_detached = 0usize;

        for (key, parent) in self.tree.parents.iter() {
            if parent.is_some() || !self.tree.fibers.contains_key(key) {
                continue;
            }

            let has_render_node = self.tree.render_nodes.contains_key(key);
            let has_children = self
                .tree
                .children
                .get(key)
                .is_some_and(|children| !children.is_empty())
                || self
                    .tree
                    .node_children
                    .get(key)
                    .is_some_and(|children| !children.is_empty());
            let has_scope_children = self
                .tree
                .scope_children
                .get(key)
                .is_some_and(|children| !children.is_empty());

            if has_render_node || has_children || has_scope_children {
                real_detached = real_detached.saturating_add(1);
            } else {
                let kind = self
                    .tree
                    .fibers
                    .get(key)
                    .map(|f| f.kind)
                    .unwrap_or(FiberKind::Normal);
                match kind {
                    FiberKind::Normal => empty_normal = empty_normal.saturating_add(1),
                    FiberKind::StateOnly => empty_scope = empty_scope.saturating_add(1),
                    FiberKind::DetachedRoot => {
                        empty_detached_root = empty_detached_root.saturating_add(1)
                    }
                }
            }
        }

        let tree_ptr = &self.tree as *const _ as usize;
        log::info!(
            "[FIBER_DIAG] tree={:#x} frame={} fibers={} islands={} element_states={} scope_children={} detached: real={} empty(normal={} scope={} root={})",
            tree_ptr,
            self.tree.current_frame,
            total_fibers,
            island_count,
            self.tree.element_states.len(),
            scope_children_total,
            real_detached,
            empty_normal,
            empty_scope,
            empty_detached_root
        );
    }
}

pub(crate) struct FiberRuntimeHandle<'a> {
    pub(crate) window: &'a mut Window,
}

pub(crate) struct FiberRuntimeHandleRef<'a> {
    pub(crate) window: &'a Window,
}

#[derive(Clone, Copy)]
struct TraversalCtx<'a> {
    handle: std::ptr::NonNull<FiberRuntimeHandle<'a>>,
    app: std::ptr::NonNull<App>,
}

impl<'a> TraversalCtx<'a> {
    fn new(handle: &mut FiberRuntimeHandle<'a>, app: &mut App) -> Self {
        Self {
            handle: std::ptr::NonNull::from(handle),
            app: std::ptr::NonNull::from(app),
        }
    }

    fn with_mut<R>(mut self, f: impl FnOnce(&mut FiberRuntimeHandle<'a>, &mut App) -> R) -> R {
        // Safety: `TraversalCtx` is constructed from valid `&mut` references and only used
        // within the traversal scope that owns those references.
        unsafe { f(self.handle.as_mut(), self.app.as_mut()) }
    }

    fn handle_mut(self) -> &'a mut FiberRuntimeHandle<'a> {
        // Safety: `TraversalCtx` is constructed from a unique `&mut FiberRuntimeHandle` and used
        // only within the traversal scope; callers must not hold multiple mutable borrows
        // simultaneously.
        unsafe {
            self.handle
                .as_ptr()
                .as_mut()
                .expect("TraversalCtx handle is null")
        }
    }

    fn app_mut(self) -> &'a mut App {
        // Safety: `TraversalCtx` is constructed from a unique `&mut App` and used only within the
        // traversal scope; callers must not hold multiple mutable borrows simultaneously.
        unsafe {
            self.app
                .as_ptr()
                .as_mut()
                .expect("TraversalCtx app is null")
        }
    }
}

#[derive(Clone)]
struct FiberInteractivityInfo {
    hover_style: bool,
    group_hover: Option<SharedString>,
    group_active: Option<SharedString>,
    drag_over_styles: bool,
    focus_handle: Option<FocusHandle>,
    tooltip_id: Option<TooltipId>,
}

#[derive(Clone)]
struct FiberDispatchTarget {
    fiber_id: GlobalElementId,
    hitbox: Hitbox,
    interactivity: Option<FiberInteractivityInfo>,
}

pub(crate) enum TraversalDecision {
    Continue,
    SkipChildren,
    SkipSubtree,
}

pub(crate) fn traverse_fibers<State>(
    root: GlobalElementId,
    state: &mut State,
    mut enter: impl FnMut(&mut State, GlobalElementId) -> TraversalDecision,
    mut exit: impl FnMut(&mut State, GlobalElementId),
    mut children: impl FnMut(&mut State, GlobalElementId) -> SmallVec<[GlobalElementId; 8]>,
) {
    let mut stack: Vec<(GlobalElementId, bool)> = vec![(root, true)];
    while let Some((fiber_id, entering)) = stack.pop() {
        if entering {
            match enter(state, fiber_id) {
                TraversalDecision::Continue => {
                    stack.push((fiber_id, false));
                    for child_id in children(state, fiber_id).into_iter().rev() {
                        stack.push((child_id, true));
                    }
                }
                TraversalDecision::SkipChildren => {
                    stack.push((fiber_id, false));
                }
                TraversalDecision::SkipSubtree => {}
            }
        } else {
            exit(state, fiber_id);
        }
    }
}

impl FiberRuntimeHandleRef<'_> {
    pub(crate) fn current_fiber_id(&self) -> Option<GlobalElementId> {
        self.window.fiber.fiber_id_stack.last().cloned()
    }

    pub(super) fn should_render_view_fiber(&self, fiber_id: &GlobalElementId) -> bool {
        if self.window.fiber.tree.get(fiber_id).is_none() {
            return false;
        };
        let dirty = self.window.fiber.tree.dirty_flags(fiber_id);
        let has_prepaint_dirty_ancestor =
            self.window.fiber.tree.has_prepaint_dirty_ancestor(fiber_id);
        let has_cached_output = self.window.fiber.tree.has_cached_output(fiber_id);
        let is_clean =
            dirty.is_subtree_clean() && has_cached_output && !has_prepaint_dirty_ancestor;
        let has_cached_child = self
            .window
            .fiber
            .tree
            .view_state
            .get((*fiber_id).into())
            .and_then(|state| state.view_data.as_ref())
            .map(|view| view.has_cached_child)
            .unwrap_or(false);
        !is_clean || !has_cached_child
    }
}

impl FiberRuntimeHandle<'_> {
    fn child_ids(&self, fiber_id: &GlobalElementId) -> SmallVec<[GlobalElementId; 8]> {
        self.window.fiber.tree.children(fiber_id).collect()
    }

    pub(crate) fn with_current_element<R>(
        &mut self,
        fiber_id: GlobalElementId,
        f: impl FnOnce(&mut Window) -> R,
    ) -> R {
        let stack_state = StackState::capture(self.window);
        let needs_push = self.window.current_fiber_id() != Some(fiber_id);
        if needs_push {
            self.push_fiber_id(fiber_id);
        }

        let result = f(self.window);

        stack_state.restore(self.window);
        result
    }

    pub(crate) fn push_fiber_id(&mut self, id: GlobalElementId) {
        #[cfg(debug_assertions)]
        {
            if self.window.fiber.fiber_id_stack.last() == Some(&id) {
                #[cfg(test)]
                DUPLICATE_FIBER_ID_PUSH_COUNT.with(|count| {
                    count.set(count.get().saturating_add(1));
                });
                log::warn!(
                    "Pushing duplicate fiber_id {:?} - possible double-push bug",
                    id
                );
            }
        }
        self.window.fiber.fiber_id_stack.push(id);
    }

    pub(crate) fn pop_fiber_id(&mut self) {
        self.window.fiber.fiber_id_stack.pop();
    }

    /// Ensure a fiber exists for the current fiber scope, creating one if necessary.
    pub(crate) fn ensure_fiber_for_current_id(&mut self) -> GlobalElementId {
        if let Some(id) = self.window.current_fiber_id() {
            if self.window.fiber.tree.get(&id).is_some() {
                return id;
            }
        }
        self.window.fiber.tree.create_placeholder_fiber()
    }

    /// Register a view's entity ID with the current fiber.
    /// This enables view-level dirty tracking.
    /// Ensure a pending fiber exists for a view root outside of render traversal.
    pub(crate) fn ensure_view_root_fiber(&mut self, view_id: EntityId) -> GlobalElementId {
        self.ensure_root_fiber_for_view(view_id)
    }

    fn ensure_root_fiber_for_view(&mut self, view_id: EntityId) -> GlobalElementId {
        let fiber_id = self.window.fiber.tree.ensure_pending_view_fiber(view_id);
        if self.window.fiber.tree.root.is_none() {
            self.window.fiber.tree.root = Some(fiber_id);
        }
        fiber_id
    }

    pub(crate) fn mark_view_dirty(&mut self, view_id: EntityId) {
        if let Some(fiber_id) = self.window.fiber.tree.view_roots.get(&view_id).copied() {
            // Mark both NEEDS_LAYOUT and NEEDS_PAINT because view notifications can affect both:
            // - Layout: image loading, text content changes, etc.
            // - Paint: color changes, state updates, etc.
            // The layout phase calls RenderNode::layout_begin which is needed for images to
            // fetch their data via use_asset.
            self.window.fiber.tree.mark_dirty(
                &fiber_id,
                DirtyFlags::NEEDS_LAYOUT | DirtyFlags::NEEDS_PAINT,
            );
            return;
        }

        if self
            .window
            .root
            .as_ref()
            .is_some_and(|root| root.entity_id() == view_id)
        {
            let fiber_id = self.ensure_root_fiber_for_view(view_id);
            self.window.fiber.tree.mark_dirty(
                &fiber_id,
                DirtyFlags::NEEDS_LAYOUT | DirtyFlags::NEEDS_PAINT,
            );
            return;
        }

        // Fallback: view not found in view_roots and not root, mark root fiber dirty
        if let Some(root_id) = self.window.fiber.tree.root {
            self.window
                .fiber
                .tree
                .mark_dirty(&root_id, DirtyFlags::NEEDS_LAYOUT | DirtyFlags::NEEDS_PAINT);
        }
    }

    pub(super) fn map_view_roots_from_element(
        &mut self,
        fiber_id: &GlobalElementId,
        element: &AnyElement,
        new_view_fibers: &mut Vec<GlobalElementId>,
    ) {
        // Check if this is a view element
        if let Some(any_view) = element.as_any_view() {
            let entity_id = any_view.entity_id();
            let is_new = !self.window.fiber.tree.view_roots.contains_key(&entity_id);
            self.window.fiber.tree.set_view_root(entity_id, *fiber_id);
            if is_new {
                new_view_fibers.push(*fiber_id);

                // Set view_data for newly discovered view elements so they can be rendered
                // in expand_view_fibers. Without this, the view would have no AnyView
                // to call render_element() on.
                let slot_key = FiberTree::key_for_id(*fiber_id);
                if let Some(view_state) = self.window.fiber.tree.view_state.get_mut(slot_key) {
                    view_state.view_data = Some(ViewData::new(any_view));
                }
            }
        }

        // Recursively process children
        let child_ids = self.child_ids(fiber_id);
        for (child_id, child_elem) in child_ids.into_iter().zip(element.children()) {
            self.map_view_roots_from_element(&child_id, child_elem, new_view_fibers);
        }
    }

    pub(crate) fn expand_view_fibers(&mut self, _root_fiber: GlobalElementId, cx: &mut App) {
        #[cfg(debug_assertions)]
        debug_assert!(
            self.window.invalidator.phase() == DrawPhase::Reconcile,
            "expand_view_fibers must only be called during the Reconcile phase, but was called during {:?}",
            self.window.invalidator.phase()
        );

        let mut queue: Vec<GlobalElementId> = self
            .window
            .fiber
            .tree
            .view_roots
            .values()
            .cloned()
            .collect();
        let mut visited = FxHashSet::default();
        let mut new_view_fibers = Vec::new();

        while let Some(fiber_id) = queue.pop() {
            if !visited.insert(fiber_id) {
                continue;
            }
            let view_state = self.window.fiber.tree.view_state.get(fiber_id.into());
            let Some(view_data) = view_state.and_then(|state| state.view_data.clone()) else {
                continue;
            };

            let should_render = self.window.should_render_view_fiber(&fiber_id);
            if !should_render {
                continue;
            }

            let (mut element, accessed) = cx.detect_accessed_entities(|cx| {
                self.with_current_element(fiber_id, |window| {
                    window.with_rendered_view(view_data.entity_id(), |window| {
                        view_data.view.render_element(window, cx)
                    })
                })
            });
            self.window
                .record_pending_view_accesses(&fiber_id, accessed);

            self.window.hydrate_view_children(&mut element);
            if let Some(view_state) = self.window.fiber.tree.view_state.get_mut(fiber_id.into()) {
                let mut updated_view_data = view_data.clone();
                updated_view_data.has_cached_child = true;
                view_state.view_data = Some(updated_view_data);
            }

            // Expand wrapper elements (like Component) BEFORE reconciliation
            // so the real children/keys participate in normal reconciliation.
            element.expand_wrappers(self.window, cx);

            let output_view_id = element.as_any_view().map(|v| v.entity_id());
            let parent_view_id = view_data.entity_id();

            // If the view's render output is directly another view element (different entity),
            // treat this view fiber as an identity-only wrapper and reconcile the returned view as
            // its single child. This avoids aliasing multiple view entities to one fiber.
            if let Some(child_view_id) = output_view_id
                && child_view_id != parent_view_id
            {
                let slot_key: DefaultKey = fiber_id.into();

                // Avoid self-parent cycles if a stale view_roots entry aliases this fiber.
                if self
                    .window
                    .fiber
                    .tree
                    .view_roots
                    .get(&child_view_id)
                    .is_some_and(|existing| *existing == fiber_id)
                {
                    self.window.fiber.tree.view_roots.remove(&child_view_id);
                }

                // Switch this fiber into wrapper mode (layout-transparent via child_ids flattening).
                let _ = self
                    .window
                    .fiber
                    .tree
                    .set_is_view_wrapper(&fiber_id, true);

                // Ensure wrapper fibers have a render node so prepaint/paint traversals don't
                // skip their children.
                self.window.fiber.tree.render_nodes.insert(
                    slot_key,
                    Box::new(crate::view::ViewNode::new(parent_view_id)),
                );

                // Wrapper fibers should not retain descriptor payloads from previous frames.
                if self.window.fiber.tree.layout_listeners.remove(slot_key).is_some() {
                    self.window.fiber.tree.previous_bounds.remove(slot_key);
                }
                if let Some(old_focus) = self.window.fiber.tree.focus_ids.remove(slot_key) {
                    self.window.fiber.tree.focusable_fibers.remove(&old_focus);
                }
                if let Some(node_children) = self.window.fiber.tree.node_children.get_mut(slot_key)
                {
                    node_children.clear();
                }

                if let Some(fiber) = self.window.fiber.tree.get_mut(&fiber_id) {
                    fiber.child_count = 1;
                }

                // Reconcile the returned view element as the wrapper's single child.
                self.window
                    .fiber
                    .tree
                    .reconcile_children(&fiber_id, std::slice::from_ref(&element), false);

                let child_fiber_id = self
                    .window
                    .fiber
                    .tree
                    .children_slice(&fiber_id)
                    .first()
                    .copied();
                if let Some(child_fiber_id) = child_fiber_id {
                    // Cache payloads for the child view element so it can render in this pass.
                    self.cache_fiber_payloads(&child_fiber_id, &mut element, cx);

                    // Ensure the child view is expanded this pass even if it was previously
                    // visited before its view_data was available.
                    visited.remove(&child_fiber_id);
                    queue.push(child_fiber_id);
                } else {
                    self.window.fiber.tree.mark_dirty(
                        &fiber_id,
                        DirtyFlags::STRUCTURE_CHANGED | DirtyFlags::NEEDS_PAINT,
                    );
                }
                continue;
            }

            // If this fiber was previously acting as a wrapper for a child view, clear that
            // mode now so it participates normally in layout.
            let was_wrapper = self.window.fiber.tree.is_view_wrapper(&fiber_id);
            if was_wrapper {
                let _ = self
                    .window
                    .fiber
                    .tree
                    .set_is_view_wrapper(&fiber_id, false);
                // Remove the wrapper's render node so legacy output can fall back to
                // element-state rendering without being masked by a stale node.
                self.window.fiber.tree.render_nodes.remove(fiber_id.into());
            }

            if output_view_id == Some(parent_view_id) {
                log::error!(
                    "view {:?} rendered itself directly; treating output as Empty to avoid a fiber cycle",
                    parent_view_id
                );
                element = AnyElement::new(crate::Empty);
            }

            self.window
                .fiber
                .tree
                .reconcile_wrapper(&fiber_id, &element, false);
            new_view_fibers.clear();
            self.map_view_roots_from_element(&fiber_id, &element, &mut new_view_fibers);
            self.cache_fiber_payloads(&fiber_id, &mut element, cx);

            queue.extend(new_view_fibers.iter().cloned());
        }
    }

    fn take_element_object(
        element: &mut AnyElement,
    ) -> (Box<dyn crate::element::ElementObject>, &'static str) {
        let mut replacement = AnyElement::new(crate::Empty);
        std::mem::swap(&mut replacement.inner, &mut element.inner);
        let type_name = replacement.inner.element_type_name();
        (replacement.inner, type_name)
    }

    /// Register or clear a fiber's focus handle in the focusable_fibers map.
    ///
    /// Called after extracting focus info from a fiber's render node.
    fn register_focus(
        &mut self,
        focus_update: Option<(crate::FocusHandle, GlobalElementId, Option<crate::FocusId>)>,
        clear_focus: Option<crate::FocusId>,
    ) {
        if let Some(old_focus) = clear_focus {
            self.window.fiber.tree.focusable_fibers.remove(&old_focus);
        }
        if let Some((focus_handle, fiber_id, old_focus)) = focus_update {
            if let Some(old_focus) = old_focus {
                if old_focus != focus_handle.id {
                    self.window.fiber.tree.focusable_fibers.remove(&old_focus);
                }
            }
            if let Some(existing) = self
                .window
                .fiber
                .tree
                .focusable_fibers
                .insert(focus_handle.id, fiber_id)
                && existing != fiber_id
            {
                self.window.fiber.tree.focus_ids.remove(existing.into());
            }
        }
    }

    /// Synchronize a fiber's render node with the element.
    ///
    /// This clears payloads, sets z_index, creates/updates the render node,
    /// and marks dirty flags as needed. Returns whether the fiber has a render node.
    fn sync_render_node(
        &mut self,
        fiber_id: &GlobalElementId,
        element: &mut AnyElement,
        cx: &mut App,
    ) -> bool {
        let z_index = element.modifiers().z_index;
        let slot_key = FiberTree::key_for_id(*fiber_id);

        // Extract layout listener from element before it's consumed by create/update_render_node.
        // This handles both updates (new listener replaces old) and removals (no listener clears old).
        let layout_listener = element.take_layout_listener();
        match layout_listener {
            Some(listener) => {
                self.window
                    .fiber
                    .tree
                    .layout_listeners
                    .insert(slot_key, listener);
            }
            None => {
                // Listener removed - clear both listener and previous_bounds
                if self
                    .window
                    .fiber
                    .tree
                    .layout_listeners
                    .remove(slot_key)
                    .is_some()
                {
                    self.window.fiber.tree.previous_bounds.remove(slot_key);
                }
            }
        }

        if self.window.fiber.tree.fibers.get(slot_key).is_none() {
            return false;
        }

        // Step 1: Update z_index tracking
        let previous_priority = self
            .window
            .fiber
            .tree
            .deferred_priorities
            .get(slot_key)
            .copied();
        match z_index {
            Some(priority) => {
                self.window
                    .fiber
                    .tree
                    .deferred_priorities
                    .insert(slot_key, priority);
                if previous_priority != Some(priority) {
                    self.window
                        .fiber
                        .tree
                        .mark_dirty(fiber_id, DirtyFlags::NEEDS_PAINT);
                }
            }
            None => {
                if previous_priority.is_some() {
                    self.window
                        .fiber
                        .tree
                        .mark_dirty(fiber_id, DirtyFlags::NEEDS_PAINT);
                }
                self.window.fiber.tree.deferred_priorities.remove(slot_key);
            }
        }

        // Step 2: Create/update/clear retained render node based on the expected concrete type.
        let expected_node_type_id = element.render_node_type_id();
        let existing_node_type_id = self
            .window
            .fiber
            .tree
            .render_nodes
            .get(slot_key)
            .map(|node| node.node_type_id());

        match (expected_node_type_id, existing_node_type_id) {
            (None, _) => {
                // All elements should now be fiber-native with proper render nodes.
                // StateOnly fibers (e.g. placeholder fibers) legitimately have no render node.
                if self
                    .window
                    .fiber
                    .tree
                    .fibers
                    .get(slot_key)
                    .is_some_and(|fiber| fiber.kind == FiberKind::StateOnly)
                {
                    self.window.fiber.tree.render_nodes.remove(slot_key);
                    return false;
                }

                let (_, type_name) = Self::take_element_object(element);
                panic!(
                    "Element type '{}' does not implement render_node_type_id(). \
                     All elements must be fiber-native. Add #[derive(Element)] or implement \
                     render_node_type_id/create_render_node/update_render_node manually.",
                    type_name
                );
            }
            (Some(_expected), None) => {
                // No existing node, create a new one.
                let new_node = element.create_render_node();
                if let Some(new_node) = new_node {
                    self.window
                        .fiber
                        .tree
                        .render_nodes
                        .insert(slot_key, new_node);
                    // A new node must be laid out and painted.
                    self.window.fiber.tree.mark_dirty(
                        fiber_id,
                        DirtyFlags::STRUCTURE_CHANGED
                            | DirtyFlags::CONTENT_CHANGED
                            | DirtyFlags::NEEDS_PAINT,
                    );
                    true
                } else {
                    #[cfg(debug_assertions)]
                    {
                        panic!(
                            "element reported a retained node type but create_render_node returned None"
                        )
                    }
                    #[cfg(not(debug_assertions))]
                    {
                        false
                    }
                }
            }
            (Some(expected), Some(existing)) if expected == existing => {
                // Take the node out, update it, then put it back. We must avoid holding borrows on
                // the fiber tree while invoking element code.
                let mut node = self.window.fiber.tree.render_nodes.remove(slot_key);
                let update_result = node.as_mut().and_then(|existing_node| {
                    element.update_render_node(existing_node.as_mut(), self.window, cx)
                });
                if let Some(result) = update_result {
                    if let Some(node) = node {
                        self.window.fiber.tree.render_nodes.insert(slot_key, node);
                    }
                    if result.layout_changed {
                        self.window.fiber.tree.mark_content_changed(fiber_id);
                    }
                    if result.paint_changed {
                        self.window
                            .fiber
                            .tree
                            .mark_dirty(fiber_id, DirtyFlags::NEEDS_PAINT);
                    }
                    true
                } else {
                    // Treat as replace. This should not happen when types match, but keeping this
                    // fallback avoids crashing if an element implementation violates the contract.
                    let replacement = element.create_render_node();
                    if let Some(new_node) = replacement {
                        self.window
                            .fiber
                            .tree
                            .render_nodes
                            .insert(slot_key, new_node);
                        self.window.fiber.tree.mark_dirty(
                            fiber_id,
                            DirtyFlags::STRUCTURE_CHANGED
                                | DirtyFlags::CONTENT_CHANGED
                                | DirtyFlags::NEEDS_PAINT,
                        );
                        true
                    } else {
                        if let Some(node) = node {
                            self.window.fiber.tree.render_nodes.insert(slot_key, node);
                        }
                        false
                    }
                }
            }
            (Some(_expected), Some(_existing)) => {
                // Existing node type doesn't match expected, replace it without calling into the
                // element's update path.
                self.window.fiber.tree.render_nodes.remove(slot_key);
                let new_node = element.create_render_node();
                if let Some(new_node) = new_node {
                    self.window
                        .fiber
                        .tree
                        .render_nodes
                        .insert(slot_key, new_node);
                    self.window.fiber.tree.mark_dirty(
                        fiber_id,
                        DirtyFlags::STRUCTURE_CHANGED
                            | DirtyFlags::CONTENT_CHANGED
                            | DirtyFlags::NEEDS_PAINT,
                    );
                    true
                } else {
                    #[cfg(debug_assertions)]
                    {
                        panic!(
                            "element reported a retained node type but create_render_node returned None"
                        )
                    }
                    #[cfg(not(debug_assertions))]
                    {
                        false
                    }
                }
            }
        }
    }

    pub(crate) fn cache_fiber_payloads(
        &mut self,
        fiber_id: &GlobalElementId,
        element: &mut AnyElement,
        cx: &mut App,
    ) {
        #[cfg(debug_assertions)]
        debug_assert!(
            self.window.invalidator.phase() == DrawPhase::Reconcile,
            "cache_fiber_payloads must only be called during the Reconcile phase, but was called during {:?}",
            self.window.invalidator.phase()
        );

        // Note: Component expansion now happens BEFORE reconciliation via expand_wrappers(),
        // so the element here is already the final expanded element.

        // Handle View elements specially to preserve child references
        if let Some(any_view) = element.as_any_view() {
            let entity_id = any_view.entity_id();
            let existing_has_cached_child = self
                .window
                .fiber
                .tree
                .view_state
                .get((*fiber_id).into())
                .and_then(|state| state.view_data.as_ref())
                .map(|d| d.has_cached_child)
                .unwrap_or(false);

            if let Some(view_state) = self
                .window
                .fiber
                .tree
                .view_state
                .get_mut((*fiber_id).into())
            {
                view_state.view_data = Some(ViewData::new(any_view));

                // Preserve existing has_cached_child if new view_data doesn't have it set
                if let Some(view_data) = &mut view_state.view_data {
                    if !view_data.has_cached_child && existing_has_cached_child {
                        view_data.has_cached_child = true;
                    }
                }
            }

            let slot_key = FiberTree::key_for_id(*fiber_id);
            if let Some(fiber) = self.window.fiber.tree.fibers.get_mut(slot_key) {
                fiber.view_id = Some(entity_id);
            }
            if let Some(priority) = element.modifiers().z_index {
                self.window
                    .fiber
                    .tree
                    .deferred_priorities
                    .insert(slot_key, priority);
            } else {
                self.window.fiber.tree.deferred_priorities.remove(slot_key);
            }
            self.window.fiber.tree.set_view_root(entity_id, *fiber_id);
            // Views don't support on_layout; clear any stale listener from a previous element type
            if self
                .window
                .fiber
                .tree
                .layout_listeners
                .remove(slot_key)
                .is_some()
            {
                self.window.fiber.tree.previous_bounds.remove(slot_key);
            }
            return;
        }

        // Standard path for non-View elements
        self.sync_render_node(fiber_id, element, cx);

        let focus_handle = self
            .window
            .fiber
            .tree
            .render_nodes
            .get((*fiber_id).into())
            .and_then(|node| node.focus_handle());
        let mut focus_update = None;
        let mut clear_focus = None;

        let view_id = self
            .window
            .fiber
            .tree
            .view_state
            .get((*fiber_id).into())
            .and_then(|state| state.view_data.as_ref().map(|data| data.entity_id()));

        let slot_key: DefaultKey = (*fiber_id).into();
        if let Some(fiber) = self.window.fiber.tree.fibers.get_mut(slot_key) {
            let old_focus = self.window.fiber.tree.focus_ids.get(slot_key).copied();
            if let Some(focus_handle) = focus_handle {
                self.window
                    .fiber
                    .tree
                    .focus_ids
                    .insert(slot_key, focus_handle.id);
                focus_update = Some((focus_handle, *fiber_id, old_focus));
            } else {
                self.window.fiber.tree.focus_ids.remove(slot_key);
                clear_focus = old_focus;
            }

            if view_id.is_some() {
                fiber.view_id = view_id;
            } else if let Some(view_state) = self
                .window
                .fiber
                .tree
                .view_state
                .get_mut((*fiber_id).into())
            {
                view_state.view_descriptor_hash = 0;
            }
        }
        self.register_focus(focus_update, clear_focus);

        // Recursively process children
        let child_ids = self.child_ids(fiber_id);
        for (child_id, child_elem) in child_ids.into_iter().zip(element.children_mut().iter_mut()) {
            self.cache_fiber_payloads(&child_id, child_elem, cx);
        }
    }

    /// Measurement-mode variant of cache_fiber_payloads.
    ///
    /// This version is safe for transient measurement subtrees:
    /// - Does NOT touch focusable_fibers (no focus registration)
    /// - Does NOT set view_roots (measurement fibers shouldn't affect view lookup)
    /// - Still creates/updates render nodes (needed for RenderNode::measure)
    /// - Still stores legacy elements (for legacy measure paths)
    /// - Bails out if a View element is encountered (measurement shouldn't span views)
    pub(crate) fn cache_fiber_payloads_measurement(
        &mut self,
        fiber_id: &GlobalElementId,
        element: &mut AnyElement,
        cx: &mut App,
    ) {
        // Note: Component expansion now happens BEFORE reconciliation via expand_wrappers(),
        // so the element here is already the final expanded element.

        // Bail out for View elements - measurement subtrees shouldn't span view boundaries
        if element.as_any_view().is_some() {
            // For views in measurement, just clear payloads and return.
            // We don't register view roots or do any view-specific handling.
            if let Some(view_state) = self
                .window
                .fiber
                .tree
                .view_state
                .get_mut((*fiber_id).into())
            {
                *view_state = FiberViewState::default();
            }
            let slot_key: DefaultKey = (*fiber_id).into();
            if self.window.fiber.tree.fibers.get(slot_key).is_some() {
                if let Some(priority) = element.modifiers().z_index {
                    self.window
                        .fiber
                        .tree
                        .deferred_priorities
                        .insert(slot_key, priority);
                } else {
                    self.window.fiber.tree.deferred_priorities.remove(slot_key);
                }
            }
            return;
        }

        // Create/update retained render node
        self.sync_render_node(fiber_id, element, cx);
        // NOTE: We intentionally do NOT handle focus_id or focusable_fibers here.
        // Measurement subtrees should never affect global focus state.

        // Recursively process children
        let child_ids = self.child_ids(fiber_id);
        for (child_id, child_elem) in child_ids.into_iter().zip(element.children_mut().iter_mut()) {
            self.cache_fiber_payloads_measurement(&child_id, child_elem, cx);
        }
    }

    /// Overlay-mode variant of cache_fiber_payloads.
    ///
    /// Used for detached overlay subtrees (e.g. `Window::defer_draw`) where View elements
    /// should *not* be treated as a retained view boundary. In overlays, view elements
    /// must behave like normal/legacy elements so they can render correctly without
    /// participating in the window root's `view_roots` expansion pipeline.
    pub(crate) fn cache_fiber_payloads_overlay(
        &mut self,
        fiber_id: &GlobalElementId,
        element: &mut AnyElement,
        cx: &mut App,
    ) {
        // Note: Component expansion now happens BEFORE reconciliation via expand_wrappers(),
        // so the element here is already the final expanded element.

        // This is the standard path from `cache_fiber_payloads`, but without the
        // special-case early return for View elements.

        // Create/update retained render node
        self.sync_render_node(fiber_id, element, cx);

        let focus_handle = self
            .window
            .fiber
            .tree
            .render_nodes
            .get((*fiber_id).into())
            .and_then(|node| node.focus_handle());
        let mut focus_update = None;
        let mut clear_focus = None;
        let slot_key: DefaultKey = (*fiber_id).into();
        if self.window.fiber.tree.fibers.get(slot_key).is_some() {
            let old_focus = self.window.fiber.tree.focus_ids.get(slot_key).copied();
            if let Some(focus_handle) = focus_handle {
                self.window
                    .fiber
                    .tree
                    .focus_ids
                    .insert(slot_key, focus_handle.id);
                focus_update = Some((focus_handle, *fiber_id, old_focus));
            } else {
                self.window.fiber.tree.focus_ids.remove(slot_key);
                clear_focus = old_focus;
            }
        }
        // Overlays don't participate in view boundary metadata (no view_data wiring here).
        if let Some(view_state) = self
            .window
            .fiber
            .tree
            .view_state
            .get_mut((*fiber_id).into())
        {
            view_state.view_descriptor_hash = 0;
        }
        self.register_focus(focus_update, clear_focus);

        // Recursively process children
        let child_ids = self.child_ids(fiber_id);
        let element_children = element.children_mut();
        for (child_id, child_elem) in child_ids.into_iter().zip(element_children.iter_mut()) {
            self.cache_fiber_payloads_overlay(&child_id, child_elem, cx);
        }
    }
}

use std::mem;

struct PaintFrame {
    fiber_id: GlobalElementId,
    capture: PaintCapture,
    bounds: Bounds<Pixels>,
    stack_state: StackState,
    style: Option<Style>,
    hitbox: Option<Hitbox>,
    group: Option<SharedString>,
    store_paint_list: bool,
    paint_scene: bool,
    before_segment: Option<SceneSegmentId>,
    after_segment: Option<SceneSegmentId>,
    pushed_text_style: bool,
    pushed_content_mask: bool,
    pushed_image_cache: bool,
    pushed_tab_group: bool,
    pushed_view_id: bool,
    pushed_fiber_id: bool,
    previous_opacity: Option<f32>,
    segment_list: Vec<SceneSegmentId>,
    render_node_frame: Option<crate::PaintFrame>,
}

/// Cached prepaint state for a subtree.
#[derive(Clone)]
pub(crate) struct PrepaintState {
    accessed_entities: FxHashSet<EntityId>,
    line_layout_range: Range<LineLayoutIndex>,
}

/// Cached paint commands for a subtree.
#[derive(Clone)]
pub(crate) struct PaintList {
    line_layout_range: Range<LineLayoutIndex>,
}

#[derive(Clone)]
pub(crate) struct PrepaintCapture {
    line_layout_start: LineLayoutIndex,
}

#[derive(Clone)]
pub(crate) struct PaintCapture {
    line_layout_start: LineLayoutIndex,
}

#[derive(Clone, Copy)]
pub(super) struct DeferredDrawKey {
    pub(super) owner_id: GlobalElementId,
    pub(super) index: usize,
    pub(super) priority: usize,
    pub(super) sequence: usize,
}

#[derive(Clone, Copy)]
struct StackState {
    text_style_len: usize,
    content_mask_len: usize,
    transform_stack_depth: usize,
    transform_stack_offset: Point<Pixels>,
    image_cache_len: usize,
    rendered_entity_len: usize,
    fiber_id_len: usize,
}

impl StackState {
    fn capture(window: &Window) -> Self {
        Self {
            text_style_len: window.text_style_stack.len(),
            content_mask_len: window.content_mask_stack.len(),
            transform_stack_depth: window.transform_stack.depth(),
            transform_stack_offset: window.transform_stack.local_offset(),
            image_cache_len: window.image_cache_stack.len(),
            rendered_entity_len: window.rendered_entity_stack.len(),
            fiber_id_len: window.fiber.fiber_id_stack.len(),
        }
    }

    fn restore(self, window: &mut Window) {
        #[cfg(debug_assertions)]
        {
            debug_assert!(
                window.text_style_stack.len() >= self.text_style_len,
                "text_style_stack underflow: expected >= {}, got {}",
                self.text_style_len,
                window.text_style_stack.len()
            );
            debug_assert!(
                window.content_mask_stack.len() >= self.content_mask_len,
                "content_mask_stack underflow: expected >= {}, got {}",
                self.content_mask_len,
                window.content_mask_stack.len()
            );
            debug_assert!(
                window.image_cache_stack.len() >= self.image_cache_len,
                "image_cache_stack underflow: expected >= {}, got {}",
                self.image_cache_len,
                window.image_cache_stack.len()
            );
            debug_assert!(
                window.rendered_entity_stack.len() >= self.rendered_entity_len,
                "rendered_entity_stack underflow: expected >= {}, got {}",
                self.rendered_entity_len,
                window.rendered_entity_stack.len()
            );
            debug_assert!(
                window.fiber.fiber_id_stack.len() >= self.fiber_id_len,
                "fiber_id_stack underflow: expected >= {}, got {}",
                self.fiber_id_len,
                window.fiber.fiber_id_stack.len()
            );
        }
        window.text_style_stack.truncate(self.text_style_len);
        window.content_mask_stack.truncate(self.content_mask_len);
        window.transform_stack.truncate(self.transform_stack_depth);
        window
            .transform_stack
            .set_local_offset(self.transform_stack_offset);
        window.image_cache_stack.truncate(self.image_cache_len);
        window
            .rendered_entity_stack
            .truncate(self.rendered_entity_len);
        window.fiber.fiber_id_stack.truncate(self.fiber_id_len);
    }
}

impl FiberRuntimeHandle<'_> {
    pub(crate) fn rebuild_collection_ordering(&mut self) {
        let structure_epoch = self.window.fiber.tree.structure_epoch;
        self.window.fiber.tree.ensure_preorder_indices();
        let mut active_mouse_listeners = std::mem::replace(
            &mut self.window.fiber.active_mouse_listeners,
            ActiveFiberList::new(),
        );
        let before_len = active_mouse_listeners.members.len();
        {
            let tree = &self.window.fiber.tree;
            active_mouse_listeners.members.retain(|fiber_id| {
                let effects = tree.effects.get((*fiber_id).into());
                // Get interactivity from render node
                let interactivity = tree
                    .render_nodes
                    .get((*fiber_id).into())
                    .and_then(|node| node.interactivity());
                has_mouse_effects(interactivity, effects)
            });
        }
        if active_mouse_listeners.members.len() != before_len {
            active_mouse_listeners.dirty = true;
        }
        self.window.fiber.active_mouse_listeners = active_mouse_listeners;
        let mut tab_stops = mem::take(&mut self.window.fiber.rendered_tab_stops);
        {
            let tree = &self.window.fiber.tree;
            tab_stops.rebuild_order_if_needed(structure_epoch, |owner_id| {
                tree.preorder_index(&owner_id).unwrap_or(u64::MAX)
            });
        }
        self.window.fiber.rendered_tab_stops = tab_stops;
    }

    pub(super) fn collect_tooltip_requests(&mut self) -> Vec<TooltipRequest> {
        {
            let fiber = &mut self.window.fiber;
            fiber.active_tooltips.rebuild_if_needed(&mut fiber.tree);
        }
        let mut requests = Vec::new();
        for fiber_id in self.window.fiber.active_tooltips.ordered.iter().copied() {
            if let Some(entries) = self.window.fiber.tree.tooltips.get(fiber_id.into()) {
                requests.extend(entries.iter().cloned());
            }
        }
        requests
    }

    fn collect_cursor_style_requests(&mut self) -> Vec<CursorStyleRequest> {
        {
            let fiber = &mut self.window.fiber;
            fiber
                .active_cursor_styles
                .rebuild_if_needed(&mut fiber.tree);
        }
        let mut requests = Vec::new();
        for fiber_id in self
            .window
            .fiber
            .active_cursor_styles
            .ordered
            .iter()
            .copied()
        {
            if let Some(entries) = self.window.fiber.tree.cursor_styles.get(fiber_id.into()) {
                requests.extend(entries.iter().cloned());
            }
        }
        requests
    }

    pub(super) fn collect_deferred_draw_keys(&mut self) -> Vec<DeferredDrawKey> {
        {
            let fiber = &mut self.window.fiber;
            fiber
                .active_deferred_draws
                .rebuild_if_needed(&mut fiber.tree);
        }
        let mut keys = Vec::new();
        let mut sequence = 0usize;
        for fiber_id in self
            .window
            .fiber
            .active_deferred_draws
            .ordered
            .iter()
            .copied()
        {
            if let Some(draws) = self.window.fiber.tree.deferred_draws.get(fiber_id.into()) {
                for (index, draw) in draws.iter().enumerate() {
                    keys.push(DeferredDrawKey {
                        owner_id: fiber_id,
                        index,
                        priority: draw.priority,
                        sequence,
                    });
                    sequence += 1;
                }
            }
        }
        keys
    }

    pub(super) fn latest_input_handler(&mut self, cx: &App) -> Option<PlatformInputHandler> {
        let async_cx = self.window.to_async(cx);
        let focus_id = self.window.focus?;
        let fiber_id = self
            .window
            .fiber
            .tree
            .focusable_fibers
            .get(&focus_id)
            .copied()?;
        self.window
            .fiber
            .tree
            .input_handlers
            .get(fiber_id.into())
            .map(|_| PlatformInputHandler::new(async_cx, fiber_id))
    }

    pub(super) fn cursor_style_for_frame(&mut self) -> Option<CursorStyle> {
        let requests = self.collect_cursor_style_requests();
        let mut result = None;
        for request in requests.iter().rev() {
            match request.hitbox_id {
                None => {
                    result = Some(request.style);
                    break;
                }
                Some(hitbox_id) => {
                    if self.window.hitbox_is_hovered(hitbox_id) {
                        result = Some(request.style);
                        break;
                    }
                }
            }
        }

        if result.is_none() {
            let hit_test = &self.window.mouse_hit_test;
            for hitbox_id in hit_test.ids.iter().take(hit_test.hover_hitbox_count) {
                if let Some(style) = self
                    .window
                    .fiber
                    .tree
                    .effects
                    .get((*hitbox_id).into())
                    .and_then(|effects| effects.cursor_style)
                {
                    result = Some(style);
                    break;
                }
            }
        }

        result
    }

    pub(super) fn prepaint_deferred_draws(
        &mut self,
        deferred_draws: &[DeferredDrawKey],
        cx: &mut App,
    ) {
        for draw_key in deferred_draws {
            let (
                current_view,
                text_style_stack,
                rem_size,
                mut element,
                fiber_id,
                reference_fiber,
                local_offset,
                requires_layout,
                layout_constraints,
            ) = {
                let Some(draws) = self
                    .window
                    .fiber
                    .tree
                    .deferred_draws
                    .get_mut(draw_key.owner_id.into())
                else {
                    continue;
                };
                let Some(draw) = draws.get_mut(draw_key.index) else {
                    continue;
                };
                let element = draw.element.take();
                (
                    draw.current_view,
                    draw.text_style_stack.clone(),
                    draw.rem_size,
                    element,
                    draw.fiber_id,
                    draw.reference_fiber,
                    draw.local_offset,
                    draw.requires_layout,
                    draw.layout_constraints,
                )
            };

            let absolute_offset = if let Some(reference_fiber) = reference_fiber
                && let Some(origin) = self
                    .window
                    .fiber
                    .tree
                    .bounds
                    .get(reference_fiber.into())
                    .map(|bounds| bounds.origin)
            {
                local_offset + origin
            } else {
                local_offset
            };

            self.window.text_style_stack.clone_from(&text_style_stack);

            if let Some(fiber_id) = fiber_id {
                self.window.with_rendered_view(current_view, |window| {
                    window.with_rem_size(Some(rem_size), |window| {
                        // Detached overlays (`Window::defer_draw`) need their own reconcile+layout pass.
                        // Deferred fibers (`deferred(...)`) already have layout from the main layout pass.
	                        if requires_layout {
	                            if let Some(mut element) = element.take() {
	                                // Expand wrapper elements BEFORE reconciliation
	                                element.expand_wrappers(window, cx);
	                                window.fiber.tree.reconcile(&fiber_id, &element, true);
	                                window.fibers().cache_fiber_payloads_overlay(
	                                    &fiber_id,
	                                    &mut element,
	                                    cx,
	                                );
	                            }

                            let needs_layout = window.fiber.tree.get(&fiber_id).is_some()
                                && (window
                                    .fiber
                                    .tree
                                    .dirty_flags(&fiber_id)
                                    .contains(crate::DirtyFlags::NEEDS_LAYOUT)
                                    || window
                                        .fiber
                                        .tree
                                        .layout_state
                                        .get(fiber_id.into())
                                        .is_some_and(|layout_state| {
                                            layout_state.taffy_cache.is_empty()
                                        }));
		                            if needs_layout {
		                                window.prepare_detached_root_for_layout(fiber_id, cx);
		                                window.compute_layout_for_fiber(fiber_id, layout_constraints, cx);
		                            }
                        }

                        let mut prepaint_cx = PrepaintCx::new(window);
                        prepaint_cx.with_absolute_element_offset(absolute_offset, |window| {
                            window
                                .fibers()
                                .prepaint_fiber_tree_internal(fiber_id, cx, true)
                        })
                    })
                });
            }

            if let Some(draws) = self
                .window
                .fiber
                .tree
                .deferred_draws
                .get_mut(draw_key.owner_id.into())
                && let Some(draw) = draws.get_mut(draw_key.index)
            {
                // Descriptor is only needed for the reconcile step above; after that, the retained
                // fiber subtree owns all persistent state (nodes and legacy escape hatch storage).
                draw.element = None;
            }
        }
        self.window.text_style_stack.clear();
    }

    pub(super) fn paint_deferred_draws(
        &mut self,
        deferred_draws: &[DeferredDrawKey],
        cx: &mut App,
    ) {
        for draw_key in deferred_draws {
            let (current_view, text_style_stack, rem_size, fiber_id, reference_fiber, local_offset) = {
                let Some(draws) = self
                    .window
                    .fiber
                    .tree
                    .deferred_draws
                    .get_mut(draw_key.owner_id.into())
                else {
                    continue;
                };
                let Some(draw) = draws.get_mut(draw_key.index) else {
                    continue;
                };
                (
                    draw.current_view,
                    draw.text_style_stack.clone(),
                    draw.rem_size,
                    draw.fiber_id,
                    draw.reference_fiber,
                    draw.local_offset,
                )
            };

            let absolute_offset = if let Some(reference_fiber) = reference_fiber
                && let Some(origin) = self
                    .window
                    .fiber
                    .tree
                    .bounds
                    .get(reference_fiber.into())
                    .map(|bounds| bounds.origin)
            {
                local_offset + origin
            } else {
                local_offset
            };

            self.window.text_style_stack.clone_from(&text_style_stack);

            if let Some(fiber_id) = fiber_id {
                self.window.with_rendered_view(current_view, |window| {
                    window.with_rem_size(Some(rem_size), |window| {
                        let mut paint_cx = PaintCx::new(window);
                        paint_cx.with_absolute_element_offset(absolute_offset, |window| {
                            window
                                .fibers()
                                .paint_fiber_tree_internal(fiber_id, cx, true)
                        })
                    })
                });
            }

            if let Some(draws) = self
                .window
                .fiber
                .tree
                .deferred_draws
                .get_mut(draw_key.owner_id.into())
                && let Some(draw) = draws.get_mut(draw_key.index)
            {
                draw.element = None;
            }
        }
        self.window.text_style_stack.clear();
    }

    pub(crate) fn prepaint_capture(&self) -> PrepaintCapture {
        PrepaintCapture {
            line_layout_start: self.window.text_system().layout_index(),
        }
    }

    pub(crate) fn paint_capture(&self) -> PaintCapture {
        PaintCapture {
            line_layout_start: self.window.text_system().layout_index(),
        }
    }

    fn capture_prepaint_state(
        &mut self,
        capture: PrepaintCapture,
        accessed_entities: FxHashSet<EntityId>,
    ) -> PrepaintState {
        PrepaintState {
            accessed_entities,
            line_layout_range: capture.line_layout_start..self.window.text_system().layout_index(),
        }
    }

    /// Store prepaint output directly on the fiber.
    pub(crate) fn store_prepaint_state(
        &mut self,
        fiber_id: &GlobalElementId,
        capture: PrepaintCapture,
        accessed_entities: FxHashSet<EntityId>,
    ) {
        let state = self.capture_prepaint_state(capture, accessed_entities);
        if let Some(cache) = self
            .window
            .fiber
            .tree
            .paint_cache
            .get_mut((*fiber_id).into())
        {
            cache.prepaint_state = Some(state);
        }
    }

    /// Replay a cached prepaint state into the current frame.
    pub(crate) fn replay_prepaint_state(
        &mut self,
        fiber_id: &GlobalElementId,
        cx: &mut App,
    ) -> bool {
        let Some(state) = self
            .window
            .fiber
            .tree
            .paint_cache
            .get((*fiber_id).into())
            .and_then(|cache| cache.prepaint_state.as_ref())
        else {
            return false;
        };

        cx.entities.extend_accessed(&state.accessed_entities);

        // Update the cached ranges on the fiber's prepaint state
        let line_layout_range = self.reuse_line_layouts(state.line_layout_range.clone());
        if let Some(cache) = self
            .window
            .fiber
            .tree
            .paint_cache
            .get_mut((*fiber_id).into())
        {
            if let Some(state) = cache.prepaint_state.as_mut() {
                state.line_layout_range = line_layout_range;
            }
        }

        true
    }

    fn capture_paint_list(&mut self, capture: PaintCapture) -> PaintList {
        PaintList {
            line_layout_range: capture.line_layout_start..self.window.text_system().layout_index(),
        }
    }

    /// Store paint output directly on the fiber.
    pub(crate) fn store_paint_list(&mut self, fiber_id: &GlobalElementId, capture: PaintCapture) {
        let list = self.capture_paint_list(capture);
        if let Some(cache) = self
            .window
            .fiber
            .tree
            .paint_cache
            .get_mut((*fiber_id).into())
        {
            cache.paint_list = Some(list);
        }
    }

    fn clear_prepaint_state(&mut self, fiber_id: &GlobalElementId) {
        if let Some(cache) = self
            .window
            .fiber
            .tree
            .paint_cache
            .get_mut((*fiber_id).into())
        {
            cache.prepaint_state = None;
        }
    }

    fn clear_paint_state(&mut self, fiber_id: &GlobalElementId) {
        if let Some(cache) = self
            .window
            .fiber
            .tree
            .paint_cache
            .get_mut((*fiber_id).into())
        {
            cache.paint_list = None;
            cache.scene_segment_list = None;
        }
    }

    fn store_paint_results(&mut self, fiber_id: &GlobalElementId, frame: &PaintFrame) {
        if frame.store_paint_list {
            self.store_paint_list(fiber_id, frame.capture.clone());
            self.window.fiber.tree.clear_dirty_flags(fiber_id);
            if let Some(cache) = self
                .window
                .fiber
                .tree
                .paint_cache
                .get_mut((*fiber_id).into())
            {
                cache.scene_segment_list = Some(frame.segment_list.clone());
            }
        } else {
            self.clear_paint_state(fiber_id);
        }
    }

    /// Replay a cached paint list into the current frame.
    pub(crate) fn replay_paint_list(&mut self, fiber_id: &GlobalElementId) {
        let Some(list) = self
            .window
            .fiber
            .tree
            .paint_cache
            .get((*fiber_id).into())
            .and_then(|cache| cache.paint_list.as_ref())
            .cloned()
        else {
            return;
        };

        // Update the cached line layout range on the fiber's paint list
        let line_layout_range = self.reuse_line_layouts(list.line_layout_range);
        if let Some(cache) = self
            .window
            .fiber
            .tree
            .paint_cache
            .get_mut((*fiber_id).into())
        {
            if let Some(list) = cache.paint_list.as_mut() {
                list.line_layout_range = line_layout_range;
            }
        }
    }

    fn reuse_line_layouts(
        &mut self,
        line_layout_range: Range<LineLayoutIndex>,
    ) -> Range<LineLayoutIndex> {
        let line_layout_start = self.window.text_system().layout_index();
        self.window.text_system().reuse_layouts(line_layout_range);
        let line_layout_end = self.window.text_system().layout_index();
        line_layout_start..line_layout_end
    }

    fn paint_after_children_segment(&mut self, frame: &mut PaintFrame, cx: &mut App) {
        let Some(style) = frame.style.as_ref() else {
            return;
        };
        let Some(segments) = self.window.fiber.tree.scene_segments(&frame.fiber_id) else {
            return;
        };
        let after_segment = if frame.paint_scene {
            let after_segment = self.ensure_after_segment(&frame.fiber_id, segments);
            self.window
                .next_frame
                .scene
                .reset_segment(&mut self.window.segment_pool, after_segment);
            self.window
                .next_frame
                .scene
                .push_fiber_segment(after_segment);
            style.paint_after_children(frame.bounds, self.window, cx);
            self.window.next_frame.scene.pop_segment();
            Some(after_segment)
        } else {
            segments.after.inspect(|after_segment| {
                self.window
                    .next_frame
                    .scene
                    .push_fiber_segment(*after_segment);
                self.window.next_frame.scene.pop_segment();
            })
        };
        if let Some(after_segment) = after_segment {
            frame.after_segment = Some(after_segment);
            frame.segment_list.push(after_segment);
        }
    }

    pub(crate) fn invalidate_fiber_hitboxes(&mut self, fiber_id: &GlobalElementId) {
        if let Some(state) = self
            .window
            .fiber
            .tree
            .hitbox_state
            .get_mut((*fiber_id).into())
        {
            state.hitbox = None;
            state.hitbox_subtree_bounds = None;
        }
    }

    fn invalidate_descendant_hitboxes(&mut self, fiber_id: &GlobalElementId) {
        let mut changed = false;
        let mut stack: Vec<GlobalElementId> = self.window.fiber.tree.children(fiber_id).collect();
        while let Some(child_id) = stack.pop() {
            if self
                .window
                .fiber
                .tree
                .hitbox_state
                .get(child_id.into())
                .is_some_and(|state| {
                    state.hitbox.is_some() || state.hitbox_subtree_bounds.is_some()
                })
            {
                changed = true;
            }
            self.invalidate_fiber_hitboxes(&child_id);
            stack.extend(self.window.fiber.tree.children(&child_id));
        }
        if changed {
            self.window.fiber.tree.bump_hitbox_epoch();
        }
    }

    pub(super) fn truncate_hitboxes(&mut self, new_len: usize) {
        while self.window.fiber.hitbox_stack.len() > new_len {
            if let Some(hitbox_id) = self.window.fiber.hitbox_stack.pop() {
                if let Some(state) = self
                    .window
                    .fiber
                    .tree
                    .hitbox_state
                    .get_mut(hitbox_id.into())
                {
                    state.hitbox = None;
                    state.hitbox_subtree_bounds = None;
                }
            }
        }
    }

    fn child_bounds_for_fiber(
        &mut self,
        fiber_id: &GlobalElementId,
        scale_factor: f32,
        bounds_cache: &mut FxHashMap<GlobalElementId, Bounds<Pixels>>,
    ) -> Vec<Bounds<Pixels>> {
        let child_ids: SmallVec<[GlobalElementId; 8]> =
            self.window.fiber.tree.children(fiber_id).collect();
        let mut child_bounds = Vec::with_capacity(child_ids.len());
        for child_id in &child_ids {
            child_bounds.push(self.layout_bounds_with_offset(child_id, scale_factor, bounds_cache));
        }
        child_bounds
    }

    fn layout_bounds_with_offset(
        &mut self,
        fiber_id: &GlobalElementId,
        scale_factor: f32,
        bounds_cache: &mut FxHashMap<GlobalElementId, Bounds<Pixels>>,
    ) -> Bounds<Pixels> {
        let mut bounds = self
            .window
            .layout_bounds_cached(fiber_id, scale_factor, bounds_cache);
        bounds.origin += PrepaintCx::new(self.window).element_offset();
        bounds
    }

    /// Prepaint the fiber tree iteratively.
    /// Walks the tree, computing bounds, inserting hitboxes, and registering event handlers.
    pub(crate) fn prepaint_fiber_tree(&mut self, root: GlobalElementId, cx: &mut App) {
        self.prepaint_fiber_tree_internal(root, cx, false);
    }

    pub(crate) fn prepaint_fiber_tree_internal(
        &mut self,
        root: GlobalElementId,
        cx: &mut App,
        deferred_pass: bool,
    ) {
        #[cfg(debug_assertions)]
        let initial_fiber_id_stack_len = self.window.fiber.fiber_id_stack.len();

        self.window.fiber.tree.rebuild_layout_islands_if_needed();
        let scale_factor = self.window.scale_factor();
        // Reuse the Window's bounds cache to avoid per-frame HashMap allocation
        let mut bounds_cache = std::mem::take(&mut self.window.fiber.layout_bounds_cache);
        self.window.fiber.rendered_tab_stops.clear_groups();

        struct PrepaintScope {
            fiber_id: GlobalElementId,
            bounds: Bounds<Pixels>,
            capture: PrepaintCapture,
            stack_state: StackState,
            store_prepaint_state: bool,
            previous_hitbox: Option<HitboxData>,
            /// Frame returned by render node's prepaint_begin, if any.
            /// Used to call prepaint_end with the same frame.
            render_node_frame: Option<crate::PrepaintFrame>,
            /// Snapshot of accessed_entities at entry, used to compute new accesses at exit.
            access_baseline: FxHashSet<EntityId>,
            /// Known previous accesses (from view render or cache) to include in final result.
            previous_accesses: FxHashSet<EntityId>,
        }

        struct PrepaintTraversal<'a> {
            ctx: TraversalCtx<'a>,
            scale_factor: f32,
            bounds_cache: FxHashMap<GlobalElementId, Bounds<Pixels>>,
            frame_stack: Vec<PrepaintScope>,
            deferred_pass: bool,
        }

        fn enter_prepaint(
            state: &mut PrepaintTraversal<'_>,
            fiber_id: GlobalElementId,
        ) -> TraversalDecision {
            let handle = state.ctx.handle_mut();
            let cx = state.ctx.app_mut();
            let (is_deferred, z_index, can_replay) = {
                let Some(_fiber) = handle.window.fiber.tree.get(&fiber_id) else {
                    return TraversalDecision::SkipSubtree;
                };
                let z_index = handle
                    .window
                    .fiber
                    .tree
                    .deferred_priorities
                    .get(fiber_id.into())
                    .copied();
                let is_deferred = z_index.is_some();
                let can_replay = !handle.window.refreshing
                    && handle.window.fiber.tree.can_replay_prepaint(&fiber_id);
                (is_deferred, z_index, can_replay)
            };

            if is_deferred && !state.deferred_pass {
                handle
                    .window
                    .fiber
                    .tree
                    .deferred_draws
                    .remove(fiber_id.into());
                handle.window.fiber.active_deferred_draws.remove(&fiber_id);
                let priority = z_index.unwrap_or_default();
                let absolute_offset = PrepaintCx::new(handle.window).element_offset();
                handle.defer_fiber_draw(&fiber_id, absolute_offset, priority);
                return TraversalDecision::SkipSubtree;
            }

            if can_replay {
                #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
                {
                    handle.window.frame_diagnostics.prepaint_replayed_subtrees += 1;
                }
                handle.replay_prepaint_state(&fiber_id, cx);
                return TraversalDecision::SkipSubtree;
            }

            #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
            {
                handle.window.frame_diagnostics.prepaint_fibers += 1;
            }

            let previous_hitbox = handle
                .window
                .fiber
                .tree
                .hitbox_state
                .get(fiber_id.into())
                .and_then(|state| state.hitbox.clone());
            handle.invalidate_fiber_hitboxes(&fiber_id);
            handle.window.fiber.tree.tooltips.remove(fiber_id.into());
            handle.window.fiber.active_tooltips.remove(&fiber_id);
            if !state.deferred_pass {
                handle
                    .window
                    .fiber
                    .tree
                    .deferred_draws
                    .remove(fiber_id.into());
                handle.window.fiber.active_deferred_draws.remove(&fiber_id);
            }
            if let Some(groups) = handle
                .window
                .fiber
                .tree
                .deferred_draw_overlay_groups
                .get_mut(fiber_id.into())
            {
                for group in groups.values_mut() {
                    group.next_index = 0;
                }
            }
            if let Some(old_focus) = handle.window.fiber.tree.focus_ids.remove(fiber_id.into()) {
                handle.window.fiber.tree.focusable_fibers.remove(&old_focus);
            }

            let capture = handle.prepaint_capture();
            let access_baseline = cx.entities.accessed_entities.borrow().clone();
            let previous_accesses =
                if let Some(pending) = handle.window.take_pending_view_accesses(&fiber_id) {
                    pending
                } else if let Some(state) = handle
                    .window
                    .fiber
                    .tree
                    .paint_cache
                    .get(fiber_id.into())
                    .and_then(|cache| cache.prepaint_state.as_ref())
                {
                    state.accessed_entities.clone()
                } else {
                    FxHashSet::default()
                };

            let stack_state = StackState::capture(handle.window);
            if let Some(view_id) = handle
                .window
                .fiber
                .tree
                .get(&fiber_id)
                .and_then(|fiber| handle.window.fiber_view_id(&fiber_id, fiber))
            {
                handle.window.rendered_entity_stack.push(view_id);
            }
            handle.push_fiber_id(fiber_id);

            let bounds = handle.layout_bounds_with_offset(
                &fiber_id,
                state.scale_factor,
                &mut state.bounds_cache,
            );

            // Queue on_layout event if bounds changed and fiber has a listener.
            let fiber_key = FiberTree::key_for_id(fiber_id);
            if handle
                .window
                .fiber
                .tree
                .layout_listeners
                .contains_key(fiber_key)
            {
                let prev = handle
                    .window
                    .fiber
                    .tree
                    .previous_bounds
                    .get(fiber_key)
                    .copied();
                if prev.map_or(true, |p| p != bounds) {
                    handle
                        .window
                        .fiber
                        .tree
                        .previous_bounds
                        .insert(fiber_key, bounds);
                    handle
                        .window
                        .fiber
                        .pending_layout_events
                        .push((fiber_id, bounds));
                }
            }

            let mut skip_children = false;
            let mut store_prepaint_state = true;
            let mut render_node_frame: Option<crate::PrepaintFrame> = None;

            // Call render node prepaint_begin if fiber has a node.
            // We use take/put to avoid aliasing issues - nodes can freely call Window APIs.
            let mut render_node = handle
                .window
                .fiber
                .tree
                .render_nodes
                .remove(fiber_id.into());

            if let Some(ref mut node) = render_node {
                let child_bounds = if node.needs_child_bounds() {
                    handle.child_bounds_for_fiber(
                        &fiber_id,
                        state.scale_factor,
                        &mut state.bounds_cache,
                    )
                } else {
                    Vec::new()
                };
                let mut ctx = crate::PrepaintCtx {
                    fiber_id,
                    bounds,
                    child_bounds,
                    inspector_id: None,
                    window: handle.window,
                    cx,
                };
                let frame = node.prepaint_begin(&mut ctx);
                if frame.skip_children {
                    skip_children = true;
                }
                render_node_frame = Some(frame);
            }

            // Put the node back
            if let Some(node) = render_node {
                handle
                    .window
                    .fiber
                    .tree
                    .render_nodes
                    .insert(fiber_id.into(), node);
            }

            // Check if the render node handled prepaint (node OR legacy, never both).
            let node_handled = render_node_frame.as_ref().map_or(false, |f| f.handled);
            if node_handled {
                if let Some(ref frame) = render_node_frame {
                    skip_children = frame.skip_children;
                }
            } else {
                // Fibers without render nodes are not paintable; skip their children.
                skip_children = true;
                store_prepaint_state = false;
            }

            state.frame_stack.push(PrepaintScope {
                fiber_id,
                bounds,
                capture,
                stack_state,
                store_prepaint_state,
                previous_hitbox,
                render_node_frame,
                access_baseline,
                previous_accesses,
            });

            if skip_children {
                handle.invalidate_descendant_hitboxes(&fiber_id);
                TraversalDecision::SkipChildren
            } else {
                TraversalDecision::Continue
            }
        }

        fn exit_prepaint(state: &mut PrepaintTraversal<'_>, fiber_id: GlobalElementId) {
            let window = state.ctx.handle_mut();
            let cx = state.ctx.app_mut();
            let Some(frame) = state.frame_stack.pop() else {
                return;
            };
            debug_assert_eq!(frame.fiber_id, fiber_id);

            // Prune unused retained overlay roots for `Window::defer_draw` callsites owned by this fiber.
            //
            // This keeps overlay fibers retained across frames (for state/caching) while the callsite
            // continues to request them, and removes them when they are no longer requested.
            let to_remove: Vec<GlobalElementId> = window
                .window
                .fiber
                .tree
                .deferred_draw_overlay_groups
                .get_mut(fiber_id.into())
                .map(|groups| {
                    let mut to_remove = Vec::new();
                    for group in groups.values_mut() {
                        if group.next_index < group.roots.len() {
                            to_remove.extend(group.roots.drain(group.next_index..));
                        }
                        group.next_index = 0;
                    }
                    to_remove
                })
                .unwrap_or_default();
            for removed_root in to_remove {
                window.window.fiber.tree.remove(&removed_root);
            }

            // Call render node prepaint_end if we called prepaint_begin.
            // We use take/put to avoid aliasing issues - nodes can freely call Window APIs.
            if let Some(render_node_frame) = frame.render_node_frame {
                let mut render_node = window
                    .window
                    .fiber
                    .tree
                    .render_nodes
                    .remove(fiber_id.into());

                if let Some(ref mut node) = render_node {
                    let mut ctx = crate::PrepaintCtx {
                        fiber_id,
                        bounds: frame.bounds,
                        child_bounds: Vec::new(), // Not needed in prepaint_end
                        inspector_id: None,
                        window: window.window,
                        cx,
                    };
                    node.prepaint_end(&mut ctx, render_node_frame);
                }

                // Put the node back
                if let Some(node) = render_node {
                    window
                        .window
                        .fiber
                        .tree
                        .render_nodes
                        .insert(fiber_id.into(), node);
                }
            }

            let new_accesses: FxHashSet<EntityId> = cx
                .entities
                .accessed_entities
                .borrow()
                .difference(&frame.access_baseline)
                .copied()
                .collect();
            let mut accessed_entities = frame.previous_accesses;
            accessed_entities.extend(new_accesses);
            if frame.store_prepaint_state {
                window.store_prepaint_state(
                    &frame.fiber_id,
                    frame.capture.clone(),
                    accessed_entities,
                );
            } else {
                window.clear_prepaint_state(&frame.fiber_id);
            }

            let current_hitbox = window
                .window
                .fiber
                .tree
                .hitbox_state
                .get(fiber_id.into())
                .and_then(|state| state.hitbox.as_ref());
            if frame.previous_hitbox.as_ref() != current_hitbox {
                window.window.fiber.tree.bump_hitbox_epoch();
            }

            // Cache the union of hitbox bounds for this subtree so hit-testing can
            // prune irrelevant branches without walking the full tree.
            let subtree_hitbox_bounds = {
                let transform_id = window.window.transform_stack.current();
                let mut subtree_bounds: Option<Bounds<Pixels>> = None;

                for child_id in window.window.fiber.tree.children_slice(&fiber_id) {
                    if let Some(child_bounds) = window
                        .window
                        .fiber
                        .tree
                        .hitbox_state
                        .get((*child_id).into())
                        .and_then(|state| state.hitbox_subtree_bounds)
                    {
                        if child_bounds.transform_id == transform_id
                            && child_bounds.bounds.size.width > crate::px(0.)
                            && child_bounds.bounds.size.height > crate::px(0.)
                        {
                            subtree_bounds = Some(match subtree_bounds {
                                Some(existing) => existing.union(&child_bounds.bounds),
                                None => child_bounds.bounds,
                            });
                        }
                    }
                }

                if let Some(data) = window
                    .window
                    .fiber
                    .tree
                    .hitbox_state
                    .get(fiber_id.into())
                    .and_then(|state| state.hitbox.as_ref())
                {
                    if data.transform_id == transform_id
                        && data.bounds.size.width > crate::px(0.)
                        && data.bounds.size.height > crate::px(0.)
                    {
                        subtree_bounds = Some(match subtree_bounds {
                            Some(existing) => existing.union(&data.bounds),
                            None => data.bounds,
                        });
                    }
                }

                subtree_bounds.map(|bounds| HitboxSubtreeBounds {
                    transform_id,
                    bounds,
                })
            };

            if let Some(state) = window
                .window
                .fiber
                .tree
                .hitbox_state
                .get_mut(fiber_id.into())
            {
                state.hitbox_subtree_bounds = subtree_hitbox_bounds;
            }

            frame.stack_state.restore(window.window);
        }

        fn prepaint_children(
            state: &mut PrepaintTraversal<'_>,
            fiber_id: GlobalElementId,
        ) -> SmallVec<[GlobalElementId; 8]> {
            let window = state.ctx.handle_mut();
            window.window.fiber.tree.children(&fiber_id).collect()
        }

        let mut traversal = PrepaintTraversal {
            ctx: TraversalCtx::new(self, cx),
            scale_factor,
            bounds_cache,
            frame_stack: Vec::new(),
            deferred_pass,
        };

        traverse_fibers(
            root,
            &mut traversal,
            enter_prepaint,
            exit_prepaint,
            prepaint_children,
        );
        debug_assert!(
            traversal.frame_stack.is_empty(),
            "prepaint traversal frame stack not fully unwound"
        );

        // Return the bounds cache to Window for reuse
        let bounds_cache = traversal.bounds_cache;
        traversal
            .ctx
            .with_mut(|handle, _cx| handle.window.fiber.layout_bounds_cache = bounds_cache);

        #[cfg(debug_assertions)]
        debug_assert_eq!(
            self.window.fiber.fiber_id_stack.len(),
            initial_fiber_id_stack_len,
            "fiber_id_stack not balanced after prepaint: started with {}, ended with {}",
            initial_fiber_id_stack_len,
            self.window.fiber.fiber_id_stack.len()
        );
    }

    /// Paint the fiber tree iteratively, replaying cached paint lists where possible.
    pub(crate) fn paint_fiber_tree(&mut self, root: GlobalElementId, cx: &mut App) {
        self.paint_fiber_tree_internal(root, cx, false);
    }

    fn ensure_before_segment(&mut self, fiber_id: &GlobalElementId) -> (SceneSegmentId, bool) {
        if let Some(segments) = self.window.fiber.tree.scene_segments(fiber_id) {
            (segments.before, false)
        } else {
            let segment_id = self
                .window
                .next_frame
                .scene
                .alloc_segment(&mut self.window.segment_pool);
            self.window.fiber.tree.insert_scene_segments(
                fiber_id,
                crate::fiber::FiberSceneSegments {
                    before: segment_id,
                    after: None,
                },
            );
            (segment_id, true)
        }
    }

    fn ensure_after_segment(
        &mut self,
        fiber_id: &GlobalElementId,
        mut segments: crate::fiber::FiberSceneSegments,
    ) -> SceneSegmentId {
        if let Some(after) = segments.after {
            after
        } else {
            let segment_id = self
                .window
                .next_frame
                .scene
                .alloc_segment(&mut self.window.segment_pool);
            segments.after = Some(segment_id);
            self.window
                .fiber
                .tree
                .insert_scene_segments(fiber_id, segments);
            segment_id
        }
    }

    pub(crate) fn paint_fiber_tree_internal(
        &mut self,
        root: GlobalElementId,
        cx: &mut App,
        deferred_pass: bool,
    ) {
        #[cfg(debug_assertions)]
        let initial_fiber_id_stack_len = self.window.fiber.fiber_id_stack.len();

        self.window.fiber.tree.rebuild_layout_islands_if_needed();
        let scale_factor = self.window.scale_factor();
        // Reuse the Window's bounds cache to avoid per-frame HashMap allocation
        let mut bounds_cache = std::mem::take(&mut self.window.fiber.layout_bounds_cache);

        fn reset_fiber_effects(window: &mut FiberRuntimeHandle<'_>, fiber_id: GlobalElementId) {
            let tab_stops = window
                .window
                .fiber
                .tree
                .remove_tab_stops_for_fiber(&fiber_id);
            window
                .window
                .remove_rendered_tab_stops_for_fiber(fiber_id, tab_stops);
            window
                .window
                .fiber
                .tree
                .cursor_styles
                .remove(fiber_id.into());
            window.window.fiber.active_cursor_styles.remove(&fiber_id);
            window
                .window
                .fiber
                .tree
                .input_handlers
                .remove(fiber_id.into());
            window.window.fiber.active_input_handlers.remove(&fiber_id);
            window
                .window
                .fiber
                .tree
                .key_contexts
                .remove(fiber_id.into());
            if let Some(effects) = window.window.fiber.tree.effects.get_mut(fiber_id.into()) {
                effects.click_listeners.clear();
                effects.any_mouse_listeners.clear();
                effects.key_listeners.clear();
                effects.modifiers_changed_listeners.clear();
                effects.action_listeners.clear();
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
            window.window.update_active_mouse_listeners(&fiber_id);
        }

        fn unwind_paint_frame(window: &mut FiberRuntimeHandle<'_>, frame: &PaintFrame) {
            if frame.pushed_tab_group {
                window.window.fiber.rendered_tab_stops.end_group();
            }
            if frame.pushed_content_mask {
                window.window.content_mask_stack.pop();
            }
            if frame.pushed_text_style {
                window.window.text_style_stack.pop();
            }
            if let Some(previous_opacity) = frame.previous_opacity {
                window.window.element_opacity = previous_opacity;
            }
            if frame.pushed_image_cache {
                window.window.image_cache_stack.pop();
            }
            if frame.pushed_view_id {
                window.window.rendered_entity_stack.pop();
            }
            if frame.pushed_fiber_id {
                window.pop_fiber_id();
            }
        }
        struct PaintTraversal<'a> {
            ctx: TraversalCtx<'a>,
            scale_factor: f32,
            bounds_cache: FxHashMap<GlobalElementId, Bounds<Pixels>>,
            frame_stack: Vec<PaintFrame>,
            deferred_pass: bool,
        }

        fn enter_paint(
            state: &mut PaintTraversal<'_>,
            fiber_id: GlobalElementId,
        ) -> TraversalDecision {
            let handle = state.ctx.handle_mut();
            let cx = state.ctx.app_mut();
            let (can_replay, should_skip_work, mut paint_scene, dirty_flags) = {
                let Some(_fiber) = handle.window.fiber.tree.get(&fiber_id) else {
                    return TraversalDecision::SkipSubtree;
                };
                let is_deferred = handle
                    .window
                    .fiber
                    .tree
                    .deferred_priorities
                    .contains_key(fiber_id.into());
                // Deferred elements are skipped during main pass (they'll be painted later as subtree roots)
                let should_skip_work = is_deferred && !state.deferred_pass;
                let can_replay = !should_skip_work
                    && !handle.window.refreshing
                    && handle.window.fiber.tree.can_replay_paint(&fiber_id);
                let dirty_flags = handle.window.fiber.tree.dirty_flags(&fiber_id);
                let mut paint_scene = dirty_flags.needs_paint()
                    || handle
                        .window
                        .fiber
                        .tree
                        .has_prepaint_dirty_ancestor(&fiber_id);
                (can_replay, should_skip_work, paint_scene, dirty_flags)
            };

            // Skip deferred elements during main pass (they'll be painted later as subtree roots).
            // Exception: don't skip if this fiber IS the traversal root (allows painting deferred
            // fibers directly without going through non-deferred ancestors).
            let is_traversal_root = state.frame_stack.is_empty();
            log::trace!(
                "[PAINT_ENTER] fiber_id={:?} can_replay={} should_skip={} paint_scene={} dirty={:?} is_root={}",
                fiber_id,
                can_replay,
                should_skip_work,
                paint_scene,
                dirty_flags,
                is_traversal_root
            );
            if should_skip_work && !is_traversal_root {
                log::trace!("[PAINT_SKIP_DEFERRED] fiber_id={:?} skipping deferred during main pass", fiber_id);
                return TraversalDecision::SkipSubtree;
            }

            if can_replay {
                let segment_list = handle
                    .window
                    .fiber
                    .tree
                    .paint_cache
                    .get(fiber_id.into())
                    .and_then(|cache| cache.scene_segment_list.as_ref())
                    .cloned();
                if let Some(segment_list) = segment_list {
                    #[cfg(any(
                        debug_assertions,
                        feature = "diagnostics",
                        feature = "test-support"
                    ))]
                    {
                        handle.window.frame_diagnostics.paint_replayed_subtrees += 1;
                    }
                    log::trace!(
                        "[PAINT_REPLAY] fiber_id={:?} replaying cached paint with {} segments",
                        fiber_id,
                        segment_list.len()
                    );
                    handle.replay_paint_list(&fiber_id);
                    if let Some(parent) = state.frame_stack.last_mut() {
                        parent.segment_list.extend(segment_list);
                    }
                    return TraversalDecision::SkipSubtree;
                }
            }

            let bounds = handle.layout_bounds_with_offset(
                &fiber_id,
                state.scale_factor,
                &mut state.bounds_cache,
            );
            let clipped_bounds = bounds.intersect(&handle.window.content_mask().bounds);

            // If this subtree is entirely outside the current content mask, skip painting it.
            //
            // This is especially important for scroll performance: when a scroll container
            // invalidates paint, all descendants become ineligible for cached replay due to the
            // dirty-ancestor check. Without culling, we end up repainting large offscreen subtrees.
            if clipped_bounds.is_empty() {
                log::trace!(
                    "[PAINT_CLIPPED] fiber_id={:?} bounds={:?} content_mask={:?}",
                    fiber_id,
                    bounds,
                    handle.window.content_mask().bounds
                );
                // Only safe to skip painting when there is no pending work in this subtree.
                //
                // Dirty flags are cleared globally in `end_of_frame_cleanup`, so if we "skip paint"
                // for a dirty-but-clipped subtree (e.g. because hover changed while scrolling),
                // we'd drop the invalidation and end up replaying stale cached primitives the next
                // time the subtree scrolls back into view.
                let has_pending_work = paint_scene
                    || dirty_flags.needs_paint()
                    || dirty_flags.contains(DirtyFlags::HAS_PAINT_DIRTY_DESCENDANT);
                if has_pending_work || handle.window.refreshing {
                    // Proceed with painting so cached primitives match current state.
                } else if let Some(segment_list) = handle
                    .window
                    .fiber
                    .tree
                    .paint_cache
                    .get(fiber_id.into())
                    .and_then(|cache| cache.scene_segment_list.as_ref())
                    .cloned()
                {
                    // When scene culling is disabled (e.g. within a scrollable container), we must
                    // preserve cached segment contents even if they're currently clipped out.
                    // Otherwise, transform-only scroll would clear offscreen segments and they'd
                    // never reappear until a full repaint.
                    if handle.window.should_cull_scene_primitives() {
                        log::trace!(
                            "[PAINT_CULL_RESET] fiber_id={:?} resetting {} segments due to culling",
                            fiber_id,
                            segment_list.len()
                        );
                        for segment_id in segment_list.iter().copied() {
                            handle
                                .window
                                .next_frame
                                .scene
                                .reset_segment(&mut handle.window.segment_pool, segment_id);
                        }
                    }
                    #[cfg(any(
                        debug_assertions,
                        feature = "diagnostics",
                        feature = "test-support"
                    ))]
                    {
                        handle.window.frame_diagnostics.paint_replayed_subtrees += 1;
                    }
                    handle.replay_paint_list(&fiber_id);
                    if let Some(parent) = state.frame_stack.last_mut() {
                        parent.segment_list.extend(segment_list);
                    }
                    return TraversalDecision::SkipSubtree;
                }
            }

            #[cfg(any(debug_assertions, feature = "diagnostics", feature = "test-support"))]
            {
                handle.window.frame_diagnostics.paint_fibers += 1;
            }

            if !can_replay && !paint_scene {
                // We can't replay cached output, so ensure we reset and repaint this segment.
                log::trace!(
                    "[PAINT_FORCE_REPAINT] fiber_id={:?} forcing repaint because can_replay=false and paint_scene=false",
                    fiber_id
                );
                paint_scene = true;
            }

            reset_fiber_effects(handle, fiber_id);

            let stack_state = StackState::capture(handle.window);
            let mut pushed_view_id = false;
            if let Some(fiber) = handle.window.fiber.tree.get(&fiber_id) {
                if let Some(view_id) = handle.window.fiber_view_id(&fiber_id, fiber) {
                    handle.window.rendered_entity_stack.push(view_id);
                    pushed_view_id = true;
                }
            }
            handle.push_fiber_id(fiber_id);
            let pushed_fiber_id = true;

            let (before_segment, created_before_segment) = handle.ensure_before_segment(&fiber_id);
            if created_before_segment {
                paint_scene = true;
            }
            log::trace!(
                "[PAINT_EXECUTE] fiber_id={:?} before_segment={:?} created={} paint_scene={} bounds={:?}",
                fiber_id,
                before_segment,
                created_before_segment,
                paint_scene,
                bounds
            );
            if paint_scene {
                handle
                    .window
                    .next_frame
                    .scene
                    .reset_segment(&mut handle.window.segment_pool, before_segment);
            }
            handle
                .window
                .next_frame
                .scene
                .push_fiber_segment(before_segment);

            let capture = handle.paint_capture();
            let mut frame = PaintFrame {
                fiber_id,
                capture,
                bounds,
                stack_state,
                style: None,
                hitbox: None,
                group: None,
                store_paint_list: paint_scene,
                paint_scene,
                before_segment: Some(before_segment),
                after_segment: None,
                pushed_text_style: false,
                pushed_content_mask: false,
                pushed_image_cache: false,
                pushed_tab_group: false,
                pushed_view_id,
                pushed_fiber_id,
                previous_opacity: None,
                segment_list: vec![before_segment],
                render_node_frame: None,
            };
            let mut skip_children = false;

            // Call render node paint_begin FIRST to determine if it handles the phase.
            // We use take/put to avoid aliasing issues - nodes can freely call Window APIs.
            let mut render_node = handle
                .window
                .fiber
                .tree
                .render_nodes
                .remove(fiber_id.into());

            if let Some(ref mut node) = render_node {
                let child_bounds = if node.needs_child_bounds() {
                    handle.child_bounds_for_fiber(
                        &fiber_id,
                        state.scale_factor,
                        &mut state.bounds_cache,
                    )
                } else {
                    Vec::new()
                };
                let mut ctx = crate::PaintCtx {
                    fiber_id,
                    bounds,
                    child_bounds,
                    inspector_id: None,
                    window: handle.window,
                    cx,
                };
                let node_frame = node.paint_begin(&mut ctx);
                if node_frame.skip_children {
                    skip_children = true;
                }
                frame.render_node_frame = Some(node_frame);
            }

            // Put the node back
            if let Some(node) = render_node {
                handle
                    .window
                    .fiber
                    .tree
                    .render_nodes
                    .insert(fiber_id.into(), node);
            }

            // Check if the render node handled paint (node OR legacy, never both)
            let node_handled = frame
                .render_node_frame
                .as_ref()
                .map_or(false, |f| f.handled);
            if !node_handled {
                // Fibers without render nodes are not paintable; skip their children.
                skip_children = true;
            }

            state.frame_stack.push(frame);
            if skip_children {
                TraversalDecision::SkipChildren
            } else {
                TraversalDecision::Continue
            }
        }

        fn exit_paint(state: &mut PaintTraversal<'_>, fiber_id: GlobalElementId) {
            let window = state.ctx.handle_mut();
            let cx = state.ctx.app_mut();
            let Some(mut frame) = state.frame_stack.pop() else {
                return;
            };
            debug_assert_eq!(frame.fiber_id, fiber_id);

            if let Some(_hitbox) = frame.hitbox.as_ref() {
                #[cfg(any(feature = "inspector", debug_assertions))]
                window.window.insert_inspector_hitbox(_hitbox.id, None, cx);
                if let Some(group) = frame.group.as_ref() {
                    GroupHitboxes::pop(group, cx);
                }
            }

            if frame.before_segment.is_some() {
                window.window.next_frame.scene.pop_segment();
            }

            // Call render node paint_end if present.
            // We use take/put to avoid aliasing issues - nodes can freely call Window APIs.
            if let Some(render_node_frame) = frame.render_node_frame.take() {
                let mut render_node = window
                    .window
                    .fiber
                    .tree
                    .render_nodes
                    .remove(fiber_id.into());

                if let Some(ref mut node) = render_node {
                    // If the node paints after its children (e.g. borders), ensure an after-segment
                    // exists and run paint_end with that segment as the active segment so the output
                    // is ordered after children in the final scene.
                    let segments = window.window.fiber.tree.scene_segments(&fiber_id);
                    let existing_after = segments.and_then(|segments| segments.after);
                    let needs_after_segment =
                        existing_after.is_some() || node.needs_after_segment();
                    let mut pushed_after_segment = false;

                    if needs_after_segment {
                        if let Some(segments) = segments {
                            let after_segment = existing_after.unwrap_or_else(|| {
                                window.ensure_after_segment(&fiber_id, segments)
                            });

                            if frame.paint_scene {
                                window
                                    .window
                                    .next_frame
                                    .scene
                                    .reset_segment(&mut window.window.segment_pool, after_segment);
                            }
                            window
                                .window
                                .next_frame
                                .scene
                                .push_fiber_segment(after_segment);
                            pushed_after_segment = true;
                            frame.after_segment = Some(after_segment);
                            frame.segment_list.push(after_segment);
                        }
                    }

                    let mut ctx = crate::PaintCtx {
                        fiber_id,
                        bounds: frame.bounds,
                        child_bounds: Vec::new(), // Not needed in paint_end
                        inspector_id: None,
                        window: window.window,
                        cx,
                    };
                    node.paint_end(&mut ctx, render_node_frame);

                    if pushed_after_segment {
                        window.window.next_frame.scene.pop_segment();
                    }
                }

                // Put the node back
                if let Some(node) = render_node {
                    window
                        .window
                        .fiber
                        .tree
                        .render_nodes
                        .insert(fiber_id.into(), node);
                }
            } else {
                // Legacy style-based after-children painting (e.g. borders) that needs to happen
                // in a dedicated after-segment.
                window.paint_after_children_segment(&mut frame, cx);
            }

            if frame.paint_scene {
                window.store_paint_results(&frame.fiber_id, &frame);
            }

            log::trace!(
                "[PAINT_EXIT] fiber_id={:?} segments={:?} paint_scene={}",
                fiber_id,
                frame.segment_list,
                frame.paint_scene
            );

            unwind_paint_frame(window, &frame);
            frame.stack_state.restore(window.window);

            if let Some(parent) = state.frame_stack.last_mut() {
                parent
                    .segment_list
                    .extend(frame.segment_list.iter().copied());
            }
        }

        fn paint_children(
            state: &mut PaintTraversal<'_>,
            fiber_id: GlobalElementId,
        ) -> SmallVec<[GlobalElementId; 8]> {
            let window = state.ctx.handle_mut();
            window.window.fiber.tree.children(&fiber_id).collect()
        }

        let mut traversal = PaintTraversal {
            ctx: TraversalCtx::new(self, cx),
            scale_factor,
            bounds_cache,
            frame_stack: Vec::new(),
            deferred_pass,
        };

        traverse_fibers(
            root,
            &mut traversal,
            enter_paint,
            exit_paint,
            paint_children,
        );
        debug_assert!(
            traversal.frame_stack.is_empty(),
            "paint traversal frame stack not fully unwound"
        );
        // Return the bounds cache to Window for reuse
        let bounds_cache = traversal.bounds_cache;
        traversal
            .ctx
            .with_mut(|handle, _cx| handle.window.fiber.layout_bounds_cache = bounds_cache);

        #[cfg(debug_assertions)]
        debug_assert_eq!(
            self.window.fiber.fiber_id_stack.len(),
            initial_fiber_id_stack_len,
            "fiber_id_stack not balanced after paint: started with {}, ended with {}",
            initial_fiber_id_stack_len,
            self.window.fiber.fiber_id_stack.len()
        );
    }

    pub fn set_cursor_style(&mut self, style: CursorStyle, hitbox: &Hitbox) {
        self.window.invalidator.debug_assert_paint();
        let entry = self
            .window
            .fiber
            .tree
            .cursor_styles
            .entry(hitbox.id.into())
            .expect("set_cursor_style requires a valid fiber");
        let entry = entry.or_insert_with(SmallVec::new);
        entry.push(CursorStyleRequest {
            hitbox_id: Some(hitbox.id),
            style,
        });
        let fiber_id: GlobalElementId = hitbox.id.into();
        self.window.fiber.active_cursor_styles.insert(fiber_id);
    }

    pub(crate) fn set_window_cursor_style_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        style: CursorStyle,
    ) {
        self.window.invalidator.debug_assert_paint();
        let entry = self
            .window
            .fiber
            .tree
            .cursor_styles
            .entry(fiber_id.into())
            .expect("set_window_cursor_style requires a valid fiber");
        let entry = entry.or_insert_with(SmallVec::new);
        entry.push(CursorStyleRequest {
            hitbox_id: None,
            style,
        });
        self.window.fiber.active_cursor_styles.insert(fiber_id);
    }

    pub(crate) fn set_tooltip_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        tooltip: AnyTooltip,
    ) -> TooltipId {
        self.window.invalidator.debug_assert_layout_or_prepaint();
        let id = self.window.next_tooltip_id.next();
        let entry = self
            .window
            .fiber
            .tree
            .tooltips
            .entry(fiber_id.into())
            .expect("set_tooltip requires a valid fiber");
        let entry = entry.or_insert_with(SmallVec::new);
        entry.push(TooltipRequest { id, tooltip });
        self.window.fiber.active_tooltips.insert(fiber_id);
        id
    }

    pub fn with_tab_group<R>(
        &mut self,
        index: Option<isize>,
        f: impl FnOnce(&mut Window) -> R,
    ) -> R {
        if let Some(index) = index {
            self.window.fiber.rendered_tab_stops.begin_group(index);
            let result = f(self.window);
            self.window.fiber.rendered_tab_stops.end_group();
            result
        } else {
            f(self.window)
        }
    }

    pub(crate) fn register_tab_stop_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        focus_handle: &FocusHandle,
        tab_index: isize,
    ) {
        self.window.invalidator.debug_assert_paint();
        let handle = focus_handle.clone().tab_stop(true).tab_index(tab_index);
        self.register_tab_stop_handle_for_fiber(fiber_id, &handle);
    }

    pub(crate) fn register_tab_stop_handle_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        focus_handle: &FocusHandle,
    ) {
        self.window.invalidator.debug_assert_paint();
        let sequence = {
            let entry = self
                .window
                .fiber
                .tree
                .tab_stops
                .entry(fiber_id.into())
                .expect("register_tab_stop_handle requires a valid fiber");
            let entry = entry.or_insert_with(SmallVec::new);
            let sequence = entry.len() as u32;
            entry.push(focus_handle.id);
            sequence
        };
        let preorder_index = self
            .window
            .fiber
            .tree
            .preorder_index(&fiber_id)
            .unwrap_or(u64::MAX);
        let order_key = crate::tab_stop::TabStopOrderKey::new(preorder_index, sequence);
        self.window
            .fiber
            .rendered_tab_stops
            .insert_with_order(fiber_id, focus_handle, order_key);
    }

    pub(crate) fn defer_draw_for_fiber(
        &mut self,
        owner_id: GlobalElementId,
        element: AnyElement,
        absolute_offset: Point<Pixels>,
        priority: usize,
        layout_constraints: Size<AvailableSpace>,
        group_key: usize,
        callsite: &'static core::panic::Location<'static>,
    ) {
        self.window.invalidator.debug_assert_layout_or_prepaint();
        let current_view = self.window.current_view();
        let text_style_stack = self.window.text_style_stack.clone();
        let rem_size = self.window.rem_size();
        let reference_fiber = self.window.current_fiber_id();
        let reference_origin = reference_fiber
            .as_ref()
            .and_then(|id| self.window.fiber.tree.bounds.get((*id).into()))
            .map(|bounds| bounds.origin);
        let local_offset = if let Some(origin) = reference_origin {
            absolute_offset - origin
        } else {
            absolute_offset
        };

        // Combine callsite location with group_key to create a unique overlay key.
        // The constant is 2^64/φ (golden ratio); multiplying spreads group_key bits
        // evenly before XOR, avoiding collisions when the same callsite is used
        // with sequential group_key values (e.g., in a loop).
        let callsite_key = (callsite as *const core::panic::Location<'static>) as usize;
        let callsite_key = callsite_key ^ group_key.wrapping_mul(0x9e37_79b9_7f4a_7c15);
        let (slot, existing_root) = {
            let groups = self
                .window
                .fiber
                .tree
                .deferred_draw_overlay_groups
                .entry(owner_id.into())
                .expect("defer_draw requires a valid fiber")
                .or_insert_with(FxHashMap::default);
            let group = groups
                .entry(callsite_key)
                .or_insert_with(DeferredDrawOverlayGroup::default);
            let slot = group.next_index;
            group.next_index += 1;
            (slot, group.roots.get(slot).copied())
        };

        let overlay_root = if let Some(existing) = existing_root
            && self.window.fiber.tree.get(&existing).is_some()
        {
            existing
        } else {
            let new_root = self.window.fiber.tree.create_placeholder_fiber();
        let groups = self
            .window
            .fiber
            .tree
            .deferred_draw_overlay_groups
                .entry(owner_id.into())
                .expect("defer_draw requires a valid fiber")
                .or_insert_with(FxHashMap::default);
            let group = groups
                .entry(callsite_key)
                .or_insert_with(DeferredDrawOverlayGroup::default);
            if slot < group.roots.len() {
                group.roots[slot] = new_root;
            } else {
                group.roots.push(new_root);
            }
            new_root
        };

        let entry = self
            .window
            .fiber
            .tree
            .deferred_draws
            .entry(owner_id.into())
            .expect("defer_draw requires a valid fiber");
        let entry = entry.or_insert_with(SmallVec::new);
        entry.push(DeferredDraw {
            current_view,
            text_style_stack,
            rem_size,
            priority,
            element: Some(element),
            fiber_id: Some(overlay_root),
            reference_fiber,
            local_offset,
            requires_layout: true,
            layout_constraints,
        });
        self.window.fiber.active_deferred_draws.insert(owner_id);
    }

    pub(crate) fn handle_input_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        focus_handle: &FocusHandle,
        input_handler: impl InputHandler,
        cx: &App,
    ) {
        self.window.invalidator.debug_assert_paint();
        self.register_input_handler_for_fiber(fiber_id, focus_handle, input_handler, cx);
    }

    pub(crate) fn defer_fiber_draw(
        &mut self,
        fiber_id: &GlobalElementId,
        absolute_offset: Point<Pixels>,
        priority: usize,
    ) {
        self.window.invalidator.debug_assert_layout_or_prepaint();
        let current_view = self.window.current_view();
        let text_style_stack = self.window.text_style_stack.clone();
        let rem_size = self.window.rem_size();
        let entry = self
            .window
            .fiber
            .tree
            .deferred_draws
            .entry((*fiber_id).into())
            .expect("defer_fiber_draw requires a valid fiber");
        let entry = entry.or_insert_with(SmallVec::new);
        entry.push(DeferredDraw {
            current_view,
            text_style_stack,
            rem_size,
            priority,
            element: None,
            fiber_id: Some(*fiber_id),
            reference_fiber: None,
            local_offset: absolute_offset,
            requires_layout: false,
            layout_constraints: AvailableSpace::min_size(),
        });
        self.window.fiber.active_deferred_draws.insert(*fiber_id);
    }

    pub fn transact<T, U>(&mut self, f: impl FnOnce(&mut Window) -> Result<T, U>) -> Result<T, U> {
        self.window.invalidator.debug_assert_layout_or_prepaint();
        let hitboxes_len = self.window.fiber.hitbox_stack.len();
        let line_layout_index = self.window.text_system().layout_index();
        let result = f(self.window);
        if result.is_err() {
            self.truncate_hitboxes(hitboxes_len);
            self.window
                .text_system()
                .truncate_layouts(line_layout_index);
        }
        result
    }
}

use crate::InteractiveElementState;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct HitboxData {
    pub(crate) transform_id: TransformId,
    pub(crate) bounds: Bounds<Pixels>,
    pub(crate) content_mask: ContentMask<Pixels>,
    pub(crate) behavior: HitboxBehavior,
}

impl FiberRuntimeHandleRef<'_> {
    pub(super) fn focus_path_for(&self, focus_id: Option<FocusId>) -> SmallVec<[FocusId; 8]> {
        let Some(focus_id) = focus_id else {
            return SmallVec::new();
        };
        let Some(fiber_id) = self
            .window
            .fiber
            .tree
            .focusable_fibers
            .get(&focus_id)
            .copied()
        else {
            return SmallVec::new();
        };
        let mut items = SmallVec::new();
        for ancestor_id in path_to_root_smallvec(&self.window.fiber.tree, fiber_id) {
            if let Some(&focus_id) = self.window.fiber.tree.focus_ids.get(ancestor_id.into()) {
                items.push(focus_id);
            }
        }
        items
    }

    pub(super) fn context_stack_for_node(&self, node_id: GlobalElementId) -> Vec<KeyContext> {
        let mut items = Vec::new();
        for fiber_id in path_to_root_smallvec(&self.window.fiber.tree, node_id) {
            if let Some(key_context) = self.window.fiber.tree.key_contexts.get(fiber_id.into()) {
                items.push(key_context.clone());
            }
        }
        items
    }

    fn any_ancestor(
        &self,
        start: Option<GlobalElementId>,
        mut predicate: impl FnMut(GlobalElementId) -> bool,
    ) -> bool {
        let mut current = start;
        while let Some(fiber_id) = current {
            if predicate(fiber_id) {
                return true;
            }
            current = self.window.fiber.tree.parent(&fiber_id);
        }
        false
    }

    pub(super) fn focus_contains(&self, parent: FocusId, child: FocusId) -> bool {
        if parent == child {
            return true;
        }
        let Some(parent_fiber) = self
            .window
            .fiber
            .tree
            .focusable_fibers
            .get(&parent)
            .copied()
        else {
            return false;
        };
        let child_fiber = self.window.fiber.tree.focusable_fibers.get(&child).copied();
        self.any_ancestor(child_fiber, |fiber_id| fiber_id == parent_fiber)
    }

    pub(super) fn focus_node_id_in_rendered_frame(
        &self,
        focus_id: Option<FocusId>,
    ) -> GlobalElementId {
        if let Some(focus_id) = focus_id {
            if let Some(fiber_id) = self
                .window
                .fiber
                .tree
                .focusable_fibers
                .get(&focus_id)
                .copied()
            {
                return fiber_id;
            }
            if let Some((key, _)) = self
                .window
                .fiber
                .tree
                .focus_ids
                .iter()
                .find(|&(_, fid)| *fid == focus_id)
            {
                return GlobalElementId::from(key);
            }
        }
        self.window
            .fiber
            .tree
            .root
            .expect("focus dispatch requires a root fiber")
    }

    pub(super) fn context_stack_for_focus_handle(
        &self,
        focus_handle: &FocusHandle,
    ) -> Option<Vec<KeyContext>> {
        let node_id = self
            .window
            .fiber
            .tree
            .focusable_fibers
            .get(&focus_handle.id)
            .copied()?;
        Some(self.context_stack_for_node(node_id))
    }

    pub(super) fn is_action_available_for_node(
        &self,
        action: &dyn Action,
        node_id: GlobalElementId,
    ) -> bool {
        let action_type = action.as_any().type_id();
        self.any_ancestor(Some(node_id), |fiber_id| {
            self.window
                .fiber
                .tree
                .effects
                .get(fiber_id.into())
                .is_some_and(|effects| {
                    effects
                        .action_listeners
                        .iter()
                        .any(|(ty, _)| *ty == action_type)
                })
        })
    }

    pub(super) fn parent_for(&self, fiber_id: &GlobalElementId) -> Option<GlobalElementId> {
        self.window.fiber.tree.parent(fiber_id)
    }

    pub(super) fn next_tab_stop(&self, focus: Option<&FocusId>) -> Option<FocusHandle> {
        self.window.fiber.rendered_tab_stops.next(focus)
    }

    pub(super) fn prev_tab_stop(&self, focus: Option<&FocusId>) -> Option<FocusHandle> {
        self.window.fiber.rendered_tab_stops.prev(focus)
    }
}

impl FiberRuntimeHandle<'_> {
    pub(crate) fn register_key_context_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        context: KeyContext,
    ) {
        if self.window.fiber.tree.get(&fiber_id).is_some() {
            self.window
                .fiber
                .tree
                .key_contexts
                .insert(fiber_id.into(), context);
        }
    }

    pub(crate) fn register_focus_handle_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        focus_handle: &FocusHandle,
    ) {
        if self.window.effects_suppressed() {
            return;
        }
        if let Some(existing) = self
            .window
            .fiber
            .tree
            .focusable_fibers
            .insert(focus_handle.id, fiber_id)
            && existing != fiber_id
        {
            self.window.fiber.tree.focus_ids.remove(existing.into());
        }
        self.window
            .fiber
            .tree
            .focus_ids
            .insert(fiber_id.into(), focus_handle.id);
    }

    pub(crate) fn register_input_handler_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        focus_handle: &FocusHandle,
        input_handler: impl InputHandler,
        _cx: &App,
    ) {
        if self.window.effects_suppressed() {
            return;
        }
        if let Some(existing) = self
            .window
            .fiber
            .tree
            .focusable_fibers
            .insert(focus_handle.id, fiber_id)
            && existing != fiber_id
        {
            self.window.fiber.tree.focus_ids.remove(existing.into());
        }
        self.window
            .fiber
            .tree
            .focus_ids
            .insert(fiber_id.into(), focus_handle.id);
        self.window
            .fiber
            .tree
            .input_handlers
            .insert(fiber_id.into(), Box::new(input_handler));
        self.window.fiber.active_input_handlers.insert(fiber_id);
    }

    pub(crate) fn register_any_mouse_listener_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        listener: crate::AnyMouseListener,
    ) {
        let entry = self
            .window
            .fiber
            .tree
            .effects
            .entry(fiber_id.into())
            .expect("register_any_mouse_listener_for_fiber requires a valid fiber");
        let effects = entry.or_insert_with(FiberEffects::new);
        effects.any_mouse_listeners.push(listener);
        self.window.fiber.active_mouse_listeners.insert(fiber_id);
    }

    pub(crate) fn register_key_listener_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        listener: crate::KeyListener,
    ) {
        let entry = self
            .window
            .fiber
            .tree
            .effects
            .entry(fiber_id.into())
            .expect("register_key_listener_for_fiber requires a valid fiber");
        let effects = entry.or_insert_with(FiberEffects::new);
        effects.key_listeners.push(listener);
    }

    pub(crate) fn register_modifiers_changed_listener_for_fiber(
        &mut self,
        fiber_id: GlobalElementId,
        listener: crate::ModifiersChangedListener,
    ) {
        let entry = self
            .window
            .fiber
            .tree
            .effects
            .entry(fiber_id.into())
            .expect("register_modifiers_changed_listener_for_fiber requires a valid fiber");
        let effects = entry.or_insert_with(FiberEffects::new);
        effects.modifiers_changed_listeners.push(listener);
    }

    /// Insert a hitbox with an associated fiber ID for dispatch lookup.
    /// Event handlers are looked up from the fiber's effects during dispatch.
    pub fn insert_hitbox_with_fiber(
        &mut self,
        bounds: Bounds<Pixels>,
        behavior: HitboxBehavior,
        fiber_id: GlobalElementId,
    ) -> Hitbox {
        self.window.invalidator.debug_assert_layout_or_prepaint();

        #[cfg(debug_assertions)]
        if self
            .window
            .fiber
            .tree
            .fibers
            .get(fiber_id.into())
            .is_some_and(|fiber| fiber.kind == FiberKind::StateOnly)
        {
            panic!(
                "attempted to insert hitbox on StateOnly fiber {:?}. StateOnly fibers are scope-only and are not hit-testable.",
                fiber_id
            );
        }

        let content_mask = PrepaintCx::new(self.window).content_mask();
        if self.window.effects_suppressed() {
            return Hitbox {
                id: fiber_id.into(),
                bounds,
                content_mask,
                behavior,
            };
        }

        let transform_id = self.window.transform_stack.current();
        if let Some(state) = self.window.fiber.tree.hitbox_state.get_mut(fiber_id.into()) {
            state.hitbox = Some(HitboxData {
                transform_id,
                bounds,
                content_mask: content_mask.clone(),
                behavior,
            });
        }
        self.window.fiber.hitbox_stack.push(fiber_id.into());

        Hitbox {
            id: fiber_id.into(),
            bounds,
            content_mask,
            behavior,
        }
    }

    pub(super) fn dispatch_mouse_event(&mut self, event: &dyn Any, cx: &mut App) {
        let previous_hit_test = HitTest {
            ids: self.window.mouse_hit_test.ids.clone(),
            hover_hitbox_count: self.window.mouse_hit_test.hover_hitbox_count,
        };

        // Use Option to avoid cloning in the can_reuse case (the common path for mouse moves
        // within the same element). None means "reuse existing hit test unchanged".
        let hit_test: Option<HitTest> = if event.is::<crate::MouseExitEvent>() {
            Some(HitTest::default())
        } else if let Some(mouse_move) = event.downcast_ref::<MouseMoveEvent>() {
            // Mouse move is by far the hottest path. Avoid a full fiber-tree hit-test when
            // the cursor is still within the current hovered hitboxes.
            let viewport = self.window.viewport_size();
            if mouse_move.position.x < crate::px(0.)
                || mouse_move.position.y < crate::px(0.)
                || mouse_move.position.x >= viewport.width
                || mouse_move.position.y >= viewport.height
            {
                Some(HitTest::default())
            } else {
                let scale_factor = self.window.scale_factor();
                let transforms = &self.window.segment_pool.transforms;
                let world_scaled = Point::new(
                    ScaledPixels(mouse_move.position.x.0 * scale_factor),
                    ScaledPixels(mouse_move.position.y.0 * scale_factor),
                );
                let hitbox_contains_point = |hitbox: &crate::window::HitboxSnapshot| {
                    if !hitbox.content_mask.bounds.contains(&mouse_move.position) {
                        return false;
                    }

                    let local_scaled =
                        transforms.world_to_local_no_cache(hitbox.transform_id, world_scaled);
                    let local_point = Point::new(
                        Pixels(local_scaled.x.0 / scale_factor),
                        Pixels(local_scaled.y.0 / scale_factor),
                    );
                    hitbox.bounds.contains(&local_point)
                };

                let entered_deferred_overlay =
                    mouse_move.pressed_button.is_none()
                        && cx.active_drag.is_none()
                        && !self.window.fiber.active_deferred_draws.members.is_empty()
                        && self.window.fiber.active_deferred_draws.members.iter().any(
                            |deferred_id| {
                                if self.window.fiber.tree.get(deferred_id).is_none() {
                                    return false;
                                };
                                if !self
                                    .window
                                    .fiber
                                    .tree
                                    .deferred_priorities
                                    .contains_key((*deferred_id).into())
                                {
                                    return false;
                                }

                                let hitbox_id: HitboxId = (*deferred_id).into();
                                if self
                                    .window
                                    .mouse_hit_test
                                    .ids
                                    .iter()
                                    .take(self.window.mouse_hit_test.hover_hitbox_count)
                                    .any(|id| *id == hitbox_id)
                                {
                                    return false;
                                }

                                let Some(hitbox) =
                                    self.window.rendered_frame.hitboxes.get(&hitbox_id)
                                else {
                                    return false;
                                };
                                hitbox_contains_point(hitbox)
                            },
                        );

                let can_reuse_hit_test = mouse_move.pressed_button.is_none()
                    && cx.active_drag.is_none()
                    && !entered_deferred_overlay
                    && !self.window.mouse_hit_test.ids.is_empty()
                    && self
                        .window
                        .mouse_hit_test
                        .ids
                        .iter()
                        .take(self.window.mouse_hit_test.hover_hitbox_count)
                        .all(|hitbox_id| {
                            let Some(hitbox) = self.window.rendered_frame.hitboxes.get(hitbox_id)
                            else {
                                return false;
                            };
                            hitbox_contains_point(hitbox)
                        });

                if can_reuse_hit_test {
                    None // Reuse existing hit test, no clone needed
                } else {
                    Some(
                        self.window
                            .rendered_frame
                            .hit_test(self.window, mouse_move.position),
                    )
                }
            }
        } else {
            Some(
                self.window
                    .rendered_frame
                    .hit_test(self.window, self.window.mouse_position()),
            )
        };

        // Track whether hit test actually changed (for dispatch optimization below)
        let hit_test_changed = if let Some(ref new_hit_test) = hit_test {
            *new_hit_test != self.window.mouse_hit_test
        } else {
            false
        };

        if let Some(new_hit_test) = hit_test {
            if new_hit_test != self.window.mouse_hit_test {
                self.window.mouse_hit_test = new_hit_test;
                self.window.reset_cursor_style(cx);
            }
        }

        #[cfg(any(feature = "inspector", debug_assertions))]
        if self.window.is_inspector_picking(cx) {
            self.window.handle_inspector_mouse_event(event, cx);
            // When inspector is picking, all other mouse handling is skipped.
            return;
        }

        // Dispatch to fiber handlers for mouse events.
        // Optimization: if hit test didn't change, previous == current, so we can avoid
        // cloning current_hit_test and just use previous_hit_test for both.
        if cx.propagate_event {
            if hit_test_changed {
                let current_hit_test = HitTest {
                    ids: self.window.mouse_hit_test.ids.clone(),
                    hover_hitbox_count: self.window.mouse_hit_test.hover_hitbox_count,
                };
                if let Some(mouse_exit) = event.downcast_ref::<crate::MouseExitEvent>() {
                    let synthetic_move = MouseMoveEvent {
                        position: mouse_exit.position,
                        pressed_button: mouse_exit.pressed_button,
                        modifiers: mouse_exit.modifiers,
                    };
                    self.dispatch_fiber_mouse_event(
                        &synthetic_move,
                        &previous_hit_test,
                        &current_hit_test,
                        cx,
                    );
                } else {
                    self.dispatch_fiber_mouse_event(
                        event,
                        &previous_hit_test,
                        &current_hit_test,
                        cx,
                    );
                }
            } else {
                // Hit test unchanged - previous == current, no extra clone needed
                if let Some(mouse_exit) = event.downcast_ref::<crate::MouseExitEvent>() {
                    let synthetic_move = MouseMoveEvent {
                        position: mouse_exit.position,
                        pressed_button: mouse_exit.pressed_button,
                        modifiers: mouse_exit.modifiers,
                    };
                    self.dispatch_fiber_mouse_event(
                        &synthetic_move,
                        &previous_hit_test,
                        &previous_hit_test,
                        cx,
                    );
                } else {
                    self.dispatch_fiber_mouse_event(
                        event,
                        &previous_hit_test,
                        &previous_hit_test,
                        cx,
                    );
                }
            }
        }

        // Some events (e.g. scroll) can move hitboxes without moving the mouse. After handlers run,
        // refresh the window's hit test and hover state so hover styling/cursor updates track the
        // content under the mouse.
        self.window.apply_pending_mouse_hit_test_refresh(cx);

        if cx.has_active_drag() {
            if event.is::<MouseMoveEvent>() {
                // If this was a mouse move event, redraw the window so that the
                // active drag can follow the mouse cursor.
                self.window.request_redraw();
            } else if event.is::<MouseUpEvent>() {
                // If this was a mouse up event, cancel the active drag and redraw
                // the window.
                cx.active_drag = None;
                self.window.request_redraw();
            }
        }
    }

    fn dispatch_fiber_mouse_event(
        &mut self,
        event: &dyn Any,
        previous_hit_test: &HitTest,
        current_hit_test: &HitTest,
        cx: &mut App,
    ) {
        use crate::MouseDownEvent;

        if let Some(event) = event.downcast_ref::<MouseMoveEvent>() {
            let fast_path = event.pressed_button.is_none() && cx.active_drag.is_none();
            if fast_path {
                let mut target_hitboxes: SmallVec<[HitboxId; 16]> = SmallVec::new();
                let mut seen_hitboxes: FxHashSet<HitboxId> = FxHashSet::default();
                for id in current_hit_test
                    .ids
                    .iter()
                    .take(current_hit_test.hover_hitbox_count)
                    .copied()
                {
                    if seen_hitboxes.insert(id) {
                        target_hitboxes.push(id);
                    }
                }
                for id in previous_hit_test
                    .ids
                    .iter()
                    .take(previous_hit_test.hover_hitbox_count)
                    .copied()
                {
                    if seen_hitboxes.insert(id) {
                        target_hitboxes.push(id);
                    }
                }

                let mut targets: SmallVec<[FiberDispatchTarget; 8]> = SmallVec::new();
                for hitbox_id in target_hitboxes {
                    let fiber_id: GlobalElementId = hitbox_id.into();
                    let Some(hitbox) = self.window.resolve_hitbox_for_event(&fiber_id) else {
                        continue;
                    };
                    if self.window.fiber.tree.get(&fiber_id).is_none() {
                        continue;
                    };
                    let effects = self.window.fiber.tree.effects.get(fiber_id.into());
                    let interactivity = self
                        .window
                        .fiber
                        .tree
                        .render_nodes
                        .get(fiber_id.into())
                        .and_then(|node| node.interactivity());
                    if !has_mouse_effects(interactivity, effects) {
                        continue;
                    }
                    let interactivity = interactivity.map(|interactivity| FiberInteractivityInfo {
                        hover_style: interactivity.hover_style.is_some(),
                        group_hover: interactivity
                            .group_hover_style
                            .as_ref()
                            .map(|group| group.group.clone()),
                        group_active: interactivity
                            .group_active_style
                            .as_ref()
                            .map(|group| group.group.clone()),
                        drag_over_styles: !interactivity.drag_over_styles.is_empty(),
                        focus_handle: interactivity.tracked_focus_handle.clone(),
                        tooltip_id: interactivity.tooltip_id,
                    });
                    targets.push(FiberDispatchTarget {
                        fiber_id,
                        hitbox,
                        interactivity,
                    });
                }

                if targets.is_empty() {
                    return;
                }

                self.dispatch_fiber_any_mouse_listeners(event, &targets, cx);
                self.dispatch_fiber_mouse_move(event, &targets, cx);
                return;
            }
        }

        if let Some(event) = event.downcast_ref::<crate::ScrollWheelEvent>() {
            let mut targets: SmallVec<[FiberDispatchTarget; 8]> = SmallVec::new();
            for hitbox_id in current_hit_test.ids.iter().copied() {
                let fiber_id: GlobalElementId = hitbox_id.into();
                let Some(hitbox) = self.window.resolve_hitbox_for_event(&fiber_id) else {
                    continue;
                };
                if self.window.fiber.tree.get(&fiber_id).is_none() {
                    continue;
                };
                let effects = self.window.fiber.tree.effects.get(fiber_id.into());
                let interactivity = self
                    .window
                    .fiber
                    .tree
                    .render_nodes
                    .get(fiber_id.into())
                    .and_then(|node| node.interactivity());
                if !has_mouse_effects(interactivity, effects) {
                    continue;
                }
                let interactivity = interactivity.map(|interactivity| FiberInteractivityInfo {
                    hover_style: interactivity.hover_style.is_some(),
                    group_hover: interactivity
                        .group_hover_style
                        .as_ref()
                        .map(|group| group.group.clone()),
                    group_active: interactivity
                        .group_active_style
                        .as_ref()
                        .map(|group| group.group.clone()),
                    drag_over_styles: !interactivity.drag_over_styles.is_empty(),
                    focus_handle: interactivity.tracked_focus_handle.clone(),
                    tooltip_id: interactivity.tooltip_id,
                });
                targets.push(FiberDispatchTarget {
                    fiber_id,
                    hitbox,
                    interactivity,
                });
            }

            if targets.is_empty() {
                return;
            }

            self.dispatch_fiber_any_mouse_listeners(event, &targets, cx);
            self.dispatch_fiber_scroll_wheel(event, &targets, cx);
            return;
        }

        let mut active_mouse_listeners = std::mem::replace(
            &mut self.window.fiber.active_mouse_listeners,
            ActiveFiberList::new(),
        );
        active_mouse_listeners.rebuild_if_needed(&mut self.window.fiber.tree);
        self.window.fiber.active_mouse_listeners = active_mouse_listeners;
        let mut targets: SmallVec<[FiberDispatchTarget; 8]> = SmallVec::new();

        for fiber_id in self
            .window
            .fiber
            .active_mouse_listeners
            .ordered
            .iter()
            .copied()
        {
            let Some(hitbox) = self.window.resolve_hitbox_for_event(&fiber_id) else {
                continue;
            };
            if self.window.fiber.tree.get(&fiber_id).is_none() {
                continue;
            };
            let effects = self.window.fiber.tree.effects.get(fiber_id.into());
            // Get interactivity from render node
            let interactivity = self
                .window
                .fiber
                .tree
                .render_nodes
                .get(fiber_id.into())
                .and_then(|node| node.interactivity());
            let has_mouse_effects = has_mouse_effects(interactivity, effects);
            if !has_mouse_effects {
                continue;
            }
            let interactivity = interactivity.map(|interactivity| FiberInteractivityInfo {
                hover_style: interactivity.hover_style.is_some(),
                group_hover: interactivity
                    .group_hover_style
                    .as_ref()
                    .map(|group| group.group.clone()),
                group_active: interactivity
                    .group_active_style
                    .as_ref()
                    .map(|group| group.group.clone()),
                drag_over_styles: !interactivity.drag_over_styles.is_empty(),
                focus_handle: interactivity.tracked_focus_handle.clone(),
                tooltip_id: interactivity.tooltip_id,
            });
            targets.push(FiberDispatchTarget {
                fiber_id,
                hitbox,
                interactivity,
            });
        }

        if targets.is_empty() {
            return;
        }

        targets.reverse();

        self.dispatch_fiber_any_mouse_listeners(event, &targets, cx);

        // Dispatch based on event type
        if let Some(e) = event.downcast_ref::<MouseDownEvent>() {
            self.dispatch_fiber_mouse_down(e, &targets, cx);
        } else if let Some(e) = event.downcast_ref::<MouseUpEvent>() {
            self.dispatch_fiber_mouse_up(e, &targets, cx);
        } else if let Some(e) = event.downcast_ref::<MouseMoveEvent>() {
            self.dispatch_fiber_mouse_move(e, &targets, cx);
        } else if let Some(e) = event.downcast_ref::<crate::MousePressureEvent>() {
            self.dispatch_fiber_mouse_pressure(e, &targets, cx);
        } else if let Some(e) = event.downcast_ref::<crate::ScrollWheelEvent>() {
            self.dispatch_fiber_scroll_wheel(e, &targets, cx);
        }
    }

    fn dispatch_fiber_any_mouse_listeners(
        &mut self,
        event: &dyn Any,
        targets: &[FiberDispatchTarget],
        cx: &mut App,
    ) {
        for target in targets.iter().rev() {
            if !cx.propagate_event {
                break;
            }
            let listeners = self
                .with_fiber_effects_mut(&target.fiber_id, |effects| {
                    mem::take(&mut effects.any_mouse_listeners)
                })
                .unwrap_or_default();
            for listener in &listeners {
                (listener.borrow_mut())(event, DispatchPhase::Capture, self.window, cx);
                if !cx.propagate_event {
                    break;
                }
            }
            let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                effects.any_mouse_listeners = listeners;
            });
        }

        if cx.propagate_event {
            for target in targets.iter() {
                if !cx.propagate_event {
                    break;
                }
                let listeners = self
                    .with_fiber_effects_mut(&target.fiber_id, |effects| {
                        mem::take(&mut effects.any_mouse_listeners)
                    })
                    .unwrap_or_default();
                for listener in &listeners {
                    (listener.borrow_mut())(event, DispatchPhase::Bubble, self.window, cx);
                    if !cx.propagate_event {
                        break;
                    }
                }
                let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                    effects.any_mouse_listeners = listeners;
                });
            }
        }
    }

    fn dispatch_fiber_mouse_down(
        &mut self,
        event: &crate::MouseDownEvent,
        targets: &[FiberDispatchTarget],
        cx: &mut App,
    ) {
        // Capture phase (reverse order = outer to inner)
        for target in targets.iter().rev() {
            if !cx.propagate_event {
                break;
            }
            let listeners = self
                .with_fiber_effects_mut(&target.fiber_id, |effects| {
                    mem::take(&mut effects.mouse_down_listeners)
                })
                .unwrap_or_default();
            for listener in &listeners {
                listener(
                    event,
                    DispatchPhase::Capture,
                    &target.hitbox,
                    self.window,
                    cx,
                );
                if !cx.propagate_event {
                    break;
                }
            }
            let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                effects.mouse_down_listeners = listeners;
            });

            if !cx.propagate_event {
                break;
            }

            self.dispatch_fiber_tooltip_clear(target, cx);
        }

        // Bubble phase (normal order = inner to outer)
        if cx.propagate_event {
            for target in targets.iter() {
                if !cx.propagate_event {
                    break;
                }
                let has_click_or_drag = self
                    .window
                    .get_fiber_effects(&target.fiber_id)
                    .is_some_and(|effects| {
                        !effects.click_listeners.is_empty() || effects.drag_listener.is_some()
                    });
                let group_active_hitbox = target
                    .interactivity
                    .as_ref()
                    .and_then(|info| info.group_active.as_ref())
                    .and_then(|group| GroupHitboxes::get(group, cx));

                let _ = self
                    .window
                    .with_element_state_in_event::<InteractiveElementState, _>(
                        &target.fiber_id,
                        |element_state, window| {
                            let mut element_state = element_state.unwrap_or_default();
                            let is_hovered = target.hitbox.is_hovered(window);
                            if !window.default_prevented() {
                                let clicked_state = element_state
                                    .clicked_state
                                    .get_or_insert_with(Default::default)
                                    .clone();
                                if !clicked_state.borrow().is_clicked() {
                                    let group_hovered =
                                        group_active_hitbox.is_some_and(|hitbox_id| {
                                            window.hitbox_is_hovered(hitbox_id)
                                        });
                                    if group_hovered || is_hovered {
                                        *clicked_state.borrow_mut() = ElementClickedState {
                                            group: group_hovered,
                                            element: is_hovered,
                                        };
                                        window.invalidate_fiber_paint(target.fiber_id);
                                    }
                                }
                            }

                            if has_click_or_drag && event.button == MouseButton::Left && is_hovered
                            {
                                let pending_mouse_down = element_state
                                    .pending_mouse_down
                                    .get_or_insert_with(Default::default)
                                    .clone();
                                *pending_mouse_down.borrow_mut() = Some(event.clone());
                                window.invalidate_fiber_paint(target.fiber_id);
                            }
                            ((), element_state)
                        },
                    );

                let listeners = self
                    .with_fiber_effects_mut(&target.fiber_id, |effects| {
                        mem::take(&mut effects.mouse_down_listeners)
                    })
                    .unwrap_or_default();
                for listener in &listeners {
                    listener(
                        event,
                        DispatchPhase::Bubble,
                        &target.hitbox,
                        self.window,
                        cx,
                    );
                    if !cx.propagate_event {
                        break;
                    }
                }
                let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                    effects.mouse_down_listeners = listeners;
                });

                if !cx.propagate_event {
                    break;
                }

                if let Some(focus_handle) = target
                    .interactivity
                    .as_ref()
                    .and_then(|info| info.focus_handle.as_ref())
                {
                    if target.hitbox.is_hovered(self.window) && !self.window.default_prevented() {
                        self.window.focus(focus_handle, cx);
                        self.window.prevent_default();
                    }
                }
            }
        }
    }

    fn dispatch_fiber_mouse_up(
        &mut self,
        event: &MouseUpEvent,
        targets: &[FiberDispatchTarget],
        cx: &mut App,
    ) {
        let mut captured_mouse_down: SmallVec<[Option<MouseDownEvent>; 8]> =
            SmallVec::from_elem(None, targets.len());

        for (index, target) in targets.iter().enumerate().rev() {
            if !cx.propagate_event {
                break;
            }
            let listeners = self
                .with_fiber_effects_mut(&target.fiber_id, |effects| {
                    mem::take(&mut effects.mouse_up_listeners)
                })
                .unwrap_or_default();
            for listener in &listeners {
                listener(
                    event,
                    DispatchPhase::Capture,
                    &target.hitbox,
                    self.window,
                    cx,
                );
                if !cx.propagate_event {
                    break;
                }
            }
            let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                effects.mouse_up_listeners = listeners;
            });

            if !cx.propagate_event {
                break;
            }

            let has_click_or_drag =
                self.window
                    .get_fiber_effects(&target.fiber_id)
                    .is_some_and(|effects| {
                        !effects.click_listeners.is_empty() || effects.drag_listener.is_some()
                    });

            let captured = self
                .window
                .with_element_state_in_event::<InteractiveElementState, _>(
                    &target.fiber_id,
                    |element_state, window| {
                        let mut element_state = element_state.unwrap_or_default();
                        let mut captured = None;
                        if has_click_or_drag {
                            let pending_mouse_down = element_state
                                .pending_mouse_down
                                .get_or_insert_with(Default::default)
                                .clone();
                            let mut pending_mouse_down = pending_mouse_down.borrow_mut();
                            if pending_mouse_down.is_some() && target.hitbox.is_hovered(window) {
                                captured = pending_mouse_down.take();
                                window.invalidate_fiber_paint(target.fiber_id);
                            } else if pending_mouse_down.is_some() {
                                pending_mouse_down.take();
                                window.invalidate_fiber_paint(target.fiber_id);
                            }
                        }

                        let clicked_state = element_state
                            .clicked_state
                            .get_or_insert_with(Default::default)
                            .clone();
                        if clicked_state.borrow().is_clicked() {
                            *clicked_state.borrow_mut() = ElementClickedState::default();
                            window.invalidate_fiber_paint(target.fiber_id);
                        }

                        (captured, element_state)
                    },
                );
            if let Some(captured) = captured {
                captured_mouse_down[index] = Some(captured);
            }
        }

        if cx.propagate_event {
            for (index, target) in targets.iter().enumerate() {
                if !cx.propagate_event {
                    break;
                }
                if let Some(mouse_down) = captured_mouse_down[index].take() {
                    let mouse_click = ClickEvent::Mouse(MouseClickEvent {
                        down: mouse_down,
                        up: event.clone(),
                    });
                    let listeners = self
                        .with_fiber_effects_mut(&target.fiber_id, |effects| {
                            mem::take(&mut effects.click_listeners)
                        })
                        .unwrap_or_default();
                    for listener in &listeners {
                        listener(&mouse_click, self.window, cx);
                        if !cx.propagate_event {
                            break;
                        }
                    }
                    let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                        effects.click_listeners = listeners;
                    });
                }

                if !cx.propagate_event {
                    break;
                }

                let (drop_listeners, can_drop_predicate) = self
                    .with_fiber_effects_mut(&target.fiber_id, |effects| {
                        (
                            mem::take(&mut effects.drop_listeners),
                            effects.can_drop_predicate.take(),
                        )
                    })
                    .unwrap_or((Vec::new(), None));

                if let Some(drag) = &cx.active_drag
                    && target.hitbox.is_hovered(self.window)
                {
                    let drag_state_type = drag.value.as_ref().type_id();
                    for (drop_state_type, listener) in &drop_listeners {
                        if *drop_state_type == drag_state_type {
                            let drag = cx
                                .active_drag
                                .take()
                                .expect("checked for type drag state type above");
                            let mut can_drop = true;
                            if let Some(predicate) = &can_drop_predicate {
                                can_drop = predicate(drag.value.as_ref(), self.window, cx);
                            }
                            if can_drop {
                                listener(drag.value.as_ref(), self.window, cx);
                                self.window.request_redraw();
                                cx.stop_propagation();
                            }
                        }
                    }
                }

                let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                    effects.drop_listeners = drop_listeners;
                    effects.can_drop_predicate = can_drop_predicate;
                });

                if !cx.propagate_event {
                    break;
                }

                let listeners = self
                    .with_fiber_effects_mut(&target.fiber_id, |effects| {
                        mem::take(&mut effects.mouse_up_listeners)
                    })
                    .unwrap_or_default();
                for listener in &listeners {
                    listener(
                        event,
                        DispatchPhase::Bubble,
                        &target.hitbox,
                        self.window,
                        cx,
                    );
                    if !cx.propagate_event {
                        break;
                    }
                }
                let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                    effects.mouse_up_listeners = listeners;
                });
            }
        }
    }

    fn dispatch_fiber_mouse_move(
        &mut self,
        event: &MouseMoveEvent,
        targets: &[FiberDispatchTarget],
        cx: &mut App,
    ) {
        for target in targets.iter().rev() {
            if !cx.propagate_event {
                break;
            }
            let listeners = self
                .with_fiber_effects_mut(&target.fiber_id, |effects| {
                    mem::take(&mut effects.mouse_move_listeners)
                })
                .unwrap_or_default();
            for listener in &listeners {
                listener(
                    event,
                    DispatchPhase::Capture,
                    &target.hitbox,
                    self.window,
                    cx,
                );
                if !cx.propagate_event {
                    break;
                }
            }
            let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                effects.mouse_move_listeners = listeners;
            });

            if !cx.propagate_event {
                break;
            }

            if let Some(info) = target.interactivity.as_ref() {
                let cursor_style = self
                    .window
                    .get_fiber_effects(&target.fiber_id)
                    .and_then(|effects| effects.cursor_style);
                let should_track_hover = info.hover_style
                    || cursor_style.is_some()
                    || (cx.active_drag.is_some() && info.drag_over_styles);
                if should_track_hover {
                    let _ = self
                        .window
                        .with_element_state_in_event::<InteractiveElementState, _>(
                            &target.fiber_id,
                            |element_state, window| {
                                let mut element_state = element_state.unwrap_or_default();
                                let hover_state = element_state
                                    .hover_state
                                    .get_or_insert_with(Default::default)
                                    .clone();
                                let hovered = target.hitbox.is_hovered(window);
                                let mut hover_state = hover_state.borrow_mut();
                                if hovered != hover_state.element {
                                    hover_state.element = hovered;
                                    drop(hover_state);
                                    window.invalidate_fiber_paint(target.fiber_id);
                                }
                                ((), element_state)
                            },
                        );
                }

                if let Some(group) = info.group_hover.as_ref() {
                    if let Some(group_hitbox_id) = GroupHitboxes::get(group, cx) {
                        let _ = self
                            .window
                            .with_element_state_in_event::<InteractiveElementState, _>(
                                &target.fiber_id,
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
                                        window.invalidate_fiber_paint(target.fiber_id);
                                    }
                                    ((), element_state)
                                },
                            );
                    }
                }
            }

            self.dispatch_fiber_tooltip_mouse_move(target, DispatchPhase::Capture, cx);
        }

        if cx.propagate_event {
            for target in targets.iter() {
                if !cx.propagate_event {
                    break;
                }

                self.dispatch_fiber_tooltip_mouse_move(target, DispatchPhase::Bubble, cx);
                if !cx.propagate_event {
                    break;
                }

                let hover_listener = self
                    .with_fiber_effects_mut(&target.fiber_id, |effects| {
                        effects.hover_listener.take()
                    })
                    .flatten();
                if let Some(hover_listener) = hover_listener {
                    let _ = self
                        .window
                        .with_element_state_in_event::<InteractiveElementState, _>(
                            &target.fiber_id,
                            |element_state, window| {
                                let mut element_state = element_state.unwrap_or_default();
                                let hover_state = element_state
                                    .hover_state
                                    .get_or_insert_with(Default::default)
                                    .clone();
                                let pending_mouse_down = element_state
                                    .pending_mouse_down
                                    .get_or_insert_with(Default::default)
                                    .clone();
                                let is_hovered = pending_mouse_down.borrow().is_none()
                                    && !cx.has_active_drag()
                                    && target.hitbox.is_hovered(window);
                                let mut hover_state = hover_state.borrow_mut();
                                if is_hovered != hover_state.element {
                                    hover_state.element = is_hovered;
                                    drop(hover_state);
                                    hover_listener(&is_hovered, window, cx);
                                }
                                ((), element_state)
                            },
                        );
                    let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                        effects.hover_listener = Some(hover_listener);
                    });
                }

                if !cx.propagate_event {
                    break;
                }

                let drag_cursor_style = self
                    .window
                    .get_fiber_effects(&target.fiber_id)
                    .and_then(|effects| effects.cursor_style);
                let drag_candidate = self
                    .window
                    .with_element_state_in_event::<InteractiveElementState, _>(
                        &target.fiber_id,
                        |element_state, _window| {
                            let mut element_state = element_state.unwrap_or_default();
                            let pending_mouse_down = element_state
                                .pending_mouse_down
                                .get_or_insert_with(Default::default)
                                .clone();
                            let mouse_down = pending_mouse_down.borrow().clone();
                            let result = if let Some(mouse_down) = mouse_down {
                                if !cx.has_active_drag()
                                    && (event.position - mouse_down.position).magnitude()
                                        > DRAG_THRESHOLD
                                {
                                    Some((pending_mouse_down, mouse_down))
                                } else {
                                    None
                                }
                            } else {
                                None
                            };
                            (result, element_state)
                        },
                    );

                if let Some((pending_mouse_down, _mouse_down)) = drag_candidate {
                    if let Some((drag_value, drag_listener)) = self
                        .window
                        .get_fiber_effects(&target.fiber_id)
                        .and_then(|effects| effects.drag_listener.as_ref().cloned())
                    {
                        let cursor_offset = event.position - target.hitbox.origin;
                        let drag =
                            (drag_listener)(drag_value.as_ref(), cursor_offset, self.window, cx);
                        let _ = self
                            .window
                            .with_element_state_in_event::<InteractiveElementState, _>(
                                &target.fiber_id,
                                |element_state, window| {
                                    let mut element_state = element_state.unwrap_or_default();
                                    if let Some(clicked_state) =
                                        element_state.clicked_state.as_ref()
                                    {
                                        *clicked_state.borrow_mut() =
                                            ElementClickedState::default();
                                    }
                                    *pending_mouse_down.borrow_mut() = None;
                                    window.invalidate_fiber_paint(target.fiber_id);
                                    ((), element_state)
                                },
                            );
                        cx.active_drag = Some(AnyDrag {
                            view: drag,
                            value: drag_value,
                            cursor_offset,
                            cursor_style: drag_cursor_style,
                        });
                        cx.stop_propagation();
                    }
                }

                if !cx.propagate_event {
                    break;
                }

                let listeners = self
                    .with_fiber_effects_mut(&target.fiber_id, |effects| {
                        mem::take(&mut effects.mouse_move_listeners)
                    })
                    .unwrap_or_default();
                for listener in &listeners {
                    listener(
                        event,
                        DispatchPhase::Bubble,
                        &target.hitbox,
                        self.window,
                        cx,
                    );
                    if !cx.propagate_event {
                        break;
                    }
                }
                let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                    effects.mouse_move_listeners = listeners;
                });
            }
        }
    }

    fn dispatch_fiber_mouse_pressure(
        &mut self,
        event: &crate::MousePressureEvent,
        targets: &[FiberDispatchTarget],
        cx: &mut App,
    ) {
        for target in targets.iter().rev() {
            if !cx.propagate_event {
                break;
            }
            let listeners = self
                .with_fiber_effects_mut(&target.fiber_id, |effects| {
                    mem::take(&mut effects.mouse_pressure_listeners)
                })
                .unwrap_or_default();
            for listener in &listeners {
                listener(
                    event,
                    DispatchPhase::Capture,
                    &target.hitbox,
                    self.window,
                    cx,
                );
                if !cx.propagate_event {
                    break;
                }
            }
            let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                effects.mouse_pressure_listeners = listeners;
            });
        }

        if cx.propagate_event {
            for target in targets.iter() {
                if !cx.propagate_event {
                    break;
                }
                let listeners = self
                    .with_fiber_effects_mut(&target.fiber_id, |effects| {
                        mem::take(&mut effects.mouse_pressure_listeners)
                    })
                    .unwrap_or_default();
                for listener in &listeners {
                    listener(
                        event,
                        DispatchPhase::Bubble,
                        &target.hitbox,
                        self.window,
                        cx,
                    );
                    if !cx.propagate_event {
                        break;
                    }
                }
                let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                    effects.mouse_pressure_listeners = listeners;
                });
            }
        }
    }

    fn dispatch_fiber_scroll_wheel(
        &mut self,
        event: &crate::ScrollWheelEvent,
        targets: &[FiberDispatchTarget],
        cx: &mut App,
    ) {
        for target in targets.iter().rev() {
            if !cx.propagate_event {
                break;
            }
            let listeners = self
                .with_fiber_effects_mut(&target.fiber_id, |effects| {
                    mem::take(&mut effects.scroll_wheel_listeners)
                })
                .unwrap_or_default();
            for listener in &listeners {
                listener(
                    event,
                    DispatchPhase::Capture,
                    &target.hitbox,
                    self.window,
                    cx,
                );
                if !cx.propagate_event {
                    break;
                }
            }
            let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                effects.scroll_wheel_listeners = listeners;
            });

            if !cx.propagate_event {
                break;
            }

            self.dispatch_fiber_tooltip_clear(target, cx);
        }

        if cx.propagate_event {
            for target in targets.iter() {
                if !cx.propagate_event {
                    break;
                }
                let listeners = self
                    .with_fiber_effects_mut(&target.fiber_id, |effects| {
                        mem::take(&mut effects.scroll_wheel_listeners)
                    })
                    .unwrap_or_default();
                for listener in &listeners {
                    listener(
                        event,
                        DispatchPhase::Bubble,
                        &target.hitbox,
                        self.window,
                        cx,
                    );
                    if !cx.propagate_event {
                        break;
                    }
                }
                let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
                    effects.scroll_wheel_listeners = listeners;
                });
            }
        }
    }

    fn dispatch_fiber_tooltip_mouse_move(
        &mut self,
        target: &FiberDispatchTarget,
        phase: DispatchPhase,
        cx: &mut App,
    ) {
        let tooltip_builder = self
            .with_fiber_effects_mut(&target.fiber_id, |effects| effects.tooltip.take())
            .flatten();
        let Some(tooltip_builder) = tooltip_builder else {
            return;
        };

        let (active_tooltip, pending_mouse_down) = self
            .window
            .with_element_state_in_event::<InteractiveElementState, _>(
                &target.fiber_id,
                |element_state, _window| {
                    let mut element_state = element_state.unwrap_or_default();
                    let active_tooltip = element_state
                        .active_tooltip
                        .get_or_insert_with(Default::default)
                        .clone();
                    let pending_mouse_down = element_state
                        .pending_mouse_down
                        .get_or_insert_with(Default::default)
                        .clone();
                    ((active_tooltip, pending_mouse_down), element_state)
                },
            );

        let tooltip_is_hoverable = tooltip_builder.hoverable;
        let build = tooltip_builder.build.clone();
        let build_tooltip: Rc<dyn Fn(&mut Window, &mut App) -> Option<(AnyView, bool)>> =
            Rc::new(move |window: &mut Window, cx: &mut App| {
                Some(((build)(window, cx), tooltip_is_hoverable))
            });

        let check_is_hovered_during_prepaint: Rc<dyn Fn(&Window) -> bool> = Rc::new({
            let hitbox_id = target.hitbox.id;
            let bounds = target.hitbox.bounds;
            let pending_mouse_down = pending_mouse_down.clone();
            move |window: &Window| {
                if pending_mouse_down.borrow().is_some() {
                    return false;
                }
                window.hitbox_is_hovered(hitbox_id) || bounds.contains(&window.mouse_position())
            }
        });

        let check_is_hovered: Rc<dyn Fn(&Window) -> bool> = Rc::new({
            let hitbox = target.hitbox.clone();
            move |window: &Window| {
                pending_mouse_down.borrow().is_none() && hitbox.is_hovered(window)
            }
        });

        handle_tooltip_mouse_move(
            &active_tooltip,
            &build_tooltip,
            &check_is_hovered,
            &check_is_hovered_during_prepaint,
            phase,
            self.window,
            cx,
        );

        let _ = self.with_fiber_effects_mut(&target.fiber_id, |effects| {
            effects.tooltip = Some(tooltip_builder);
        });
    }

    fn dispatch_fiber_tooltip_clear(&mut self, target: &FiberDispatchTarget, _cx: &mut App) {
        if self
            .window
            .get_fiber_effects(&target.fiber_id)
            .and_then(|effects| effects.tooltip.as_ref())
            .is_none()
        {
            return;
        }

        let active_tooltip_rc = self
            .window
            .with_element_state_in_event::<InteractiveElementState, _>(
                &target.fiber_id,
                |element_state, _window| {
                    let mut element_state = element_state.unwrap_or_default();
                    let active_tooltip = element_state
                        .active_tooltip
                        .get_or_insert_with(Default::default)
                        .clone();
                    (active_tooltip, element_state)
                },
            );
        if active_tooltip_rc.borrow().is_none() {
            return;
        }

        let tooltip_id = target
            .interactivity
            .as_ref()
            .and_then(|info| info.tooltip_id);
        if !tooltip_id.is_some_and(|tooltip_id| tooltip_id.is_hovered(self.window)) {
            clear_active_tooltip_if_not_hoverable(&active_tooltip_rc, self.window);
        }
    }

    pub(super) fn with_fiber_effects_mut<T>(
        &mut self,
        fiber_id: &GlobalElementId,
        f: impl FnOnce(&mut FiberEffects) -> T,
    ) -> Option<T> {
        let effects = self.window.fiber.tree.effects.get_mut((*fiber_id).into())?;
        Some(f(effects))
    }
}

impl FiberRuntimeHandle<'_> {
    fn dispatch_effect_listeners<L, C, Take, Restore, Invoke>(
        &mut self,
        path: &[GlobalElementId],
        phase: DispatchPhase,
        mut take: Take,
        mut restore: Restore,
        mut invoke: Invoke,
        cx: &mut App,
    ) -> bool
    where
        Take: FnMut(&mut Self, &GlobalElementId) -> C,
        Restore: FnMut(&mut Self, &GlobalElementId, C),
        Invoke: FnMut(&mut Self, &L, DispatchPhase, &mut App) -> bool,
        C: std::ops::Deref<Target = [L]>,
    {
        let iterator: Box<dyn Iterator<Item = &GlobalElementId>> = match phase {
            DispatchPhase::Capture => Box::new(path.iter()),
            DispatchPhase::Bubble => Box::new(path.iter().rev()),
        };
        for fiber_id in iterator {
            let listeners = take(self, fiber_id);
            for listener in listeners.iter() {
                if !invoke(self, listener, phase, cx) {
                    break;
                }
            }
            restore(self, fiber_id, listeners);
            if !cx.propagate_event {
                return false;
            }
        }
        true
    }

    pub(super) fn dispatch_key_listeners(
        &mut self,
        event: &dyn Any,
        node_id: GlobalElementId,
        cx: &mut App,
    ) {
        let path = path_to_root_smallvec(&self.window.fiber.tree, node_id);

        let take = |window: &mut Self, fiber_id: &GlobalElementId| {
            window
                .with_fiber_effects_mut(fiber_id, |effects| mem::take(&mut effects.key_listeners))
                .unwrap_or_default()
        };
        let restore = |window: &mut Self, fiber_id: &GlobalElementId, listeners| {
            let _ = window.with_fiber_effects_mut(fiber_id, |effects| {
                effects.key_listeners = listeners;
            });
        };
        let invoke = |window: &mut Self, listener: &crate::KeyListener, phase, cx: &mut App| {
            listener(event, phase, window.window, cx);
            cx.propagate_event
        };

        self.dispatch_effect_listeners(&path, DispatchPhase::Capture, take, restore, invoke, cx);
        if cx.propagate_event {
            self.dispatch_effect_listeners(&path, DispatchPhase::Bubble, take, restore, invoke, cx);
        }
    }

    pub(super) fn dispatch_modifiers_listeners(
        &mut self,
        event: &ModifiersChangedEvent,
        node_id: GlobalElementId,
        cx: &mut App,
    ) {
        let path = path_to_root_smallvec(&self.window.fiber.tree, node_id);

        let take = |window: &mut Self, fiber_id: &GlobalElementId| {
            window
                .with_fiber_effects_mut(fiber_id, |effects| {
                    mem::take(&mut effects.modifiers_changed_listeners)
                })
                .unwrap_or_default()
        };
        let restore = |window: &mut Self, fiber_id: &GlobalElementId, listeners| {
            let _ = window.with_fiber_effects_mut(fiber_id, |effects| {
                effects.modifiers_changed_listeners = listeners;
            });
        };
        let invoke = |window: &mut Self,
                      listener: &crate::ModifiersChangedListener,
                      _phase,
                      cx: &mut App| {
            listener(event, window.window, cx);
            cx.propagate_event
        };
        self.dispatch_effect_listeners(&path, DispatchPhase::Bubble, take, restore, invoke, cx);
    }

    pub(super) fn dispatch_window_action_listeners(
        &mut self,
        action: &dyn Action,
        node_id: GlobalElementId,
        cx: &mut App,
    ) -> bool {
        let path = path_to_root_smallvec(&self.window.fiber.tree, node_id);

        let action_type_id = action.as_any().type_id();
        let take = |window: &mut Self, fiber_id: &GlobalElementId| {
            window
                .with_fiber_effects_mut(fiber_id, |effects| {
                    mem::take(&mut effects.action_listeners)
                })
                .unwrap_or_default()
        };
        let restore = |window: &mut Self, fiber_id: &GlobalElementId, listeners| {
            let _ = window.with_fiber_effects_mut(fiber_id, |effects| {
                effects.action_listeners = listeners;
            });
        };
        let invoke =
            |window: &mut Self, listener: &(TypeId, crate::ActionListener), phase, cx: &mut App| {
                if listener.0 == action_type_id {
                    if phase == DispatchPhase::Bubble {
                        cx.propagate_event = false;
                    }
                    (listener.1)(action.as_any(), phase, window.window, cx);
                }
                cx.propagate_event
            };

        if !self.dispatch_effect_listeners(&path, DispatchPhase::Capture, take, restore, invoke, cx)
        {
            return false;
        }
        self.dispatch_effect_listeners(&path, DispatchPhase::Bubble, take, restore, invoke, cx)
    }
}
