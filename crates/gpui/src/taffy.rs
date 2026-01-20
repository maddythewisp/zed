use crate::window::context::PrepaintCx;
use crate::{
    AbsoluteLength, App, Bounds, DefiniteLength, Edges, GlobalElementId, Length, Pixels, Point,
    Size, Style, Window, point, size,
};
use collections::FxHashMap;
use slotmap::DefaultKey;
use smallvec::SmallVec;
use std::{fmt::Debug, hash::Hash, mem, ops::Range};
use taffy::{
    compute_root_layout,
    geometry::{Point as TaffyPoint, Rect as TaffyRect, Size as TaffySize},
    prelude::min_content,
    round_layout,
    style::AvailableSpace as TaffyAvailableSpace,
    tree::NodeId,
};

/// A unique identifier for a layout node.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
#[repr(transparent)]
pub struct LayoutId(NodeId);

impl From<LayoutId> for NodeId {
    fn from(layout_id: LayoutId) -> NodeId {
        layout_id.0
    }
}

impl From<NodeId> for LayoutId {
    fn from(node_id: NodeId) -> Self {
        LayoutId(node_id)
    }
}

pub struct TaffyLayoutEngine {
    pub(crate) pending_measure_calls: Vec<GlobalElementId>,
    pub(crate) fibers_layout_changed: Vec<GlobalElementId>,
    pub(crate) last_layout_viewport_size: Option<Size<Pixels>>,
}

impl TaffyLayoutEngine {
    pub fn new() -> Self {
        Self {
            pending_measure_calls: Vec::new(),
            fibers_layout_changed: Vec::new(),
            last_layout_viewport_size: None,
        }
    }
}

impl TaffyLayoutEngine {
    pub(crate) fn request_layout<I>(
        &mut self,
        window: &mut Window,
        fiber_id: GlobalElementId,
        style: Style,
        _children: I,
        _cx: &mut App,
    ) -> LayoutId
    where
        I: IntoIterator<Item = LayoutId>,
    {
        let _ = self;
        let rem_size = window.rem_size();
        let scale_factor = window.scale_factor();
        let taffy_style = style.to_taffy(rem_size, scale_factor);
        let _ = update_fiber_style(window, &fiber_id, taffy_style);
        clear_fiber_measure_func(window, &fiber_id);
        Self::layout_id(&fiber_id)
    }

    pub(crate) fn request_measured_layout<F>(
        &mut self,
        window: &mut Window,
        fiber_id: GlobalElementId,
        style: Style,
        measure: F,
    ) -> LayoutId
    where
        F: Fn(Size<Option<Pixels>>, Size<AvailableSpace>, &mut Window, &mut App) -> Size<Pixels>
            + 'static,
    {
        let _ = self;
        let rem_size = window.rem_size();
        let scale_factor = window.scale_factor();
        let taffy_style = style.to_taffy(rem_size, scale_factor);
        let _ = update_fiber_style(window, &fiber_id, taffy_style);
        window.fiber.tree.measure_funcs.insert(
            fiber_id.into(),
            crate::fiber::FiberMeasureData {
                measure_func: Box::new(measure),
                measure_hash: None,
            },
        );
        window.fiber.tree.clear_taffy_cache_upwards(&fiber_id);
        Self::layout_id(&fiber_id)
    }

    pub(crate) fn request_measured_layout_cached<F>(
        &mut self,
        window: &mut Window,
        fiber_id: GlobalElementId,
        style: Style,
        content_hash: u64,
        measure: F,
    ) -> LayoutId
    where
        F: Fn(Size<Option<Pixels>>, Size<AvailableSpace>, &mut Window, &mut App) -> Size<Pixels>
            + 'static,
    {
        let rem_size = window.rem_size();
        let scale_factor = window.scale_factor();
        let taffy_style = style.to_taffy(rem_size, scale_factor);
        let style_changed = update_fiber_style(window, &fiber_id, taffy_style);

        let mut content_changed = true;
        let measure_func = Box::new(measure);
        let key: DefaultKey = fiber_id.into();
        match window.fiber.tree.measure_funcs.get_mut(key) {
            Some(data) => {
                content_changed = data.measure_hash != Some(content_hash);
                data.measure_hash = Some(content_hash);
                data.measure_func = measure_func;
            }
            None => {
                window.fiber.tree.measure_funcs.insert(
                    key,
                    crate::fiber::FiberMeasureData {
                        measure_func,
                        measure_hash: Some(content_hash),
                    },
                );
            }
        }

        if style_changed || content_changed {
            // Conservative fallback: invalidate caches immediately.
            // Intrinsic sizing will replace this path once fully wired into the frame loop.
            window.fiber.tree.clear_taffy_cache_upwards(&fiber_id);
        } else {
            self.pending_measure_calls.push(fiber_id);
        }

        Self::layout_id(&fiber_id)
    }

    pub(crate) fn compute_layout(
        &mut self,
        window: &mut Window,
        layout_id: LayoutId,
        available_space: Size<AvailableSpace>,
        cx: &mut App,
    ) -> usize {
        let scale_factor = window.scale_factor();
        let transform = |space: AvailableSpace| match space {
            AvailableSpace::Definite(pixels) => {
                AvailableSpace::Definite(Pixels(pixels.0 * scale_factor))
            }
            AvailableSpace::MinContent => AvailableSpace::MinContent,
            AvailableSpace::MaxContent => AvailableSpace::MaxContent,
        };
        let available_space = size(
            transform(available_space.width),
            transform(available_space.height),
        );

        let mut fiber_tree = mem::take(&mut window.fiber.tree);
        let island_root: GlobalElementId = layout_id.into();
        fiber_tree.set_layout_context(
            window as *mut Window,
            cx as *mut App,
            scale_factor,
            island_root,
        );
        compute_root_layout(&mut fiber_tree, layout_id.into(), available_space.into());
        round_layout(&mut fiber_tree, layout_id.into());
        let layout_calls = fiber_tree.layout_calls();
        if std::env::var("GPUI_DEBUG_LAYOUT").is_ok() && layout_calls > 0 {
            log::info!("=== layout frame end: {} fibers ===", layout_calls);
        }
        fiber_tree.log_layout_cache_summary_if_enabled();
        fiber_tree.clear_layout_context();
        window.fiber.tree = fiber_tree;

        let pending_calls = mem::take(&mut self.pending_measure_calls);
        let mut bounds_cache = FxHashMap::default();
        for fiber_id in pending_calls {
            let bounds = layout_bounds(window, &fiber_id, scale_factor, &mut bounds_cache);
            let size: Size<Pixels> = bounds.size;
            let key: DefaultKey = fiber_id.into();
            let mut measure_data = window.fiber.tree.measure_funcs.remove(key);
            if let Some(data) = measure_data.as_mut() {
                (data.measure_func)(
                    Size {
                        width: Some(size.width),
                        height: Some(size.height),
                    },
                    Size {
                        width: AvailableSpace::Definite(size.width),
                        height: AvailableSpace::Definite(size.height),
                    },
                    window,
                    cx,
                );
            }
            if let Some(data) = measure_data {
                window.fiber.tree.measure_funcs.insert(key, data);
            }
        }
        layout_calls
    }

    pub(crate) fn compute_layout_for_fiber(
        &mut self,
        window: &mut Window,
        fiber_id: GlobalElementId,
        available_space: Size<AvailableSpace>,
        cx: &mut App,
    ) -> usize {
        self.compute_layout(window, Self::layout_id(&fiber_id), available_space, cx)
    }

    pub(crate) fn layout_bounds(
        &mut self,
        window: &mut Window,
        layout_id: LayoutId,
    ) -> Bounds<Pixels> {
        let _ = self;
        let scale_factor = window.scale_factor();
        let mut cache = FxHashMap::default();
        let fiber_id: GlobalElementId = layout_id.into();
        let mut bounds = layout_bounds(window, &fiber_id, scale_factor, &mut cache);
        bounds.origin += PrepaintCx::new(window).element_offset();
        bounds
    }

    pub(crate) fn layout_id(fiber_id: &GlobalElementId) -> LayoutId {
        LayoutId::from(*fiber_id)
    }

    pub(crate) fn setup_taffy_from_fibers(
        window: &mut Window,
        root: GlobalElementId,
        cx: &mut App,
    ) {
        // Set the setup_island_root so that fibers created during legacy element
        // request_layout get assigned to the correct layout island.
        window.fiber.setup_island_root = Some(root);
        window.fiber.tree.rebuild_layout_islands_if_needed();
        let rem_size = window.rem_size();
        let scale_factor = window.scale_factor();

        #[derive(Clone, Copy)]
        struct LayoutStackState {
            text_style_len: usize,
            image_cache_len: usize,
            rendered_entity_len: usize,
        }

        impl LayoutStackState {
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

        struct LayoutStackFrame {
            stack_state: LayoutStackState,
            under_layout_change: bool,
            node_frame: Option<crate::LayoutFrame>,
        }
        let mut frame_stack: Vec<LayoutStackFrame> = Vec::new();
        let mut stack: Vec<(GlobalElementId, bool)> = vec![(root, true)];
        let mut under_layout_change = false;

        while let Some((fiber_id, entering)) = stack.pop() {
            if entering {
                let structure_epoch_before = window.fiber.tree.structure_epoch;
                let (needs_layout, has_render_node) = match window.fiber.tree.get(&fiber_id) {
                    Some(_fiber) => (
                        window.fiber.tree.dirty_flags(&fiber_id).needs_layout(),
                        window
                            .fiber
                            .tree
                            .render_nodes
                            .get(fiber_id.into())
                            .is_some(),
                    ),
                    None => continue,
                };
                let mut skip_children = false;
                let mut node_frame: Option<crate::LayoutFrame> = None;
                let stack_state = LayoutStackState::capture(window);

                let was_under_layout_change = under_layout_change;
                if needs_layout {
                    under_layout_change = true;
                }
                if under_layout_change {
                    window.layout_engine.fibers_layout_changed.push(fiber_id);
                }
                if let Some(view_id) = window
                    .fiber
                    .tree
                    .get(&fiber_id)
                    .and_then(|fiber| window.fiber_view_id(&fiber_id, fiber))
                {
                    window.rendered_entity_stack.push(view_id);
                }

	                // Call layout_begin on render nodes (if they have one)
	                let mut layout_handled = false;
	                if has_render_node {
	                    let _ = window.with_render_node_mut(fiber_id, |node, window| {
	                        let mut ctx = crate::LayoutCtx {
	                            fiber_id,
	                            rem_size,
	                            scale_factor,
	                            window,
	                            cx: &mut *cx,
	                        };
	                        let frame = node.layout_begin(&mut ctx);

	                        // Track what the node pushed
	                        layout_handled = frame.handled;

	                        // If the node handled layout, update fiber's taffy_style from the node
	                        if layout_handled && needs_layout {
	                            let taffy_style = node.taffy_style(rem_size, scale_factor);
	                            let _ = update_fiber_style(window, &fiber_id, taffy_style);
	                            clear_fiber_measure_func(window, &fiber_id);
	                        }

	                        node_frame = Some(frame);
	                    });
	                }

                // Layout for element types that don't have render nodes yet.
                if !layout_handled && needs_layout {
                    let _ = update_fiber_style(window, &fiber_id, taffy::Style::default());
                    clear_fiber_measure_func(window, &fiber_id);
                }

                frame_stack.push(LayoutStackFrame {
                    stack_state,
                    under_layout_change: was_under_layout_change,
                    node_frame,
                });
                stack.push((fiber_id, false));

                if window.fiber.tree.structure_epoch != structure_epoch_before {
                    window.fiber.tree.rebuild_layout_islands_if_needed();
                }

                if !skip_children {
                    let children: SmallVec<[GlobalElementId; 8]> =
                        window.fiber.tree.children(&fiber_id).collect();
                    for child_id in children.into_iter().rev() {
                        let child_island = window.fiber.tree.outer_island_root_for(child_id);
                        if child_island == root {
                            stack.push((child_id, true));
                        }
                    }
                }
	            } else if let Some(frame) = frame_stack.pop() {
	                // Call layout_end on render nodes (if they had a node_frame)
	                if let Some(node_frame) = frame.node_frame {
	                    let _ = window.with_render_node_mut(fiber_id, |node, window| {
	                        let mut ctx = crate::LayoutCtx {
	                            fiber_id,
	                            rem_size,
	                            scale_factor,
	                            window,
	                            cx: &mut *cx,
	                        };
	                        node.layout_end(&mut ctx, node_frame);
	                    });
	                }

                under_layout_change = frame.under_layout_change;
                frame.stack_state.restore(window);
            }
        }

        // Clear the setup_island_root now that setup is complete.
        window.fiber.setup_island_root = None;
    }

    pub(crate) fn finalize_dirty_flags(window: &mut Window) {
        if window.layout_engine.fibers_layout_changed.is_empty() {
            return;
        }

        let scale_factor = window.scale_factor();
        let mut cache = FxHashMap::default();
        let mut changed_fibers: Vec<(GlobalElementId, crate::DirtyFlags)> = Vec::new();

        for fiber_id in &window.layout_engine.fibers_layout_changed {
            let new_bounds = layout_bounds(window, fiber_id, scale_factor, &mut cache);
            let old_bounds = window.fiber.tree.bounds.get((*fiber_id).into()).copied();
            let position_changed = old_bounds.map_or(true, |old| old.origin != new_bounds.origin);
            let size_changed = old_bounds.map_or(true, |old| old.size != new_bounds.size);

            window
                .fiber
                .tree
                .bounds
                .insert((*fiber_id).into(), new_bounds);

            let mut flags = crate::DirtyFlags::NONE;
            if position_changed || size_changed {
                flags.insert(crate::DirtyFlags::NEEDS_PAINT);
            }
            if flags.any() {
                changed_fibers.push((*fiber_id, flags));
            }
        }

        for (fiber_id, flags) in changed_fibers {
            window.fiber.tree.mark_dirty(&fiber_id, flags);
        }
    }
}

pub(crate) fn layout_bounds(
    window: &Window,
    fiber_id: &GlobalElementId,
    scale_factor: f32,
    cache: &mut FxHashMap<GlobalElementId, Bounds<Pixels>>,
) -> Bounds<Pixels> {
    if let Some(bounds) = cache.get(fiber_id) {
        return *bounds;
    }

    if let Some(child_id) = window.fiber.tree.view_wrapper_child(fiber_id) {
        let bounds = layout_bounds(window, &child_id, scale_factor, cache);
        cache.insert(*fiber_id, bounds);
        return bounds;
    }

    let _fiber = window
        .fiber
        .tree
        .get(fiber_id)
        .unwrap_or_else(|| panic!("missing fiber {fiber_id:?}"));
    window
        .fiber
        .tree
        .layout_state
        .get((*fiber_id).into())
        .unwrap_or_else(|| panic!("missing layout state {fiber_id:?}"));
    let final_layout = window.fiber.tree.final_layout_for_bounds(*fiber_id);
    let mut bounds = Bounds {
        origin: point(
            Pixels(final_layout.location.x / scale_factor),
            Pixels(final_layout.location.y / scale_factor),
        ),
        size: size(
            Pixels(final_layout.size.width / scale_factor),
            Pixels(final_layout.size.height / scale_factor),
        ),
    };

    if let Some(parent_id) = window.fiber.tree.layout_parent(fiber_id) {
        let parent_bounds = layout_bounds(window, &parent_id, scale_factor, cache);
        bounds.origin += parent_bounds.origin;
    }

    cache.insert(*fiber_id, bounds);
    bounds
}

fn update_fiber_style(
    window: &mut Window,
    fiber_id: &GlobalElementId,
    new_style: taffy::Style,
) -> bool {
    let mut updated = false;
    if let Some(layout_state) = window.fiber.tree.layout_state.get_mut((*fiber_id).into()) {
        if layout_state.taffy_style != new_style {
            layout_state.taffy_style = new_style;
            window.fiber.tree.clear_taffy_cache_upwards(fiber_id);
            updated = true;
        }
    }
    updated
}

fn clear_fiber_measure_func(window: &mut Window, fiber_id: &GlobalElementId) {
    if window
        .fiber
        .tree
        .measure_funcs
        .remove((*fiber_id).into())
        .is_some()
    {
        window.fiber.tree.clear_taffy_cache_upwards(fiber_id);
    }
}

/// Trait for converting GPUI types to their taffy equivalents.
pub trait ToTaffy<Output> {
    /// Convert this type to its taffy equivalent, using the given rem size and scale factor.
    fn to_taffy(&self, rem_size: Pixels, scale_factor: f32) -> Output;
}

impl ToTaffy<taffy::style::Style> for Style {
    fn to_taffy(&self, rem_size: Pixels, scale_factor: f32) -> taffy::style::Style {
        use taffy::style_helpers::{fr, length, minmax, repeat};

        fn to_grid_line(
            placement: &Range<crate::GridPlacement>,
        ) -> taffy::Line<taffy::GridPlacement> {
            taffy::Line {
                start: placement.start.into(),
                end: placement.end.into(),
            }
        }

        fn to_grid_repeat<T: taffy::style::CheapCloneStr>(
            unit: &Option<u16>,
        ) -> Vec<taffy::GridTemplateComponent<T>> {
            // grid-template-columns: repeat(<number>, minmax(0, 1fr));
            unit.map(|count| vec![repeat(count, vec![minmax(length(0.0), fr(1.0))])])
                .unwrap_or_default()
        }

        fn to_grid_repeat_min_content<T: taffy::style::CheapCloneStr>(
            unit: &Option<u16>,
        ) -> Vec<taffy::GridTemplateComponent<T>> {
            // grid-template-columns: repeat(<number>, minmax(min-content, 1fr));
            unit.map(|count| vec![repeat(count, vec![minmax(min_content(), fr(1.0))])])
                .unwrap_or_default()
        }

        taffy::style::Style {
            display: self.display.into(),
            overflow: self.overflow.into(),
            scrollbar_width: self.scrollbar_width.to_taffy(rem_size, scale_factor),
            position: self.position.into(),
            inset: self.inset.to_taffy(rem_size, scale_factor),
            size: self.size.to_taffy(rem_size, scale_factor),
            min_size: self.min_size.to_taffy(rem_size, scale_factor),
            max_size: self.max_size.to_taffy(rem_size, scale_factor),
            aspect_ratio: self.aspect_ratio,
            margin: self.margin.to_taffy(rem_size, scale_factor),
            padding: self.padding.to_taffy(rem_size, scale_factor),
            border: self.border_widths.to_taffy(rem_size, scale_factor),
            align_items: self.align_items.map(|x| x.into()),
            align_self: self.align_self.map(|x| x.into()),
            align_content: self.align_content.map(|x| x.into()),
            justify_content: self.justify_content.map(|x| x.into()),
            gap: self.gap.to_taffy(rem_size, scale_factor),
            flex_direction: self.flex_direction.into(),
            flex_wrap: self.flex_wrap.into(),
            flex_basis: self.flex_basis.to_taffy(rem_size, scale_factor),
            flex_grow: self.flex_grow,
            flex_shrink: self.flex_shrink,
            grid_template_rows: to_grid_repeat(&self.grid_rows),
            grid_template_columns: if self.grid_cols_min_content.is_some() {
                to_grid_repeat_min_content(&self.grid_cols_min_content)
            } else {
                to_grid_repeat(&self.grid_cols)
            },
            grid_row: self
                .grid_location
                .as_ref()
                .map(|location| to_grid_line(&location.row))
                .unwrap_or_default(),
            grid_column: self
                .grid_location
                .as_ref()
                .map(|location| to_grid_line(&location.column))
                .unwrap_or_default(),
            ..Default::default()
        }
    }
}

impl ToTaffy<f32> for AbsoluteLength {
    fn to_taffy(&self, rem_size: Pixels, scale_factor: f32) -> f32 {
        match self {
            AbsoluteLength::Pixels(pixels) => {
                let pixels: f32 = pixels.into();
                pixels * scale_factor
            }
            AbsoluteLength::Rems(rems) => {
                let pixels: f32 = (*rems * rem_size).into();
                pixels * scale_factor
            }
        }
    }
}

impl ToTaffy<taffy::style::LengthPercentageAuto> for Length {
    fn to_taffy(
        &self,
        rem_size: Pixels,
        scale_factor: f32,
    ) -> taffy::prelude::LengthPercentageAuto {
        match self {
            Length::Definite(length) => length.to_taffy(rem_size, scale_factor),
            Length::Auto => taffy::prelude::LengthPercentageAuto::auto(),
        }
    }
}

impl ToTaffy<taffy::style::Dimension> for Length {
    fn to_taffy(&self, rem_size: Pixels, scale_factor: f32) -> taffy::prelude::Dimension {
        match self {
            Length::Definite(length) => length.to_taffy(rem_size, scale_factor),
            Length::Auto => taffy::prelude::Dimension::auto(),
        }
    }
}

impl ToTaffy<taffy::style::LengthPercentage> for DefiniteLength {
    fn to_taffy(&self, rem_size: Pixels, scale_factor: f32) -> taffy::style::LengthPercentage {
        match self {
            DefiniteLength::Absolute(length) => match length {
                AbsoluteLength::Pixels(pixels) => {
                    let pixels: f32 = pixels.into();
                    taffy::style::LengthPercentage::length(pixels * scale_factor)
                }
                AbsoluteLength::Rems(rems) => {
                    let pixels: f32 = (*rems * rem_size).into();
                    taffy::style::LengthPercentage::length(pixels * scale_factor)
                }
            },
            DefiniteLength::Fraction(fraction) => {
                taffy::style::LengthPercentage::percent(*fraction)
            }
        }
    }
}

impl ToTaffy<taffy::style::LengthPercentageAuto> for DefiniteLength {
    fn to_taffy(&self, rem_size: Pixels, scale_factor: f32) -> taffy::style::LengthPercentageAuto {
        match self {
            DefiniteLength::Absolute(length) => match length {
                AbsoluteLength::Pixels(pixels) => {
                    let pixels: f32 = pixels.into();
                    taffy::style::LengthPercentageAuto::length(pixels * scale_factor)
                }
                AbsoluteLength::Rems(rems) => {
                    let pixels: f32 = (*rems * rem_size).into();
                    taffy::style::LengthPercentageAuto::length(pixels * scale_factor)
                }
            },
            DefiniteLength::Fraction(fraction) => {
                taffy::style::LengthPercentageAuto::percent(*fraction)
            }
        }
    }
}

impl ToTaffy<taffy::style::Dimension> for DefiniteLength {
    fn to_taffy(&self, rem_size: Pixels, scale_factor: f32) -> taffy::style::Dimension {
        match self {
            DefiniteLength::Absolute(length) => match length {
                AbsoluteLength::Pixels(pixels) => {
                    let pixels: f32 = pixels.into();
                    taffy::style::Dimension::length(pixels * scale_factor)
                }
                AbsoluteLength::Rems(rems) => {
                    taffy::style::Dimension::length((*rems * rem_size * scale_factor).into())
                }
            },
            DefiniteLength::Fraction(fraction) => taffy::style::Dimension::percent(*fraction),
        }
    }
}

impl ToTaffy<taffy::style::LengthPercentage> for AbsoluteLength {
    fn to_taffy(&self, rem_size: Pixels, scale_factor: f32) -> taffy::style::LengthPercentage {
        match self {
            AbsoluteLength::Pixels(pixels) => {
                let pixels: f32 = pixels.into();
                taffy::style::LengthPercentage::length(pixels * scale_factor)
            }
            AbsoluteLength::Rems(rems) => {
                let pixels: f32 = (*rems * rem_size).into();
                taffy::style::LengthPercentage::length(pixels * scale_factor)
            }
        }
    }
}

impl<T, T2> From<TaffyPoint<T>> for Point<T2>
where
    T: Into<T2>,
    T2: Clone + Debug + Default + PartialEq,
{
    fn from(point: TaffyPoint<T>) -> Point<T2> {
        Point {
            x: point.x.into(),
            y: point.y.into(),
        }
    }
}

impl<T, T2> From<Point<T>> for TaffyPoint<T2>
where
    T: Into<T2> + Clone + Debug + Default + PartialEq,
{
    fn from(val: Point<T>) -> Self {
        TaffyPoint {
            x: val.x.into(),
            y: val.y.into(),
        }
    }
}

impl<T, U> ToTaffy<TaffySize<U>> for Size<T>
where
    T: ToTaffy<U> + Clone + Debug + Default + PartialEq,
{
    fn to_taffy(&self, rem_size: Pixels, scale_factor: f32) -> TaffySize<U> {
        TaffySize {
            width: self.width.to_taffy(rem_size, scale_factor),
            height: self.height.to_taffy(rem_size, scale_factor),
        }
    }
}

impl<T, U> ToTaffy<TaffyRect<U>> for Edges<T>
where
    T: ToTaffy<U> + Clone + Debug + Default + PartialEq,
{
    fn to_taffy(&self, rem_size: Pixels, scale_factor: f32) -> TaffyRect<U> {
        TaffyRect {
            top: self.top.to_taffy(rem_size, scale_factor),
            right: self.right.to_taffy(rem_size, scale_factor),
            bottom: self.bottom.to_taffy(rem_size, scale_factor),
            left: self.left.to_taffy(rem_size, scale_factor),
        }
    }
}

impl<T, U> From<TaffySize<T>> for Size<U>
where
    T: Into<U>,
    U: Clone + Debug + Default + PartialEq,
{
    fn from(taffy_size: TaffySize<T>) -> Self {
        Size {
            width: taffy_size.width.into(),
            height: taffy_size.height.into(),
        }
    }
}

impl<T, U> From<Size<T>> for TaffySize<U>
where
    T: Into<U> + Clone + Debug + Default + PartialEq,
{
    fn from(size: Size<T>) -> Self {
        TaffySize {
            width: size.width.into(),
            height: size.height.into(),
        }
    }
}

/// The space available for an element to be laid out in
#[derive(Copy, Clone, Default, Debug, Eq, PartialEq)]
pub enum AvailableSpace {
    /// The amount of space available is the specified number of pixels
    Definite(Pixels),
    /// The amount of space available is indefinite and the node should be laid out under a min-content constraint
    #[default]
    MinContent,
    /// The amount of space available is indefinite and the node should be laid out under a max-content constraint
    MaxContent,
}

impl AvailableSpace {
    /// Returns a `Size` with both width and height set to `AvailableSpace::MinContent`.
    ///
    /// This function is useful when you want to create a `Size` with the minimum content constraints
    /// for both dimensions.
    ///
    /// # Examples
    ///
    /// ```
    /// use gpui::AvailableSpace;
    /// let min_content_size = AvailableSpace::min_size();
    /// assert_eq!(min_content_size.width, AvailableSpace::MinContent);
    /// assert_eq!(min_content_size.height, AvailableSpace::MinContent);
    /// ```
    pub const fn min_size() -> Size<Self> {
        Size {
            width: Self::MinContent,
            height: Self::MinContent,
        }
    }
}

impl From<AvailableSpace> for TaffyAvailableSpace {
    fn from(space: AvailableSpace) -> TaffyAvailableSpace {
        match space {
            AvailableSpace::Definite(Pixels(value)) => TaffyAvailableSpace::Definite(value),
            AvailableSpace::MinContent => TaffyAvailableSpace::MinContent,
            AvailableSpace::MaxContent => TaffyAvailableSpace::MaxContent,
        }
    }
}

impl From<TaffyAvailableSpace> for AvailableSpace {
    fn from(space: TaffyAvailableSpace) -> AvailableSpace {
        match space {
            TaffyAvailableSpace::Definite(value) => AvailableSpace::Definite(Pixels(value)),
            TaffyAvailableSpace::MinContent => AvailableSpace::MinContent,
            TaffyAvailableSpace::MaxContent => AvailableSpace::MaxContent,
        }
    }
}

impl From<Pixels> for AvailableSpace {
    fn from(pixels: Pixels) -> Self {
        AvailableSpace::Definite(pixels)
    }
}

impl From<Size<Pixels>> for Size<AvailableSpace> {
    fn from(size: Size<Pixels>) -> Self {
        Size {
            width: AvailableSpace::Definite(size.width),
            height: AvailableSpace::Definite(size.height),
        }
    }
}
