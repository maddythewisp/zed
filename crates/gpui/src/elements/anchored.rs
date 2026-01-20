use gpui_macros::Element;
use smallvec::SmallVec;
use std::any::TypeId;

use crate::fiber::AnchoredConfig;
use crate::geometry::IsZero;
use crate::taffy::ToTaffy;
use crate::{
    AnyElement, App, Axis, Bounds, Corner, Display, Edges, ElementImpl,
    IntoElement, ParentElement, Pixels, Point, Position, Size, Style, UpdateResult, Window,
    point, px,
};

/// An anchored element that can be used to display UI that
/// will avoid overflowing the window bounds.
#[derive(Element)]
#[element(crate = crate)]
pub struct Anchored {
    children: SmallVec<[AnyElement; 2]>,
    anchor_corner: Corner,
    fit_mode: AnchoredFitMode,
    anchor_position: Option<Point<Pixels>>,
    position_mode: AnchoredPositionMode,
    offset: Option<Point<Pixels>>,
}

/// anchored gives you an element that will avoid overflowing the window bounds.
/// Its children should have no margin to avoid measurement issues.
pub fn anchored() -> Anchored {
    Anchored {
        children: SmallVec::new(),
        anchor_corner: Corner::TopLeft,
        fit_mode: AnchoredFitMode::SwitchAnchor,
        anchor_position: None,
        position_mode: AnchoredPositionMode::Window,
        offset: None,
    }
}

impl Anchored {
    /// Sets which corner of the anchored element should be anchored to the current position.
    pub fn anchor(mut self, anchor: Corner) -> Self {
        self.anchor_corner = anchor;
        self
    }

    /// Sets the position in window coordinates
    /// (otherwise the location the anchored element is rendered is used)
    pub fn position(mut self, anchor: Point<Pixels>) -> Self {
        self.anchor_position = Some(anchor);
        self
    }

    /// Offset the final position by this amount.
    /// Useful when you want to anchor to an element but offset from it, such as in PopoverMenu.
    pub fn offset(mut self, offset: Point<Pixels>) -> Self {
        self.offset = Some(offset);
        self
    }

    /// Sets the position mode for this anchored element. Local will have this
    /// interpret its [`Anchored::position`] as relative to the parent element.
    /// While Window will have it interpret the position as relative to the window.
    pub fn position_mode(mut self, mode: AnchoredPositionMode) -> Self {
        self.position_mode = mode;
        self
    }

    /// Snap to window edge instead of switching anchor corner when an overflow would occur.
    pub fn snap_to_window(mut self) -> Self {
        self.fit_mode = AnchoredFitMode::SnapToWindow;
        self
    }

    /// Snap to window edge and leave some margins.
    pub fn snap_to_window_with_margin(mut self, edges: impl Into<Edges<Pixels>>) -> Self {
        self.fit_mode = AnchoredFitMode::SnapToWindowWithMargin(edges.into());
        self
    }

    pub(crate) fn config(&self) -> AnchoredConfig {
        AnchoredConfig {
            anchor_corner: self.anchor_corner,
            fit_mode: self.fit_mode,
            anchor_position: self.anchor_position,
            position_mode: self.position_mode,
            offset: self.offset,
        }
    }
}

impl ParentElement for Anchored {
    fn extend(&mut self, elements: impl IntoIterator<Item = AnyElement>) {
        self.children.extend(elements)
    }
}

impl ElementImpl for Anchored {
    fn children(&self) -> &[AnyElement] {
        &self.children
    }

    fn children_mut(&mut self) -> &mut [AnyElement] {
        &mut self.children
    }

    fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>> {
        Some(Box::new(AnchoredNode::new(self.config())))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<AnchoredNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn crate::RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        if let Some(anchored_node) = node.as_any_mut().downcast_mut::<AnchoredNode>() {
            let config = self.config();
            let update_result = if anchored_node.config != config {
                UpdateResult::LAYOUT_CHANGED
            } else {
                UpdateResult::UNCHANGED
            };
            anchored_node.update_from(config);
            Some(update_result)
        } else {
            None
        }
    }
}

impl IntoElement for Anchored {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

/// Which algorithm to use when fitting the anchored element to be inside the window.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AnchoredFitMode {
    /// Snap the anchored element to the window edge.
    SnapToWindow,
    /// Snap to window edge and leave some margins.
    SnapToWindowWithMargin(Edges<Pixels>),
    /// Switch which corner anchor this anchored element is attached to.
    SwitchAnchor,
}

/// Which algorithm to use when positioning the anchored element.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AnchoredPositionMode {
    /// Position the anchored element relative to the window.
    Window,
    /// Position the anchored element relative to its parent.
    Local,
}

impl AnchoredPositionMode {
    pub(crate) fn get_position_and_bounds(
        &self,
        anchor_position: Option<Point<Pixels>>,
        anchor_corner: Corner,
        size: Size<Pixels>,
        bounds: Bounds<Pixels>,
        offset: Option<Point<Pixels>>,
    ) -> (Point<Pixels>, Bounds<Pixels>) {
        let offset = offset.unwrap_or_default();

        match self {
            AnchoredPositionMode::Window => {
                let anchor_position = anchor_position.unwrap_or(bounds.origin);
                let bounds =
                    Bounds::from_corner_and_size(anchor_corner, anchor_position + offset, size);
                (anchor_position, bounds)
            }
            AnchoredPositionMode::Local => {
                let anchor_position = anchor_position.unwrap_or_default();
                let bounds = Bounds::from_corner_and_size(
                    anchor_corner,
                    bounds.origin + anchor_position + offset,
                    size,
                );
                (anchor_position, bounds)
            }
        }
    }
}

/// Retained render node for Anchored elements.
pub(crate) struct AnchoredNode {
    config: AnchoredConfig,
}

impl AnchoredNode {
    pub fn new(config: AnchoredConfig) -> Self {
        Self { config }
    }

    pub fn update_from(&mut self, config: AnchoredConfig) {
        self.config = config;
    }

    fn compute_offset(
        &self,
        bounds: Bounds<Pixels>,
        child_bounds: &[Bounds<Pixels>],
        window: &Window,
    ) -> Option<Point<Pixels>> {
        if child_bounds.is_empty() {
            return None;
        }

        let mut child_min = point(Pixels::MAX, Pixels::MAX);
        let mut child_max = Point::default();
        for cb in child_bounds {
            child_min = child_min.min(&cb.origin);
            child_max = child_max.max(&cb.bottom_right());
        }
        let size: Size<Pixels> = (child_max - child_min).into();

        let (origin, mut desired) = self.config.position_mode.get_position_and_bounds(
            self.config.anchor_position,
            self.config.anchor_corner,
            size,
            bounds,
            self.config.offset,
        );

        let limits = Bounds {
            origin: Point::default(),
            size: window.viewport_size(),
        };

        if self.config.fit_mode == AnchoredFitMode::SwitchAnchor {
            let mut anchor_corner = self.config.anchor_corner;

            if desired.left() < limits.left() || desired.right() > limits.right() {
                let switched = Bounds::from_corner_and_size(
                    anchor_corner.other_side_corner_along(Axis::Horizontal),
                    origin,
                    size,
                );
                if !(switched.left() < limits.left() || switched.right() > limits.right()) {
                    anchor_corner = anchor_corner.other_side_corner_along(Axis::Horizontal);
                    desired = switched
                }
            }

            if desired.top() < limits.top() || desired.bottom() > limits.bottom() {
                let switched = Bounds::from_corner_and_size(
                    anchor_corner.other_side_corner_along(Axis::Vertical),
                    origin,
                    size,
                );
                if !(switched.top() < limits.top() || switched.bottom() > limits.bottom()) {
                    desired = switched;
                }
            }
        }

        let client_inset = window.client_inset.unwrap_or(px(0.));
        let edges = match self.config.fit_mode {
            AnchoredFitMode::SnapToWindowWithMargin(edges) => edges,
            _ => Edges::default(),
        }
        .map(|edge| *edge + client_inset);

        if desired.right() > limits.right() {
            desired.origin.x -= desired.right() - limits.right() + edges.right;
        }
        if desired.left() < limits.left() {
            desired.origin.x = limits.origin.x + edges.left;
        }

        if desired.bottom() > limits.bottom() {
            desired.origin.y -= desired.bottom() - limits.bottom() + edges.bottom;
        }
        if desired.top() < limits.top() {
            desired.origin.y = limits.origin.y + edges.top;
        }

        let offset = desired.origin - bounds.origin;
        let offset = point(offset.x.round(), offset.y.round());
        if offset.is_zero() { None } else { Some(offset) }
    }
}

impl crate::RenderNode for AnchoredNode {
    fn taffy_style(&self, rem_size: Pixels, scale_factor: f32) -> taffy::style::Style {
        let anchored_style = Style {
            position: Position::Absolute,
            display: Display::Flex,
            ..Style::default()
        };
        anchored_style.to_taffy(rem_size, scale_factor)
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

    fn layout_begin(&mut self, _ctx: &mut crate::LayoutCtx) -> crate::LayoutFrame {
        crate::LayoutFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn prepaint_begin(&mut self, ctx: &mut crate::PrepaintCtx) -> crate::PrepaintFrame {
        if let Some(offset) = self.compute_offset(ctx.bounds, &ctx.child_bounds, ctx.window) {
            ctx.window.transform_stack.push_offset(offset);
            crate::PrepaintFrame {
                handled: true,
                skip_children: false,
                pushed_element_offset: true,
                ..Default::default()
            }
        } else if ctx.child_bounds.is_empty() {
            crate::PrepaintFrame {
                handled: true,
                skip_children: true,
                ..Default::default()
            }
        } else {
            crate::PrepaintFrame {
                handled: true,
                skip_children: false,
                ..Default::default()
            }
        }
    }

    fn prepaint_end(&mut self, ctx: &mut crate::PrepaintCtx, frame: crate::PrepaintFrame) {
        if frame.pushed_element_offset {
            if let Some(offset) = self.compute_offset(ctx.bounds, &ctx.child_bounds, ctx.window) {
                ctx.window.transform_stack.pop_offset(offset);
            }
        }
    }

    fn paint_begin(&mut self, ctx: &mut crate::PaintCtx) -> crate::PaintFrame {
        if let Some(offset) = self.compute_offset(ctx.bounds, &ctx.child_bounds, ctx.window) {
            ctx.window.transform_stack.push_offset(offset);
            crate::PaintFrame {
                handled: true,
                skip_children: false,
                pushed_element_offset: true,
                ..Default::default()
            }
        } else if ctx.child_bounds.is_empty() {
            crate::PaintFrame {
                handled: true,
                skip_children: true,
                ..Default::default()
            }
        } else {
            crate::PaintFrame {
                handled: true,
                skip_children: false,
                ..Default::default()
            }
        }
    }

    fn paint_end(&mut self, ctx: &mut crate::PaintCtx, frame: crate::PaintFrame) {
        if frame.pushed_element_offset {
            if let Some(offset) = self.compute_offset(ctx.bounds, &ctx.child_bounds, ctx.window) {
                ctx.window.transform_stack.pop_offset(offset);
            }
        }
    }

    // Uses default downcasting implementations.
}
