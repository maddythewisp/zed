use gpui_macros::Element;
use std::{any::TypeId, fs, path::Path, sync::Arc};

use crate::{
    App, Asset, AvailableSpace, Bounds, ElementId, ElementImpl,
    InteractiveElement, Interactivity, IntoElement, Pixels, Point, Radians, RenderNode,
    SharedString, Size, StyleRefinement, Styled, TransformationMatrix, UpdateResult, Window,
    point, px, radians, size, taffy::ToTaffy,
};
use futures::Future;
use refineable::Refineable;
use taffy::style::Style as TaffyStyle;

/// An SVG element.
#[derive(Element)]
#[element(crate = crate)]
pub struct Svg {
    interactivity: Interactivity,
    transformation: Option<Transformation>,
    path: Option<SharedString>,
    external_path: Option<SharedString>,
}

/// Create a new SVG element.
#[track_caller]
pub fn svg() -> Svg {
    Svg {
        interactivity: Interactivity::new(),
        transformation: None,
        path: None,
        external_path: None,
    }
}

impl Svg {
    /// Set the path to the SVG file for this element.
    pub fn path(mut self, path: impl Into<SharedString>) -> Self {
        self.path = Some(path.into());
        self
    }

    /// Set the path to the SVG file for this element.
    pub fn external_path(mut self, path: impl Into<SharedString>) -> Self {
        self.external_path = Some(path.into());
        self
    }

    /// Transform the SVG element with the given transformation.
    /// Note that this won't effect the hitbox or layout of the element, only the rendering.
    pub fn with_transformation(mut self, transformation: Transformation) -> Self {
        self.transformation = Some(transformation);
        self
    }

    pub(crate) fn take_interactivity(&mut self) -> Interactivity {
        std::mem::take(&mut self.interactivity)
    }
}

impl ElementImpl for Svg {
    fn id(&self) -> Option<ElementId> {
        self.interactivity.element_id.clone()
    }

    fn source_location(&self) -> Option<&'static std::panic::Location<'static>> {
        self.interactivity.source_location()
    }

    fn cached_style(&self) -> Option<&StyleRefinement> {
        Some(&self.interactivity.base_style)
    }

    fn take_layout_listener(
        &mut self,
    ) -> Option<Box<dyn FnMut(Bounds<Pixels>, &mut Window, &mut App) + 'static>> {
        self.interactivity.layout_listener.take()
    }

    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        Some(Box::new(SvgNode::new(
            self.take_interactivity(),
            self.path.take(),
            self.external_path.take(),
            self.transformation.take(),
        )))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<SvgNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        if let Some(svg_node) = node.as_any_mut().downcast_mut::<SvgNode>() {
            let interactivity = self.take_interactivity();
            let update_result = svg_node.interactivity.diff_styles(&interactivity);

            let path = self.path.take();
            let external_path = self.external_path.take();
            let transformation = self.transformation.take();

            let content_changed = svg_node.path != path
                || svg_node.external_path != external_path
                || svg_node.transformation != transformation;

            let mut layout_changed = update_result.layout_changed;
            let mut paint_changed = update_result.paint_changed;
            if content_changed {
                paint_changed = true;
            }
            if layout_changed {
                paint_changed = true;
            }

            svg_node.update_from(interactivity, path, external_path, transformation);
            Some(UpdateResult {
                layout_changed,
                paint_changed,
            })
        } else {
            None
        }
    }
}

impl IntoElement for Svg {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl Styled for Svg {
    fn style(&mut self) -> &mut StyleRefinement {
        &mut self.interactivity.base_style
    }
}

impl InteractiveElement for Svg {
    fn interactivity(&mut self) -> &mut Interactivity {
        &mut self.interactivity
    }
}

/// A transformation to apply to an SVG element.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Transformation {
    scale: Size<f32>,
    translate: Point<Pixels>,
    rotate: Radians,
}

impl Default for Transformation {
    fn default() -> Self {
        Self {
            scale: size(1.0, 1.0),
            translate: point(px(0.0), px(0.0)),
            rotate: radians(0.0),
        }
    }
}

impl Transformation {
    /// Create a new Transformation with the specified scale along each axis.
    pub fn scale(scale: Size<f32>) -> Self {
        Self {
            scale,
            translate: point(px(0.0), px(0.0)),
            rotate: radians(0.0),
        }
    }

    /// Create a new Transformation with the specified translation.
    pub fn translate(translate: Point<Pixels>) -> Self {
        Self {
            scale: size(1.0, 1.0),
            translate,
            rotate: radians(0.0),
        }
    }

    /// Create a new Transformation with the specified rotation in radians.
    pub fn rotate(rotate: impl Into<Radians>) -> Self {
        let rotate = rotate.into();
        Self {
            scale: size(1.0, 1.0),
            translate: point(px(0.0), px(0.0)),
            rotate,
        }
    }

    /// Update the scaling factor of this transformation.
    pub fn with_scaling(mut self, scale: Size<f32>) -> Self {
        self.scale = scale;
        self
    }

    /// Update the translation value of this transformation.
    pub fn with_translation(mut self, translate: Point<Pixels>) -> Self {
        self.translate = translate;
        self
    }

    /// Update the rotation angle of this transformation.
    pub fn with_rotation(mut self, rotate: impl Into<Radians>) -> Self {
        self.rotate = rotate.into();
        self
    }

    pub(crate) fn into_matrix(
        self,
        center: Point<Pixels>,
        scale_factor: f32,
    ) -> TransformationMatrix {
        // MonochromeSprite bounds are in device (ScaledPixels) space, so the transform
        // must also be in device space. Scale the translation values accordingly.
        //Note: if you read this as a sequence of matrix multiplications, start from the bottom
        let scaled_center = point(px(center.x.0 * scale_factor), px(center.y.0 * scale_factor));
        let scaled_translate = point(
            px(self.translate.x.0 * scale_factor),
            px(self.translate.y.0 * scale_factor),
        );
        TransformationMatrix::unit()
            .translate(scaled_center + scaled_translate)
            .rotate(self.rotate)
            .scale(self.scale)
            .translate(point(px(-scaled_center.x.0), px(-scaled_center.y.0)))
    }
}

pub(crate) enum SvgAsset {}

impl Asset for SvgAsset {
    type Source = SharedString;
    type Output = Result<Arc<[u8]>, Arc<std::io::Error>>;

    fn load(
        source: Self::Source,
        _cx: &mut App,
    ) -> impl Future<Output = Self::Output> + Send + 'static {
        async move {
            let bytes = fs::read(Path::new(source.as_ref())).map_err(|e| Arc::new(e))?;
            let bytes = Arc::from(bytes);
            Ok(bytes)
        }
    }
}

/// Retained render node for SVG elements.
///
/// This node owns all SVG-specific data including Interactivity, enabling
/// the node to fully handle layout, prepaint, and paint phases.
pub(crate) struct SvgNode {
    /// Interactivity state for this SVG element.
    pub interactivity: Interactivity,
    /// Path to the SVG content (inline or asset).
    pub path: Option<SharedString>,
    /// Path to an external SVG file.
    pub external_path: Option<SharedString>,
    /// Transformation to apply to the SVG.
    pub transformation: Option<Transformation>,
}

impl SvgNode {
    /// Create a new SvgNode from descriptor data.
    pub fn new(
        interactivity: Interactivity,
        path: Option<SharedString>,
        external_path: Option<SharedString>,
        transformation: Option<Transformation>,
    ) -> Self {
        Self {
            interactivity,
            path,
            external_path,
            transformation,
        }
    }

    /// Update this node from a descriptor.
    pub fn update_from(
        &mut self,
        interactivity: Interactivity,
        path: Option<SharedString>,
        external_path: Option<SharedString>,
        transformation: Option<Transformation>,
    ) {
        self.interactivity = interactivity;
        self.path = path;
        self.external_path = external_path;
        self.transformation = transformation;
    }
}

impl RenderNode for SvgNode {
    fn taffy_style(&self, rem_size: crate::Pixels, scale_factor: f32) -> TaffyStyle {
        // Compute taffy style from interactivity.base_style
        let mut style = crate::Style::default();
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

    fn uses_intrinsic_sizing_cache(&self) -> bool {
        // SVG nodes don't currently compute intrinsic sizes. Use `measure` instead so first-layout
        // measurement never depends on an intrinsic sizing pass having run.
        false
    }

    fn measure(
        &mut self,
        _known: Size<Option<Pixels>>,
        _available: Size<AvailableSpace>,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<Size<Pixels>> {
        Some(Size {
            width: Pixels(0.0),
            height: Pixels(0.0),
        })
    }

    fn layout_begin(&mut self, _ctx: &mut crate::LayoutCtx) -> crate::LayoutFrame {
        crate::LayoutFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn prepaint_begin(&mut self, ctx: &mut crate::PrepaintCtx) -> crate::PrepaintFrame {
        // Call prepare_prepaint to set up hitbox and event handlers
        let hitbox = self.interactivity.prepaint(
            Some(&ctx.fiber_id),
            ctx.inspector_id.as_ref(),
            ctx.bounds,
            ctx.bounds.size,
            ctx.window,
            ctx.cx,
            |_, _, hitbox, _, _| hitbox,
        );

        crate::PrepaintFrame {
            handled: true,
            skip_children: true, // SVG is always a leaf node
            hitbox,
            ..Default::default()
        }
    }

    fn prepaint_end(&mut self, _ctx: &mut crate::PrepaintCtx, _frame: crate::PrepaintFrame) {
        // Nothing to pop for SVG - it doesn't push any context
    }

    fn paint_begin(&mut self, ctx: &mut crate::PaintCtx) -> crate::PaintFrame {
        // Get hitbox from window (registered during prepaint)
        let hitbox = ctx.window.resolve_hitbox(&ctx.fiber_id);

        // Use interactivity.paint which handles style resolution, shadows, borders, etc.
        // The closure does SVG-specific painting
        let path = self.path.clone();
        let external_path = self.external_path.clone();
        let transformation = self.transformation;

        self.interactivity.paint(
            Some(&ctx.fiber_id),
            ctx.inspector_id.as_ref(),
            ctx.bounds,
            hitbox.as_ref(),
            ctx.window,
            ctx.cx,
            |style, window, cx| {
                if let Some(color) = style.text.color {
                    window.paint_svg_paths(
                        ctx.bounds,
                        path.as_ref(),
                        external_path.as_ref(),
                        transformation,
                        color,
                        cx,
                    );
                }
            },
        );

        crate::PaintFrame {
            handled: true,
            skip_children: true, // SVG is always a leaf node
            ..Default::default()
        }
    }

    fn paint_end(&mut self, _ctx: &mut crate::PaintCtx, _frame: crate::PaintFrame) {
        // Nothing to pop for SVG - it doesn't push any context
    }

    fn interactivity(&self) -> Option<&crate::Interactivity> {
        Some(&self.interactivity)
    }
}
