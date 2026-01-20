use gpui_macros::Element;
use refineable::Refineable as _;
use std::any::TypeId;
use taffy::style::Style as TaffyStyle;

use crate::taffy::ToTaffy;
use crate::{
    App, AvailableSpace, Bounds, CallbackSlot, ElementImpl, IntoElement,
    IntrinsicSize, IntrinsicSizeResult, LayoutCtx, LayoutFrame, PaintCtx, PaintFrame, Pixels,
    PrepaintCtx, PrepaintFrame, RenderNode, Size, SizingCtx, SizingInput, Style, StyleRefinement,
    Styled, UpdateResult, Window,
};

/// Construct a canvas element with the given paint callback.
/// Useful for adding short term custom drawing to a view.
pub fn canvas<T>(
    prepaint: impl 'static + FnMut(Bounds<Pixels>, &mut Window, &mut App) -> T,
    paint: impl 'static + FnMut(Bounds<Pixels>, &mut T, &mut Window, &mut App),
) -> Canvas<T> {
    Canvas {
        prepaint: Some(Box::new(prepaint)),
        paint: Some(Box::new(paint)),
        style: StyleRefinement::default(),
    }
}

/// A canvas element, meant for accessing the low level paint API without defining a whole
/// custom element
#[derive(Element)]
#[element(crate = crate)]
pub struct Canvas<T: 'static> {
    prepaint: Option<Box<dyn FnMut(Bounds<Pixels>, &mut Window, &mut App) -> T>>,
    paint: Option<Box<dyn FnMut(Bounds<Pixels>, &mut T, &mut Window, &mut App)>>,
    style: StyleRefinement,
}

impl<T: 'static> IntoElement for Canvas<T> {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl<T: 'static> ElementImpl for Canvas<T> {
    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        let mut style = Style::default();
        style.refine(&self.style);

        let mut node = CanvasNode::<T>::default();
        node.style = style;

        if let Some(prepaint) = self.prepaint.take() {
            node.prepaint_fn.deposit(prepaint);
        }
        if let Some(paint) = self.paint.take() {
            node.paint_fn.deposit(paint);
        }

        Some(Box::new(node))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<CanvasNode<T>>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let node = node.as_any_mut().downcast_mut::<CanvasNode<T>>()?;

        let mut new_style = Style::default();
        new_style.refine(&self.style);

        let layout_changed = !node.style.layout_eq(&new_style);
        let mut paint_changed = !node.style.paint_eq(&new_style) || layout_changed;

        node.style = new_style;

        if let Some(prepaint_fn) = self.prepaint.take() {
            node.prepaint_fn.deposit(prepaint_fn);
            paint_changed = true;
        }
        if let Some(paint_fn) = self.paint.take() {
            node.paint_fn.deposit(paint_fn);
            paint_changed = true;
        }

        Some(UpdateResult {
            layout_changed,
            paint_changed,
        })
    }
}

impl<T> Styled for Canvas<T> {
    fn style(&mut self) -> &mut crate::StyleRefinement {
        &mut self.style
    }
}

pub(crate) struct CanvasNode<T: 'static> {
    style: Style,
    state: Option<T>,
    prepaint_fn: CallbackSlot<dyn FnMut(Bounds<Pixels>, &mut Window, &mut App) -> T>,
    paint_fn: CallbackSlot<dyn FnMut(Bounds<Pixels>, &mut T, &mut Window, &mut App)>,
}

impl<T: 'static> Default for CanvasNode<T> {
    fn default() -> Self {
        Self {
            style: Style::default(),
            state: None,
            prepaint_fn: CallbackSlot::new(),
            paint_fn: CallbackSlot::new(),
        }
    }
}

impl<T: 'static> RenderNode for CanvasNode<T> {
    fn taffy_style(&self, rem_size: Pixels, scale_factor: f32) -> TaffyStyle {
        self.style.to_taffy(rem_size, scale_factor)
    }

    fn needs_child_bounds(&self) -> bool {
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

    fn compute_intrinsic_size(&mut self, _ctx: &mut SizingCtx) -> IntrinsicSizeResult {
        IntrinsicSizeResult {
            size: IntrinsicSize::default(),
            input: SizingInput::default(),
        }
    }

    fn uses_intrinsic_sizing_cache(&self) -> bool {
        false
    }

    fn layout_begin(&mut self, _ctx: &mut LayoutCtx) -> LayoutFrame {
        LayoutFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn layout_end(&mut self, _ctx: &mut LayoutCtx, _frame: LayoutFrame) {}

    fn prepaint_begin(&mut self, ctx: &mut PrepaintCtx) -> PrepaintFrame {
        let bounds = ctx.bounds;
        let window = &mut *ctx.window;
        let cx = &mut *ctx.cx;

        if let Some(new_state) = self
            .prepaint_fn
            .with_mut(|prepaint_fn| prepaint_fn(bounds, window, cx))
        {
            self.state = Some(new_state);
        }

        PrepaintFrame {
            handled: true,
            skip_children: true,
            ..Default::default()
        }
    }

    fn prepaint_end(&mut self, _ctx: &mut PrepaintCtx, _frame: PrepaintFrame) {}

    fn paint_begin(&mut self, ctx: &mut PaintCtx) -> PaintFrame {
        let Some(state) = self.state.as_mut() else {
            return PaintFrame {
                handled: true,
                skip_children: true,
                ..Default::default()
            };
        };

        let bounds = ctx.bounds;
        let style = &mut self.style;
        let window = &mut *ctx.window;
        let cx = &mut *ctx.cx;
        self.paint_fn.with_mut(|paint_fn| {
            style.paint(bounds, window, cx, |window, cx| {
                paint_fn(bounds, state, window, cx);
            });
        });

        PaintFrame {
            handled: true,
            skip_children: true,
            ..Default::default()
        }
    }

    fn paint_end(&mut self, _ctx: &mut PaintCtx, _frame: PaintFrame) {}
}
