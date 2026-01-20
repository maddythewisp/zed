use std::{any::TypeId, ops::Range, rc::Rc};

use gpui::{
    AnyElement, App, Bounds, Context, Element, Entity, ElementImpl, IntoElement,
    IntrinsicSizeResult, LayoutCtx, LayoutFrame, PaintCtx, PaintFrame, Pixels, Point, Position,
    PrepaintCtx, PrepaintFrame, Render, RenderNode, SizingCtx, Style, TaffyStyle, ToTaffy,
    UniformListDecoration, UpdateResult, Window, point, relative, size,
};
use smallvec::SmallVec;

pub trait StickyCandidate {
    fn depth(&self) -> usize;
}

pub struct StickyItems<T> {
    compute_fn: Rc<dyn Fn(Range<usize>, &mut Window, &mut App) -> SmallVec<[T; 8]>>,
    render_fn: Rc<dyn Fn(T, &mut Window, &mut App) -> SmallVec<[AnyElement; 8]>>,
    decorations: Vec<Box<dyn StickyItemsDecoration>>,
}

pub fn sticky_items<V, T>(
    entity: Entity<V>,
    compute_fn: impl Fn(&mut V, Range<usize>, &mut Window, &mut Context<V>) -> SmallVec<[T; 8]>
    + 'static,
    render_fn: impl Fn(&mut V, T, &mut Window, &mut Context<V>) -> SmallVec<[AnyElement; 8]> + 'static,
) -> StickyItems<T>
where
    V: Render,
    T: StickyCandidate + Clone + 'static,
{
    let entity_compute = entity.clone();
    let entity_render = entity;

    let compute_fn = Rc::new(
        move |range: Range<usize>, window: &mut Window, cx: &mut App| -> SmallVec<[T; 8]> {
            entity_compute.update(cx, |view, cx| compute_fn(view, range, window, cx))
        },
    );
    let render_fn = Rc::new(
        move |entry: T, window: &mut Window, cx: &mut App| -> SmallVec<[AnyElement; 8]> {
            entity_render.update(cx, |view, cx| render_fn(view, entry, window, cx))
        },
    );

    StickyItems {
        compute_fn,
        render_fn,
        decorations: Vec::new(),
    }
}

impl<T> StickyItems<T>
where
    T: StickyCandidate + Clone + 'static,
{
    /// Adds a decoration element to the sticky items.
    pub fn with_decoration(mut self, decoration: impl StickyItemsDecoration + 'static) -> Self {
        self.decorations.push(Box::new(decoration));
        self
    }
}

#[derive(Element)]
struct StickyItemsElement {
    children: SmallVec<[AnyElement; 16]>,
}

impl IntoElement for StickyItemsElement {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl ElementImpl for StickyItemsElement {
    fn children(&self) -> &[AnyElement] {
        &self.children
    }

    fn children_mut(&mut self) -> &mut [AnyElement] {
        &mut self.children
    }

    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        Some(Box::new(StickyItemsNode))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<StickyItemsNode>())
    }

    fn update_render_node(
        &mut self,
        _node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        Some(UpdateResult::UNCHANGED)
    }
}

struct StickyItemsNode;

impl RenderNode for StickyItemsNode {
    fn taffy_style(&self, rem_size: Pixels, scale_factor: f32) -> TaffyStyle {
        let style = Style {
            position: Position::Relative,
            size: size(relative(1.).into(), relative(1.).into()),
            ..Style::default()
        };
        style.to_taffy(rem_size, scale_factor)
    }

    fn compute_intrinsic_size(&mut self, _ctx: &mut SizingCtx) -> IntrinsicSizeResult {
        IntrinsicSizeResult {
            size: gpui::IntrinsicSize::default(),
            input: gpui::SizingInput::default(),
        }
    }

    fn layout_begin(&mut self, _ctx: &mut LayoutCtx) -> LayoutFrame {
        LayoutFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn layout_end(&mut self, _ctx: &mut LayoutCtx, _frame: LayoutFrame) {}

    fn prepaint_begin(&mut self, _ctx: &mut PrepaintCtx) -> PrepaintFrame {
        PrepaintFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn prepaint_end(&mut self, _ctx: &mut PrepaintCtx, _frame: PrepaintFrame) {}

    fn paint_begin(&mut self, _ctx: &mut PaintCtx) -> PaintFrame {
        PaintFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn paint_end(&mut self, _ctx: &mut PaintCtx, _frame: PaintFrame) {}
}

impl<T> UniformListDecoration for StickyItems<T>
where
    T: StickyCandidate + Clone + 'static,
{
    fn compute(
        &self,
        visible_range: Range<usize>,
        bounds: Bounds<Pixels>,
        scroll_offset: Point<Pixels>,
        item_height: Pixels,
        _item_count: usize,
        window: &mut Window,
        cx: &mut App,
    ) -> AnyElement {
        use crate::prelude::*;

        let entries = (self.compute_fn)(visible_range.clone(), window, cx);

        let Some(sticky_anchor) = find_sticky_anchor(&entries, visible_range.start) else {
            return StickyItemsElement {
                children: SmallVec::new(),
            }
            .into_any_element();
        };

        let anchor_depth = sticky_anchor.entry.depth();
        let mut elements = (self.render_fn)(sticky_anchor.entry, window, cx);
        let items_count = elements.len();

        let indents: SmallVec<[usize; 8]> = (0..items_count)
            .map(|ix| anchor_depth.saturating_sub(items_count.saturating_sub(ix)))
            .collect();

        let expanded_width = bounds.size.width + scroll_offset.x.abs();

        let drifting_y_offset = if sticky_anchor.drifting {
            let scroll_top = -scroll_offset.y;
            let anchor_top = item_height * (sticky_anchor.index + 1);
            let sticky_area_height = item_height * items_count;
            (anchor_top - scroll_top - sticky_area_height).min(Pixels::ZERO)
        } else {
            Pixels::ZERO
        };

        let (drifting_indent, rest_indents) = if sticky_anchor.drifting && !indents.is_empty() {
            let last = indents[indents.len() - 1];
            let rest: SmallVec<[usize; 8]> = indents[..indents.len() - 1].iter().copied().collect();
            (Some(last), rest)
        } else {
            (None, indents)
        };

        // Base y-offset for positioning children relative to StickyItemsElement
        let base_y = -scroll_offset.y;

        let mut children: SmallVec<[AnyElement; 16]> = SmallVec::new();

        // Add rest element children (order matters for hitbox priority)
        let (mut drifting_element, rest_elements) =
            if sticky_anchor.drifting && !elements.is_empty() {
                let last = elements.pop().expect("elements is not empty");
                (Some(last), elements)
            } else {
                (None, elements)
            };

        for (ix, element) in rest_elements.into_iter().enumerate() {
            let top = base_y + item_height * ix;
            let wrapper = div()
                .absolute()
                .top(top)
                .left(px(0.))
                .w(expanded_width)
                .h(item_height)
                .child(element);
            children.push(wrapper.into_any_element());
        }

        // Add drifting element (painted after rest so it appears on top)
        if let Some(drifting_element) = drifting_element.take() {
            let rest_count = children.len();
            let top = base_y + item_height * rest_count + drifting_y_offset;
            let wrapper = div()
                .absolute()
                .top(top)
                .left(px(0.))
                .w(expanded_width)
                .h(item_height)
                .child(drifting_element);
            children.push(wrapper.into_any_element());
        }

        // Add decoration children
        for decoration in &self.decorations {
            // Rest decorations (painted first, so they appear behind elements)
            if !rest_indents.is_empty() {
                let decoration_bounds = Bounds::new(
                    bounds.origin + point(px(0.), base_y),
                    bounds.size,
                );
                let rest_dec = decoration.as_ref().compute(
                    &rest_indents,
                    decoration_bounds,
                    scroll_offset,
                    item_height,
                    window,
                    cx,
                );
                let wrapper = div()
                    .absolute()
                    .top(px(0.))
                    .left(px(0.))
                    .w(expanded_width)
                    .h(bounds.size.height)
                    .child(rest_dec);
                children.insert(0, wrapper.into_any_element());
            }

            // Drifting decoration
            if let Some(drifting_indent) = drifting_indent {
                let drifting_indent_vec: SmallVec<[usize; 8]> =
                    [drifting_indent].into_iter().collect();

                let drifting_top = base_y + item_height * rest_indents.len() + drifting_y_offset;
                let decoration_bounds = Bounds::new(
                    bounds.origin + point(px(0.), drifting_top),
                    bounds.size,
                );

                let drifting_dec = decoration.as_ref().compute(
                    &drifting_indent_vec,
                    decoration_bounds,
                    scroll_offset,
                    item_height,
                    window,
                    cx,
                );
                let wrapper = div()
                    .absolute()
                    .top(drifting_top)
                    .left(px(0.))
                    .w(expanded_width)
                    .h(bounds.size.height)
                    .child(drifting_dec);
                children.push(wrapper.into_any_element());
            }
        }

        StickyItemsElement { children }.into_any_element()
    }
}

struct StickyAnchor<T> {
    entry: T,
    index: usize,
    drifting: bool,
}

fn find_sticky_anchor<T: StickyCandidate + Clone>(
    entries: &SmallVec<[T; 8]>,
    visible_range_start: usize,
) -> Option<StickyAnchor<T>> {
    let mut iter = entries.iter().enumerate().peekable();
    while let Some((ix, current_entry)) = iter.next() {
        let depth = current_entry.depth();

        if depth < ix {
            return Some(StickyAnchor {
                entry: current_entry.clone(),
                index: visible_range_start + ix,
                drifting: false,
            });
        }

        if let Some(&(_next_ix, next_entry)) = iter.peek() {
            let next_depth = next_entry.depth();
            let next_item_outdented = next_depth + 1 == depth;

            let depth_same_as_index = depth == ix;
            let depth_greater_than_index = depth == ix + 1;

            if next_item_outdented && (depth_same_as_index || depth_greater_than_index) {
                return Some(StickyAnchor {
                    entry: current_entry.clone(),
                    index: visible_range_start + ix,
                    drifting: depth_greater_than_index,
                });
            }
        }
    }

    None
}

/// A decoration for a [`StickyItems`]. This can be used for various things,
/// such as rendering indent guides, or other visual effects.
pub trait StickyItemsDecoration {
    /// Compute the decoration element, given the visible range of list items,
    /// the bounds of the list, and the height of each item.
    fn compute(
        &self,
        indents: &SmallVec<[usize; 8]>,
        bounds: Bounds<Pixels>,
        scroll_offset: Point<Pixels>,
        item_height: Pixels,
        window: &mut Window,
        cx: &mut App,
    ) -> AnyElement;
}
