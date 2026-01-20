use std::{any::TypeId, cmp::Ordering, ops::Range, rc::Rc};

use gpui::{
    AnyElement, App, Bounds, DispatchPhase, ElementImpl, Entity, Hitbox,
    HitboxBehavior, Hsla, IntoElement, MouseButton, MouseDownEvent, MouseMoveEvent, PaintFrame,
    Pixels, Point, PrepaintFrame, RenderNode, UpdateResult, Window, fill, point, size,
};
use smallvec::SmallVec;

use crate::prelude::*;

/// Represents the colors used for different states of indent guides.
#[derive(Debug, Clone)]
pub struct IndentGuideColors {
    /// The color of the indent guide when it's neither active nor hovered.
    pub default: Hsla,
    /// The color of the indent guide when it's hovered.
    pub hover: Hsla,
    /// The color of the indent guide when it's active.
    pub active: Hsla,
}

impl IndentGuideColors {
    /// Returns the indent guide colors that should be used for panels.
    pub fn panel(cx: &App) -> Self {
        Self {
            default: cx.theme().colors().panel_indent_guide,
            hover: cx.theme().colors().panel_indent_guide_hover,
            active: cx.theme().colors().panel_indent_guide_active,
        }
    }
}

pub struct IndentGuides {
    colors: IndentGuideColors,
    indent_size: Pixels,
    compute_indents_fn:
        Option<Box<dyn Fn(Range<usize>, &mut Window, &mut App) -> SmallVec<[usize; 64]>>>,
    render_fn: Option<
        Box<
            dyn Fn(
                RenderIndentGuideParams,
                &mut Window,
                &mut App,
            ) -> SmallVec<[RenderedIndentGuide; 12]>,
        >,
    >,
    on_click: Option<Rc<dyn Fn(&IndentGuideLayout, &mut Window, &mut App)>>,
}

pub fn indent_guides(indent_size: Pixels, colors: IndentGuideColors) -> IndentGuides {
    IndentGuides {
        colors,
        indent_size,
        compute_indents_fn: None,
        render_fn: None,
        on_click: None,
    }
}

impl IndentGuides {
    /// Sets the callback that will be called when the user clicks on an indent guide.
    pub fn on_click(
        mut self,
        on_click: impl Fn(&IndentGuideLayout, &mut Window, &mut App) + 'static,
    ) -> Self {
        self.on_click = Some(Rc::new(on_click));
        self
    }

    /// Sets the function that computes indents for uniform list decoration.
    pub fn with_compute_indents_fn<V: Render>(
        mut self,
        entity: Entity<V>,
        compute_indents_fn: impl Fn(
            &mut V,
            Range<usize>,
            &mut Window,
            &mut Context<V>,
        ) -> SmallVec<[usize; 64]>
        + 'static,
    ) -> Self {
        let compute_indents_fn = Box::new(move |range, window: &mut Window, cx: &mut App| {
            entity.update(cx, |this, cx| compute_indents_fn(this, range, window, cx))
        });
        self.compute_indents_fn = Some(compute_indents_fn);
        self
    }

    /// Sets a custom callback that will be called when the indent guides need to be rendered.
    pub fn with_render_fn<V: Render>(
        mut self,
        entity: Entity<V>,
        render_fn: impl Fn(
            &mut V,
            RenderIndentGuideParams,
            &mut Window,
            &mut App,
        ) -> SmallVec<[RenderedIndentGuide; 12]>
        + 'static,
    ) -> Self {
        let render_fn = move |params, window: &mut Window, cx: &mut App| {
            entity.update(cx, |this, cx| render_fn(this, params, window, cx))
        };
        self.render_fn = Some(Box::new(render_fn));
        self
    }

    fn render_from_layout(
        &self,
        indent_guides: SmallVec<[IndentGuideLayout; 12]>,
        bounds: Bounds<Pixels>,
        item_height: Pixels,
        window: &mut Window,
        cx: &mut App,
    ) -> AnyElement {
        let mut indent_guides = if let Some(ref custom_render) = self.render_fn {
            let params = RenderIndentGuideParams {
                indent_guides,
                indent_size: self.indent_size,
                item_height,
            };
            custom_render(params, window, cx)
        } else {
            indent_guides
                .into_iter()
                .map(|layout| RenderedIndentGuide {
                    bounds: Bounds::new(
                        point(
                            layout.offset.x * self.indent_size,
                            layout.offset.y * item_height,
                        ),
                        size(px(1.), layout.length * item_height),
                    ),
                    layout,
                    is_active: false,
                    hitbox: None,
                })
                .collect()
        };
        for guide in &mut indent_guides {
            guide.bounds.origin += bounds.origin;
            if let Some(hitbox) = guide.hitbox.as_mut() {
                hitbox.origin += bounds.origin;
            }
        }

        let indent_guides = IndentGuidesElement {
            indent_guides: Rc::new(indent_guides),
            colors: self.colors.clone(),
            on_hovered_indent_guide_click: self.on_click.clone(),
        };
        indent_guides.into_any_element()
    }
}

/// Parameters for rendering indent guides.
pub struct RenderIndentGuideParams {
    /// The calculated layouts for the indent guides to be rendered.
    pub indent_guides: SmallVec<[IndentGuideLayout; 12]>,
    /// The size of each indentation level in pixels.
    pub indent_size: Pixels,
    /// The height of each item in pixels.
    pub item_height: Pixels,
}

/// Represents a rendered indent guide with its visual properties and interaction areas.
pub struct RenderedIndentGuide {
    /// The bounds of the rendered indent guide in pixels.
    pub bounds: Bounds<Pixels>,
    /// The layout information for the indent guide.
    pub layout: IndentGuideLayout,
    /// Indicates whether the indent guide is currently active.
    pub is_active: bool,
    /// Can be used to customize the hitbox of the indent guide,
    /// if this is set to `None`, the bounds of the indent guide will be used.
    pub hitbox: Option<Bounds<Pixels>>,
}

/// Represents the layout information for an indent guide.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct IndentGuideLayout {
    /// The starting position of the indent guide, where x is the indentation level
    /// and y is the starting row.
    pub offset: Point<usize>,
    /// The length of the indent guide in rows.
    pub length: usize,
    /// Indicates whether the indent guide continues beyond the visible bounds.
    pub continues_offscreen: bool,
}

/// Implements the necessary functionality for rendering indent guides inside a uniform list.
mod uniform_list {
    use gpui::UniformListDecoration;

    use super::*;

    impl UniformListDecoration for IndentGuides {
        fn compute(
            &self,
            mut visible_range: Range<usize>,
            bounds: Bounds<Pixels>,
            _scroll_offset: Point<Pixels>,
            item_height: Pixels,
            item_count: usize,
            window: &mut Window,
            cx: &mut App,
        ) -> AnyElement {
            let includes_trailing_indent = visible_range.end < item_count;
            // Check if we have entries after the visible range,
            // if so extend the visible range so we can fetch a trailing indent,
            // which is needed to compute indent guides correctly.
            if includes_trailing_indent {
                visible_range.end += 1;
            }
            let Some(ref compute_indents_fn) = self.compute_indents_fn else {
                panic!("compute_indents_fn is required for UniformListDecoration");
            };
            let visible_entries = &compute_indents_fn(visible_range.clone(), window, cx);
            let indent_guides = compute_indent_guides(
                visible_entries,
                visible_range.start,
                includes_trailing_indent,
            );
            self.render_from_layout(indent_guides, bounds, item_height, window, cx)
        }
    }
}

/// Implements the necessary functionality for rendering indent guides inside a sticky items.
mod sticky_items {
    use crate::StickyItemsDecoration;

    use super::*;

    impl StickyItemsDecoration for IndentGuides {
        fn compute(
            &self,
            indents: &SmallVec<[usize; 8]>,
            bounds: Bounds<Pixels>,
            _scroll_offset: Point<Pixels>,
            item_height: Pixels,
            window: &mut Window,
            cx: &mut App,
        ) -> AnyElement {
            let indent_guides = compute_indent_guides(indents, 0, false);
            self.render_from_layout(indent_guides, bounds, item_height, window, cx)
        }
    }
}

#[derive(Element)]
#[element(crate = gpui)]
struct IndentGuidesElement {
    colors: IndentGuideColors,
    indent_guides: Rc<SmallVec<[RenderedIndentGuide; 12]>>,
    on_hovered_indent_guide_click: Option<Rc<dyn Fn(&IndentGuideLayout, &mut Window, &mut App)>>,
}

impl ElementImpl for IndentGuidesElement {
    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        Some(Box::new(IndentGuidesNode {
            colors: self.colors.clone(),
            indent_guides: self.indent_guides.clone(),
            on_hovered_indent_guide_click: self.on_hovered_indent_guide_click.clone(),
            hitboxes: None,
        }))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<IndentGuidesNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let node = node.as_any_mut().downcast_mut::<IndentGuidesNode>()?;

        let paint_changed = !Rc::ptr_eq(&node.indent_guides, &self.indent_guides)
            || node.colors.default != self.colors.default
            || node.colors.hover != self.colors.hover
            || node.colors.active != self.colors.active;

        node.colors = self.colors.clone();
        node.indent_guides = self.indent_guides.clone();
        node.on_hovered_indent_guide_click = self.on_hovered_indent_guide_click.clone();

        Some(UpdateResult {
            layout_changed: false,
            paint_changed,
        })
    }
}

impl IntoElement for IndentGuidesElement {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

struct IndentGuidesNode {
    colors: IndentGuideColors,
    indent_guides: Rc<SmallVec<[RenderedIndentGuide; 12]>>,
    on_hovered_indent_guide_click: Option<Rc<dyn Fn(&IndentGuideLayout, &mut Window, &mut App)>>,
    hitboxes: Option<Rc<SmallVec<[Hitbox; 12]>>>,
}

impl RenderNode for IndentGuidesNode {
    fn prepaint_begin(&mut self, ctx: &mut gpui::PrepaintCtx) -> PrepaintFrame {
        if self.on_hovered_indent_guide_click.is_some() {
            let hitboxes: SmallVec<[Hitbox; 12]> = self
                .indent_guides
                .iter()
                .map(|guide| {
                    ctx.window
                        .insert_hitbox(guide.hitbox.unwrap_or(guide.bounds), HitboxBehavior::Normal)
                })
                .collect();
            self.hitboxes = Some(Rc::new(hitboxes));
        } else {
            self.hitboxes = None;
        }

        PrepaintFrame {
            handled: true,
            skip_children: true,
            ..Default::default()
        }
    }

    // Uses on_mouse_event because indent guides have multiple hitboxes requiring manual hit testing
    #[allow(deprecated)]
    fn paint_begin(&mut self, ctx: &mut gpui::PaintCtx) -> PaintFrame {
        let current_view = ctx.window.current_view();

        match (&self.hitboxes, &self.on_hovered_indent_guide_click) {
            (Some(hitboxes), Some(on_click)) => {
                ctx.window.on_mouse_event({
                    let hitboxes = hitboxes.clone();
                    let indent_guides = self.indent_guides.clone();
                    let on_click = on_click.clone();
                    move |event: &MouseDownEvent, phase, window, cx| {
                        if phase == DispatchPhase::Bubble && event.button == MouseButton::Left {
                            let mut active_hitbox_ix = None;
                            for (i, hitbox) in hitboxes.iter().enumerate() {
                                if hitbox.is_hovered(window) {
                                    active_hitbox_ix = Some(i);
                                    break;
                                }
                            }

                            let Some(active_hitbox_ix) = active_hitbox_ix else {
                                return;
                            };

                            let active_indent_guide = &indent_guides[active_hitbox_ix].layout;
                            on_click(active_indent_guide, window, cx);

                            cx.stop_propagation();
                            window.prevent_default();
                        }
                    }
                });

                let mut hovered_hitbox_id = None;
                for (i, hitbox) in hitboxes.iter().enumerate() {
                    ctx.window
                        .set_cursor_style(gpui::CursorStyle::PointingHand, hitbox);
                    let indent_guide = &self.indent_guides[i];
                    let fill_color = if hitbox.is_hovered(ctx.window) {
                        hovered_hitbox_id = Some(hitbox.id);
                        self.colors.hover
                    } else if indent_guide.is_active {
                        self.colors.active
                    } else {
                        self.colors.default
                    };

                    ctx.window.paint_quad(fill(indent_guide.bounds, fill_color));
                }

                ctx.window.on_mouse_event({
                    let prev_hovered_hitbox_id = hovered_hitbox_id;
                    let hitboxes = hitboxes.clone();
                    move |_: &MouseMoveEvent, phase, window, cx| {
                        let mut hovered_hitbox_id = None;
                        for hitbox in hitboxes.as_ref() {
                            if hitbox.is_hovered(window) {
                                hovered_hitbox_id = Some(hitbox.id);
                                break;
                            }
                        }
                        if phase == DispatchPhase::Capture {
                            match (prev_hovered_hitbox_id, hovered_hitbox_id) {
                                (Some(prev_id), Some(id)) => {
                                    if prev_id != id {
                                        cx.notify(current_view)
                                    }
                                }
                                (None, Some(_)) => cx.notify(current_view),
                                (Some(_), None) => cx.notify(current_view),
                                (None, None) => {}
                            }
                        }
                    }
                });
            }
            _ => {
                for indent_guide in self.indent_guides.as_ref() {
                    let fill_color = if indent_guide.is_active {
                        self.colors.active
                    } else {
                        self.colors.default
                    };

                    ctx.window.paint_quad(fill(indent_guide.bounds, fill_color));
                }
            }
        }

        PaintFrame {
            handled: true,
            skip_children: true,
            ..Default::default()
        }
    }
}

fn compute_indent_guides(
    indents: &[usize],
    offset: usize,
    includes_trailing_indent: bool,
) -> SmallVec<[IndentGuideLayout; 12]> {
    let mut indent_guides = SmallVec::<[IndentGuideLayout; 12]>::new();
    let mut indent_stack = SmallVec::<[IndentGuideLayout; 8]>::new();

    let mut min_depth = usize::MAX;
    for (row, &depth) in indents.iter().enumerate() {
        if includes_trailing_indent && row == indents.len() - 1 {
            continue;
        }

        let current_row = row + offset;
        let current_depth = indent_stack.len();
        if depth < min_depth {
            min_depth = depth;
        }

        match depth.cmp(&current_depth) {
            Ordering::Less => {
                for _ in 0..(current_depth - depth) {
                    if let Some(guide) = indent_stack.pop() {
                        indent_guides.push(guide);
                    }
                }
            }
            Ordering::Greater => {
                for new_depth in current_depth..depth {
                    indent_stack.push(IndentGuideLayout {
                        offset: Point::new(new_depth, current_row),
                        length: current_row,
                        continues_offscreen: false,
                    });
                }
            }
            _ => {}
        }

        for indent in indent_stack.iter_mut() {
            indent.length = current_row - indent.offset.y + 1;
        }
    }

    indent_guides.extend(indent_stack);

    for guide in indent_guides.iter_mut() {
        if includes_trailing_indent
            && guide.offset.y + guide.length == offset + indents.len().saturating_sub(1)
        {
            guide.continues_offscreen = indents
                .last()
                .map(|last_indent| guide.offset.x < *last_indent)
                .unwrap_or(false);
        }
    }

    indent_guides
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_indent_guides() {
        fn assert_compute_indent_guides(
            input: &[usize],
            offset: usize,
            includes_trailing_indent: bool,
            expected: Vec<IndentGuideLayout>,
        ) {
            use std::collections::HashSet;
            assert_eq!(
                compute_indent_guides(input, offset, includes_trailing_indent)
                    .into_vec()
                    .into_iter()
                    .collect::<HashSet<_>>(),
                expected.into_iter().collect::<HashSet<_>>(),
            );
        }

        assert_compute_indent_guides(
            &[0, 1, 2, 2, 1, 0],
            0,
            false,
            vec![
                IndentGuideLayout {
                    offset: Point::new(0, 1),
                    length: 4,
                    continues_offscreen: false,
                },
                IndentGuideLayout {
                    offset: Point::new(1, 2),
                    length: 2,
                    continues_offscreen: false,
                },
            ],
        );

        assert_compute_indent_guides(
            &[2, 2, 2, 1, 1],
            0,
            false,
            vec![
                IndentGuideLayout {
                    offset: Point::new(0, 0),
                    length: 5,
                    continues_offscreen: false,
                },
                IndentGuideLayout {
                    offset: Point::new(1, 0),
                    length: 3,
                    continues_offscreen: false,
                },
            ],
        );

        assert_compute_indent_guides(
            &[1, 2, 3, 2, 1],
            0,
            false,
            vec![
                IndentGuideLayout {
                    offset: Point::new(0, 0),
                    length: 5,
                    continues_offscreen: false,
                },
                IndentGuideLayout {
                    offset: Point::new(1, 1),
                    length: 3,
                    continues_offscreen: false,
                },
                IndentGuideLayout {
                    offset: Point::new(2, 2),
                    length: 1,
                    continues_offscreen: false,
                },
            ],
        );

        assert_compute_indent_guides(
            &[0, 1, 0],
            0,
            true,
            vec![IndentGuideLayout {
                offset: Point::new(0, 1),
                length: 1,
                continues_offscreen: false,
            }],
        );

        assert_compute_indent_guides(
            &[0, 1, 1],
            0,
            true,
            vec![IndentGuideLayout {
                offset: Point::new(0, 1),
                length: 1,
                continues_offscreen: true,
            }],
        );
        assert_compute_indent_guides(
            &[0, 1, 2],
            0,
            true,
            vec![IndentGuideLayout {
                offset: Point::new(0, 1),
                length: 1,
                continues_offscreen: true,
            }],
        );
    }
}
