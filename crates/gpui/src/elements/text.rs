use super::div::{
    register_tooltip_mouse_handlers, set_tooltip_on_window, MouseDownListener, MouseMoveListener,
    MouseUpListener,
};
use crate::{
    ActiveTooltip, AnyView, App, AvailableSpace, Bounds, DispatchPhase, Element, ElementId,
    ElementImpl, GlobalElementId, HighlightStyle, Hitbox, HitboxBehavior,
    InspectorElementId, IntoElement, LayoutId, MouseMoveEvent, Pixels, Point, SharedString, Size,
    TextOverflow, TextRun, TextStyle, TruncateFrom, UpdateResult, WhiteSpace, Window,
    WrappedLine, WrappedLineLayout,
};
use anyhow::Context as _;
use collections::FxHasher;
use itertools::Itertools;
use smallvec::SmallVec;
use std::{
    any::TypeId,
    borrow::Cow,
    cell::{Cell, RefCell},
    hash::{Hash, Hasher},
    mem,
    ops::Range,
    rc::Rc,
    sync::Arc,
};
use util::ResultExt;

/// Extension trait for adding styling and interactivity to text elements.
///
/// This trait provides fluent methods for styling text and adding click handlers,
/// hover handlers, and tooltips. It can be used directly on string types:
///
/// ```ignore
/// // Styled text (highlights resolved at render time using inherited style)
/// div().child("Hello".with_highlights(vec![(0..5, highlight)]))
///
/// // Styled text with explicit base style
/// div().child("Hello".with_default_highlights(&text_style, vec![(0..5, highlight)]))
///
/// // Interactive text
/// div().child("Click me".on_click(vec![0..8], |_, window, cx| { ... }))
/// div().child("Hover text".on_hover(|ix, event, window, cx| { ... }))
/// div().child("Info".tooltip(|ix, window, cx| Some(tooltip_view)))
///
/// // Combined styling and interactivity
/// div().child(
///     "Click here"
///         .with_default_highlights(&window.text_style(), vec![(0..5, underline)])
///         .on_click(vec![0..5], |_, window, cx| { ... })
/// )
/// ```
pub trait TextElement: Sized {
    /// Apply highlight styles to specific ranges of the text.
    ///
    /// The highlights are resolved at render time using the inherited text style
    /// from parent elements.
    fn with_highlights(
        self,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText;

    /// Apply highlight styles with an explicit base text style.
    ///
    /// Use this when you need to override the inherited style, for example
    /// when rendering code blocks with a custom font.
    fn with_default_highlights(
        self,
        default_style: &TextStyle,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText;

    /// Set pre-computed text runs directly.
    ///
    /// This is a lower-level API for cases where runs are already computed,
    /// such as syntax highlighting from a language server.
    fn with_runs(self, runs: Vec<TextRun>) -> StyledText;

    /// Convert this text element into an InteractiveText with the given click handler.
    ///
    /// The handler receives the index of the clicked range when the user clicks
    /// within one of the specified ranges.
    fn on_click(
        self,
        ranges: Vec<Range<usize>>,
        listener: impl Fn(usize, &mut Window, &mut App) + 'static,
    ) -> InteractiveText;

    /// Convert this text element into an InteractiveText with the given hover handler.
    ///
    /// The handler receives the character index under the mouse, or None when
    /// the mouse leaves the text.
    fn on_hover(
        self,
        listener: impl Fn(Option<usize>, MouseMoveEvent, &mut Window, &mut App) + 'static,
    ) -> InteractiveText;

    /// Convert this text element into an InteractiveText with a tooltip builder.
    ///
    /// The builder receives the character index under the mouse and can return
    /// a tooltip view to display.
    fn tooltip(
        self,
        builder: impl Fn(usize, &mut Window, &mut App) -> Option<AnyView> + 'static,
    ) -> InteractiveText;
}

impl Element for &'static str {
    type RequestLayoutState = ();
    type PrepaintState = ();

    fn id(&self) -> Option<ElementId> {
        None
    }

    fn source_location(&self) -> Option<&'static core::panic::Location<'static>> {
        None
    }

    fn request_layout(
        &mut self,
        _global_id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _window: &mut Window,
        _cx: &mut App,
    ) -> (LayoutId, Self::RequestLayoutState) {
        unreachable!("&'static str uses retained node path")
    }

    fn prepaint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _text_layout: &mut Self::RequestLayoutState,
        _window: &mut Window,
        _cx: &mut App,
    ) {
        unreachable!("&'static str uses retained node path")
    }

    fn paint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _text_layout: &mut Self::RequestLayoutState,
        _: &mut (),
        _window: &mut Window,
        _cx: &mut App,
    ) {
        unreachable!("&'static str uses retained node path")
    }

    fn into_any(self) -> crate::AnyElement {
        crate::AnyElement::new(self)
    }

    fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>> {
        Some(Box::new(TextNode::new(SharedString::from(*self), None)))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<TextNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn crate::RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        if let Some(text_node) = node.as_any_mut().downcast_mut::<TextNode>() {
            let text_changed = text_node.text != *self;
            let runs_changed = text_node.runs.is_some();
            if text_changed || runs_changed {
                text_node.update_from(SharedString::from(*self), None);
                Some(UpdateResult::LAYOUT_CHANGED)
            } else {
                Some(UpdateResult::UNCHANGED)
            }
        } else {
            None
        }
    }
}

impl IntoElement for &'static str {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl TextElement for &'static str {
    fn with_highlights(
        self,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText {
        StyledText::new_internal(self).with_highlights(highlights)
    }

    fn with_default_highlights(
        self,
        default_style: &TextStyle,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText {
        StyledText::new_internal(self).with_default_highlights(default_style, highlights)
    }

    fn with_runs(self, runs: Vec<TextRun>) -> StyledText {
        StyledText::new_internal(self).with_runs(runs)
    }

    fn on_click(
        self,
        ranges: Vec<Range<usize>>,
        listener: impl Fn(usize, &mut Window, &mut App) + 'static,
    ) -> InteractiveText {
        InteractiveText::new_from_text(SharedString::from(self), None, None)
            .on_click(ranges, listener)
    }

    fn on_hover(
        self,
        listener: impl Fn(Option<usize>, MouseMoveEvent, &mut Window, &mut App) + 'static,
    ) -> InteractiveText {
        InteractiveText::new_from_text(SharedString::from(self), None, None).on_hover(listener)
    }

    fn tooltip(
        self,
        builder: impl Fn(usize, &mut Window, &mut App) -> Option<AnyView> + 'static,
    ) -> InteractiveText {
        InteractiveText::new_from_text(SharedString::from(self), None, None).tooltip(builder)
    }
}

impl IntoElement for String {
    type Element = SharedString;

    fn into_element(self) -> Self::Element {
        self.into()
    }
}

impl TextElement for String {
    fn with_highlights(
        self,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText {
        StyledText::new_internal(self).with_highlights(highlights)
    }

    fn with_default_highlights(
        self,
        default_style: &TextStyle,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText {
        StyledText::new_internal(self).with_default_highlights(default_style, highlights)
    }

    fn with_runs(self, runs: Vec<TextRun>) -> StyledText {
        StyledText::new_internal(self).with_runs(runs)
    }

    fn on_click(
        self,
        ranges: Vec<Range<usize>>,
        listener: impl Fn(usize, &mut Window, &mut App) + 'static,
    ) -> InteractiveText {
        InteractiveText::new_from_text(SharedString::from(self), None, None)
            .on_click(ranges, listener)
    }

    fn on_hover(
        self,
        listener: impl Fn(Option<usize>, MouseMoveEvent, &mut Window, &mut App) + 'static,
    ) -> InteractiveText {
        InteractiveText::new_from_text(SharedString::from(self), None, None).on_hover(listener)
    }

    fn tooltip(
        self,
        builder: impl Fn(usize, &mut Window, &mut App) -> Option<AnyView> + 'static,
    ) -> InteractiveText {
        InteractiveText::new_from_text(SharedString::from(self), None, None).tooltip(builder)
    }
}

impl Element for SharedString {
    type RequestLayoutState = ();
    type PrepaintState = ();

    fn id(&self) -> Option<ElementId> {
        None
    }

    fn source_location(&self) -> Option<&'static core::panic::Location<'static>> {
        None
    }

    fn request_layout(
        &mut self,
        _global_id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _window: &mut Window,
        _cx: &mut App,
    ) -> (LayoutId, Self::RequestLayoutState) {
        unreachable!("SharedString uses retained node path")
    }

    fn prepaint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _text_layout: &mut Self::RequestLayoutState,
        _window: &mut Window,
        _cx: &mut App,
    ) {
        unreachable!("SharedString uses retained node path")
    }

    fn paint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _text_layout: &mut Self::RequestLayoutState,
        _: &mut Self::PrepaintState,
        _window: &mut Window,
        _cx: &mut App,
    ) {
        unreachable!("SharedString uses retained node path")
    }

    fn into_any(self) -> crate::AnyElement {
        crate::AnyElement::new(self)
    }

    fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>> {
        Some(Box::new(TextNode::new(self.clone(), None)))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<TextNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn crate::RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        if let Some(text_node) = node.as_any_mut().downcast_mut::<TextNode>() {
            let text_changed = text_node.text != *self;
            let runs_changed = text_node.runs.is_some();
            if text_changed || runs_changed {
                text_node.update_from(self.clone(), None);
                Some(UpdateResult::LAYOUT_CHANGED)
            } else {
                Some(UpdateResult::UNCHANGED)
            }
        } else {
            None
        }
    }
}

impl IntoElement for SharedString {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl TextElement for SharedString {
    fn with_highlights(
        self,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText {
        StyledText::new_internal(self).with_highlights(highlights)
    }

    fn with_default_highlights(
        self,
        default_style: &TextStyle,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText {
        StyledText::new_internal(self).with_default_highlights(default_style, highlights)
    }

    fn with_runs(self, runs: Vec<TextRun>) -> StyledText {
        StyledText::new_internal(self).with_runs(runs)
    }

    fn on_click(
        self,
        ranges: Vec<Range<usize>>,
        listener: impl Fn(usize, &mut Window, &mut App) + 'static,
    ) -> InteractiveText {
        InteractiveText::new_from_text(self, None, None).on_click(ranges, listener)
    }

    fn on_hover(
        self,
        listener: impl Fn(Option<usize>, MouseMoveEvent, &mut Window, &mut App) + 'static,
    ) -> InteractiveText {
        InteractiveText::new_from_text(self, None, None).on_hover(listener)
    }

    fn tooltip(
        self,
        builder: impl Fn(usize, &mut Window, &mut App) -> Option<AnyView> + 'static,
    ) -> InteractiveText {
        InteractiveText::new_from_text(self, None, None).tooltip(builder)
    }
}

/// Renders text with runs of different styles.
///
/// Callers are responsible for setting the correct style for each run.
/// For text with a uniform style, you can usually avoid calling this constructor
/// and just pass text directly.
#[derive(gpui_macros::Element)]
#[element(crate = crate)]
pub struct StyledText {
    text: SharedString,
    runs: Option<Vec<TextRun>>,
    delayed_highlights: Option<Vec<(Range<usize>, HighlightStyle)>>,
    layout: TextLayout,
}

impl StyledText {
    /// Internal constructor. Use the `TextElement` trait methods instead:
    /// - `"text".with_highlights(...)`
    /// - `"text".with_default_highlights(...)`
    /// - `"text".with_runs(...)`
    pub(crate) fn new_internal(text: impl Into<SharedString>) -> Self {
        StyledText {
            text: text.into(),
            runs: None,
            delayed_highlights: None,
            layout: TextLayout::default(),
        }
    }

    /// Get the layout for this element. This can be used to map indices to pixels and vice versa.
    pub fn layout(&self) -> &TextLayout {
        &self.layout
    }

    /// Set the styling attributes for the given text, as well as
    /// as any ranges of text that have had their style customized.
    pub fn with_default_highlights(
        mut self,
        default_style: &TextStyle,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> Self {
        debug_assert!(
            self.delayed_highlights.is_none(),
            "Can't use `with_default_highlights` and `with_highlights`"
        );
        let runs = Self::compute_runs(&self.text, default_style, highlights);
        self.with_runs(runs)
    }

    /// Set the styling attributes for the given text, as well as
    /// as any ranges of text that have had their style customized.
    pub fn with_highlights(
        mut self,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> Self {
        debug_assert!(
            self.runs.is_none(),
            "Can't use `with_highlights` and `with_default_highlights`"
        );
        self.delayed_highlights = Some(
            highlights
                .into_iter()
                .inspect(|(run, _)| {
                    debug_assert!(self.text.is_char_boundary(run.start));
                    debug_assert!(self.text.is_char_boundary(run.end));
                })
                .collect::<Vec<_>>(),
        );
        self
    }

    fn compute_runs(
        text: &str,
        default_style: &TextStyle,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> Vec<TextRun> {
        let mut runs = Vec::new();
        let mut ix = 0;
        for (range, highlight) in highlights {
            if ix < range.start {
                debug_assert!(text.is_char_boundary(range.start));
                runs.push(default_style.clone().to_run(range.start - ix));
            }
            debug_assert!(text.is_char_boundary(range.end));
            runs.push(
                default_style
                    .clone()
                    .highlight(highlight)
                    .to_run(range.len()),
            );
            ix = range.end;
        }
        if ix < text.len() {
            runs.push(default_style.to_run(text.len() - ix));
        }
        runs
    }

    /// Set the text runs for this piece of text.
    pub fn with_runs(mut self, runs: Vec<TextRun>) -> Self {
        let mut text = &**self.text;
        for run in &runs {
            text = text.get(run.len..).expect("invalid text run");
        }
        assert!(text.is_empty(), "invalid text run");
        self.runs = Some(runs);
        self
    }
}

impl ElementImpl for StyledText {
    fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>> {
        // Create TextNode with the external layout handle for API compatibility
        let runs = self.runs.take();
        Some(Box::new(TextNode::with_external_layout(
            self.text.clone(),
            runs,
            self.layout.clone(),
        )))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<TextNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn crate::RenderNode,
        window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let text_node = node.as_any_mut().downcast_mut::<TextNode>()?;

        // Resolve delayed_highlights to runs if needed
        let runs = self.runs.take().or_else(|| {
            self.delayed_highlights.take().map(|delayed_highlights| {
                Self::compute_runs(&self.text, &window.text_style(), delayed_highlights)
            })
        });

        let text_changed = text_node.text != self.text;
        let runs_changed = runs.is_some() || text_node.runs.is_some();

        if text_changed || runs_changed {
            text_node.update_from(self.text.clone(), runs);
            Some(UpdateResult::LAYOUT_CHANGED)
        } else {
            Some(UpdateResult::UNCHANGED)
        }
    }
}

impl IntoElement for StyledText {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl TextElement for StyledText {
    fn with_highlights(
        self,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText {
        StyledText::with_highlights(self, highlights)
    }

    fn with_default_highlights(
        self,
        default_style: &TextStyle,
        highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText {
        StyledText::with_default_highlights(self, default_style, highlights)
    }

    fn with_runs(self, runs: Vec<TextRun>) -> StyledText {
        StyledText::with_runs(self, runs)
    }

    fn on_click(
        mut self,
        ranges: Vec<Range<usize>>,
        listener: impl Fn(usize, &mut Window, &mut App) + 'static,
    ) -> InteractiveText {
        let runs = self.runs.take();
        InteractiveText::new_from_text(self.text, runs, Some(self.layout)).on_click(ranges, listener)
    }

    fn on_hover(
        mut self,
        listener: impl Fn(Option<usize>, MouseMoveEvent, &mut Window, &mut App) + 'static,
    ) -> InteractiveText {
        let runs = self.runs.take();
        InteractiveText::new_from_text(self.text, runs, Some(self.layout)).on_hover(listener)
    }

    fn tooltip(
        mut self,
        builder: impl Fn(usize, &mut Window, &mut App) -> Option<AnyView> + 'static,
    ) -> InteractiveText {
        let runs = self.runs.take();
        InteractiveText::new_from_text(self.text, runs, Some(self.layout)).tooltip(builder)
    }
}

/// The Layout for TextElement. This can be used to map indices to pixels and vice versa.
#[derive(Default, Clone)]
pub struct TextLayout(Rc<RefCell<Option<TextLayoutInner>>>);

#[allow(dead_code)]
struct TextLayoutInner {
    len: usize,
    lines: SmallVec<[WrappedLine; 1]>,
    line_height: Pixels,
    wrap_width: Option<Pixels>,
    size: Option<Size<Pixels>>,
    bounds: Option<Bounds<Pixels>>,
}

#[allow(dead_code)]
impl TextLayout {
    fn layout(
        &self,
        text: SharedString,
        runs: Option<Vec<TextRun>>,
        window: &mut Window,
        _: &mut App,
    ) -> LayoutId {
        let text_style = window.text_style();
        let font_size = text_style.font_size.to_pixels(window.rem_size());
        let line_height = text_style
            .line_height
            .to_pixels(font_size.into(), window.rem_size());

        let runs = if let Some(runs) = runs {
            runs
        } else {
            vec![text_style.to_run(text.len())]
        };

        let content_hash = {
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            text.hash(&mut hasher);
            font_size.0.to_bits().hash(&mut hasher);
            line_height.0.to_bits().hash(&mut hasher);
            hasher.finish()
        };

        window.request_measured_layout_cached(Default::default(), content_hash, {
            let element_state = self.clone();

            move |known_dimensions, available_space, window, cx| {
                let wrap_width = if text_style.white_space == WhiteSpace::Normal {
                    known_dimensions.width.or(match available_space.width {
                        crate::AvailableSpace::Definite(x) => Some(x),
                        _ => None,
                    })
                } else {
                    None
                };

                let (truncate_width, truncation_affix, truncate_from) =
                    if let Some(text_overflow) = text_style.text_overflow.clone() {
                        let width = known_dimensions.width.or(match available_space.width {
                            crate::AvailableSpace::Definite(x) => match text_style.line_clamp {
                                Some(max_lines) => Some(x * max_lines),
                                None => Some(x),
                            },
                            _ => None,
                        });

                        match text_overflow {
                            TextOverflow::Truncate(s) => (width, s, TruncateFrom::End),
                            TextOverflow::TruncateStart(s) => (width, s, TruncateFrom::Start),
                        }
                    } else {
                        (None, "".into(), TruncateFrom::End)
                    };

                // Only use cached layout if:
                // 1. We have a cached size
                // 2. wrap_width matches (or both are None)
                // 3. truncate_width is None (if truncate_width is Some, we need to re-layout
                //    because the previous layout may have been computed without truncation)
                if let Some(text_layout) = element_state.0.borrow().as_ref()
                    && let Some(size) = text_layout.size
                    && (wrap_width.is_none() || wrap_width == text_layout.wrap_width)
                    && truncate_width.is_none()
                {
                    return size;
                }

                let mut line_wrapper = cx.text_system().line_wrapper(text_style.font(), font_size);
                let (text, runs) = if let Some(truncate_width) = truncate_width {
                    line_wrapper.truncate_line(
                        text.clone(),
                        truncate_width,
                        &truncation_affix,
                        &runs,
                        truncate_from,
                    )
                } else {
                    (text.clone(), Cow::Borrowed(&*runs))
                };
                let len = text.len();

                let Some(lines) = window
                    .text_system()
                    .shape_text(
                        text,
                        font_size,
                        &runs,
                        wrap_width,            // Wrap if we know the width.
                        text_style.line_clamp, // Limit the number of lines if line_clamp is set.
                    )
                    .log_err()
                else {
                    element_state.0.borrow_mut().replace(TextLayoutInner {
                        lines: Default::default(),
                        len: 0,
                        line_height,
                        wrap_width,
                        size: Some(Size::default()),
                        bounds: None,
                    });
                    return Size::default();
                };

                let mut size: Size<Pixels> = Size::default();
                for line in &lines {
                    let line_size = line.size(line_height);
                    size.height += line_size.height;
                    size.width = size.width.max(line_size.width).ceil();
                }

                element_state.0.borrow_mut().replace(TextLayoutInner {
                    lines,
                    len,
                    line_height,
                    wrap_width,
                    size: Some(size),
                    bounds: None,
                });

                size
            }
        })
    }

    fn prepaint(&self, bounds: Bounds<Pixels>, text: &str) {
        let mut element_state = self.0.borrow_mut();
        let element_state = element_state
            .as_mut()
            .with_context(|| format!("measurement has not been performed on {text}"))
            .unwrap();
        element_state.bounds = Some(bounds);
    }

    fn paint(&self, text: &str, window: &mut Window, cx: &mut App) {
        let element_state = self.0.borrow();
        let element_state = element_state
            .as_ref()
            .with_context(|| format!("measurement has not been performed on {text}"))
            .unwrap();
        let bounds = element_state
            .bounds
            .with_context(|| format!("prepaint has not been performed on {text}"))
            .unwrap();

        let line_height = element_state.line_height;
        let mut line_origin = bounds.origin;
        let text_style = window.text_style();
        for line in &element_state.lines {
            line.paint_background(
                line_origin,
                line_height,
                text_style.text_align,
                Some(bounds),
                window,
                cx,
            )
            .log_err();
            line.paint(
                line_origin,
                line_height,
                text_style.text_align,
                Some(bounds),
                window,
                cx,
            )
            .log_err();
            line_origin.y += line.size(line_height).height;
        }
    }

    /// Get the byte index into the input of the pixel position.
    pub fn index_for_position(&self, mut position: Point<Pixels>) -> Result<usize, usize> {
        let element_state = self.0.borrow();
        let element_state = element_state
            .as_ref()
            .expect("measurement has not been performed");
        let bounds = element_state
            .bounds
            .expect("prepaint has not been performed");

        if position.y < bounds.top() {
            return Err(0);
        }

        let line_height = element_state.line_height;
        let mut line_origin = bounds.origin;
        let mut line_start_ix = 0;
        for line in &element_state.lines {
            let line_bottom = line_origin.y + line.size(line_height).height;
            if position.y > line_bottom {
                line_origin.y = line_bottom;
                line_start_ix += line.len() + 1;
            } else {
                let position_within_line = position - line_origin;
                match line.index_for_position(position_within_line, line_height) {
                    Ok(index_within_line) => return Ok(line_start_ix + index_within_line),
                    Err(index_within_line) => return Err(line_start_ix + index_within_line),
                }
            }
        }

        Err(line_start_ix.saturating_sub(1))
    }

    /// Get the pixel position for the given byte index.
    pub fn position_for_index(&self, index: usize) -> Option<Point<Pixels>> {
        let element_state = self.0.borrow();
        let element_state = element_state
            .as_ref()
            .expect("measurement has not been performed");
        let bounds = element_state
            .bounds
            .expect("prepaint has not been performed");
        let line_height = element_state.line_height;

        let mut line_origin = bounds.origin;
        let mut line_start_ix = 0;

        for line in &element_state.lines {
            let line_end_ix = line_start_ix + line.len();
            if index < line_start_ix {
                break;
            } else if index > line_end_ix {
                line_origin.y += line.size(line_height).height;
                line_start_ix = line_end_ix + 1;
                continue;
            } else {
                let ix_within_line = index - line_start_ix;
                return Some(line_origin + line.position_for_index(ix_within_line, line_height)?);
            }
        }

        None
    }

    /// Retrieve the layout for the line containing the given byte index.
    pub fn line_layout_for_index(&self, index: usize) -> Option<Arc<WrappedLineLayout>> {
        let element_state = self.0.borrow();
        let element_state = element_state
            .as_ref()
            .expect("measurement has not been performed");
        let bounds = element_state
            .bounds
            .expect("prepaint has not been performed");
        let line_height = element_state.line_height;

        let mut line_origin = bounds.origin;
        let mut line_start_ix = 0;

        for line in &element_state.lines {
            let line_end_ix = line_start_ix + line.len();
            if index < line_start_ix {
                break;
            } else if index > line_end_ix {
                line_origin.y += line.size(line_height).height;
                line_start_ix = line_end_ix + 1;
                continue;
            } else {
                return Some(line.layout.clone());
            }
        }

        None
    }

    /// The bounds of this layout.
    pub fn bounds(&self) -> Bounds<Pixels> {
        self.0.borrow().as_ref().unwrap().bounds.unwrap()
    }

    /// The line height for this layout.
    pub fn line_height(&self) -> Pixels {
        self.0.borrow().as_ref().unwrap().line_height
    }

    /// The UTF-8 length of the underlying text.
    pub fn len(&self) -> usize {
        self.0.borrow().as_ref().unwrap().len
    }

    /// The text for this layout.
    pub fn text(&self) -> String {
        self.0
            .borrow()
            .as_ref()
            .unwrap()
            .lines
            .iter()
            .map(|s| &s.text)
            .join("\n")
    }

    /// The text for this layout (with soft-wraps as newlines)
    pub fn wrapped_text(&self) -> String {
        let mut accumulator = String::new();

        for wrapped in self.0.borrow().as_ref().unwrap().lines.iter() {
            let mut seen = 0;
            for boundary in wrapped.layout.wrap_boundaries.iter() {
                let index = wrapped.layout.unwrapped_layout.runs[boundary.run_ix].glyphs
                    [boundary.glyph_ix]
                    .index;

                accumulator.push_str(&wrapped.text[seen..index]);
                accumulator.push('\n');
                seen = index;
            }
            accumulator.push_str(&wrapped.text[seen..]);
            accumulator.push('\n');
        }
        // Remove trailing newline
        accumulator.pop();
        accumulator
    }
}

/// A text element that can be interacted with.
///
/// Created via the fluent API on string types:
/// ```ignore
/// "Click me".on_click(vec![0..8], |ix, window, cx| { ... })
/// "Styled text".with_default_highlights(&style, highlights).on_click(...)
/// ```
#[derive(gpui_macros::Element)]
#[element(crate = crate)]
pub struct InteractiveText {
    text: SharedString,
    runs: Option<Vec<TextRun>>,
    external_layout: Option<TextLayout>,
    click_listener:
        Option<Rc<dyn Fn(&[Range<usize>], InteractiveTextClickEvent, &mut Window, &mut App)>>,
    hover_listener: Option<Rc<dyn Fn(Option<usize>, MouseMoveEvent, &mut Window, &mut App)>>,
    tooltip_builder: Option<Rc<dyn Fn(usize, &mut Window, &mut App) -> Option<AnyView>>>,
    clickable_ranges: Vec<Range<usize>>,
}

pub(crate) struct InteractiveTextClickEvent {
    mouse_down_index: usize,
    mouse_up_index: usize,
}

/// InteractiveText adds mouse interactions to text.
impl InteractiveText {
    /// Internal constructor used by the TextElement trait.
    /// Use the fluent API instead: `"text".on_click(ranges, handler)`
    pub(crate) fn new_from_text(
        text: SharedString,
        runs: Option<Vec<TextRun>>,
        external_layout: Option<TextLayout>,
    ) -> Self {
        Self {
            text,
            runs,
            external_layout,
            click_listener: None,
            hover_listener: None,
            tooltip_builder: None,
            clickable_ranges: Vec::new(),
        }
    }

    /// on_click is called when the user clicks on one of the given ranges, passing the index of
    /// the clicked range.
    pub fn on_click(
        mut self,
        ranges: Vec<Range<usize>>,
        listener: impl Fn(usize, &mut Window, &mut App) + 'static,
    ) -> Self {
        self.click_listener = Some(Rc::new(move |ranges, event, window, cx| {
            for (range_ix, range) in ranges.iter().enumerate() {
                if range.contains(&event.mouse_down_index) && range.contains(&event.mouse_up_index)
                {
                    listener(range_ix, window, cx);
                }
            }
        }));
        self.clickable_ranges = ranges;
        self
    }

    /// on_hover is called when the mouse moves over a character within the text, passing the
    /// index of the hovered character, or None if the mouse leaves the text.
    pub fn on_hover(
        mut self,
        listener: impl Fn(Option<usize>, MouseMoveEvent, &mut Window, &mut App) + 'static,
    ) -> Self {
        self.hover_listener = Some(Rc::new(listener));
        self
    }

    /// tooltip lets you specify a tooltip for a given character index in the string.
    pub fn tooltip(
        mut self,
        builder: impl Fn(usize, &mut Window, &mut App) -> Option<AnyView> + 'static,
    ) -> Self {
        self.tooltip_builder = Some(Rc::new(builder));
        self
    }
}

impl ElementImpl for InteractiveText {
    fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>> {
        let mut node = if let Some(external_layout) = self.external_layout.take() {
            TextNode::with_external_layout(self.text.clone(), self.runs.take(), external_layout)
        } else {
            TextNode::new(self.text.clone(), self.runs.take())
        };

        node.enable_interactivity();

        if let Some(click_listener) = self.click_listener.take() {
            let clickable_ranges = mem::take(&mut self.clickable_ranges);
            node.set_click_listener(click_listener, clickable_ranges);
        }
        if let Some(hover_listener) = self.hover_listener.take() {
            node.set_hover_listener(hover_listener);
        }
        if let Some(tooltip_builder) = self.tooltip_builder.take() {
            node.set_tooltip_builder(tooltip_builder);
        }

        Some(Box::new(node))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<TextNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn crate::RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let text_node = node.as_any_mut().downcast_mut::<TextNode>()?;

        let text_changed = text_node.text != self.text;
        let runs_changed = self.runs.is_some() || text_node.runs.is_some();

        if text_changed || runs_changed {
            text_node.update_from(self.text.clone(), self.runs.take());
        }

        text_node.enable_interactivity();

        if let Some(click_listener) = self.click_listener.take() {
            let clickable_ranges = mem::take(&mut self.clickable_ranges);
            text_node.set_click_listener(click_listener, clickable_ranges);
        }
        if let Some(hover_listener) = self.hover_listener.take() {
            text_node.set_hover_listener(hover_listener);
        }
        if let Some(tooltip_builder) = self.tooltip_builder.take() {
            text_node.set_tooltip_builder(tooltip_builder);
        }

        if text_changed || runs_changed {
            Some(UpdateResult::LAYOUT_CHANGED)
        } else {
            Some(UpdateResult::PAINT_ONLY)
        }
    }
}

impl IntoElement for InteractiveText {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl TextElement for InteractiveText {
    fn with_highlights(
        self,
        _highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText {
        // InteractiveText already has styling configured; converting back to StyledText
        // would lose the interactive handlers. This is a no-op that returns plain StyledText.
        // Users should apply highlights before adding interactivity.
        StyledText::new_internal(self.text)
    }

    fn with_default_highlights(
        self,
        _default_style: &TextStyle,
        _highlights: impl IntoIterator<Item = (Range<usize>, HighlightStyle)>,
    ) -> StyledText {
        // Same as with_highlights - users should apply styling before interactivity.
        StyledText::new_internal(self.text)
    }

    fn with_runs(self, runs: Vec<TextRun>) -> StyledText {
        StyledText::new_internal(self.text).with_runs(runs)
    }

    fn on_click(
        self,
        ranges: Vec<Range<usize>>,
        listener: impl Fn(usize, &mut Window, &mut App) + 'static,
    ) -> InteractiveText {
        InteractiveText::on_click(self, ranges, listener)
    }

    fn on_hover(
        self,
        listener: impl Fn(Option<usize>, MouseMoveEvent, &mut Window, &mut App) + 'static,
    ) -> InteractiveText {
        InteractiveText::on_hover(self, listener)
    }

    fn tooltip(
        self,
        builder: impl Fn(usize, &mut Window, &mut App) -> Option<AnyView> + 'static,
    ) -> InteractiveText {
        InteractiveText::tooltip(self, builder)
    }
}

/// Retained render node for text elements.
///
/// This node owns all text-specific data including the text content,
/// optional style runs, and cached layout (shaped lines).
pub(crate) struct TextNode {
    /// The text content.
    text: SharedString,
    /// Optional styled runs.
    runs: Option<Vec<TextRun>>,
    /// Resolved text style captured during layout_begin.
    /// This is the inherited text style from parent elements.
    resolved_text_style: Option<TextStyle>,
    /// Cached intrinsic sizing (min/max content) keyed by `SizingInput`.
    intrinsic: Option<(crate::SizingInput, crate::IntrinsicSize)>,
    /// Cached layout data (populated during measure).
    layout: Option<TextNodeLayout>,
    /// Bounds set during prepaint.
    bounds: Option<Bounds<Pixels>>,
    /// Optional external layout handle for StyledText compatibility.
    /// When present, layout data is synced to this handle so external
    /// code can access it (e.g., for index-to-pixel mapping).
    external_layout: Option<TextLayout>,
    /// Optional interactive state (only allocated when text has interactive features).
    interactive: Option<Box<TextNodeInteractive>>,
}

/// Cached layout data for TextNode.
struct TextNodeLayout {
    input: crate::SizingInput,
    lines: SmallVec<[WrappedLine; 1]>,
    line_height: Pixels,
    wrap_width: Option<Pixels>,
    truncate_width: Option<Pixels>,
    size: Size<Pixels>,
    text_style: TextStyle,
}

/// Interactive state for TextNode.
/// This is only allocated when the text has interactive features (click, hover, tooltip).
struct TextNodeInteractive {
    /// The character index where the mouse button was pressed.
    mouse_down_index: Rc<Cell<Option<usize>>>,
    /// The character index currently being hovered.
    hovered_index: Rc<Cell<Option<usize>>>,
    /// The currently active tooltip.
    active_tooltip: Rc<RefCell<Option<ActiveTooltip>>>,
    /// Hitbox for the entire text region.
    hitbox: Option<Hitbox>,
    /// Click listener callback (wrapped in Rc for use in event handlers).
    click_listener: Option<
        Rc<dyn Fn(&[Range<usize>], InteractiveTextClickEvent, &mut Window, &mut App)>,
    >,
    /// Hover listener callback (wrapped in Rc for use in event handlers).
    hover_listener: Option<Rc<dyn Fn(Option<usize>, MouseMoveEvent, &mut Window, &mut App)>>,
    /// Tooltip builder function.
    tooltip_builder: Option<Rc<dyn Fn(usize, &mut Window, &mut App) -> Option<AnyView>>>,
    /// Clickable ranges for the click listener.
    clickable_ranges: Vec<Range<usize>>,
}

impl Default for TextNodeInteractive {
    fn default() -> Self {
        Self {
            mouse_down_index: Rc::new(Cell::new(None)),
            hovered_index: Rc::new(Cell::new(None)),
            active_tooltip: Rc::new(RefCell::new(None)),
            hitbox: None,
            click_listener: None,
            hover_listener: None,
            tooltip_builder: None,
            clickable_ranges: Vec::new(),
        }
    }
}

/// Compute the character index for a given position within the text bounds.
/// This is a standalone function for use in event handler closures.
fn index_for_position_in_lines(
    position: Point<Pixels>,
    bounds: Bounds<Pixels>,
    lines: &[WrappedLine],
    line_height: Pixels,
) -> Option<usize> {
    if position.y < bounds.top() {
        return Some(0);
    }

    let mut line_origin = bounds.origin;
    let mut line_start_ix = 0;
    for line in lines {
        let line_bottom = line_origin.y + line.size(line_height).height;
        if position.y > line_bottom {
            line_origin.y = line_bottom;
            line_start_ix += line.len() + 1;
        } else {
            let position_within_line = position - line_origin;
            return match line.index_for_position(position_within_line, line_height) {
                Ok(index_within_line) => Some(line_start_ix + index_within_line),
                Err(index_within_line) => Some(line_start_ix + index_within_line),
            };
        }
    }

    Some(line_start_ix.saturating_sub(1))
}

impl TextNode {
    /// Create a new TextNode with the given text.
    pub fn new(text: SharedString, runs: Option<Vec<TextRun>>) -> Self {
        Self {
            text,
            runs,
            resolved_text_style: None,
            intrinsic: None,
            layout: None,
            bounds: None,
            external_layout: None,
            interactive: None,
        }
    }

    /// Create a new TextNode with an external layout handle for StyledText compatibility.
    pub fn with_external_layout(
        text: SharedString,
        runs: Option<Vec<TextRun>>,
        external_layout: TextLayout,
    ) -> Self {
        Self {
            text,
            runs,
            resolved_text_style: None,
            intrinsic: None,
            layout: None,
            bounds: None,
            external_layout: Some(external_layout),
            interactive: None,
        }
    }

    /// Enable interactivity for this text node.
    /// This allocates the interactive state if not already present.
    pub fn enable_interactivity(&mut self) {
        if self.interactive.is_none() {
            self.interactive = Some(Box::default());
        }
    }

    /// Set the click listener for this text node.
    /// This also enables interactivity if not already enabled.
    pub fn set_click_listener(
        &mut self,
        listener: Rc<dyn Fn(&[Range<usize>], InteractiveTextClickEvent, &mut Window, &mut App)>,
        clickable_ranges: Vec<Range<usize>>,
    ) {
        self.enable_interactivity();
        if let Some(interactive) = self.interactive.as_mut() {
            interactive.click_listener = Some(listener);
            interactive.clickable_ranges = clickable_ranges;
        }
    }

    /// Set the hover listener for this text node.
    /// This also enables interactivity if not already enabled.
    pub fn set_hover_listener(
        &mut self,
        listener: Rc<dyn Fn(Option<usize>, MouseMoveEvent, &mut Window, &mut App)>,
    ) {
        self.enable_interactivity();
        if let Some(interactive) = self.interactive.as_mut() {
            interactive.hover_listener = Some(listener);
        }
    }

    /// Set the tooltip builder for this text node.
    /// This also enables interactivity if not already enabled.
    pub fn set_tooltip_builder(
        &mut self,
        builder: Rc<dyn Fn(usize, &mut Window, &mut App) -> Option<AnyView>>,
    ) {
        self.enable_interactivity();
        if let Some(interactive) = self.interactive.as_mut() {
            interactive.tooltip_builder = Some(builder);
        }
    }

    /// Update this node from a descriptor.
    pub fn update_from(&mut self, text: SharedString, runs: Option<Vec<TextRun>>) {
        // Clear cached layout since text content may have changed
        self.layout = None;
        self.intrinsic = None;
        self.text = text;
        self.runs = runs;
    }

    /// Sync internal layout data to the external TextLayout handle, if present.
    fn sync_external_layout(&self) {
        if let Some(ref external) = self.external_layout {
            if let Some(ref layout) = self.layout {
                external.0.borrow_mut().replace(TextLayoutInner {
                    len: self.text.len(),
                    lines: layout.lines.clone(),
                    line_height: layout.line_height,
                    wrap_width: layout.wrap_width,
                    size: Some(layout.size),
                    bounds: self.bounds,
                });
            }
        }
    }

    fn sizing_input(
        &self,
        text_style: &TextStyle,
        font_size: Pixels,
        line_height: Pixels,
        runs: &[TextRun],
    ) -> crate::SizingInput {
        let mut content_hasher = FxHasher::default();
        self.text.hash(&mut content_hasher);
        runs.len().hash(&mut content_hasher);
        for run in runs {
            run.len.hash(&mut content_hasher);
            run.font.hash(&mut content_hasher);
        }
        let content_hash = content_hasher.finish();

        let mut style_hasher = FxHasher::default();
        text_style.font().hash(&mut style_hasher);
        font_size.0.to_bits().hash(&mut style_hasher);
        line_height.0.to_bits().hash(&mut style_hasher);
        text_style.line_clamp.hash(&mut style_hasher);
        match text_style.white_space {
            WhiteSpace::Normal => 0u8.hash(&mut style_hasher),
            WhiteSpace::Nowrap => 1u8.hash(&mut style_hasher),
        }
        match &text_style.text_overflow {
            None => 0u8.hash(&mut style_hasher),
            Some(TextOverflow::Truncate(affix)) => {
                1u8.hash(&mut style_hasher);
                affix.hash(&mut style_hasher);
            }
            Some(TextOverflow::TruncateStart(affix)) => {
                2u8.hash(&mut style_hasher);
                affix.hash(&mut style_hasher);
            }
        }
        match text_style.text_align {
            crate::TextAlign::Left => 0u8.hash(&mut style_hasher),
            crate::TextAlign::Center => 1u8.hash(&mut style_hasher),
            crate::TextAlign::Right => 2u8.hash(&mut style_hasher),
        }
        let style_hash = style_hasher.finish();

        crate::SizingInput::new(content_hash, style_hash)
    }

    fn shape_and_store_layout(
        &mut self,
        input: crate::SizingInput,
        text_style: TextStyle,
        wrap_width: Option<Pixels>,
        truncate_width: Option<Pixels>,
        window: &mut Window,
        cx: &mut App,
    ) -> Size<Pixels> {
        let font_size = text_style.font_size.to_pixels(window.rem_size());
        let line_height = text_style
            .line_height
            .to_pixels(font_size.into(), window.rem_size());

        let runs = self
            .runs
            .clone()
            .unwrap_or_else(|| vec![text_style.to_run(self.text.len())]);

        let (truncate_width, truncation_affix, truncate_from) = if let Some(text_overflow) =
            text_style.text_overflow.clone()
            && truncate_width.is_some()
        {
            match text_overflow {
                TextOverflow::Truncate(s) => (truncate_width, s, TruncateFrom::End),
                TextOverflow::TruncateStart(s) => (truncate_width, s, TruncateFrom::Start),
            }
        } else {
            (None, "".into(), TruncateFrom::End)
        };

        // Only use cached layout if:
        // - input matches (content/style),
        // - wrap_width matches, and
        // - truncation state matches.
        if let Some(layout) = &self.layout
            && layout.input == input
            && layout.wrap_width == wrap_width
            && layout.truncate_width == truncate_width
        {
            return layout.size;
        }

        let mut line_wrapper = cx.text_system().line_wrapper(text_style.font(), font_size);
        let (text, runs) = if let Some(truncate_width) = truncate_width {
            line_wrapper.truncate_line(
                self.text.clone(),
                truncate_width,
                &truncation_affix,
                &runs,
                truncate_from,
            )
        } else {
            (self.text.clone(), Cow::Borrowed(&*runs))
        };

        let Some(lines) = window
            .text_system()
            .shape_text(text, font_size, &runs, wrap_width, text_style.line_clamp)
            .log_err()
        else {
            let size = Size::default();
            self.layout = Some(TextNodeLayout {
                input,
                lines: Default::default(),
                line_height,
                wrap_width,
                truncate_width,
                size,
                text_style,
            });
            return size;
        };

        let mut size: Size<Pixels> = Size::default();
        for line in &lines {
            let line_size = line.size(line_height);
            size.height += line_size.height;
            size.width = size.width.max(line_size.width).ceil();
        }

        self.layout = Some(TextNodeLayout {
            input,
            lines,
            line_height,
            wrap_width,
            truncate_width,
            size,
            text_style,
        });

        // Sync to external layout handle if present
        self.sync_external_layout();

        size
    }
}

impl crate::RenderNode for TextNode {
    fn needs_child_bounds(&self) -> bool {
        false
    }

    fn layout_begin(&mut self, ctx: &mut crate::LayoutCtx) -> crate::LayoutFrame {
        // Capture the inherited text style from the window stack.
        // This is set up by parent Divs during their layout_begin.
        self.resolved_text_style = Some(ctx.window.text_style());
        crate::LayoutFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn compute_intrinsic_size(&mut self, ctx: &mut crate::SizingCtx) -> crate::IntrinsicSizeResult {
        let text_style = ctx.window.text_style();
        self.resolved_text_style = Some(text_style.clone());

        let font_size = text_style.font_size.to_pixels(ctx.window.rem_size());
        let line_height = text_style
            .line_height
            .to_pixels(font_size.into(), ctx.window.rem_size());

        let runs = self
            .runs
            .clone()
            .unwrap_or_else(|| vec![text_style.to_run(self.text.len())]);

        let input = self.sizing_input(&text_style, font_size, line_height, &runs);
        if let Some((cached_input, cached_size)) = &self.intrinsic
            && *cached_input == input
        {
            return crate::IntrinsicSizeResult {
                size: cached_size.clone(),
                input,
            };
        }

        // Unwrapped (max-content) size; we currently treat min-content as equivalent to preserve
        // existing behavior. Height-for-width is handled in `resolve_size_query`.
        let size =
            self.shape_and_store_layout(input.clone(), text_style, None, None, ctx.window, ctx.cx);

        let intrinsic = crate::IntrinsicSize {
            min_content: size,
            max_content: size,
        };
        self.intrinsic = Some((input.clone(), intrinsic.clone()));

        crate::IntrinsicSizeResult {
            size: intrinsic,
            input,
        }
    }

    fn resolve_size_query(
        &mut self,
        query: crate::SizeQuery,
        cached: &crate::IntrinsicSize,
        ctx: &mut crate::SizingCtx,
    ) -> Size<Pixels> {
        let Some(mut text_style) = self.resolved_text_style.clone().or_else(|| {
            // Best-effort fallback: this can happen when layout is invoked without a preceding
            // intrinsic sizing pass (e.g. legacy in-frame layouts).
            Some(ctx.window.text_style())
        }) else {
            return cached.max_content;
        };

        // For queries that provide a definite width, compute wrapped height for that width.
        match query {
            crate::SizeQuery::MinContent => cached.min_content,
            crate::SizeQuery::MaxContent => cached.max_content,
            crate::SizeQuery::ForHeight(height) => Size {
                width: cached.max_content.width,
                height,
            },
            crate::SizeQuery::ForWidth(width) => {
                let wrap_width = if text_style.white_space == WhiteSpace::Normal {
                    Some(width)
                } else {
                    None
                };

                let truncate_width = if text_style.text_overflow.is_some() {
                    match text_style.line_clamp {
                        Some(max_lines) => Some(width * max_lines),
                        None => Some(width),
                    }
                } else {
                    None
                };

                let font_size = text_style.font_size.to_pixels(ctx.window.rem_size());
                let line_height = text_style
                    .line_height
                    .to_pixels(font_size.into(), ctx.window.rem_size());
                let runs = self
                    .runs
                    .clone()
                    .unwrap_or_else(|| vec![text_style.to_run(self.text.len())]);
                let input = self.sizing_input(&text_style, font_size, line_height, &runs);

                self.shape_and_store_layout(
                    input,
                    text_style,
                    wrap_width,
                    truncate_width,
                    ctx.window,
                    ctx.cx,
                )
            }
            crate::SizeQuery::Definite(size) => {
                let wrap_width = if text_style.white_space == WhiteSpace::Normal {
                    Some(size.width)
                } else {
                    None
                };

                let truncate_width = if text_style.text_overflow.is_some() {
                    match text_style.line_clamp {
                        Some(max_lines) => Some(size.width * max_lines),
                        None => Some(size.width),
                    }
                } else {
                    None
                };

                let font_size = text_style.font_size.to_pixels(ctx.window.rem_size());
                let line_height = text_style
                    .line_height
                    .to_pixels(font_size.into(), ctx.window.rem_size());
                let runs = self
                    .runs
                    .clone()
                    .unwrap_or_else(|| vec![text_style.to_run(self.text.len())]);
                let input = self.sizing_input(&text_style, font_size, line_height, &runs);

                let measured = self.shape_and_store_layout(
                    input,
                    text_style,
                    wrap_width,
                    truncate_width,
                    ctx.window,
                    ctx.cx,
                );

                Size {
                    width: measured.width.min(size.width),
                    height: measured.height.min(size.height),
                }
            }
        }
    }

    fn measure(
        &mut self,
        known: Size<Option<Pixels>>,
        available: Size<AvailableSpace>,
        window: &mut Window,
        cx: &mut App,
    ) -> Option<Size<Pixels>> {
        let text_style = self
            .resolved_text_style
            .clone()
            .unwrap_or_else(|| window.text_style());

        let wrap_width = if text_style.white_space == WhiteSpace::Normal {
            known.width.or(match available.width {
                AvailableSpace::Definite(x) => Some(x),
                _ => None,
            })
        } else {
            None
        };

        let truncate_width = if text_style.text_overflow.is_some() {
            known.width.or(match available.width {
                AvailableSpace::Definite(x) => match text_style.line_clamp {
                    Some(max_lines) => Some(x * max_lines),
                    None => Some(x),
                },
                _ => None,
            })
        } else {
            None
        };

        let font_size = text_style.font_size.to_pixels(window.rem_size());
        let line_height = text_style
            .line_height
            .to_pixels(font_size.into(), window.rem_size());
        let runs = self
            .runs
            .clone()
            .unwrap_or_else(|| vec![text_style.to_run(self.text.len())]);
        let input = self.sizing_input(&text_style, font_size, line_height, &runs);

        Some(self.shape_and_store_layout(input, text_style, wrap_width, truncate_width, window, cx))
    }

    fn prepaint_begin(&mut self, ctx: &mut crate::PrepaintCtx) -> crate::PrepaintFrame {
        self.bounds = Some(ctx.bounds);

        // Sync bounds to external layout handle if present
        self.sync_external_layout();

        // Register hitbox for interactive text
        if let Some(interactive) = self.interactive.as_mut() {
            interactive.hitbox = Some(ctx.window.insert_hitbox_with_fiber(
                ctx.bounds,
                HitboxBehavior::default(),
                ctx.fiber_id,
            ));
        }

        crate::PrepaintFrame {
            handled: true,
            skip_children: true,
            ..Default::default()
        }
    }

    fn prepaint_end(&mut self, _ctx: &mut crate::PrepaintCtx, _frame: crate::PrepaintFrame) {
        // Nothing to pop for text
    }

    fn paint_begin(&mut self, ctx: &mut crate::PaintCtx) -> crate::PaintFrame {
        // Use paint-time bounds directly. Text depends on up-to-date bounds for correct positioning
        // (e.g. after window resizes). Prepaint may be replayed, so relying solely on cached
        // prepaint bounds can lead to stale coordinates.
        let bounds = ctx.bounds;
        self.bounds = Some(bounds);

        // Paint the text
        if let Some(ref layout) = self.layout {
            let mut line_origin = bounds.origin;
            for line in &layout.lines {
                line.paint_background(
                    line_origin,
                    layout.line_height,
                    layout.text_style.text_align,
                    Some(bounds),
                    ctx.window,
                    ctx.cx,
                )
                .log_err();
                line.paint(
                    line_origin,
                    layout.line_height,
                    layout.text_style.text_align,
                    Some(bounds),
                    ctx.window,
                    ctx.cx,
                )
                .log_err();
                line_origin.y += line.size(layout.line_height).height;
            }
        }

        // Register interactive event handlers via fiber effects
        if let Some(interactive) = self.interactive.as_mut() {
            let Some(hitbox) = interactive.hitbox.clone() else {
                return crate::PaintFrame {
                    handled: true,
                    skip_children: true,
                    ..Default::default()
                };
            };

            // Get the layout lines and line_height for index_for_position calculations
            let layout_data = self.layout.as_ref().map(|l| (l.lines.clone(), l.line_height));

            // Register mouse event listeners via fiber effects for unified dispatch
            if let Some(effects) = ctx.window.register_fiber_effects(&ctx.fiber_id) {
                // Mouse down handler - record the index where mouse was pressed
                let mouse_down_listener: MouseDownListener = Rc::new({
                    let mouse_down_index = interactive.mouse_down_index.clone();
                    let layout_data = layout_data.clone();
                    move |event, phase, hitbox, window, _cx| {
                        if phase == DispatchPhase::Bubble
                            && hitbox.is_hovered(window)
                            && event.button == crate::MouseButton::Left
                        {
                            if let Some((ref lines, line_height)) = layout_data {
                                let index = index_for_position_in_lines(
                                    event.position,
                                    bounds,
                                    lines,
                                    line_height,
                                );
                                mouse_down_index.set(index);
                            }
                        }
                    }
                });
                effects.mouse_down_listeners.push(mouse_down_listener);

                // Mouse up handler - detect clicks
                if let Some(click_listener) = interactive.click_listener.clone() {
                    let mouse_up_listener: MouseUpListener = Rc::new({
                        let mouse_down_index = interactive.mouse_down_index.clone();
                        let clickable_ranges = interactive.clickable_ranges.clone();
                        let layout_data = layout_data.clone();
                        move |event, phase, hitbox, window, cx| {
                            if phase == DispatchPhase::Bubble
                                && event.button == crate::MouseButton::Left
                            {
                                if let Some(mouse_down_ix) = mouse_down_index.take() {
                                    if let Some((ref lines, line_height)) = layout_data
                                        && hitbox.is_hovered(window)
                                    {
                                        if let Some(mouse_up_ix) = index_for_position_in_lines(
                                            event.position,
                                            bounds,
                                            lines,
                                            line_height,
                                        ) {
                                            click_listener(
                                                &clickable_ranges,
                                                InteractiveTextClickEvent {
                                                    mouse_down_index: mouse_down_ix,
                                                    mouse_up_index: mouse_up_ix,
                                                },
                                                window,
                                                cx,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    });
                    effects.mouse_up_listeners.push(mouse_up_listener);
                }

                // Mouse move handler - track hover state
                if let Some(hover_listener) = interactive.hover_listener.clone() {
                    let mouse_move_listener: MouseMoveListener = Rc::new({
                        let hovered_index = interactive.hovered_index.clone();
                        let layout_data = layout_data.clone();
                        move |event, phase, hitbox, window, cx| {
                            if phase == DispatchPhase::Bubble {
                                let current_index = if hitbox.is_hovered(window) {
                                    layout_data.as_ref().and_then(|(lines, line_height)| {
                                        index_for_position_in_lines(
                                            event.position,
                                            bounds,
                                            lines,
                                            *line_height,
                                        )
                                    })
                                } else {
                                    None
                                };

                                let prev_index = hovered_index.get();
                                if current_index != prev_index {
                                    hovered_index.set(current_index);
                                    hover_listener(current_index, event.clone(), window, cx);
                                }
                            }
                        }
                    });
                    effects.mouse_move_listeners.push(mouse_move_listener);
                }
            }

            // Tooltip handling (still uses window.on_mouse_event for shared behavior)
            if let Some(tooltip_builder) = interactive.tooltip_builder.clone() {
                let active_tooltip = interactive.active_tooltip.clone();
                let hovered_index = interactive.hovered_index.clone();

                let build_tooltip: Rc<dyn Fn(&mut Window, &mut App) -> Option<(AnyView, bool)>> =
                    Rc::new(move |window, cx| {
                        let ix = hovered_index.get()?;
                        let view = tooltip_builder(ix, window, cx)?;
                        Some((view, false))
                    });

                let check_is_hovered: Rc<dyn Fn(&Window) -> bool> = Rc::new({
                    let hitbox = hitbox.clone();
                    move |window| hitbox.is_hovered(window)
                });

                let check_is_hovered_during_prepaint: Rc<dyn Fn(&Window) -> bool> = Rc::new({
                    move |window| bounds.contains(&window.mouse_position())
                });

                let tooltip_id = set_tooltip_on_window(&active_tooltip, ctx.window);

                register_tooltip_mouse_handlers(
                    &active_tooltip,
                    tooltip_id,
                    build_tooltip,
                    check_is_hovered,
                    check_is_hovered_during_prepaint,
                    ctx.window,
                );
            }
        }

        crate::PaintFrame {
            handled: true,
            skip_children: true,
            ..Default::default()
        }
    }

    fn paint_end(&mut self, _ctx: &mut crate::PaintCtx, _frame: crate::PaintFrame) {
        // Nothing to pop for text
    }

    // Uses default downcasting implementations.
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{self as gpui, Context, Render, RenderNode, TestAppContext, div, point, px, size};

    struct RootView;

    impl Render for RootView {
        fn render(&mut self, _window: &mut Window, _cx: &mut Context<Self>) -> impl IntoElement {
            div()
        }
    }

    #[gpui::test]
    fn test_text_node_update_render_node_preserves_layout_when_unchanged(cx: &mut TestAppContext) {
        let (_view, cx) = cx.add_window_view(|_, _| RootView);

        cx.update(|window, app| {
            let known = Size {
                width: None,
                height: None,
            };
            let available = Size {
                width: AvailableSpace::Definite(px(500.)),
                height: AvailableSpace::Definite(px(500.)),
            };

            // &'static str updates should preserve cached layout when unchanged.
            let mut node = TextNode::new(SharedString::from("hello"), None);
            node.measure(known, available, window, app);
            assert!(node.layout.is_some());
            let mut element: &'static str = "hello";
            match element.update_render_node(&mut node, window, app) {
                Some(update) => assert!(!update.any_change()),
                None => panic!("expected &'static str to update an existing TextNode"),
            }
            assert!(node.layout.is_some());

            element = "world";
            match element.update_render_node(&mut node, window, app) {
                Some(update) => assert!(update.layout_changed && update.paint_changed),
                None => panic!("expected &'static str to update an existing TextNode"),
            }
            assert!(node.layout.is_none());

            // SharedString updates should preserve cached layout when unchanged.
            let mut node = TextNode::new(SharedString::from("hello"), None);
            node.measure(known, available, window, app);
            assert!(node.layout.is_some());
            let mut element = SharedString::from("hello");
            match element.update_render_node(&mut node, window, app) {
                Some(update) => assert!(!update.any_change()),
                None => panic!("expected SharedString to update an existing TextNode"),
            }
            assert!(node.layout.is_some());

            element = SharedString::from("world");
            match element.update_render_node(&mut node, window, app) {
                Some(update) => assert!(update.layout_changed && update.paint_changed),
                None => panic!("expected SharedString to update an existing TextNode"),
            }
            assert!(node.layout.is_none());
        });
    }

    #[gpui::test]
    fn test_text_node_paint_begin_uses_paint_bounds(cx: &mut TestAppContext) {
        let (_view, cx) = cx.add_window_view(|_, _| RootView);

        cx.update(|window, app| {
            let known = Size {
                width: None,
                height: None,
            };
            let available = Size {
                width: AvailableSpace::Definite(px(500.)),
                height: AvailableSpace::Definite(px(500.)),
            };

            let mut node = TextNode::new(SharedString::from("hello"), None);
            node.measure(known, available, window, app);

            let old_bounds = Bounds::new(point(px(1.), px(2.)), size(px(3.), px(4.)));
            let new_bounds = Bounds::new(point(px(10.), px(20.)), size(px(30.), px(40.)));
            node.bounds = Some(old_bounds);

            window.invalidator.set_phase(crate::window::DrawPhase::Paint);
            window.next_frame.scene.begin_frame();

            let fiber_id = window.fiber.tree.create_placeholder_fiber();
            let mut paint_ctx = crate::PaintCtx {
                fiber_id,
                bounds: new_bounds,
                child_bounds: Vec::new(),
                inspector_id: None,
                window,
                cx: app,
            };
            node.paint_begin(&mut paint_ctx);

            assert_eq!(
                node.bounds,
                Some(new_bounds),
                "TextNode must use paint-time bounds to avoid stale positioning when prepaint is replayed"
            );
        });
    }
}
