use gpui_macros::Element;
use std::{
    any::TypeId,
    rc::Rc,
    time::{Duration, Instant},
};

use refineable::Refineable;
use smallvec::SmallVec;
use taffy::style::{Dimension, Style as TaffyStyle};

use crate::{
    AnyElement, App, DefiniteLength, ElementId, ElementImpl, GlobalElementId,
    IntoElement, Length, Pixels, Style, Window,
    render_node::{
        CallbackSlot, ConditionalSlot, LayoutCtx, LayoutFrame, PaintCtx, PaintFrame, PrepaintCtx,
        PrepaintFrame, RenderNode, UpdateResult,
    },
};

pub use easing::*;

/// An animation that can be applied to an element.
#[derive(Clone)]
pub struct Animation {
    /// The amount of time for which this animation should run
    pub duration: Duration,
    /// Whether to repeat this animation when it finishes
    pub oneshot: bool,
    /// A function that takes a delta between 0 and 1 and returns a new delta
    /// between 0 and 1 based on the given easing function.
    pub easing: Rc<dyn Fn(f32) -> f32>,
}

impl Animation {
    /// Create a new animation with the given duration.
    /// By default the animation will only run once and will use a linear easing function.
    pub fn new(duration: Duration) -> Self {
        Self {
            duration,
            oneshot: true,
            easing: Rc::new(linear),
        }
    }

    /// Set the animation to loop when it finishes.
    pub fn repeat(mut self) -> Self {
        self.oneshot = false;
        self
    }

    /// Set the easing function to use for this animation.
    /// The easing function will take a time delta between 0 and 1 and return a new delta
    /// between 0 and 1
    pub fn with_easing(mut self, easing: impl Fn(f32) -> f32 + 'static) -> Self {
        self.easing = Rc::new(easing);
        self
    }
}

/// An extension trait for adding the animation wrapper to both Elements and Components
pub trait AnimationExt {
    /// Render this component or element with an animation
    fn with_animation(
        self,
        id: impl Into<ElementId>,
        animation: Animation,
        animator: impl Fn(Self, f32) -> Self + 'static,
    ) -> AnimationElement<Self>
    where
        Self: Sized + IntoElement + 'static,
    {
        AnimationElement {
            id: id.into(),
            element: Some(self),
            animator: Box::new(move |this, _, value| animator(this, value)),
            animations: smallvec::smallvec![animation],
        }
    }

    /// Render this component or element with a chain of animations
    fn with_animations(
        self,
        id: impl Into<ElementId>,
        animations: Vec<Animation>,
        animator: impl Fn(Self, usize, f32) -> Self + 'static,
    ) -> AnimationElement<Self>
    where
        Self: Sized + IntoElement + 'static,
    {
        AnimationElement {
            id: id.into(),
            element: Some(self),
            animator: Box::new(animator),
            animations: animations.into(),
        }
    }
}

impl<E: IntoElement + 'static> AnimationExt for E {}

/// A GPUI element that applies an animation to another element
#[derive(Element)]
#[element(crate = crate)]
pub struct AnimationElement<E: IntoElement + 'static> {
    id: ElementId,
    element: Option<E>,
    animations: SmallVec<[Animation; 1]>,
    animator: Box<dyn Fn(E, usize, f32) -> E + 'static>,
}

impl<E: IntoElement + 'static> AnimationElement<E> {
    /// Returns a new [`AnimationElement<E>`] after applying the given function
    /// to the element being animated.
    pub fn map_element(mut self, f: impl FnOnce(E) -> E) -> AnimationElement<E> {
        self.element = self.element.map(f);
        self
    }
}

impl<E: IntoElement + 'static> IntoElement for AnimationElement<E> {
    type Element = AnimationElement<E>;

    fn into_element(self) -> Self::Element {
        self
    }
}

/// Type alias for the callback that produces the animated child.
type ProduceChildFn = dyn FnOnce(usize, f32) -> AnyElement;

/// Retained render node for AnimationElement.
///
/// Stores animation timing state and uses fiber-backed rendering for the
/// animated child. The child is produced during layout and reconciled into
/// the main fiber tree via a node-owned conditional slot, so it participates
/// in the normal layout/prepaint/paint passes.
pub(crate) struct AnimationNode {
    start: Instant,
    animation_ix: usize,
    animations: SmallVec<[Animation; 1]>,
    produce_child: CallbackSlot<ProduceChildFn>,
    pending_child: Option<AnyElement>,
    cached_taffy_style: TaffyStyle,
    done: bool,
}

impl AnimationNode {
    fn new(animations: SmallVec<[Animation; 1]>) -> Self {
        Self {
            start: Instant::now(),
            animation_ix: 0,
            animations,
            produce_child: CallbackSlot::new(),
            pending_child: None,
            cached_taffy_style: TaffyStyle {
                size: taffy::prelude::Size {
                    width: Dimension::auto(),
                    height: Dimension::auto(),
                },
                ..Default::default()
            },
            done: false,
        }
    }

    fn compute_delta_and_advance(&mut self) -> (usize, f32) {
        let animation_ix = self.animation_ix;
        let mut delta = self.start.elapsed().as_secs_f32()
            / self.animations[animation_ix].duration.as_secs_f32();

        if delta > 1.0 {
            if self.animations[animation_ix].oneshot {
                if animation_ix >= self.animations.len() - 1 {
                    self.done = true;
                } else {
                    self.start = Instant::now();
                    self.animation_ix += 1;
                }
                delta = 1.0;
            } else {
                delta %= 1.0;
            }
        }

        let eased_delta = (self.animations[animation_ix].easing)(delta);
        debug_assert!(
            (0.0..=1.0).contains(&eased_delta),
            "delta should always be between 0 and 1"
        );

        (animation_ix, eased_delta)
    }
}

impl RenderNode for AnimationNode {
    fn taffy_style(&self, _rem_size: Pixels, _scale_factor: f32) -> TaffyStyle {
        self.cached_taffy_style.clone()
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
        false
    }

    fn layout_begin(&mut self, ctx: &mut LayoutCtx) -> LayoutFrame {
        let (animation_ix, delta) = self.compute_delta_and_advance();

        // Store the produced child for slot reconciliation.
        self.pending_child = None;
        if let Some(produce) = self.produce_child.take() {
            let child = produce(animation_ix, delta);

            // The AnimationElement itself should be layout-transparent: it shouldn't break
            // percentage sizing on the animated child (e.g. `size_full()`), which expects
            // to resolve against the original parent constraints.
            //
            // When the animated child uses fractional sizing in an axis, make the wrapper
            // fill that axis so the child's percentage behaves as if it were directly under
            // the wrapper's parent.
            let mut style = Style::default();
            if let Some(refinement) = child.style() {
                style.refine(refinement);
            }

            let child_uses_fraction_width = matches!(
                style.size.width,
                Length::Definite(DefiniteLength::Fraction(_))
            );
            let child_uses_fraction_height = matches!(
                style.size.height,
                Length::Definite(DefiniteLength::Fraction(_))
            );

            self.cached_taffy_style.size.width = if child_uses_fraction_width {
                Dimension::percent(1.0)
            } else {
                Dimension::auto()
            };
            self.cached_taffy_style.size.height = if child_uses_fraction_height {
                Dimension::percent(1.0)
            } else {
                Dimension::auto()
            };

            self.pending_child = Some(child);
        }

        if !self.done {
            ctx.window.request_animation_frame();
        }

        LayoutFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn layout_end(&mut self, _ctx: &mut LayoutCtx, _frame: LayoutFrame) {}

    fn conditional_slots(&mut self, _fiber_id: GlobalElementId) -> SmallVec<[ConditionalSlot; 4]> {
        let mut slots = SmallVec::new();

        let child = self.pending_child.take();
        if let Some(child) = child {
            slots.push(ConditionalSlot::active(0, move || child));
        } else {
            // Keep the existing child if we didn't produce a new one this layout pass.
            slots.push(ConditionalSlot {
                slot_index: 0,
                active: true,
                element_factory: None,
            });
        }

        slots
    }

    fn prepaint_begin(&mut self, ctx: &mut PrepaintCtx) -> PrepaintFrame {
        let _ = ctx;
        PrepaintFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn prepaint_end(&mut self, _ctx: &mut PrepaintCtx, _frame: PrepaintFrame) {}

    fn paint_begin(&mut self, ctx: &mut PaintCtx) -> PaintFrame {
        let _ = ctx;
        PaintFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn paint_end(&mut self, _ctx: &mut PaintCtx, _frame: PaintFrame) {}
}

impl<E: IntoElement + 'static> ElementImpl for AnimationElement<E> {
    fn id(&self) -> Option<ElementId> {
        Some(self.id.clone())
    }

    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        let mut node = AnimationNode::new(std::mem::take(&mut self.animations));

        // Deposit the callback immediately (same logic as update_render_node)
        if let Some(element) = self.element.take() {
            let animator = std::mem::replace(&mut self.animator, Box::new(|e, _, _| e));
            node.produce_child
                .deposit(Box::new(move |animation_ix, delta| {
                    (animator)(element, animation_ix, delta).into_any_element()
                }));
        }

        Some(Box::new(node))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<AnimationNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let node = node.as_any_mut().downcast_mut::<AnimationNode>()?;

        node.animations = std::mem::take(&mut self.animations);

        let element = self.element.take()?;
        let animator = std::mem::replace(&mut self.animator, Box::new(|e, _, _| e));

        node.produce_child
            .deposit(Box::new(move |animation_ix, delta| {
                (animator)(element, animation_ix, delta).into_any_element()
            }));

        Some(UpdateResult::LAYOUT_CHANGED)
    }
}

mod easing {
    use std::f32::consts::PI;

    /// The linear easing function, or delta itself
    pub fn linear(delta: f32) -> f32 {
        delta
    }

    /// The quadratic easing function, delta * delta
    pub fn quadratic(delta: f32) -> f32 {
        delta * delta
    }

    /// The quadratic ease-in-out function, which starts and ends slowly but speeds up in the middle
    pub fn ease_in_out(delta: f32) -> f32 {
        if delta < 0.5 {
            2.0 * delta * delta
        } else {
            let x = -2.0 * delta + 2.0;
            1.0 - x * x / 2.0
        }
    }

    /// The Quint ease-out function, which starts quickly and decelerates to a stop
    pub fn ease_out_quint() -> impl Fn(f32) -> f32 {
        move |delta| 1.0 - (1.0 - delta).powi(5)
    }

    /// Apply the given easing function, first in the forward direction and then in the reverse direction
    pub fn bounce(easing: impl Fn(f32) -> f32) -> impl Fn(f32) -> f32 {
        move |delta| {
            if delta < 0.5 {
                easing(delta * 2.0)
            } else {
                easing((1.0 - delta) * 2.0)
            }
        }
    }

    /// A custom easing function for pulsating alpha that slows down as it approaches 0.1
    pub fn pulsating_between(min: f32, max: f32) -> impl Fn(f32) -> f32 {
        let range = max - min;

        move |delta| {
            // Use a combination of sine and cubic functions for a more natural breathing rhythm
            let t = (delta * 2.0 * PI).sin();
            let breath = (t * t * t + t) / 2.0;

            // Map the breath to our desired alpha range
            let normalized_alpha = (breath + 1.0) / 2.0;

            min + (normalized_alpha * range)
        }
    }
}
