use crate::{
    AnyElement, AnyEntity, AnyWeakEntity, App, AvailableSpace, Bounds, Context, Element, ElementId,
    Entity, EntityId, GlobalElementId, InspectorElementId, IntoElement, LayoutId, Pixels, Render,
    RenderNode, Size, UpdateResult, WeakEntity,
};
use crate::{Empty, Window};
use anyhow::Result;
use std::{any::TypeId, fmt};
use taffy::style::{Dimension, Style as TaffyStyle};

#[doc(hidden)]
pub struct ViewLayoutState;

impl<V: Render> Element for Entity<V> {
    type RequestLayoutState = ViewLayoutState;
    type PrepaintState = ();

    fn id(&self) -> Option<ElementId> {
        Some(ElementId::View(self.entity_id()))
    }

    fn source_location(&self) -> Option<&'static std::panic::Location<'static>> {
        None
    }

    fn request_layout(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _window: &mut Window,
        _cx: &mut App,
    ) -> (LayoutId, Self::RequestLayoutState) {
        unreachable!("Entity<V: Render> uses retained node path")
    }

    fn prepaint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _element: &mut Self::RequestLayoutState,
        _window: &mut Window,
        _cx: &mut App,
    ) {
        unreachable!("Entity<V: Render> uses retained node path")
    }

    fn paint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _element: &mut Self::RequestLayoutState,
        _prepaint: &mut Self::PrepaintState,
        _window: &mut Window,
        _cx: &mut App,
    ) {
        unreachable!("Entity<V: Render> uses retained node path")
    }

    fn as_any_view(&self) -> Option<AnyView> {
        Some(AnyView::from(self.clone()))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<ViewNode>())
    }

    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        Some(Box::new(ViewNode::new(self.entity_id())))
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let view_node = node.as_any_mut().downcast_mut::<ViewNode>()?;
        // View identity is stable (EntityId doesn't change), so nothing to update
        debug_assert_eq!(view_node.view_id, self.entity_id());
        Some(UpdateResult::UNCHANGED)
    }
}

/// Retained render node for view elements.
///
/// Views are boundaries in the component tree. The ViewNode itself doesn't render anything -
/// it just marks the boundary and delegates to the view's rendered element tree (which is
/// expanded during reconciliation via `expand_view_fibers`).
pub(crate) struct ViewNode {
    view_id: EntityId,
}

impl ViewNode {
    pub(crate) fn new(view_id: EntityId) -> Self {
        Self { view_id }
    }
}

impl RenderNode for ViewNode {
    fn taffy_style(&self, _rem_size: Pixels, _scale_factor: f32) -> TaffyStyle {
        // Views are layout-transparent - their child element determines the layout
        TaffyStyle {
            size: taffy::prelude::Size {
                width: Dimension::auto(),
                height: Dimension::auto(),
            },
            ..Default::default()
        }
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

    fn measure(
        &mut self,
        _known: Size<Option<Pixels>>,
        _available: Size<AvailableSpace>,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<Size<Pixels>> {
        // Views don't have intrinsic size - layout comes from their children
        None
    }

    fn layout_begin(&mut self, ctx: &mut crate::LayoutCtx) -> crate::LayoutFrame {
        // Register this view fiber in view_roots so expand_view_fibers can find it
        // and render the view's content
        ctx.window
            .fiber
            .tree
            .set_view_root(self.view_id, ctx.fiber_id);

        // Push view boundary for scope tracking
        ctx.window.rendered_entity_stack.push(self.view_id);
        crate::LayoutFrame {
            handled: true,
            pushed_view_boundary: true,
            ..Default::default()
        }
    }

    fn layout_end(&mut self, ctx: &mut crate::LayoutCtx, frame: crate::LayoutFrame) {
        if frame.pushed_view_boundary {
            ctx.window.rendered_entity_stack.pop();
        }
    }

    fn prepaint_begin(&mut self, ctx: &mut crate::PrepaintCtx) -> crate::PrepaintFrame {
        // Set the view context for prepaint
        ctx.window.set_view_id(self.view_id);
        crate::PrepaintFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn prepaint_end(&mut self, _ctx: &mut crate::PrepaintCtx, _frame: crate::PrepaintFrame) {
        // Nothing to pop - view context is per-frame
    }

    fn paint_begin(&mut self, _ctx: &mut crate::PaintCtx) -> crate::PaintFrame {
        // Views don't paint anything themselves - their children do
        crate::PaintFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn paint_end(&mut self, _ctx: &mut crate::PaintCtx, _frame: crate::PaintFrame) {
        // Nothing to clean up
    }
}

/// A dynamically-typed handle to a view, which can be downcast to a [Entity] for a specific type.
#[derive(Clone, Debug)]
pub struct AnyView {
    entity: AnyEntity,
    render: fn(&AnyView, &mut Window, &mut App) -> AnyElement,
}

impl<V: Render> From<Entity<V>> for AnyView {
    fn from(value: Entity<V>) -> Self {
        AnyView {
            entity: value.into_any(),
            render: any_view::render::<V>,
        }
    }
}

impl AnyView {
    /// Convert this to a weak handle.
    pub fn downgrade(&self) -> AnyWeakView {
        AnyWeakView {
            entity: self.entity.downgrade(),
            render: self.render,
        }
    }

    /// Convert this to a [Entity] of a specific type.
    /// If this handle does not contain a view of the specified type, returns itself in an `Err` variant.
    pub fn downcast<T: 'static>(self) -> Result<Entity<T>, Self> {
        match self.entity.downcast() {
            Ok(entity) => Ok(entity),
            Err(entity) => Err(Self {
                entity,
                render: self.render,
            }),
        }
    }

    /// Gets the [TypeId] of the underlying view.
    pub fn entity_type(&self) -> TypeId {
        self.entity.entity_type
    }

    /// Gets the entity id of this handle.
    pub fn entity_id(&self) -> EntityId {
        self.entity.entity_id()
    }

    /// Render this view to an AnyElement.
    pub(crate) fn render_element(&self, window: &mut Window, cx: &mut App) -> AnyElement {
        (self.render)(self, window, cx)
    }
}

impl PartialEq for AnyView {
    fn eq(&self, other: &Self) -> bool {
        self.entity == other.entity
    }
}

impl Eq for AnyView {}

impl Element for AnyView {
    type RequestLayoutState = ViewLayoutState;
    type PrepaintState = ();

    fn id(&self) -> Option<ElementId> {
        Some(ElementId::View(self.entity_id()))
    }

    fn source_location(&self) -> Option<&'static core::panic::Location<'static>> {
        None
    }

    fn request_layout(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _window: &mut Window,
        _cx: &mut App,
    ) -> (LayoutId, Self::RequestLayoutState) {
        unreachable!("AnyView uses retained node path")
    }

    fn prepaint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _element: &mut Self::RequestLayoutState,
        _window: &mut Window,
        _cx: &mut App,
    ) {
        unreachable!("AnyView uses retained node path")
    }

    fn paint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _element: &mut Self::RequestLayoutState,
        _prepaint: &mut Self::PrepaintState,
        _window: &mut Window,
        _cx: &mut App,
    ) {
        unreachable!("AnyView uses retained node path")
    }

    fn as_any_view(&self) -> Option<AnyView> {
        Some(self.clone())
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<ViewNode>())
    }

    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        Some(Box::new(ViewNode::new(self.entity_id())))
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let view_node = node.as_any_mut().downcast_mut::<ViewNode>()?;
        debug_assert_eq!(view_node.view_id, self.entity_id());
        Some(UpdateResult::UNCHANGED)
    }
}

impl<V: 'static + Render> IntoElement for Entity<V> {
    type Element = Entity<V>;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl IntoElement for AnyView {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

/// A weak, dynamically-typed view handle that does not prevent the view from being released.
pub struct AnyWeakView {
    entity: AnyWeakEntity,
    render: fn(&AnyView, &mut Window, &mut App) -> AnyElement,
}

impl AnyWeakView {
    /// Convert to a strongly-typed handle if the referenced view has not yet been released.
    pub fn upgrade(&self) -> Option<AnyView> {
        let entity = self.entity.upgrade()?;
        Some(AnyView {
            entity,
            render: self.render,
        })
    }
}

impl<V: 'static + Render> From<WeakEntity<V>> for AnyWeakView {
    fn from(view: WeakEntity<V>) -> Self {
        AnyWeakView {
            entity: view.into(),
            render: any_view::render::<V>,
        }
    }
}

impl PartialEq for AnyWeakView {
    fn eq(&self, other: &Self) -> bool {
        self.entity == other.entity
    }
}

impl std::fmt::Debug for AnyWeakView {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AnyWeakView")
            .field("entity_id", &self.entity.entity_id)
            .finish_non_exhaustive()
    }
}

mod any_view {
    use crate::{AnyElement, AnyView, App, IntoElement, Render, Window};

    pub(crate) fn render<V: 'static + Render>(
        view: &AnyView,
        window: &mut Window,
        cx: &mut App,
    ) -> AnyElement {
        let view = view.clone().downcast::<V>().unwrap();
        view.update(cx, |view, cx| view.render(window, cx).into_any_element())
    }
}

/// A view that renders nothing
pub struct EmptyView;

impl Render for EmptyView {
    fn render(&mut self, _window: &mut Window, _cx: &mut Context<Self>) -> impl IntoElement {
        Empty
    }
}
