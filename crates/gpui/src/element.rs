//! Elements are the workhorses of GPUI. They are responsible for laying out and painting all of
//! the contents of a window. Elements form a tree and are laid out according to the web layout
//! standards as implemented by [taffy](https://github.com/DioxusLabs/taffy). Most of the time,
//! you won't need to interact with this module or these APIs directly. Elements provide their
//! own APIs and GPUI, or other element implementation, uses the APIs in this module to convert
//! that element tree into the pixels you see on the screen.
//!
//! # Element Basics
//!
//! Elements are constructed by calling [`Render::render()`] on the root view of the window,
//! which recursively constructs the element tree from the current state of the application,.
//! These elements are then laid out by Taffy, and painted to the screen according to their own
//! implementation of [`Element::paint()`]. Before the start of the next frame, the entire element
//! tree and any callbacks they have registered with GPUI are dropped and the process repeats.
//!
//! But some state is too simple and voluminous to store in every view that needs it, e.g.
//! whether a hover has been started or not. For this, GPUI provides the [`Element::PrepaintState`], associated type.
//!
//! # Implementing your own elements
//!
//! Elements are intended to be the low level, imperative API to GPUI. They are responsible for upholding,
//! or breaking, GPUI's features as they deem necessary. As an example, most GPUI elements are expected
//! to stay in the bounds that their parent element gives them. But with [`Window::with_content_mask`],
//! you can ignore this restriction and paint anywhere inside of the window's bounds. This is useful for overlays
//! and popups and anything else that shows up 'on top' of other elements.
//! With great power, comes great responsibility.
//!
//! However, most of the time, you won't need to implement your own elements. GPUI provides a number of
//! elements that should cover most common use cases out of the box and it's recommended that you use those
//! to construct `components`, using the [`RenderOnce`] trait and the `#[derive(IntoElement)]` macro. Only implement
//! elements when you need to take manual control of the layout and painting process, such as when using
//! your own custom layout algorithm or rendering a code editor.

use crate::{
    AnyView, App, Bounds, Context, ElementId, EntityId, FocusHandle, InspectorElementId, LayoutId,
    Pixels, StyleRefinement, UpdateResult, Window, util::FluentBuilder,
};
use std::{
    any::{Any, TypeId, type_name},
    panic,
};
use taffy::tree::NodeId;

fn debug_fiber_id_stack(window: &Window) -> String {
    let stack = &window.fiber.fiber_id_stack;
    let len = stack.len();
    let preview_len = len.min(16);
    let start = len.saturating_sub(preview_len);
    let preview = &stack[start..];
    format!("len={len} tail={preview:?}")
}

/// Implemented by types that participate in laying out and painting the contents of a window.
/// Elements form a tree and are laid out according to web-based layout rules, as implemented by Taffy.
/// You can create custom elements by implementing this trait, see the module-level documentation
/// for more details.
pub trait Element: 'static + IntoElement {
    /// The type of state returned from [`Element::request_layout`]. A mutable reference to this state is subsequently
    /// provided to [`Element::prepaint`] and [`Element::paint`].
    type RequestLayoutState: 'static;

    /// The type of state returned from [`Element::prepaint`]. A mutable reference to this state is subsequently
    /// provided to [`Element::paint`].
    type PrepaintState: 'static;

    /// If this element has a unique identifier, return it here. This is used to track elements across frames, and
    /// will cause a GlobalElementId to be passed to the request_layout, prepaint, and paint methods.
    ///
    /// The global id can in turn be used to access state that's connected to an element with the same id across
    /// frames. This id must be unique among children of the first containing element with an id.
    fn id(&self) -> Option<ElementId>;

    /// Source location where this element was constructed, used to disambiguate elements in the
    /// inspector and navigate to their source code.
    fn source_location(&self) -> Option<&'static panic::Location<'static>>;

    /// Before an element can be painted, we need to know where it's going to be and how big it is.
    /// Use this method to request a layout from Taffy and initialize the element's state.
    fn request_layout(
        &mut self,
        id: Option<&GlobalElementId>,
        inspector_id: Option<&InspectorElementId>,
        window: &mut Window,
        cx: &mut App,
    ) -> (LayoutId, Self::RequestLayoutState);

    /// After laying out an element, we need to commit its bounds to the current frame for hitbox
    /// purposes. The state argument is the same state that was returned from [`Element::request_layout()`].
    fn prepaint(
        &mut self,
        id: Option<&GlobalElementId>,
        inspector_id: Option<&InspectorElementId>,
        bounds: Bounds<Pixels>,
        request_layout: &mut Self::RequestLayoutState,
        window: &mut Window,
        cx: &mut App,
    ) -> Self::PrepaintState;

    /// Once layout has been completed, this method will be called to paint the element to the screen.
    /// The state argument is the same state that was returned from [`Element::request_layout()`].
    fn paint(
        &mut self,
        id: Option<&GlobalElementId>,
        inspector_id: Option<&InspectorElementId>,
        bounds: Bounds<Pixels>,
        request_layout: &mut Self::RequestLayoutState,
        prepaint: &mut Self::PrepaintState,
        window: &mut Window,
        cx: &mut App,
    );

    /// Convert this element into a dynamically-typed [`AnyElement`].
    fn into_any(self) -> AnyElement {
        AnyElement::new(self)
    }

    /// Returns a slice of this element's children for fiber tree construction.
    /// Default returns an empty slice.
    fn fiber_children(&self) -> &[AnyElement] {
        &[]
    }

    /// Returns a mutable slice of this element's children.
    /// Default returns an empty slice.
    fn fiber_children_mut(&mut self) -> &mut [AnyElement] {
        &mut []
    }

    /// Returns this element's cached style, if any.
    fn cached_style(&self) -> Option<&StyleRefinement> {
        None
    }

    /// Returns a dynamic view handle if this element represents a view.
    fn as_any_view(&self) -> Option<AnyView> {
        None
    }

    /// Creates a new render node for this element.
    ///
    /// Return `Some(node)` if this element supports retained rendering.
    /// Return `None` for legacy/opaque elements that don't support caching.
    ///
    /// Retained nodes persist in the fiber tree and own element-specific state
    /// (interactivity, scroll position, caches, etc.). They enable incremental
    /// rendering by implementing the scope-based prepaint/paint lifecycle.
    ///
    /// Default returns `None`, meaning the element is treated as opaque
    /// (always dirty, no caching).
    fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>> {
        None
    }

    /// Returns the `TypeId` of the concrete render node this element creates, if any.
    ///
    /// Used by the fiber renderer to decide whether an existing retained node can be updated
    /// in-place, or must be replaced. This avoids relying on per-element downcast failures to
    /// detect type mismatches.
    ///
    /// Implementations should return `Some(TypeId::of::<Node>())` when `create_render_node`
    /// returns `Some(Node)`, and `None` otherwise.
    ///
    /// Default returns `None` (no retained render node support).
    fn render_node_type_id(&self) -> Option<TypeId> {
        None
    }

    /// Updates an existing render node with this element's current data.
    ///
    /// Called during reconciliation when a matching node already exists.
    /// Returns `Some(UpdateResult)` if the update succeeded (node type matched),
    /// `None` if the node type didn't match (caller should create a new node).
    ///
    /// The `UpdateResult` indicates what changed, which the fiber system uses
    /// to set appropriate dirty flags:
    /// - `layout_changed`: layout needs to be recomputed
    /// - `paint_changed`: prepaint and paint need to re-run
    ///
    /// IMPORTANT: Implementations must only consume element data (via `take_*`
    /// methods) when the downcast succeeds. If the downcast fails, the element
    /// data must remain intact so `create_render_node` can be called.
    ///
    /// Default implementation returns `None` (no render node support).
    fn update_render_node(
        &mut self,
        _node: &mut dyn crate::RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        None
    }

    /// Expand this element into its inner element, if applicable.
    ///
    /// This is used to make wrapper elements like `Component<C>` transparent
    /// to the fiber system. When an element is expanded, its inner element
    /// is used directly for reconciliation instead of the wrapper.
    ///
    /// Returns `Some(inner_element)` if this element should be expanded,
    /// `None` if it should be processed as-is.
    ///
    /// Default returns `None` (no expansion).
    fn try_expand(&mut self, _window: &mut Window, _cx: &mut App) -> Option<AnyElement> {
        None
    }

    /// Takes the layout listener from this element, if any.
    ///
    /// Called during fiber reconciliation to extract layout listeners
    /// for storage in the FiberTree. Only Interactivity-backed elements
    /// (Div, Svg) support this; other elements return None.
    ///
    /// Default returns `None`.
    fn take_layout_listener(
        &mut self,
    ) -> Option<Box<dyn FnMut(Bounds<Pixels>, &mut Window, &mut App) + 'static>> {
        None
    }
}

/// A helper trait for implementing fiber-native elements with reduced boilerplate.
///
/// When using `#[derive(Element)]`, implement this trait instead of `Element` directly.
/// The derive macro will generate the `Element` impl that delegates to these methods.
///
/// # Example
///
/// ```ignore
/// use gpui::{Element, ElementImpl, IntoElement, RenderNode, AnyElement, UpdateResult};
///
/// #[derive(Element, IntoElement)]
/// struct MyFiberElement {
///     id: ElementId,
///     children: SmallVec<[AnyElement; 4]>,
/// }
///
/// impl ElementImpl for MyFiberElement {
///     fn children(&self) -> &[AnyElement] {
///         &self.children
///     }
///
///     fn children_mut(&mut self) -> &mut [AnyElement] {
///         &mut self.children
///     }
///
///     fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
///         Some(Box::new(MyNode::new()))
///     }
///
///     fn render_node_type_id(&self) -> Option<TypeId> {
///         Some(TypeId::of::<MyNode>())
///     }
///
///     fn update_render_node(
///         &mut self,
///         node: &mut dyn RenderNode,
///         _window: &mut Window,
///         _cx: &mut App,
///     ) -> Option<UpdateResult> {
///         Some(UpdateResult::UNCHANGED)
///     }
/// }
/// ```
pub trait ElementImpl: 'static + IntoElement {
    /// Returns a slice of this element's children.
    fn children(&self) -> &[AnyElement] {
        &[]
    }

    /// Returns a mutable slice of this element's children.
    fn children_mut(&mut self) -> &mut [AnyElement] {
        &mut []
    }

    /// Creates a new render node for this element.
    fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>> {
        None
    }

    /// Returns the TypeId of the render node this element creates.
    fn render_node_type_id(&self) -> Option<TypeId> {
        None
    }

    /// Updates an existing render node with this element's current data.
    fn update_render_node(
        &mut self,
        _node: &mut dyn crate::RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        None
    }

    /// Returns the element's ID for reconciliation keying.
    fn id(&self) -> Option<ElementId> {
        None
    }

    /// Returns the source location where this element was created (for inspector).
    fn source_location(&self) -> Option<&'static core::panic::Location<'static>> {
        None
    }

    /// Returns the cached style for this element.
    fn cached_style(&self) -> Option<&StyleRefinement> {
        None
    }

    /// Takes the layout listener callback from this element.
    fn take_layout_listener(
        &mut self,
    ) -> Option<Box<dyn FnMut(Bounds<Pixels>, &mut Window, &mut App) + 'static>> {
        None
    }
}

/// Implemented by any type that can be converted into an element.
pub trait IntoElement: Sized {
    /// The specific type of element into which the implementing type is converted.
    /// Useful for converting other types into elements automatically, like Strings
    type Element: Element;

    /// Convert self into a type that implements [`Element`].
    fn into_element(self) -> Self::Element;

    /// Convert self into a dynamically-typed [`AnyElement`].
    fn into_any_element(self) -> AnyElement {
        self.into_element().into_any()
    }

    /// Set the z-index for this element, controlling its stacking order.
    /// Elements with a z-index are painted after all other content.
    /// Higher values appear on top of lower values.
    fn z_index(self, index: usize) -> AnyElement {
        let mut element = self.into_any_element();
        element.modifiers.z_index = Some(index);
        element
    }
}

impl<T: IntoElement> FluentBuilder for T {}

/// An object that can be drawn to the screen. This is the trait that distinguishes "views" from
/// other entities. Views are `Entity`'s which `impl Render` and drawn to the screen.
pub trait Render: 'static + Sized {
    /// Render this view into an element tree.
    fn render(&mut self, window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement;
}

impl Render for Empty {
    fn render(&mut self, _window: &mut Window, _cx: &mut Context<Self>) -> impl IntoElement {
        Empty
    }
}

/// You can derive [`IntoElement`] on any type that implements this trait.
/// It is used to construct reusable `components` out of plain data. Think of
/// components as a recipe for a certain pattern of elements. RenderOnce allows
/// you to invoke this pattern, without breaking the fluent builder pattern of
/// the element APIs.
pub trait RenderOnce: 'static {
    /// Render this component into an element tree. Note that this method
    /// takes ownership of self, as compared to [`Render::render()`] method
    /// which takes a mutable reference.
    fn render(self, window: &mut Window, cx: &mut App) -> impl IntoElement;
}

/// This is a helper trait to provide a uniform interface for constructing elements that
/// can accept any number of any kind of child elements
pub trait ParentElement {
    /// Extend this element's children with the given child elements.
    fn extend(&mut self, elements: impl IntoIterator<Item = AnyElement>);

    /// Add a single child element to this element.
    fn child(mut self, child: impl IntoElement) -> Self
    where
        Self: Sized,
    {
        self.extend(std::iter::once(child.into_any_element()));
        self
    }

    /// Add multiple child elements to this element.
    fn children(mut self, children: impl IntoIterator<Item = impl IntoElement>) -> Self
    where
        Self: Sized,
    {
        self.extend(children.into_iter().map(|child| child.into_any_element()));
        self
    }
}

/// An element for rendering components. An implementation detail of the [`IntoElement`] derive macro
/// for [`RenderOnce`]
#[doc(hidden)]
pub struct Component<C: RenderOnce> {
    component: Option<C>,
    #[cfg(debug_assertions)]
    source: &'static core::panic::Location<'static>,
}

impl<C: RenderOnce> Component<C> {
    /// Create a new component from the given RenderOnce type.
    #[track_caller]
    pub fn new(component: C) -> Self {
        Component {
            component: Some(component),
            #[cfg(debug_assertions)]
            source: core::panic::Location::caller(),
        }
    }
}

impl<C: RenderOnce> Element for Component<C> {
    type RequestLayoutState = AnyElement;
    type PrepaintState = ();

    fn id(&self) -> Option<ElementId> {
        None
    }

    fn source_location(&self) -> Option<&'static core::panic::Location<'static>> {
        #[cfg(debug_assertions)]
        return Some(self.source);

        #[cfg(not(debug_assertions))]
        return None;
    }

    fn request_layout(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        window: &mut Window,
        cx: &mut App,
    ) -> (LayoutId, Self::RequestLayoutState) {
        window.with_global_id(ElementId::Name(type_name::<C>().into()), |scoped_id, window| {
            log::trace!(
                "[COMPONENT] request_layout enter, component_type={}, scoped_id={:?}, current_fiber_id={:?}, fiber_stack={}",
                type_name::<C>(),
                scoped_id,
                window.current_fiber_id(),
                debug_fiber_id_stack(window)
            );
            let mut element = self
                .component
                .take()
                .unwrap()
                .render(window, cx)
                .into_any_element();

            let layout_id = element.request_layout(window, cx);
            (layout_id, element)
        })
    }

    fn prepaint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _: Bounds<Pixels>,
        _element: &mut AnyElement,
        _window: &mut Window,
        _cx: &mut App,
    ) {
        // All elements are fiber-backed - prepaint is handled by the fiber system.
    }

    fn paint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _: Bounds<Pixels>,
        _element: &mut Self::RequestLayoutState,
        _: &mut Self::PrepaintState,
        _window: &mut Window,
        _cx: &mut App,
    ) {
        // All elements are fiber-backed - paint is handled by the fiber system.
    }

    fn try_expand(&mut self, window: &mut Window, cx: &mut App) -> Option<AnyElement> {
        self.component
            .take()
            .map(|c| c.render(window, cx).into_any_element())
    }
}

impl<C: RenderOnce> IntoElement for Component<C> {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

/// A globally unique identifier for an element, used to track state across frames.
pub type GlobalElementId = NodeId;

pub(crate) trait ElementObject {
    fn inner_element(&mut self) -> &mut dyn Any;
    fn element_type_name(&self) -> &'static str;
    fn id(&self) -> Option<ElementId>;

    fn request_layout(&mut self, window: &mut Window, cx: &mut App) -> LayoutId;

    fn reset(&mut self);

    fn fiber_children(&self) -> &[AnyElement];
    fn fiber_children_mut(&mut self) -> &mut [AnyElement];
    fn cached_style(&self) -> Option<&StyleRefinement>;
    fn as_any_view(&self) -> Option<AnyView>;
    fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>>;
    fn render_node_type_id(&self) -> Option<TypeId>;
    fn update_render_node(
        &mut self,
        node: &mut dyn crate::RenderNode,
        window: &mut Window,
        cx: &mut App,
    ) -> Option<UpdateResult>;
    fn try_expand(&mut self, window: &mut Window, cx: &mut App) -> Option<AnyElement>;
    fn take_layout_listener(
        &mut self,
    ) -> Option<Box<dyn FnMut(Bounds<Pixels>, &mut Window, &mut App) + 'static>>;
}

/// A wrapper around an implementer of [`Element`] that allows it to be drawn in a window.
pub struct Drawable<E: Element> {
    /// The drawn element.
    pub element: E,
    requested_layout: bool,
}

/// A wrapper around an implementer of [`Element`] that allows it to be drawn in a window.
impl<E: Element> Drawable<E> {
    pub(crate) fn new(element: E) -> Self {
        Drawable {
            element,
            requested_layout: false,
        }
    }

    fn request_layout(&mut self, window: &mut Window, cx: &mut App) -> LayoutId {
        log::trace!(
            "[DRAWABLE] request_layout called, element_type={}, fiber_stack={}",
            std::any::type_name::<E>(),
            debug_fiber_id_stack(window)
        );
        if self.requested_layout {
            log::error!("[DRAWABLE] PANIC: request_layout called more than once!");
            panic!("must call request_layout only once");
        }
        self.requested_layout = true;

        let current_id = window
            .current_fiber_id()
            .filter(|id| window.fiber.tree.get(id).is_some());
        let global_id = current_id.unwrap_or_else(|| window.fiber.tree.create_placeholder_fiber());
        log::trace!(
            "[DRAWABLE] request_layout captured_id={:?}, element_type={}, current_fiber_id={:?}, fiber_stack={}",
            global_id,
            std::any::type_name::<E>(),
            current_id,
            debug_fiber_id_stack(window)
        );

        let inspector_id;
        #[cfg(any(feature = "inspector", debug_assertions))]
        {
            inspector_id = self.element.source_location().map(|source| {
                let path = crate::InspectorElementPath {
                    global_id,
                    source_location: source,
                };
                window.build_inspector_element_id(path)
            });
        }
        #[cfg(not(any(feature = "inspector", debug_assertions)))]
        {
            inspector_id = None;
        }

        let (layout_id, _request_layout) = window.with_element_id_stack(&global_id, |window| {
            self.element.request_layout(
                Some(&global_id),
                inspector_id.as_ref(),
                window,
                cx,
            )
        });
        log::trace!(
            "[DRAWABLE] Transitioned to RequestLayout phase, layout_id={:?}",
            layout_id
        );
        layout_id
    }
}

impl<E> ElementObject for Drawable<E>
where
    E: Element,
    E::RequestLayoutState: 'static,
{
    fn inner_element(&mut self) -> &mut dyn Any {
        &mut self.element
    }

    fn element_type_name(&self) -> &'static str {
        type_name::<E>()
    }

    fn id(&self) -> Option<ElementId> {
        self.element.id()
    }

    fn request_layout(&mut self, window: &mut Window, cx: &mut App) -> LayoutId {
        Drawable::request_layout(self, window, cx)
    }

    fn reset(&mut self) {
        self.requested_layout = false;
    }

    fn fiber_children(&self) -> &[AnyElement] {
        self.element.fiber_children()
    }

    fn fiber_children_mut(&mut self) -> &mut [AnyElement] {
        self.element.fiber_children_mut()
    }

    fn cached_style(&self) -> Option<&StyleRefinement> {
        self.element.cached_style()
    }

    fn as_any_view(&self) -> Option<AnyView> {
        self.element.as_any_view()
    }

    fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>> {
        self.element.create_render_node()
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        self.element.render_node_type_id()
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn crate::RenderNode,
        window: &mut Window,
        cx: &mut App,
    ) -> Option<UpdateResult> {
        self.element.update_render_node(node, window, cx)
    }

    fn try_expand(&mut self, window: &mut Window, cx: &mut App) -> Option<AnyElement> {
        self.element.try_expand(window, cx)
    }

    fn take_layout_listener(
        &mut self,
    ) -> Option<Box<dyn FnMut(Bounds<Pixels>, &mut Window, &mut App) + 'static>> {
        self.element.take_layout_listener()
    }
}

#[allow(missing_docs)]
#[derive(Clone)]
pub(crate) struct ViewData {
    pub view: AnyView,
    pub has_cached_child: bool,
}

impl ViewData {
    pub fn new(view: AnyView) -> Self {
        Self {
            view,
            has_cached_child: false,
        }
    }

    pub fn entity_id(&self) -> EntityId {
        self.view.entity_id()
    }
}

/// Modifiers that can be applied to any element descriptor.
///
/// These are metadata that affect how the element is rendered without
/// creating additional nodes in the retained fiber tree. They are
/// lowered into fiber flags during reconciliation.
#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Hash)]
pub struct ElementModifiers {
    /// If set, this element's subtree is painted in a deferred pass
    /// (after all non-deferred content). Higher priority = painted later (on top).
    pub z_index: Option<usize>,
}

/// A dynamically typed element that can be used to store any element type.
pub struct AnyElement {
    pub(crate) inner: Box<dyn ElementObject>,
    pub(crate) modifiers: ElementModifiers,
}

impl Default for AnyElement {
    fn default() -> Self {
        AnyElement::new(Empty)
    }
}

impl AnyElement {
    pub(crate) fn new<E>(element: E) -> Self
    where
        E: 'static + Element,
        E::RequestLayoutState: Any,
    {
        AnyElement {
            inner: Box::new(Drawable::new(element)),
            modifiers: ElementModifiers::default(),
        }
    }

    /// Attempt to downcast a reference to the boxed element to a specific type.
    pub fn downcast_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.inner.inner_element().downcast_mut::<T>()
    }

    pub(crate) fn children(&self) -> &[AnyElement] {
        self.inner.fiber_children()
    }

    /// Returns the element's reconciliation key.
    /// This is derived from Element::id().
    pub(crate) fn element_id(&self) -> Option<ElementId> {
        self.inner.id()
    }

    pub(crate) fn style(&self) -> Option<&StyleRefinement> {
        self.cached_style()
    }

    /// Returns the modifiers applied to this element.
    pub(crate) fn modifiers(&self) -> ElementModifiers {
        self.modifiers
    }

    pub(crate) fn child_count(&self) -> usize {
        self.children().len()
    }

    /// Returns the total number of elements in this subtree, including self.
    pub fn count(&self) -> usize {
        let mut count = 1;
        for child in self.children() {
            count += child.count();
        }
        count
    }

    /// Resets the element to its initial drawing phase.
    pub fn reset(&mut self) {
        self.inner.reset();
    }

    pub(crate) fn cached_style(&self) -> Option<&StyleRefinement> {
        self.inner.cached_style()
    }

    pub(crate) fn as_any_view(&self) -> Option<AnyView> {
        self.inner.as_any_view()
    }

    pub(crate) fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>> {
        self.inner.create_render_node()
    }

    pub(crate) fn render_node_type_id(&self) -> Option<TypeId> {
        self.inner.render_node_type_id()
    }

    pub(crate) fn update_render_node(
        &mut self,
        node: &mut dyn crate::RenderNode,
        window: &mut Window,
        cx: &mut App,
    ) -> Option<UpdateResult> {
        self.inner.update_render_node(node, window, cx)
    }

    /// Expands this element if it's a wrapper type like Component.
    ///
    /// Returns the inner element if expansion occurred, None if the element
    /// should be processed as-is.
    pub(crate) fn try_expand(&mut self, window: &mut Window, cx: &mut App) -> Option<AnyElement> {
        self.inner.try_expand(window, cx)
    }

    /// Expands wrapper elements (like Component) in place, propagating modifiers.
    ///
    /// This recursively expands this element and all its children so that
    /// Components are transparent before reconciliation. This ensures the real
    /// children/keys participate in normal reconciliation with proper identity.
    pub(crate) fn expand_wrappers(&mut self, window: &mut Window, cx: &mut App) {
        // First expand this element if it's a wrapper
        while let Some(mut expanded) = self.try_expand(window, cx) {
            // Propagate modifiers from the wrapper to the expanded element.
            // If the wrapper had modifiers (e.g., z_index), they should
            // be inherited by the rendered element (unless already set).
            if self.modifiers.z_index.is_some()
                && expanded.modifiers.z_index.is_none()
            {
                expanded.modifiers.z_index = self.modifiers.z_index;
            }
            *self = expanded;
        }

        // Then recursively expand all children
        for child in self.children_mut() {
            child.expand_wrappers(window, cx);
        }
    }

    pub(crate) fn expand_views_detached(&mut self, window: &mut Window, cx: &mut App) {
        let mut stack: Vec<*mut AnyElement> = vec![self as *mut AnyElement];
        while let Some(element_ptr) = stack.pop() {
            // SAFETY: stack pointers always come from `children_mut()` of elements still owned by
            // `self`, and we never store these pointers across external calls.
            let element = unsafe { &mut *element_ptr };

            if let Some(view) = element.as_any_view()
            {
                let view_id = view.entity_id();
                let modifiers = element.modifiers;
                let mut rendered = window.with_rendered_view(view_id, |window| {
                    view.render_element(window, cx)
                });
                rendered.modifiers = modifiers;
                *element = rendered;
            }

            for child in element.children_mut().iter_mut() {
                stack.push(child as *mut AnyElement);
            }
        }
    }

    /// Takes the layout listener from this element, if any.
    ///
    /// Called during fiber reconciliation to extract layout listeners
    /// for storage in the FiberTree.
    pub(crate) fn take_layout_listener(
        &mut self,
    ) -> Option<Box<dyn FnMut(Bounds<Pixels>, &mut Window, &mut App) + 'static>> {
        self.inner.take_layout_listener()
    }

    pub(crate) fn children_mut(&mut self) -> &mut [AnyElement] {
        self.inner.fiber_children_mut()
    }

    /// Request the layout ID of the element stored in this `AnyElement`.
    /// Used for laying out child elements in a parent element.
    pub fn request_layout(&mut self, window: &mut Window, cx: &mut App) -> LayoutId {
        self.inner.request_layout(window, cx)
    }

    /// Prepares the element to be painted by storing its bounds, giving it a chance to draw hitboxes and
    /// request autoscroll before the final paint pass is confirmed.
    ///
    /// Note: For fiber-backed elements (all elements), prepaint is handled by the fiber system.
    /// This method is a no-op.
    pub fn prepaint(&mut self, _window: &mut Window, _cx: &mut App) -> Option<FocusHandle> {
        // All elements are fiber-backed - prepaint is handled by the fiber system.
        None
    }

    /// Paints the element stored in this `AnyElement`.
    ///
    /// Note: For fiber-backed elements (all elements), paint is handled by the fiber system.
    /// This method is a no-op.
    pub fn paint(&mut self, _window: &mut Window, _cx: &mut App) {
        // All elements are fiber-backed - paint is handled by the fiber system.
    }

}

impl Element for AnyElement {
    type RequestLayoutState = ();
    type PrepaintState = ();

    fn id(&self) -> Option<ElementId> {
        None
    }

    fn source_location(&self) -> Option<&'static panic::Location<'static>> {
        None
    }

    fn request_layout(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        window: &mut Window,
        cx: &mut App,
    ) -> (LayoutId, Self::RequestLayoutState) {
        let layout_id = self.request_layout(window, cx);
        (layout_id, ())
    }

    fn prepaint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _: Bounds<Pixels>,
        _: &mut Self::RequestLayoutState,
        window: &mut Window,
        cx: &mut App,
    ) {
        self.prepaint(window, cx);
    }

    fn paint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _: Bounds<Pixels>,
        _: &mut Self::RequestLayoutState,
        _: &mut Self::PrepaintState,
        window: &mut Window,
        cx: &mut App,
    ) {
        self.paint(window, cx);
    }
}

impl IntoElement for AnyElement {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }

    fn into_any_element(self) -> AnyElement {
        self
    }
}

/// The empty element, which renders nothing.
pub struct Empty;

impl IntoElement for Empty {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl Element for Empty {
    type RequestLayoutState = ();
    type PrepaintState = ();

    fn id(&self) -> Option<ElementId> {
        None
    }

    fn source_location(&self) -> Option<&'static panic::Location<'static>> {
        None
    }

    fn request_layout(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _window: &mut Window,
        _cx: &mut App,
    ) -> (LayoutId, Self::RequestLayoutState) {
        unreachable!("Empty uses retained node path")
    }

    fn prepaint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _state: &mut Self::RequestLayoutState,
        _window: &mut Window,
        _cx: &mut App,
    ) {
        unreachable!("Empty uses retained node path")
    }

    fn paint(
        &mut self,
        _id: Option<&GlobalElementId>,
        _inspector_id: Option<&InspectorElementId>,
        _bounds: Bounds<Pixels>,
        _request_layout: &mut Self::RequestLayoutState,
        _prepaint: &mut Self::PrepaintState,
        _window: &mut Window,
        _cx: &mut App,
    ) {
        unreachable!("Empty uses retained node path")
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<crate::render_node::EmptyNode>())
    }

    fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>> {
        Some(Box::new(crate::render_node::EmptyNode))
    }

    fn update_render_node(
        &mut self,
        _node: &mut dyn crate::RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        Some(UpdateResult::UNCHANGED)
    }
}

/// Helper trait for computing element hashes with common patterns.
/// Reduces boilerplate when implementing fiber hash methods for elements.
pub trait ElementHashing {
    /// Hash an interactivity's base, hover, focus, and active styles for layout.
    /// This is used by elements that have interactive styling like Div and Svg.
    fn hash_interactivity_layout(interactivity: &crate::Interactivity) -> u64 {
        use collections::FxHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = FxHasher::default();
        interactivity.base_style.layout_hash().hash(&mut hasher);
        if let Some(ref s) = interactivity.hover_style {
            s.layout_hash().hash(&mut hasher);
        }
        if let Some(ref s) = interactivity.focus_style {
            s.layout_hash().hash(&mut hasher);
        }
        if let Some(ref s) = interactivity.in_focus_style {
            s.layout_hash().hash(&mut hasher);
        }
        if let Some(ref s) = interactivity.focus_visible_style {
            s.layout_hash().hash(&mut hasher);
        }
        if let Some(ref s) = interactivity.active_style {
            s.layout_hash().hash(&mut hasher);
        }
        hasher.finish()
    }

    /// Hash an interactivity's base, hover, focus, and active styles for painting.
    /// Includes element_id since it may affect paint behavior (e.g., focus rings).
    fn hash_interactivity_paint(interactivity: &crate::Interactivity) -> u64 {
        use collections::FxHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = FxHasher::default();
        interactivity.element_id.hash(&mut hasher);
        interactivity.base_style.paint_hash().hash(&mut hasher);
        if let Some(ref s) = interactivity.hover_style {
            s.paint_hash().hash(&mut hasher);
        }
        if let Some(ref s) = interactivity.focus_style {
            s.paint_hash().hash(&mut hasher);
        }
        if let Some(ref s) = interactivity.in_focus_style {
            s.paint_hash().hash(&mut hasher);
        }
        if let Some(ref s) = interactivity.focus_visible_style {
            s.paint_hash().hash(&mut hasher);
        }
        if let Some(ref s) = interactivity.active_style {
            s.paint_hash().hash(&mut hasher);
        }
        hasher.finish()
    }

    /// Convenience function to hash multiple values into a single u64.
    /// Useful for implementing content_hash, layout_hash, and paint_hash.
    fn hash_values<F>(f: F) -> u64
    where
        F: FnOnce(&mut collections::FxHasher),
    {
        use collections::FxHasher;
        use std::hash::Hasher;
        let mut hasher = FxHasher::default();
        f(&mut hasher);
        hasher.finish()
    }
}
