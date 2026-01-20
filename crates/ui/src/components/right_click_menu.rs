use std::{any::TypeId, cell::RefCell, rc::Rc};

use gpui::{
    AnyElement, App, Bounds, CallbackSlot, ConditionalSlot, Corner, DismissEvent, DispatchPhase,
    Element, ElementId, ElementImpl, Entity, Focusable as _, GlobalElementId, Hitbox,
    HitboxBehavior, InteractiveElement, IntoElement, LayoutCtx, LayoutFrame, LayoutId, ManagedView,
    MouseButton, PaintCtx, PaintFrame, ParentElement, Pixels, Point, PrepaintCtx, PrepaintFrame,
    RenderNode, UpdateResult, Window, anchored, div, px,
};
use smallvec::SmallVec;

#[derive(Element)]
pub struct RightClickMenu<M: ManagedView> {
    id: ElementId,
    child_builder: Option<Box<dyn FnOnce(bool, &mut Window, &mut App) -> AnyElement + 'static>>,
    menu_builder: Option<Rc<dyn Fn(&mut Window, &mut App) -> Entity<M> + 'static>>,
    anchor: Option<Corner>,
    attach: Option<Corner>,
}

impl<M: ManagedView> RightClickMenu<M> {
    pub fn menu(mut self, f: impl Fn(&mut Window, &mut App) -> Entity<M> + 'static) -> Self {
        self.menu_builder = Some(Rc::new(f));
        self
    }

    pub fn trigger<F, E>(mut self, e: F) -> Self
    where
        F: FnOnce(bool, &mut Window, &mut App) -> E + 'static,
        E: IntoElement + 'static,
    {
        self.child_builder = Some(Box::new(move |is_menu_active, window, cx| {
            e(is_menu_active, window, cx).into_any_element()
        }));
        self
    }

    /// anchor defines which corner of the menu to anchor to the attachment point
    /// (by default the cursor position, but see attach)
    pub fn anchor(mut self, anchor: Corner) -> Self {
        self.anchor = Some(anchor);
        self
    }

    /// attach defines which corner of the handle to attach the menu's anchor to
    pub fn attach(mut self, attach: Corner) -> Self {
        self.attach = Some(attach);
        self
    }
}

/// Creates a [`RightClickMenu`]
pub fn right_click_menu<M: ManagedView>(id: impl Into<ElementId>) -> RightClickMenu<M> {
    RightClickMenu {
        id: id.into(),
        child_builder: None,
        menu_builder: None,
        anchor: None,
        attach: None,
    }
}

pub struct MenuHandleElementState<M> {
    menu: Rc<RefCell<Option<Entity<M>>>>,
    position: Rc<RefCell<Point<Pixels>>>,
}

impl<M> Clone for MenuHandleElementState<M> {
    fn clone(&self) -> Self {
        Self {
            menu: Rc::clone(&self.menu),
            position: Rc::clone(&self.position),
        }
    }
}

impl<M> Default for MenuHandleElementState<M> {
    fn default() -> Self {
        Self {
            menu: Rc::default(),
            position: Rc::default(),
        }
    }
}

/// Result from building children in layout phase.
struct RightClickBuildResult {
    child: Option<AnyElement>,
    menu: Option<AnyElement>,
    open_menu_callback: Option<OpenMenuCallback>,
}

/// Type-erased callback for opening the menu on right-click.
type OpenMenuCallback = Box<
    dyn Fn(
        Point<Pixels>,
        Option<Bounds<Pixels>>,
        &mut Window,
        &mut App,
    ) + 'static,
>;

/// Type-erased callback for building children during layout.
type BuildChildrenFn =
    dyn FnOnce(&mut Window, &mut App, GlobalElementId) -> RightClickBuildResult + 'static;

/// Retained render node for RightClickMenu.
struct RightClickMenuNode {
    anchor: Option<Corner>,
    attach: Option<Corner>,

    build_callback: CallbackSlot<BuildChildrenFn>,

    pending_child: Option<AnyElement>,
    pending_menu: Option<AnyElement>,

    open_menu_callback: Option<OpenMenuCallback>,
    child_layout_id: Option<LayoutId>,
    hitbox: Option<Hitbox>,
}

impl RightClickMenuNode {
    fn new(anchor: Option<Corner>, attach: Option<Corner>) -> Self {
        Self {
            anchor,
            attach,
            build_callback: CallbackSlot::new(),
            pending_child: None,
            pending_menu: None,
            open_menu_callback: None,
            child_layout_id: None,
            hitbox: None,
        }
    }
}

impl RenderNode for RightClickMenuNode {
    fn layout_begin(&mut self, ctx: &mut LayoutCtx) -> LayoutFrame {
        if let Some(build) = self.build_callback.take() {
            let result = build(ctx.window, ctx.cx, ctx.fiber_id.clone());
            self.pending_child = result.child;
            self.pending_menu = result.menu;
            self.open_menu_callback = result.open_menu_callback;
        }

        LayoutFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn layout_end(&mut self, _ctx: &mut LayoutCtx, _frame: LayoutFrame) {}

    fn conditional_slots(&mut self, _fiber_id: GlobalElementId) -> SmallVec<[ConditionalSlot; 4]> {
        let mut slots = SmallVec::new();

        // Slot 0: child (always present)
        if let Some(child) = self.pending_child.take() {
            slots.push(ConditionalSlot::active(0, move || child));
        } else {
            slots.push(ConditionalSlot {
                slot_index: 0,
                active: true,
                element_factory: None,
            });
        }

        // Slot 1: menu (conditional)
        if let Some(menu) = self.pending_menu.take() {
            slots.push(ConditionalSlot::active(1, move || menu));
        } else {
            slots.push(ConditionalSlot {
                slot_index: 1,
                active: false,
                element_factory: None,
            });
        }

        slots
    }

    fn prepaint_begin(&mut self, ctx: &mut PrepaintCtx) -> PrepaintFrame {
        self.hitbox = Some(ctx.insert_hitbox(ctx.bounds, HitboxBehavior::Normal));

        PrepaintFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn prepaint_end(&mut self, _ctx: &mut PrepaintCtx, _frame: PrepaintFrame) {}

    fn paint_begin(&mut self, ctx: &mut PaintCtx) -> PaintFrame {
        let Some(open_menu_callback) = self.open_menu_callback.take() else {
            return PaintFrame {
                handled: true,
                ..Default::default()
            };
        };

        if self.hitbox.is_none() {
            return PaintFrame {
                handled: true,
                ..Default::default()
            };
        }

        let child_layout_id = self.child_layout_id;
        let attach = self.attach;

        // Use the efficient typed listener path - the listener is only called
        // when this fiber's hitbox is hit, so no manual is_hovered check needed.
        ctx.on_mouse_down(move |event, phase, _hitbox, window, cx| {
            if phase == DispatchPhase::Bubble && event.button == MouseButton::Right {
                cx.stop_propagation();
                window.prevent_default();

                let child_bounds = child_layout_id.map(|id| window.layout_bounds(id));
                let position = if let Some(child_bounds) = child_bounds {
                    if let Some(attach) = attach {
                        child_bounds.corner(attach)
                    } else {
                        window.mouse_position()
                    }
                } else {
                    window.mouse_position()
                };

                open_menu_callback(position, child_bounds, window, cx);
            }
        });

        PaintFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn paint_end(&mut self, _ctx: &mut PaintCtx, _frame: PaintFrame) {}
}

impl<M: ManagedView> ElementImpl for RightClickMenu<M> {
    fn id(&self) -> Option<ElementId> {
        Some(self.id.clone())
    }

    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        let node = RightClickMenuNode::new(self.anchor, self.attach);

        let child_builder = self.child_builder.take();
        let menu_builder = self.menu_builder.clone();
        let anchor = self.anchor;

        node.build_callback.deposit(Box::new(
            move |window: &mut Window, cx: &mut App, fiber_id: GlobalElementId| {
                build_children::<M>(child_builder, menu_builder, anchor, window, cx, fiber_id)
            },
        ));

        Some(Box::new(node))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<RightClickMenuNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let node = node.as_any_mut().downcast_mut::<RightClickMenuNode>()?;

        node.anchor = self.anchor;
        node.attach = self.attach;

        let child_builder = self.child_builder.take();
        let menu_builder = self.menu_builder.clone();
        let anchor = self.anchor;

        node.build_callback.deposit(Box::new(
            move |window: &mut Window, cx: &mut App, fiber_id: GlobalElementId| {
                build_children::<M>(child_builder, menu_builder, anchor, window, cx, fiber_id)
            },
        ));

        Some(UpdateResult::LAYOUT_CHANGED)
    }
}

fn build_children<M: ManagedView>(
    child_builder: Option<Box<dyn FnOnce(bool, &mut Window, &mut App) -> AnyElement + 'static>>,
    menu_builder: Option<Rc<dyn Fn(&mut Window, &mut App) -> Entity<M> + 'static>>,
    anchor: Option<Corner>,
    window: &mut Window,
    cx: &mut App,
    fiber_id: GlobalElementId,
) -> RightClickBuildResult {
    window.with_optional_element_state::<MenuHandleElementState<M>, _>(
        Some(&fiber_id),
        |element_state, window| {
            let element_state = element_state.unwrap().unwrap_or_default();

            // Build child element
            let child = child_builder.map(|builder| {
                let is_menu_active = element_state.menu.borrow().is_some();
                (builder)(is_menu_active, window, cx)
            });

            // Build menu element if menu is open
            let menu = element_state.menu.borrow().as_ref().map(|menu_entity| {
                let mut anchored_element = anchored().snap_to_window_with_margin(px(8.));
                if let Some(anchor) = anchor {
                    anchored_element = anchored_element.anchor(anchor);
                }
                anchored_element = anchored_element.position(*element_state.position.borrow());

                anchored_element
                    .child(div().occlude().child(menu_entity.clone()))
                    .z_index(1)
                    .into_any_element()
            });

            // Create the open menu callback
            let open_menu_callback: Option<OpenMenuCallback> = menu_builder.map(|builder| {
                let menu_state = element_state.menu.clone();
                let position_state = element_state.position.clone();

                Box::new(
                    move |position: Point<Pixels>,
                          _child_bounds: Option<Bounds<Pixels>>,
                          window: &mut Window,
                          cx: &mut App| {
                        let new_menu = (builder)(window, cx);
                        let menu_state_clone = menu_state.clone();
                        let previous_focus_handle = window.focused(cx);

                        window
                            .subscribe(&new_menu, cx, move |modal, _: &DismissEvent, window, cx| {
                                if modal.focus_handle(cx).contains_focused(window, cx)
                                    && let Some(previous_focus_handle) =
                                        previous_focus_handle.as_ref()
                                {
                                    window.focus(previous_focus_handle, cx);
                                }
                                *menu_state_clone.borrow_mut() = None;
                                window.refresh();
                            })
                            .detach();

                        let focus_handle = new_menu.focus_handle(cx);
                        window.on_next_frame(move |window, _cx| {
                            window.on_next_frame(move |window, cx| {
                                window.focus(&focus_handle, cx);
                            });
                        });

                        *menu_state.borrow_mut() = Some(new_menu);
                        *position_state.borrow_mut() = position;
                        window.refresh();
                    },
                ) as OpenMenuCallback
            });

            let result = RightClickBuildResult {
                child,
                menu,
                open_menu_callback,
            };

            (result, Some(element_state))
        },
    )
}

impl<M: ManagedView> IntoElement for RightClickMenu<M> {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}
