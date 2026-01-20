use std::{any::TypeId, cell::RefCell, rc::Rc};

use gpui::{
    AnyElement, AnyView, App, Bounds, CallbackSlot, ConditionalSlot, Corner, DismissEvent,
    DispatchPhase, ElementId, ElementImpl, Entity, Focusable as _,
    GlobalElementId, Hitbox, HitboxBehavior, InteractiveElement, IntrinsicSizeResult, IntoElement,
    LayoutCtx, LayoutFrame, ManagedView, PaintCtx, PaintFrame, ParentElement, Pixels, Point,
    PrepaintCtx, PrepaintFrame, RenderNode, SizingCtx, Style, TaffyStyle, ToTaffy, UpdateResult,
    Window, anchored, div, point, prelude::FluentBuilder, px, relative, size,
};
use smallvec::SmallVec;

use crate::prelude::*;

pub trait PopoverTrigger: IntoElement + Clickable + Toggleable + 'static {}

impl<T: IntoElement + Clickable + Toggleable + 'static> PopoverTrigger for T {}

impl<T: Clickable + gpui::IntoElement + 'static> Clickable for gpui::AnimationElement<T> {
    fn on_click(
        self,
        handler: impl Fn(&gpui::ClickEvent, &mut Window, &mut App) + 'static,
    ) -> Self {
        self.map_element(|e| e.on_click(handler))
    }

    fn cursor_style(self, cursor_style: gpui::CursorStyle) -> Self {
        self.map_element(|e| e.cursor_style(cursor_style))
    }
}

impl<T: Toggleable + gpui::IntoElement + 'static> Toggleable for gpui::AnimationElement<T> {
    fn toggle_state(self, selected: bool) -> Self {
        self.map_element(|e| e.toggle_state(selected))
    }
}

pub struct PopoverMenuHandle<M>(Rc<RefCell<Option<PopoverMenuHandleState<M>>>>);

impl<M> Clone for PopoverMenuHandle<M> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<M> Default for PopoverMenuHandle<M> {
    fn default() -> Self {
        Self(Rc::default())
    }
}

struct PopoverMenuHandleState<M> {
    menu_builder: Rc<dyn Fn(&mut Window, &mut App) -> Option<Entity<M>>>,
    menu: Rc<RefCell<Option<Entity<M>>>>,
    on_open: Option<Rc<dyn Fn(&mut Window, &mut App)>>,
}

impl<M: ManagedView> PopoverMenuHandle<M> {
    pub fn show(&self, window: &mut Window, cx: &mut App) {
        if let Some(state) = self.0.borrow().as_ref() {
            show_menu(
                &state.menu_builder,
                &state.menu,
                state.on_open.clone(),
                window,
                cx,
            );
        }
    }

    pub fn hide(&self, cx: &mut App) {
        if let Some(state) = self.0.borrow().as_ref()
            && let Some(menu) = state.menu.borrow().as_ref()
        {
            menu.update(cx, |_, cx| cx.emit(DismissEvent));
        }
    }

    pub fn toggle(&self, window: &mut Window, cx: &mut App) {
        if let Some(state) = self.0.borrow().as_ref() {
            if state.menu.borrow().is_some() {
                self.hide(cx);
            } else {
                self.show(window, cx);
            }
        }
    }

    pub fn is_deployed(&self) -> bool {
        self.0
            .borrow()
            .as_ref()
            .is_some_and(|state| state.menu.borrow().as_ref().is_some())
    }

    pub fn is_focused(&self, window: &Window, cx: &App) -> bool {
        self.0.borrow().as_ref().is_some_and(|state| {
            state
                .menu
                .borrow()
                .as_ref()
                .is_some_and(|model| model.focus_handle(cx).is_focused(window))
        })
    }

    pub fn refresh_menu(
        &self,
        window: &mut Window,
        cx: &mut App,
        new_menu_builder: Rc<dyn Fn(&mut Window, &mut App) -> Option<Entity<M>>>,
    ) {
        let show_menu = if let Some(state) = self.0.borrow_mut().as_mut() {
            state.menu_builder = new_menu_builder;
            state.menu.borrow().is_some()
        } else {
            false
        };

        if show_menu {
            self.show(window, cx);
        }
    }
}

#[derive(Element)]
pub struct PopoverMenu<M: ManagedView> {
    id: ElementId,
    child_builder: Option<
        Box<
            dyn FnOnce(
                    Rc<RefCell<Option<Entity<M>>>>,
                    Option<Rc<dyn Fn(&mut Window, &mut App) -> Option<Entity<M>> + 'static>>,
                ) -> AnyElement
                + 'static,
        >,
    >,
    menu_builder: Option<Rc<dyn Fn(&mut Window, &mut App) -> Option<Entity<M>> + 'static>>,
    anchor: Corner,
    attach: Option<Corner>,
    offset: Option<Point<Pixels>>,
    trigger_handle: Option<PopoverMenuHandle<M>>,
    on_open: Option<Rc<dyn Fn(&mut Window, &mut App)>>,
    full_width: bool,
}

impl<M: ManagedView> PopoverMenu<M> {
    /// Returns a new [`PopoverMenu`].
    pub fn new(id: impl Into<ElementId>) -> Self {
        Self {
            id: id.into(),
            child_builder: None,
            menu_builder: None,
            anchor: Corner::TopLeft,
            attach: None,
            offset: None,
            trigger_handle: None,
            on_open: None,
            full_width: false,
        }
    }

    pub fn full_width(mut self, full_width: bool) -> Self {
        self.full_width = full_width;
        self
    }

    pub fn menu(
        mut self,
        f: impl Fn(&mut Window, &mut App) -> Option<Entity<M>> + 'static,
    ) -> Self {
        self.menu_builder = Some(Rc::new(f));
        self
    }

    pub fn with_handle(mut self, handle: PopoverMenuHandle<M>) -> Self {
        self.trigger_handle = Some(handle);
        self
    }

    pub fn trigger<T: PopoverTrigger>(mut self, t: T) -> Self {
        let on_open = self.on_open.clone();
        self.child_builder = Some(Box::new(move |menu, builder| {
            let open = menu.borrow().is_some();
            t.toggle_state(open)
                .when_some(builder, |el, builder| {
                    el.on_click(move |_event, window, cx| {
                        show_menu(&builder, &menu, on_open.clone(), window, cx)
                    })
                })
                .into_any_element()
        }));
        self
    }

    /// This method prevents the trigger button tooltip from being seen when the menu is open.
    pub fn trigger_with_tooltip<T: PopoverTrigger + ButtonCommon>(
        mut self,
        t: T,
        tooltip_builder: impl Fn(&mut Window, &mut App) -> AnyView + 'static,
    ) -> Self {
        let on_open = self.on_open.clone();
        self.child_builder = Some(Box::new(move |menu, builder| {
            let open = menu.borrow().is_some();
            t.toggle_state(open)
                .when_some(builder, |el, builder| {
                    el.on_click(move |_, window, cx| {
                        show_menu(&builder, &menu, on_open.clone(), window, cx)
                    })
                    .when(!open, |t| {
                        t.tooltip(move |window, cx| tooltip_builder(window, cx))
                    })
                })
                .into_any_element()
        }));
        self
    }

    /// Defines which corner of the menu to anchor to the attachment point.
    /// By default, it uses the cursor position. Also see the `attach` method.
    pub fn anchor(mut self, anchor: Corner) -> Self {
        self.anchor = anchor;
        self
    }

    /// Defines which corner of the handle to attach the menu's anchor to.
    pub fn attach(mut self, attach: Corner) -> Self {
        self.attach = Some(attach);
        self
    }

    /// Offsets the position of the content by that many pixels.
    pub fn offset(mut self, offset: Point<Pixels>) -> Self {
        self.offset = Some(offset);
        self
    }

    /// Attaches something upon opening the menu.
    pub fn on_open(mut self, on_open: Rc<dyn Fn(&mut Window, &mut App)>) -> Self {
        self.on_open = Some(on_open);
        self
    }
}

fn show_menu<M: ManagedView>(
    builder: &Rc<dyn Fn(&mut Window, &mut App) -> Option<Entity<M>>>,
    menu: &Rc<RefCell<Option<Entity<M>>>>,
    on_open: Option<Rc<dyn Fn(&mut Window, &mut App)>>,
    window: &mut Window,
    cx: &mut App,
) {
    let previous_focus_handle = window.focused(cx);
    let Some(new_menu) = (builder)(window, cx) else {
        return;
    };
    let menu2 = menu.clone();

    window
        .subscribe(&new_menu, cx, move |modal, _: &DismissEvent, window, cx| {
            if modal.focus_handle(cx).contains_focused(window, cx)
                && let Some(previous_focus_handle) = previous_focus_handle.as_ref()
            {
                window.focus(previous_focus_handle, cx);
            }
            *menu2.borrow_mut() = None;
            window.refresh();
        })
        .detach();

    let focus_handle = new_menu.focus_handle(cx);
    window.on_next_frame(move |window, _cx| {
        window.on_next_frame(move |window, cx| {
            window.focus(&focus_handle, cx);
        });
    });
    *menu.borrow_mut() = Some(new_menu);
    window.refresh();

    if let Some(on_open) = on_open {
        on_open(window, cx);
    }
}

pub struct PopoverMenuElementState<M> {
    menu: Rc<RefCell<Option<Entity<M>>>>,
    child_bounds: Option<Bounds<Pixels>>,
}

impl<M> Clone for PopoverMenuElementState<M> {
    fn clone(&self) -> Self {
        Self {
            menu: Rc::clone(&self.menu),
            child_bounds: self.child_bounds,
        }
    }
}

impl<M> Default for PopoverMenuElementState<M> {
    fn default() -> Self {
        Self {
            menu: Rc::default(),
            child_bounds: None,
        }
    }
}

/// Result from building children in layout phase.
struct PopoverBuildResult {
    child: Option<AnyElement>,
    menu: Option<AnyElement>,
    dismiss_callback: Option<Rc<dyn Fn(&mut App)>>,
}

/// Type-erased callback for building children during layout.
type BuildChildrenFn =
    dyn FnOnce(&mut Window, &mut App, GlobalElementId) -> PopoverBuildResult + 'static;

/// Retained render node for PopoverMenu.
struct PopoverMenuNode {
    anchor: Corner,
    attach: Option<Corner>,
    offset: Option<Point<Pixels>>,
    full_width: bool,

    build_callback: CallbackSlot<BuildChildrenFn>,

    pending_child: Option<AnyElement>,
    pending_menu: Option<AnyElement>,

    dismiss_callback: Option<Rc<dyn Fn(&mut App)>>,
    hitbox: Option<Hitbox>,
}

impl PopoverMenuNode {
    fn new(anchor: Corner, attach: Option<Corner>, offset: Option<Point<Pixels>>, full_width: bool) -> Self {
        Self {
            anchor,
            attach,
            offset,
            full_width,
            build_callback: CallbackSlot::new(),
            pending_child: None,
            pending_menu: None,
            dismiss_callback: None,
            hitbox: None,
        }
    }
}

impl RenderNode for PopoverMenuNode {
    fn taffy_style(&self, rem_size: Pixels, scale_factor: f32) -> TaffyStyle {
        if self.full_width {
            let style = Style {
                size: size(relative(1.).into(), gpui::Length::Auto),
                ..Style::default()
            };
            style.to_taffy(rem_size, scale_factor)
        } else {
            TaffyStyle::default()
        }
    }

    fn compute_intrinsic_size(&mut self, _ctx: &mut SizingCtx) -> IntrinsicSizeResult {
        IntrinsicSizeResult {
            size: gpui::IntrinsicSize::default(),
            input: gpui::SizingInput::default(),
        }
    }

    fn layout_begin(&mut self, ctx: &mut LayoutCtx) -> LayoutFrame {
        if let Some(build) = self.build_callback.take() {
            let result = build(ctx.window, ctx.cx, ctx.fiber_id.clone());
            self.pending_child = result.child;
            self.pending_menu = result.menu;
            self.dismiss_callback = result.dismiss_callback;
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
        // If menu is open and we have a hitbox, register dismiss-on-click handler
        let Some(dismiss_callback) = self.dismiss_callback.clone() else {
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

        // Use the efficient typed listener path - the listener is only called
        // when this fiber's hitbox is hit, so no manual is_hovered check needed.
        ctx.on_mouse_down(move |_event, phase, _hitbox, _window, cx| {
            if phase == DispatchPhase::Bubble {
                (dismiss_callback)(cx);
                cx.stop_propagation();
            }
        });

        PaintFrame {
            handled: true,
            ..Default::default()
        }
    }

    fn paint_end(&mut self, _ctx: &mut PaintCtx, _frame: PaintFrame) {}
}

impl<M: ManagedView> ElementImpl for PopoverMenu<M> {
    fn id(&self) -> Option<ElementId> {
        Some(self.id.clone())
    }

    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        let node = PopoverMenuNode::new(self.anchor, self.attach, self.offset, self.full_width);

        let child_builder = self.child_builder.take();
        let menu_builder = self.menu_builder.clone();
        let trigger_handle = self.trigger_handle.take();
        let on_open = self.on_open.clone();
        let anchor = self.anchor;
        let attach = self.attach;
        let offset = self.offset;

        node.build_callback.deposit(Box::new(
            move |window: &mut Window, cx: &mut App, fiber_id: GlobalElementId| {
                build_children::<M>(
                    child_builder,
                    menu_builder,
                    trigger_handle,
                    on_open,
                    anchor,
                    attach,
                    offset,
                    window,
                    cx,
                    fiber_id,
                )
            },
        ));

        Some(Box::new(node))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<PopoverMenuNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let node = node.as_any_mut().downcast_mut::<PopoverMenuNode>()?;

        node.anchor = self.anchor;
        node.attach = self.attach;
        node.offset = self.offset;
        node.full_width = self.full_width;

        let child_builder = self.child_builder.take();
        let menu_builder = self.menu_builder.clone();
        let trigger_handle = self.trigger_handle.take();
        let on_open = self.on_open.clone();
        let anchor = self.anchor;
        let attach = self.attach;
        let offset = self.offset;

        node.build_callback.deposit(Box::new(
            move |window: &mut Window, cx: &mut App, fiber_id: GlobalElementId| {
                build_children::<M>(
                    child_builder,
                    menu_builder,
                    trigger_handle,
                    on_open,
                    anchor,
                    attach,
                    offset,
                    window,
                    cx,
                    fiber_id,
                )
            },
        ));

        Some(UpdateResult::LAYOUT_CHANGED)
    }
}

fn build_children<M: ManagedView>(
    child_builder: Option<
        Box<
            dyn FnOnce(
                    Rc<RefCell<Option<Entity<M>>>>,
                    Option<Rc<dyn Fn(&mut Window, &mut App) -> Option<Entity<M>> + 'static>>,
                ) -> AnyElement
                + 'static,
        >,
    >,
    menu_builder: Option<Rc<dyn Fn(&mut Window, &mut App) -> Option<Entity<M>> + 'static>>,
    trigger_handle: Option<PopoverMenuHandle<M>>,
    on_open: Option<Rc<dyn Fn(&mut Window, &mut App)>>,
    anchor: Corner,
    attach: Option<Corner>,
    offset: Option<Point<Pixels>>,
    window: &mut Window,
    _cx: &mut App,
    fiber_id: GlobalElementId,
) -> PopoverBuildResult {
    window.with_optional_element_state::<PopoverMenuElementState<M>, _>(
        Some(&fiber_id),
        |element_state, window| {
            let element_state = element_state.unwrap().unwrap_or_default();

            // Set up trigger handle if provided
            if let Some(trigger_handle) = trigger_handle {
                if let Some(menu_builder) = menu_builder.clone() {
                    *trigger_handle.0.borrow_mut() = Some(PopoverMenuHandleState {
                        menu_builder,
                        menu: element_state.menu.clone(),
                        on_open: on_open.clone(),
                    });
                }
            }

            // Build child element
            let child = child_builder.map(|builder| {
                (builder)(element_state.menu.clone(), menu_builder.clone())
            });

            // Build menu element if menu is open
            let menu = element_state.menu.borrow().as_ref().map(|menu_entity| {
                let resolved_attach = attach.unwrap_or(match anchor {
                    Corner::TopLeft => Corner::BottomLeft,
                    Corner::TopRight => Corner::BottomRight,
                    Corner::BottomLeft => Corner::TopLeft,
                    Corner::BottomRight => Corner::TopRight,
                });

                let resolved_offset = offset.unwrap_or_else(|| {
                    let offset_px = rems_from_px(5.) * window.rem_size();
                    match anchor {
                        Corner::TopRight | Corner::BottomRight => point(offset_px, px(0.)),
                        Corner::TopLeft | Corner::BottomLeft => point(-offset_px, px(0.)),
                    }
                });

                let mut anchored_element = anchored()
                    .snap_to_window_with_margin(px(8.))
                    .anchor(anchor)
                    .offset(resolved_offset);

                if let Some(child_bounds) = element_state.child_bounds {
                    anchored_element = anchored_element
                        .position(child_bounds.corner(resolved_attach) + resolved_offset);
                }

                anchored_element
                    .child(div().occlude().child(menu_entity.clone()))
                    .z_index(1)
                    .into_any_element()
            });

            // Create type-erased dismiss callback if menu is open
            let dismiss_callback: Option<Rc<dyn Fn(&mut App)>> =
                element_state.menu.borrow().as_ref().map(|menu_entity| {
                    let menu = menu_entity.clone();
                    Rc::new(move |cx: &mut App| {
                        menu.update(cx, |_, cx| cx.emit(DismissEvent));
                    }) as Rc<dyn Fn(&mut App)>
                });

            let result = PopoverBuildResult {
                child,
                menu,
                dismiss_callback,
            };

            (result, Some(element_state))
        },
    )
}

impl<M: ManagedView> IntoElement for PopoverMenu<M> {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}
