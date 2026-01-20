use super::{DrawPhase, Window, WindowInvalidator};
use crate::fiber::FiberRuntimeHandle;
use crate::{
    AnyElement, AnyTooltip, App, Bounds, ContentMask, CursorStyle, DispatchPhase,
    FocusHandle, GlobalElementId, Hitbox, HitboxBehavior, InputHandler, KeyContext, KeyEvent,
    ModifiersChangedEvent, MouseEvent, Pixels, Point,
};
use std::any::Any;
use std::any::TypeId;
use std::cell::RefCell;
use std::rc::Rc;

pub(super) struct EventPhaseScope {
    invalidator: WindowInvalidator,
    previous: DrawPhase,
}

impl EventPhaseScope {
    pub(super) fn new(invalidator: WindowInvalidator) -> Self {
        let previous = invalidator.phase();
        invalidator.set_phase(DrawPhase::Event);
        Self {
            invalidator,
            previous,
        }
    }

    pub(super) fn reassert(&self) {
        self.invalidator.set_phase(DrawPhase::Event);
    }
}

impl Drop for EventPhaseScope {
    fn drop(&mut self) {
        self.invalidator.set_phase(self.previous);
    }
}

pub(crate) struct PrepaintCx<'a> {
    pub(super) window: &'a mut Window,
}

impl<'a> PrepaintCx<'a> {
    pub(crate) fn new(window: &'a mut Window) -> Self {
        Self { window }
    }

    pub(crate) fn element_offset(&self) -> Point<Pixels> {
        self.window.transform_stack.local_offset()
    }

    pub(crate) fn content_mask(&self) -> ContentMask<Pixels> {
        self.window
            .content_mask_stack
            .last()
            .cloned()
            .unwrap_or_else(|| ContentMask {
                bounds: Bounds {
                    origin: Point::default(),
                    size: self.window.viewport_size,
                },
            })
    }

    pub(crate) fn with_content_mask<R>(
        &mut self,
        mask: Option<ContentMask<Pixels>>,
        f: impl FnOnce(&mut Window) -> R,
    ) -> R {
        self.window.with_content_mask(mask, f)
    }

    pub(crate) fn with_absolute_element_offset<R>(
        &mut self,
        offset: Point<Pixels>,
        f: impl FnOnce(&mut Window) -> R,
    ) -> R {
        let current = self.window.transform_stack.local_offset();
        let delta = offset - current;
        self.window.transform_stack.push_offset(delta);
        let result = f(self.window);
        self.window.transform_stack.pop_offset(delta);
        result
    }

    pub(crate) fn prepaint_fiber_tree(&mut self, root: GlobalElementId, cx: &mut App) {
        self.window.fibers().prepaint_fiber_tree(root, cx);
    }

    pub(crate) fn collect_deferred_draw_keys(&mut self) -> Vec<crate::fiber::DeferredDrawKey> {
        self.window.fibers().collect_deferred_draw_keys()
    }

    pub(crate) fn prepaint_deferred_draws(
        &mut self,
        deferred_draws: &[crate::fiber::DeferredDrawKey],
        cx: &mut App,
    ) {
        self.window
            .fibers()
            .prepaint_deferred_draws(deferred_draws, cx);
    }
}

pub(crate) struct PaintCx<'a> {
    pub(super) window: &'a mut Window,
}

impl<'a> PaintCx<'a> {
    pub(crate) fn new(window: &'a mut Window) -> Self {
        Self { window }
    }

    pub(crate) fn content_mask(&self) -> ContentMask<Pixels> {
        self.window
            .content_mask_stack
            .last()
            .cloned()
            .unwrap_or_else(|| ContentMask {
                bounds: Bounds {
                    origin: Point::default(),
                    size: self.window.viewport_size,
                },
            })
    }

    pub(crate) fn with_content_mask<R>(
        &mut self,
        mask: Option<ContentMask<Pixels>>,
        f: impl FnOnce(&mut Window) -> R,
    ) -> R {
        self.window.with_content_mask(mask, f)
    }

    pub(crate) fn with_absolute_element_offset<R>(
        &mut self,
        offset: Point<Pixels>,
        f: impl FnOnce(&mut Window) -> R,
    ) -> R {
        let current = self.window.transform_stack.local_offset();
        let delta = offset - current;
        self.window.transform_stack.push_offset(delta);
        let result = f(self.window);
        self.window.transform_stack.pop_offset(delta);
        result
    }

    pub(crate) fn paint_fiber_tree(&mut self, root: GlobalElementId, cx: &mut App) {
        self.window.fibers().paint_fiber_tree(root, cx);
    }

    pub(crate) fn paint_deferred_draws(
        &mut self,
        deferred_draws: &[crate::fiber::DeferredDrawKey],
        cx: &mut App,
    ) {
        self.window
            .fibers()
            .paint_deferred_draws(deferred_draws, cx);
    }
}

pub(crate) struct EventCx<'a> {
    handle: FiberRuntimeHandle<'a>,
}

impl<'a> EventCx<'a> {
    pub(crate) fn new(window: &'a mut Window) -> Self {
        Self {
            handle: FiberRuntimeHandle { window },
        }
    }

    pub(crate) fn dispatch_mouse_event(&mut self, event: &dyn Any, cx: &mut App) {
        self.handle.dispatch_mouse_event(event, cx);
    }
}

pub(crate) struct FiberCx<'a> {
    handle: FiberRuntimeHandle<'a>,
    fiber_id: GlobalElementId,
}

impl<'a> FiberCx<'a> {
    fn for_fiber(window: &'a mut Window, fiber_id: GlobalElementId) -> Self {
        Self {
            handle: FiberRuntimeHandle { window },
            fiber_id,
        }
    }

    pub(crate) fn set_key_context(&mut self, context: KeyContext) {
        self.handle
            .register_key_context_for_fiber(self.fiber_id, context);
    }

    pub(crate) fn set_focus_handle(&mut self, focus_handle: &crate::FocusHandle) {
        self.handle
            .register_focus_handle_for_fiber(self.fiber_id, focus_handle);
    }

    pub(crate) fn insert_hitbox(
        &mut self,
        bounds: Bounds<Pixels>,
        behavior: HitboxBehavior,
    ) -> Hitbox {
        self.handle
            .insert_hitbox_with_fiber(bounds, behavior, self.fiber_id)
    }

    pub(crate) fn on_mouse_event<Event: MouseEvent>(
        &mut self,
        mut listener: impl FnMut(&Event, DispatchPhase, &mut Window, &mut App) + 'static,
    ) {
        self.handle.register_any_mouse_listener_for_fiber(
            self.fiber_id,
            Rc::new(RefCell::new(
                move |event: &dyn Any, phase: DispatchPhase, window: &mut Window, cx: &mut App| {
                    if let Some(event) = event.downcast_ref() {
                        listener(event, phase, window, cx)
                    }
                },
            )),
        );
    }

    pub(crate) fn on_key_event<Event: KeyEvent>(
        &mut self,
        listener: impl Fn(&Event, DispatchPhase, &mut Window, &mut App) + 'static,
    ) {
        self.handle.register_key_listener_for_fiber(
            self.fiber_id,
            Rc::new(
                move |event: &dyn Any, phase, window: &mut Window, cx: &mut App| {
                    if let Some(event) = event.downcast_ref::<Event>() {
                        listener(event, phase, window, cx)
                    }
                },
            ),
        );
    }

    pub(crate) fn on_modifiers_changed(
        &mut self,
        listener: impl Fn(&ModifiersChangedEvent, &mut Window, &mut App) + 'static,
    ) {
        self.handle.register_modifiers_changed_listener_for_fiber(
            self.fiber_id,
            Rc::new(
                move |event: &ModifiersChangedEvent, window: &mut Window, cx: &mut App| {
                    listener(event, window, cx)
                },
            ),
        );
    }

    pub(crate) fn on_action(
        &mut self,
        action_type: TypeId,
        listener: impl Fn(&dyn Any, DispatchPhase, &mut Window, &mut App) + 'static,
    ) {
        let effects = self
            .handle
            .window
            .register_fiber_effects(&self.fiber_id)
            .expect("on_action requires a valid fiber");
        effects
            .action_listeners
            .push((action_type, Rc::new(listener)));
    }

    pub(crate) fn set_window_cursor_style(&mut self, style: CursorStyle) {
        self.handle
            .set_window_cursor_style_for_fiber(self.fiber_id, style);
    }

    pub(crate) fn set_tooltip(&mut self, tooltip: AnyTooltip) -> crate::TooltipId {
        self.handle.set_tooltip_for_fiber(self.fiber_id, tooltip)
    }

    pub(crate) fn register_tab_stop(&mut self, focus_handle: &FocusHandle, tab_index: isize) {
        self.handle
            .register_tab_stop_for_fiber(self.fiber_id, focus_handle, tab_index);
    }

    pub(crate) fn register_tab_stop_handle(&mut self, focus_handle: &FocusHandle) {
        self.handle
            .register_tab_stop_handle_for_fiber(self.fiber_id, focus_handle);
    }

    pub(crate) fn defer_draw(
        &mut self,
        element: AnyElement,
        absolute_offset: Point<Pixels>,
        priority: usize,
        callsite: &'static core::panic::Location<'static>,
    ) {
        self.handle.defer_draw_for_fiber(
            self.fiber_id,
            element,
            absolute_offset,
            priority,
            crate::AvailableSpace::min_size(),
            0,
            callsite,
        );
    }

    pub(crate) fn handle_input(
        &mut self,
        focus_handle: &FocusHandle,
        input_handler: impl InputHandler,
        cx: &App,
    ) {
        self.handle
            .handle_input_for_fiber(self.fiber_id, focus_handle, input_handler, cx);
    }
}

impl Window {
    pub(crate) fn with_fiber_cx<R>(&mut self, f: impl FnOnce(&mut FiberCx<'_>) -> R) -> R {
        let fiber_id = self
            .current_fiber_id()
            .expect("this operation requires an active fiber");
        let mut cx = FiberCx::for_fiber(self, fiber_id);
        f(&mut cx)
    }

    pub(crate) fn with_fiber_cx_for<R>(
        &mut self,
        fiber_id: GlobalElementId,
        f: impl FnOnce(&mut FiberCx<'_>) -> R,
    ) -> R {
        let mut cx = FiberCx::for_fiber(self, fiber_id);
        f(&mut cx)
    }
}
