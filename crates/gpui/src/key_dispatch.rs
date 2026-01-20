//! KeyDispatch is where GPUI deals with binding actions to key events.
//!
//! The key pieces to making a key binding work are to define an action,
//! implement a method that takes that action as a type parameter,
//! and then to register the action during render on a focused node
//! with a keymap context:
//!
//! ```ignore
//! actions!(editor,[Undo, Redo]);
//!
//! impl Editor {
//!   fn undo(&mut self, _: &Undo, _window: &mut Window, _cx: &mut Context<Self>) { ... }
//!   fn redo(&mut self, _: &Redo, _window: &mut Window, _cx: &mut Context<Self>) { ... }
//! }
//!
//! impl Render for Editor {
//!   fn render(&mut self, window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
//!     div()
//!       .track_focus(&self.focus_handle(cx))
//!       .key_context("Editor")
//!       .on_action(cx.listener(Editor::undo))
//!       .on_action(cx.listener(Editor::redo))
//!     ...
//!    }
//! }
//!```
//!
//! The keybindings themselves are managed independently by calling cx.bind_keys().
//! (Though mostly when developing Zed itself, you just need to add a new line to
//!  assets/keymaps/default-{platform}.json).
//!
//! ```ignore
//! cx.bind_keys([
//!   KeyBinding::new("cmd-z", Editor::undo, Some("Editor")),
//!   KeyBinding::new("cmd-shift-z", Editor::redo, Some("Editor")),
//! ])
//! ```
//!
//! With all of this in place, GPUI will ensure that if you have an Editor that contains
//! the focus, hitting cmd-z will Undo.
//!
//! In real apps, it is a little more complicated than this, because typically you have
//! several nested views that each register keyboard handlers. In this case action matching
//! bubbles up from the bottom. For example in Zed, the Workspace is the top-level view, which contains Pane's, which contain Editors. If there are conflicting keybindings defined
//! then the Editor's bindings take precedence over the Pane's bindings, which take precedence over the Workspace.
//!
//! In GPUI, keybindings are not limited to just single keystrokes, you can define
//! sequences by separating the keys with a space:
//!
//!  KeyBinding::new("cmd-k left", pane::SplitLeft, Some("Pane"))

use crate::{Action, KeyBinding, KeyContext, Keymap, Keystroke};
use smallvec::SmallVec;
use std::{cell::RefCell, rc::Rc};

/// Key dispatch helper that evaluates bindings using a keymap and context stack.
pub(crate) struct KeyDispatcher {
    keymap: Rc<RefCell<Keymap>>,
}

impl KeyDispatcher {
    pub fn new(keymap: Rc<RefCell<Keymap>>) -> Self {
        Self { keymap }
    }

    /// Returns key bindings that invoke an action on the currently focused element. Bindings are
    /// returned in the order they were added. For display, the last binding should take precedence.
    ///
    /// Bindings are only included if they are the highest precedence match for their keystrokes, so
    /// shadowed bindings are not included.
    pub fn bindings_for_action(
        &self,
        action: &dyn Action,
        context_stack: &[KeyContext],
    ) -> Vec<KeyBinding> {
        let keymap = self.keymap.borrow();
        keymap
            .bindings_for_action(action)
            .filter(|binding| {
                Self::binding_matches_predicate_and_not_shadowed(&keymap, binding, context_stack)
            })
            .cloned()
            .collect()
    }

    /// Returns the highest precedence binding for the given action and context stack. This is the
    /// same as the last result of `bindings_for_action`, but more efficient than getting all bindings.
    pub fn highest_precedence_binding_for_action(
        &self,
        action: &dyn Action,
        context_stack: &[KeyContext],
    ) -> Option<KeyBinding> {
        let keymap = self.keymap.borrow();
        keymap
            .bindings_for_action(action)
            .rev()
            .find(|binding| {
                Self::binding_matches_predicate_and_not_shadowed(&keymap, binding, context_stack)
            })
            .cloned()
    }

    fn binding_matches_predicate_and_not_shadowed(
        keymap: &Keymap,
        binding: &KeyBinding,
        context_stack: &[KeyContext],
    ) -> bool {
        let (bindings, _) = keymap.bindings_for_input(&binding.keystrokes, context_stack);
        if let Some(found) = bindings.iter().next() {
            found.action.partial_eq(binding.action.as_ref())
        } else {
            false
        }
    }

    fn bindings_for_input(
        &self,
        input: &[Keystroke],
        context_stack: &[KeyContext],
    ) -> (SmallVec<[KeyBinding; 1]>, bool, Vec<KeyContext>) {
        let (bindings, partial) = self
            .keymap
            .borrow()
            .bindings_for_input(input, context_stack);
        (bindings, partial, context_stack.to_vec())
    }

    /// Find the bindings that can follow the current input sequence.
    pub fn possible_next_bindings_for_input(
        &self,
        input: &[Keystroke],
        context_stack: &[KeyContext],
    ) -> Vec<KeyBinding> {
        self.keymap
            .borrow()
            .possible_next_bindings_for_input(input, context_stack)
    }

    /// dispatch_key processes the keystroke
    /// input should be set to the value of `pending` from the previous call to dispatch_key.
    /// This returns three instructions to the input handler:
    /// - bindings: any bindings to execute before processing this keystroke
    /// - pending: the new set of pending keystrokes to store
    /// - to_replay: any keystroke that had been pushed to pending, but are no-longer matched,
    ///   these should be replayed first.
    pub fn dispatch_key(
        &self,
        mut input: SmallVec<[Keystroke; 1]>,
        keystroke: Keystroke,
        context_stack: &[KeyContext],
    ) -> DispatchResult {
        input.push(keystroke.clone());
        let (bindings, pending, resolved_stack) = self.bindings_for_input(&input, context_stack);

        if pending {
            return DispatchResult {
                pending: input,
                pending_has_binding: !bindings.is_empty(),
                context_stack: resolved_stack,
                ..Default::default()
            };
        } else if !bindings.is_empty() {
            return DispatchResult {
                bindings,
                context_stack: resolved_stack,
                ..Default::default()
            };
        } else if input.len() == 1 {
            return DispatchResult {
                context_stack: resolved_stack,
                ..Default::default()
            };
        }
        input.pop();

        let (suffix, mut to_replay) = self.replay_prefix(input, context_stack);

        let mut result = self.dispatch_key(suffix, keystroke, context_stack);
        to_replay.extend(result.to_replay);
        result.to_replay = to_replay;
        result
    }

    /// If the user types a matching prefix of a binding and then waits for a timeout
    /// flush_dispatch() converts any previously pending input to replay events.
    pub fn flush_dispatch(
        &self,
        input: SmallVec<[Keystroke; 1]>,
        context_stack: &[KeyContext],
    ) -> SmallVec<[Replay; 1]> {
        let (suffix, mut to_replay) = self.replay_prefix(input, context_stack);

        if !suffix.is_empty() {
            to_replay.extend(self.flush_dispatch(suffix, context_stack))
        }

        to_replay
    }

    /// Converts the longest prefix of input to a replay event and returns the rest.
    fn replay_prefix(
        &self,
        mut input: SmallVec<[Keystroke; 1]>,
        context_stack: &[KeyContext],
    ) -> (SmallVec<[Keystroke; 1]>, SmallVec<[Replay; 1]>) {
        let mut to_replay: SmallVec<[Replay; 1]> = Default::default();
        for last in (0..input.len()).rev() {
            let (bindings, _, _) = self.bindings_for_input(&input[0..=last], context_stack);
            if !bindings.is_empty() {
                to_replay.push(Replay {
                    keystroke: input.drain(0..=last).next_back().unwrap(),
                    bindings,
                });
                break;
            }
        }
        if to_replay.is_empty() {
            to_replay.push(Replay {
                keystroke: input.remove(0),
                ..Default::default()
            });
        }
        (input, to_replay)
    }
}

#[derive(Default, Debug)]
pub(crate) struct Replay {
    pub(crate) keystroke: Keystroke,
    pub(crate) bindings: SmallVec<[KeyBinding; 1]>,
}

#[derive(Default, Debug)]
pub(crate) struct DispatchResult {
    pub(crate) pending: SmallVec<[Keystroke; 1]>,
    pub(crate) pending_has_binding: bool,
    pub(crate) bindings: SmallVec<[KeyBinding; 1]>,
    pub(crate) to_replay: SmallVec<[Replay; 1]>,
    pub(crate) context_stack: Vec<KeyContext>,
}

#[cfg(test)]
mod tests {
    use crate::{
        self as gpui, AppContext, DispatchResult, KeyDispatcher, Keystroke,
    };
    use smallvec::SmallVec;
    use std::{any::TypeId, cell::RefCell, ops::Range, rc::Rc};

    use crate::{
        Action, App, Bounds, Context, ElementImpl, FocusHandle, InputHandler, IntoElement,
        KeyBinding, KeyContext, Keymap, PaintCtx, PaintFrame, Pixels, Point, RenderNode, Render,
        Subscription, TestAppContext, UTF16Selection, UpdateResult, Window,
    };

    #[derive(PartialEq, Eq)]
    struct TestAction;

    impl Action for TestAction {
        fn name(&self) -> &'static str {
            "test::TestAction"
        }

        fn name_for_type() -> &'static str
        where
            Self: ::std::marker::Sized,
        {
            "test::TestAction"
        }

        fn partial_eq(&self, action: &dyn Action) -> bool {
            action.as_any().downcast_ref::<Self>() == Some(self)
        }

        fn boxed_clone(&self) -> std::boxed::Box<dyn Action> {
            Box::new(TestAction)
        }

        fn build(_value: serde_json::Value) -> anyhow::Result<Box<dyn Action>>
        where
            Self: Sized,
        {
            Ok(Box::new(TestAction))
        }
    }

    #[test]
    fn test_keybinding_for_action_bounds() {
        let keymap = Keymap::new(vec![KeyBinding::new(
            "cmd-n",
            TestAction,
            Some("ProjectPanel"),
        )]);
        let keymap = Rc::new(RefCell::new(keymap));
        let dispatcher = KeyDispatcher::new(keymap);

        let contexts = vec![
            KeyContext::parse("Workspace").unwrap(),
            KeyContext::parse("ProjectPanel").unwrap(),
        ];

        let keybinding = dispatcher.bindings_for_action(&TestAction, &contexts);

        assert!(keybinding[0].action.partial_eq(&TestAction))
    }

    #[test]
    fn test_pending_has_binding_state() {
        let bindings = vec![
            KeyBinding::new("ctrl-b h", TestAction, None),
            KeyBinding::new("space", TestAction, Some("ContextA")),
            KeyBinding::new("space f g", TestAction, Some("ContextB")),
        ];
        let keymap = Rc::new(RefCell::new(Keymap::new(bindings)));
        let dispatcher = KeyDispatcher::new(keymap);

        fn dispatch(
            dispatcher: &KeyDispatcher,
            pending: SmallVec<[Keystroke; 1]>,
            key: &str,
            context_stack: &[KeyContext],
        ) -> DispatchResult {
            dispatcher.dispatch_key(pending, Keystroke::parse(key).unwrap(), context_stack)
        }

        let dispatch_path: Vec<KeyContext> = Vec::new();
        let result = dispatch(&dispatcher, SmallVec::new(), "ctrl-b", &dispatch_path);
        assert_eq!(result.pending.len(), 1);
        assert!(!result.pending_has_binding);

        let result = dispatch(&dispatcher, result.pending, "h", &dispatch_path);
        assert_eq!(result.pending.len(), 0);
        assert_eq!(result.bindings.len(), 1);
        assert!(!result.pending_has_binding);

        let context_stack = vec![KeyContext::parse("ContextB").unwrap()];
        let result = dispatch(&dispatcher, SmallVec::new(), "space", &context_stack);

        assert_eq!(result.pending.len(), 1);
        assert!(!result.pending_has_binding);
    }

    #[crate::test]
    fn test_pending_input_observers_notified_on_focus_change(cx: &mut TestAppContext) {
        #[derive(Clone)]
        struct CustomView {
            focus_handle: FocusHandle,
            text: Rc<RefCell<String>>,
        }

        impl CustomView {
            fn new(cx: &mut Context<Self>) -> Self {
                Self {
                    focus_handle: cx.focus_handle(),
                    text: Rc::default(),
                }
            }
        }

        impl Render for CustomView {
            fn render(&mut self, _: &mut Window, _: &mut Context<Self>) -> impl IntoElement {
                CustomElement {
                    focus_handle: self.focus_handle.clone(),
                    text: self.text.clone(),
                }
            }
        }

        #[derive(gpui_macros::Element)]
        #[element(crate = crate)]
        struct CustomElement {
            focus_handle: FocusHandle,
            text: Rc<RefCell<String>>,
        }

        impl IntoElement for CustomElement {
            type Element = Self;
            fn into_element(self) -> Self { self }
        }

        impl ElementImpl for CustomElement {
            fn id(&self) -> Option<crate::ElementId> {
                Some("custom".into())
            }

            fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
                Some(Box::new(CustomNode {
                    focus_handle: self.focus_handle.clone(),
                    text: self.text.clone(),
                }))
            }

            fn render_node_type_id(&self) -> Option<TypeId> {
                Some(TypeId::of::<CustomNode>())
            }

            fn update_render_node(
                &mut self,
                node: &mut dyn RenderNode,
                _window: &mut Window,
                _cx: &mut App,
            ) -> Option<UpdateResult> {
                let node = node.as_any_mut().downcast_mut::<CustomNode>()?;
                node.focus_handle = self.focus_handle.clone();
                node.text = self.text.clone();
                Some(UpdateResult::UNCHANGED)
            }
        }

        struct CustomNode {
            focus_handle: FocusHandle,
            text: Rc<RefCell<String>>,
        }

        impl RenderNode for CustomNode {
            fn taffy_style(&self, _: Pixels, _: f32) -> taffy::style::Style {
                taffy::style::Style::default()
            }

            fn compute_intrinsic_size(&mut self, _: &mut crate::SizingCtx) -> crate::IntrinsicSizeResult {
                crate::IntrinsicSizeResult {
                    size: crate::IntrinsicSize::default(),
                    input: crate::SizingInput::default(),
                }
            }

            fn measure(&mut self, _: crate::Size<Option<Pixels>>, _: crate::Size<crate::AvailableSpace>, _: &mut Window, _: &mut App) -> Option<crate::Size<Pixels>> {
                None
            }

            fn layout_begin(&mut self, _: &mut crate::LayoutCtx) -> crate::LayoutFrame {
                crate::LayoutFrame { handled: true, ..Default::default() }
            }

            fn layout_end(&mut self, _: &mut crate::LayoutCtx, _: crate::LayoutFrame) {}

            fn prepaint_begin(&mut self, ctx: &mut crate::PrepaintCtx) -> crate::PrepaintFrame {
                ctx.window.with_fiber_cx_for(ctx.fiber_id, |fiber| {
                    fiber.set_focus_handle(&self.focus_handle);
                });
                crate::PrepaintFrame { handled: true, ..Default::default() }
            }

            fn prepaint_end(&mut self, _: &mut crate::PrepaintCtx, _: crate::PrepaintFrame) {}

            fn paint_begin(&mut self, ctx: &mut PaintCtx) -> PaintFrame {
                let mut key_context = KeyContext::default();
                key_context.add("Terminal");
                let focus_handle = self.focus_handle.clone();
                let text = self.text.clone();
                ctx.window.with_fiber_cx_for(ctx.fiber_id, |fiber| {
                    fiber.set_key_context(key_context);
                    fiber.handle_input(&focus_handle, CustomInputHandler { text }, ctx.cx);
                    fiber.on_action(TypeId::of::<TestAction>(), |_, _, _, _| {});
                });
                PaintFrame { handled: true, ..Default::default() }
            }

            fn paint_end(&mut self, _: &mut PaintCtx, _: PaintFrame) {}
        }

        struct CustomInputHandler {
            text: Rc<RefCell<String>>,
        }

        impl InputHandler for CustomInputHandler {
            fn selected_text_range(&mut self, _: bool, _: &mut Window, _: &mut App) -> Option<UTF16Selection> { None }
            fn marked_text_range(&mut self, _: &mut Window, _: &mut App) -> Option<Range<usize>> { None }
            fn text_for_range(&mut self, _: Range<usize>, _: &mut Option<Range<usize>>, _: &mut Window, _: &mut App) -> Option<String> { None }
            fn replace_text_in_range(&mut self, replacement_range: Option<Range<usize>>, text: &str, _: &mut Window, _: &mut App) {
                if replacement_range.is_some() { unimplemented!() }
                self.text.borrow_mut().push_str(text)
            }
            fn replace_and_mark_text_in_range(&mut self, replacement_range: Option<Range<usize>>, new_text: &str, _: Option<Range<usize>>, _: &mut Window, _: &mut App) {
                if replacement_range.is_some() { unimplemented!() }
                self.text.borrow_mut().push_str(new_text)
            }
            fn unmark_text(&mut self, _: &mut Window, _: &mut App) {}
            fn bounds_for_range(&mut self, _: Range<usize>, _: &mut Window, _: &mut App) -> Option<Bounds<Pixels>> { None }
            fn character_index_for_point(&mut self, _: Point<Pixels>, _: &mut Window, _: &mut App) -> Option<usize> { None }
        }

        cx.update(|cx| {
            cx.bind_keys([KeyBinding::new("ctrl-b", TestAction, Some("Terminal"))]);
            cx.bind_keys([KeyBinding::new("ctrl-b h", TestAction, Some("Terminal"))]);
        });

        let (test, cx) = cx.add_window_view(|_, cx| CustomView::new(cx));
        let focus_handle = test.update(cx, |test, _| test.focus_handle.clone());

        let pending_input_changed_count = Rc::new(RefCell::new(0usize));
        let pending_input_changed_count_for_observer = pending_input_changed_count.clone();

        struct PendingInputObserver {
            _subscription: Subscription,
        }

        let _observer = cx.update(|window, cx| {
            cx.new(|cx| PendingInputObserver {
                _subscription: cx.observe_pending_input(window, move |_, _, _| {
                    *pending_input_changed_count_for_observer.borrow_mut() += 1;
                }),
            })
        });

        cx.update(|window, cx| {
            window.focus(&focus_handle, cx);
            window.activate_window();
        });

        cx.simulate_keystrokes("ctrl-b");

        let count_after_pending = Rc::new(RefCell::new(0usize));
        let count_after_pending_for_assertion = count_after_pending.clone();

        cx.update(|window, cx| {
            assert!(window.has_pending_keystrokes());
            *count_after_pending.borrow_mut() = *pending_input_changed_count.borrow();
            assert!(*count_after_pending.borrow() > 0);

            window.focus(&cx.focus_handle(), cx);

            assert!(!window.has_pending_keystrokes());
        });

        // Focus-triggered pending-input notifications are deferred to the end of the current
        // effect cycle, so the observer callback should run after the focus update completes.
        cx.update(|_, _| {
            let count_after_focus_change = *pending_input_changed_count.borrow();
            assert!(count_after_focus_change > *count_after_pending_for_assertion.borrow());
        });
    }

    #[crate::test]
    fn test_input_handler_pending(cx: &mut TestAppContext) {
        #[derive(Clone)]
        struct CustomView {
            focus_handle: FocusHandle,
            text: Rc<RefCell<String>>,
        }

        impl CustomView {
            fn new(cx: &mut Context<Self>) -> Self {
                Self {
                    focus_handle: cx.focus_handle(),
                    text: Rc::default(),
                }
            }
        }

        impl Render for CustomView {
            fn render(&mut self, _: &mut Window, _: &mut Context<Self>) -> impl IntoElement {
                CustomElement {
                    focus_handle: self.focus_handle.clone(),
                    text: self.text.clone(),
                }
            }
        }

        #[derive(gpui_macros::Element)]
        #[element(crate = crate)]
        struct CustomElement {
            focus_handle: FocusHandle,
            text: Rc<RefCell<String>>,
        }

        impl IntoElement for CustomElement {
            type Element = Self;
            fn into_element(self) -> Self { self }
        }

        impl ElementImpl for CustomElement {
            fn id(&self) -> Option<crate::ElementId> {
                Some("custom".into())
            }

            fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
                Some(Box::new(CustomNode {
                    focus_handle: self.focus_handle.clone(),
                    text: self.text.clone(),
                }))
            }

            fn render_node_type_id(&self) -> Option<TypeId> {
                Some(TypeId::of::<CustomNode>())
            }

            fn update_render_node(
                &mut self,
                node: &mut dyn RenderNode,
                _window: &mut Window,
                _cx: &mut App,
            ) -> Option<UpdateResult> {
                let node = node.as_any_mut().downcast_mut::<CustomNode>()?;
                node.focus_handle = self.focus_handle.clone();
                node.text = self.text.clone();
                Some(UpdateResult::UNCHANGED)
            }
        }

        struct CustomNode {
            focus_handle: FocusHandle,
            text: Rc<RefCell<String>>,
        }

        impl RenderNode for CustomNode {
            fn taffy_style(&self, _: Pixels, _: f32) -> taffy::style::Style {
                taffy::style::Style::default()
            }

            fn compute_intrinsic_size(&mut self, _: &mut crate::SizingCtx) -> crate::IntrinsicSizeResult {
                crate::IntrinsicSizeResult {
                    size: crate::IntrinsicSize::default(),
                    input: crate::SizingInput::default(),
                }
            }

            fn measure(&mut self, _: crate::Size<Option<Pixels>>, _: crate::Size<crate::AvailableSpace>, _: &mut Window, _: &mut App) -> Option<crate::Size<Pixels>> {
                None
            }

            fn layout_begin(&mut self, _: &mut crate::LayoutCtx) -> crate::LayoutFrame {
                crate::LayoutFrame { handled: true, ..Default::default() }
            }

            fn layout_end(&mut self, _: &mut crate::LayoutCtx, _: crate::LayoutFrame) {}

            fn prepaint_begin(&mut self, ctx: &mut crate::PrepaintCtx) -> crate::PrepaintFrame {
                ctx.window.with_fiber_cx_for(ctx.fiber_id, |fiber| {
                    fiber.set_focus_handle(&self.focus_handle);
                });
                crate::PrepaintFrame { handled: true, ..Default::default() }
            }

            fn prepaint_end(&mut self, _: &mut crate::PrepaintCtx, _: crate::PrepaintFrame) {}

            fn paint_begin(&mut self, ctx: &mut PaintCtx) -> PaintFrame {
                let mut key_context = KeyContext::default();
                key_context.add("Terminal");
                let focus_handle = self.focus_handle.clone();
                let text = self.text.clone();
                ctx.window.with_fiber_cx_for(ctx.fiber_id, |fiber| {
                    fiber.set_key_context(key_context);
                    fiber.handle_input(&focus_handle, CustomInputHandler { text }, ctx.cx);
                    fiber.on_action(TypeId::of::<TestAction>(), |_, _, _, _| {});
                });
                PaintFrame { handled: true, ..Default::default() }
            }

            fn paint_end(&mut self, _: &mut PaintCtx, _: PaintFrame) {}
        }

        struct CustomInputHandler {
            text: Rc<RefCell<String>>,
        }

        impl InputHandler for CustomInputHandler {
            fn selected_text_range(&mut self, _: bool, _: &mut Window, _: &mut App) -> Option<UTF16Selection> { None }
            fn marked_text_range(&mut self, _: &mut Window, _: &mut App) -> Option<Range<usize>> { None }
            fn text_for_range(&mut self, _: Range<usize>, _: &mut Option<Range<usize>>, _: &mut Window, _: &mut App) -> Option<String> { None }
            fn replace_text_in_range(&mut self, replacement_range: Option<Range<usize>>, text: &str, _: &mut Window, _: &mut App) {
                if replacement_range.is_some() { unimplemented!() }
                self.text.borrow_mut().push_str(text)
            }
            fn replace_and_mark_text_in_range(&mut self, replacement_range: Option<Range<usize>>, new_text: &str, _: Option<Range<usize>>, _: &mut Window, _: &mut App) {
                if replacement_range.is_some() { unimplemented!() }
                self.text.borrow_mut().push_str(new_text)
            }
            fn unmark_text(&mut self, _: &mut Window, _: &mut App) {}
            fn bounds_for_range(&mut self, _: Range<usize>, _: &mut Window, _: &mut App) -> Option<Bounds<Pixels>> { None }
            fn character_index_for_point(&mut self, _: Point<Pixels>, _: &mut Window, _: &mut App) -> Option<usize> { None }
        }

        cx.update(|cx| {
            cx.bind_keys([KeyBinding::new("ctrl-b", TestAction, Some("Terminal"))]);
            cx.bind_keys([KeyBinding::new("ctrl-b h", TestAction, Some("Terminal"))]);
        });

        let (test, cx) = cx.add_window_view(|_, cx| CustomView::new(cx));
        let focus_handle = test.update(cx, |test, _| test.focus_handle.clone());

        cx.update(|window, cx| {
            window.focus(&focus_handle, cx);
            window.activate_window();
        });

        cx.simulate_keystrokes("ctrl-b h");

        cx.update(|window, _| {
            assert!(window.has_pending_keystrokes());
        });
    }

    #[crate::test]
    fn test_focus_preserved_across_window_activation(cx: &mut TestAppContext) {
        let cx = cx.add_empty_window();

        let focus_handle = cx.update(|window, cx| {
            let handle = cx.focus_handle();
            window.focus(&handle, cx);
            window.activate_window();
            handle
        });
        cx.run_until_parked();

        cx.update(|window, _| {
            assert!(window.is_window_active(), "Window should be active");
            assert!(
                focus_handle.is_focused(window),
                "Element should be focused after window.focus() call"
            );
        });

        cx.deactivate_window();

        cx.update(|window, _| {
            assert!(!window.is_window_active(), "Window should not be active");
            assert!(
                !focus_handle.is_focused(window),
                "Element should not appear focused when window is inactive"
            );
        });

        cx.update(|window, _| {
            window.activate_window();
        });
        cx.run_until_parked();

        cx.update(|window, _| {
            assert!(window.is_window_active(), "Window should be active again");
            assert!(
                focus_handle.is_focused(window),
                "Element should be focused after window reactivation"
            );
        });
    }
}
