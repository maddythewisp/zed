use crate::{
    AppState, CollaboratorId, FollowerState, Pane, Workspace, WorkspaceSettings,
    pane_group::element::pane_axis,
    workspace_settings::{PaneSplitDirectionHorizontal, PaneSplitDirectionVertical},
};
use anyhow::Result;
use call::{ActiveCall, ParticipantLocation};
use collections::HashMap;
use gpui::{
    Along, AnyWeakView, Axis, Bounds, Entity, Hsla, IntoElement, MouseButton, Pixels, Point,
    WeakEntity, Window, point, size,
};
use parking_lot::Mutex;
use project::Project;
use schemars::JsonSchema;
use serde::Deserialize;
use settings::Settings;
use std::sync::Arc;
use ui::prelude::*;

pub const HANDLE_HITBOX_SIZE: f32 = 4.0;
const HORIZONTAL_MIN_SIZE: f32 = 80.;
const VERTICAL_MIN_SIZE: f32 = 100.;

/// One or many panes, arranged in a horizontal or vertical axis due to a split.
/// Panes have all their tabs and capabilities preserved, and can be split again or resized.
/// Single-pane group is a regular pane.
#[derive(Clone)]
pub struct PaneGroup {
    pub root: Member,
    pub is_center: bool,
}

pub struct PaneRenderResult {
    pub element: gpui::AnyElement,
    pub contains_active_pane: bool,
}

impl PaneGroup {
    pub fn with_root(root: Member) -> Self {
        Self {
            root,
            is_center: false,
        }
    }

    pub fn new(pane: Entity<Pane>) -> Self {
        Self {
            root: Member::Pane(pane),
            is_center: false,
        }
    }

    pub fn set_is_center(&mut self, is_center: bool) {
        self.is_center = is_center;
    }

    pub fn split(
        &mut self,
        old_pane: &Entity<Pane>,
        new_pane: &Entity<Pane>,
        direction: SplitDirection,
        cx: &mut App,
    ) -> Result<()> {
        let result = match &mut self.root {
            Member::Pane(pane) => {
                if pane == old_pane {
                    self.root = Member::new_axis(old_pane.clone(), new_pane.clone(), direction);
                    Ok(())
                } else {
                    anyhow::bail!("Pane not found");
                }
            }
            Member::Axis(axis) => axis.split(old_pane, new_pane, direction),
        };
        if result.is_ok() {
            self.mark_positions(cx);
        }
        result
    }

    pub fn bounding_box_for_pane(&self, pane: &Entity<Pane>) -> Option<Bounds<Pixels>> {
        match &self.root {
            Member::Pane(_) => None,
            Member::Axis(axis) => axis.bounding_box_for_pane(pane),
        }
    }

    pub fn pane_at_pixel_position(&self, coordinate: Point<Pixels>) -> Option<&Entity<Pane>> {
        match &self.root {
            Member::Pane(pane) => Some(pane),
            Member::Axis(axis) => axis.pane_at_pixel_position(coordinate),
        }
    }

    /// Moves active pane to span the entire border in the given direction,
    /// similar to Vim ctrl+w shift-[hjkl] motion.
    ///
    /// Returns:
    /// - Ok(true) if it found and moved a pane
    /// - Ok(false) if it found but did not move the pane
    /// - Err(_) if it did not find the pane
    pub fn move_to_border(
        &mut self,
        active_pane: &Entity<Pane>,
        direction: SplitDirection,
        cx: &mut App,
    ) -> Result<bool> {
        if let Some(pane) = self.find_pane_at_border(direction)
            && pane == active_pane
        {
            return Ok(false);
        }

        if !self.remove_internal(active_pane)? {
            return Ok(false);
        }

        if let Member::Axis(root) = &mut self.root
            && direction.axis() == root.axis
        {
            let idx = if direction.increasing() {
                root.members.len()
            } else {
                0
            };
            root.insert_pane(idx, active_pane);
            self.mark_positions(cx);
            return Ok(true);
        }

        let members = if direction.increasing() {
            vec![self.root.clone(), Member::Pane(active_pane.clone())]
        } else {
            vec![Member::Pane(active_pane.clone()), self.root.clone()]
        };
        self.root = Member::Axis(PaneAxis::new(direction.axis(), members));
        self.mark_positions(cx);
        Ok(true)
    }

    fn find_pane_at_border(&self, direction: SplitDirection) -> Option<&Entity<Pane>> {
        match &self.root {
            Member::Pane(pane) => Some(pane),
            Member::Axis(axis) => axis.find_pane_at_border(direction),
        }
    }

    /// Returns:
    /// - Ok(true) if it found and removed a pane
    /// - Ok(false) if it found but did not remove the pane
    /// - Err(_) if it did not find the pane
    pub fn remove(&mut self, pane: &Entity<Pane>, cx: &mut App) -> Result<bool> {
        let result = self.remove_internal(pane);
        if let Ok(true) = result {
            self.mark_positions(cx);
        }
        result
    }

    fn remove_internal(&mut self, pane: &Entity<Pane>) -> Result<bool> {
        match &mut self.root {
            Member::Pane(_) => Ok(false),
            Member::Axis(axis) => {
                if let Some(last_pane) = axis.remove(pane)? {
                    self.root = last_pane;
                }
                Ok(true)
            }
        }
    }

    pub fn resize(
        &mut self,
        pane: &Entity<Pane>,
        direction: Axis,
        amount: Pixels,
        bounds: &Bounds<Pixels>,
        cx: &mut App,
    ) {
        match &mut self.root {
            Member::Pane(_) => {}
            Member::Axis(axis) => {
                let _ = axis.resize(pane, direction, amount, bounds);
            }
        };
        self.mark_positions(cx);
    }

    pub fn reset_pane_sizes(&mut self, cx: &mut App) {
        match &mut self.root {
            Member::Pane(_) => {}
            Member::Axis(axis) => {
                let _ = axis.reset_pane_sizes();
            }
        };
        self.mark_positions(cx);
    }

    pub fn swap(&mut self, from: &Entity<Pane>, to: &Entity<Pane>, cx: &mut App) {
        match &mut self.root {
            Member::Pane(_) => {}
            Member::Axis(axis) => axis.swap(from, to),
        };
        self.mark_positions(cx);
    }

    pub fn mark_positions(&mut self, cx: &mut App) {
        self.root.mark_positions(self.is_center, true, true, cx);
    }

    pub fn render(
        &self,
        zoomed: Option<&AnyWeakView>,
        render_cx: &dyn PaneLeaderDecorator,
        window: &mut Window,
        cx: &mut App,
    ) -> impl IntoElement {
        self.root.render(0, zoomed, render_cx, window, cx).element
    }

    pub fn panes(&self) -> Vec<&Entity<Pane>> {
        let mut panes = Vec::new();
        self.root.collect_panes(&mut panes);
        panes
    }

    pub fn first_pane(&self) -> Entity<Pane> {
        self.root.first_pane()
    }

    pub fn last_pane(&self) -> Entity<Pane> {
        self.root.last_pane()
    }

    pub fn find_pane_in_direction(
        &mut self,
        active_pane: &Entity<Pane>,
        direction: SplitDirection,
        cx: &App,
    ) -> Option<&Entity<Pane>> {
        let bounding_box = self.bounding_box_for_pane(active_pane)?;
        let cursor = active_pane.read(cx).pixel_position_of_cursor(cx);
        let center = match cursor {
            Some(cursor) if bounding_box.contains(&cursor) => cursor,
            _ => bounding_box.center(),
        };

        let distance_to_next = crate::HANDLE_HITBOX_SIZE;

        let target = match direction {
            SplitDirection::Left => {
                Point::new(bounding_box.left() - distance_to_next.into(), center.y)
            }
            SplitDirection::Right => {
                Point::new(bounding_box.right() + distance_to_next.into(), center.y)
            }
            SplitDirection::Up => {
                Point::new(center.x, bounding_box.top() - distance_to_next.into())
            }
            SplitDirection::Down => {
                Point::new(center.x, bounding_box.bottom() + distance_to_next.into())
            }
        };
        self.pane_at_pixel_position(target)
    }

    pub fn invert_axies(&mut self, cx: &mut App) {
        self.root.invert_pane_axies();
        self.mark_positions(cx);
    }
}

#[derive(Debug, Clone)]
pub enum Member {
    Axis(PaneAxis),
    Pane(Entity<Pane>),
}

impl Member {
    pub fn mark_positions(
        &mut self,
        in_center_group: bool,
        is_upper_left: bool,
        is_upper_right: bool,
        cx: &mut App,
    ) {
        match self {
            Member::Axis(pane_axis) => {
                let len = pane_axis.members.len();
                for (idx, member) in pane_axis.members.iter_mut().enumerate() {
                    let member_upper_left = match pane_axis.axis {
                        Axis::Vertical => is_upper_left && idx == 0,
                        Axis::Horizontal => is_upper_left && idx == 0,
                    };
                    let member_upper_right = match pane_axis.axis {
                        Axis::Vertical => is_upper_right && idx == 0,
                        Axis::Horizontal => is_upper_right && idx == len - 1,
                    };
                    member.mark_positions(
                        in_center_group,
                        member_upper_left,
                        member_upper_right,
                        cx,
                    );
                }
            }
            Member::Pane(entity) => entity.update(cx, |pane, _| {
                pane.in_center_group = in_center_group;
                pane.is_upper_left = is_upper_left;
                pane.is_upper_right = is_upper_right;
            }),
        }
    }
}

#[derive(Clone, Copy)]
pub struct PaneRenderContext<'a> {
    pub project: &'a Entity<Project>,
    pub follower_states: &'a HashMap<CollaboratorId, FollowerState>,
    pub active_call: Option<&'a Entity<ActiveCall>>,
    pub active_pane: &'a Entity<Pane>,
    pub app_state: &'a Arc<AppState>,
    pub workspace: &'a WeakEntity<Workspace>,
}

#[derive(Default)]
pub struct LeaderDecoration {
    border: Option<Hsla>,
    status_box: Option<AnyElement>,
}

pub trait PaneLeaderDecorator {
    fn decorate(&self, pane: &Entity<Pane>, cx: &App) -> LeaderDecoration;
    fn active_pane(&self) -> &Entity<Pane>;
    fn workspace(&self) -> &WeakEntity<Workspace>;
}

pub struct ActivePaneDecorator<'a> {
    active_pane: &'a Entity<Pane>,
    workspace: &'a WeakEntity<Workspace>,
}

impl<'a> ActivePaneDecorator<'a> {
    pub fn new(active_pane: &'a Entity<Pane>, workspace: &'a WeakEntity<Workspace>) -> Self {
        Self {
            active_pane,
            workspace,
        }
    }
}

impl PaneLeaderDecorator for ActivePaneDecorator<'_> {
    fn decorate(&self, _: &Entity<Pane>, _: &App) -> LeaderDecoration {
        LeaderDecoration::default()
    }
    fn active_pane(&self) -> &Entity<Pane> {
        self.active_pane
    }

    fn workspace(&self) -> &WeakEntity<Workspace> {
        self.workspace
    }
}

impl PaneLeaderDecorator for PaneRenderContext<'_> {
    fn decorate(&self, pane: &Entity<Pane>, cx: &App) -> LeaderDecoration {
        let follower_state = self.follower_states.iter().find_map(|(leader_id, state)| {
            if state.center_pane == *pane {
                Some((*leader_id, state))
            } else {
                None
            }
        });
        let Some((leader_id, follower_state)) = follower_state else {
            return LeaderDecoration::default();
        };

        let mut leader_color;
        let status_box;
        match leader_id {
            CollaboratorId::PeerId(peer_id) => {
                let Some(leader) = self.active_call.as_ref().and_then(|call| {
                    let room = call.read(cx).room()?.read(cx);
                    room.remote_participant_for_peer_id(peer_id)
                }) else {
                    return LeaderDecoration::default();
                };

                let is_in_unshared_view = follower_state.active_view_id.is_some_and(|view_id| {
                    !follower_state
                        .items_by_leader_view_id
                        .contains_key(&view_id)
                });

                let mut leader_join_data = None;
                let leader_status_box = match leader.location {
                    ParticipantLocation::SharedProject {
                        project_id: leader_project_id,
                    } => {
                        if Some(leader_project_id) == self.project.read(cx).remote_id() {
                            is_in_unshared_view.then(|| {
                                Label::new(format!(
                                    "{} is in an unshared pane",
                                    leader.user.github_login
                                ))
                            })
                        } else {
                            leader_join_data = Some((leader_project_id, leader.user.id));
                            Some(Label::new(format!(
                                "Follow {} to their active project",
                                leader.user.github_login,
                            )))
                        }
                    }
                    ParticipantLocation::UnsharedProject => Some(Label::new(format!(
                        "{} is viewing an unshared Zed project",
                        leader.user.github_login
                    ))),
                    ParticipantLocation::External => Some(Label::new(format!(
                        "{} is viewing a window outside of Zed",
                        leader.user.github_login
                    ))),
                };
                status_box = leader_status_box.map(|status| {
                    div()
                        .absolute()
                        .w_96()
                        .bottom_3()
                        .right_3()
                        .elevation_2(cx)
                        .p_1()
                        .child(status)
                        .when_some(
                            leader_join_data,
                            |this, (leader_project_id, leader_user_id)| {
                                let app_state = self.app_state.clone();
                                this.cursor_pointer().on_mouse_down(
                                    MouseButton::Left,
                                    move |_, _, cx| {
                                        crate::join_in_room_project(
                                            leader_project_id,
                                            leader_user_id,
                                            app_state.clone(),
                                            cx,
                                        )
                                        .detach_and_log_err(cx);
                                    },
                                )
                            },
                        )
                        .into_any_element()
                });
                leader_color = cx
                    .theme()
                    .players()
                    .color_for_participant(leader.participant_index.0)
                    .cursor;
            }
            CollaboratorId::Agent => {
                status_box = None;
                leader_color = cx.theme().players().agent().cursor;
            }
        }

        let is_in_panel = follower_state.dock_pane.is_some();
        if is_in_panel {
            leader_color.fade_out(0.75);
        } else {
            leader_color.fade_out(0.3);
        }

        LeaderDecoration {
            status_box,
            border: Some(leader_color),
        }
    }

    fn active_pane(&self) -> &Entity<Pane> {
        self.active_pane
    }

    fn workspace(&self) -> &WeakEntity<Workspace> {
        self.workspace
    }
}

impl Member {
    fn new_axis(old_pane: Entity<Pane>, new_pane: Entity<Pane>, direction: SplitDirection) -> Self {
        use Axis::*;
        use SplitDirection::*;

        let axis = match direction {
            Up | Down => Vertical,
            Left | Right => Horizontal,
        };

        let members = match direction {
            Up | Left => vec![Member::Pane(new_pane), Member::Pane(old_pane)],
            Down | Right => vec![Member::Pane(old_pane), Member::Pane(new_pane)],
        };

        Member::Axis(PaneAxis::new(axis, members))
    }

    fn first_pane(&self) -> Entity<Pane> {
        match self {
            Member::Axis(axis) => axis.members[0].first_pane(),
            Member::Pane(pane) => pane.clone(),
        }
    }

    fn last_pane(&self) -> Entity<Pane> {
        match self {
            Member::Axis(axis) => axis.members.last().unwrap().last_pane(),
            Member::Pane(pane) => pane.clone(),
        }
    }

    pub fn render(
        &self,
        basis: usize,
        zoomed: Option<&AnyWeakView>,
        render_cx: &dyn PaneLeaderDecorator,
        window: &mut Window,
        cx: &mut App,
    ) -> PaneRenderResult {
        match self {
            Member::Pane(pane) => {
                if zoomed == Some(&pane.downgrade().into()) {
                    return PaneRenderResult {
                        element: div().into_any(),
                        contains_active_pane: false,
                    };
                }

                let decoration = render_cx.decorate(pane, cx);
                let is_active = pane == render_cx.active_pane();

                PaneRenderResult {
                    element: div()
                        .relative()
                        .flex_1()
                        .size_full()
                        .child(pane.clone())
                        .when_some(decoration.border, |this, color| {
                            this.child(
                                div()
                                    .absolute()
                                    .size_full()
                                    .left_0()
                                    .top_0()
                                    .border_2()
                                    .border_color(color),
                            )
                        })
                        .children(decoration.status_box)
                        .into_any(),
                    contains_active_pane: is_active,
                }
            }
            Member::Axis(axis) => axis.render(basis + 1, zoomed, render_cx, window, cx),
        }
    }

    fn collect_panes<'a>(&'a self, panes: &mut Vec<&'a Entity<Pane>>) {
        match self {
            Member::Axis(axis) => {
                for member in &axis.members {
                    member.collect_panes(panes);
                }
            }
            Member::Pane(pane) => panes.push(pane),
        }
    }

    fn invert_pane_axies(&mut self) {
        match self {
            Self::Axis(axis) => {
                axis.axis = axis.axis.invert();
                for member in axis.members.iter_mut() {
                    member.invert_pane_axies();
                }
            }
            Self::Pane(_) => {}
        }
    }
}

#[derive(Debug, Clone)]
pub struct PaneAxis {
    pub axis: Axis,
    pub members: Vec<Member>,
    pub flexes: Arc<Mutex<Vec<f32>>>,
    pub bounding_boxes: Arc<Mutex<Vec<Option<Bounds<Pixels>>>>>,
}

impl PaneAxis {
    pub fn new(axis: Axis, members: Vec<Member>) -> Self {
        let flexes = Arc::new(Mutex::new(vec![1.; members.len()]));
        let bounding_boxes = Arc::new(Mutex::new(vec![None; members.len()]));
        Self {
            axis,
            members,
            flexes,
            bounding_boxes,
        }
    }

    pub fn load(axis: Axis, members: Vec<Member>, flexes: Option<Vec<f32>>) -> Self {
        let mut flexes = flexes.unwrap_or_else(|| vec![1.; members.len()]);
        if flexes.len() != members.len()
            || (flexes.iter().copied().sum::<f32>() - flexes.len() as f32).abs() >= 0.001
        {
            flexes = vec![1.; members.len()];
        }

        let flexes = Arc::new(Mutex::new(flexes));
        let bounding_boxes = Arc::new(Mutex::new(vec![None; members.len()]));
        Self {
            axis,
            members,
            flexes,
            bounding_boxes,
        }
    }

    fn split(
        &mut self,
        old_pane: &Entity<Pane>,
        new_pane: &Entity<Pane>,
        direction: SplitDirection,
    ) -> Result<()> {
        for (mut idx, member) in self.members.iter_mut().enumerate() {
            match member {
                Member::Axis(axis) => {
                    if axis.split(old_pane, new_pane, direction).is_ok() {
                        return Ok(());
                    }
                }
                Member::Pane(pane) => {
                    if pane == old_pane {
                        if direction.axis() == self.axis {
                            if direction.increasing() {
                                idx += 1;
                            }
                            self.insert_pane(idx, new_pane);
                        } else {
                            *member =
                                Member::new_axis(old_pane.clone(), new_pane.clone(), direction);
                        }
                        return Ok(());
                    }
                }
            }
        }
        anyhow::bail!("Pane not found");
    }

    fn insert_pane(&mut self, idx: usize, new_pane: &Entity<Pane>) {
        self.members.insert(idx, Member::Pane(new_pane.clone()));
        *self.flexes.lock() = vec![1.; self.members.len()];
    }

    fn find_pane_at_border(&self, direction: SplitDirection) -> Option<&Entity<Pane>> {
        if self.axis != direction.axis() {
            return None;
        }
        let member = if direction.increasing() {
            self.members.last()
        } else {
            self.members.first()
        };
        member.and_then(|e| match e {
            Member::Pane(pane) => Some(pane),
            Member::Axis(_) => None,
        })
    }

    fn remove(&mut self, pane_to_remove: &Entity<Pane>) -> Result<Option<Member>> {
        let mut found_pane = false;
        let mut remove_member = None;
        for (idx, member) in self.members.iter_mut().enumerate() {
            match member {
                Member::Axis(axis) => {
                    if let Ok(last_pane) = axis.remove(pane_to_remove) {
                        if let Some(last_pane) = last_pane {
                            *member = last_pane;
                        }
                        found_pane = true;
                        break;
                    }
                }
                Member::Pane(pane) => {
                    if pane == pane_to_remove {
                        found_pane = true;
                        remove_member = Some(idx);
                        break;
                    }
                }
            }
        }

        if found_pane {
            if let Some(idx) = remove_member {
                self.members.remove(idx);
                *self.flexes.lock() = vec![1.; self.members.len()];
            }

            if self.members.len() == 1 {
                let result = self.members.pop();
                *self.flexes.lock() = vec![1.; self.members.len()];
                Ok(result)
            } else {
                Ok(None)
            }
        } else {
            anyhow::bail!("Pane not found");
        }
    }

    fn reset_pane_sizes(&self) {
        *self.flexes.lock() = vec![1.; self.members.len()];
        for member in self.members.iter() {
            if let Member::Axis(axis) = member {
                axis.reset_pane_sizes();
            }
        }
    }

    fn resize(
        &mut self,
        pane: &Entity<Pane>,
        axis: Axis,
        amount: Pixels,
        bounds: &Bounds<Pixels>,
    ) -> Option<bool> {
        let container_size = self
            .bounding_boxes
            .lock()
            .iter()
            .filter_map(|e| *e)
            .reduce(|acc, e| acc.union(&e))
            .unwrap_or(*bounds)
            .size;

        let found_pane = self
            .members
            .iter()
            .any(|member| matches!(member, Member::Pane(p) if p == pane));

        if found_pane && self.axis != axis {
            return Some(false); // pane found but this is not the correct axis direction
        }
        let mut found_axis_index: Option<usize> = None;
        if !found_pane {
            for (i, pa) in self.members.iter_mut().enumerate() {
                if let Member::Axis(pa) = pa
                    && let Some(done) = pa.resize(pane, axis, amount, bounds)
                {
                    if done {
                        return Some(true); // pane found and operations already done
                    } else if self.axis != axis {
                        return Some(false); // pane found but this is not the correct axis direction
                    } else {
                        found_axis_index = Some(i); // pane found and this is correct direction
                    }
                }
            }
            found_axis_index?; // no pane found
        }

        let min_size = match axis {
            Axis::Horizontal => px(HORIZONTAL_MIN_SIZE),
            Axis::Vertical => px(VERTICAL_MIN_SIZE),
        };
        let mut flexes = self.flexes.lock();

        let ix = if found_pane {
            self.members.iter().position(|m| {
                if let Member::Pane(p) = m {
                    p == pane
                } else {
                    false
                }
            })
        } else {
            found_axis_index
        };

        if ix.is_none() {
            return Some(true);
        }

        let ix = ix.unwrap_or(0);

        let size = move |ix, flexes: &[f32]| {
            container_size.along(axis) * (flexes[ix] / flexes.len() as f32)
        };

        // Don't allow resizing to less than the minimum size, if elements are already too small
        if min_size - px(1.) > size(ix, flexes.as_slice()) {
            return Some(true);
        }

        let flex_changes = |pixel_dx, target_ix, next: isize, flexes: &[f32]| {
            let flex_change = flexes.len() as f32 * pixel_dx / container_size.along(axis);
            let current_target_flex = flexes[target_ix] + flex_change;
            let next_target_flex = flexes[(target_ix as isize + next) as usize] - flex_change;
            (current_target_flex, next_target_flex)
        };

        let apply_changes =
            |current_ix: usize, proposed_current_pixel_change: Pixels, flexes: &mut [f32]| {
                let next_target_size = Pixels::max(
                    size(current_ix + 1, flexes) - proposed_current_pixel_change,
                    min_size,
                );
                let current_target_size = Pixels::max(
                    size(current_ix, flexes) + size(current_ix + 1, flexes) - next_target_size,
                    min_size,
                );

                let current_pixel_change = current_target_size - size(current_ix, flexes);

                let (current_target_flex, next_target_flex) =
                    flex_changes(current_pixel_change, current_ix, 1, flexes);

                flexes[current_ix] = current_target_flex;
                flexes[current_ix + 1] = next_target_flex;
            };

        if ix + 1 == flexes.len() {
            apply_changes(ix - 1, -1.0 * amount, flexes.as_mut_slice());
        } else {
            apply_changes(ix, amount, flexes.as_mut_slice());
        }
        Some(true)
    }

    fn swap(&mut self, from: &Entity<Pane>, to: &Entity<Pane>) {
        for member in self.members.iter_mut() {
            match member {
                Member::Axis(axis) => axis.swap(from, to),
                Member::Pane(pane) => {
                    if pane == from {
                        *member = Member::Pane(to.clone());
                    } else if pane == to {
                        *member = Member::Pane(from.clone())
                    }
                }
            }
        }
    }

    fn bounding_box_for_pane(&self, pane: &Entity<Pane>) -> Option<Bounds<Pixels>> {
        debug_assert!(self.members.len() == self.bounding_boxes.lock().len());

        for (idx, member) in self.members.iter().enumerate() {
            match member {
                Member::Pane(found) => {
                    if pane == found {
                        return self.bounding_boxes.lock()[idx];
                    }
                }
                Member::Axis(axis) => {
                    if let Some(rect) = axis.bounding_box_for_pane(pane) {
                        return Some(rect);
                    }
                }
            }
        }
        None
    }

    fn pane_at_pixel_position(&self, coordinate: Point<Pixels>) -> Option<&Entity<Pane>> {
        debug_assert!(self.members.len() == self.bounding_boxes.lock().len());

        let bounding_boxes = self.bounding_boxes.lock();

        for (idx, member) in self.members.iter().enumerate() {
            if let Some(coordinates) = bounding_boxes[idx]
                && coordinates.contains(&coordinate)
            {
                return match member {
                    Member::Pane(found) => Some(found),
                    Member::Axis(axis) => axis.pane_at_pixel_position(coordinate),
                };
            }
        }
        None
    }

    fn render(
        &self,
        basis: usize,
        zoomed: Option<&AnyWeakView>,
        render_cx: &dyn PaneLeaderDecorator,
        window: &mut Window,
        cx: &mut App,
    ) -> PaneRenderResult {
        debug_assert!(self.members.len() == self.flexes.lock().len());
        let mut active_pane_ix = None;
        let mut contains_active_pane = false;
        let mut is_leaf_pane = vec![false; self.members.len()];

        let rendered_children = self
            .members
            .iter()
            .enumerate()
            .map(|(ix, member)| {
                match member {
                    Member::Pane(pane) => {
                        is_leaf_pane[ix] = true;
                        if pane == render_cx.active_pane() {
                            active_pane_ix = Some(ix);
                            contains_active_pane = true;
                        }
                    }
                    Member::Axis(_) => {
                        is_leaf_pane[ix] = false;
                    }
                }

                let result = member.render((basis + ix) * 10, zoomed, render_cx, window, cx);
                if result.contains_active_pane {
                    contains_active_pane = true;
                }
                result.element.into_any_element()
            })
            .collect::<Vec<_>>();

        let element = pane_axis(
            self.axis,
            self.flexes.clone(),
            self.bounding_boxes.clone(),
            render_cx.workspace().clone(),
        )
        .with_is_leaf_pane_mask(is_leaf_pane)
        .children(rendered_children)
        .with_active_pane(active_pane_ix)
        .into_any_element();

        PaneRenderResult {
            element,
            contains_active_pane,
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum SplitDirection {
    Up,
    Down,
    Left,
    Right,
}

impl std::fmt::Display for SplitDirection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SplitDirection::Up => write!(f, "up"),
            SplitDirection::Down => write!(f, "down"),
            SplitDirection::Left => write!(f, "left"),
            SplitDirection::Right => write!(f, "right"),
        }
    }
}

impl SplitDirection {
    pub fn all() -> [Self; 4] {
        [Self::Up, Self::Down, Self::Left, Self::Right]
    }

    pub fn vertical(cx: &mut App) -> Self {
        match WorkspaceSettings::get_global(cx).pane_split_direction_vertical {
            PaneSplitDirectionVertical::Left => SplitDirection::Left,
            PaneSplitDirectionVertical::Right => SplitDirection::Right,
        }
    }

    pub fn horizontal(cx: &mut App) -> Self {
        match WorkspaceSettings::get_global(cx).pane_split_direction_horizontal {
            PaneSplitDirectionHorizontal::Down => SplitDirection::Down,
            PaneSplitDirectionHorizontal::Up => SplitDirection::Up,
        }
    }

    pub fn edge(&self, rect: Bounds<Pixels>) -> Pixels {
        match self {
            Self::Up => rect.origin.y,
            Self::Down => rect.bottom_left().y,
            Self::Left => rect.bottom_left().x,
            Self::Right => rect.bottom_right().x,
        }
    }

    pub fn along_edge(&self, bounds: Bounds<Pixels>, length: Pixels) -> Bounds<Pixels> {
        match self {
            Self::Up => Bounds {
                origin: bounds.origin,
                size: size(bounds.size.width, length),
            },
            Self::Down => Bounds {
                origin: point(bounds.bottom_left().x, bounds.bottom_left().y - length),
                size: size(bounds.size.width, length),
            },
            Self::Left => Bounds {
                origin: bounds.origin,
                size: size(length, bounds.size.height),
            },
            Self::Right => Bounds {
                origin: point(bounds.bottom_right().x - length, bounds.bottom_left().y),
                size: size(length, bounds.size.height),
            },
        }
    }

    pub fn axis(&self) -> Axis {
        match self {
            Self::Up | Self::Down => Axis::Vertical,
            Self::Left | Self::Right => Axis::Horizontal,
        }
    }

    pub fn increasing(&self) -> bool {
        match self {
            Self::Left | Self::Up => false,
            Self::Down | Self::Right => true,
        }
    }

    pub fn opposite(&self) -> SplitDirection {
        match self {
            Self::Down => Self::Up,
            Self::Up => Self::Down,
            Self::Left => Self::Right,
            Self::Right => Self::Left,
        }
    }
}

mod element {
    use std::{any::TypeId, cell::RefCell, iter, rc::Rc, sync::Arc};

    use gpui::{
        Along, AnyElement, App, Axis, BorderStyle, Bounds, CursorStyle, ElementImpl, Hitbox,
        HitboxBehavior, IntoElement, IntrinsicSizeResult, LayoutCtx,
        LayoutFrame, MouseDownEvent, MouseMoveEvent, MouseUpEvent, PaintCtx, PaintFrame,
        ParentElement, Pixels, Point, PrepaintCtx, PrepaintFrame, RenderNode, Size, SizingCtx,
        Style, TaffyStyle, ToTaffy, UpdateResult, WeakEntity, Window, px, relative, size,
    };
    use parking_lot::Mutex;
    use settings::Settings;
    use smallvec::SmallVec;
    use ui::prelude::*;
    use util::ResultExt;

    use crate::Workspace;
    use crate::WorkspaceSettings;

    use super::{HANDLE_HITBOX_SIZE, HORIZONTAL_MIN_SIZE, VERTICAL_MIN_SIZE};

    const DIVIDER_SIZE: f32 = 1.0;

    pub(super) fn pane_axis(
        axis: Axis,
        flexes: Arc<Mutex<Vec<f32>>>,
        bounding_boxes: Arc<Mutex<Vec<Option<Bounds<Pixels>>>>>,
        workspace: WeakEntity<Workspace>,
    ) -> PaneAxisElement {
        PaneAxisElement {
            axis,
            flexes,
            bounding_boxes,
            children: SmallVec::new(),
            active_pane_ix: None,
            workspace,
            is_leaf_pane_mask: Vec::new(),
        }
    }

    #[derive(Element)]
    pub struct PaneAxisElement {
        axis: Axis,
        flexes: Arc<Mutex<Vec<f32>>>,
        bounding_boxes: Arc<Mutex<Vec<Option<Bounds<Pixels>>>>>,
        children: SmallVec<[AnyElement; 2]>,
        active_pane_ix: Option<usize>,
        workspace: WeakEntity<Workspace>,
        is_leaf_pane_mask: Vec<bool>,
    }

    impl PaneAxisElement {
        pub fn with_active_pane(mut self, active_pane_ix: Option<usize>) -> Self {
            self.active_pane_ix = active_pane_ix;
            self
        }

        pub fn with_is_leaf_pane_mask(mut self, mask: Vec<bool>) -> Self {
            self.is_leaf_pane_mask = mask;
            self
        }
    }

    impl IntoElement for PaneAxisElement {
        type Element = Self;

        fn into_element(self) -> Self::Element {
            self
        }
    }

    impl ElementImpl for PaneAxisElement {
        fn children(&self) -> &[AnyElement] {
            &self.children
        }

        fn children_mut(&mut self) -> &mut [AnyElement] {
            &mut self.children
        }

        fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
            Some(Box::new(PaneAxisNode::new(
                self.axis,
                self.flexes.clone(),
                self.bounding_boxes.clone(),
                self.active_pane_ix,
                self.workspace.clone(),
                std::mem::take(&mut self.is_leaf_pane_mask),
            )))
        }

        fn render_node_type_id(&self) -> Option<TypeId> {
            Some(TypeId::of::<PaneAxisNode>())
        }

        fn update_render_node(
            &mut self,
            node: &mut dyn RenderNode,
            _window: &mut Window,
            _cx: &mut App,
        ) -> Option<UpdateResult> {
            let pane_node = node.as_any_mut().downcast_mut::<PaneAxisNode>()?;

            let paint_changed = pane_node.active_pane_ix != self.active_pane_ix
                || pane_node.is_leaf_pane_mask != self.is_leaf_pane_mask;

            pane_node.active_pane_ix = self.active_pane_ix;
            pane_node.is_leaf_pane_mask = std::mem::take(&mut self.is_leaf_pane_mask);

            Some(if paint_changed {
                UpdateResult::PAINT_ONLY
            } else {
                UpdateResult::UNCHANGED
            })
        }
    }

    impl ParentElement for PaneAxisElement {
        fn extend(&mut self, elements: impl IntoIterator<Item = AnyElement>) {
            let flexes = self.flexes.lock();
            let n = flexes.len();
            let start_ix = self.children.len();

            for (i, child) in elements.into_iter().enumerate() {
                let ix = start_ix + i;
                let flex_value = if ix < n { flexes[ix] } else { 1.0 };

                let mut wrapper = div()
                    .id(ix)
                    .flex_basis(px(0.))
                    .min_w(px(HORIZONTAL_MIN_SIZE))
                    .min_h(px(VERTICAL_MIN_SIZE))
                    .size_full()
                    .child(child);
                wrapper.style().flex_grow = Some(flex_value);
                wrapper.style().flex_shrink = Some(1.);

                self.children.push(wrapper.into_any_element());
            }
        }
    }

    struct PaneAxisNode {
        axis: Axis,
        flexes: Arc<Mutex<Vec<f32>>>,
        bounding_boxes: Arc<Mutex<Vec<Option<Bounds<Pixels>>>>>,
        active_pane_ix: Option<usize>,
        workspace: WeakEntity<Workspace>,
        is_leaf_pane_mask: Vec<bool>,
        dragged_handle: Rc<RefCell<Option<usize>>>,
        child_layouts: Vec<ChildLayout>,
    }

    struct ChildLayout {
        bounds: Bounds<Pixels>,
        handle: Option<HandleLayout>,
        is_leaf_pane: bool,
    }

    struct HandleLayout {
        hitbox: Hitbox,
        divider_bounds: Bounds<Pixels>,
    }

    impl PaneAxisNode {
        fn new(
            axis: Axis,
            flexes: Arc<Mutex<Vec<f32>>>,
            bounding_boxes: Arc<Mutex<Vec<Option<Bounds<Pixels>>>>>,
            active_pane_ix: Option<usize>,
            workspace: WeakEntity<Workspace>,
            is_leaf_pane_mask: Vec<bool>,
        ) -> Self {
            Self {
                axis,
                flexes,
                bounding_boxes,
                active_pane_ix,
                workspace,
                is_leaf_pane_mask,
                dragged_handle: Rc::new(RefCell::new(None)),
                child_layouts: Vec::new(),
            }
        }

        fn compute_resize(
            flexes: &Arc<Mutex<Vec<f32>>>,
            e: &MouseMoveEvent,
            ix: usize,
            axis: Axis,
            child_start: Point<Pixels>,
            container_size: Size<Pixels>,
            workspace: WeakEntity<Workspace>,
            window: &mut Window,
            cx: &mut App,
        ) {
            let min_size = match axis {
                Axis::Horizontal => px(HORIZONTAL_MIN_SIZE),
                Axis::Vertical => px(VERTICAL_MIN_SIZE),
            };
            let mut flexes = flexes.lock();
            debug_assert!(flex_values_in_bounds(flexes.as_slice()));

            let size = move |ix, flexes: &[f32]| {
                container_size.along(axis) * (flexes[ix] / flexes.len() as f32)
            };

            if min_size - px(1.) > size(ix, flexes.as_slice()) {
                return;
            }

            let mut proposed_current_pixel_change =
                (e.position - child_start).along(axis) - size(ix, flexes.as_slice());

            let flex_changes = |pixel_dx, target_ix, next: isize, flexes: &[f32]| {
                let flex_change = pixel_dx / container_size.along(axis);
                let current_target_flex = flexes[target_ix] + flex_change;
                let next_target_flex = flexes[(target_ix as isize + next) as usize] - flex_change;
                (current_target_flex, next_target_flex)
            };

            let mut successors = iter::from_fn({
                let forward = proposed_current_pixel_change > px(0.);
                let mut ix_offset = 0;
                let len = flexes.len();
                move || {
                    let result = if forward {
                        (ix + 1 + ix_offset < len).then(|| ix + ix_offset)
                    } else {
                        (ix as isize - ix_offset as isize >= 0).then(|| ix - ix_offset)
                    };
                    ix_offset += 1;
                    result
                }
            });

            while proposed_current_pixel_change.abs() > px(0.) {
                let Some(current_ix) = successors.next() else {
                    break;
                };

                let next_target_size = Pixels::max(
                    size(current_ix + 1, flexes.as_slice()) - proposed_current_pixel_change,
                    min_size,
                );

                let current_target_size = Pixels::max(
                    size(current_ix, flexes.as_slice()) + size(current_ix + 1, flexes.as_slice())
                        - next_target_size,
                    min_size,
                );

                let current_pixel_change =
                    current_target_size - size(current_ix, flexes.as_slice());

                let (current_target_flex, next_target_flex) =
                    flex_changes(current_pixel_change, current_ix, 1, flexes.as_slice());

                flexes[current_ix] = current_target_flex;
                flexes[current_ix + 1] = next_target_flex;

                proposed_current_pixel_change -= current_pixel_change;
            }

            workspace
                .update(cx, |this, cx| this.serialize_workspace(window, cx))
                .log_err();
            cx.stop_propagation();
            window.refresh();
        }

        fn layout_handle(axis: Axis, pane_bounds: Bounds<Pixels>, window: &mut Window) -> HandleLayout {
            let handle_bounds = Bounds {
                origin: pane_bounds.origin.apply_along(axis, |origin| {
                    origin + pane_bounds.size.along(axis) - px(HANDLE_HITBOX_SIZE / 2.)
                }),
                size: pane_bounds
                    .size
                    .apply_along(axis, |_| px(HANDLE_HITBOX_SIZE)),
            };
            let divider_bounds = Bounds {
                origin: pane_bounds
                    .origin
                    .apply_along(axis, |origin| origin + pane_bounds.size.along(axis)),
                size: pane_bounds.size.apply_along(axis, |_| px(DIVIDER_SIZE)),
            };

            HandleLayout {
                hitbox: window.insert_hitbox(handle_bounds, HitboxBehavior::BlockMouse),
                divider_bounds,
            }
        }
    }

    impl RenderNode for PaneAxisNode {
        fn taffy_style(&self, rem_size: Pixels, scale_factor: f32) -> TaffyStyle {
            let style = Style {
                flex_grow: 1.,
                flex_shrink: 1.,
                flex_basis: relative(0.).into(),
                size: size(relative(1.).into(), relative(1.).into()),
                flex_direction: match self.axis {
                    Axis::Horizontal => gpui::FlexDirection::Row,
                    Axis::Vertical => gpui::FlexDirection::Column,
                },
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

        fn prepaint_begin(&mut self, ctx: &mut PrepaintCtx) -> PrepaintFrame {
            let len = ctx.child_bounds.len();
            self.child_layouts.clear();

            let mut bounding_boxes = self.bounding_boxes.lock();
            bounding_boxes.clear();

            for (ix, bounds) in ctx.child_bounds.iter().enumerate() {
                bounding_boxes.push(Some(*bounds));

                let is_leaf_pane = self.is_leaf_pane_mask.get(ix).copied().unwrap_or(true);

                let handle = if ix < len - 1 {
                    Some(Self::layout_handle(self.axis, *bounds, ctx.window))
                } else {
                    None
                };

                self.child_layouts.push(ChildLayout {
                    bounds: *bounds,
                    handle,
                    is_leaf_pane,
                });
            }

            PrepaintFrame {
                handled: true,
                ..Default::default()
            }
        }

        fn prepaint_end(&mut self, _ctx: &mut PrepaintCtx, _frame: PrepaintFrame) {}

        // Uses on_mouse_event for pane resize handles with multiple hitboxes
        #[allow(deprecated)]
        fn paint_begin(&mut self, ctx: &mut PaintCtx) -> PaintFrame {
            let bounds = ctx.bounds;

            let overlay_opacity = WorkspaceSettings::get(None, ctx.cx)
                .active_pane_modifiers
                .inactive_opacity
                .map(|val| val.0.clamp(0.0, 1.0))
                .and_then(|val| (val <= 1.).then_some(val));

            let mut overlay_background = ctx.cx.theme().colors().editor_background;
            if let Some(opacity) = overlay_opacity {
                overlay_background.fade_out(opacity);
            }

            let overlay_border = WorkspaceSettings::get(None, ctx.cx)
                .active_pane_modifiers
                .border_size
                .and_then(|val| (val >= 0.).then_some(val));

            for (ix, child) in self.child_layouts.iter().enumerate() {
                if overlay_opacity.is_some() || overlay_border.is_some() {
                    let overlay_bounds = Bounds {
                        origin: child
                            .bounds
                            .origin
                            .apply_along(Axis::Horizontal, |val| val + px(1.)),
                        size: child
                            .bounds
                            .size
                            .apply_along(Axis::Horizontal, |val| val - px(1.)),
                    };

                    if overlay_opacity.is_some()
                        && child.is_leaf_pane
                        && self.active_pane_ix != Some(ix)
                    {
                        ctx.window
                            .paint_quad(gpui::fill(overlay_bounds, overlay_background));
                    }

                    if let Some(border) = overlay_border
                        && self.active_pane_ix == Some(ix)
                        && child.is_leaf_pane
                    {
                        ctx.window.paint_quad(gpui::quad(
                            overlay_bounds,
                            0.,
                            gpui::transparent_black(),
                            border,
                            ctx.cx.theme().colors().border_selected,
                            BorderStyle::Solid,
                        ));
                    }
                }

                if let Some(handle) = &child.handle {
                    let cursor_style = match self.axis {
                        Axis::Vertical => CursorStyle::ResizeRow,
                        Axis::Horizontal => CursorStyle::ResizeColumn,
                    };

                    if self
                        .dragged_handle
                        .borrow()
                        .is_some_and(|dragged_ix| dragged_ix == ix)
                    {
                        ctx.window.set_window_cursor_style(cursor_style);
                    } else {
                        ctx.window.set_cursor_style(cursor_style, &handle.hitbox);
                    }

                    ctx.window.paint_quad(gpui::fill(
                        handle.divider_bounds,
                        ctx.cx.theme().colors().pane_group_border,
                    ));

                    ctx.window.on_mouse_event({
                        let dragged_handle = self.dragged_handle.clone();
                        let flexes = self.flexes.clone();
                        let workspace = self.workspace.clone();
                        let handle_hitbox = handle.hitbox.clone();
                        move |e: &MouseDownEvent, phase, window, cx| {
                            if phase.bubble() && handle_hitbox.is_hovered(window) {
                                dragged_handle.replace(Some(ix));
                                if e.click_count >= 2 {
                                    let mut borrow = flexes.lock();
                                    *borrow = vec![1.; borrow.len()];
                                    workspace
                                        .update(cx, |this, cx| this.serialize_workspace(window, cx))
                                        .log_err();
                                    window.refresh();
                                }
                                cx.stop_propagation();
                            }
                        }
                    });

                    ctx.window.on_mouse_event({
                        let workspace = self.workspace.clone();
                        let dragged_handle = self.dragged_handle.clone();
                        let flexes = self.flexes.clone();
                        let child_bounds = child.bounds;
                        let axis = self.axis;
                        move |e: &MouseMoveEvent, phase, window, cx| {
                            let dragged_handle = dragged_handle.borrow();
                            if phase.bubble() && *dragged_handle == Some(ix) {
                                Self::compute_resize(
                                    &flexes,
                                    e,
                                    ix,
                                    axis,
                                    child_bounds.origin,
                                    bounds.size,
                                    workspace.clone(),
                                    window,
                                    cx,
                                )
                            }
                        }
                    });
                }
            }

            ctx.window.on_mouse_event({
                let dragged_handle = self.dragged_handle.clone();
                move |_: &MouseUpEvent, phase, _window, _cx| {
                    if phase.bubble() {
                        dragged_handle.replace(None);
                    }
                }
            });

            PaintFrame {
                handled: true,
                ..Default::default()
            }
        }

        fn paint_end(&mut self, _ctx: &mut PaintCtx, _frame: PaintFrame) {}
    }

    fn flex_values_in_bounds(flexes: &[f32]) -> bool {
        (flexes.iter().copied().sum::<f32>() - flexes.len() as f32).abs() < 0.001
    }
}
