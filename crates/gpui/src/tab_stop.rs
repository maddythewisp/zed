use ::sum_tree::SumTree;
use collections::FxHashMap;
use sum_tree::Bias;

use crate::{FocusHandle, FocusId, GlobalElementId};

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
pub(crate) struct TabStopOrderKey {
    pub(crate) preorder_index: u64,
    pub(crate) sequence: u32,
}

impl TabStopOrderKey {
    pub(crate) fn new(preorder_index: u64, sequence: u32) -> Self {
        Self {
            preorder_index,
            sequence,
        }
    }
}

/// Represents a collection of focus handles using the tab-index APIs.
#[derive(Debug)]
pub(crate) struct TabStopMap {
    current_path: TabStopPath,
    by_id: FxHashMap<FocusId, TabStopEntry>,
    order: SumTree<TabStopEntry>,
    last_structure_epoch: u64,
}

type TabIndex = isize;

#[derive(Debug, Default, PartialEq, Eq, Clone, Ord, PartialOrd)]
struct TabStopPath(smallvec::SmallVec<[TabIndex; 6]>);

#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct TabStopNode {
    /// Path to access the node in the tree
    /// The final node in the list is a leaf node corresponding to an actual focus handle,
    /// all other nodes are group nodes
    path: TabStopPath,
    /// Order key used to stabilize sibling ordering.
    order_key: TabStopOrderKey,

    /// Whether this node is a tab stop
    tab_stop: bool,
}

#[derive(Clone, Debug)]
struct TabStopEntry {
    node: TabStopNode,
    handle: FocusHandle,
    owner_id: GlobalElementId,
}

impl Ord for TabStopNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.path
            .cmp(&other.path)
            .then(self.order_key.cmp(&other.order_key))
    }
}

impl PartialOrd for TabStopNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Default for TabStopMap {
    fn default() -> Self {
        Self {
            current_path: TabStopPath::default(),
            by_id: FxHashMap::default(),
            order: SumTree::new(()),
            last_structure_epoch: u64::MAX,
        }
    }
}

impl TabStopMap {
    pub fn insert_with_order(
        &mut self,
        owner_id: GlobalElementId,
        focus_handle: &FocusHandle,
        order_key: TabStopOrderKey,
    ) {
        let mut path = self.current_path.clone();
        path.0.push(focus_handle.tab_index);
        let node = TabStopNode {
            order_key,
            tab_stop: focus_handle.tab_stop,
            path,
        };
        let entry = TabStopEntry {
            node,
            handle: focus_handle.clone(),
            owner_id,
        };
        if let Some(existing) = self.by_id.remove(&focus_handle.id) {
            self.order.remove(&existing.node, ());
        }
        self.by_id.insert(focus_handle.id, entry.clone());
        self.order.insert_or_replace(entry, ());
    }

    pub fn remove(&mut self, focus_id: &FocusId) {
        if let Some(entry) = self.by_id.remove(focus_id) {
            self.order.remove(&entry.node, ());
        }
    }

    pub(crate) fn remove_if_owned_by(&mut self, focus_id: &FocusId, owner_id: GlobalElementId) {
        let should_remove = self
            .by_id
            .get(focus_id)
            .is_some_and(|entry| entry.owner_id == owner_id);
        if should_remove {
            self.remove(focus_id);
        }
    }

    #[cfg(any(test, debug_assertions))]
    pub(crate) fn contains(&self, focus_id: &FocusId) -> bool {
        self.by_id.contains_key(focus_id)
    }

    pub fn begin_group(&mut self, tab_index: isize) {
        self.current_path.0.push(tab_index);
    }

    pub fn end_group(&mut self) {
        self.current_path.0.pop();
    }

    pub fn clear_groups(&mut self) {
        self.current_path.0.clear();
    }

    pub fn rebuild_order_if_needed<F>(&mut self, structure_epoch: u64, mut order_for_owner: F)
    where
        F: FnMut(GlobalElementId) -> u64,
    {
        if self.last_structure_epoch == structure_epoch {
            return;
        }

        self.last_structure_epoch = structure_epoch;
        let mut order = SumTree::new(());
        for entry in self.by_id.values_mut() {
            entry.node.order_key.preorder_index = order_for_owner(entry.owner_id);
            order.insert_or_replace(entry.clone(), ());
        }
        self.order = order;
    }

    pub fn next(&self, focused_id: Option<&FocusId>) -> Option<FocusHandle> {
        let Some(focused_id) = focused_id else {
            let first = self.order.first()?;
            if first.node.tab_stop {
                return Some(first.handle.clone());
            } else {
                return self
                    .next_inner(&first.node)
                    .map(|entry| entry.handle.clone());
            }
        };

        let Some(node) = self.tab_node_for_focus_id(focused_id) else {
            return self.next(None);
        };
        let item = self.next_inner(node);

        if let Some(item) = item {
            Some(item.handle.clone())
        } else {
            self.next(None)
        }
    }

    fn next_inner(&self, node: &TabStopNode) -> Option<&TabStopEntry> {
        let mut cursor = self.order.cursor::<TabStopNode>(());
        cursor.seek(node, Bias::Left);
        cursor.next();
        while let Some(item) = cursor.item()
            && !item.node.tab_stop
        {
            cursor.next();
        }

        cursor.item()
    }

    pub fn prev(&self, focused_id: Option<&FocusId>) -> Option<FocusHandle> {
        let Some(focused_id) = focused_id else {
            let last = self.order.last()?;
            if last.node.tab_stop {
                return Some(last.handle.clone());
            } else {
                return self
                    .prev_inner(&last.node)
                    .map(|entry| entry.handle.clone());
            }
        };

        let Some(node) = self.tab_node_for_focus_id(focused_id) else {
            return self.prev(None);
        };
        let item = self.prev_inner(node);

        if let Some(item) = item {
            Some(item.handle.clone())
        } else {
            self.prev(None)
        }
    }

    fn prev_inner(&self, node: &TabStopNode) -> Option<&TabStopEntry> {
        let mut cursor = self.order.cursor::<TabStopNode>(());
        cursor.seek(node, Bias::Left);
        cursor.prev();
        while let Some(item) = cursor.item()
            && !item.node.tab_stop
        {
            cursor.prev();
        }

        cursor.item()
    }

    fn tab_node_for_focus_id(&self, focused_id: &FocusId) -> Option<&TabStopNode> {
        let Some(entry) = self.by_id.get(focused_id) else {
            return None;
        };
        Some(&entry.node)
    }
}

mod sum_tree_impl {
    use sum_tree::SeekTarget;

    use crate::tab_stop::{TabStopEntry, TabStopNode, TabStopOrderKey, TabStopPath};

    #[derive(Clone, Debug)]
    pub struct TabStopOrderNodeSummary {
        max_order_key: TabStopOrderKey,
        max_path: TabStopPath,
        pub tab_stops: usize,
    }

    pub type TabStopCount = usize;

    impl sum_tree::ContextLessSummary for TabStopOrderNodeSummary {
        fn zero() -> Self {
            TabStopOrderNodeSummary {
                max_order_key: TabStopOrderKey::default(),
                max_path: TabStopPath::default(),
                tab_stops: 0,
            }
        }

        fn add_summary(&mut self, summary: &Self) {
            self.max_order_key = summary.max_order_key;
            self.max_path = summary.max_path.clone();
            self.tab_stops += summary.tab_stops;
        }
    }

    impl sum_tree::KeyedItem for TabStopEntry {
        type Key = TabStopNode;

        fn key(&self) -> Self::Key {
            self.node.clone()
        }
    }

    impl sum_tree::Item for TabStopEntry {
        type Summary = TabStopOrderNodeSummary;

        fn summary(&self, _cx: <Self::Summary as sum_tree::Summary>::Context<'_>) -> Self::Summary {
            TabStopOrderNodeSummary {
                max_order_key: self.node.order_key,
                max_path: self.node.path.clone(),
                tab_stops: if self.node.tab_stop { 1 } else { 0 },
            }
        }
    }

    impl<'a> sum_tree::Dimension<'a, TabStopOrderNodeSummary> for TabStopCount {
        fn zero(_: <TabStopOrderNodeSummary as sum_tree::Summary>::Context<'_>) -> Self {
            0
        }

        fn add_summary(
            &mut self,
            summary: &'a TabStopOrderNodeSummary,
            _: <TabStopOrderNodeSummary as sum_tree::Summary>::Context<'_>,
        ) {
            *self += summary.tab_stops;
        }
    }

    impl<'a> sum_tree::Dimension<'a, TabStopOrderNodeSummary> for TabStopNode {
        fn zero(_: <TabStopOrderNodeSummary as sum_tree::Summary>::Context<'_>) -> Self {
            TabStopNode::default()
        }

        fn add_summary(
            &mut self,
            summary: &'a TabStopOrderNodeSummary,
            _: <TabStopOrderNodeSummary as sum_tree::Summary>::Context<'_>,
        ) {
            self.order_key = summary.max_order_key;
            self.path = summary.max_path.clone();
        }
    }

    impl<'a, 'b> SeekTarget<'a, TabStopOrderNodeSummary, TabStopNode> for &'b TabStopNode {
        fn cmp(
            &self,
            cursor_location: &TabStopNode,
            _: <TabStopOrderNodeSummary as sum_tree::Summary>::Context<'_>,
        ) -> std::cmp::Ordering {
            Iterator::cmp(self.path.0.iter(), cursor_location.path.0.iter()).then(
                <TabStopOrderKey as Ord>::cmp(&self.order_key, &cursor_location.order_key),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools as _;

    use crate::{FocusHandle, FocusId, FocusMap, GlobalElementId, TabStopMap, TabStopOrderKey};
    use std::sync::Arc;

    fn insert_with_order(
        map: &mut TabStopMap,
        owner_id: GlobalElementId,
        next_order: &mut u32,
        handle: &FocusHandle,
    ) {
        let order_key = TabStopOrderKey::new(0, *next_order);
        *next_order += 1;
        map.insert_with_order(owner_id, handle, order_key);
    }

    #[test]
    fn test_tab_handles() {
        let focus_map = Arc::new(FocusMap::default());
        let mut tab_index_map = TabStopMap::default();
        let owner_id = GlobalElementId::from(0u64);
        let mut next_order = 0;

        let focus_handles = [
            FocusHandle::new(&focus_map).tab_stop(true).tab_index(0),
            FocusHandle::new(&focus_map).tab_stop(true).tab_index(1),
            FocusHandle::new(&focus_map).tab_stop(true).tab_index(1),
            FocusHandle::new(&focus_map),
            FocusHandle::new(&focus_map).tab_index(2),
            FocusHandle::new(&focus_map).tab_stop(true).tab_index(0),
            FocusHandle::new(&focus_map).tab_stop(true).tab_index(2),
        ];

        for handle in focus_handles.iter() {
            insert_with_order(&mut tab_index_map, owner_id, &mut next_order, handle);
        }
        let expected = [
            focus_handles[0].clone(),
            focus_handles[5].clone(),
            focus_handles[1].clone(),
            focus_handles[2].clone(),
            focus_handles[6].clone(),
        ];

        let mut prev = None;
        let mut found = vec![];
        for _ in 0..expected.len() {
            let handle = tab_index_map.next(prev.as_ref()).unwrap();
            prev = Some(handle.id);
            found.push(handle.id);
        }

        assert_eq!(
            found,
            expected.iter().map(|handle| handle.id).collect::<Vec<_>>()
        );

        // Select first tab index if no handle is currently focused.
        assert_eq!(tab_index_map.next(None), Some(expected[0].clone()));
        // Select last tab index if no handle is currently focused.
        assert_eq!(tab_index_map.prev(None), expected.last().cloned(),);

        assert_eq!(
            tab_index_map.next(Some(&expected[0].id)),
            Some(expected[1].clone())
        );
        assert_eq!(
            tab_index_map.next(Some(&expected[1].id)),
            Some(expected[2].clone())
        );
        assert_eq!(
            tab_index_map.next(Some(&expected[2].id)),
            Some(expected[3].clone())
        );
        assert_eq!(
            tab_index_map.next(Some(&expected[3].id)),
            Some(expected[4].clone())
        );
        assert_eq!(
            tab_index_map.next(Some(&expected[4].id)),
            Some(expected[0].clone())
        );

        // prev
        assert_eq!(tab_index_map.prev(None), Some(expected[4].clone()));
        assert_eq!(
            tab_index_map.prev(Some(&expected[0].id)),
            Some(expected[4].clone())
        );
        assert_eq!(
            tab_index_map.prev(Some(&expected[1].id)),
            Some(expected[0].clone())
        );
        assert_eq!(
            tab_index_map.prev(Some(&expected[2].id)),
            Some(expected[1].clone())
        );
        assert_eq!(
            tab_index_map.prev(Some(&expected[3].id)),
            Some(expected[2].clone())
        );
        assert_eq!(
            tab_index_map.prev(Some(&expected[4].id)),
            Some(expected[3].clone())
        );
    }

    #[test]
    fn test_tab_non_stop_filtering() {
        let focus_map = Arc::new(FocusMap::default());
        let mut tab_index_map = TabStopMap::default();
        let owner_id = GlobalElementId::from(0u64);
        let mut next_order = 0;

        // Check that we can query next from a non-stop tab
        let tab_non_stop_1 = FocusHandle::new(&focus_map).tab_stop(false).tab_index(1);
        let tab_stop_2 = FocusHandle::new(&focus_map).tab_stop(true).tab_index(2);
        insert_with_order(
            &mut tab_index_map,
            owner_id,
            &mut next_order,
            &tab_non_stop_1,
        );
        insert_with_order(&mut tab_index_map, owner_id, &mut next_order, &tab_stop_2);
        let result = tab_index_map.next(Some(&tab_non_stop_1.id)).unwrap();
        assert_eq!(result.id, tab_stop_2.id);

        // Check that we skip over non-stop tabs
        let tab_stop_0 = FocusHandle::new(&focus_map).tab_stop(true).tab_index(0);
        let tab_non_stop_0 = FocusHandle::new(&focus_map).tab_stop(false).tab_index(0);
        insert_with_order(&mut tab_index_map, owner_id, &mut next_order, &tab_stop_0);
        insert_with_order(
            &mut tab_index_map,
            owner_id,
            &mut next_order,
            &tab_non_stop_0,
        );
        let result = tab_index_map.next(Some(&tab_stop_0.id)).unwrap();
        assert_eq!(result.id, tab_stop_2.id);
    }

    #[must_use]
    struct TabStopMapTest {
        tab_map: TabStopMap,
        focus_map: Arc<FocusMap>,
        expected: Vec<(usize, FocusId)>,
        owner_id: GlobalElementId,
        next_order: u32,
    }

    impl TabStopMapTest {
        #[must_use]
        fn new() -> Self {
            Self {
                tab_map: TabStopMap::default(),
                focus_map: Arc::new(FocusMap::default()),
                expected: Vec::default(),
                owner_id: GlobalElementId::from(0u64),
                next_order: 0,
            }
        }

        #[must_use]
        fn tab_non_stop(mut self, index: isize) -> Self {
            let handle = FocusHandle::new(&self.focus_map)
                .tab_stop(false)
                .tab_index(index);
            insert_with_order(
                &mut self.tab_map,
                self.owner_id,
                &mut self.next_order,
                &handle,
            );
            self
        }

        #[must_use]
        fn tab_stop(mut self, index: isize, expected: usize) -> Self {
            let handle = FocusHandle::new(&self.focus_map)
                .tab_stop(true)
                .tab_index(index);
            insert_with_order(
                &mut self.tab_map,
                self.owner_id,
                &mut self.next_order,
                &handle,
            );
            self.expected.push((expected, handle.id));
            self.expected.sort_by_key(|(expected, _)| *expected);
            self
        }

        #[must_use]
        fn tab_group(mut self, tab_index: isize, children: impl FnOnce(Self) -> Self) -> Self {
            self.tab_map.begin_group(tab_index);
            self = children(self);
            self.tab_map.end_group();
            self
        }

        fn traverse_tab_map(
            &self,
            traverse: impl Fn(&TabStopMap, Option<&FocusId>) -> Option<FocusHandle>,
        ) -> Vec<FocusId> {
            let mut last_focus_id = None;
            let mut found = vec![];
            for _ in 0..self.expected.len() {
                let handle = traverse(&self.tab_map, last_focus_id.as_ref()).unwrap();
                last_focus_id = Some(handle.id);
                found.push(handle.id);
            }
            found
        }

        fn assert(self) {
            let mut expected = self.expected.iter().map(|(_, id)| *id).collect_vec();

            // Check next order
            let forward_found = self.traverse_tab_map(|tab_map, prev| tab_map.next(prev));
            assert_eq!(forward_found, expected);

            // Test overflow. Last to first
            assert_eq!(
                self.tab_map
                    .next(forward_found.last())
                    .map(|handle| handle.id),
                expected.first().cloned()
            );

            // Check previous order
            let reversed_found = self.traverse_tab_map(|tab_map, prev| tab_map.prev(prev));
            expected.reverse();
            assert_eq!(reversed_found, expected);

            // Test overflow. First to last
            assert_eq!(
                self.tab_map
                    .prev(reversed_found.last())
                    .map(|handle| handle.id),
                expected.first().cloned(),
            );
        }
    }

    #[test]
    fn test_with_disabled_tab_stop() {
        TabStopMapTest::new()
            .tab_stop(0, 0)
            .tab_non_stop(1)
            .tab_stop(2, 1)
            .tab_stop(3, 2)
            .assert();
    }

    #[test]
    fn test_with_multiple_disabled_tab_stops() {
        TabStopMapTest::new()
            .tab_non_stop(0)
            .tab_stop(1, 0)
            .tab_non_stop(3)
            .tab_stop(3, 1)
            .tab_non_stop(4)
            .assert();
    }

    #[test]
    fn test_tab_group_functionality() {
        TabStopMapTest::new()
            .tab_stop(0, 0)
            .tab_stop(0, 1)
            .tab_group(2, |t| t.tab_stop(0, 2).tab_stop(1, 3))
            .tab_stop(3, 4)
            .tab_stop(4, 5)
            .assert()
    }

    #[test]
    fn test_sibling_groups() {
        TabStopMapTest::new()
            .tab_stop(0, 0)
            .tab_stop(1, 1)
            .tab_group(2, |test| test.tab_stop(0, 2).tab_stop(1, 3))
            .tab_stop(3, 4)
            .tab_stop(4, 5)
            .tab_group(6, |test| test.tab_stop(0, 6).tab_stop(1, 7))
            .tab_stop(7, 8)
            .tab_stop(8, 9)
            .assert();
    }

    #[test]
    fn test_nested_group() {
        TabStopMapTest::new()
            .tab_stop(0, 0)
            .tab_stop(1, 1)
            .tab_group(2, |t| {
                t.tab_group(0, |t| t.tab_stop(0, 2).tab_stop(1, 3))
                    .tab_stop(1, 4)
            })
            .tab_stop(3, 5)
            .tab_stop(4, 6)
            .assert();
    }

    #[test]
    fn test_sibling_nested_groups() {
        TabStopMapTest::new()
            .tab_stop(0, 0)
            .tab_stop(1, 1)
            .tab_group(2, |builder| {
                builder
                    .tab_stop(0, 2)
                    .tab_stop(2, 5)
                    .tab_group(1, |builder| builder.tab_stop(0, 3).tab_stop(1, 4))
                    .tab_group(3, |builder| builder.tab_stop(0, 6).tab_stop(1, 7))
            })
            .tab_stop(3, 8)
            .tab_stop(4, 9)
            .assert();
    }

    #[test]
    fn test_sibling_nested_groups_out_of_order() {
        TabStopMapTest::new()
            .tab_stop(9, 9)
            .tab_stop(8, 8)
            .tab_group(7, |builder| {
                builder
                    .tab_stop(0, 2)
                    .tab_stop(2, 5)
                    .tab_group(3, |builder| builder.tab_stop(1, 7).tab_stop(0, 6))
                    .tab_group(1, |builder| builder.tab_stop(0, 3).tab_stop(1, 4))
            })
            .tab_stop(3, 0)
            .tab_stop(4, 1)
            .assert();
    }
}
