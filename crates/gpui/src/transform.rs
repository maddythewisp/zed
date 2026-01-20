use crate::{Point, ScaledPixels};
use collections::FxHashMap;
use std::sync::atomic::{AtomicU32, Ordering};

/// Unique identifier for a transform context.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct TransformId(u32);

impl TransformId {
    /// The root transform context.
    pub const ROOT: TransformId = TransformId(0);

    fn next() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(1);
        TransformId(COUNTER.fetch_add(1, Ordering::Relaxed))
    }

    /// Returns `true` if this is the root transform.
    pub fn is_root(self) -> bool {
        self.0 == 0
    }

    /// Returns the underlying numeric ID.
    pub fn as_u32(self) -> u32 {
        self.0
    }

    pub(crate) fn from_u32(id: u32) -> Self {
        TransformId(id)
    }
}

impl From<TransformId> for u32 {
    fn from(id: TransformId) -> Self {
        id.0
    }
}

/// A 2D affine transform (translation + uniform scale) with a parent pointer for hierarchical
/// resolution.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Transform2D {
    /// Translation in parent's coordinate space.
    pub offset: Point<ScaledPixels>,
    /// Uniform scale factor (1.0 = no scaling).
    pub scale: f32,
    /// Parent transform (for hierarchical resolution).
    pub parent: TransformId,
}

impl Default for Transform2D {
    fn default() -> Self {
        Self {
            offset: Point::default(),
            scale: 1.0,
            parent: TransformId::ROOT,
        }
    }
}

impl Transform2D {
    /// Create a translation-only transform.
    pub fn translate(offset: Point<ScaledPixels>, parent: TransformId) -> Self {
        Self {
            offset,
            scale: 1.0,
            parent,
        }
    }

    /// Apply this transform to a local-space point.
    pub fn apply(self, local: Point<ScaledPixels>) -> Point<ScaledPixels> {
        Point::new(
            ScaledPixels(local.x.0 * self.scale + self.offset.x.0),
            ScaledPixels(local.y.0 * self.scale + self.offset.y.0),
        )
    }

    /// Apply the inverse transform (for hit testing).
    pub fn apply_inverse(self, world: Point<ScaledPixels>) -> Point<ScaledPixels> {
        Point::new(
            ScaledPixels((world.x.0 - self.offset.x.0) / self.scale),
            ScaledPixels((world.y.0 - self.offset.y.0) / self.scale),
        )
    }

    /// Compose this transform with a parent world transform.
    pub fn compose(self, parent_world: Transform2D) -> Transform2D {
        Transform2D {
            offset: Point::new(
                ScaledPixels(self.offset.x.0 * parent_world.scale + parent_world.offset.x.0),
                ScaledPixels(self.offset.y.0 * parent_world.scale + parent_world.offset.y.0),
            ),
            scale: self.scale * parent_world.scale,
            parent: parent_world.parent,
        }
    }
}

/// Storage for all active transforms.
///
/// `TransformId` values are stable and can be referenced by cached scene primitives and hitboxes.
pub struct TransformTable {
    transforms: FxHashMap<TransformId, Transform2D>,
    world_cache: FxHashMap<TransformId, Transform2D>,
}

#[derive(Clone, Copy, Debug, Default)]
#[repr(C)]
pub(crate) struct GpuTransform {
    pub offset: [f32; 2],
    pub scale: f32,
    pub parent_index: u32,
}

impl GpuTransform {
    pub(crate) fn identity() -> Self {
        Self {
            offset: [0.0, 0.0],
            scale: 1.0,
            parent_index: 0,
        }
    }
}

impl Default for TransformTable {
    fn default() -> Self {
        Self::new()
    }
}

impl TransformTable {
    /// Create a new transform table containing only the root transform.
    pub fn new() -> Self {
        let mut transforms = FxHashMap::default();
        transforms.insert(TransformId::ROOT, Transform2D::default());
        Self {
            transforms,
            world_cache: FxHashMap::default(),
        }
    }

    /// Remove all transforms and reset to just the root.
    pub fn clear(&mut self) {
        self.transforms.clear();
        self.transforms
            .insert(TransformId::ROOT, Transform2D::default());
        self.world_cache.clear();
    }

    /// Allocate a new transform.
    pub fn push(&mut self, transform: Transform2D) -> TransformId {
        let id = TransformId::next();
        self.transforms.insert(id, transform);
        self.world_cache.remove(&id);
        id
    }

    /// Insert or update a transform with a specific id.
    pub fn insert(&mut self, id: TransformId, transform: Transform2D) {
        self.transforms.insert(id, transform);
        self.invalidate_world_cache(id);
    }

    /// Update an existing transform (e.g., on scroll).
    pub fn update(&mut self, id: TransformId, transform: Transform2D) {
        self.transforms.insert(id, transform);
        self.invalidate_world_cache(id);
    }

    /// Update just the offset (optimized for scroll).
    pub fn update_offset(&mut self, id: TransformId, offset: Point<ScaledPixels>) {
        if let Some(t) = self.transforms.get_mut(&id) {
            t.offset = offset;
            self.invalidate_world_cache(id);
        }
    }

    fn invalidate_world_cache(&mut self, _id: TransformId) {
        // For simplicity, clear the entire cache on any update.
        self.world_cache.clear();
    }

    /// Get a local transform (or default if missing).
    pub fn get(&self, id: TransformId) -> Transform2D {
        self.transforms.get(&id).copied().unwrap_or_default()
    }

    /// Get the world transform (resolved through the parent chain).
    pub fn get_world(&mut self, id: TransformId) -> Transform2D {
        if let Some(cached) = self.world_cache.get(&id) {
            return *cached;
        }

        let local = self.get(id);
        let world = if local.parent.is_root() {
            local
        } else {
            let parent_world = self.get_world(local.parent);
            local.compose(parent_world)
        };

        self.world_cache.insert(id, world);
        world
    }

    pub(crate) fn get_world_no_cache(&self, id: TransformId) -> Transform2D {
        let mut offset = Point::<ScaledPixels>::default();
        let mut scale = 1.0;
        let mut current = id;
        for _ in 0..16 {
            if current.is_root() {
                break;
            }

            let t = self.get(current);
            offset = Point::new(
                ScaledPixels(offset.x.0 * t.scale + t.offset.x.0),
                ScaledPixels(offset.y.0 * t.scale + t.offset.y.0),
            );
            scale *= t.scale;
            current = t.parent;
        }

        Transform2D {
            offset,
            scale,
            parent: TransformId::ROOT,
        }
    }

    /// Transform a local point to world coordinates.
    pub fn local_to_world(
        &mut self,
        id: TransformId,
        local: Point<ScaledPixels>,
    ) -> Point<ScaledPixels> {
        self.get_world(id).apply(local)
    }

    /// Transform a world point to local coordinates (for hit testing).
    pub fn world_to_local(
        &mut self,
        id: TransformId,
        world: Point<ScaledPixels>,
    ) -> Point<ScaledPixels> {
        self.get_world(id).apply_inverse(world)
    }

    pub(crate) fn world_to_local_no_cache(
        &self,
        id: TransformId,
        world: Point<ScaledPixels>,
    ) -> Point<ScaledPixels> {
        self.get_world_no_cache(id).apply_inverse(world)
    }

    pub(crate) fn to_gpu_transforms(&self) -> Vec<GpuTransform> {
        let max_id = self
            .transforms
            .keys()
            .map(|id| id.as_u32())
            .max()
            .unwrap_or(0);

        let mut gpu = vec![GpuTransform::identity(); max_id as usize + 1];
        for (&id, t) in self.transforms.iter() {
            let index = id.as_u32() as usize;
            if index >= gpu.len() {
                continue;
            }
            gpu[index] = GpuTransform {
                offset: [t.offset.x.0, t.offset.y.0],
                scale: t.scale,
                parent_index: t.parent.as_u32(),
            };
        }
        gpu
    }

    pub(crate) fn remove(&mut self, id: TransformId) {
        if id.is_root() {
            return;
        }
        self.transforms.remove(&id);
        self.invalidate_world_cache(id);
    }
}
