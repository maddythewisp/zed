// todo("windows"): remove
#![cfg_attr(windows, allow(dead_code))]

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use slotmap::{SlotMap, new_key_type};

use crate::{
    AtlasTextureId, AtlasTile, Background, Bounds, ContentMask, Corners, Edges, Hsla, Pixels,
    Point, Radians, ScaledPixels, Size, TransformId, TransformTable, point,
};
use std::{
    fmt::Debug,
    iter::Peekable,
    ops::{Add, Sub},
    slice,
};

#[allow(non_camel_case_types, unused)]
pub(crate) type PathVertex_ScaledPixels = PathVertex<ScaledPixels>;

pub(crate) type DrawOrder = u32;

new_key_type! { pub(crate) struct SceneSegmentId; }

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum SceneSegmentRef {
    Fiber(SceneSegmentId),
    Transient,
}

/// Shared storage for fiber scene segments that persists across frame swaps.
/// This is stored at the Window level so segment IDs remain valid when
/// rendered_frame and next_frame are swapped.
#[derive(Default)]
pub(crate) struct SceneSegmentPool {
    segments: SlotMap<SceneSegmentId, SceneSegment>,
    pub(crate) transforms: TransformTable,
}

impl SceneSegmentPool {
    pub fn alloc_segment(&mut self) -> SceneSegmentId {
        self.segments.insert(SceneSegment::default())
    }

    pub fn reset_segment(&mut self, id: SceneSegmentId, mutation_epoch: u64) {
        if let Some(segment) = self.segment_mut(id) {
            segment.clear();
            segment.mutated_epoch = mutation_epoch;
        }
    }

    pub fn remove_segment(&mut self, id: SceneSegmentId) {
        let _ = self.segments.remove(id);
    }

    fn segment_mut(&mut self, id: SceneSegmentId) -> Option<&mut SceneSegment> {
        self.segments.get_mut(id)
    }

    fn segment(&self, id: SceneSegmentId) -> Option<&SceneSegment> {
        self.segments.get(id)
    }

    /// Clear all segments in the pool without deallocating them.
    /// Used on window refresh to ensure stale data is removed.
    #[cfg(test)]
    pub fn reset_all(&mut self, mutation_epoch: u64) {
        for segment in self.segments.values_mut() {
            segment.clear();
            segment.mutated_epoch = mutation_epoch;
        }
    }

    /// Returns the number of segments currently allocated in the pool.
    #[cfg(test)]
    pub fn len(&self) -> usize {
        self.segments.len()
    }
}

#[derive(Default)]
struct SceneSegment {
    next_order: DrawOrder,
    mutated_epoch: u64,
    layer_stack: Vec<DrawOrder>,
    shadows: Vec<Shadow>,
    shadow_transforms: Vec<TransformationMatrix>,
    quads: Vec<Quad>,
    quad_transforms: Vec<TransformationMatrix>,
    paths: Vec<Path<ScaledPixels>>,
    underlines: Vec<Underline>,
    underline_transforms: Vec<TransformationMatrix>,
    monochrome_sprites: Vec<MonochromeSprite>,
    subpixel_sprites: Vec<SubpixelSprite>,
    polychrome_sprites: Vec<PolychromeSprite>,
    polychrome_sprite_transforms: Vec<TransformationMatrix>,
    surfaces: Vec<PaintSurface>,
}

impl SceneSegment {
    fn clear(&mut self) {
        self.next_order = 0;
        self.layer_stack.clear();
        self.shadows.clear();
        self.shadow_transforms.clear();
        self.quads.clear();
        self.quad_transforms.clear();
        self.paths.clear();
        self.underlines.clear();
        self.underline_transforms.clear();
        self.monochrome_sprites.clear();
        self.subpixel_sprites.clear();
        self.polychrome_sprites.clear();
        self.polychrome_sprite_transforms.clear();
        self.surfaces.clear();
    }

    fn next_draw_order(&mut self) -> DrawOrder {
        let order = self.next_order;
        self.next_order = self.next_order.saturating_add(1);
        order
    }
}

pub(crate) struct Scene {
    segment_order: Vec<SceneSegmentRef>,
    segment_order_epoch: u64,
    mutation_epoch: u64,
    transient: SceneSegment,
    active_stack: Vec<SceneSegmentRef>,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub(crate) struct SceneFinishStats {
    pub(crate) total_pool_segments: usize,
    pub(crate) mutated_pool_segments: usize,
    pub(crate) transient_mutated: bool,
}

impl Default for Scene {
    fn default() -> Self {
        Self {
            segment_order: vec![SceneSegmentRef::Transient],
            segment_order_epoch: u64::MAX,
            mutation_epoch: 0,
            transient: SceneSegment::default(),
            active_stack: vec![SceneSegmentRef::Transient],
        }
    }
}

impl Scene {
    #[cfg(test)]
    pub fn mutation_epoch(&self) -> u64 {
        self.mutation_epoch
    }

    pub fn begin_frame(&mut self) {
        self.mutation_epoch = self.mutation_epoch.wrapping_add(1);
        self.active_stack.clear();
        self.active_stack.push(SceneSegmentRef::Transient);
        self.clear_transient();
    }

    pub fn clear_transient(&mut self) {
        self.transient.clear();
    }

    pub fn segment_order_epoch(&self) -> u64 {
        self.segment_order_epoch
    }

    pub fn set_segment_order(&mut self, order: Vec<SceneSegmentId>, structure_epoch: u64) {
        self.segment_order.clear();
        self.segment_order
            .extend(order.into_iter().map(SceneSegmentRef::Fiber));
        self.segment_order.push(SceneSegmentRef::Transient);
        self.segment_order_epoch = structure_epoch;
    }

    pub fn alloc_segment(&mut self, pool: &mut SceneSegmentPool) -> SceneSegmentId {
        pool.alloc_segment()
    }

    pub fn reset_segment(&mut self, pool: &mut SceneSegmentPool, id: SceneSegmentId) {
        let mutation_epoch = self.mutation_epoch;
        pool.reset_segment(id, mutation_epoch);
    }

    pub fn push_fiber_segment(&mut self, id: SceneSegmentId) {
        self.active_stack.push(SceneSegmentRef::Fiber(id));
    }

    pub fn pop_segment(&mut self) {
        if self.active_stack.len() > 1 {
            self.active_stack.pop();
        }
    }

    fn active_segment_mut<'a>(
        &'a mut self,
        pool: &'a mut SceneSegmentPool,
    ) -> &'a mut SceneSegment {
        let active = self
            .active_stack
            .last()
            .copied()
            .unwrap_or(SceneSegmentRef::Transient);
        if let SceneSegmentRef::Fiber(id) = active {
            if let Some(segment) = pool.segment_mut(id) {
                return segment;
            }
        }
        &mut self.transient
    }

    pub fn push_layer(&mut self, pool: &mut SceneSegmentPool) {
        let segment = self.active_segment_mut(pool);
        let order = segment.next_draw_order();
        segment.layer_stack.push(order);
    }

    pub fn pop_layer(&mut self, pool: &mut SceneSegmentPool) {
        let segment = self.active_segment_mut(pool);
        segment.layer_stack.pop();
    }

    pub fn insert_primitive(
        &mut self,
        pool: &mut SceneSegmentPool,
        primitive: impl Into<Primitive>,
        cull: bool,
    ) {
        let mut primitive = primitive.into();
        let transformed_bounds = transformed_bounds(&primitive);
        if transformed_bounds.is_empty() {
            return;
        }
        if cull {
            let transform_index = primitive.transform_index();
            let world_bounds = if transform_index == 0 {
                transformed_bounds
            } else {
                let world_transform = pool
                    .transforms
                    .get_world_no_cache(TransformId::from_u32(transform_index));
                Bounds {
                    origin: world_transform.apply(transformed_bounds.origin),
                    size: Size {
                        width: ScaledPixels(
                            transformed_bounds.size.width.0 * world_transform.scale,
                        ),
                        height: ScaledPixels(
                            transformed_bounds.size.height.0 * world_transform.scale,
                        ),
                    },
                }
            };

            let clipped_bounds = world_bounds.intersect(&primitive.content_mask().bounds);
            if clipped_bounds.is_empty() {
                return;
            }
        }

        let mutation_epoch = self.mutation_epoch;
        let segment = self.active_segment_mut(pool);
        segment.mutated_epoch = mutation_epoch;
        let order = segment
            .layer_stack
            .last()
            .copied()
            .unwrap_or_else(|| segment.next_draw_order());
        match &mut primitive {
            Primitive::Shadow(shadow, transform) => {
                shadow.order = order;
                segment.shadows.push(shadow.clone());
                segment.shadow_transforms.push(*transform);
            }
            Primitive::Quad(quad, transform) => {
                quad.order = order;
                segment.quads.push(quad.clone());
                segment.quad_transforms.push(*transform);
            }
            Primitive::Path(path) => {
                path.order = order;
                path.id = PathId(segment.paths.len());
                segment.paths.push(path.clone());
            }
            Primitive::Underline(underline, transform) => {
                underline.order = order;
                segment.underlines.push(underline.clone());
                segment.underline_transforms.push(*transform);
            }
            Primitive::MonochromeSprite(sprite) => {
                sprite.order = order;
                segment.monochrome_sprites.push(sprite.clone());
            }
            Primitive::SubpixelSprite(sprite) => {
                sprite.order = order;
                segment.subpixel_sprites.push(sprite.clone());
            }
            Primitive::PolychromeSprite(sprite, transform) => {
                sprite.order = order;
                segment.polychrome_sprites.push(sprite.clone());
                segment.polychrome_sprite_transforms.push(*transform);
            }
            Primitive::Surface(surface) => {
                surface.order = order;
                segment.surfaces.push(surface.clone());
            }
        }

        /// Compute an axis-aligned bounding box for a transformed primitive.
        fn transformed_bounds(primitive: &Primitive) -> Bounds<ScaledPixels> {
            fn apply_transform(
                bounds: &Bounds<ScaledPixels>,
                transform: &TransformationMatrix,
            ) -> Bounds<ScaledPixels> {
                if transform.is_unit() {
                    return *bounds;
                }
                let [[a, b], [c, d]] = transform.rotation_scale;
                let tx = transform.translation[0];
                let ty = transform.translation[1];

                let x0 = bounds.origin.x.0;
                let x1 = x0 + bounds.size.width.0;
                let y0 = bounds.origin.y.0;
                let y1 = y0 + bounds.size.height.0;

                let ax0 = a * x0;
                let ax1 = a * x1;
                let by0 = b * y0;
                let by1 = b * y1;

                let cx0 = c * x0;
                let cx1 = c * x1;
                let dy0 = d * y0;
                let dy1 = d * y1;

                let min_x = tx + ax0.min(ax1) + by0.min(by1);
                let max_x = tx + ax0.max(ax1) + by0.max(by1);
                let min_y = ty + cx0.min(cx1) + dy0.min(dy1);
                let max_y = ty + cx0.max(cx1) + dy0.max(dy1);

                Bounds {
                    origin: point(ScaledPixels(min_x), ScaledPixels(min_y)),
                    size: Size {
                        width: ScaledPixels((max_x - min_x).max(0.0)),
                        height: ScaledPixels((max_y - min_y).max(0.0)),
                    },
                }
            }

            match primitive {
                Primitive::Shadow(shadow, transform) => apply_transform(&shadow.bounds, transform),
                Primitive::Quad(quad, transform) => apply_transform(&quad.bounds, transform),
                Primitive::Underline(underline, transform) => {
                    apply_transform(&underline.bounds, transform)
                }
                Primitive::MonochromeSprite(sprite) => {
                    apply_transform(&sprite.bounds, &sprite.transformation)
                }
                Primitive::PolychromeSprite(sprite, transform) => {
                    apply_transform(&sprite.bounds, transform)
                }
                Primitive::SubpixelSprite(sprite) => {
                    apply_transform(&sprite.bounds, &sprite.transformation)
                }
                Primitive::Path(path) => path.bounds,
                Primitive::Surface(surface) => surface.bounds,
            }
        }
    }

    fn sort_with_aux_by_key<T, U, K: Ord>(
        items: &mut Vec<T>,
        aux: &mut Vec<U>,
        mut key_fn: impl FnMut(&T) -> K,
    ) {
        debug_assert_eq!(items.len(), aux.len());
        if items.len() <= 1 {
            return;
        }

        let len = items.len();

        let mut perm: Vec<usize> = (0..len).collect();
        perm.sort_by_key(|&index| key_fn(&items[index]));

        let mut pos: Vec<usize> = vec![0; len];
        for (desired_pos, &current_pos) in perm.iter().enumerate() {
            pos[current_pos] = desired_pos;
        }

        for i in 0..len {
            while pos[i] != i {
                let j = pos[i];
                items.swap(i, j);
                aux.swap(i, j);
                pos.swap(i, j);
            }
        }
    }

    pub(crate) fn finish(&mut self, pool: &mut SceneSegmentPool) -> SceneFinishStats {
        let mutation_epoch = self.mutation_epoch;
        let mut stats = SceneFinishStats::default();
        // NOTE: `finish` sorts only segments that were mutated this frame, to avoid
        // re-sorting cached fiber segments that were replayed from a previous frame.
        //
        // `SlotMap::values_mut` is O(n) over occupied segments and is fine here.
        for segment in pool.segments.values_mut() {
            stats.total_pool_segments += 1;
            if segment.mutated_epoch != mutation_epoch {
                continue;
            }
            stats.mutated_pool_segments += 1;
            Self::sort_with_aux_by_key(
                &mut segment.shadows,
                &mut segment.shadow_transforms,
                |shadow| shadow.order,
            );
            Self::sort_with_aux_by_key(&mut segment.quads, &mut segment.quad_transforms, |quad| {
                quad.order
            });
            segment.paths.sort_by_key(|path| path.order);
            Self::sort_with_aux_by_key(
                &mut segment.underlines,
                &mut segment.underline_transforms,
                |underline| underline.order,
            );
            segment
                .monochrome_sprites
                .sort_by_key(|sprite| (sprite.order, sprite.tile.tile_id));
            segment
                .subpixel_sprites
                .sort_by_key(|sprite| (sprite.order, sprite.tile.tile_id));
            Self::sort_with_aux_by_key(
                &mut segment.polychrome_sprites,
                &mut segment.polychrome_sprite_transforms,
                |sprite| (sprite.order, sprite.tile.tile_id),
            );
            segment.surfaces.sort_by_key(|surface| surface.order);
        }
        if self.transient.mutated_epoch == mutation_epoch {
            stats.transient_mutated = true;
            Self::sort_with_aux_by_key(
                &mut self.transient.shadows,
                &mut self.transient.shadow_transforms,
                |shadow| shadow.order,
            );
            Self::sort_with_aux_by_key(
                &mut self.transient.quads,
                &mut self.transient.quad_transforms,
                |quad| quad.order,
            );
            self.transient.paths.sort_by_key(|path| path.order);
            Self::sort_with_aux_by_key(
                &mut self.transient.underlines,
                &mut self.transient.underline_transforms,
                |underline| underline.order,
            );
            self.transient
                .monochrome_sprites
                .sort_by_key(|sprite| (sprite.order, sprite.tile.tile_id));
            self.transient
                .subpixel_sprites
                .sort_by_key(|sprite| (sprite.order, sprite.tile.tile_id));
            Self::sort_with_aux_by_key(
                &mut self.transient.polychrome_sprites,
                &mut self.transient.polychrome_sprite_transforms,
                |sprite| (sprite.order, sprite.tile.tile_id),
            );
            self.transient.surfaces.sort_by_key(|surface| surface.order);
        }

        stats
    }

    #[cfg_attr(
        all(
            any(target_os = "linux", target_os = "freebsd"),
            not(any(feature = "x11", feature = "wayland"))
        ),
        allow(dead_code)
    )]
    pub(crate) fn batches<'a>(
        &'a self,
        pool: &'a SceneSegmentPool,
    ) -> impl Iterator<Item = PrimitiveBatch<'a>> {
        SegmentBatchIterator {
            scene: self,
            pool,
            order_index: 0,
            current: None,
        }
    }

    fn count_segments(&self, pool: &SceneSegmentPool, f: impl Fn(&SceneSegment) -> usize) -> usize {
        let mut total = f(&self.transient);
        for segment in pool.segments.values() {
            total += f(segment);
        }
        total
    }

    pub(crate) fn paths_len(&self, pool: &SceneSegmentPool) -> usize {
        self.count_segments(pool, |segment| segment.paths.len())
    }

    pub(crate) fn shadows_len(&self, pool: &SceneSegmentPool) -> usize {
        self.count_segments(pool, |segment| segment.shadows.len())
    }

    pub(crate) fn quads_len(&self, pool: &SceneSegmentPool) -> usize {
        self.count_segments(pool, |segment| segment.quads.len())
    }

    pub(crate) fn underlines_len(&self, pool: &SceneSegmentPool) -> usize {
        self.count_segments(pool, |segment| segment.underlines.len())
    }

    pub(crate) fn monochrome_sprites_len(&self, pool: &SceneSegmentPool) -> usize {
        self.count_segments(pool, |segment| segment.monochrome_sprites.len())
    }

    pub(crate) fn subpixel_sprites_len(&self, pool: &SceneSegmentPool) -> usize {
        self.count_segments(pool, |segment| segment.subpixel_sprites.len())
    }

    pub(crate) fn polychrome_sprites_len(&self, pool: &SceneSegmentPool) -> usize {
        self.count_segments(pool, |segment| segment.polychrome_sprites.len())
    }

    pub(crate) fn surfaces_len(&self, pool: &SceneSegmentPool) -> usize {
        self.count_segments(pool, |segment| segment.surfaces.len())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Default)]
#[cfg_attr(
    all(
        any(target_os = "linux", target_os = "freebsd"),
        not(any(feature = "x11", feature = "wayland"))
    ),
    allow(dead_code)
)]
pub(crate) enum PrimitiveKind {
    Shadow,
    #[default]
    Quad,
    Path,
    Underline,
    MonochromeSprite,
    SubpixelSprite,
    PolychromeSprite,
    Surface,
}

#[derive(Clone)]
pub(crate) enum Primitive {
    Shadow(Shadow, TransformationMatrix),
    Quad(Quad, TransformationMatrix),
    Path(Path<ScaledPixels>),
    Underline(Underline, TransformationMatrix),
    MonochromeSprite(MonochromeSprite),
    SubpixelSprite(SubpixelSprite),
    PolychromeSprite(PolychromeSprite, TransformationMatrix),
    Surface(PaintSurface),
}

impl Primitive {
    #[allow(dead_code)]
    pub fn bounds(&self) -> &Bounds<ScaledPixels> {
        match self {
            Primitive::Shadow(shadow, _) => &shadow.bounds,
            Primitive::Quad(quad, _) => &quad.bounds,
            Primitive::Path(path) => &path.bounds,
            Primitive::Underline(underline, _) => &underline.bounds,
            Primitive::MonochromeSprite(sprite) => &sprite.bounds,
            Primitive::SubpixelSprite(sprite) => &sprite.bounds,
            Primitive::PolychromeSprite(sprite, _) => &sprite.bounds,
            Primitive::Surface(surface) => &surface.bounds,
        }
    }

    pub fn content_mask(&self) -> &ContentMask<ScaledPixels> {
        match self {
            Primitive::Shadow(shadow, _) => &shadow.content_mask,
            Primitive::Quad(quad, _) => &quad.content_mask,
            Primitive::Path(path) => &path.content_mask,
            Primitive::Underline(underline, _) => &underline.content_mask,
            Primitive::MonochromeSprite(sprite) => &sprite.content_mask,
            Primitive::SubpixelSprite(sprite) => &sprite.content_mask,
            Primitive::PolychromeSprite(sprite, _) => &sprite.content_mask,
            Primitive::Surface(surface) => &surface.content_mask,
        }
    }

    fn transform_index(&self) -> u32 {
        match self {
            Primitive::Shadow(shadow, _) => shadow.transform_index,
            Primitive::Quad(quad, _) => quad.transform_index,
            Primitive::Path(path) => path.transform_index,
            Primitive::Underline(underline, _) => underline.transform_index,
            Primitive::MonochromeSprite(sprite) => sprite.transform_index,
            Primitive::SubpixelSprite(sprite) => sprite.transform_index,
            Primitive::PolychromeSprite(sprite, _) => sprite.transform_index,
            Primitive::Surface(surface) => surface.transform_index,
        }
    }
}

#[cfg_attr(
    all(
        any(target_os = "linux", target_os = "freebsd"),
        not(any(feature = "x11", feature = "wayland"))
    ),
    allow(dead_code)
)]
struct BatchIterator<'a> {
    shadows: &'a [Shadow],
    shadow_transforms: &'a [TransformationMatrix],
    shadows_start: usize,
    shadows_iter: Peekable<slice::Iter<'a, Shadow>>,
    quads: &'a [Quad],
    quad_transforms: &'a [TransformationMatrix],
    quads_start: usize,
    quads_iter: Peekable<slice::Iter<'a, Quad>>,
    paths: &'a [Path<ScaledPixels>],
    paths_start: usize,
    paths_iter: Peekable<slice::Iter<'a, Path<ScaledPixels>>>,
    underlines: &'a [Underline],
    underline_transforms: &'a [TransformationMatrix],
    underlines_start: usize,
    underlines_iter: Peekable<slice::Iter<'a, Underline>>,
    monochrome_sprites: &'a [MonochromeSprite],
    monochrome_sprites_start: usize,
    monochrome_sprites_iter: Peekable<slice::Iter<'a, MonochromeSprite>>,
    subpixel_sprites: &'a [SubpixelSprite],
    subpixel_sprites_start: usize,
    subpixel_sprites_iter: Peekable<slice::Iter<'a, SubpixelSprite>>,
    polychrome_sprites: &'a [PolychromeSprite],
    polychrome_sprite_transforms: &'a [TransformationMatrix],
    polychrome_sprites_start: usize,
    polychrome_sprites_iter: Peekable<slice::Iter<'a, PolychromeSprite>>,
    surfaces: &'a [PaintSurface],
    surfaces_start: usize,
    surfaces_iter: Peekable<slice::Iter<'a, PaintSurface>>,
}

impl<'a> Iterator for BatchIterator<'a> {
    type Item = PrimitiveBatch<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut orders_and_kinds = [
            (
                self.shadows_iter.peek().map(|s| s.order),
                PrimitiveKind::Shadow,
            ),
            (self.quads_iter.peek().map(|q| q.order), PrimitiveKind::Quad),
            (self.paths_iter.peek().map(|q| q.order), PrimitiveKind::Path),
            (
                self.underlines_iter.peek().map(|u| u.order),
                PrimitiveKind::Underline,
            ),
            (
                self.monochrome_sprites_iter.peek().map(|s| s.order),
                PrimitiveKind::MonochromeSprite,
            ),
            (
                self.subpixel_sprites_iter.peek().map(|s| s.order),
                PrimitiveKind::SubpixelSprite,
            ),
            (
                self.polychrome_sprites_iter.peek().map(|s| s.order),
                PrimitiveKind::PolychromeSprite,
            ),
            (
                self.surfaces_iter.peek().map(|s| s.order),
                PrimitiveKind::Surface,
            ),
        ];
        orders_and_kinds.sort_by_key(|(order, kind)| (order.unwrap_or(u32::MAX), *kind));

        let first = orders_and_kinds[0];
        let second = orders_and_kinds[1];
        let (batch_kind, max_order_and_kind) = if first.0.is_some() {
            (first.1, (second.0.unwrap_or(u32::MAX), second.1))
        } else {
            return None;
        };

        match batch_kind {
            PrimitiveKind::Shadow => {
                let shadows_start = self.shadows_start;
                let mut shadows_end = shadows_start + 1;
                self.shadows_iter.next();
                while self
                    .shadows_iter
                    .next_if(|shadow| (shadow.order, batch_kind) < max_order_and_kind)
                    .is_some()
                {
                    shadows_end += 1;
                }
                self.shadows_start = shadows_end;
                Some(PrimitiveBatch::Shadows(
                    &self.shadows[shadows_start..shadows_end],
                    &self.shadow_transforms[shadows_start..shadows_end],
                ))
            }
            PrimitiveKind::Quad => {
                let quads_start = self.quads_start;
                let mut quads_end = quads_start + 1;
                self.quads_iter.next();
                while self
                    .quads_iter
                    .next_if(|quad| (quad.order, batch_kind) < max_order_and_kind)
                    .is_some()
                {
                    quads_end += 1;
                }
                self.quads_start = quads_end;
                Some(PrimitiveBatch::Quads(
                    &self.quads[quads_start..quads_end],
                    &self.quad_transforms[quads_start..quads_end],
                ))
            }
            PrimitiveKind::Path => {
                let paths_start = self.paths_start;
                let mut paths_end = paths_start + 1;
                self.paths_iter.next();
                while self
                    .paths_iter
                    .next_if(|path| (path.order, batch_kind) < max_order_and_kind)
                    .is_some()
                {
                    paths_end += 1;
                }
                self.paths_start = paths_end;
                Some(PrimitiveBatch::Paths(&self.paths[paths_start..paths_end]))
            }
            PrimitiveKind::Underline => {
                let underlines_start = self.underlines_start;
                let mut underlines_end = underlines_start + 1;
                self.underlines_iter.next();
                while self
                    .underlines_iter
                    .next_if(|underline| (underline.order, batch_kind) < max_order_and_kind)
                    .is_some()
                {
                    underlines_end += 1;
                }
                self.underlines_start = underlines_end;
                Some(PrimitiveBatch::Underlines(
                    &self.underlines[underlines_start..underlines_end],
                    &self.underline_transforms[underlines_start..underlines_end],
                ))
            }
            PrimitiveKind::MonochromeSprite => {
                let texture_id = self.monochrome_sprites_iter.peek().unwrap().tile.texture_id;
                let sprites_start = self.monochrome_sprites_start;
                let mut sprites_end = sprites_start + 1;
                self.monochrome_sprites_iter.next();
                while self
                    .monochrome_sprites_iter
                    .next_if(|sprite| {
                        (sprite.order, batch_kind) < max_order_and_kind
                            && sprite.tile.texture_id == texture_id
                    })
                    .is_some()
                {
                    sprites_end += 1;
                }
                self.monochrome_sprites_start = sprites_end;
                Some(PrimitiveBatch::MonochromeSprites {
                    texture_id,
                    sprites: &self.monochrome_sprites[sprites_start..sprites_end],
                })
            }
            PrimitiveKind::SubpixelSprite => {
                let texture_id = self.subpixel_sprites_iter.peek().unwrap().tile.texture_id;
                let sprites_start = self.subpixel_sprites_start;
                let mut sprites_end = sprites_start + 1;
                self.subpixel_sprites_iter.next();
                while self
                    .subpixel_sprites_iter
                    .next_if(|sprite| {
                        (sprite.order, batch_kind) < max_order_and_kind
                            && sprite.tile.texture_id == texture_id
                    })
                    .is_some()
                {
                    sprites_end += 1;
                }
                self.subpixel_sprites_start = sprites_end;
                Some(PrimitiveBatch::SubpixelSprites {
                    texture_id,
                    sprites: &self.subpixel_sprites[sprites_start..sprites_end],
                })
            }
            PrimitiveKind::PolychromeSprite => {
                let texture_id = self.polychrome_sprites_iter.peek().unwrap().tile.texture_id;
                let sprites_start = self.polychrome_sprites_start;
                let mut sprites_end = self.polychrome_sprites_start + 1;
                self.polychrome_sprites_iter.next();
                while self
                    .polychrome_sprites_iter
                    .next_if(|sprite| {
                        (sprite.order, batch_kind) < max_order_and_kind
                            && sprite.tile.texture_id == texture_id
                    })
                    .is_some()
                {
                    sprites_end += 1;
                }
                self.polychrome_sprites_start = sprites_end;
                Some(PrimitiveBatch::PolychromeSprites {
                    texture_id,
                    sprites: &self.polychrome_sprites[sprites_start..sprites_end],
                    transforms: &self.polychrome_sprite_transforms[sprites_start..sprites_end],
                })
            }
            PrimitiveKind::Surface => {
                let surfaces_start = self.surfaces_start;
                let mut surfaces_end = surfaces_start + 1;
                self.surfaces_iter.next();
                while self
                    .surfaces_iter
                    .next_if(|surface| (surface.order, batch_kind) < max_order_and_kind)
                    .is_some()
                {
                    surfaces_end += 1;
                }
                self.surfaces_start = surfaces_end;
                Some(PrimitiveBatch::Surfaces(
                    &self.surfaces[surfaces_start..surfaces_end],
                ))
            }
        }
    }
}

struct SegmentBatchIterator<'a> {
    scene: &'a Scene,
    pool: &'a SceneSegmentPool,
    order_index: usize,
    current: Option<BatchIterator<'a>>,
}

impl<'a> Iterator for SegmentBatchIterator<'a> {
    type Item = PrimitiveBatch<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(current) = self.current.as_mut() {
                if let Some(batch) = current.next() {
                    return Some(batch);
                }
            }

            let segment_ref = self.scene.segment_order.get(self.order_index)?;
            self.order_index += 1;
            let segment = match segment_ref {
                SceneSegmentRef::Fiber(id) => self.pool.segment(*id),
                SceneSegmentRef::Transient => Some(&self.scene.transient),
            };

            self.current = segment.map(|segment| BatchIterator {
                shadows: &segment.shadows,
                shadow_transforms: &segment.shadow_transforms,
                shadows_start: 0,
                shadows_iter: segment.shadows.iter().peekable(),
                quads: &segment.quads,
                quad_transforms: &segment.quad_transforms,
                quads_start: 0,
                quads_iter: segment.quads.iter().peekable(),
                paths: &segment.paths,
                paths_start: 0,
                paths_iter: segment.paths.iter().peekable(),
                underlines: &segment.underlines,
                underline_transforms: &segment.underline_transforms,
                underlines_start: 0,
                underlines_iter: segment.underlines.iter().peekable(),
                monochrome_sprites: &segment.monochrome_sprites,
                monochrome_sprites_start: 0,
                monochrome_sprites_iter: segment.monochrome_sprites.iter().peekable(),
                subpixel_sprites: &segment.subpixel_sprites,
                subpixel_sprites_start: 0,
                subpixel_sprites_iter: segment.subpixel_sprites.iter().peekable(),
                polychrome_sprites: &segment.polychrome_sprites,
                polychrome_sprite_transforms: &segment.polychrome_sprite_transforms,
                polychrome_sprites_start: 0,
                polychrome_sprites_iter: segment.polychrome_sprites.iter().peekable(),
                surfaces: &segment.surfaces,
                surfaces_start: 0,
                surfaces_iter: segment.surfaces.iter().peekable(),
            });
        }
    }
}

#[derive(Debug)]
#[cfg_attr(
    all(
        any(target_os = "linux", target_os = "freebsd"),
        not(any(feature = "x11", feature = "wayland"))
    ),
    allow(dead_code)
)]
pub(crate) enum PrimitiveBatch<'a> {
    Shadows(&'a [Shadow], &'a [TransformationMatrix]),
    Quads(&'a [Quad], &'a [TransformationMatrix]),
    Paths(&'a [Path<ScaledPixels>]),
    Underlines(&'a [Underline], &'a [TransformationMatrix]),
    MonochromeSprites {
        texture_id: AtlasTextureId,
        sprites: &'a [MonochromeSprite],
    },
    #[cfg_attr(target_os = "macos", allow(dead_code))]
    SubpixelSprites {
        texture_id: AtlasTextureId,
        sprites: &'a [SubpixelSprite],
    },
    PolychromeSprites {
        texture_id: AtlasTextureId,
        sprites: &'a [PolychromeSprite],
        transforms: &'a [TransformationMatrix],
    },
    Surfaces(&'a [PaintSurface]),
}

#[derive(Default, Debug, Clone)]
#[repr(C)]
pub(crate) struct Quad {
    pub order: DrawOrder,
    pub border_style: BorderStyle,
    pub transform_index: u32,
    pub pad: u32,
    pub bounds: Bounds<ScaledPixels>,
    pub content_mask: ContentMask<ScaledPixels>,
    pub background: Background,
    pub border_color: Hsla,
    pub corner_radii: Corners<ScaledPixels>,
    pub border_widths: Edges<ScaledPixels>,
}

impl From<(Quad, TransformationMatrix)> for Primitive {
    fn from((quad, transform): (Quad, TransformationMatrix)) -> Self {
        Primitive::Quad(quad, transform)
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub(crate) struct Underline {
    pub order: DrawOrder,
    pub transform_index: u32,
    pub bounds: Bounds<ScaledPixels>,
    pub content_mask: ContentMask<ScaledPixels>,
    pub color: Hsla,
    pub thickness: ScaledPixels,
    pub wavy: u32,
}

impl From<(Underline, TransformationMatrix)> for Primitive {
    fn from((underline, transform): (Underline, TransformationMatrix)) -> Self {
        Primitive::Underline(underline, transform)
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub(crate) struct Shadow {
    pub order: DrawOrder,
    pub blur_radius: ScaledPixels,
    pub transform_index: u32,
    pub pad: u32,
    pub bounds: Bounds<ScaledPixels>,
    pub corner_radii: Corners<ScaledPixels>,
    pub content_mask: ContentMask<ScaledPixels>,
    pub color: Hsla,
}

impl From<(Shadow, TransformationMatrix)> for Primitive {
    fn from((shadow, transform): (Shadow, TransformationMatrix)) -> Self {
        Primitive::Shadow(shadow, transform)
    }
}

/// The style of a border.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, JsonSchema)]
#[repr(C)]
pub enum BorderStyle {
    /// A solid border.
    #[default]
    Solid = 0,
    /// A dashed border.
    Dashed = 1,
}

/// A data type representing a 2 dimensional transformation that can be applied to an element.
///
/// Matrices are stored in row-major order and applied as `M * position + t` in logical
/// (window) space. Callers should scale the translation exactly once when converting to
/// device space.
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(C)]
pub struct TransformationMatrix {
    /// 2x2 matrix containing rotation and scale,
    /// stored row-major
    pub rotation_scale: [[f32; 2]; 2],
    /// translation vector
    pub translation: [f32; 2],
}

impl Eq for TransformationMatrix {}

impl TransformationMatrix {
    /// The unit matrix, has no effect.
    pub fn unit() -> Self {
        Self {
            rotation_scale: [[1.0, 0.0], [0.0, 1.0]],
            translation: [0.0, 0.0],
        }
    }

    /// Move the origin by a given point in logical pixels.
    pub fn translate(mut self, point: Point<Pixels>) -> Self {
        self.compose(Self {
            rotation_scale: [[1.0, 0.0], [0.0, 1.0]],
            translation: [point.x.0, point.y.0],
        })
    }

    /// Clockwise rotation in radians around the origin
    pub fn rotate(self, angle: Radians) -> Self {
        self.compose(Self {
            rotation_scale: [
                [angle.0.cos(), -angle.0.sin()],
                [angle.0.sin(), angle.0.cos()],
            ],
            translation: [0.0, 0.0],
        })
    }

    /// Scale around the origin
    pub fn scale(self, size: Size<f32>) -> Self {
        self.compose(Self {
            rotation_scale: [[size.width, 0.0], [0.0, size.height]],
            translation: [0.0, 0.0],
        })
    }

    /// Perform matrix multiplication with another transformation
    /// to produce a new transformation that is the result of
    /// applying both transformations: first, `other`, then `self`.
    #[inline]
    pub fn compose(self, other: TransformationMatrix) -> TransformationMatrix {
        if other == Self::unit() {
            return self;
        }
        // Perform matrix multiplication
        TransformationMatrix {
            rotation_scale: [
                [
                    self.rotation_scale[0][0] * other.rotation_scale[0][0]
                        + self.rotation_scale[0][1] * other.rotation_scale[1][0],
                    self.rotation_scale[0][0] * other.rotation_scale[0][1]
                        + self.rotation_scale[0][1] * other.rotation_scale[1][1],
                ],
                [
                    self.rotation_scale[1][0] * other.rotation_scale[0][0]
                        + self.rotation_scale[1][1] * other.rotation_scale[1][0],
                    self.rotation_scale[1][0] * other.rotation_scale[0][1]
                        + self.rotation_scale[1][1] * other.rotation_scale[1][1],
                ],
            ],
            translation: [
                self.translation[0]
                    + self.rotation_scale[0][0] * other.translation[0]
                    + self.rotation_scale[0][1] * other.translation[1],
                self.translation[1]
                    + self.rotation_scale[1][0] * other.translation[0]
                    + self.rotation_scale[1][1] * other.translation[1],
            ],
        }
    }

    /// Returns true when the matrix has no effect (identity rotation/scale and zero translation).
    pub fn is_unit(&self) -> bool {
        *self == Self::unit()
    }

    /// Returns true when only translation is present (rotation/scale is the identity).
    pub fn is_translation_only(&self) -> bool {
        self.rotation_scale == [[1.0, 0.0], [0.0, 1.0]]
    }

    /// Apply the inverse transform to the given point. Returns `None` if the matrix
    /// is not invertible.
    pub fn apply_inverse(&self, point: Point<Pixels>) -> Option<Point<Pixels>> {
        // Inverse of a 2x2 matrix
        let det = self.rotation_scale[0][0] * self.rotation_scale[1][1]
            - self.rotation_scale[0][1] * self.rotation_scale[1][0];
        if det == 0.0 {
            return None;
        }

        let inv = [
            [
                self.rotation_scale[1][1] / det,
                -self.rotation_scale[0][1] / det,
            ],
            [
                -self.rotation_scale[1][0] / det,
                self.rotation_scale[0][0] / det,
            ],
        ];

        let translated = [
            point.x.0 - self.translation[0],
            point.y.0 - self.translation[1],
        ];

        let local = [
            inv[0][0] * translated[0] + inv[0][1] * translated[1],
            inv[1][0] * translated[0] + inv[1][1] * translated[1],
        ];

        Some(Point::new(local[0].into(), local[1].into()))
    }

    /// Apply transformation to a point, mainly useful for debugging
    pub fn apply(&self, point: Point<Pixels>) -> Point<Pixels> {
        let input = [point.x.0, point.y.0];
        let mut output = self.translation;
        for (i, output_cell) in output.iter_mut().enumerate() {
            for (k, input_cell) in input.iter().enumerate() {
                *output_cell += self.rotation_scale[i][k] * *input_cell;
            }
        }
        Point::new(output[0].into(), output[1].into())
    }
}

impl Default for TransformationMatrix {
    fn default() -> Self {
        Self::unit()
    }
}

#[derive(Clone, Debug)]
#[repr(C)]
pub(crate) struct MonochromeSprite {
    pub order: DrawOrder,
    pub transform_index: u32,
    pub bounds: Bounds<ScaledPixels>,
    pub content_mask: ContentMask<ScaledPixels>,
    pub color: Hsla,
    pub tile: AtlasTile,
    pub transformation: TransformationMatrix,
}

impl From<MonochromeSprite> for Primitive {
    fn from(sprite: MonochromeSprite) -> Self {
        Primitive::MonochromeSprite(sprite)
    }
}

#[derive(Clone, Debug)]
#[repr(C)]
pub(crate) struct SubpixelSprite {
    pub order: DrawOrder,
    pub transform_index: u32,
    pub bounds: Bounds<ScaledPixels>,
    pub content_mask: ContentMask<ScaledPixels>,
    pub color: Hsla,
    pub tile: AtlasTile,
    pub transformation: TransformationMatrix,
}

impl From<SubpixelSprite> for Primitive {
    fn from(sprite: SubpixelSprite) -> Self {
        Primitive::SubpixelSprite(sprite)
    }
}

#[derive(Clone, Debug)]
#[repr(C)]
pub(crate) struct PolychromeSprite {
    pub order: DrawOrder,
    pub transform_index: u32,
    pub grayscale: bool,
    pub opacity: f32,
    pub bounds: Bounds<ScaledPixels>,
    pub content_mask: ContentMask<ScaledPixels>,
    pub corner_radii: Corners<ScaledPixels>,
    pub tile: AtlasTile,
}

impl From<(PolychromeSprite, TransformationMatrix)> for Primitive {
    fn from((sprite, transform): (PolychromeSprite, TransformationMatrix)) -> Self {
        Primitive::PolychromeSprite(sprite, transform)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct PaintSurface {
    pub order: DrawOrder,
    pub transform_index: u32,
    pub bounds: Bounds<ScaledPixels>,
    pub content_mask: ContentMask<ScaledPixels>,
    #[cfg(target_os = "macos")]
    pub image_buffer: core_video::pixel_buffer::CVPixelBuffer,
}

impl From<PaintSurface> for Primitive {
    fn from(surface: PaintSurface) -> Self {
        Primitive::Surface(surface)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct PathId(pub(crate) usize);

/// A line made up of a series of vertices and control points.
#[derive(Clone, Debug)]
pub struct Path<P: Clone + Debug + Default + PartialEq> {
    pub(crate) id: PathId,
    pub(crate) order: DrawOrder,
    pub(crate) transform_index: u32,
    pub(crate) bounds: Bounds<P>,
    pub(crate) content_mask: ContentMask<P>,
    pub(crate) vertices: Vec<PathVertex<P>>,
    pub(crate) color: Background,
    start: Point<P>,
    current: Point<P>,
    contour_count: usize,
}

impl Path<Pixels> {
    /// Create a new path with the given starting point.
    pub fn new(start: Point<Pixels>) -> Self {
        Self {
            id: PathId(0),
            order: DrawOrder::default(),
            transform_index: 0,
            vertices: Vec::new(),
            start,
            current: start,
            bounds: Bounds {
                origin: start,
                size: Default::default(),
            },
            content_mask: Default::default(),
            color: Default::default(),
            contour_count: 0,
        }
    }

    /// Scale this path by the given factor.
    pub fn scale(&self, factor: f32) -> Path<ScaledPixels> {
        Path {
            id: self.id,
            order: self.order,
            transform_index: self.transform_index,
            bounds: self.bounds.scale(factor),
            content_mask: self.content_mask.scale(factor),
            vertices: self
                .vertices
                .iter()
                .map(|vertex| vertex.scale(factor))
                .collect(),
            start: self.start.map(|start| start.scale(factor)),
            current: self.current.scale(factor),
            contour_count: self.contour_count,
            color: self.color,
        }
    }

    /// Move the start, current point to the given point.
    pub fn move_to(&mut self, to: Point<Pixels>) {
        self.contour_count += 1;
        self.start = to;
        self.current = to;
    }

    /// Draw a straight line from the current point to the given point.
    pub fn line_to(&mut self, to: Point<Pixels>) {
        self.contour_count += 1;
        if self.contour_count > 1 {
            self.push_triangle(
                (self.start, self.current, to),
                (point(0., 1.), point(0., 1.), point(0., 1.)),
            );
        }
        self.current = to;
    }

    /// Draw a curve from the current point to the given point, using the given control point.
    pub fn curve_to(&mut self, to: Point<Pixels>, ctrl: Point<Pixels>) {
        self.contour_count += 1;
        if self.contour_count > 1 {
            self.push_triangle(
                (self.start, self.current, to),
                (point(0., 1.), point(0., 1.), point(0., 1.)),
            );
        }

        self.push_triangle(
            (self.current, ctrl, to),
            (point(0., 0.), point(0.5, 0.), point(1., 1.)),
        );
        self.current = to;
    }

    /// Push a triangle to the Path.
    pub fn push_triangle(
        &mut self,
        xy: (Point<Pixels>, Point<Pixels>, Point<Pixels>),
        st: (Point<f32>, Point<f32>, Point<f32>),
    ) {
        self.bounds = self
            .bounds
            .union(&Bounds {
                origin: xy.0,
                size: Default::default(),
            })
            .union(&Bounds {
                origin: xy.1,
                size: Default::default(),
            })
            .union(&Bounds {
                origin: xy.2,
                size: Default::default(),
            });

        self.vertices.push(PathVertex {
            xy_position: xy.0,
            st_position: st.0,
            content_mask: Default::default(),
        });
        self.vertices.push(PathVertex {
            xy_position: xy.1,
            st_position: st.1,
            content_mask: Default::default(),
        });
        self.vertices.push(PathVertex {
            xy_position: xy.2,
            st_position: st.2,
            content_mask: Default::default(),
        });
    }
}

impl<T> Path<T>
where
    T: Clone + Debug + Default + PartialEq + PartialOrd + Add<T, Output = T> + Sub<Output = T>,
{
    #[allow(unused)]
    pub(crate) fn clipped_bounds(&self) -> Bounds<T> {
        self.bounds.clone()
    }
}

impl From<Path<ScaledPixels>> for Primitive {
    fn from(path: Path<ScaledPixels>) -> Self {
        Primitive::Path(path)
    }
}

#[derive(Clone, Debug)]
#[repr(C)]
pub(crate) struct PathVertex<P: Clone + Debug + Default + PartialEq> {
    pub(crate) xy_position: Point<P>,
    pub(crate) st_position: Point<f32>,
    pub(crate) content_mask: ContentMask<P>,
}

impl PathVertex<Pixels> {
    pub fn scale(&self, factor: f32) -> PathVertex<ScaledPixels> {
        PathVertex {
            xy_position: self.xy_position.scale(factor),
            st_position: self.st_position,
            content_mask: self.content_mask.scale(factor),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scene_segment_pool_alloc_returns_unique_ids() {
        let mut pool = SceneSegmentPool::default();

        let id1 = pool.alloc_segment();
        let id2 = pool.alloc_segment();
        let id3 = pool.alloc_segment();

        assert_ne!(id1, id2);
        assert_ne!(id2, id3);
        assert_ne!(id1, id3);
    }

    #[test]
    fn scene_segment_pool_remove_and_realloc() {
        let mut pool = SceneSegmentPool::default();

        let id1 = pool.alloc_segment();
        let id2 = pool.alloc_segment();

        // Remove first segment
        pool.remove_segment(id1);

        // Allocate again - should not reuse the removed key (generation increments)
        let id3 = pool.alloc_segment();
        assert_ne!(id3, id1, "Removed segment key should not be reused");

        // id2 should still be valid
        assert!(pool.segment(id2).is_some());
        assert!(pool.segment(id3).is_some());
    }

    #[test]
    fn scene_segment_pool_reset_segment_clears_content() {
        let mut pool = SceneSegmentPool::default();
        let id = pool.alloc_segment();

        // Get the segment and verify it exists
        let segment = pool.segment_mut(id);
        assert!(segment.is_some());

        // Reset the segment
        pool.reset_segment(id, 42);

        // Verify the mutation_epoch was updated
        let segment = pool.segment(id);
        assert!(segment.is_some());
        assert_eq!(segment.unwrap().mutated_epoch, 42);
    }

    #[test]
    fn scene_segment_pool_reset_all_clears_all_segments() {
        let mut pool = SceneSegmentPool::default();

        // Allocate multiple segments
        let id1 = pool.alloc_segment();
        let id2 = pool.alloc_segment();
        let id3 = pool.alloc_segment();

        // Reset all with a specific epoch
        pool.reset_all(100);

        // Verify all segments were reset
        assert_eq!(pool.segment(id1).unwrap().mutated_epoch, 100);
        assert_eq!(pool.segment(id2).unwrap().mutated_epoch, 100);
        assert_eq!(pool.segment(id3).unwrap().mutated_epoch, 100);
    }

    #[test]
    fn scene_segment_pool_reset_all_preserves_removed_segments() {
        let mut pool = SceneSegmentPool::default();

        let id1 = pool.alloc_segment();
        let id2 = pool.alloc_segment();

        // Remove one segment
        pool.remove_segment(id1);

        // Reset all
        pool.reset_all(50);

        // id1 should still be removed (None)
        assert!(pool.segment(id1).is_none());

        // id2 should be reset
        assert_eq!(pool.segment(id2).unwrap().mutated_epoch, 50);
    }

    #[test]
    fn scene_mutation_epoch_starts_at_zero() {
        let scene = Scene::default();
        assert_eq!(scene.mutation_epoch(), 0);
    }

    #[test]
    fn scene_segment_id_equality() {
        let mut pool = SceneSegmentPool::default();
        let id1 = pool.alloc_segment();
        let id2 = pool.alloc_segment();
        assert_ne!(id1, id2);
        assert_eq!(id1, id1);
    }
}
