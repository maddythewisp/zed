use crate::{AvailableSpace, Pixels, Size};
use collections::FxHashMap;
use std::hash::Hash;

const DEFAULT_BUCKET_SIZE_PX: f32 = 12.0;

/// A quantized representation of layout constraints for cache lookup.
/// Constraints are bucketed to allow cache hits when constraints change slightly.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ConstraintsBucket {
    /// Bucketed width constraint.
    pub width_bucket: u16,
    /// Bucketed max height constraint.
    pub max_height_bucket: u16,
}

impl ConstraintsBucket {
    /// Create a constraints bucket from available space with a custom bucket size.
    pub fn from_available_space(
        available_space: Size<AvailableSpace>,
        bucket_size_px: f32,
    ) -> Self {
        Self {
            width_bucket: bucket_available_space(available_space.width, bucket_size_px),
            max_height_bucket: bucket_available_space(available_space.height, bucket_size_px),
        }
    }

    /// Create a constraints bucket from available space with the default bucket size.
    pub fn from_available_space_default(available_space: Size<AvailableSpace>) -> Self {
        Self::from_available_space(available_space, DEFAULT_BUCKET_SIZE_PX)
    }
}

fn bucket_available_space(space: AvailableSpace, bucket_size_px: f32) -> u16 {
    match space {
        AvailableSpace::Definite(width) => bucket_pixels(width, bucket_size_px),
        AvailableSpace::MinContent => u16::MAX - 1,
        AvailableSpace::MaxContent => u16::MAX,
    }
}

fn bucket_pixels(pixels: Pixels, bucket_size_px: f32) -> u16 {
    let width_px = pixels.0;
    if width_px.is_nan() || width_px <= 0.0 || bucket_size_px <= 0.0 {
        return 0;
    }

    let bucketed = (width_px / bucket_size_px).floor();
    if bucketed >= u16::MAX as f32 {
        u16::MAX
    } else {
        bucketed as u16
    }
}

/// Policy for hysteresis when comparing measured sizes.
#[derive(Clone, Copy, Debug)]
pub enum HysteresisPolicy {
    /// Ignore size changes smaller than the given threshold.
    Pixels(Pixels),
    /// Quantize height to multiples of the given line height.
    LineUnits(Pixels),
}

impl Default for HysteresisPolicy {
    fn default() -> Self {
        Self::Pixels(Pixels(0.5))
    }
}

/// A key for looking up cached measurements.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct MeasurementKey {
    callsite: usize,
    discriminator: u64,
}

impl MeasurementKey {
    /// Create a measurement key for the current callsite with an optional discriminator.
    #[track_caller]
    pub fn for_callsite(discriminator: u64) -> Self {
        let callsite = core::panic::Location::caller() as *const core::panic::Location<'static>;
        Self {
            callsite: callsite as usize,
            discriminator,
        }
    }
}

/// A cache for storing measured element sizes keyed by constraints and generation.
#[derive(Debug)]
pub struct MeasurementCache<K: Eq + Hash> {
    entries: FxHashMap<(K, ConstraintsBucket, u64), Size<Pixels>>,
    /// The current generation counter for cache entries.
    pub current_generation: u64,
    /// The hysteresis policy for comparing sizes.
    pub hysteresis: HysteresisPolicy,
}

impl<K: Eq + Hash> Default for MeasurementCache<K> {
    fn default() -> Self {
        Self {
            entries: FxHashMap::default(),
            current_generation: 0,
            hysteresis: HysteresisPolicy::default(),
        }
    }
}

impl<K: Eq + Hash> MeasurementCache<K> {
    /// Create a new measurement cache with the given hysteresis policy.
    pub fn with_hysteresis(hysteresis: HysteresisPolicy) -> Self {
        Self {
            entries: FxHashMap::default(),
            current_generation: 0,
            hysteresis,
        }
    }
}

impl<K: Eq + Hash + Copy> MeasurementCache<K> {
    /// Get a cached measurement for the exact generation.
    pub fn get(
        &self,
        key: K,
        constraints: ConstraintsBucket,
        generation: u64,
    ) -> Option<Size<Pixels>> {
        self.entries.get(&(key, constraints, generation)).copied()
    }

    /// Get a cached measurement, accepting entries from the previous generation if current is not found.
    pub fn get_stale_ok(
        &self,
        key: K,
        constraints: ConstraintsBucket,
        current_generation: u64,
    ) -> Option<(Size<Pixels>, u64)> {
        if let Some(size) = self.get(key, constraints, current_generation) {
            return Some((size, current_generation));
        }
        if current_generation > 0 {
            if let Some(size) = self.get(key, constraints, current_generation - 1) {
                return Some((size, current_generation - 1));
            }
        }
        None
    }

    /// Insert a measurement into the cache. Returns true if the value changed.
    pub fn insert(
        &mut self,
        key: K,
        constraints: ConstraintsBucket,
        generation: u64,
        size: Size<Pixels>,
    ) -> bool {
        let quantized = match self.hysteresis {
            HysteresisPolicy::Pixels(threshold) => {
                if let Some(existing) = self.entries.get(&(key, constraints, generation)) {
                    if approx_equal_size(*existing, size, threshold) {
                        return false;
                    }
                }
                size
            }
            HysteresisPolicy::LineUnits(line_height) => Size {
                width: size.width,
                height: quantize_to_line_units(size.height, line_height),
            },
        };

        self.entries.insert((key, constraints, generation), quantized);
        true
    }

    /// Remove entries older than the previous generation.
    pub fn evict_stale_generations(&mut self, current_generation: u64) {
        let min_generation = current_generation.saturating_sub(1);
        self.entries.retain(|(_, _, generation), _| *generation >= min_generation);
    }
}

fn approx_equal_size(a: Size<Pixels>, b: Size<Pixels>, threshold: Pixels) -> bool {
    (a.width.0 - b.width.0).abs() < threshold.0 && (a.height.0 - b.height.0).abs() < threshold.0
}

fn quantize_to_line_units(height: Pixels, line_height: Pixels) -> Pixels {
    if line_height.0 <= 0.0 {
        return height;
    }
    let lines = (height.0 / line_height.0).ceil();
    Pixels(lines * line_height.0)
}

