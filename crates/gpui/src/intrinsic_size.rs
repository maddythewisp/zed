//! Intrinsic sizing system for two-phase layout.
//!
//! Intrinsic sizes represent an element's natural dimensions independent of
//! its position in the layout tree. By computing and caching these separately
//! from layout positioning, we can avoid expensive recomputation when only
//! positions (not sizes) change.

use crate::{App, AvailableSpace, GlobalElementId, Pixels, Size, Window, size};
use collections::FxHasher;
use std::hash::{Hash, Hasher};

/// Cached intrinsic sizing information for an element.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct IntrinsicSize {
    /// Minimum content size - the smallest this element can be.
    /// For text: width of longest word, height of one line.
    /// For containers: aggregate of children's min-content.
    pub min_content: Size<Pixels>,

    /// Maximum content size - the natural size without constraints.
    /// For text: width of unwrapped text, height of one line.
    /// For containers: aggregate of children's max-content.
    pub max_content: Size<Pixels>,
}

/// Input key for intrinsic size computation.
/// If this changes, intrinsic size must be recomputed.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct SizingInput {
    /// Hash of content that affects sizing (text content, image source, etc.)
    pub content_hash: u64,
    /// Hash of styles that affect sizing (font-size, padding, etc.)
    pub style_hash: u64,
}

impl SizingInput {
    /// Create a new sizing input with explicit hashes.
    pub fn new(content_hash: u64, style_hash: u64) -> Self {
        Self {
            content_hash,
            style_hash,
        }
    }

    /// Create a sizing input from hashable content.
    pub fn from_content<T: Hash>(content: &T) -> Self {
        let mut hasher = FxHasher::default();
        content.hash(&mut hasher);
        Self {
            content_hash: hasher.finish(),
            style_hash: 0,
        }
    }

    /// Add a style hash to this sizing input.
    pub fn with_style<T: Hash>(mut self, style: &T) -> Self {
        let mut hasher = FxHasher::default();
        style.hash(&mut hasher);
        self.style_hash = hasher.finish();
        self
    }
}

/// Cached intrinsic size with its input key.
#[derive(Clone, Debug)]
pub struct CachedIntrinsicSize {
    /// The cached intrinsic size.
    pub size: IntrinsicSize,
}

/// Result of computing intrinsic size.
pub struct IntrinsicSizeResult {
    /// The computed intrinsic size.
    pub size: IntrinsicSize,
    /// The input key used for cache invalidation.
    pub input: SizingInput,
}

/// Sizing context passed to elements during intrinsic size computation.
pub struct SizingCtx<'a> {
    /// The fiber's global element ID.
    pub fiber_id: GlobalElementId,
    /// Window access.
    pub window: &'a mut Window,
    /// App context.
    pub cx: &'a mut App,
    /// The current rem size for unit conversion.
    pub rem_size: Pixels,
    /// The current scale factor for unit conversion.
    pub scale_factor: f32,
}

/// Query type for measure functions during layout.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SizeQuery {
    /// Return min-content size
    MinContent,
    /// Return max-content size
    MaxContent,
    /// Return size for definite width constraint
    ForWidth(Pixels),
    /// Return size for definite height constraint
    ForHeight(Pixels),
    /// Return size for definite constraints
    Definite(Size<Pixels>),
}

impl SizeQuery {
    /// Create a size query from taffy's known and available size parameters.
    pub fn from_taffy(known: Size<Option<Pixels>>, available: Size<AvailableSpace>) -> Self {
        match (known.width, known.height) {
            (Some(w), Some(h)) => SizeQuery::Definite(size(w, h)),
            (Some(w), None) => SizeQuery::ForWidth(w),
            (None, Some(h)) => SizeQuery::ForHeight(h),
            (None, None) => match available.width {
                AvailableSpace::MinContent => SizeQuery::MinContent,
                AvailableSpace::MaxContent => SizeQuery::MaxContent,
                AvailableSpace::Definite(w) => SizeQuery::ForWidth(w),
            },
        }
    }
}
