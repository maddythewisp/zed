use gpui_macros::Element;

use crate::{
    AnyElement, AnyImageCache, App, Asset, AssetLogger, AvailableSpace, DefiniteLength, Display,
    ElementId, ElementImpl, Entity, GlobalElementId, Image, ImageCache,
    InteractiveElement, Interactivity, IntoElement, Length, ObjectFit, Pixels, RenderImage,
    Resource, SharedString, SharedUri, Size, Style, StyleRefinement, Styled, Task, UpdateResult,
    Window, px, taffy::ToTaffy,
};
use anyhow::{Context as _, Result};
use refineable::Refineable;

use collections::FxHasher;
use futures::{AsyncReadExt, Future};
use image::{
    AnimationDecoder, DynamicImage, Frame, ImageError, ImageFormat, Rgba,
    codecs::{gif::GifDecoder, webp::WebPDecoder},
};
use smallvec::SmallVec;
use std::{
    any::TypeId,
    fs,
    hash::{Hash, Hasher},
    io::{self, Cursor},
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
    time::{Duration, Instant},
};
use thiserror::Error;
use util::ResultExt;


/// The delay before showing the loading state.
pub const LOADING_DELAY: Duration = Duration::from_millis(200);

/// A type alias to the resource loader that the `img()` element uses.
///
/// Note: that this is only for Resources, like URLs or file paths.
/// Custom loaders, or external images will not use this asset loader
pub type ImgResourceLoader = AssetLogger<ImageAssetLoader>;

/// A source of image content.
#[derive(Clone)]
pub enum ImageSource {
    /// The image content will be loaded from some resource location
    Resource(Resource),
    /// Cached image data
    Render(Arc<RenderImage>),
    /// Cached image data
    Image(Arc<Image>),
    /// A custom loading function to use
    Custom(Arc<dyn Fn(&mut Window, &mut App) -> Option<Result<Arc<RenderImage>, ImageCacheError>>>),
}

fn is_uri(uri: &str) -> bool {
    http_client::Uri::from_str(uri).is_ok()
}

impl From<SharedUri> for ImageSource {
    fn from(value: SharedUri) -> Self {
        Self::Resource(Resource::Uri(value))
    }
}

impl<'a> From<&'a str> for ImageSource {
    fn from(s: &'a str) -> Self {
        if is_uri(s) {
            Self::Resource(Resource::Uri(s.to_string().into()))
        } else {
            Self::Resource(Resource::Embedded(s.to_string().into()))
        }
    }
}

impl From<String> for ImageSource {
    fn from(s: String) -> Self {
        if is_uri(&s) {
            Self::Resource(Resource::Uri(s.into()))
        } else {
            Self::Resource(Resource::Embedded(s.into()))
        }
    }
}

impl From<SharedString> for ImageSource {
    fn from(s: SharedString) -> Self {
        s.as_ref().into()
    }
}

impl From<&Path> for ImageSource {
    fn from(value: &Path) -> Self {
        Self::Resource(value.to_path_buf().into())
    }
}

impl From<Arc<Path>> for ImageSource {
    fn from(value: Arc<Path>) -> Self {
        Self::Resource(value.into())
    }
}

impl From<PathBuf> for ImageSource {
    fn from(value: PathBuf) -> Self {
        Self::Resource(value.into())
    }
}

impl From<Arc<RenderImage>> for ImageSource {
    fn from(value: Arc<RenderImage>) -> Self {
        Self::Render(value)
    }
}

impl From<Arc<Image>> for ImageSource {
    fn from(value: Arc<Image>) -> Self {
        Self::Image(value)
    }
}

impl<F> From<F> for ImageSource
where
    F: Fn(&mut Window, &mut App) -> Option<Result<Arc<RenderImage>, ImageCacheError>> + 'static,
{
    fn from(value: F) -> Self {
        Self::Custom(Arc::new(value))
    }
}

/// The style of an image element.
pub struct ImageStyle {
    grayscale: bool,
    object_fit: ObjectFit,
    loading: Option<Box<dyn Fn() -> AnyElement>>,
    fallback: Option<Box<dyn Fn() -> AnyElement>>,
}

impl Default for ImageStyle {
    fn default() -> Self {
        Self {
            grayscale: false,
            object_fit: ObjectFit::Contain,
            loading: None,
            fallback: None,
        }
    }
}

/// Style an image element.
pub trait StyledImage: Sized {
    /// Get a mutable [ImageStyle] from the element.
    fn image_style(&mut self) -> &mut ImageStyle;

    /// Set the image to be displayed in grayscale.
    fn grayscale(mut self, grayscale: bool) -> Self {
        self.image_style().grayscale = grayscale;
        self
    }

    /// Set the object fit for the image.
    fn object_fit(mut self, object_fit: ObjectFit) -> Self {
        self.image_style().object_fit = object_fit;
        self
    }

    /// Set a fallback function that will be invoked to render an error view should
    /// the image fail to load.
    fn with_fallback(mut self, fallback: impl Fn() -> AnyElement + 'static) -> Self {
        self.image_style().fallback = Some(Box::new(fallback));
        self
    }

    /// Set a fallback function that will be invoked to render a view while the image
    /// is still being loaded.
    fn with_loading(mut self, loading: impl Fn() -> AnyElement + 'static) -> Self {
        self.image_style().loading = Some(Box::new(loading));
        self
    }
}

impl StyledImage for Img {
    fn image_style(&mut self) -> &mut ImageStyle {
        &mut self.style
    }
}

/// An image element.
#[derive(Element)]
#[element(crate = crate)]
pub struct Img {
    interactivity: Interactivity,
    source: ImageSource,
    style: ImageStyle,
    image_cache: Option<AnyImageCache>,
}

/// Create a new image element.
#[track_caller]
pub fn img(source: impl Into<ImageSource>) -> Img {
    Img {
        interactivity: Interactivity::new(),
        source: source.into(),
        style: ImageStyle::default(),
        image_cache: None,
    }
}

impl Img {
    /// A list of all format extensions currently supported by this img element
    pub fn extensions() -> &'static [&'static str] {
        // This is the list in [image::ImageFormat::from_extension] + `svg`
        &[
            "avif", "jpg", "jpeg", "png", "gif", "webp", "tif", "tiff", "tga", "dds", "bmp", "ico",
            "hdr", "exr", "pbm", "pam", "ppm", "pgm", "ff", "farbfeld", "qoi", "svg",
        ]
    }

    /// Sets the image cache for the current node.
    ///
    /// If the `image_cache` is not explicitly provided, the function will determine the image cache by:
    ///
    /// 1. Checking if any ancestor node of the current node contains an `ImageCacheElement`, If such a node exists, the image cache specified by that ancestor will be used.
    /// 2. If no ancestor node contains an `ImageCacheElement`, the global image cache will be used as a fallback.
    ///
    /// This mechanism provides a flexible way to manage image caching, allowing precise control when needed,
    /// while ensuring a default behavior when no cache is explicitly specified.
    #[inline]
    pub fn image_cache<I: ImageCache>(self, image_cache: &Entity<I>) -> Self {
        Self {
            image_cache: Some(image_cache.clone().into()),
            ..self
        }
    }

    pub(crate) fn take_interactivity(&mut self) -> Interactivity {
        std::mem::take(&mut self.interactivity)
    }
}

impl ElementImpl for Img {
    fn id(&self) -> Option<ElementId> {
        self.interactivity.element_id.clone()
    }

    fn source_location(&self) -> Option<&'static std::panic::Location<'static>> {
        self.interactivity.source_location()
    }

    fn cached_style(&self) -> Option<&StyleRefinement> {
        Some(&self.interactivity.base_style)
    }

    fn create_render_node(&mut self) -> Option<Box<dyn crate::RenderNode>> {
        let loading_factory = self.style.loading.take().map(Arc::from);
        let fallback_factory = self.style.fallback.take().map(Arc::from);
        Some(Box::new(ImgNode::new(
            self.take_interactivity(),
            self.source.clone(),
            self.style.grayscale,
            self.style.object_fit,
            self.image_cache.take(),
            loading_factory,
            fallback_factory,
        )))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<ImgNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn crate::RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        if let Some(img_node) = node.as_any_mut().downcast_mut::<ImgNode>() {
            let interactivity = self.take_interactivity();
            let update_result = img_node.interactivity.diff_styles(&interactivity);

            let source = self.source.clone();
            let grayscale = self.style.grayscale;
            let object_fit = self.style.object_fit;
            let image_cache = self.image_cache.take();
            let loading_factory = self.style.loading.take().map(Arc::from);
            let fallback_factory = self.style.fallback.take().map(Arc::from);

            let image_cache_changed = match (&img_node.image_cache, &image_cache) {
                (Some(a), Some(b)) => !a.identity_eq(b),
                (None, None) => false,
                _ => true,
            };

            let content_changed = !img_node.source.identity_eq(&source)
                || img_node.grayscale != grayscale
                || img_node.object_fit != object_fit
                || image_cache_changed
                || img_node.loading_factory.is_some() != loading_factory.is_some()
                || img_node.fallback_factory.is_some() != fallback_factory.is_some();

            let mut layout_changed = update_result.layout_changed;
            let mut paint_changed = update_result.paint_changed;
            if content_changed {
                layout_changed = true;
                paint_changed = true;
            }
            if layout_changed {
                paint_changed = true;
            }

            img_node.update_from(
                interactivity,
                source,
                grayscale,
                object_fit,
                image_cache,
                loading_factory,
                fallback_factory,
            );
            Some(UpdateResult {
                layout_changed,
                paint_changed,
            })
        } else {
            None
        }
    }
}

impl Styled for Img {
    fn style(&mut self) -> &mut StyleRefinement {
        &mut self.interactivity.base_style
    }
}

impl InteractiveElement for Img {
    fn interactivity(&mut self) -> &mut Interactivity {
        &mut self.interactivity
    }
}

impl IntoElement for Img {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

/// Retained render node for Img elements.
///
/// This node owns all image-specific data and state, enabling fully
/// node-handled layout, prepaint, and paint phases.
pub(crate) struct ImgNode {
    /// Interactivity state for this image element.
    pub interactivity: Interactivity,
    /// The image source.
    pub source: ImageSource,
    /// Whether to render in grayscale.
    pub grayscale: bool,
    /// How to fit the image within bounds.
    pub object_fit: ObjectFit,
    /// Optional image cache override.
    pub image_cache: Option<AnyImageCache>,
    /// Factory function to create loading indicator element.
    pub loading_factory: Option<Arc<dyn Fn() -> AnyElement>>,
    /// Factory function to create fallback element on error.
    pub fallback_factory: Option<Arc<dyn Fn() -> AnyElement>>,

    // --- Persistent state (retained across frames) ---
    /// Current animation frame index.
    frame_index: usize,
    /// Timestamp of last frame change for animation.
    last_frame_time: Option<Instant>,
    /// Loading state: (start time, notification task).
    started_loading: Option<(Instant, Task<()>)>,
    pending_paint_style: Option<Style>,

    // --- Layout cache (computed in layout_begin) ---
    /// Cached taffy style computed from interactivity.base_style.
    cached_taffy_style: taffy::style::Style,
    /// Cached image data result for current frame.
    cached_image_data: Option<Result<Arc<RenderImage>, ImageCacheError>>,
    /// Image cache to use (either explicit or inherited).
    effective_image_cache: Option<AnyImageCache>,
}

impl ImgNode {
    /// Create a new ImgNode from descriptor data.
    pub fn new(
        interactivity: Interactivity,
        source: ImageSource,
        grayscale: bool,
        object_fit: ObjectFit,
        image_cache: Option<AnyImageCache>,
        loading_factory: Option<Arc<dyn Fn() -> AnyElement>>,
        fallback_factory: Option<Arc<dyn Fn() -> AnyElement>>,
    ) -> Self {
        Self {
            interactivity,
            source,
            grayscale,
            object_fit,
            image_cache,
            loading_factory,
            fallback_factory,
            frame_index: 0,
            last_frame_time: None,
            started_loading: None,
            pending_paint_style: None,
            cached_taffy_style: taffy::style::Style::default(),
            cached_image_data: None,
            effective_image_cache: None,
        }
    }

    /// Update this node from a descriptor.
    pub fn update_from(
        &mut self,
        interactivity: Interactivity,
        source: ImageSource,
        grayscale: bool,
        object_fit: ObjectFit,
        image_cache: Option<AnyImageCache>,
        loading_factory: Option<Arc<dyn Fn() -> AnyElement>>,
        fallback_factory: Option<Arc<dyn Fn() -> AnyElement>>,
    ) {
        self.interactivity = interactivity;
        self.source = source;
        self.grayscale = grayscale;
        self.object_fit = object_fit;
        self.image_cache = image_cache;
        self.loading_factory = loading_factory;
        self.fallback_factory = fallback_factory;
    }
}

impl crate::RenderNode for ImgNode {
    fn needs_child_bounds(&self) -> bool {
        false
    }

    fn layout_begin(&mut self, ctx: &mut crate::LayoutCtx) -> crate::LayoutFrame {
        let mut frame = crate::LayoutFrame {
            handled: true,
            ..Default::default()
        };

        // Determine effective image cache (explicit or inherited from stack)
        self.effective_image_cache = self
            .image_cache
            .clone()
            .or_else(|| ctx.window.image_cache_stack.last().cloned());

        // Fetch/update image data and handle animation state
        let image_result =
            self.source
                .use_data(self.effective_image_cache.clone(), ctx.window, ctx.cx);

        match &image_result {
            Some(Ok(data)) => {
                // Image loaded successfully - handle animation
                let frame_count = data.frame_count();
                if frame_count > 1 {
                    let current_time = Instant::now();
                    if let Some(last_frame_time) = self.last_frame_time {
                        let elapsed = current_time - last_frame_time;
                        let frame_duration = Duration::from(data.delay(self.frame_index));

                        if elapsed >= frame_duration {
                            self.frame_index = (self.frame_index + 1) % frame_count;
                            self.last_frame_time = Some(current_time - (elapsed - frame_duration));
                        }
                    } else {
                        self.last_frame_time = Some(current_time);
                    }
                    ctx.window.request_animation_frame();
                }
                self.started_loading = None;
            }
            Some(Err(_)) => {
                self.started_loading = None;
            }
            None => {
                if self.started_loading.is_none() {
                    // Start the loading delay timer
                    let current_view = ctx.window.current_view();
                    let task = ctx.window.spawn(ctx.cx, async move |cx| {
                        cx.background_executor().timer(LOADING_DELAY).await;
                        cx.update(move |_, cx| {
                            cx.notify(current_view);
                        })
                        .log_err();
                    });
                    self.started_loading = Some((Instant::now(), task));
                }
            }
        }

        self.cached_image_data = image_result;

        // Compute taffy style from interactivity.base_style
        let mut style = Style::default();
        style.refine(&self.interactivity.base_style);

        if let Some(text_style) = style.text_style() {
            ctx.window.text_style_stack.push(text_style.clone());
            frame.pushed_text_style = true;
        }

        // If image loaded, adjust style for aspect ratio and intrinsic sizing
        if let Some(Ok(ref data)) = self.cached_image_data {
            let image_size = data.render_size(self.frame_index);
            style.aspect_ratio = Some(image_size.width / image_size.height);

            if let Length::Auto = style.size.width {
                style.size.width = match style.size.height {
                    Length::Definite(DefiniteLength::Absolute(abs_length)) => {
                        let height_px = abs_length.to_pixels(ctx.rem_size);
                        Length::Definite(
                            px(image_size.width.0 * height_px.0 / image_size.height.0).into(),
                        )
                    }
                    _ => Length::Definite(image_size.width.into()),
                };
            }

            if let Length::Auto = style.size.height {
                style.size.height = match style.size.width {
                    Length::Definite(DefiniteLength::Absolute(abs_length)) => {
                        let width_px = abs_length.to_pixels(ctx.rem_size);
                        Length::Definite(
                            px(image_size.height.0 * width_px.0 / image_size.width.0).into(),
                        )
                    }
                    _ => Length::Definite(image_size.height.into()),
                };
            }
        }

        self.cached_taffy_style = style.to_taffy(ctx.rem_size, ctx.scale_factor);

        frame
    }

    fn layout_end(&mut self, ctx: &mut crate::LayoutCtx, frame: crate::LayoutFrame) {
        if frame.pushed_text_style {
            ctx.window.text_style_stack.pop();
        }
    }

    fn taffy_style(&self, _rem_size: crate::Pixels, _scale_factor: f32) -> taffy::style::Style {
        // Return the pre-computed style from layout_begin
        self.cached_taffy_style.clone()
    }

    fn compute_intrinsic_size(
        &mut self,
        _ctx: &mut crate::SizingCtx,
    ) -> crate::IntrinsicSizeResult {
        let mut hasher = FxHasher::default();

        match &self.source {
            ImageSource::Resource(resource) => {
                0u8.hash(&mut hasher);
                resource.hash(&mut hasher);
            }
            ImageSource::Render(image) => {
                1u8.hash(&mut hasher);
                (Arc::as_ptr(image) as usize).hash(&mut hasher);
            }
            ImageSource::Image(image) => {
                2u8.hash(&mut hasher);
                (Arc::as_ptr(image) as usize).hash(&mut hasher);
            }
            ImageSource::Custom(loader) => {
                3u8.hash(&mut hasher);
                (Arc::as_ptr(loader) as *const () as usize).hash(&mut hasher);
            }
        }

        self.frame_index.hash(&mut hasher);
        if let Some(Ok(ref data)) = self.cached_image_data {
            let image_size = data.render_size(self.frame_index);
            image_size.width.0.to_bits().hash(&mut hasher);
            image_size.height.0.to_bits().hash(&mut hasher);
        }

        let input = crate::SizingInput::new(hasher.finish(), 0);

        let size = if let Some(Ok(ref data)) = self.cached_image_data {
            let image_size = data.render_size(self.frame_index);
            crate::IntrinsicSize {
                min_content: image_size,
                max_content: image_size,
            }
        } else {
            crate::IntrinsicSize::default()
        };

        crate::IntrinsicSizeResult { size, input }
    }

    fn resolve_size_query(
        &mut self,
        query: crate::SizeQuery,
        cached: &crate::IntrinsicSize,
        _ctx: &mut crate::SizingCtx,
    ) -> Size<Pixels> {
        match query {
            crate::SizeQuery::MinContent => cached.min_content,
            crate::SizeQuery::MaxContent => cached.max_content,
            crate::SizeQuery::ForWidth(width) => Size {
                width,
                height: cached.max_content.height,
            },
            crate::SizeQuery::ForHeight(height) => Size {
                width: cached.max_content.width,
                height,
            },
            crate::SizeQuery::Definite(size) => size,
        }
    }

    fn measure(
        &mut self,
        known: Size<Option<Pixels>>,
        _available: Size<AvailableSpace>,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<Size<Pixels>> {
        // If we have image data, use its intrinsic size
        if let Some(Ok(ref data)) = self.cached_image_data {
            let image_size = data.render_size(self.frame_index);
            Some(Size {
                width: known.width.unwrap_or(image_size.width),
                height: known.height.unwrap_or(image_size.height),
            })
        } else {
            // No image data - let Taffy use style-defined size
            None
        }
    }

    fn prepaint_begin(&mut self, ctx: &mut crate::PrepaintCtx) -> crate::PrepaintFrame {
        use crate::window::context::PrepaintCx;

        let mut frame = crate::PrepaintFrame {
            handled: true,
            ..Default::default()
        };

        let prepaint = self.interactivity.prepare_prepaint(
            ctx.fiber_id,
            ctx.inspector_id.as_ref(),
            ctx.bounds,
            ctx.bounds.size,
            ctx.window,
            ctx.cx,
        );

        if prepaint.style.display == Display::None {
            frame.skip_children = true;
            return frame;
        }

        let has_children = !ctx
            .window
            .fiber
            .tree
            .children_slice(&ctx.fiber_id)
            .is_empty();
        if has_children {
            if let Some(text_style) = prepaint.style.text_style() {
                ctx.window.text_style_stack.push(text_style.clone());
                frame.pushed_text_style = true;
            }

            let child_mask = crate::ContentMask { bounds: ctx.bounds };
            let world_mask = ctx.window.transform_mask_to_world(child_mask);
            let intersected = world_mask.intersect(&PrepaintCx::new(ctx.window).content_mask());
            ctx.window.content_mask_stack.push(intersected);
            frame.pushed_content_mask = true;
        }

        frame
    }

    fn prepaint_end(&mut self, ctx: &mut crate::PrepaintCtx, frame: crate::PrepaintFrame) {
        if frame.pushed_content_mask {
            ctx.window.content_mask_stack.pop();
        }
        if frame.pushed_text_style {
            ctx.window.text_style_stack.pop();
        }
    }

    fn paint_begin(&mut self, ctx: &mut crate::PaintCtx) -> crate::PaintFrame {
        use crate::window::context::PaintCx;

        let mut frame = crate::PaintFrame {
            handled: true,
            ..Default::default()
        };
        self.pending_paint_style = None;

        let hitbox = ctx.window.resolve_hitbox(&ctx.fiber_id);

        let Some(paint) = self.interactivity.prepare_paint(
            ctx.fiber_id,
            ctx.bounds,
            hitbox.as_ref(),
            ctx.window,
            ctx.cx,
        ) else {
            return frame;
        };
        let crate::elements::div::InteractivityPaint { style, tab_group } = paint;

        if style.display == Display::None {
            frame.skip_children = true;
            return frame;
        }

        if let Some(opacity) = style.opacity {
            frame.previous_opacity = Some(ctx.window.element_opacity);
            ctx.window.element_opacity *= opacity;
        }

        if let Some(text_style) = style.text_style() {
            ctx.window.text_style_stack.push(text_style.clone());
            frame.pushed_text_style = true;
        }

        if let Some(tab_group) = tab_group {
            ctx.window.begin_tab_group(tab_group);
            frame.pushed_tab_group = true;
        }

        style.paint_before_children(ctx.bounds, ctx.window, ctx.cx);

        if let Some(Ok(image)) = self.cached_image_data.clone() {
            let image_bounds = self
                .object_fit
                .get_bounds(ctx.bounds, image.size(self.frame_index));
            let corner_radii = style
                .corner_radii
                .to_pixels(ctx.window.rem_size())
                .clamp_radii_for_quad_size(image_bounds.size);
            ctx.window
                .paint_image(
                    image_bounds,
                    corner_radii,
                    image,
                    self.frame_index,
                    self.grayscale,
                )
                .log_err();
        }

        if let Some(hitbox) = hitbox.as_ref() {
            if let Some(drag) = ctx.cx.active_drag.as_ref() {
                if let Some(mouse_cursor) = drag.cursor_style {
                    ctx.window.set_window_cursor_style(mouse_cursor);
                }
            } else if let Some(mouse_cursor) = style.mouse_cursor {
                ctx.window.set_cursor_style(mouse_cursor, hitbox);
            }

            if let Some(group) = self.interactivity.group.clone() {
                crate::GroupHitboxes::push(group, hitbox.id, ctx.cx);
                frame.pushed_group_hitbox = self.interactivity.group.clone();
            }

            if let Some(area) = self.interactivity.window_control {
                ctx.window
                    .insert_window_control_hitbox(area, hitbox.clone());
            }

            #[cfg(any(feature = "inspector", debug_assertions))]
            ctx.window
                .insert_inspector_hitbox(hitbox.id, ctx.inspector_id.as_ref(), ctx.cx);
        }

        self.interactivity
            .paint_keyboard_listeners(ctx.window, ctx.cx);

        let has_children = !ctx
            .window
            .fiber
            .tree
            .children_slice(&ctx.fiber_id)
            .is_empty();
        if has_children {
            let child_mask = crate::ContentMask { bounds: ctx.bounds };
            let world_mask = ctx.window.transform_mask_to_world(child_mask);
            let intersected = world_mask.intersect(&PaintCx::new(ctx.window).content_mask());
            ctx.window.content_mask_stack.push(intersected);
            frame.pushed_content_mask = true;
        }

        self.pending_paint_style = Some(style);

        frame
    }

    fn paint_end(&mut self, ctx: &mut crate::PaintCtx, frame: crate::PaintFrame) {
        if frame.pushed_content_mask {
            ctx.window.content_mask_stack.pop();
        }

        if let Some(style) = self.pending_paint_style.take() {
            style.paint_after_children(ctx.bounds, ctx.window, ctx.cx);
        }

        if let Some(group) = frame.pushed_group_hitbox.as_ref() {
            crate::GroupHitboxes::pop(group, ctx.cx);
        }
        if let Some(previous_opacity) = frame.previous_opacity {
            ctx.window.element_opacity = previous_opacity;
        }
        if frame.pushed_tab_group {
            ctx.window.end_tab_group();
        }
        if frame.pushed_text_style {
            ctx.window.text_style_stack.pop();
        }
    }

    fn needs_after_segment(&self) -> bool {
        self.pending_paint_style.as_ref().is_some_and(|style| {
            style
                .border_color
                .is_some_and(|color| !color.is_transparent())
                && style.border_widths.any(|length| !length.is_zero())
        })
    }

    fn conditional_slots(
        &mut self,
        _fiber_id: GlobalElementId,
    ) -> SmallVec<[crate::ConditionalSlot; 4]> {
        let mut slots = SmallVec::new();

        match &self.cached_image_data {
            Some(Err(_)) => {
                if let Some(factory) = self.fallback_factory.clone() {
                    slots.push(crate::ConditionalSlot::active(
                        1,
                        move || {
                            let element = (factory)();
                            crate::div()
                                .absolute()
                                .inset_0()
                                .overflow_hidden()
                                .with_element_child(element)
                                .into_any_element()
                        },
                    ));
                }
            }
            None => {
                if let Some((started_loading, _task)) = self.started_loading.as_ref() {
                    if started_loading.elapsed() > LOADING_DELAY {
                        if let Some(factory) = self.loading_factory.clone() {
                            slots.push(crate::ConditionalSlot::active(
                                0,
                                move || {
                                    let element = (factory)();
                                    crate::div()
                                        .absolute()
                                        .inset_0()
                                        .overflow_hidden()
                                        .with_element_child(element)
                                        .into_any_element()
                                },
                            ));
                        }
                    }
                }
            }
            Some(Ok(_)) => {}
        }

        slots
    }

    fn interactivity(&self) -> Option<&crate::Interactivity> {
        Some(&self.interactivity)
    }
}

impl ImageSource {
    pub(crate) fn identity_eq(&self, other: &ImageSource) -> bool {
        match (self, other) {
            (ImageSource::Resource(a), ImageSource::Resource(b)) => a == b,
            (ImageSource::Render(a), ImageSource::Render(b)) => Arc::ptr_eq(a, b),
            (ImageSource::Image(a), ImageSource::Image(b)) => Arc::ptr_eq(a, b),
            (ImageSource::Custom(a), ImageSource::Custom(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }

    pub(crate) fn use_data(
        &self,
        cache: Option<AnyImageCache>,
        window: &mut Window,
        cx: &mut App,
    ) -> Option<Result<Arc<RenderImage>, ImageCacheError>> {
        match self {
            ImageSource::Resource(resource) => {
                if let Some(cache) = cache {
                    cache.load(resource, window, cx)
                } else {
                    window.use_asset::<ImgResourceLoader>(resource, cx)
                }
            }
            ImageSource::Custom(loading_fn) => loading_fn(window, cx),
            ImageSource::Render(data) => Some(Ok(data.to_owned())),
            ImageSource::Image(data) => window.use_asset::<AssetLogger<ImageDecoder>>(data, cx),
        }
    }

    pub(crate) fn get_data(
        &self,
        cache: Option<AnyImageCache>,
        window: &mut Window,
        cx: &mut App,
    ) -> Option<Result<Arc<RenderImage>, ImageCacheError>> {
        match self {
            ImageSource::Resource(resource) => {
                if let Some(cache) = cache {
                    cache.load(resource, window, cx)
                } else {
                    window.get_asset::<ImgResourceLoader>(resource, cx)
                }
            }
            ImageSource::Custom(loading_fn) => loading_fn(window, cx),
            ImageSource::Render(data) => Some(Ok(data.to_owned())),
            ImageSource::Image(data) => window.get_asset::<AssetLogger<ImageDecoder>>(data, cx),
        }
    }

    /// Remove this image source from the asset system
    pub fn remove_asset(&self, cx: &mut App) {
        match self {
            ImageSource::Resource(resource) => {
                cx.remove_asset::<ImgResourceLoader>(resource);
            }
            ImageSource::Custom(_) | ImageSource::Render(_) => {}
            ImageSource::Image(data) => cx.remove_asset::<AssetLogger<ImageDecoder>>(data),
        }
    }
}

#[derive(Clone)]
enum ImageDecoder {}

impl Asset for ImageDecoder {
    type Source = Arc<Image>;
    type Output = Result<Arc<RenderImage>, ImageCacheError>;

    fn load(
        source: Self::Source,
        cx: &mut App,
    ) -> impl Future<Output = Self::Output> + Send + 'static {
        let renderer = cx.svg_renderer();
        async move { source.to_image_data(renderer).map_err(Into::into) }
    }
}

/// An image loader for the GPUI asset system
#[derive(Clone)]
pub enum ImageAssetLoader {}

impl Asset for ImageAssetLoader {
    type Source = Resource;
    type Output = Result<Arc<RenderImage>, ImageCacheError>;

    fn load(
        source: Self::Source,
        cx: &mut App,
    ) -> impl Future<Output = Self::Output> + Send + 'static {
        let client = cx.http_client();
        // TODO: Can we make SVGs always rescale?
        // let scale_factor = cx.scale_factor();
        let svg_renderer = cx.svg_renderer();
        let asset_source = cx.asset_source().clone();
        async move {
            let bytes = match source.clone() {
                Resource::Path(uri) => fs::read(uri.as_ref())?,
                Resource::Uri(uri) => {
                    let mut response = client
                        .get(uri.as_ref(), ().into(), true)
                        .await
                        .with_context(|| format!("loading image asset from {uri:?}"))?;
                    let mut body = Vec::new();
                    response.body_mut().read_to_end(&mut body).await?;
                    if !response.status().is_success() {
                        let mut body = String::from_utf8_lossy(&body).into_owned();
                        let first_line = body.lines().next().unwrap_or("").trim_end();
                        body.truncate(first_line.len());
                        return Err(ImageCacheError::BadStatus {
                            uri,
                            status: response.status(),
                            body,
                        });
                    }
                    body
                }
                Resource::Embedded(path) => {
                    let data = asset_source.load(&path).ok().flatten();
                    if let Some(data) = data {
                        data.to_vec()
                    } else {
                        return Err(ImageCacheError::Asset(
                            format!("Embedded resource not found: {}", path).into(),
                        ));
                    }
                }
            };

            if let Ok(format) = image::guess_format(&bytes) {
                let data = match format {
                    ImageFormat::Gif => {
                        let decoder = GifDecoder::new(Cursor::new(&bytes))?;
                        let mut frames = SmallVec::new();

                        for frame in decoder.into_frames() {
                            let mut frame = frame?;
                            // Convert from RGBA to BGRA.
                            for pixel in frame.buffer_mut().chunks_exact_mut(4) {
                                pixel.swap(0, 2);
                            }
                            frames.push(frame);
                        }

                        frames
                    }
                    ImageFormat::WebP => {
                        let mut decoder = WebPDecoder::new(Cursor::new(&bytes))?;

                        if decoder.has_animation() {
                            let _ = decoder.set_background_color(Rgba([0, 0, 0, 0]));
                            let mut frames = SmallVec::new();

                            for frame in decoder.into_frames() {
                                let mut frame = frame?;
                                // Convert from RGBA to BGRA.
                                for pixel in frame.buffer_mut().chunks_exact_mut(4) {
                                    pixel.swap(0, 2);
                                }
                                frames.push(frame);
                            }

                            frames
                        } else {
                            let mut data = DynamicImage::from_decoder(decoder)?.into_rgba8();

                            // Convert from RGBA to BGRA.
                            for pixel in data.chunks_exact_mut(4) {
                                pixel.swap(0, 2);
                            }

                            SmallVec::from_elem(Frame::new(data), 1)
                        }
                    }
                    _ => {
                        let mut data =
                            image::load_from_memory_with_format(&bytes, format)?.into_rgba8();

                        // Convert from RGBA to BGRA.
                        for pixel in data.chunks_exact_mut(4) {
                            pixel.swap(0, 2);
                        }

                        SmallVec::from_elem(Frame::new(data), 1)
                    }
                };

                Ok(Arc::new(RenderImage::new(data)))
            } else {
                svg_renderer
                    .render_single_frame(&bytes, 1.0, true)
                    .map_err(Into::into)
            }
        }
    }
}

/// An error that can occur when interacting with the image cache.
#[derive(Debug, Error, Clone)]
pub enum ImageCacheError {
    /// Some other kind of error occurred
    #[error("error: {0}")]
    Other(#[from] Arc<anyhow::Error>),
    /// An error that occurred while reading the image from disk.
    #[error("IO error: {0}")]
    Io(Arc<std::io::Error>),
    /// An error that occurred while processing an image.
    #[error("unexpected http status for {uri}: {status}, body: {body}")]
    BadStatus {
        /// The URI of the image.
        uri: SharedUri,
        /// The HTTP status code.
        status: http_client::StatusCode,
        /// The HTTP response body.
        body: String,
    },
    /// An error that occurred while processing an asset.
    #[error("asset error: {0}")]
    Asset(SharedString),
    /// An error that occurred while processing an image.
    #[error("image error: {0}")]
    Image(Arc<ImageError>),
    /// An error that occurred while processing an SVG.
    #[error("svg error: {0}")]
    Usvg(Arc<usvg::Error>),
}

impl From<anyhow::Error> for ImageCacheError {
    fn from(value: anyhow::Error) -> Self {
        Self::Other(Arc::new(value))
    }
}

impl From<io::Error> for ImageCacheError {
    fn from(value: io::Error) -> Self {
        Self::Io(Arc::new(value))
    }
}

impl From<usvg::Error> for ImageCacheError {
    fn from(value: usvg::Error) -> Self {
        Self::Usvg(Arc::new(value))
    }
}

impl From<image::ImageError> for ImageCacheError {
    fn from(value: image::ImageError) -> Self {
        Self::Image(Arc::new(value))
    }
}
