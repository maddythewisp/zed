use gpui_macros::Element;

use crate::{
    App, ElementImpl, IntoElement, ObjectFit, PaintFrame, Pixels, PrepaintFrame,
    RenderNode, StyleRefinement, Styled, UpdateResult, Window, taffy::ToTaffy,
};
#[cfg(target_os = "macos")]
use core_video::pixel_buffer::CVPixelBuffer;
use refineable::Refineable;
use std::any::TypeId;
use taffy::style::Style as TaffyStyle;

/// A source of a surface's content.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SurfaceSource {
    /// A macOS image buffer from CoreVideo
    #[cfg(target_os = "macos")]
    Surface(CVPixelBuffer),
}

#[cfg(target_os = "macos")]
impl From<CVPixelBuffer> for SurfaceSource {
    fn from(value: CVPixelBuffer) -> Self {
        SurfaceSource::Surface(value)
    }
}

/// A surface element.
#[derive(Element)]
#[element(crate = crate)]
pub struct Surface {
    source: SurfaceSource,
    object_fit: ObjectFit,
    style: StyleRefinement,
}

/// Create a new surface element.
#[cfg(target_os = "macos")]
pub fn surface(source: impl Into<SurfaceSource>) -> Surface {
    Surface {
        source: source.into(),
        object_fit: ObjectFit::Contain,
        style: Default::default(),
    }
}

impl Surface {
    /// Set the object fit for the image.
    pub fn object_fit(mut self, object_fit: ObjectFit) -> Self {
        self.object_fit = object_fit;
        self
    }
}

impl ElementImpl for Surface {
    fn create_render_node(&mut self) -> Option<Box<dyn RenderNode>> {
        Some(Box::new(SurfaceNode {
            source: self.source.clone(),
            object_fit: self.object_fit,
            style: self.style.clone(),
        }))
    }

    fn render_node_type_id(&self) -> Option<TypeId> {
        Some(TypeId::of::<SurfaceNode>())
    }

    fn update_render_node(
        &mut self,
        node: &mut dyn RenderNode,
        _window: &mut Window,
        _cx: &mut App,
    ) -> Option<UpdateResult> {
        let node = node.as_any_mut().downcast_mut::<SurfaceNode>()?;
        let layout_changed = node.style != self.style;
        let paint_changed =
            layout_changed || node.source != self.source || node.object_fit != self.object_fit;

        node.source = self.source.clone();
        node.object_fit = self.object_fit;
        node.style = self.style.clone();

        Some(UpdateResult {
            layout_changed,
            paint_changed,
        })
    }
}

impl IntoElement for Surface {
    type Element = Self;

    fn into_element(self) -> Self::Element {
        self
    }
}

impl Styled for Surface {
    fn style(&mut self) -> &mut StyleRefinement {
        &mut self.style
    }
}

struct SurfaceNode {
    source: SurfaceSource,
    object_fit: ObjectFit,
    style: StyleRefinement,
}

impl RenderNode for SurfaceNode {
    fn taffy_style(&self, rem_size: Pixels, scale_factor: f32) -> TaffyStyle {
        let mut style = crate::Style::default();
        style.refine(&self.style);
        style.to_taffy(rem_size, scale_factor)
    }

    fn prepaint_begin(&mut self, _ctx: &mut crate::PrepaintCtx) -> PrepaintFrame {
        PrepaintFrame {
            handled: true,
            skip_children: true,
            ..Default::default()
        }
    }

    fn paint_begin(&mut self, ctx: &mut crate::PaintCtx) -> PaintFrame {
        #[cfg(target_os = "macos")]
        match &self.source {
            SurfaceSource::Surface(surface) => {
                let size = crate::size(surface.get_width().into(), surface.get_height().into());
                let new_bounds = self.object_fit.get_bounds(ctx.bounds, size);
                ctx.window.paint_surface(new_bounds, surface.clone());
            }
        }

        PaintFrame {
            handled: true,
            skip_children: true,
            ..Default::default()
        }
    }
}
