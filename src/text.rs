use ggez::graphics::spritebatch::SpriteBatch;
use ggez::graphics::{Image, DrawParam, Rect, mint, Drawable, BlendMode};
use smallvec::SmallVec;
use ggez::mint::{Vector4, Point2};
use ggez::{Context, GameResult};
use ggez::nalgebra::{Vector3, Matrix4};

pub struct SpriteText {
    spr_b: SpriteBatch,
    dims: (f32, f32),
    char_spacing: f32,
    line_height: f32,
    wrap_width: f32,
}

impl SpriteText {
    pub fn new(img: Image) -> SpriteText {
        SpriteText {
            dims: (img.width() as f32, img.height() as f32),
            char_spacing: 1.5,
            line_height: 16.0,
            wrap_width: f32::INFINITY,
            spr_b: SpriteBatch::new(img),
        }
    }

    pub fn add(&mut self, text: &str, mut p: DrawParam) {
        let mut text_dims = (0f32, self.line_height);
        let mut curr_pos = (0f32, 0f32);
        let mut char_params = SmallVec::<[DrawParam;32]>::new();

        // got through all characters and place them line by line
        // this positioning is temporary and will be modified later
        for ch in text.chars() {
            let mut char_p = self.get_char_param(ch);
            let mut new_width = text_dims.0 + self.char_spacing + char_p.src.w * self.dims.0;
            if text_dims.0 != 0. { new_width += self.char_spacing; }
            else { text_dims.1 = char_p.src.h * self.dims.1; }

            if new_width <= self.wrap_width {
                curr_pos.0 = text_dims.0;
                text_dims.0 = new_width;
            } else {
                curr_pos.0 = 0.;
                curr_pos.1 += self.line_height;
                text_dims.1 += self.line_height;
            }

            char_p.dest = Point2::from([curr_pos.0, curr_pos.1]);
            char_params.push(char_p);
        }
        // now that the chars are placed apply possible transformations from p
        let offset_inverse = Matrix4::new_translation(&Vector3::new(-p.offset.x * text_dims.0, -p.offset.y * text_dims.1, 0.0));
        p.offset.x = 0.0;
        p.offset.y = 0.0;
        let matrix = Matrix4::from(p.to_matrix());

        for mut char_p in char_params {
            let vec = ggez::nalgebra::Vector4::from( Vector4 { x: char_p.dest.x, y: char_p.dest.y, z: 0.0f32, w: 1.0f32 });
            //println!("vec: {:?}", vec);
            let transformed_vec = matrix * offset_inverse * vec;
            //println!("transformed_vec: {:?}", transformed_vec);
            char_p.dest = Point2::from([transformed_vec.x, transformed_vec.y]);
            char_p.scale = p.scale;
            char_p.rotation = p.rotation;
            char_p.color = p.color;
            // consume the param by adding it to the spritebatch
            //println!("char_p: {:?}", char_p);
            self.spr_b.add(char_p);
        }
    }

    fn get_char_param(&self, ch: char) -> DrawParam {
        let ch = ch as u8;
        let native_src = match ch {
            48 => Rect::new(212., 4., 20., 29.),
            49 => Rect::new(233., 5., 11., 28.),
            50 => Rect::new(245., 4., 20., 29.),
            51 => Rect::new(266., 4., 18., 29.),
            52 => Rect::new(285., 5., 21., 28.),
            53 => Rect::new(307., 5., 18., 28.),
            54 => Rect::new(326., 3., 19., 30.),
            55 => Rect::new(346., 5., 20., 28.),
            56 => Rect::new(367., 4., 19., 29.),
            57 => Rect::new(387., 4., 19., 30.),
            _ => Rect::new(212., 4., 20., 29.),
        };
        let (w, h) = self.dims;
        DrawParam::default().src(
            Rect::new(native_src.x / w, native_src.y / h, native_src.w / w, native_src.h / h)
        )
    }

    pub fn clear(&mut self) {
        self.spr_b.clear();
    }
}

impl Drawable for SpriteText {
    fn draw(&self, ctx: &mut Context, param: DrawParam) -> GameResult<()> {
        self.spr_b.draw(ctx, param)
    }

    fn dimensions(&self, ctx: &mut Context) -> Option<Rect> {
        self.spr_b.dimensions(ctx)
    }

    fn set_blend_mode(&mut self, mode: Option<BlendMode>) {
        self.spr_b.set_blend_mode(mode);
    }

    fn blend_mode(&self) -> Option<BlendMode> {
        self.spr_b.blend_mode()
    }
}