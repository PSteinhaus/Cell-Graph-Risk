use std::cmp::{min, max};

use ggez::{Context, timer};
use ggez::graphics::{BLACK, Color, DrawParam, Rect, WHITE, set_screen_coordinates, drawable_size, screen_coordinates};
use ggez::graphics::spritebatch::SpriteBatch;
use ggez::nalgebra::{Point2, Vector2};
use smallvec::SmallVec;

use crate::{ANYONE_PLAYER, CANCER_PLAYER, MainState, NId, NO_PLAYER, PlayerId, PlayerState, is_player};
use crate::game_mechanics::{CellType, GameEdge, GameNode};
use crate::game_mechanics::fighting::Troop;
use crate::helpers::lerp_colors;
use crate::physics::{Edge, PhysicsState, Node, EdgeType, EMPTY_NODE_RADIUS};

impl MainState {
    pub fn battle_color<'a, I: Iterator<Item = &'a Troop>>(&self, ctx: &Context, troop_iter: I) -> Color {
        Self::battle_color_static(&self.players, ctx, troop_iter).0
    }

    pub fn battle_color_static<'a, I: Iterator<Item = &'a Troop>>(players: &[PlayerState], ctx: &Context, troop_iter: I) -> (Color, PlayerId, f32) {
        let troops: SmallVec<[&Troop; 6]> = troop_iter.collect();
        let len = troops.len();
        const PHASE_DURATION: f32 = 3.0;
        let phase_rel = (timer::time_since_start(ctx).as_secs_f32() % PHASE_DURATION) / PHASE_DURATION;
        let phase_total: f32 = phase_rel * (len) as f32;
        let phase_floor = phase_total.floor();
        let i_start = min(phase_floor as usize, len-1);
        let i_end = if i_start == len-1 {
            // in the last phase wrap around to the color of the first player
            0
        } else {
            phase_total.ceil() as usize
        };
        let (start_player, end_player) = (troops[i_start].player, troops[i_end].player);
        let color_start = Self::player_color_static(troops[i_start].player, players);
        let color_end   = Self::player_color_static(troops[i_end].player, players);
        let col1_factor = phase_total - phase_floor;

        let color = lerp_colors(&color_start, &color_end, col1_factor);
        let (current_p_id, p_id_factor) = if col1_factor > 0.5 {
            (start_player, col1_factor)
        } else {
            (end_player, 1. - col1_factor)
        };

        (color, current_p_id, (p_id_factor - 0.5) * 2.)
    }

    pub fn player_color(&self, id: PlayerId) -> Color {
        Self::player_color_static(id, &self.players)
    }

    pub fn color_no_player() -> Color {
        Color::new(0.6, 0.6, 0.6, 1.0)
    }
    pub fn color_bg() -> Color { Color::from((80u8, 80u8, 80u8)) }

    pub fn player_color_static(id: PlayerId, players: &[PlayerState]) -> Color {
        match id {
            NO_PLAYER => Self::color_no_player(),
            ANYONE_PLAYER => WHITE,
            CANCER_PLAYER => BLACK,
            other_id => players[usize::from(other_id)].color
        }
    }

    pub fn color_allowed(pl_states: &Vec<PlayerState>, color: &Color) -> bool {
        // make sure the color isn't too dark
        if color.r + color.g + color.b < 0.75 { return false; }
        const MIN_MAX: f32 = 0.6;
        if color.r < MIN_MAX && color.g < MIN_MAX && color.b < MIN_MAX { return false; }
        // make sure the color isn't too gray
        let middle = (color.r + color.g + color.b) / 3.;
        let total_distance = (color.r - middle).abs() + (color.g - middle).abs() + (color.b - middle).abs();
        if total_distance < 0.25 { return false; }
        // make sure it isn't too white
        let total_distance = (color.r - 1.).abs() + (color.g - 1.).abs() + (color.b - 1.).abs();
        if total_distance < 0.25 { return false; }
        // go through all other colors and check that it's not too similar
        for c in pl_states.iter().map(|ps| &ps.color) {
            let total_distance = (color.r - c.r).abs() + (color.g - c.g).abs() + (color.b - c.b).abs();
            if total_distance < 0.5 { return false; }
        }
        true
    }

    pub fn draw_param_node(&self, n_id: NId, ctx: &Context) -> DrawParam {
        let node = self.physics_state.node_at(n_id);
        let g_node = &self.game_state.nodes[usize::from(n_id)];
        let ctrl = g_node.controlled_by();
        let scale = node.radius / EMPTY_NODE_RADIUS;
        let mut p = DrawParam::new()
            .offset(Point2::new(0.5, 0.5))
            .dest(Point2::new(node.position.x, node.position.y))
            .color(if g_node.fighting() { Self::battle_color_static(&self.players, ctx, g_node.troop_iter()).0 } else { self.player_color(ctrl) })
            .scale([scale, scale]);

        let c_type = g_node.cell_type();
        p.src = self.draw_source_rect(&c_type);
        if let CellType::Propulsion(_,angle) = c_type {
            p.rotation = *angle;
        }
        p
    }

    pub fn draw_source_rect(&self, cell_type: &CellType) -> Rect {
        Self::draw_source_rect_static(cell_type, self.spr_b_node_dim)
    }

    pub fn draw_source_rect_static(cell_type: &CellType, dimensions: (u16, u16)) -> Rect {
        use CellType::*;
        let native_rect = match cell_type {
            Cancer => Rect::new(1.0, 1.0, 140.0, 140.0),
            Propulsion(_,_) => Rect::new(143.0, 1.0, 140.0, 140.0),
            Wall => Rect::new(285.0, 1.0, 140.0, 140.0),
            _ => Rect::new(427.0, 1.0, 124.0, 124.0),
        };
        let (w, h) = dimensions;
        let (w, h) = (w as f32, h as f32);
        Rect::new(native_rect.x / w, native_rect.y / h, native_rect.w / w, native_rect.h / h)
    }

    pub fn draw_param_edge(&self, edge: &Edge, g_edge: &GameEdge) -> DrawParam {
        self.draw_param_edge_from_n_ids(edge.node_indices, g_edge.controlled_by())
    }

    /// WARNING: only used for drawing the edge selection currently; some things are hardcoded for this special use right now;
    pub fn draw_param_edge_from_n_ids(&self, n_ids: [NId; 2], p_id_controlling: PlayerId) -> DrawParam {
        let (node1, node2) = (self.physics_state.node_at(n_ids[0]), self.physics_state.node_at(n_ids[1]));
        let e_id = self.physics_state.edge_id_between(n_ids[0], n_ids[1]).unwrap();
        let edge = self.physics_state.edge_at(e_id);
        let vec: Vector2<f32> = node2.position - node1.position;

        Self::draw_param_edge_static(
            edge,
            node1.position,
            node1.radius + node2.radius,
            vec,
            self.edge_sprite_width,
            p_id_controlling,
            &self.players,
            false,
            self.is_unchangeable_edge(e_id)
        )
    }

    pub fn draw_param_edge_static(p_edge: &Edge, pos: Point2<f32>, radii_comb: f32, vec: Vector2<f32>, spr_width: f32, p_id_controlling: PlayerId, players: &[PlayerState], use_strain: bool, unchangeable: bool) -> DrawParam {
        let rotation = ggez::nalgebra::RealField::atan2(vec.y, vec.x);
        let nrm = vec.norm();
        let color = if use_strain {
            lerp_colors(&(0.1, 0.1, 0.1).into(), &Self::player_color_static(p_id_controlling, players), p_edge.strain(nrm - radii_comb))
        } else {
            Self::player_color_static(p_id_controlling, players)
        };
        DrawParam::new()
            .offset(Point2::new(0.0, 0.5))
            .src(Self::edge_src_rect(p_edge))
            .dest(Point2::new(pos.x, pos.y))
            .rotation(rotation)
            .scale(Vector2::new(nrm / spr_width, if unchangeable { 2. } else { 1. }))
            .color(color)
    }

    pub fn draw_param_edge_without_edge_static(is_wall: bool, pos: Point2<f32>, vec: Vector2<f32>, spr_width: f32, color: &Color) -> DrawParam {
        let rotation = ggez::nalgebra::RealField::atan2(vec.y, vec.x);
        DrawParam::new()
            .offset(Point2::new(0.0, 0.5))
            .src(if is_wall { Self::edge_src_rect_bg() }
                 else       { Self::edge_src_rect_main() })
            .dest(Point2::new(pos.x, pos.y))
            .rotation(rotation)
            .scale(Vector2::new(vec.norm() / spr_width, 1.0))
            .color(*color)
    }

    pub fn edge_src_rect(p_edge: &Edge) -> Rect {
        return if let EdgeType::Wall = p_edge.e_type {
            Self::edge_src_rect_bg()
        } else {
            Self::edge_src_rect_main()
        }
    }

    pub fn edge_src_rect_main() -> Rect {
        Rect::new(0., 0., 1., 0.5)
    }

    pub fn edge_src_rect_bg() -> Rect {
        Rect::new(0., 0.5, 1., 0.5)
    }

    pub fn draw_edge(players: &[PlayerState],
                 ctx: &Context,
                 spr_width: f32,
                 physics_state: &PhysicsState,
                 spr_batch_edge: &mut SpriteBatch,
                 spr_batch_troop: &mut SpriteBatch,
                 edge: &Edge,
                 g_edge: &GameEdge,
                 g_nodes: &[GameNode],
                 spr_b_node_dim: (u16, u16),
                 is_unchangeable: bool)
    {
        let (node1, node2) = (physics_state.node_at(edge.node_indices[0]),physics_state.node_at(edge.node_indices[1]));
        let vec: Vector2<f32> = node2.position - node1.position;
        // draw the edge
        spr_batch_edge.add(Self::draw_param_edge_static(
            edge,
            node1.position,
            node1.radius + node2.radius,
            vec,
            spr_width,
            g_edge.controlled_by(),
            players,
            !is_unchangeable,
            is_unchangeable
        ));
        let src_rect = Self::draw_source_rect_static(&CellType::Basic, spr_b_node_dim);
        // calculate the troop positions based on the starting point of the edge and the advancement of the troops
        for adv_troop in g_edge.troop_iter() {
            // TODO: think about / test whether this is really ok, or whether we should base this on the sqrt of count instead
            let scale = 0.49 + 0.01 * adv_troop.troop.count as f32;
            let pos = node1.position + vec * adv_troop.advancement;
            spr_batch_troop.add(DrawParam::new()
                .offset(Point2::new(0.5, 0.5))
                .src(src_rect)
                .dest(pos)
                .scale(Vector2::new(scale, scale))
                .color(Self::player_color_static(adv_troop.troop.player, players))
            );
        }
        // draw the fights
        for fight in g_edge.fights.iter() {
            let units: u16 = fight.troop_iter().map(|t| t.count as u16).sum();
            let scale = 0.7 + 0.01 * units as f32;
            let pos = node1.position + vec * fight.advancement;
            spr_batch_troop.add(DrawParam::new()
                .offset(Point2::new(0.5, 0.5))
                .src(src_rect)
                .dest(pos)
                .scale(Vector2::new(scale, scale))
                .color(Self::battle_color_static(players, ctx, fight.troop_iter()).0)
            );
        }
    }

    pub fn position_camera(&self, ctx: &mut Context) {
        let (mut s_x, mut s_y, mut b_x, mut b_y) = (f32::INFINITY, f32::INFINITY, f32::NEG_INFINITY, f32::NEG_INFINITY);
        for (n_id, node) in self.physics_state.nodes.iter().enumerate() {
            // only count nodes in use by the players
            if is_player(self.game_state.nodes[n_id].controlled_by()) {
                let pos = node.position;
                if pos.x < s_x { s_x = pos.x }
                if pos.x > b_x { b_x = pos.x }
                if pos.y < s_y { s_y = pos.y }
                if pos.y > b_y { b_y = pos.y }
            }
        }
        // just to be sure
        if s_x == f32::INFINITY {
            return;
        }
        const BORDER: f32 = 2048.;
        let (mut w, mut h) = (b_x - s_x + (BORDER * 2.), b_y - s_y + (BORDER * 2.));
        // make sure the ratio matches the screen ratio
        let screen_size = drawable_size(ctx);
        let ratio = screen_size.0 / screen_size.1;
        let current_ratio = w / h;
        if current_ratio > ratio {
            // the width is too great, increase the height
            let old_h = h;
            h = 1. / ratio * w;
            // keep the image centered
            s_y -= (h - old_h) / 2.;
        } else {
            // the height is too great, increase the width
            let old_w = w;
            w = ratio * h;
            // keep the image centered
            s_x -= (w - old_w) / 2.;
        }
        s_x -= BORDER;
        s_y -= BORDER;
        // make the camera movement smooth
        // important because there are cases where there is a big instantaneous change
        // i.e. when a node gets added or removed to the set of player used nodes
        let old = screen_coordinates(ctx);
        const CAM_SPEED: f32 = 0.1;
        (s_x, s_y) = (old.x + (s_x - old.x) * CAM_SPEED, old.y + (s_y - old.y) * CAM_SPEED);
        (w, h)     = (old.w + (w - old.w) * CAM_SPEED, old.h + (h - old.h) * CAM_SPEED);
        let screen_coords = Rect::new(s_x, s_y, w, h);
        set_screen_coordinates(ctx, screen_coords).unwrap();
    }
}