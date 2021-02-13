use std::cmp::min;

use ggez::{Context, timer};
use ggez::graphics::{BLACK, Color, DrawParam, Rect, WHITE};
use ggez::graphics::spritebatch::SpriteBatch;
use ggez::nalgebra::{Point2, Vector2};
use smallvec::SmallVec;

use crate::{ANYONE_PLAYER, CANCER_PLAYER, MainState, NId, NO_PLAYER, PlayerId, PlayerState};
use crate::game_mechanics::{CellType, GameEdge, GameNode};
use crate::game_mechanics::fighting::Troop;
use crate::helpers::lerp_colors;
use crate::physics::{Edge, PhysicsState, Node, EdgeType};

impl MainState {
    pub fn battle_color<'a, I: Iterator<Item = &'a Troop>>(&self, ctx: &Context, troop_iter: I) -> Color {
        Self::battle_color_static(&self.players, ctx, troop_iter)
    }

    pub fn battle_color_static<'a, I: Iterator<Item = &'a Troop>>(players: &[PlayerState], ctx: &Context, troop_iter: I) -> Color {
        let troops: SmallVec<[&Troop; 6]> = troop_iter.collect();
        let len = troops.len();
        const PHASE_DURATION: f32 = 2.0;
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
        let color_start = Self::player_color_static(troops[i_start].player, players);
        let color_end   = Self::player_color_static(troops[i_end].player, players);

        lerp_colors(&color_start, &color_end, phase_total - phase_floor)
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

    pub fn draw_param_node(&self, n_id: NId, ctx: &Context) -> DrawParam {
        let node = self.physics_state.node_at(n_id);
        let g_node = &self.game_state.nodes[usize::from(n_id)];
        let ctrl = g_node.controlled_by();
        let mut p = DrawParam::new()
            .offset(Point2::new(0.5, 0.5))
            .dest(Point2::new(node.position.x, node.position.y))
            .color(if g_node.fighting() { Self::battle_color_static(&self.players, ctx, g_node.troop_iter()) } else { self.player_color(ctrl) });

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

    pub fn draw_param_edge_from_n_ids(&self, n_ids: [NId; 2], p_id_controlling: PlayerId) -> DrawParam {
        let (node1, node2) = (self.physics_state.node_at(n_ids[0]), self.physics_state.node_at(n_ids[1]));
        let e_id = self.physics_state.edge_id_between(n_ids[0], n_ids[1]).unwrap();
        let edge = self.physics_state.edge_at(e_id);
        let vec: Vector2<f32> = node2.position - node1.position;

        Self::draw_param_edge_static(
            edge,
            node1.position,
            vec,
            self.edge_sprite_width,
            p_id_controlling,
            &self.players
        )
    }

    pub fn draw_param_edge_static(p_edge: &Edge, pos: Point2<f32>, vec: Vector2<f32>, spr_width: f32, p_id_controlling: PlayerId, players: &[PlayerState]) -> DrawParam {
        let rotation = ggez::nalgebra::RealField::atan2(vec.y, vec.x);
        DrawParam::new()
            .offset(Point2::new(0.0, 0.5))
            .src(Self::edge_src_rect(p_edge))
            .dest(Point2::new(pos.x, pos.y))
            .rotation(rotation)
            .scale(Vector2::new(vec.norm() / spr_width, 1.0))
            .color(Self::player_color_static(p_id_controlling, players))
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
                 spr_b_node_dim: (u16, u16))
    {
        let (node1, node2) = (physics_state.node_at(edge.node_indices[0]),physics_state.node_at(edge.node_indices[1]));
        let vec: Vector2<f32> = node2.position - node1.position;
        // draw the edge
        spr_batch_edge.add(Self::draw_param_edge_static(
            edge,
            node1.position,
            vec,
            spr_width,
            g_edge.controlled_by(),
            players
        ));
        let src_rect = Self::draw_source_rect_static(&CellType::Basic, spr_b_node_dim);
        // calculate the troop positions based on the starting point of the edge and the advancement of the troops
        for adv_troop in g_edge.troop_iter() {
            let pos = node1.position + vec * adv_troop.advancement;
            spr_batch_troop.add(DrawParam::new()
                .offset(Point2::new(0.5, 0.5))
                .src(src_rect)
                .dest(pos)
                .scale(Vector2::new(0.5, 0.5))
                .color(Self::player_color_static(adv_troop.troop.player, players))
            );
        }
        // draw the fights
        for fight in g_edge.fights.iter() {
            let pos = node1.position + vec * fight.advancement;
            spr_batch_troop.add(DrawParam::new()
                .offset(Point2::new(0.5, 0.5))
                .src(src_rect)
                .dest(pos)
                .scale(Vector2::new(0.75, 0.75))
                .color(Self::battle_color_static(players, ctx, fight.troop_iter()))
            );
        }
    }
}