#![feature(drain_filter)]
#![feature(destructuring_assignment)]
#![feature(in_band_lifetimes)]

use ggez;
use ggez::{event, conf};
use ggez::graphics;
use ggez::nalgebra::{Point2, Vector2};
use ggez::timer;
use ggez::{Context, GameResult};
use std::env;
use std::path;
use crate::physics::{PhysicsState, Node, EdgeType, EMPTY_NODE_RADIUS};
use crate::game_mechanics::*;
use rand::{Rng, thread_rng};
use ggez::graphics::{Rect, Image, DrawParam, Color, WHITE, BLACK};
use ggez::graphics::spritebatch::SpriteBatch;
use ggez::input::gamepad::{GamepadId, gamepad};
use ggez::event::{Button, Axis};
use crate::helpers::{angle_diff_abs, u16_to_btn, btn_to_u16};
use ggez::input::gamepad::gilrs::ev::Button::South;
use smallvec::{SmallVec, smallvec};
use std::mem::transmute;
use crate::text::SpriteText;
use std::f32::consts::PI;

mod physics;
mod game_mechanics;
mod text;
mod proximity;
mod drawing;
mod helpers;
mod levels;

type PlayerId = u8;             // more than 255 players shouldn't be needed
type EId = u16;
type NId = u16;
type UnitCount = u8;
const MAX_UNIT_COUNT: UnitCount = 99;
const MIN_UNIT_COUNT: UnitCount = 1;
const NO_EDGE: EId = EId::MAX;
const NO_NODE: NId = NId::MAX;
const NO_PLAYER: PlayerId = u8::MAX;
const ANYONE_PLAYER: PlayerId = u8::MAX - 1;
const CANCER_PLAYER: PlayerId = u8::MAX - 2;

fn is_player(p_id: PlayerId) -> bool {
    // equivalent to this:
    //match p_id {
    //    NO_PLAYER | ANYONE_PLAYER | CANCER_PLAYER => false,
    //    id => true
    //}
    p_id < CANCER_PLAYER
}

struct MainState {
    players: Vec<PlayerState>,
    game_state: GameState,
    physics_state: PhysicsState,
    unchangeable_nodes: usize,
    unmovable_nodes: usize,
    unchangeable_edges: usize,
    ticks: usize,
    proximity_nodes: Vec<Vec<NId>>,
    proximity_walls: Vec<Vec<NId>>,
    edge_sprite_width: f32,
    spr_b_node_dim: (u16, u16),
    spr_b_node: SpriteBatch,
    spr_b_edge: SpriteBatch,
    spr_b_text: SpriteText,
}

impl MainState {
    fn new(ctx: &mut Context) -> MainState {
        let img = Image::new(ctx, "/edge.png").unwrap();
        let spr_sheet = Image::new(ctx, "/spritesheet.png").unwrap();
        let spr_text = Image::new(ctx, "/trebuchet_ms_regular_24.png").unwrap();
        MainState {
            players: Vec::new(),
            game_state: GameState::new(),
            physics_state: PhysicsState::new(),
            unchangeable_nodes: 0,
            unmovable_nodes: 0,
            unchangeable_edges: 0,
            ticks: 0,
            proximity_nodes: Vec::new(),
            proximity_walls: Vec::new(),
            edge_sprite_width: img.width() as f32,
            spr_b_node_dim: (spr_sheet.width(), spr_sheet.height()),
            spr_b_node: SpriteBatch::new(spr_sheet),
            spr_b_edge: SpriteBatch::new(img),
            spr_b_text: SpriteText::new(spr_text),
        }
    }

    fn add_node(&mut self, position: Point2<f32>) {
        self.add_node_of_type(position, CellType::Basic);
    }

    fn add_node_of_type(&mut self, position: Point2<f32>, cell_type: CellType) {
        // add to physics
        self.physics_state.add_node(position);
        if let CellType::Wall = cell_type {
            self.physics_state.nodes.last_mut().unwrap().node_collision = true;
        }
        // add to game state
        self.game_state.add_node(cell_type);
        // reserve a vector for the proximity state
        self.proximity_nodes.push(Vec::new());

        // reserve a text for unit count
        //let txt_fragm = TextFragment::new("42").scale(Scale::uniform(200.0));
        //let txt = Text::new(txt_fragm);
        //self.unit_count_texts.push(txt);
    }

    fn remove_node(&mut self, node_index: NId) -> SmallVec<[PlayerId; 4]> {
        // if a player is on this node place him somewhere else
        // only send him to nodes that he can actually be on (i.e. his own or those in battle state carrying his troops)
        // collect players that have to be removed (in case there are any) and remove them afterwards
        let mut players_to_remove: SmallVec<[PlayerId; 4]> = smallvec![];
        if self.is_unchangeable(node_index) { return players_to_remove; }
        for p_id in 0..self.game_state.player_node_ids.len() {
            if self.game_state.kick_player_from_node(p_id as PlayerId, node_index, &self.physics_state) {
                players_to_remove.push(p_id as PlayerId);
            }
        }
        for p_id in players_to_remove.iter() {
            self.remove_player(*p_id);
        }
        // first remove all edges to this node
        while !self.physics_state.node_at(node_index).edge_indices.is_empty() {
            let e_id = *self.physics_state.node_at(node_index).edge_indices.last().unwrap();
            self.remove_edge(e_id);
        }
        // remove it from the proximity state
        self.proximity_nodes.swap_remove(usize::from(node_index));
        // go through all n_ids saved in proximity_walls and update them if necessary
        let last_n_id = (self.node_count() - 1) as NId;
        for vec in self.proximity_walls.iter_mut() {
            vec.retain(|n_id| *n_id != node_index);
            for n_id in vec.iter_mut() {
                if *n_id == last_n_id {
                    *n_id = node_index;
                    break;
                }
            }
        }
        // remove from game state
        self.game_state.remove_node(node_index);
        // remove it from physics
        self.physics_state.remove_node(node_index);
        // return which players got removed (if any)
        players_to_remove
    }

    fn node_count(&self) -> usize { self.physics_state.node_count() }
    fn edge_count(&self) -> usize { self.physics_state.edge_count() }

    fn remove_player(&mut self, p_id: PlayerId) {
        // remove him from the game state
        self.game_state.remove_player(p_id);
        // remove the player from the collection
        self.players.swap_remove(usize::from(p_id));
    }

    fn add_edge(&mut self, node1_index: NId, node2_index: NId) -> bool {
        Self::add_edge_static(&mut self.physics_state, &mut self.game_state, node1_index, node2_index, &mut self.proximity_walls, self.unchangeable_edges)
    }

    fn add_edge_static(p_state: &mut PhysicsState, g_state: &mut GameState, node1_index: NId, node2_index: NId, prox_walls: &mut Vec<Vec<NId>>, unchangeable_edges: usize) -> bool {
        // first check if you can add this edge
        if p_state.can_add_edge(node1_index, node2_index) {
            // add to physics
            p_state.add_edge(node1_index, node2_index);
            // reserve a vector for the proximity state
            prox_walls.push(Vec::new());
            // add to game state
            let is_wall = g_state.add_edge(&[node1_index, node2_index]);
            // if it's a wall update the physics state
            if is_wall {
                let mut edges_to_remove = Vec::<EId>::new();
                Self::turn_to_wall_static(p_state, &mut g_state.nodes, (p_state.edge_count() as EId) - 1,  &mut edges_to_remove);
                Self::remove_multiple_edges_static(p_state, g_state, &mut edges_to_remove, prox_walls, unchangeable_edges);
            }
            return true;
        }
        return false;
    }

    fn remove_edge(&mut self, edge_index: EId) {
        Self::remove_edge_static(&mut self.physics_state, &mut self.game_state, edge_index, &mut self.proximity_walls, self.unchangeable_edges);
    }

    /// Returns whether the edge was actually removed
    fn remove_edge_static(p_state: &mut PhysicsState, g_state: &mut GameState, edge_index: EId, prox_walls: &mut Vec<Vec<NId>>, unchangeable_edges: usize) -> bool {
        // unchangeable edges cannot be removed
        if edge_index < unchangeable_edges as EId { return false; }
        // get the nodes connected by this edge
        let n_ids = p_state.edge_at(edge_index).node_indices;
        // remove from game state
        g_state.remove_edge(edge_index, n_ids);
        // remove from physics
        p_state.remove_edge(edge_index);
        // remove from the proximity state
        prox_walls.swap_remove(usize::from(edge_index));
        true
    }

    fn remove_multiple_edges_static(p_state: &mut PhysicsState, g_state: &mut GameState, edges_to_remove: &mut Vec<EId>, prox_walls: &mut Vec<Vec<NId>>, unchangeable_edges: usize) {
        // first we remove all unchangeable edges from the vector to make sure that the step described next will work as intended
        edges_to_remove.retain(|e_id| *e_id >= unchangeable_edges as EId);
        // here we need to use some caution
        // removing an edge will invalidate the EId of the last edge
        // therefore we need to check whether the last edge is contained in this collection
        // and update the EId if true
        while !edges_to_remove.is_empty() {
            // first check whether the list contains the last EId
            let first_e_id = *edges_to_remove.last().unwrap();
            let last_e_id = p_state.edge_count() as EId - 1;
            for e_id in edges_to_remove.iter_mut() {
                if *e_id == last_e_id {
                    // update it to the value it will have after the removal of the first edge
                    *e_id = first_e_id;
                    break;
                }
            }
            Self::remove_edge_static(p_state, g_state, first_e_id, prox_walls, unchangeable_edges);
            edges_to_remove.pop();
        }
    }

    fn split_nodes(&mut self, nodes_to_be_split: Vec<NId>) {
        for n_id in nodes_to_be_split.into_iter() {
            // get the old node position
            let pos = self.physics_state.node_at(n_id).position;
            // add a new one there
            self.add_node_of_type(pos, CellType::Basic);

            // give half the edges to the new one
            let p_node = self.physics_state.node_at_mut(n_id);
            let g_node = &mut self.game_state.nodes[usize::from(n_id)];
            let mut switched_edges: SmallVec<[EId; 16]> = smallvec![];
            for i in 0..p_node.edge_indices.len() / 2 {
                let e_removed = p_node.edge_indices.pop().unwrap();
                g_node.remove_troop_path(&e_removed);
                switched_edges.push(e_removed);
            }

            // give half the units to the new one
            let p_id = g_node.controlled_by();
            let troop = g_node.troop_of_player_mut(p_id);
            let mut units: UnitCount = 0;
            if let Some(t) = troop {
                units = t.remove_units(t.count / 2);
            }

            // push the nodes away from each other
            let angle = rand::thread_rng().gen_range(0f32, 2. * PI);
            const PUSH_SCALE: f32 = 5.;
            let push = Vector2::<f32>::from([angle.cos(), angle.sin()]) * PUSH_SCALE;
            p_node.add_velocity(push);

            let new_n_id = (self.node_count() - 1) as NId;
            let new_p_node = &mut self.physics_state.nodes[usize::from(new_n_id)];
            // gain edges
            for e_id in switched_edges.into_iter() {
                new_p_node.edge_indices.push(e_id);
                let edge = &mut self.physics_state.edges[usize::from(e_id)];
                edge.node_indices[edge.pos_in_edge(n_id as NId)] = new_n_id;
                // make sure that players can only have this edge selected if they're on the other node in this edge
                for (p_id, p_e_id_opt) in self.game_state.player_edge_ids.iter_mut().enumerate() {
                    if let Some(p_e_id) = p_e_id_opt {
                        if *p_e_id == e_id {
                            if self.game_state.player_node_ids[p_id] != edge.other_node(new_n_id) {
                                *p_e_id_opt = None;
                            }
                        }
                    }
                }
            }
            // gain units
            let new_g_node = &mut self.game_state.nodes[usize::from(new_n_id)];
            new_g_node.add_units(p_id,units);
            // make sure it has the right owner even if no units were added
            new_g_node.set_controlled_by(p_id);

            // get pushed
            new_p_node.add_velocity(-push);
        }
    }

    fn player_id_using(&self, gamepad_id: GamepadId) -> PlayerId {
        if let Some(position) = self.players.iter().position(|p_state| p_state.gamepad_id == gamepad_id) {
            return position as PlayerId;
        }
        else {
            return NO_PLAYER;
        }
    }

    fn player_using(&self, gamepad_id: GamepadId) -> &PlayerState {
        &self.players[usize::from(self.player_id_using(gamepad_id))]
    }
    fn player_using_mut(&mut self, gamepad_id: GamepadId) -> &mut PlayerState {
        let id = self.player_id_using(gamepad_id);
        &mut self.players[usize::from(id)]
    }

    fn add_player(&mut self, gamepad_id: GamepadId, color: Color) -> bool {
        println!("adding player with GamepadId {:?}", gamepad_id);
        let mut rng = thread_rng();
        // for now choose a random StartNode node for the player to start on
        // so collect these n_ids first
        let start_n_ids: SmallVec<[NId; 32]> = self.game_state.nodes.iter()
            .enumerate()
            .filter(|(_n_id, node)| if let CellType::StartNode = node.cell_type() { true } else { false })
            .map(|(n_id, _node)| n_id as NId)
            .collect();
        if start_n_ids.len() != 0 {
            self.players.push(PlayerState::new(gamepad_id, color));
            let i = rng.gen_range(0, start_n_ids.len());
            let start_n_id = start_n_ids[i];
            self.game_state.add_player(start_n_id, &self.physics_state);
            return true;
        } else {
            return false;
        }
    }

    fn identify_or_add_player(&mut self, gamepad_id: GamepadId) -> Option<PlayerId> {
        // identify the player
        let mut player_id = self.player_id_using(gamepad_id);
        // if there is no player using this gamepad add one
        if player_id == NO_PLAYER {
            let mut pl_color = Color::new(0., 0., 0., 1.);
            for _ in 0..100 {   // try at best a hundred times
                let rgb = thread_rng().gen::<(u8,u8,u8)>();    // choose a random color
                pl_color = Color::from(rgb);
                if Self::color_allowed(&self.players, &pl_color) {
                    println!("allowed color found");
                    break;
                }
            }
            if self.add_player(gamepad_id, pl_color) {
                player_id = (self.players.len() - 1) as PlayerId;
            } else {
                return None
            }
        }
        Some(player_id)
    }

    fn player_node(&self, id: PlayerId) -> &Node {
        Self::player_node_static(&self.physics_state, &self.game_state, id)
    }

    fn player_node_static<'a>(physics_state: &'a PhysicsState, game_state: &GameState, id: PlayerId) -> &'a Node {
        physics_state.node_at(game_state.player_node_ids[usize::from(id)])
    }

    fn player_node_mut(&mut self, id: PlayerId) -> &mut Node {
        self.physics_state.node_at_mut(self.game_state.player_node_ids[usize::from(id)])
    }

    fn turn_to_wall_static(p_state: &mut PhysicsState, g_state_nodes: &mut [GameNode], e_id: EId, e_to_be_rem: &mut Vec<EId>) {
        if p_state.turn_to_wall(e_id, e_to_be_rem) {
            let n_ids = p_state.edge_at(e_id).node_indices;
            GameState::turn_to_wall_static(g_state_nodes, e_id, n_ids[0], n_ids[1]);
        }
    }

    fn handle_input(&mut self, ctx: &mut Context) {
        let dt = timer::delta(ctx).as_secs_f32();
        for (player_id, player) in self.players.iter_mut().enumerate() {
            // get the gamepad
            let gamepad = gamepad(ctx, player.gamepad_id);
            // advance the duration that the pressed buttons have been pressed
            for button_code in 1..20u16 {
                let button = u16_to_btn(button_code).unwrap();
                if gamepad.is_pressed(button) {
                    player.increase_button_pressed_duration(button, dt);
                }
            }
            // if LB is pressed for some time add all edges to the list of edges to distribute to
            const DISTRIBUTION_TRIGGER_DURATION: f32 = 1.0;
            if player.pressed_for_at_least(Button::LeftTrigger, DISTRIBUTION_TRIGGER_DURATION) {
                let p_node_id = self.game_state.player_node_ids[usize::from(player_id)];
                for e_id in self.physics_state.node_at(p_node_id).edge_indices.iter() {
                    self.game_state.add_troop_path_checked(p_node_id,*e_id, &self.physics_state);
                }
            }
            // if RB is pressed for some time remove all edges from the list of edges to distribute to
            if player.pressed_for_at_least(Button::RightTrigger, DISTRIBUTION_TRIGGER_DURATION) {
                self.game_state.player_node_mut(player_id as PlayerId).clear_troop_paths();
            }

            // HANDLE STICK INPUT
            use Axis::*;
            const DEADZONE: f32 = 0.70;
            // left stick
            let mut stick_active = true;
            let (x, y) = (gamepad.value(LeftStickX), -gamepad.value(LeftStickY));
            let norm = (x.powf(2.0) + y.powf(2.0)).sqrt();
            if norm < DEADZONE {
                // set the cooldown to 0, since the cooldown is meant for when a stick is continually pressed
                player.remove_left_axis_cooldown();
                stick_active = false;
            }
            if gamepad.is_pressed(South) {
                let g_node_ptr = self.game_state.player_node_mut(player_id as PlayerId) as *mut GameNode;
                unsafe {
                    match (*g_node_ptr).cell_type_mut() {
                        CellType::Wall => {},   // not sure if I really want to disallow adding edges coming from walls
                        CellType::Propulsion(consumption, current_angle) => {
                            // boost the cell into the chosen direction, burning units in the process
                            const BOOST_DEADZONE: f32 = 0.20;
                            if norm > BOOST_DEADZONE {
                                let angle = y.atan2(x);
                                // calculate the boost intensity
                                let boost = norm * 2.5;
                                // calculate the consumption of units that this boost causes
                                let new_consumption = *consumption + (boost / 50.);
                                // check if the consumption need can be met
                                let troop_op = (*g_node_ptr).troop_of_player_mut(player_id as PlayerId);
                                if let Some(troop) = troop_op {
                                    let cost = new_consumption.floor() as UnitCount;
                                    if troop.count > cost {
                                        // there are enough troops to pay the consumption need and still hold the node
                                        troop.remove_units(cost);
                                        *consumption = new_consumption % 1.;
                                        *current_angle = angle;
                                        // now that the cost has been payed boost the node
                                        let player_n_id = usize::from(self.game_state.player_node_ids[player_id]) as NId;
                                        let force = Vector2::new(boost * angle.cos(), boost * angle.sin());
                                        self.physics_state.node_at_mut(player_n_id).apply_force(force);
                                    }
                                }
                            }
                        }
                        _ => {
                            // new edge selection starts
                            let mut chosen_node = NO_NODE;
                            if stick_active {
                                let angle = y.atan2(x);
                                let player_n_id = usize::from(self.game_state.player_node_ids[player_id]);
                                // find the target node for the new edge
                                let mut smallest_diff: f32 = 1.5;  // the worst fit still needs to be better than this
                                // go through all nodes and find those in range
                                const NEW_EDGE_RANGE: f32 = 800.0;
                                for (n_id, node) in self.physics_state.node_iter().enumerate() {
                                    if n_id == player_n_id { continue; }
                                    let vec = node.position - self.physics_state.node_at(player_n_id as NId).position;
                                    // first check if it's in range
                                    if vec.norm() <= NEW_EDGE_RANGE {
                                        // then check if this edge already exists
                                        if self.physics_state.neighbors(player_n_id as NId).find(|id| *id == n_id as NId).is_none() {
                                            // calc the angle
                                            let e_angle = vec.y.atan2(vec.x);
                                            let diff = angle_diff_abs(e_angle, angle);
                                            // check if it fits better than anything found until now
                                            if diff < smallest_diff {
                                                smallest_diff = diff;
                                                chosen_node = n_id as NId;
                                            }
                                        }
                                    }
                                }
                            }
                            self.game_state.player_new_edge_n_ids[player_id] = if chosen_node == NO_NODE {
                                // reset the new edge selection (in other words set the new edge target back to None)
                                None
                            } else {
                                // save the selection, so it can be added when 'A' is released
                                Some(chosen_node)
                            }
                        }
                    }
                }
            }
            else if stick_active && player.try_left_axis_cooldown(dt) {
                let angle = y.atan2(x);
                // get the node where the player is currently at
                let p_node_id = self.game_state.player_node_ids[player_id];
                let p_node = &self.physics_state.node_at(p_node_id);
                // check the selected edge for its angle and move there if the direction pressed is roughly the same
                let mut chosen_node = NO_NODE;
                if let Some(pl_edge_id) = self.game_state.player_edge_ids[player_id] {
                    let edge = self.physics_state.edge_at(pl_edge_id);
                    if let EdgeType::Wall = edge.e_type {}
                    else {
                        let vec = self.physics_state.edge_vec_2d(pl_edge_id, p_node_id);
                        let e_angle = vec.y.atan2(vec.x);
                        const SELECTED_EDGE_PRIORITY_DIFF: f32 = 1.5;
                        if angle_diff_abs(e_angle, angle) < SELECTED_EDGE_PRIORITY_DIFF {
                            let node = edge.other_node(p_node_id);
                            if self.game_state.nodes[usize::from(node)].player_can_access(player_id as PlayerId) {
                                chosen_node = self.physics_state.edge_at(pl_edge_id).other_node(p_node_id);
                            }
                        }
                    }
                }
                if chosen_node == NO_NODE {
                    // go through all edges of the node and find the edge and associated neighbor node closest to the chosen angle
                    let mut smallest_diff: f32 = 1.5;  // the worst fit still needs to be better than this
                    for e_id in p_node.edge_indices.iter() {
                        let edge = self.physics_state.edge_at(*e_id as EId);
                        if let EdgeType::Wall = edge.e_type {}
                        else {
                            let vec = self.physics_state.edge_vec_2d(*e_id, p_node_id);
                            let edge_angle = vec.y.atan2(vec.x);
                            let difference = angle_diff_abs(edge_angle, angle);
                            if difference < smallest_diff {
                                let node = edge.other_node(p_node_id);
                                if self.game_state.nodes[usize::from(node)].player_can_access(player_id as PlayerId) {
                                    smallest_diff = difference;
                                    chosen_node = node;
                                }
                            }
                        }
                    }
                }
                if chosen_node != NO_NODE {
                    self.game_state.player_node_ids[player_id] = chosen_node;
                    // also set the chosen edge to None if it cannot be reached from the new node
                    if let Some(pl_edge_id) = self.game_state.player_edge_ids[player_id] {
                        if !self.physics_state.edge_at(pl_edge_id).contains_node(chosen_node) {
                            self.game_state.player_edge_ids[player_id] = None;
                        }
                    }
                }
            }
            // right stick
            let mut stick_active = true;
            let (x, y) = (gamepad.value(RightStickX), -gamepad.value(RightStickY));
            if (x.powf(2.0) + y.powf(2.0)).sqrt() < DEADZONE {
                stick_active = false;
            }
            if stick_active {
                let angle = y.atan2(x);
                // get the node where the player is currently at
                let p_node_id = self.game_state.player_node_ids[player_id];
                let p_node = self.physics_state.node_at(p_node_id);
                // go through all edges of the node and find the edge and associated neighbor node closest to the chosen angle
                let mut smallest_diff: f32 = 1.5;  // the worst fit still needs to be better than this
                let mut chosen_edge = NO_EDGE;
                for e_id in p_node.edge_indices.iter() {
                    let vec = self.physics_state.edge_vec_2d(*e_id, p_node_id);
                    let edge_angle = vec.y.atan2(vec.x);
                    let difference = angle_diff_abs(edge_angle, angle);
                    if difference < smallest_diff {
                        smallest_diff = difference;
                        chosen_edge = *e_id;
                    }
                }
                if chosen_edge != NO_EDGE {
                    self.game_state.player_edge_ids[player_id] = Some(chosen_edge);
                }
            }

            // handle button input
            if gamepad.is_pressed(Button::West) {
                // "X": reduce selected edge length
                const SHORTENING_SPEED: f32 = 500.0;
                if let Some(e_id) = self.game_state.player_edge_ids[usize::from(player_id)] {
                    // if an edge is selected and owned by the player shorten it
                    if can_control(player_id as PlayerId, self.game_state.edges[usize::from(e_id)].controlled_by()) && !(e_id < self.unchangeable_edges as EId) {
                        self.physics_state.edge_at_mut(e_id).shorten(timer::delta(ctx).as_secs_f32() * SHORTENING_SPEED);
                    }
                }
            }
            if gamepad.is_pressed(Button::North) {
                // "Y": increase selected edge length
                const LENGTHENING_SPEED: f32 = 500.0;
                if let Some(e_id) = self.game_state.player_edge_ids[usize::from(player_id)] {
                    // if an edge is selected and owned by the player shorten it
                    if can_control(player_id as PlayerId, self.game_state.edges[usize::from(e_id)].controlled_by()) && !(e_id < self.unchangeable_edges as EId) {
                        self.physics_state.edge_at_mut(e_id).lengthen(timer::delta(ctx).as_secs_f32() * LENGTHENING_SPEED);
                    }
                }
            }
        }
    }

    pub fn try_add_edge(g_state: &mut GameState, p_state: &mut PhysicsState, start_n_id: NId, target_n_id: NId, prox_walls: &mut Vec<Vec<NId>>, unchangeable_edges: usize) -> bool {
        const NEW_EDGE_UNIT_COST: UnitCount = 2;
        let game_node = &mut g_state.nodes[usize::from(start_n_id)];
        let ctrl = game_node.controlled_by();
        if ctrl != NO_PLAYER {
            if let Some(troop) = game_node.troop_of_player_mut(ctrl) {
                // check if the node can pay the unit cost
                if troop.count > NEW_EDGE_UNIT_COST {
                    // pay the price and add the new edge
                    troop.remove_units(NEW_EDGE_UNIT_COST);
                    return Self::add_edge_static(p_state, g_state,start_n_id, target_n_id, prox_walls, unchangeable_edges);
                }
            }
        }
        return false;
    }
}

#[repr(u16)]
#[derive(Copy, Clone)]
enum UnitCountDrawMode {
    NoDrawing = 0,
    Units = 1,
    DesiredCount = 2,
    Fight = 3,
}

struct PlayerState {
    color: Color,
    gamepad_id: GamepadId,
    left_axis_cooldown: f32,
    gamepad_pressed_timers: [f32; 20], // because there are 20 buttons
    unit_count_draw_mode: UnitCountDrawMode
}

impl PlayerState {
    fn new(gamepad_id: GamepadId, color: Color) -> PlayerState {
        PlayerState {
            color,
            gamepad_id,
            left_axis_cooldown: 0.0,
            gamepad_pressed_timers: [0.0; 20],
            unit_count_draw_mode: UnitCountDrawMode::NoDrawing,
        }
    }
    /// Advances the cooldown timer dt seconds and returns true if the cooldown is done.
    /// If that happens the cooldown is automatically reset.
    fn try_left_axis_cooldown(&mut self, dt: f32) -> bool {
        const COOLDOWN: f32 = 0.2;
        self.left_axis_cooldown -= dt;
        if self.left_axis_cooldown <= 0.0 {
            self.left_axis_cooldown = COOLDOWN;
            return true;
        }
        return false;
    }
    fn remove_left_axis_cooldown(&mut self) {
        self.left_axis_cooldown = 0.0;
    }
    fn pressed_for(&self, button: Button) -> f32 {
        self.gamepad_pressed_timers[usize::from(btn_to_u16(button))]
    }
    fn pressed_for_at_least(&self, button: Button, duration: f32) -> bool {
        self.gamepad_pressed_timers[usize::from(btn_to_u16(button))] >= duration
    }
    fn increase_button_pressed_duration(&mut self, button: Button, duration: f32) {
        self.gamepad_pressed_timers[usize::from(btn_to_u16(button))] += duration;
    }
    fn reset_button_pressed_duration(&mut self, button: Button) {
        self.gamepad_pressed_timers[usize::from(btn_to_u16(button))] = 0.0;
    }
    fn current_removal_type(&self) -> NodeEdgeOrNothing {
        const NODE_REMOVAL_DURATION: f32 = 1.25;
        const NO_REMOVAL_DURATION: f32 = 2.5;
        use NodeEdgeOrNothing::*;
        match self.pressed_for(Button::East) {
            d if d >= NO_REMOVAL_DURATION => Nothing,
            d if d >= NODE_REMOVAL_DURATION => Node,
            d if d != 0.0 => Edge,
            _ => Nothing,
        }
    }
}

enum NodeEdgeOrNothing {
    Node,
    Edge,
    Nothing,
}

impl event::EventHandler for MainState {
    fn update(&mut self, ctx: &mut Context) -> GameResult {
        let dt = timer::delta(ctx);
        //if timer::ticks(ctx) % 100 == 0 {
            //println!("Delta frame time: {:?} ", dt);
            //println!("Average FPS: {}", timer::fps(ctx));
        //}
        // handle user input
        self.handle_input(ctx);

        const DESIRED_SIMULATION_FPS: u32 = 60;
        const DESIRED_DELTA: f32 = 1.0 / (DESIRED_SIMULATION_FPS as f32);
        while timer::check_update_time(ctx, DESIRED_SIMULATION_FPS) {
            let secs = dt.as_secs_f32();
            let ratio = (DESIRED_DELTA / secs) * thread_rng().gen_range(0.997, 1.003);  // this is for some added randomness because it has
                                                                                                    // a nice effect, that is utterly empirical and hard to explain
                                                                                                    // just comment out the random factor to see it for yourself
            let dur = ratio * secs;
            // update the game state
            let mut edges_to_be_removed = Vec::<EId>::new();
            let mut nodes_to_be_split   = Vec::<NId>::new();
            let players_to_be_removed = self.game_state.update(&mut self.physics_state,
                                                               dur,
                                                               &mut self.proximity_nodes,
                                                               &mut self.proximity_walls,
                                                               &mut edges_to_be_removed,
                                                               &mut nodes_to_be_split,
                                                                self.unchangeable_nodes,
                                                                self.unchangeable_edges);
            Self::remove_multiple_edges_static(&mut self.physics_state, &mut self.game_state, &mut edges_to_be_removed, &mut self.proximity_walls, self.unchangeable_edges);
            self.split_nodes(nodes_to_be_split);
            for player in players_to_be_removed.into_iter() {
                self.remove_player(player);
            }
            // update the proximity state
            self.update_proximity_state(ctx);
            // update the physics simulation
            self.physics_state.simulate_step(dur, &self.proximity_nodes, &self.proximity_walls, &mut edges_to_be_removed, self.unmovable_nodes, self.unchangeable_edges);
            Self::remove_multiple_edges_static(&mut self.physics_state, &mut self.game_state, &mut edges_to_be_removed, &mut self.proximity_walls, self.unchangeable_edges);
            // position the camera nicely
            self.position_camera(ctx);
            self.ticks += 1;
        }
        // TODO: iterate over gamepads from time to time and check whether they're still connected;
        //       if not remove the player associated with the pad in question;
        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult {
        graphics::clear(ctx, Self::color_bg());

        // Draw the edges chosen by the players
        let mut add_last: SmallVec<[DrawParam; 8]> = smallvec![];
        for (i, player) in self.players.iter().enumerate() {
            // if the player is currently adding a new edge draw it (lightly) instead of the actually selected edge
            if let Some(target_n_id) = self.game_state.player_new_edge_n_ids[i] {
                let start = self.physics_state.node_at(self.game_state.player_node_ids[i]).position;
                let end = self.physics_state.node_at(target_n_id).position;
                let vec = end - start;
                let mut p = Self::draw_param_edge_without_edge_static(false,
                                                                      start,
                                                                      vec,
                                                                      self.edge_sprite_width,
                                                                      &player.color);
                p.color.a = 0.5;    // draw it lightly
                add_last.push(p);
            }
            else if let Some(edge_id) = self.game_state.player_edge_ids[i] {
                let mut p = self.draw_param_edge(self.physics_state.edge_at(edge_id), &self.game_state.edges[usize::from(edge_id)])
                    .color(if let NodeEdgeOrNothing::Edge = player.current_removal_type() { BLACK } else { WHITE } )
                    .src(Self::edge_src_rect_bg());
                p.scale.y *= 1.70;
                self.spr_b_edge.add(p);
            }
        }
        // Fill the edges spritebatch
        {
            let mut g_edge_iter = self.game_state.edges.iter();
            for (e_id, edge) in self.physics_state.edge_iter().enumerate() {
                let g_edge = g_edge_iter.next().unwrap();
                let unchangeable = self.is_unchangeable_edge(e_id as EId);
                Self::draw_edge(&self.players, ctx, self.edge_sprite_width, &self.physics_state,
                                &mut self.spr_b_edge, &mut self.spr_b_node, edge, g_edge, &self.game_state.nodes, self.spr_b_node_dim, unchangeable);
            }
        }

        // Draw the player positions
        for (i, player) in self.players.iter().enumerate() {
            let mut p = self.draw_param_node(self.game_state.player_node_ids[i], ctx)
                .color(if let NodeEdgeOrNothing::Node = player.current_removal_type() { BLACK } else { WHITE } );
            let bg_scale = (1. + (0.5 / p.scale.x)) * p.scale.x;
            p.scale = [bg_scale, bg_scale].into();
            self.spr_b_node.add(p);
        }
        // Fill the nodes spritebatch
        for (i, node) in self.physics_state.node_iter().enumerate() {
            let player_on_this = self.game_state.player_node_ids.iter().position(|n_id| *n_id == (i as NId));
            let c_type = self.game_state.nodes[usize::from(i)].cell_type();
            if let None = player_on_this {
                if let CellType::Wall = c_type {} else {
                    // draw the background
                    // TODO: instead of using scale here better use some prepared sprites (utilize offset to make it look better...)
                    let mut p = self.draw_param_node(i as NId, ctx)
                        .color(Self::color_bg());
                    let bg_scale = p.scale.x * (1. + (match c_type {
                        CellType::Cancer => 0.65,
                        CellType::Propulsion(_,_) => 0.65,
                        _ => 0.5,
                    }) / p.scale.x);
                    p.scale = [bg_scale, bg_scale].into();
                    self.spr_b_node.add(p);
                }
            }
            if let CellType::Wall = c_type {
                let p = self.draw_param_node(i as NId, ctx)
                    .color(Self::color_bg())
                    .src(self.draw_source_rect(&CellType::Basic));
                self.spr_b_node.add(p);
            }
            // draw the node itself
            self.spr_b_node.add(self.draw_param_node(i as NId, ctx));

            // draw the unit text
            let g_node = &self.game_state.nodes[i];
            let ctrl = g_node.controlled_by();
            use UnitCountDrawMode::*;
            // behavior depends on whether this is the current node of a player
            let (mut u_draw_mode, mut scale) =
                if let Some(p_id) = player_on_this {
                    const P_SCALE: f32 = 2.5;
                    // if it is, then it also depends on whether the player is currently changing the desired unit count
                    use Button::*;
                    (if self.players[p_id].pressed_for(LeftTrigger2) > 0. || self.players[p_id].pressed_for(RightTrigger2) > 0. {
                        DesiredCount
                    } else {
                        match self.players[p_id].unit_count_draw_mode {
                            NoDrawing => Units, // no drawing is not allowed when the player is there
                            other_mode => other_mode,
                        }
                    }, P_SCALE)
                } else {
                    const N_SCALE: f32 = 2.5;
                    (match ctrl {
                        NO_PLAYER | ANYONE_PLAYER => { NoDrawing },
                        CANCER_PLAYER => { NoDrawing }
                        p_id => { self.players[usize::from(p_id)].unit_count_draw_mode }
                    }, N_SCALE)
                };
            scale *= (node.radius / EMPTY_NODE_RADIUS).sqrt();
            if g_node.fighting() {
                u_draw_mode = Fight;
            }

            match u_draw_mode {
                NoDrawing => {},
                Fight => {
                    let (_col, curr_p_id, p_factor) = Self::battle_color_static(&self.players, ctx, g_node.troop_iter());
                    let unit_count_string = if let Some(t) = g_node.troop_of_player(curr_p_id) { t.count.to_string() } else { "0".to_string() };
                    let mut string_color = WHITE.clone();
                    string_color.a = p_factor;
                    println!("p_factor: {}", p_factor);
                    println!("string_color.a: {}", string_color.a);
                    self.spr_b_text.add(unit_count_string.as_str(),
                                        DrawParam::default().dest(node.position).scale(Vector2::new(scale, scale)).offset(Point2::new(0.5, 0.5)).color(string_color));
                },
                Units => {
                    let unit_count_string = if let Some(t) = g_node.troop_of_player(ctrl) { t.count.to_string() } else { "0".to_string() };
                    self.spr_b_text.add(unit_count_string.as_str(),
                                        DrawParam::default().dest(node.position).scale(Vector2::new(scale, scale)).offset(Point2::new(0.5, 0.5)).color(WHITE));
                },
                DesiredCount => {
                    self.spr_b_text.add(g_node.desired_unit_count().to_string().as_str(),
                                        DrawParam::default().dest(node.position).scale(Vector2::new(scale, scale)).offset(Point2::new(0.5, 0.5)).color(BLACK));
                },
            }
        }

        // draw the possible new edge over the others to make it a bit more visible
        for p in add_last.into_iter() {
            self.spr_b_edge.add(p);
        }

        // Draw the edge_bg (currently only used for player selections)
        //graphics::draw(ctx, &self.spr_b_edge_bg, DrawParam::new())?;
        // Draw the edges
        graphics::draw(ctx, &self.spr_b_edge, DrawParam::new())?;
        // Draw the nodes
        graphics::draw(ctx, &self.spr_b_node, DrawParam::new())?;
        // Draw the unit count texts
        graphics::draw(ctx, &self.spr_b_text, DrawParam::new())?;
        //graphics::draw_queued_text(ctx, DrawParam::new(), None, FilterMode::Linear)?;

        //self.spr_b_edge_bg.clear();
        self.spr_b_node.clear();
        self.spr_b_edge.clear();
        self.spr_b_text.clear();

        graphics::present(ctx)?;
        Ok(())
    }

    fn gamepad_button_down_event(&mut self, _ctx: &mut Context, btn: Button, id: GamepadId) {
        let player_id;
        if let Some(p_id) = self.identify_or_add_player(id) {
            player_id = p_id;
        } else {
            return; // player couldn't be added
        };
        use Button::*;
        // plan for control layout:
        match btn {
            // "B": cut selected edge (but not here, this is something to check when "B" is released)
            East => {},
            // "Y": increase selected edge length (but not here, this is something to check continually)
            North => {},
            // "X": reduce selected edge length (but not here, this is something to check continually)
            West => {},
            // "A": start adding a new node/edge or burn units for propulsion (propulsion cell only) (but not here, this is something to check continually)
            // "LB": add selected edge to list of troop destinations (send troops)
            LeftTrigger => {
                if let Some(e_id) = self.game_state.player_edge_ids[usize::from(player_id)] {
                    let n_id = self.game_state.player_node_ids[usize::from(player_id)];
                    self.game_state.add_troop_path_checked(n_id, e_id, &self.physics_state);
                }
            },
            // "RB": remove selected edge from list of troop destinations (stop sending troops)
            RightTrigger => {
                if let Some(e_id) = self.game_state.player_edge_ids[usize::from(player_id)] {
                    let n_id = self.game_state.player_node_ids[usize::from(player_id)];
                    self.game_state.nodes[usize::from(n_id)].remove_troop_path(&e_id);
                }
            },
            // "LT": increase desired troop count on this node
            LeftTrigger2 => {
                let game_node = self.game_state.player_node_mut(player_id);
                game_node.set_desired_unit_count(
                    match game_node.desired_unit_count().checked_add(1) {
                        Some(new_count) => new_count,
                        None => MAX_UNIT_COUNT
                    }
                );
            },
            // "RT": decrease desired troop count on this node
            RightTrigger2 => {
                let game_node = self.game_state.player_node_mut(player_id);
                game_node.set_desired_unit_count(
                    match game_node.desired_unit_count().checked_sub(1) {
                        Some(new_count) => new_count,
                        None => MIN_UNIT_COUNT
                    }
                );
            },
            // "D_PAD_DOWN":    transform cell into cancer cell
            DPadDown => {
                let n_id = self.game_state.player_node_ids[usize::from(player_id)];
                let unchangeable = self.is_unchangeable(n_id);
                let game_node = &mut self.game_state.nodes[usize::from(n_id)];
                game_node.try_start_mutation(CellType::Cancer, unchangeable);
            }
            // "D_PAD_RIGHT":  transform cell into wall cell
            DPadRight => {
                let n_id = self.game_state.player_node_ids[usize::from(player_id)];
                let unchangeable = self.is_unchangeable(n_id);
                let game_node = &mut self.game_state.nodes[usize::from(n_id)];
                game_node.try_start_mutation(CellType::Wall, unchangeable);
            }
            // "D_PAD_UP": transform cell into basic cell, or attempt a split mutation, if already basic
            DPadUp => {
                let n_id = self.game_state.player_node_ids[usize::from(player_id)];
                let unchangeable = self.is_unchangeable(n_id);
                let game_node = &mut self.game_state.nodes[usize::from(n_id)];
                if let CellType::Basic = game_node.cell_type() {
                    game_node.try_start_mutation(CellType::Split, unchangeable);
                } else {
                    game_node.try_start_mutation(CellType::Basic, unchangeable);
                }
            }
            // "D_PAD_LEFT":  transform cell into propulsion cell
            DPadLeft => {
                let n_id = self.game_state.player_node_ids[usize::from(player_id)];
                let unchangeable = self.is_unchangeable(n_id);
                let game_node = &mut self.game_state.nodes[usize::from(n_id)];
                game_node.try_start_mutation(CellType::Propulsion(0.0,0.0), unchangeable);
            }
            // "Select": change unit count draw mode
            Select => {
                let repr = self.players[usize::from(player_id)].unit_count_draw_mode as u16;
                println!("repr: {}", repr);
                unsafe { self.players[usize::from(player_id)].unit_count_draw_mode = transmute::<u16, UnitCountDrawMode>((repr + 1) % 3); }
            },
            _ => {}
        }
    }

    fn gamepad_button_up_event(&mut self, _ctx: &mut Context, btn: Button, id: GamepadId) {
        let player_id;
        if let Some(p_id) = self.identify_or_add_player(id) {
            player_id = p_id;
        } else {
            return; // player couldn't be added
        };
        let player = &self.players[usize::from(player_id)];
        let mut player_removed = false;
        use Button::*;
        match btn {
            // "B": cut selected edge or remove selected node
            East => {
                // whether it's the edge or the node (or nothing if held for long enough) depends on how long the button was pressed
                use NodeEdgeOrNothing::*;
                match player.current_removal_type() {
                    Nothing => {},
                    Node => {
                        let player_node_id = self.game_state.player_node_ids[usize::from(player_id)];
                        if self.game_state.nodes[usize::from(player_node_id)].controlled_by() == player_id && !self.is_unchangeable(player_node_id) {
                            player_removed = self.remove_node(player_node_id).contains(&player_id);
                        }
                    },
                    Edge => {
                        if let Some(e_id) = self.game_state.player_edge_ids[usize::from(player_id)] {
                            // if an edge is selected and owned by the player cut it
                            let e_ctrl = self.game_state.edges[usize::from(e_id)].controlled_by();
                            if can_control(player_id, e_ctrl) && !self.is_unchangeable_edge(e_id) {
                                self.remove_edge(e_id);
                            }
                        }
                    }
                }
            },
            South => {
                // 'A' was released
                // check if there's a new edge to add
                if let Some(target_n_id) = self.game_state.player_new_edge_n_ids[usize::from(player_id)] {
                    // check if the player controls the node completely
                    let p_n_id = self.game_state.player_node_ids[usize::from(player_id)];
                    Self::try_add_edge(&mut self.game_state, &mut self.physics_state, p_n_id, target_n_id, &mut self.proximity_walls, self.unchangeable_edges);
                }
                self.game_state.player_new_edge_n_ids[usize::from(player_id)] = None;
            }
            _ => {}
        }
        // the player identity needs to be checked here again, because due to the removal of a node
        // the player ids can change (and thereby get invalidated)
        if !player_removed {
            self.players[usize::from(player_id)].reset_button_pressed_duration(btn);
        }
    }

    fn resize_event(&mut self, ctx: &mut Context, width: f32, height: f32) {
        println!("Resized screen to {}, {}", width, height);
        // set to my native screen resolution just to have some convention to work with
        graphics::set_screen_coordinates(ctx, Rect::new(-3840.0, -2160.0, 3.0*3840.0, 3.0*2160.0)).unwrap();
    }
}

pub fn main() -> GameResult {

    let resource_dir = if let Ok(manifest_dir) = env::var("CARGO_MANIFEST_DIR") {
        let mut path = path::PathBuf::from(manifest_dir);
        path.push("resources");
        path
    } else {
        path::PathBuf::from("./resources")
    };

    let cb = ggez::ContextBuilder::new("Nodes and Edges", "PSteinhaus")
        .add_resource_path(resource_dir)
        .window_mode(
            conf::WindowMode::default()
                .fullscreen_type(conf::FullscreenType::Windowed)
                .dimensions(1280.0, 720.0)
        )
        .window_setup(
            conf::WindowSetup::default().samples(
                conf::NumSamples::from_u32(4)
                    .expect("Option msaa needs to be 1, 2, 4, 8 or 16!"),
            ),
        );
    let (ctx, event_loop) = &mut cb.build()?;
    println!("drawable size: {:?}", graphics::drawable_size(&ctx));

    let state = &mut MainState::new(ctx);
    state.init_lvl(ctx);
    /*
    println!("Factor: {}", graphics::window(ctx).get_hidpi_factor());
    println!("Inner size: {:?}", graphics::window(ctx).get_inner_size());
    println!("Outer size: {:?}", graphics::window(ctx).get_outer_size());
    println!("size: {:?}", graphics::size(ctx));
    println!("Renderer: {}", graphics::renderer_info(ctx).unwrap());
    */
    event::run(ctx, event_loop, state)
}