#![feature(drain_filter)]
#![feature(destructuring_assignment)]

use ggez;
use ggez::{event, conf};
use ggez::graphics;
use ggez::nalgebra::{Point2, Vector2};
use ggez::nalgebra as na;
use ggez::timer;
use ggez::{Context, GameResult};
use std::env;
use std::path;
use std::time::Instant;
use crate::physics::{PhysicsState, Node};
use crate::game_mechanics::*;
use rand::{Rng, thread_rng};
use ggez::graphics::{Rect, DrawMode, Image, DrawParam, Drawable, Color, WHITE, BLACK};
use ggez::graphics::spritebatch::SpriteBatch;
use ggez::input::gamepad::{GamepadId, gamepad};
use ggez::event::{Button, Axis};
use crate::helpers::angle_diff_abs;

mod physics;
mod game_mechanics;
mod helpers;

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

struct MainState {
    players: Vec<PlayerState>,
    game_state: GameState,
    physics_state: PhysicsState,
    edge_sprite_width: f32,
    spr_b_node: SpriteBatch,
    spr_b_edge: SpriteBatch,
    color_no_player: Color
}

impl MainState {
    fn new(ctx: &mut Context) -> MainState {
        let img = Image::new(ctx, "/edge.png").unwrap();
        MainState {
            players: Vec::new(),
            game_state: GameState::new(),
            physics_state: PhysicsState::new(),
            edge_sprite_width: img.width() as f32,
            spr_b_node: SpriteBatch::new(Image::new(ctx, "/node.png").unwrap()),
            spr_b_edge: SpriteBatch::new(img),
            color_no_player: Color::from_rgb(110, 110, 110),
        }
    }

    fn add_node(&mut self, position: Point2<f32>) {
        // add to physics
        self.physics_state.add_node(position);
        // add to game state
        self.game_state.add_node();
    }

    fn remove_node(&mut self, node_index: NId) {
        // if a player is on this node place him somewhere else
        // only send him to nodes that he can actually be on (i.e. his own or those in battle state carrying his troops)
        // collect players that have to be removed (in case there are any) and remove them afterwards
        let mut players_to_remove: Vec<PlayerId> = Vec::new();
        for (p_id, player_node) in self.game_state.player_node_ids.iter_mut().enumerate() {
            if *player_node == node_index {
                let mut searching_node = true;
                // first try to send him to a neighbor
                for neighbor in self.physics_state.neighbors(*player_node) {
                    if self.game_state.nodes[usize::from(neighbor)].player_can_access(p_id as PlayerId) {
                        *player_node = neighbor;
                        searching_node = false;
                        break;
                    }
                }
                // if this fails just send him to some node that he can access
                if searching_node {
                    for (n_id, game_node) in self.game_state.nodes.iter().enumerate() {
                        if game_node.player_can_access(p_id as PlayerId) {
                            *player_node = n_id as NId;
                            searching_node = false;
                            break;
                        }
                    }
                }
                // if this fails as well then the player has lost (and is removed from the game for now)
                if searching_node {
                    players_to_remove.push(p_id as PlayerId)
                }
            }
        }
        for p_id in players_to_remove {
            self.remove_player(p_id);
        }
        // first remove all edges to this node
        while !self.physics_state.node_at(node_index).edge_indices.is_empty() {
            let e_id = *self.physics_state.node_at(node_index).edge_indices.last().unwrap();
            self.remove_edge(e_id);
        }
        // remove from game state
        self.game_state.remove_node(node_index);
        // remove it from physics
        self.physics_state.remove_node(node_index);
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
        // first check if you can add this edge
        if self.physics_state.can_add_edge(node1_index, node2_index) {
            // add to physics
            self.physics_state.add_edge(node1_index, node2_index);
            // add to game state
            self.game_state.add_edge();
            return true;
        }
        return false;
    }

    fn remove_edge(&mut self, edge_index: EId) {
        // get the nodes connected by this edge
        let n_ids = self.physics_state.edge_at(edge_index).node_indices;
        // remove from game state
        self.game_state.remove_edge(edge_index, n_ids);
        // remove from physics
        self.physics_state.remove_edge(edge_index);
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

    fn add_player(&mut self, gamepad_id: GamepadId, color: Color) {
        println!("adding player with GamepadId {:?}", gamepad_id);
        self.players.push(PlayerState::new(gamepad_id, color));
        let mut rng = thread_rng();
        // for now choose a random node for the player to start on
        self.game_state.player_node_ids.push(rng.gen_range(0, self.node_count() as NId));
        self.game_state.player_edge_ids.push(None);
    }

    fn identify_or_add_player(&mut self, gamepad_id: GamepadId) -> PlayerId {
        // identify the player
        let mut player_id = self.player_id_using(gamepad_id);
        // if there is no player using this gamepad add one
        if player_id == NO_PLAYER {
            let rgb = thread_rng().gen::<(u8,u8,u8)>();    // choose a random color
            self.add_player(gamepad_id, Color::from(rgb));
            player_id = (self.players.len() - 1) as PlayerId;
        }
        player_id
    }

    fn player_color(&self, id: PlayerId) -> &Color {
        match id {
            NO_PLAYER => &self.color_no_player,
            ANYONE_PLAYER => &WHITE,
            CANCER_PLAYER => &BLACK,
            other_id => &self.players[usize::from(other_id)].color
        }
    }

    fn player_node(&self, id: PlayerId) -> &Node {
        &self.physics_state.node_at(self.game_state.player_node_ids[usize::from(id)])
    }

    fn draw_param_node(&self, n_id: NId) -> DrawParam {
        let node = &self.physics_state.node_at(n_id);

        DrawParam::new()
            .offset(Point2::new(0.5, 0.5))
            .dest(Point2::new(node.position.x, node.position.y))
            .color(*self.player_color(self.game_state.nodes[usize::from(n_id)].controlled_by()))
    }

    fn draw_param_edge(&self, e_id: usize) -> DrawParam {
        let edge = &self.physics_state.edge_at(e_id as EId);
        let (node1, node2) = (&self.physics_state.node_at(edge.node_indices[0]), &self.physics_state.node_at(edge.node_indices[1]));
        let vec: Vector2<f32> = node2.position - node1.position;

        DrawParam::new()
            .offset(Point2::new(0.0, 0.5))
            .dest(Point2::new(node1.position.x, node1.position.y))
            .rotation(na::RealField::atan2(vec.y, vec.x))
            .scale(Vector2::new(vec.norm() / self.edge_sprite_width, 1.0))
            .color(*self.player_color(self.game_state.edges[e_id].controlled_by()))
    }

    fn handle_input(&mut self, ctx: &mut Context) {
        for (player_id, player) in self.players.iter_mut().enumerate() {
            // get the gamepad
            let gamepad = gamepad(ctx, player.gamepad_id);

            // handle stick input
            use Axis::*;
            const DEADZONE: f32 = 0.75;
            // left stick
            let mut stick_active = true;
            let (x, y) = (gamepad.value(LeftStickX), -gamepad.value(LeftStickY));
            if (x.powf(2.0) + y.powf(2.0)).sqrt() < DEADZONE {
                // set the cooldown to 0, since the cooldown is meant for when a stick is continually pressed
                player.remove_left_axis_cooldown();
                stick_active = false;
            }
            if stick_active && player.try_left_axis_cooldown(timer::delta(ctx).as_secs_f32()) {
                let angle = y.atan2(x);
                // get the node where the player is currently at
                let p_node_id = self.game_state.player_node_ids[player_id];
                let p_node = &self.physics_state.node_at(p_node_id);
                // check the selected edge for its angle and move there if the direction pressed is roughly the same
                let mut chosen_node = NO_NODE;
                if let Some(pl_edge_id) = self.game_state.player_edge_ids[player_id] {
                    let vec = self.physics_state.edge_vec_2d(pl_edge_id, p_node_id);
                    let e_angle = vec.y.atan2(vec.x);
                    const SELECTED_EDGE_PRIORITY_DIFF: f32 = 1.5;
                    if angle_diff_abs(e_angle, angle) < SELECTED_EDGE_PRIORITY_DIFF {
                        chosen_node = self.physics_state.edge_at(pl_edge_id).other_node(p_node_id);
                    }
                }
                if chosen_node == NO_NODE {
                    // go through all edges of the node and find the edge and associated neighbor node closest to the chosen angle
                    let mut smallest_diff: f32 = 1.5;  // the worst fit still needs to be better than this
                    let mut chosen_edge = NO_EDGE;
                    for e_id in p_node.edge_indices.iter() {
                        let vec = self.physics_state.edge_vec_2d(*e_id, p_node_id);
                        let edge_angle = vec.y.atan2(vec.x);
                        let difference = angle_diff_abs(edge_angle, angle);
                        if difference < smallest_diff {
                            smallest_diff = difference;
                            chosen_node = self.physics_state.edge_at(*e_id).other_node(p_node_id);
                            chosen_edge = *e_id;
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
                let p_node = &self.physics_state.node_at(p_node_id);
                // go through all edges of the node and find the edge and associated neighbor node closest to the chosen angle
                let mut smallest_diff: f32 = 1.5;  // the worst fit still needs to be better than this
                let mut chosen_node = NO_NODE;
                let mut chosen_edge = NO_EDGE;
                for e_id in p_node.edge_indices.iter() {
                    let vec = self.physics_state.edge_vec_2d(*e_id, p_node_id);
                    let edge_angle = vec.y.atan2(vec.x);
                    let difference = angle_diff_abs(edge_angle, angle);
                    if difference < smallest_diff {
                        smallest_diff = difference;
                        chosen_node = self.physics_state.edge_at(*e_id).other_node(p_node_id);
                        chosen_edge = *e_id;
                    }
                }
                if chosen_edge != NO_EDGE {
                    self.game_state.player_edge_ids[player_id] = Some(chosen_edge);
                }
            }

            // handle button input
            if gamepad.is_pressed(Button::West) {
                // "X": reduce selected edge length (but not here, this is something to check continually)
                const SHORTENING_SPEED: f32 = 500.0;
                if let Some(e_id) = self.game_state.player_edge_ids[usize::from(player_id)] {
                    // if an edge is selected and owned by the player shorten it
                    if self.game_state.edges[usize::from(e_id)].controlled_by() == (player_id as PlayerId) {
                        self.physics_state.edge_at_mut(e_id).shorten(timer::delta(ctx).as_secs_f32() * SHORTENING_SPEED);
                    }
                }
            }
        }
    }
}

struct PlayerState {
    color: Color,
    gamepad_id: GamepadId,
    left_axis_cooldown: f32
}

impl PlayerState {
    fn new(gamepad_id: GamepadId, color: Color) -> PlayerState {
        PlayerState {
            color,
            gamepad_id,
            left_axis_cooldown: 0.0
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
}

impl event::EventHandler for MainState {
    fn update(&mut self, ctx: &mut Context) -> GameResult {
        let dt = timer::delta(ctx);
        if timer::ticks(ctx) % 100 == 0 {
            println!("Delta frame time: {:?} ", dt);
            println!("Average FPS: {}", timer::fps(ctx));
        }
        // handle user input
        self.handle_input(ctx);
        // update the game state
        self.game_state.update(&self.physics_state, dt.as_secs_f32());
        // update the physics simulation
        const DESIRED_SIMULATION_FPS: u32 = 60;
        while timer::check_update_time(ctx, DESIRED_SIMULATION_FPS) {
            self.physics_state.simulate_step(dt.as_secs_f32());
        }
        // TODO: iterate over gamepads from time to time and check whether they're still connected;
        //       if not remove the player associated with the pad in question;
        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult {
        graphics::clear(ctx, graphics::Color::from((80u8, 80u8, 80u8)));

        // Fill the nodes spritebatch
        for (i, _node) in self.physics_state.node_iter().enumerate() {
            self.spr_b_node.add(self.draw_param_node(i as NId));
        }
        // Draw the player positions
        for (i, player) in self.players.iter().enumerate() {
            let p = self.draw_param_node(self.game_state.player_node_ids[i])
                .color(player.color);
            self.spr_b_node.add(p);
        }

        // Fill the edges spritebatch
        for (i, _edge) in self.physics_state.edge_iter().enumerate() {
            self.spr_b_edge.add(self.draw_param_edge(i));
        }
        // Draw the edges chosen by the players
        for (i, player) in self.players.iter().enumerate() {
            if let Some(edge_id) = self.game_state.player_edge_ids[i] {
                let p = self.draw_param_edge(usize::from(edge_id))
                    .color(player.color);
                self.spr_b_edge.add(p);
            }
        }

        // Draw the edges
        graphics::draw(ctx, &self.spr_b_edge, DrawParam::new())?;
        // Draw the nodes
        graphics::draw(ctx, &self.spr_b_node, DrawParam::new())?;

        self.spr_b_node.clear();
        self.spr_b_edge.clear();

        graphics::present(ctx)?;
        Ok(())
    }

    fn gamepad_button_down_event(&mut self, ctx: &mut Context, btn: Button, id: GamepadId) {
        let player_id = self.identify_or_add_player(id);
        use Button::*;
        // plan for control layout:
        match btn {
            // "B": cut selected edge
            East => {
                if let Some(e_id) = self.game_state.player_edge_ids[usize::from(player_id)] {
                    // if an edge is selected and owned by the player cut it
                    //if self.game_state.edges[usize::from(e_id)].controlled_by == player_id {
                    self.remove_edge(e_id);
                    //}
                }
            },
            // "Y": destroy selected node
            North => {
                let player_node_id = self.game_state.player_node_ids[usize::from(player_id)];
                //if self.game_state.nodes[usize::from(player_node_id)].controlled_by == player_id {
                    self.remove_node(player_node_id);
                //}
            },
            // "X": reduce selected edge length (but not here, this is something to check continually)
            West => {},
            // "A": start adding a new node/edge or burn units for propulsion (propulsion cell only) (but not here, this is something to check continually)
            // "LB": add selected edge to list of troop destinations (send troops)
            LeftTrigger2 => {
                if let Some(e_id) = self.game_state.player_edge_ids[usize::from(player_id)] {
                    let n_id = self.game_state.player_node_ids[usize::from(player_id)];
                    self.game_state.nodes[usize::from(n_id)].add_troop_path(e_id);
                }
            }
            // "RB": remove selected edge from list of troop destinations (stop sending troops)
            RightTrigger2 => {
                if let Some(e_id) = self.game_state.player_edge_ids[usize::from(player_id)] {
                    let n_id = self.game_state.player_node_ids[usize::from(player_id)];
                    self.game_state.nodes[usize::from(n_id)].remove_troop_path(&e_id);
                }
            }
            // "LT": increase desired troop count on this node
            LeftTrigger => {
                let game_node = &mut self.game_state.nodes[usize::from(self.game_state.player_node_ids[usize::from(player_id)])];
                game_node.set_desired_unit_count(
                    match game_node.desired_unit_count().checked_add(1) {
                        Some(new_count) => new_count,
                        None => MAX_UNIT_COUNT
                    }
                );
            }
            // "RT": decrease desired troop count on this node
            RightTrigger => {
                let game_node = &mut self.game_state.nodes[usize::from(self.game_state.player_node_ids[usize::from(player_id)])];
                game_node.set_desired_unit_count(
                    match game_node.desired_unit_count().checked_sub(1) {
                        Some(new_count) => new_count,
                        None => MIN_UNIT_COUNT
                    }
                );
            }
            // "D_PAD_UP":    transform cell into cancer cell
            // "D_PAD_LEFT":  transform cell into wall cell
            // "D_PAD_RIGHT": transform cell into propulsion cell
            // "D_PAD_DOWN":  transform cell into basic cell
            _ => {}
        }
    }

    fn gamepad_button_up_event(&mut self, ctx: &mut Context, btn: Button, id: GamepadId) {
        let player_id = self.identify_or_add_player(id);
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
                .dimensions(1280.0 , 720.0)
        )
        .window_setup(
            conf::WindowSetup::default().samples(
                conf::NumSamples::from_u32(1)
                    .expect("Option msaa needs to be 1, 2, 4, 8 or 16!"),
            ),
        );
    let (ctx, event_loop) = &mut cb.build()?;
    println!("drawable size: {:?}", graphics::drawable_size(&ctx));

    let state = &mut MainState::new(ctx);
    // add some nodes
    let mut rng = rand::thread_rng();

    for _ in 0..50 {
        state.add_node(Point2::new(rng.gen_range(100.0, 3740.0), rng.gen_range(100.0, 2060.0)));
    }
    //state.add_node(Point2::new(0.0, 0.0));
    /*
    state.add_node(Point2::new(120.0, 75.0));
    state.add_node(Point2::new(78.0, 160.0));
    state.add_node(Point2::new(80.0, 230.0));
    */
    // add some edges
    // in this case a long trail
    /*
    for i in 0..9 {
        state.add_edge(i, i+1);
    }
    for i in 0..8 {
        state.add_edge(i, i+2);
    }
    */

    // in this case the edges are random
    {
        let node_len = state.physics_state.node_count() as NId;
        for _ in 0..100 {
            let (i, j) = (rng.gen_range(0, node_len), rng.gen_range(0, node_len));
            state.add_edge(i, j);
        }
    }

    // Now let's remove some stuff (for test purposes)
    {
        //state.remove_edge(0);
        for _ in 0..20 {
            state.remove_edge(rng.gen_range(0, state.physics_state.edge_count()) as EId);
        }
        for _ in 0..10 {
            state.remove_node(rng.gen_range(0, state.physics_state.node_count()) as NId);
        }
    }
    // Remove all nodes without edges (for beauty purposes)
    {
        let mut i = 0;
        while i < state.physics_state.node_count() {
            if state.physics_state.node_at(i as NId).edge_indices.is_empty() {
                state.remove_node(i as NId);
            }
            else { i+=1; }
        }
    }

    /*
    state.add_edge(0,1);
    state.add_edge(1,2);
    state.add_edge(2,0);
    state.add_edge(2,3);
     */
    /*
    println!("Factor: {}", graphics::window(ctx).get_hidpi_factor());
    println!("Inner size: {:?}", graphics::window(ctx).get_inner_size());
    println!("Outer size: {:?}", graphics::window(ctx).get_outer_size());
    println!("size: {:?}", graphics::size(ctx));
    println!("Renderer: {}", graphics::renderer_info(ctx).unwrap());
    */
    event::run(ctx, event_loop, state)
}
