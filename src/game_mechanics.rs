use std::collections::BTreeSet;

use ggez::nalgebra::clamp;
use smallvec::{SmallVec};

use fighting::{AdvancingTroop, EdgeFight, Fight, Troop};

use crate::{ANYONE_PLAYER, CANCER_PLAYER, EId, MainState, MAX_UNIT_COUNT, NId, NO_PLAYER, PlayerId, UnitCount, PlayerState};
use crate::game_mechanics::CellType::Basic;
use crate::helpers::*;
use crate::physics::PhysicsState;
use crate::proximity::proximity_nodes;

pub mod fighting;

pub struct GameState {
    /// on which node each player currently is
    pub player_node_ids: Vec<NId>,
    /// which edge is currently selected by each player
    pub player_edge_ids: Vec<Option<EId>>,
    /// new edges, which are added by the players, are stored here before being added
    pub player_new_edge_n_ids: Vec<Option<NId>>,
    pub nodes: Vec<GameNode>,
    pub edges: Vec<GameEdge>,
    troop_distribution_timer: Timer,
    unit_production_timer_producer: Timer,
    unit_production_timer_cancer: Timer,
}

impl GameState {
    pub fn new() -> GameState {
        GameState {
            player_node_ids: Vec::new(),
            player_edge_ids: Vec::new(),
            player_new_edge_n_ids: Vec::new(),
            nodes: Vec::new(),
            edges: Vec::new(),
            troop_distribution_timer: Timer::new(),
            unit_production_timer_producer: Timer::new(),
            unit_production_timer_cancer: Timer::new(),
        }
    }
    pub fn add_player(&mut self, start_n_id: NId, physics_state: &PhysicsState) {
        self.player_node_ids.push(start_n_id);
        self.player_edge_ids.push(None);
        self.player_new_edge_n_ids.push(None);
        // give him some starting units
        self.add_units(physics_state, start_n_id, (self.player_node_ids.len()-1) as PlayerId, 20);
    }
    pub fn player_node(&self, p_id: PlayerId) -> &GameNode {
        &self.nodes[usize::from(self.player_node_ids[usize::from(p_id)])]
    }
    pub fn player_node_mut(&mut self, p_id: PlayerId) -> &mut GameNode {
        &mut self.nodes[usize::from(self.player_node_ids[usize::from(p_id)])]
    }
    /// Add the troop path only if it doesn't lead to a producer
    pub fn add_troop_path_checked(&mut self, n_id: NId, path: EId, physics_state: &PhysicsState) {
        let target_n_id = physics_state.edge_at(path).other_node(n_id);
        let target_node_c_type = self.nodes[usize::from(target_n_id)].cell_type;
        let node = &mut self.nodes[usize::from(n_id)];
        Self::add_troop_path_static(path, node, target_node_c_type);
    }
    pub fn add_troop_path_static(path: EId, node: &mut GameNode, target_node_c_type: CellType) {
        if let CellType::Producer = target_node_c_type {
            // the target is a producer, don't add the path
        } else {
            node.add_troop_path(path);
        }
    }
    pub fn add_node(&mut self, cell_type: CellType) {
        self.nodes.push(GameNode::with_type(cell_type));
    }
    pub fn add_edge(&mut self, n_ids: &[NId; 2]) -> bool {
        let mut edge = GameEdge::new();
        let g_nodes: SmallVec<[&GameNode; 2]> = n_ids.iter().map(|n_id| &self.nodes[usize::from(*n_id)]).collect();
        let cell_types: SmallVec<[CellType; 2]> = g_nodes.iter().map(|g_n| g_n.cell_type).collect();

        // give it the correct controller
        edge.update_controlled_by([g_nodes[0], g_nodes[1]]);
        std::mem::drop(g_nodes);

        // check if this edge contains a cancer cell or a producer
        self.edges.push(edge);
        let mut is_wall = true;
        for i in 0..2 {
            use CellType::*;
            match cell_types[i] {
                Producer | Cancer => {
                    // if so, try to add this new edge to its list of troop distribution targets
                    let node = &mut self.nodes[usize::from(n_ids[i])];
                    let e_id = (self.edges.len() - 1) as EId;
                    Self::add_troop_path_static(e_id, node, cell_types[1 - i]);

                    is_wall = false;
                },
                Wall => {},
                _ => { is_wall = false; }
            }
        }
        is_wall
    }
    pub fn remove_node(&mut self, node_index: NId) {
        // remove the game node from the collection
        self.nodes.swap_remove(usize::from(node_index));
    }

    pub fn remove_edge(&mut self, edge_index: EId, n_ids: [NId; 2]) {
        let last_index = (self.edges.len() - 1) as EId;
        // make sure no player keeps this edge as his selected edge
        for edge in self.player_edge_ids.iter_mut() {
            if let Some(selected_e_id) = edge {
                if *selected_e_id == edge_index {
                    *edge = None;
                } else if *selected_e_id == last_index {
                    // also make sure the last edge index is updated properly
                    *selected_e_id = edge_index;
                }
            }
        }
        // remove this edge from all lists of paths to send troops through kept by the nodes
        for n_id in n_ids.iter() {
            self.nodes[usize::from(*n_id)].remove_troop_path(&edge_index);
        }
        self.edges.swap_remove(usize::from(edge_index));
        // do housekeeping (i.e. inform all nodes that the edge found at the last index is now found at edge_index)
        if last_index != edge_index {
            for node in self.nodes.iter_mut() {
                if node.troop_send_paths.remove(&last_index) {
                    node.troop_send_paths.insert(edge_index);
                }
            }
        }
    }

    pub fn remove_player(&mut self, p_id: PlayerId) {
        // remove him from the list of player nodes and edges
        self.player_node_ids.swap_remove(usize::from(p_id));
        self.player_edge_ids.swap_remove(usize::from(p_id));
        // now do housekeeping
        // TODO: remove all his units (also the travelling ones) (and thereby turn all his nodes and edges into uncontrolled territory)

    }

    fn check_for_troop_distribution(&mut self, dt: f32) -> bool {
        const DURATION_BETWEEN_DISTRIBUTIONS: f32 = 0.75;
        self.troop_distribution_timer.check(dt, DURATION_BETWEEN_DISTRIBUTIONS)
    }

    fn check_for_unit_production_producer(&mut self, dt: f32) -> bool {
        const DURATION_BETWEEN_UNITS_PRODUCED: f32 = 0.75;
        self.unit_production_timer_producer.check(dt, DURATION_BETWEEN_UNITS_PRODUCED)
    }

    fn check_for_unit_production_cancer(&mut self, dt: f32) -> bool {
        const DURATION_BETWEEN_UNITS_PRODUCED: f32 = 2.25;
        self.unit_production_timer_cancer.check(dt, DURATION_BETWEEN_UNITS_PRODUCED)
    }

    pub fn add_units(&mut self, physics_state: &PhysicsState, n_id: NId, p_id: PlayerId, unit_count: UnitCount) -> UnitCount {
        let (units_added, control_changed) = self.nodes[usize::from(n_id)].add_units(p_id, unit_count);
        if control_changed { self.update_node_and_edges_control(n_id, physics_state); }
        units_added
    }
    /// Returns true if the player is to be removed from the game
    pub fn kick_player_from_node(&mut self, p_id: PlayerId, n_id: NId, physics_state: &PhysicsState) -> bool {
        Self::kick_player_from_node_static(&mut self.player_node_ids[usize::from(p_id)], p_id, n_id, physics_state, &self.nodes)
    }

    pub fn kick_player_from_node_static(p_n_id: &mut NId, p_id: PlayerId, n_id: NId, physics_state: &PhysicsState, nodes: &[GameNode]) -> bool {
        if *p_n_id == n_id {
            let mut searching_node = true;
            // first try to send him to a neighbor
            for neighbor in physics_state.neighbors(*p_n_id) {
                if nodes[usize::from(neighbor)].player_can_access(p_id as PlayerId) {
                    *p_n_id = neighbor;
                    searching_node = false;
                    break;
                }
            }
            // if this fails just send him to some node that he can access
            if searching_node {
                for (new_n_id, game_node) in nodes.iter().enumerate() {
                    let new_n_id = new_n_id as NId;
                    if new_n_id != n_id && game_node.player_can_access(p_id as PlayerId) {
                        *p_n_id = new_n_id;
                        searching_node = false;
                        break;
                    }
                }
            }
            // if this fails as well then the player has lost (and is removed from the game for now)
            if searching_node {
                // TODO: maybe check whether the player still has some travelling units left and keep him if he has
                // if we choose to do that though, we need to change the program to respect that there can be players without a node
                return true;
            }
        }
        return false;
    }

    fn update_node_and_edges_control(&mut self, n_id: NId, physics_state: &PhysicsState) -> SmallVec<[PlayerId; 4]> {
        let g_node = &self.nodes[usize::from(n_id)];
        for e_id in physics_state.node_at(n_id).edge_indices.iter() {
            // get the other node of this edge
            let other_n_id = physics_state.edge_at(*e_id).other_node(n_id);
            let game_nodes = [g_node, &self.nodes[usize::from(other_n_id)]];
            self.edges[usize::from(*e_id)].update_controlled_by(game_nodes);
        }
        // also kick all players that might be on this node if they have no units here anymore
        let mut players_to_be_removed = SmallVec::<[PlayerId; 4]>::new();
        let controller = g_node.controlled_by();
        if controller != NO_PLAYER {
            for (p_id, p_n_id) in self.player_node_ids.iter_mut().enumerate() {
                let p_id = p_id as PlayerId;
                if n_id == *p_n_id && controller != p_id {
                    if Self::kick_player_from_node_static(p_n_id, p_id, n_id, physics_state, &self.nodes) {
                        players_to_be_removed.push(p_id);
                    }
                }
            }
        }
        return players_to_be_removed;
    }
    /// Returns players who have to be removed.
    pub fn update(&mut self, physics_state: &mut PhysicsState, dt: f32, prox_nodes: &Vec<Vec<NId>>, prox_walls: &mut Vec<Vec<EId>>, e_to_b_rem: &mut Vec<EId>) -> SmallVec<[PlayerId; 4]> {
        // update all nodes
        let distribute_troops   = self.check_for_troop_distribution(dt);
        let production_producer = self.check_for_unit_production_producer(dt);
        let production_cancer   = self.check_for_unit_production_cancer(dt);
        // collect the nodes that switch their control along the way
        let mut control_changed_nodes = SmallVec::<[NId; 32]>::new();
        let mut edges_to_try_add = SmallVec::<[(NId, NId); 32]>::new();
        // I need to check different nodes at the same time, so I need a raw pointer to go around Rusts safety checks
        let nodes_ptr: *const Vec<GameNode> = &self.nodes;
        use CellType::*;
        for (n_id, node) in self.nodes.iter_mut().enumerate() {
            // first manage possible fights
            if node.fighting() {
                let fight_ended = node.advance_fight(dt);
                if fight_ended {
                    // a control change has happened, update the edges of this node
                    control_changed_nodes.push(n_id as NId);
                }
            }

            // then check for mutations and advance them
            if node.advance_mutation(physics_state, dt, n_id as NId, unsafe {&*nodes_ptr}, e_to_b_rem) {
                control_changed_nodes.push(n_id as NId);
            }

            // if this cell is owned by the cancer player but not a cancer cell check whether you can turn it into one and do so if you can
            if node.controlled_by() == CANCER_PLAYER {
                match node.cell_type {
                    Cancer => {},
                    _ => {
                        node.try_start_mutation(Cancer);
                    }
                }
            }

            // manage special cell type behavior
            match node.cell_type() {
                Producer => {
                    if production_producer { node.add_units(ANYONE_PLAYER, 1); }
                },
                Cancer => {
                    if production_cancer { node.add_units(CANCER_PLAYER, 1); }
                    // let cancer try to connect to other nodes in range
                    let neighbors: SmallVec<[NId; 16]> = physics_state.neighbors(n_id as NId).collect();
                    for close_n_id in proximity_nodes(prox_nodes, n_id as NId) {
                        let close_n_id = *close_n_id;
                        const CONNECTION_RANGE: f32 = 600.;
                        // but don't try to connect to wall cells
                        unsafe {
                            let close_node = & ( * nodes_ptr)[usize::from(close_n_id)];
                            match close_node.cell_type {
                                Wall => {},
                                _ => {
                                    if physics_state.distance(n_id as NId, close_n_id) <= CONNECTION_RANGE
                                        && (*close_node).controlled_by() != CANCER_PLAYER && !neighbors.contains(&(close_n_id as NId))
                                    {
                                        edges_to_try_add.push((n_id as NId, close_n_id as NId));
                                    }
                                }
                            }
                        }
                    }
                },
                _ => {}
            }
        }
        // manage unit distribution
        // let cells distribute troops to their chosen edges
        if distribute_troops {
            for n_id in 0..self.nodes.len() {
                let n_id = n_id as NId;
                let nodes_unchecked = &self.nodes as *const Vec<GameNode>;  // necessary to circumvent the borrow checker; its use is completely safe and doing it differently would have been a large fuss for nothing
                let edges_unchecked = &self.edges as *const Vec<GameEdge>;
                let node = &mut self.nodes[usize::from(n_id)];
                // uncontrolled cells and disputed cells cannot distribute troops
                let can_distribute = node.controlled_by != NO_PLAYER && (node.troop_send_paths.len() != 0);//match node.cell_type {
                    //Producer | Cancer => node.controlled_by != NO_PLAYER,
                    //_ => node.controlled_by != NO_PLAYER && (node.troop_send_paths.len() != 0),
                //};
                if can_distribute {
                    if let Some(troop) = Troop::troop_of_player_mut(&mut node.troops, node.controlled_by) {
                        if let Some(available_units) = troop.count.checked_sub(node.desired_unit_count) {
                            let units_to_distribute = clamp(1, 0, available_units);
                            if units_to_distribute != 0 {
                                troop.remove_units(units_to_distribute);
                                let mut possible_e_ids: SmallVec<[EId; 16]> = /* match node.cell_type {
                                    Cancer => { physics_state.node_at(n_id).edge_indices.iter().copied().collect() },
                                    Producer => unsafe {
                                        physics_state.neighbors(n_id)
                                            .filter(|neighbor_id| match (*nodes_unchecked)[usize::from(*neighbor_id)].controlled_by() {
                                                NO_PLAYER | ANYONE_PLAYER => false,
                                                _ => true
                                            })
                                            .map(|neighbor_id| physics_state.edge_id_between(n_id, neighbor_id).unwrap())
                                            .collect()
                                    },
                                    _ => */ node.troop_send_paths.iter().copied().collect();
                                //};
                                // filter out all edges that point towards a producer, as producers may not be targeted
                                // also filter out all edges that have already reached their maximum unit count
                                unsafe {
                                    match node.cell_type {
                                        Producer => {
                                            possible_e_ids.retain(|e_id| { let ctrl = (*nodes_unchecked)[usize::from(physics_state.edge_at(*e_id).other_node(n_id))]
                                                .controlled_by();
                                                return ctrl != ANYONE_PLAYER && ctrl != NO_PLAYER   // for producers also filter out edges that lead to uncontrolled nodes
                                                && (*edges_unchecked)[usize::from(*e_id)].addable_unit_count() >= units_to_distribute;
                                            })
                                        }
                                        _ => {
                                            possible_e_ids.retain(|e_id| (*nodes_unchecked)[usize::from(physics_state.edge_at(*e_id).other_node(n_id))]
                                                .controlled_by() != ANYONE_PLAYER
                                                && (*edges_unchecked)[usize::from(*e_id)].addable_unit_count() >= units_to_distribute );
                                        }
                                    }
                                }
                                let e_count = possible_e_ids.len();
                                if e_count != 0 {
                                    // choose an edge
                                    let e_id = possible_e_ids[usize::from(node.troop_distribution_counter) % e_count];
                                    // advance the counter
                                    node.troop_distribution_counter = node.troop_distribution_counter.wrapping_add(1);
                                    // add the troop
                                    self.edges[usize::from(e_id)].add_troop(
                                        physics_state.edge_at(e_id).pos_in_edge(n_id),
                                        node.controlled_by(),
                                        units_to_distribute
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
        // try to add edges
        for (source_n_id, target_n_id) in edges_to_try_add {
            println!("trying to add cancer edge");
            MainState::try_add_edge(self, physics_state, source_n_id, target_n_id, prox_walls);
        }
        // update all edges
        for (e_id, edge) in self.edges.iter_mut().enumerate() {
            edge.advance_troops(&mut self.nodes, physics_state, &mut control_changed_nodes, e_id as EId, dt);
            edge.advance_fights(dt, physics_state.edge_length_id(e_id as EId));
        }
        // recalculate the edge control values for all edges of the control changed nodes
        // also kick players from nodes that they're not allowed to be on anymore
        // in this process players can be kicked completely out of the game, so collect those players to hand them back to the main state
        let mut players_to_be_removed = SmallVec::<[PlayerId; 4]>::new();
        for n_id in control_changed_nodes.iter() {
            players_to_be_removed.append(&mut self.update_node_and_edges_control(*n_id, physics_state));
        }
        return players_to_be_removed;
    }
}

/// holds the units currently traveling the edge and other game related edge data
pub struct GameEdge {
    /// holds tuples containing the relative advancement of the unit and the player id;
    /// [0] contains units traveling from node1 to node2 (as defined by the corresponding physical edge)
    /// [1] contains units traveling form node2 to node1
    /// the advancement is measured as the relative advancement from node1 to node2
    advancing_troops: [Vec<AdvancingTroop>; 2],
    /// If both ends of this edge are owned by one player and no foreign units are on it, then it is controlled by him.
    ///
    /// This value is cached here because fast access to it allows for optimized control-dependent operations including drawing the edge.
    /// i.e. it is needed often and changes rarely
    controlled_by: PlayerId,
    /// If there is a fight on this edge it is found in one of these two Options, measured in relative advancement from node1 to node2
    /// There can only ever at most be 2 fights at once (if fights occur whenever troops are controlled by different players, that is)
    pub fights: SmallVec<[EdgeFight; 2]>,
}

impl GameEdge {
    fn new() -> GameEdge { GameEdge {
        advancing_troops: [Vec::new(), Vec::new()],
        controlled_by: NO_PLAYER,
        fights: SmallVec::new()
    } }
    pub fn controlled_by(&self) -> PlayerId { self.controlled_by }

    pub fn unit_count(&self) -> UnitCount {
        let mut count: UnitCount = 0;
        for adv_troop in self.troop_iter() {
            count += adv_troop.troop.count;
        }
        for fight in self.fights.iter() {
            for t in fight.troop_iter() {
                count += t.count;
            }
        }
        count
    }
    fn max_unit_count(&self) -> UnitCount {
        if self.controlled_by == CANCER_PLAYER {
            20
        } else {
            99
        }
    }
    fn addable_unit_count(&self) -> UnitCount { self.max_unit_count().saturating_sub(self.unit_count()) }

    fn update_controlled_by(&mut self, game_nodes: [&GameNode; 2]) {
        let (ctrl1, ctrl2) = (game_nodes[0].controlled_by(), game_nodes[1].controlled_by());
        // if one node is controlled by the "anyone player" (i.e. a producer) then the edge can be controlled by anyone
        if ctrl1 == ANYONE_PLAYER || ctrl2 == ANYONE_PLAYER {
            self.controlled_by = ANYONE_PLAYER;
        }
        else if ctrl1 == ctrl2 {
            self.controlled_by = game_nodes[0].controlled_by;
        } else {
            self.controlled_by = NO_PLAYER;
        }
    }
}

pub fn can_control(p_id: PlayerId, other_p_id: PlayerId) -> bool {
    p_id == other_p_id || other_p_id == ANYONE_PLAYER
}

#[derive(Copy, Clone)]
pub enum CellType {
    Basic,
    Propulsion(f32, f32),    // holds the unit consumption (meaning when this value is >=1 a unit/units have to be removed)
                             // and the current angle of the node
    Wall,
    Cancer,
    Producer /*{ prod_rate: f32 }*/,
}

pub struct GameNode {
    troops: Vec<Troop>,
    desired_unit_count: UnitCount,
    /// where units should be send to regularly
    troop_send_paths: BTreeSet<EId>,
    /// If only one player has troops at this node, then it is controlled by him.
    ///
    /// This value is cached here because fast access to it allows for optimized control-dependent operations including drawing the node.
    /// i.e. it is needed often and changes rarely
    controlled_by: PlayerId,
    /// how this Node acts as a cell (i.e. as a wall, as a producer, as cancer, ...)
    cell_type: CellType,
    fight: Option<Fight>,
    troop_distribution_counter: u8,
    mutating: Option<(CellType, f32)>
}

fn mutation_cost(cell_type: &CellType) -> UnitCount {
    match cell_type {
        _ => 5
    }
}

fn mutation_duration(cell_type: &CellType) -> f32 {
    match cell_type {
        _ => 6.0
    }
}

impl GameNode {
    const INITIAL_DESIRED_UNIT_COUNT: UnitCount = 1;

    fn new() -> GameNode {
        GameNode {
            troops: Vec::with_capacity(4),
            desired_unit_count: Self::INITIAL_DESIRED_UNIT_COUNT,
            troop_send_paths: BTreeSet::new(),
            controlled_by: NO_PLAYER,
            cell_type: CellType::Basic,
            fight: None,
            troop_distribution_counter: 0,
            mutating: None,
        }
    }

    fn with_type(cell_type: CellType) -> GameNode {
        let mut g_node = GameNode::new();
        g_node.cell_type = cell_type;
        use CellType::*;
        match cell_type {
            Producer => { g_node.controlled_by = ANYONE_PLAYER; },
            Cancer   => { g_node.controlled_by = CANCER_PLAYER; },
            _ => {}
        }
        g_node
    }
    /// Returns whether this node can currently mutate into a given type.
    pub fn can_mutate(&self, cell_type: &CellType) -> bool {
        return self.controlled_by != NO_PLAYER  // there needs to be clear control to decide who pays
            && self.mutating.is_none()  // there may not be any other mutation currently running
            && std::mem::discriminant(&self.cell_type) != std::mem::discriminant(cell_type) // one may not mutate a cell into the same type it currently has
            && self.troop_of_player(self.controlled_by).unwrap().count >= mutation_cost(cell_type) + 1  // and the cost must be payable
    }

    fn start_mutation(&mut self, cell_type: CellType) {
        self.mutating = Some((cell_type, mutation_duration(&cell_type)));
        self.troop_of_player_mut(self.controlled_by).unwrap().remove_units(mutation_cost(&cell_type));
    }

    pub fn try_start_mutation(&mut self, cell_type: CellType) -> bool {
        if self.can_mutate(&cell_type) {
            self.start_mutation(cell_type);
            return true;
        }
        false
    }

    /// Returns whether a control change has happened in this node
    fn advance_mutation(&mut self, physics_state: &mut PhysicsState, dt: f32, n_id: NId, g_nodes: &[GameNode], e_to_b_rem: &mut Vec<EId>) -> bool {
        if let Some((c_type, duration_left)) = &mut self.mutating {
            *duration_left -= dt;
            if *duration_left <= 0. {
                // finish the mutation
                let old_type = self.cell_type;
                self.cell_type = *c_type;
                self.mutating = None;
                // depending on the type of mutation do some additional things:
                use CellType::*;
                match self.cell_type {
                    Cancer => {
                        // take over all units on this node and thereby trigger a control change
                        let mut stolen_units: UnitCount = 0;
                        for troop in self.troops.iter() {
                            stolen_units += troop.count;
                        }
                        self.troops.clear();
                        self.add_units(CANCER_PLAYER, stolen_units);
                        // add all edges to the send paths list
                        for e_id in physics_state.node_at(n_id as NId).edge_indices.iter() {
                            self.add_troop_path(*e_id);
                        }
                        return true;
                    }
                    Wall => {
                        // check whether edges have to be turned into walls now
                        let mut edges_to_turn_to_walls = SmallVec::<[EId; 8]>::new();
                        for e_id in physics_state.node_at(n_id as NId).edge_indices.iter() {
                            let edge = physics_state.edge_at(*e_id);
                            let other_n_id = edge.other_node(n_id);
                            if let Wall = g_nodes[usize::from(other_n_id)].cell_type() {
                                // both cells are wall cells, so the edge between them has to become a wall
                                edges_to_turn_to_walls.push(*e_id);
                            }
                        }
                        for e_id in edges_to_turn_to_walls {
                            physics_state.turn_to_wall(e_id, e_to_b_rem);
                        }
                    }
                    _ => {}
                }
                // if the old state was "Wall" then update the physical wall state of all edges as well
                if let Wall = old_type {
                    // turn all edges back to normal
                    for edge in physics_state.edges_of_node(n_id) {
                        unsafe {
                            let edge = &mut *edge;
                            edge.turn_to_normal();
                        }
                    }
                }
            }
        }
        false
    }

    /// Returns true if the player controls this node or if he has troops here
    pub fn player_can_access(&self, p_id: PlayerId) -> bool {
        self.controlled_by == p_id || self.troops.iter().find(|x| x.player == p_id).is_some()
    }

    fn set_controlled_by(&mut self, p_id: PlayerId) {
        if self.controlled_by != p_id {
            // the controlling player changed, reset the send paths and the desired unit count
            // TODO: think about whether I really want this
            match self.cell_type {
                CellType::Cancer => {
                    if p_id != CANCER_PLAYER && p_id != NO_PLAYER {
                        // cancer has just been beaten, turn this cell back into a normal cell
                        self.cell_type = Basic;
                        self.troop_send_paths.clear();
                    }
                },
                _ => { self.troop_send_paths.clear(); }
            }
            self.desired_unit_count = Self::INITIAL_DESIRED_UNIT_COUNT;
        }
        self.controlled_by = p_id;
    }

    /// tries to add 'amount' units;
    /// returns how many were actually added and whether a control-change was triggered
    fn add_units(&mut self, p_id: PlayerId, amount: UnitCount) -> (UnitCount, bool) {
        if amount == 0 { return (0, false); }
        let mut control_change = false;
        let troop = if let Some(t) = Troop::troop_of_player_mut(&mut self.troops, p_id) {
            //println!("troop of player {} found", p_id);
            t
        } else {
            // there is no troop of this player here, create a new one
            //println!("creating new troop of player {}", p_id);
            let new_troop = Troop { count: 0, player: p_id };
            match self.troops.len() {
                0 => {
                    // if this is the first troop here, claim this node for the player
                    self.set_controlled_by(p_id);
                    control_change = true;
                },
                1 => {
                    // if this is the second troop here, start a fight!
                    self.start_fight();
                    control_change = true;
                }
                _ => {}
            }
            self.troops.push(new_troop);
            self.troops.last_mut().unwrap()
        };

        let ret = (troop.add_units(amount), control_change);
        //println!("troop after addition: {:?}", troop);
        ret
    }
    pub fn set_desired_unit_count(&mut self, unit_count: UnitCount) {
        self.desired_unit_count = clamp(unit_count, 1, MAX_UNIT_COUNT);
    }
    pub fn desired_unit_count(&self) -> UnitCount {
        self.desired_unit_count
    }
    fn add_troop_path(&mut self, path: EId) {
        self.troop_send_paths.insert(path);
    }
    pub fn remove_troop_path(&mut self, path: &EId) {
        self.troop_send_paths.remove(path);
    }
    pub fn clear_troop_paths(&mut self) {
        self.troop_send_paths.clear();
    }
    pub fn controlled_by(&self) -> PlayerId { self.controlled_by }
    pub fn cell_type(&self) -> &CellType { &self.cell_type }
    pub fn cell_type_mut(&mut self) -> &mut CellType { &mut self.cell_type }
}