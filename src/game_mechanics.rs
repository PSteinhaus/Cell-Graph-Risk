use crate::{NId, EId, PlayerId, NO_PLAYER, UnitCount, MAX_UNIT_COUNT};
use std::collections::BTreeSet;
use ggez::nalgebra::clamp;
use crate::physics::PhysicsState;

pub struct GameState {
    /// on which node each player currently is
    pub player_node_ids: Vec<NId>,
    /// which edge is currently selected by each player
    pub player_edge_ids: Vec<Option<EId>>,
    pub nodes: Vec<GameNode>,
    pub edges: Vec<GameEdge>,
    time_since_last_troop_distribution: f32,
}

impl GameState {
    pub fn new() -> GameState {
        GameState {
            player_node_ids: Vec::new(),
            player_edge_ids: Vec::new(),
            nodes: Vec::new(),
            edges: Vec::new(),
            time_since_last_troop_distribution: 0.0,
        }
    }
    pub fn add_node(&mut self) {
        self.nodes.push(GameNode::new());
    }
    pub fn add_edge(&mut self) { self.edges.push(GameEdge::new()); }
    pub fn remove_node(&mut self, node_index: NId) {
        // remove the game node from the collection
        self.nodes.swap_remove(usize::from(node_index));
    }

    pub fn remove_edge(&mut self, edge_index: EId, n_ids: [NId; 2]) {
        // make sure no player keeps this edge as his selected edge
        for edge in self.player_edge_ids.iter_mut() {
            if let Some(selected_e_id) = edge {
                if *selected_e_id == edge_index {
                    *edge = None;
                }
            }
        }
        // remove this edge from all lists of paths to send troops through kept by the nodes
        for n_id in n_ids.iter() {
            self.nodes[usize::from(*n_id)].remove_troop_path(&edge_index);
        }
        self.edges.swap_remove(usize::from(edge_index));
    }

    pub fn remove_player(&mut self, p_id: PlayerId) {
        // remove him from the list of player nodes and edges
        self.player_node_ids.swap_remove(usize::from(p_id));
        self.player_edge_ids.swap_remove(usize::from(p_id));
        // now do housekeeping
        // TODO: remove all his units (and thereby turn all his nodes and edges into uncontrolled territory)

    }

    fn check_for_troop_distribution(&mut self, dt: f32) -> bool {
        const DURATION_BETWEEN_DISTRIBUTIONS: f32 = 1.0;
        self.time_since_last_troop_distribution += dt;
        if self.time_since_last_troop_distribution >= DURATION_BETWEEN_DISTRIBUTIONS {
            self.time_since_last_troop_distribution -= DURATION_BETWEEN_DISTRIBUTIONS;
            true
        } else {
            false
        }
    }

    pub fn update(&mut self, physics_state: &PhysicsState, dt: f32) {
        // update all nodes
        let distribute_troops = self.check_for_troop_distribution(dt);
        use CellType::*;
        for (n_id, node) in self.nodes.iter_mut().enumerate() {
            match node.cell_type() {
                Basic => {

                }
                _ => {}
            }
            // all cells distribute troops to their chosen edges
            if distribute_troops {
                for e_id in node.troop_send_paths.iter() {
                    let unit_amount = Troop::remove_units(&mut node.troops, node.controlled_by, 1);
                    if unit_amount == 0 {
                        break;
                    }
                    self.edges[usize::from(*e_id)].add_troop(
                        physics_state.edge_at(*e_id).pos_in_edge(n_id as NId),
                        node.controlled_by(),
                        unit_amount
                    )
                }
            }
        }
        // update all edges
        for (e_id, edge) in self.edges.iter_mut().enumerate() {
            edge.advance_troops(&mut self.nodes, physics_state, e_id as EId, dt);
        }
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
    /// If there is a fight on this edge it is found here, measured in relative advancement from node1 to node2
    fight_pos: Option<f32>,
}

struct AdvancingTroop {
    troop: Troop,
    advancement: f32,
}

impl AdvancingTroop {
    fn new(troop: Troop) -> AdvancingTroop {
        AdvancingTroop {
            troop,
            advancement: 0.0
        }
    }
}

impl GameEdge {
    fn new() -> GameEdge { GameEdge {
        advancing_troops: [Vec::new(), Vec::new()],
        controlled_by: NO_PLAYER,
        fight_pos: None
    } }
    pub fn controlled_by(&self) -> PlayerId { self.controlled_by }

    fn add_troop(&mut self, start_index: usize, p_id: PlayerId, amount: UnitCount) {
        if amount == 0 { return; }
        self.advancing_troops[start_index].push(AdvancingTroop::new(Troop { count: amount, player: p_id }))
    }
    /// Let the units on this edge advance.
    /// If troops reach an end of this edge add them to the node found there.
    /// If troops walk into troops of other players start a fight.
    fn advance_troops(&mut self, game_nodes: &mut [GameNode], physics_state: &PhysicsState, my_e_id: EId, dt: f32) {
        const ADVANCE_SPEED: f32 = 80.;
        let edge = physics_state.edge_at(my_e_id);
        let length = physics_state.edge_length(edge);
        let advance = ADVANCE_SPEED * dt / length;

        for advancing_troop in self.advancing_troops[0].iter_mut() {
            advancing_troop.advancement += advance;
        }
        // remove all advancing troops that have reached the end of the edge
        let removed_troops = self.advancing_troops[0].drain_filter(|adv_t| adv_t.advancement >= 1.).collect::<Vec<_>>();
        // add them to the node found there
        let dest_n_id = edge.node_indices[1];
        let game_node = &mut game_nodes[usize::from(dest_n_id)];
        for adv_troop in removed_troops.into_iter() {
            let troop = adv_troop.troop;
            game_node.add_units(troop.player, troop.count);
        }

        // TODO: check whether a new fight should be created or whether there already is a fight and some troops should join

    }
}

/// holds the unit-count and the player owning these units
struct Troop {
    count: UnitCount,
    player: PlayerId
}

impl Troop {
    fn troop_of_player(troops: &mut [Troop], p_id: PlayerId) -> Option<&Troop> {
        troops.iter().find(|troop| troop.player == p_id)
    }
    fn troop_of_player_mut(troops: &mut [Troop], p_id: PlayerId) -> Option<&mut Troop> {
        troops.iter_mut().find(|troop| troop.player == p_id)
    }
    fn remove_units(troops: &mut Vec<Troop>, p_id: PlayerId, amount: UnitCount) -> UnitCount {
        let mut amount_removed: UnitCount = 0;
        if let Some(troop) = Self::troop_of_player_mut(troops, p_id) {
            (troop.count, amount_removed) = match troop.count.checked_sub(amount) {
                Some(new_count) => (new_count, amount),
                None => (0, troop.count)
            };
        }
        // if all units of a troop have been removed remove it
        troops.retain(|t| t.count != 0);
        amount_removed
    }
}

pub enum CellType {
    Basic,
    Propulsion,
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
}

impl GameNode {
    fn new() -> GameNode {
        GameNode {
            troops: Vec::with_capacity(4),
            desired_unit_count: 1,
            troop_send_paths: BTreeSet::new(),
            controlled_by: NO_PLAYER,
            cell_type: CellType::Basic
        }
    }
    /// Returns true if the player controls this node or if he has troops here
    pub fn player_can_access(&self, p_id: PlayerId) -> bool {
        self.controlled_by == p_id || self.troops.iter().find(|x| x.player == p_id).is_some()
    }

    fn add_units(&mut self, p_id: PlayerId, amount: UnitCount) -> UnitCount {
        if amount == 0 { return 0; }
        let mut amount_added: UnitCount = 0;
        let mut troop = if let Some(t) = Troop::troop_of_player_mut(&mut self.troops, p_id) {
            t
        } else {
            let new_troop = Troop { count: 0, player: p_id };
            self.troops.push(new_troop);
            let i = self.troops.len()-1;
            &mut self.troops[i]
        };
        if let Some(new_count) = troop.count.checked_add(amount) {
            (troop.count, amount_added) = if new_count <= UnitCount::MAX {
                (new_count, amount)
            } else {
                (UnitCount::MAX, UnitCount::MAX - troop.count)
            };
        } else {
            (troop.count, amount_added) = (UnitCount::MAX, UnitCount::MAX - troop.count);
        }
        amount_added
    }
    pub fn set_desired_unit_count(&mut self, unit_count: UnitCount) {
        self.desired_unit_count = clamp(unit_count, 1, MAX_UNIT_COUNT);
    }
    pub fn desired_unit_count(&self) -> UnitCount {
        self.desired_unit_count
    }
    pub fn add_troop_path(&mut self, path: EId) {
        self.troop_send_paths.insert(path);
    }
    pub fn remove_troop_path(&mut self, path: &EId) {
        self.troop_send_paths.remove(&path);
    }
    pub fn controlled_by(&self) -> PlayerId { self.controlled_by }
    pub fn cell_type(&self) -> &CellType { &self.cell_type }
}