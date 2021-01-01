use crate::{NId, EId, PlayerId, NO_PLAYER, UnitCount, MAX_UNIT_COUNT};
use std::collections::BTreeSet;
use ggez::nalgebra::clamp;
use smallvec::{SmallVec};
use crate::physics::PhysicsState;
use crate::helpers::*;
use core::slice::IterMut;
use std::slice::Iter;
use rand::{thread_rng, Rng};
use rand::seq::SliceRandom;

pub struct GameState {
    /// on which node each player currently is
    pub player_node_ids: Vec<NId>,
    /// which edge is currently selected by each player
    pub player_edge_ids: Vec<Option<EId>>,
    pub nodes: Vec<GameNode>,
    pub edges: Vec<GameEdge>,
    troop_distribution_timer: Timer,
}

impl GameState {
    pub fn new() -> GameState {
        GameState {
            player_node_ids: Vec::new(),
            player_edge_ids: Vec::new(),
            nodes: Vec::new(),
            edges: Vec::new(),
            troop_distribution_timer: Timer::new(),
        }
    }
    pub fn player_node(&self, p_id: PlayerId) -> &GameNode {
        &self.nodes[usize::from(self.player_node_ids[usize::from(p_id)])]
    }
    pub fn player_node_mut(&mut self, p_id: PlayerId) -> &mut GameNode {
        &mut self.nodes[usize::from(self.player_node_ids[usize::from(p_id)])]
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
        // do housekeeping
        let swapped_id = self.edges.len() as EId;
        if swapped_id != edge_index {
            for node in self.nodes.iter_mut() {
                if node.troop_send_paths.remove(&swapped_id) {
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
        // TODO: remove all his units (and thereby turn all his nodes and edges into uncontrolled territory)

    }

    fn check_for_troop_distribution(&mut self, dt: f32) -> bool {
        const DURATION_BETWEEN_DISTRIBUTIONS: f32 = 1.0;
        self.troop_distribution_timer.check(dt, DURATION_BETWEEN_DISTRIBUTIONS)
    }

    pub fn add_units(&mut self, physics_state: &PhysicsState, n_id: NId, p_id: PlayerId, unit_count: UnitCount) -> UnitCount {
        let (units_added, control_changed) = self.nodes[usize::from(n_id)].add_units(p_id, unit_count);
        if control_changed { self.update_edges_control(n_id, physics_state); }
        units_added
    }

    fn update_edges_control(&mut self, n_id: NId, physics_state: &PhysicsState) {
        for e_id in physics_state.node_at(n_id).edge_indices.iter() {
            // get the other node of this edge
            let other_n_id = physics_state.edge_at(*e_id).other_node(n_id);
            let game_nodes = [&self.nodes[usize::from(n_id)], &self.nodes[usize::from(other_n_id)]];
            self.edges[usize::from(*e_id)].update_controlled_by(game_nodes);
        }
    }

    pub fn update(&mut self, physics_state: &PhysicsState, dt: f32) {
        // update all nodes
        let distribute_troops = self.check_for_troop_distribution(dt);
        // collect the nodes that switch their control along the way
        let mut control_changed_nodes = SmallVec::<[NId; 32]>::new();
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

            match node.cell_type() {
                Basic => {

                }
                _ => {}
            }
            // let cells distribute troops to their chosen edges
            // uncontrolled cells and disputed cells cannot distribute troops
            if distribute_troops && node.controlled_by != NO_PLAYER {
                if let Some(available_units) = Troop::troop_of_player(&node.troops,node.controlled_by).unwrap().count.checked_sub(node.desired_unit_count) {
                    let mut units_to_distribute = clamp(node.troop_send_paths.len() as UnitCount,
                                                  0,
                                                  available_units);
                    Troop::remove_units_of_player(&mut node.troops, node.controlled_by, units_to_distribute);
                    for e_id in node.troop_send_paths.iter() {
                        if units_to_distribute == 0 {
                            break;
                        }
                        self.edges[usize::from(*e_id)].add_troop(
                            physics_state.edge_at(*e_id).pos_in_edge(n_id as NId),
                            node.controlled_by(),
                            1
                        )
                    }
                }
            }
        }
        // update all edges
        for (e_id, edge) in self.edges.iter_mut().enumerate() {
            edge.advance_troops(&mut self.nodes, physics_state, &mut control_changed_nodes, e_id as EId, dt);
            edge.advance_fights(dt, physics_state.edge_length_id(e_id as EId));
        }
        // recalculate the edge control values for all edges of the control changed nodes
        for n_id in control_changed_nodes.iter() {
            self.update_edges_control(*n_id, physics_state);
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
    /// If there is a fight on this edge it is found in one of these two Options, measured in relative advancement from node1 to node2
    /// There can only ever at most be 2 fights at once (if fights occur whenever troops are controlled by different players, that is)
    pub fights: SmallVec<[EdgeFight; 2]>,
}

struct Fight {
    battle_timer: Timer,
}

impl<'a> Fight {
    fn new() -> Fight {
        Fight {
            battle_timer: Timer::new(),
        }
    }

    fn advance<I: Iterator<Item = &'a mut Troop>>(&mut self, troop_iter_mut: I, dt: f32) -> bool {
        const DURATION_BETWEEN_BATTLES: f32 = 2.0;
        if self.battle_timer.check(dt, DURATION_BETWEEN_BATTLES) {
            // let the troops battle each other
            let mut troops: SmallVec<[&mut Troop; 8]> = troop_iter_mut.collect();
            // get a (randomly shuffled) turn order
            troops.shuffle(&mut thread_rng());
            // let one after the other attack
            let mut i = 0;
            let last_defender_index = troops.len() - 1;
            println!("BATTLING");
            while i < last_defender_index {
                // calculate the attack (for now let it be 1)
                let my_attack = 1;
                let other_attack = 1;
                // flip a coin to see if you win the encounter
                if thread_rng().gen_bool(0.5) {
                    // you win, damage the other
                    troops[i+1].count = troops[i+1].count.saturating_sub(my_attack);
                    println!("count: {}", troops[i+1].count);
                } else {
                    // the other one wins, let him damage you
                    troops[i].count = troops[i].count.saturating_sub(other_attack);
                    println!("count: {}", troops[i].count);
                }
                // the next one has already fought, so skip him
                i += 2;
            }
            return true;
        }
        false
    }

    fn winner<I: Iterator<Item = &'a Troop>>(mut troop_iter: I) -> Option<&'a Troop> {
        // if only one is standing return this troop
        let first_troop_alive = troop_iter.find(|troop| troop.count != 0).unwrap();
        if let Some(_other_troop) = troop_iter.find(|troop| troop.count != 0) {
            return None;
        } else {
            return Some(first_troop_alive);
        }
    }
}

pub struct EdgeFight {
    troops: [SmallVec<[Troop; 2]>; 2],
    fight: Fight,
    acceleration: f32,
    velocity: f32,
    pub advancement: f32,
}

impl EdgeFight {
    fn new(troops: [&Troop; 2], pos: f32) -> EdgeFight {
        EdgeFight {
            troops: [SmallVec::from_elem(*troops[0], 1), SmallVec::from_elem(*troops[1], 1)],
            fight: Fight::new(),
            acceleration: 0.0,
            velocity: 0.0,
            advancement: pos,
        }
    }

    fn troop_iter(&self) -> EdgeFightTroopIterator {
        EdgeFightTroopIterator::new(self.troops.iter())
    }

    fn troop_iter_mut(&mut self) -> EdgeFightTroopIteratorMut {
        EdgeFightTroopIteratorMut::new(self.troops.iter_mut())
    }

    fn remove_empty_troops(&mut self) {
        for vec in self.troops.iter_mut() {
            vec.retain(|troop| troop.count != 0);
        }
    }

    fn advance_fight(&mut self, dt: f32, edge_length: f32) -> bool {
        // apply acceleration and velocity
        self.velocity += self.acceleration * dt;
        self.advancement += self.velocity * dt / edge_length.sqrt();
        // apply friction
        self.acceleration *= 0.6;
        self.velocity *= 0.985;
        // clone your troops to check for possible losses afterwards
        let troop_copy = self.troops.clone();
        // advance the fight if it is time for that
        if self.fight.advance(EdgeFightTroopIteratorMut::new(self.troops.iter_mut()), dt) {
            // for each side check how many units were lost
            let mut lost_units: [i32; 2] = [0, 0];
            for i in 0..2 {
                let mut copy_iter = troop_copy[i].iter();
                for t in self.troops[i].iter() {
                    let copied_troop = copy_iter.next().unwrap();
                    lost_units[i] += i32::from(copied_troop.count.saturating_sub(t.count));
                }
            }
            // calculate the added acceleration based on the lost units
            const ACC_FACTOR: f32 = 100.0;
            self.acceleration += (lost_units[1] - lost_units[0]) as f32 * ACC_FACTOR;
            // remove all troops from the fight that have lost all units
            self.remove_empty_troops();
            return true;
        }
        false
    }

    /// Returns the last troop standing if there is only one left
    // call this function to determine whether this EdgeFight can be disbanded,
    // sending the remaining troop back on track
    fn winner(&self) -> Option<(usize, Troop)> {
        let troop_count_per_direction = [self.troops[0].len(), self.troops[1].len()];
        if troop_count_per_direction[0] == 1 && troop_count_per_direction[1] == 0 {
            // direction 0 has won
            return Some((0, self.troops[0][0]));
        } else if troop_count_per_direction[0] == 0 && troop_count_per_direction[1] == 1 {
            // direction 1 has won
            return Some((1, self.troops[1][0]));
        }
        None
    }

    fn add_troop(&mut self, i: usize, new_troop: &mut Troop) -> UnitCount {
        let mut added_units: UnitCount = 0;
        // search for allied troops
        for troop_vec in self.troops.iter_mut() {
            for troop in troop_vec.iter_mut() {
                if troop.player == new_troop.player {
                    added_units = troop.add_units(new_troop.count);
                    new_troop.remove_units(added_units);
                    return added_units
                }
            }
        }
        // no allied troops found to merge with, so add this troop
        self.troops[i].push(*new_troop);
        added_units = new_troop.count;
        new_troop.count = 0;
        added_units
    }
}

struct EdgeFightTroopIterator<'a> {
    vec_iter: Iter<'a, SmallVec<[Troop; 2]>>,
    troop_iter: Iter<'a, Troop>
}

impl EdgeFightTroopIterator<'_> {
    fn new(mut vec_iter: Iter<SmallVec<[Troop; 2]>>) -> EdgeFightTroopIterator {
        let t_iter = vec_iter.next().unwrap().iter();
        EdgeFightTroopIterator {
            vec_iter,
            troop_iter: t_iter,
        }
    }
}

impl<'a> Iterator for EdgeFightTroopIterator<'a> {
    type Item = &'a Troop;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(troop) = self.troop_iter.next() {
            Some(troop)
        } else if let Some(s_vec) = self.vec_iter.next() {
            // go to the next direction
            self.troop_iter = s_vec.iter();
            self.next()
        } else {
            None
        }
    }
}

struct EdgeFightTroopIteratorMut<'a> {
    vec_iter: IterMut<'a, SmallVec<[Troop; 2]>>,
    troop_iter: IterMut<'a, Troop>
}

impl EdgeFightTroopIteratorMut<'_> {
    fn new(mut vec_iter: IterMut<SmallVec<[Troop; 2]>>) -> EdgeFightTroopIteratorMut {
        let t_iter = vec_iter.next().unwrap().iter_mut();
        EdgeFightTroopIteratorMut {
            vec_iter,
            troop_iter: t_iter,
        }
    }
}

impl<'a> Iterator for EdgeFightTroopIteratorMut<'a> {
    type Item = &'a mut Troop;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(troop) = self.troop_iter.next() {
            Some(troop)
        } else if let Some(s_vec) = self.vec_iter.next() {
            // go to the next direction
            self.troop_iter = s_vec.iter_mut();
            self.next()
        } else {
            None
        }
    }
}

pub struct EdgeTroopIter<'a> {
    vec_iter: Iter<'a, Vec<AdvancingTroop>>,
    troop_iter: Iter<'a, AdvancingTroop>,
}

impl<'a> Iterator for EdgeTroopIter<'a> {
    type Item = &'a AdvancingTroop;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(troop) = self.troop_iter.next() {
            Some(troop)
        } else if let Some(s_vec) = self.vec_iter.next() {
            // go to the next direction
            self.troop_iter = s_vec.iter();
            self.next()
        } else {
            None
        }
    }
}

pub struct AdvancingTroop {
    pub troop: Troop,
    pub advancement: f32,
}

impl AdvancingTroop {
    fn new(troop: Troop, start_index: usize) -> AdvancingTroop {
        AdvancingTroop {
            troop,
            advancement: if start_index == 0 { 0.0 } else { 1.0 }
        }
    }
}

impl GameEdge {
    fn new() -> GameEdge { GameEdge {
        advancing_troops: [Vec::new(), Vec::new()],
        controlled_by: NO_PLAYER,
        fights: SmallVec::new()
    } }
    pub fn controlled_by(&self) -> PlayerId { self.controlled_by }

    fn add_troop(&mut self, start_index: usize, p_id: PlayerId, amount: UnitCount) {
        if amount == 0 { return; }
        self.advancing_troops[start_index].push(AdvancingTroop::new(Troop { count: amount, player: p_id }, start_index))
    }

    fn add_troop_advanced(&mut self, start_index: usize, troop: Troop, advancement: f32) {
        // add them so that the vector remains sorted
        let mut not_inserted = true;
        if start_index == 0 {
            for i in (0..self.advancing_troops[start_index].len()).rev() {
                if self.advancing_troops[start_index][i].advancement > advancement {
                    self.advancing_troops[start_index].insert(i, AdvancingTroop { troop, advancement });
                    not_inserted = false;
                    break;
                }
            }
        } else {
            for i in (0..self.advancing_troops[start_index].len()).rev() {
                if self.advancing_troops[start_index][i].advancement < advancement {
                    self.advancing_troops[start_index].insert(i, AdvancingTroop { troop, advancement });
                    not_inserted = false;
                    break;
                }
            }
        }
        if not_inserted {
            self.advancing_troops[start_index].push(AdvancingTroop { troop, advancement });
        }
    }

    pub fn troop_iter(&self) -> EdgeTroopIter {
        let mut vec_iter = self.advancing_troops.iter();
        let troop_iter = vec_iter.next().unwrap().iter();
        EdgeTroopIter { vec_iter, troop_iter }
    }

    fn advance_fights(&mut self, dt: f32, edge_length: f32) {
        let mut fight_happened = false;
        for fight in self.fights.iter_mut() {
            let old_adv = fight.advancement;
            if fight.advance_fight(dt, edge_length) { fight_happened = true; }
            let new_adv = fight.advancement;
            let min_adv = if old_adv < new_adv { old_adv } else { new_adv };
            let max_adv = if old_adv > new_adv { old_adv } else { new_adv };
            // check if the fight just moved over some advancing troops
            // and add them to the fight if true
            for (i, vec_adv_t) in self.advancing_troops.iter_mut().enumerate() {
                for adv_t in vec_adv_t.iter_mut() {
                    if min_adv <= adv_t.advancement && adv_t.advancement <= max_adv {
                        fight.add_troop(i, &mut adv_t.troop);
                    }
                }
            }
        }
        // remove possible empty advancing troops
        self.advancing_troops[0].retain(|t| t.troop.count != 0);
        self.advancing_troops[1].retain(|t| t.troop.count != 0);
        // check if the fight has moved completely towards one node and
        // check for winners
        // remove all fights for either of this is true
        // collect their troops and add them back onto the edge
        let mut units_to_add = SmallVec::<[(usize, Troop, f32); 8]>::new();
        self.fights.retain(|fight| -> bool {
            if fight.advancement <= 0.0 {
                for troop in fight.troop_iter() {
                    units_to_add.push((1, *troop, fight.advancement));
                }
                return false;
            }
            else if fight.advancement >= 1.0 {
                for troop in fight.troop_iter() {
                    units_to_add.push((0, *troop, fight.advancement));
                }
                return false;
            }
            else if fight_happened {
                if let Some((st_index, troop)) = fight.winner() {
                    units_to_add.push((st_index, troop, fight.advancement));
                    return false;
                }
                return true;
            }
            else { true }
        });
        for w in units_to_add {
            // add them to the edge
            println!("adding troop");
            self.add_troop_advanced(w.0, w.1, w.2);
        }

    }

    fn update_controlled_by(&mut self, game_nodes: [&GameNode; 2]) {
        if game_nodes[0].controlled_by == game_nodes[1].controlled_by {
            self.controlled_by = game_nodes[0].controlled_by;
        } else {
            self.controlled_by = NO_PLAYER;
        }
    }

    fn check_for_fights(&mut self, advance: f32) {
        let mut checking = true;
        while checking {
            // first check whether troops have entered known fights
            // for direction 0
            let mut checked_players = SmallVec::<[PlayerId; 8]>::new();
            for advancing_troop in self.advancing_troops[0].iter_mut() {  // first elements are the oldest/most advanced
                let my_player = advancing_troop.troop.player;
                if checked_players.contains(&my_player) { continue; }
                let mut no_fight_found = true;
                // go through all known fights
                for edge_fight in self.fights.iter_mut() {
                    if edge_fight.advancement <= advancing_troop.advancement && edge_fight.advancement >= advancing_troop.advancement - advance {
                        // you've just passed this fight, join it
                        edge_fight.add_troop(0, &mut advancing_troop.troop);
                        no_fight_found = false;
                    }
                }
                if no_fight_found {
                    checked_players.push(my_player);
                }
            }
            // remove possible empty troops
            self.advancing_troops[0].retain(|t| t.troop.count != 0);
            // for direction 1
            let mut checked_players = SmallVec::<[PlayerId; 8]>::new();
            for advancing_troop in self.advancing_troops[1].iter_mut() {  // first elements are the oldest/most advanced
                let my_player = advancing_troop.troop.player;
                if checked_players.contains(&my_player) { continue; }
                let mut no_fight_found = true;
                // go through all known fights
                for edge_fight in self.fights.iter_mut() {
                    if edge_fight.advancement >= advancing_troop.advancement && edge_fight.advancement <= advancing_troop.advancement + advance {
                        // you've just passed this fight, join it
                        edge_fight.add_troop(1, &mut advancing_troop.troop);
                        no_fight_found = false;
                    }
                }
                if no_fight_found {
                    checked_players.push(my_player);
                }
            }
            // remove possible empty troops
            self.advancing_troops[1].retain(|t| t.troop.count != 0);
            // check whether new fights should be created
            // troops of different players shouldn't be able to pass each other,
            // therefore check for one direction for each troop whether a troop of another player moving in the opposite direction has passed it
            // if so, stop it and start a fight
            let mut fighting_troops: Option<(usize, usize)> = None; // (adv_troop_id1 on advancing_troops[0], adv_troop_id2 on advancing_troops[1])
            let mut checked_players = SmallVec::<[PlayerId; 8]>::new();
            for (my_i, advancing_troop) in self.advancing_troops[0].iter().enumerate() {  // first elements are the oldest/most advanced
                let my_player = advancing_troop.troop.player;
                if checked_players.contains(&my_player) { continue; }
                for (other_i, other_troop) in self.advancing_troops[1].iter().enumerate() {
                    if my_player != other_troop.troop.player {
                        if advancing_troop.advancement >= other_troop.advancement {
                            // this enemy troop has passed you, start a fight!
                            fighting_troops = Some((my_i, other_i));
                            break;
                        }
                    }
                }
                checked_players.push(my_player);
                if fighting_troops.is_some() { break; }
            }
            if let Some(fighting_troops) = fighting_troops {
                // start a fight; for this calculate the midpoint between the troops and place them there
                let midpoint: f32 = (self.advancing_troops[0][fighting_troops.0].advancement + self.advancing_troops[1][fighting_troops.1].advancement) / 2.0;
                self.fights.push(EdgeFight::new([&self.advancing_troops[0][fighting_troops.0].troop, &self.advancing_troops[1][fighting_troops.1].troop], midpoint));
                // remove the troops from the edge
                self.advancing_troops[0].remove(fighting_troops.0);
                self.advancing_troops[1].remove(fighting_troops.1);
            } else {
                // you got through without finding any need for starting fights, so you're done
                checking = false;
            }
        }
    }
    /// Let the units on this edge advance.
    /// If troops reach an end of this edge add them to the node found there.
    /// If troops walk into troops of other players start a fight.
    fn advance_troops(&mut self, game_nodes: &mut [GameNode], physics_state: &PhysicsState, control_changed_nodes: &mut SmallVec<[NId; 32]>, my_e_id: EId, dt: f32) {
        const ADVANCE_SPEED: f32 = 300.;
        let edge = physics_state.edge_at(my_e_id);
        let length = physics_state.edge_length(edge);
        let advance = ADVANCE_SPEED * dt / length;

        // do one direction and then the next
        // first direction
        for advancing_troop in self.advancing_troops[0].iter_mut() {
            advancing_troop.advancement += advance;
        }
        // remove all advancing troops that have reached the end of the edge
        let removed_troops = self.advancing_troops[0].drain_filter(|adv_t| adv_t.advancement >= 1.).collect::<SmallVec<[AdvancingTroop; 4]>>();
        // add them to the node found there
        let dest_n_id = edge.node_indices[1];
        let game_node = &mut game_nodes[usize::from(dest_n_id)];
        for adv_troop in removed_troops.into_iter() {
            let mut troop = adv_troop.troop;
            let (added_units, control_change) = game_node.add_units(troop.player, troop.count);
            if added_units != troop.count {
                // send the rest of this troop back home
                troop.remove_units(added_units);
                self.add_troop(1, troop.player, troop.count);
            }
            // if the player controlling this node changed add it to the nodes which edges have to recalc their control-value
            if control_change {
                control_changed_nodes.push(dest_n_id);
            }
        }

        // second direction
        for advancing_troop in self.advancing_troops[1].iter_mut() {
            advancing_troop.advancement -= advance;
        }
        // remove all advancing troops that have reached the end of the edge
        let removed_troops = self.advancing_troops[1].drain_filter(|adv_t| adv_t.advancement <= 0.).collect::<SmallVec<[AdvancingTroop; 4]>>();
        // add them to the node found there
        let dest_n_id = edge.node_indices[0];
        let game_node = &mut game_nodes[usize::from(dest_n_id)];
        for adv_troop in removed_troops.into_iter() {
            let mut troop = adv_troop.troop;
            let (added_units, control_change) = game_node.add_units(troop.player, troop.count);
            if added_units != troop.count {
                // send the rest of this troop back home
                troop.remove_units(added_units);
                self.add_troop(0, troop.player, troop.count);
            }
            // if the player controlling this node changed add it to the nodes which edges have to recalc their control-value
            if control_change {
                control_changed_nodes.push(dest_n_id);
            }
        }
        self.check_for_fights(advance);
    }
}

/// holds the unit-count and the player owning these units
#[derive(Copy, Clone)]
pub struct Troop {
    pub count: UnitCount,
    pub player: PlayerId
}

impl Troop {
    fn troop_of_player(troops: &[Troop], p_id: PlayerId) -> Option<&Troop> {
        troops.iter().find(|troop| troop.player == p_id)
    }
    fn troop_of_player_mut(troops: &mut [Troop], p_id: PlayerId) -> Option<&mut Troop> {
        troops.iter_mut().find(|troop| troop.player == p_id)
    }
    fn remove_units_of_player(troops: &mut Vec<Troop>, p_id: PlayerId, amount: UnitCount) -> UnitCount {
        let mut amount_removed: UnitCount = 0;
        if let Some(troop) = Self::troop_of_player_mut(troops, p_id) {
            amount_removed = troop.remove_units(amount);
        }
        // if all units of a troop have been removed remove it
        troops.retain(|t| t.count != 0);
        amount_removed
    }
    /// Returns how many were actually added
    fn add_units(&mut self, amount: UnitCount) -> UnitCount {
        let mut amount_added = 0;
        if let Some(new_count) = self.count.checked_add(amount) {
            (self.count, amount_added) = if new_count <= UnitCount::MAX {
                (new_count, amount)
            } else {
                (UnitCount::MAX, UnitCount::MAX - self.count)
            };
        } else {
            (self.count, amount_added) = (UnitCount::MAX, UnitCount::MAX - self.count);
        }
        amount_added
    }
    /// Returns how many were actually removed
    fn remove_units(&mut self, amount: UnitCount) -> UnitCount {
        let mut amount_removed: UnitCount = 0;
        (self.count, amount_removed) = match self.count.checked_sub(amount) {
            Some(new_count) => (new_count, amount),
            None => (0, self.count)
        };
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
    fight: Option<Fight>
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
        }
    }
    /// Returns true if the player controls this node or if he has troops here
    pub fn player_can_access(&self, p_id: PlayerId) -> bool {
        self.controlled_by == p_id || self.troops.iter().find(|x| x.player == p_id).is_some()
    }

    fn start_fight(&mut self) {
        self.fight = Some(Fight::new());
        self.set_controlled_by(NO_PLAYER);
    }

    fn fighting(&self) -> bool { self.fight.is_some() }

    /// Returns whether the fight has ended (signaling the need for an update of the neighboring edges)
    fn advance_fight(&mut self, dt: f32) -> bool {
        if let Some(active_fight) = &mut self.fight {
            active_fight.advance(self.troops.iter_mut(), dt);
            // check for a winner and end the fight
            if let Some(winning_troop) = Fight::winner(self.troops.iter()) {
                // hand control over to the new player
                self.set_controlled_by(winning_troop.player);
                // stop the fight
                self.fight = None;
                return true;
            }
        }
        false
    }

    fn set_controlled_by(&mut self, p_id: PlayerId) {
        if self.controlled_by != p_id {
            // the controlling player changed, reset the send paths and the desired unit count
            self.troop_send_paths.clear();
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
            t
        } else {
            // there is no troop of this player here, create a new one
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

        (troop.add_units(amount), control_change)
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
        println!("removed e_id: {}", *path);
        self.troop_send_paths.remove(path);
    }
    pub fn clear_troop_paths(&mut self) {
        self.troop_send_paths.clear();
    }
    pub fn controlled_by(&self) -> PlayerId { self.controlled_by }
    pub fn cell_type(&self) -> &CellType { &self.cell_type }
}