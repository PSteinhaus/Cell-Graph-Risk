use std::cmp::Ordering;

use rand::seq::{IteratorRandom, SliceRandom};
use rand::thread_rng;
use smallvec::alloc::slice::{Iter, IterMut};
use smallvec::{SmallVec, smallvec};

use crate::{ANYONE_PLAYER, CANCER_PLAYER, PlayerId, UnitCount, NId, EId, NO_PLAYER};
use crate::helpers::Timer;
use crate::game_mechanics::{GameEdge, GameNode};
use crate::physics::PhysicsState;

pub struct Fight {
    battle_timer: Timer,
}

impl<'a> Fight {
    pub(crate) fn new() -> Fight {
        Fight {
            battle_timer: Timer::new(),
        }
    }

    pub(crate) fn advance<I: Iterator<Item = &'a mut Troop>>(&mut self, troop_iter_mut: I, dt: f32) -> bool {
        const DURATION_BETWEEN_BATTLES: f32 = 0.75;
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
                let my_attack = (troops[i].count as f32).powf(0.25f32) as UnitCount;
                // you attack, damage the other
                troops[i+1].count = troops[i+1].count.saturating_sub(my_attack);
                println!("count: {}", troops[i+1].count);
                // the next one has already fought, so skip him
                i += 2;
            }
            return true;
        }
        false
    }

    pub(crate) fn winner<I: Iterator<Item = &'a Troop>>(mut troop_iter: I) -> Option<&'a Troop> {
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
    win_timer: f32,
    pub advancement: f32,
}

impl EdgeFight {
    pub(crate) fn new(troops: [&Troop; 2], pos: f32) -> EdgeFight {
        EdgeFight {
            troops: [SmallVec::from_elem(*troops[0], 1), SmallVec::from_elem(*troops[1], 1)],
            fight: Fight::new(),
            acceleration: 0.0,
            velocity: 0.0,
            win_timer: 0.0,
            advancement: pos,
        }
    }

    pub fn troop_iter(&self) -> EdgeFightTroopIterator {
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

    pub(crate) fn advance_fight(&mut self, dt: f32, edge_length: f32) -> bool {
        // apply acceleration and velocity
        self.velocity += self.acceleration * dt;
        self.advancement += self.velocity * dt / edge_length.sqrt();
        // apply friction
        self.acceleration *= 0.6;
        self.velocity *= 0.95;
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
    pub(crate) fn winner(&mut self, dt: f32) -> Option<(usize, Troop)> {
        let troop_count_per_direction = [self.troops[0].len(), self.troops[1].len()];
        let winner = if troop_count_per_direction[0] == 1 && troop_count_per_direction[1] == 0 {
            // direction 0 has won
            Some((0, self.troops[0][0]))
        } else if troop_count_per_direction[0] == 0 && troop_count_per_direction[1] == 1 {
            // direction 1 has won
            Some((1, self.troops[1][0]))
        } else {
            // no winner, reset the win timer
            self.win_timer = 0.0;
            None
        };

        // don't kill this fight the instant a winner emerges; instead let it move on a little so the winner can enjoy the gained acceleration
        if winner.is_some() {
            const WIN_DURATION: f32 = 0.65;
            if self.win_timer >= WIN_DURATION {
                return winner;
            } else {
                // advance the win-timer
                self.win_timer += dt;
            }
        }
        None
    }

    pub(crate) fn add_troop(&mut self, i: usize, new_troop: &mut Troop) -> UnitCount {
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

pub struct EdgeFightTroopIterator<'a> {
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
    pub(crate) fn new(troop: Troop, start_index: usize) -> AdvancingTroop {
        AdvancingTroop {
            troop,
            advancement: if start_index == 0 { 0.0 } else { 1.0 }
        }
    }
}

impl PartialEq for AdvancingTroop {
    fn eq(&self, other: &Self) -> bool {
        self.advancement == other.advancement
    }
}

impl PartialOrd for AdvancingTroop {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.advancement.partial_cmp(&other.advancement)
    }
}

impl Eq for AdvancingTroop {}

impl Ord for AdvancingTroop {
    fn cmp(&self, other: &Self) -> Ordering {
        self.advancement.partial_cmp(&other.advancement).unwrap()
    }
}

/// holds the unit-count and the player owning these units
#[derive(Copy, Clone, Debug)]
pub struct Troop {
    pub count: UnitCount,
    pub player: PlayerId
}

impl Troop {
    pub(crate) fn troop_of_player(troops: &[Troop], p_id: PlayerId) -> Option<&Troop> {
        if p_id == ANYONE_PLAYER {
            // choose a random troop
            troops.iter().choose(&mut thread_rng())
        } else {
            troops.iter().find(|troop| troop.player == p_id)
        }
    }
    pub(crate) fn troop_of_player_mut(troops: &mut [Troop], p_id: PlayerId) -> Option<&mut Troop> {
        if p_id == ANYONE_PLAYER {
            // choose a random troop
            troops.iter_mut().choose(&mut thread_rng())
        } else {
            troops.iter_mut().find(|troop| troop.player == p_id)
        }
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
    fn max_unit_count(&self) -> UnitCount {
        match self.player {
            ANYONE_PLAYER => 2,
            CANCER_PLAYER => 11,
            _ => 99,
        }
    }
    /// Returns how many were actually added
    pub(crate) fn add_units(&mut self, amount: UnitCount) -> UnitCount {
        let mut amount_added = 0;
        let max = self.max_unit_count();
        //println!("max = {}", max);
        if let Some(new_count) = self.count.checked_add(amount) {
            (self.count, amount_added) = if new_count <= max {
                (new_count, amount)
            } else {
                (max, max - self.count)
            };
        } else {
            (self.count, amount_added) = (max, max - self.count);
        }
        //println!("amount_added = {}", amount_added);
        amount_added
    }
    /// Returns how many were actually removed
    pub fn remove_units(&mut self, amount: UnitCount) -> UnitCount {
        let mut amount_removed: UnitCount = 0;
        (self.count, amount_removed) = match self.count.checked_sub(amount) {
            Some(new_count) => (new_count, amount),
            None => (0, self.count)
        };
        amount_removed
    }
}

impl GameEdge {
    fn can_take_on_more_units(&self) -> bool { self.unit_count() < self.max_unit_count() }
    /// Returns whether the troop was added successfully.
    pub(crate) fn add_troop(&mut self, start_index: usize, p_id: PlayerId, amount: UnitCount) -> bool {
        if amount == 0 { return true; }
        if self.unit_count() < self.max_unit_count() {
            self.advancing_troops[start_index].push(AdvancingTroop::new(Troop { count: amount, player: p_id }, start_index));
            return true;
        }
        false
    }
    /// WARNING: This function does not check for a maximum unit count. Handle with care.
    fn add_troop_advanced(&mut self, start_index: usize, troop: Troop, advancement: f32) {
        // add them so that the vector remains sorted
        let mut not_inserted = true;
        if start_index == 0 {
            for i in 0..self.advancing_troops[start_index].len() {
                if self.advancing_troops[start_index][i].advancement < advancement {
                    self.advancing_troops[start_index].insert(i, AdvancingTroop { troop, advancement });
                    not_inserted = false;
                    break;
                }
            }
        } else {
            for i in 0..self.advancing_troops[start_index].len() {
                if self.advancing_troops[start_index][i].advancement > advancement {
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

    pub(crate) fn advance_fights(&mut self, dt: f32, edge_length: f32) {
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
            //else if fight_happened {
            else if let Some((st_index, troop)) = fight.winner(dt) {
                units_to_add.push((st_index, troop, fight.advancement));
                return false;
            }
            //return true;
            //}
            else { true }
        });
        for w in units_to_add {
            // add them to the edge
            println!("adding troop");
            self.add_troop_advanced(w.0, w.1, w.2);
        }

    }

    fn check_for_fights(&mut self, advance: f32) {
        let mut checking = true;
        while checking {
            // first check whether troops have entered known fights
            // for direction 0
            let mut checked_players: SmallVec<[PlayerId; 8]> = smallvec![ANYONE_PLAYER]; // ANYONE_PLAYER doesn't need to be checked because its units don't interfere
            for advancing_troop in self.advancing_troops[0].iter_mut() {  // first elements are the oldest/most advanced
                let my_player = advancing_troop.troop.player;
                if checked_players.contains(&my_player) { continue; }
                let mut no_fight_found = true;
                // go through all known fights
                for edge_fight in self.fights.iter_mut() {
                    if advancing_troop.advancement - advance <= edge_fight.advancement && edge_fight.advancement <= advancing_troop.advancement {
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
            let mut checked_players: SmallVec<[PlayerId; 8]> = smallvec![ANYONE_PLAYER];
            for advancing_troop in self.advancing_troops[1].iter_mut() {  // first elements are the oldest/most advanced
                let my_player = advancing_troop.troop.player;
                if checked_players.contains(&my_player) { continue; }
                let mut no_fight_found = true;
                // go through all known fights
                for edge_fight in self.fights.iter_mut() {
                    if advancing_troop.advancement <= edge_fight.advancement && edge_fight.advancement <= advancing_troop.advancement + advance {
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
            let mut checked_players: SmallVec<[PlayerId; 8]> = smallvec![ANYONE_PLAYER];
            for (my_i, advancing_troop) in self.advancing_troops[0].iter().enumerate() {  // first elements are the oldest/most advanced
                let my_player = advancing_troop.troop.player;
                if checked_players.contains(&my_player) { continue; }
                for (other_i, other_troop) in self.advancing_troops[1].iter().enumerate() {
                    if my_player != other_troop.troop.player && other_troop.troop.player != ANYONE_PLAYER { // don't start fights with the ANYONE_PLAYER (i.e. the producer player)
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
    pub(crate) fn advance_troops(&mut self, game_nodes: &mut [GameNode], physics_state: &PhysicsState, control_changed_nodes: &mut SmallVec<[NId; 32]>, my_e_id: EId, dt: f32) {
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

impl GameNode {
    pub fn troop_iter(&self) -> Iter<Troop> {
        self.troops.iter()
    }

    pub fn troop_of_player(&self, player_id: PlayerId) -> Option<&Troop> {
        Troop::troop_of_player(&self.troops, player_id)
    }

    pub fn troop_of_player_mut(&mut self, player_id: PlayerId) -> Option<&mut Troop> {
        Troop::troop_of_player_mut(&mut self.troops, player_id)
    }

    pub(crate) fn start_fight(&mut self) {
        self.fight = Some(Fight::new());
        self.set_controlled_by(NO_PLAYER);
    }

    pub fn fighting(&self) -> bool { self.fight.is_some() }

    /// Returns whether the fight has ended (signaling the need for an update of the neighboring edges)
    pub(crate) fn advance_fight(&mut self, dt: f32) -> bool {
        if let Some(active_fight) = &mut self.fight {
            active_fight.advance(self.troops.iter_mut(), dt);
            // remove possible empty troops
            self.troops.retain(|t| t.count != 0);
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
}