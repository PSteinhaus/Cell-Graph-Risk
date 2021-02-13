use crate::{MainState, NId};
use ggez::{Context, timer};
use crate::physics::EdgeType;
use ggez::nalgebra::{Vector2, Vector, Point2};

pub fn proximity_nodes(prox_nodes: &Vec<Vec<NId>>, n_id: NId) -> &[NId] {
    &prox_nodes[usize::from(n_id)]
}

impl MainState {
    pub fn proximity_nodes(&self, n_id: NId) -> &[NId] {
        proximity_nodes(&self.proximity_nodes, n_id)
    }

    pub fn update_proximity_state(&mut self, ctx: &mut Context) {
        // go through a quarter of the nodes per loop cycle
        // the reason for this is to balance the work of having
        // to go through all nodes^2 all the time
        const PHASES: usize = 8;
        let phase = timer::ticks(ctx) % PHASES;
        let count = self.node_count();
        let part = count / PHASES;
        let start = phase * part;
        let end = if phase == PHASES - 1 {
            count
        } else {
            start + part
        };
        for n_id in start..end {
            // reset the list of close nodes
            let prox_vec = &mut self.proximity_nodes[n_id];
            prox_vec.clear();
            // go through all other nodes and add their id to the list if they're close enough
            let pos = self.physics_state.node_at(n_id as NId).position;
            for (other_n_id, other_node) in self.physics_state.nodes.iter().enumerate() {
                let distance = (other_node.position - pos).norm();
                const PROXIMITY: f32 = 800.0;
                if distance <= PROXIMITY && other_n_id != n_id {
                    prox_vec.push(other_n_id as NId);
                }
            }
        }
        // go through a part of all edges
        // for each wall collect the nodes in range
        let count = self.edge_count();
        let part = count / PHASES;
        let start = phase * part;
        let end = if phase == PHASES - 1 {
            count
        } else {
            start + part
        };
        for e_id in start..end {
            // reset the list of close nodes
            let prox_vec = &mut self.proximity_walls[e_id];
            prox_vec.clear();
            // go through all nodes and add their id to the list if they're close enough
            // TODO: sometimes some nodes are not found by this method for some reason
            //       if nothing helps just stop doing this and simply iterate over all nodes when calculating wall collisions
            let edge = &self.physics_state.edges[e_id];
            // but only if you're a wall (I have no need for proximity data otherwise)
            if let EdgeType::Wall = edge.e_type {
                let (n_id0, n_id1) = (edge.node_indices[0], edge.node_indices[1]);
                let pos0 = self.physics_state.node_at(n_id0).position;
                let pos1 = self.physics_state.node_at(n_id1).position;
                let mid: Point2<f32> = ((pos0.coords + pos1.coords) / 2.).into();
                const BORDER: f32 = 300.0;
                let search_radius: f32 = (pos1 - pos0).norm() / 2. + BORDER;
                for (other_n_id, other_node) in self.physics_state.nodes.iter().enumerate() {
                    let other_n_id = other_n_id as NId;
                    if other_n_id != n_id0 && other_n_id != n_id1 {
                        let distance = (other_node.position - mid).norm();
                        if distance <= search_radius {
                            prox_vec.push(other_n_id);
                        }
                    }
                }
            }
        }
    }
}