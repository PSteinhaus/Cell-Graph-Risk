use crate::{MainState, NId};
use ggez::{Context, timer};
use crate::physics::EdgeType;

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
            // reset the list of close walls
            let prox_vec_wall = &mut self.proximity_walls[n_id];
            prox_vec_wall.clear();
            // go through all other nodes and add their id to the list if they're close enough
            let pos = &self.physics_state.node_at(n_id as NId).position;
            for (other_n_id, other_node) in self.physics_state.nodes.iter().enumerate() {
                let distance = (other_node.position - pos).norm();
                const PROXIMITY: f32 = 800.0;
                if distance <= PROXIMITY && other_n_id != n_id {
                    prox_vec.push(other_n_id as NId);
                    // go through all edges of the nodes in your proximity
                    // if you find a wall save the wall EId in self.proximity_walls[n_id]
                    for e_id in other_node.edge_indices.iter() {
                        let edge = &self.physics_state.edges[usize::from(*e_id)];
                        if let EdgeType::Wall(_) = edge.e_type {
                            // BUT ONLY if the wall isn't one of your edges!
                            if edge.other_node(other_n_id as NId) != (n_id as NId) {
                                prox_vec_wall.push(*e_id);
                            }
                        }
                    }
                }
            }
        }
    }
}