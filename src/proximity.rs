use crate::{MainState, NId};
use ggez::{Context, timer};

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
            for (other_n_id, node) in self.physics_state.node_iter().enumerate() {
                let distance = (node.position - pos).norm();
                const PROXIMITY: f32 = 800.0;
                if distance <= PROXIMITY && other_n_id != n_id {
                    prox_vec.push(other_n_id as NId);
                }
            }
        }
    }
}