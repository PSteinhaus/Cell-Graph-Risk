use crate::{MainState, NId};

pub fn proximity_nodes(prox_nodes: &Vec<Vec<NId>>, n_id: NId) -> &[NId] {
    &prox_nodes[usize::from(n_id)]
}

impl MainState {
    pub fn proximity_nodes(&self, n_id: NId) -> &[NId] {
        proximity_nodes(&self.proximity_nodes, n_id)
    }

    pub fn update_proximity_state(&mut self) {
        // go through all physical nodes
        for n_id in 0..self.node_count() {
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