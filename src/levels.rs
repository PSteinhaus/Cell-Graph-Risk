use crate::{MainState, NId, EId};
use ggez::Context;
use ggez::nalgebra::Point2;
use rand::Rng;
use crate::game_mechanics::CellType;
use crate::physics::{EMPTY_NODE_MASS, EMPTY_NODE_RADIUS};

impl MainState {
    pub fn init_lvl(&mut self, ctx: &mut Context) {
        self.place_unchangeables();
        self.place_changeables();
    }

    pub fn is_unchangeable_edge(&self, e_id: EId) -> bool {
        e_id < self.unchangeable_edges as EId
    }

    pub fn is_unchangeable(&self, n_id: NId) -> bool {
        n_id < self.unchangeable_nodes as NId
    }

    pub fn is_unmoveable(&self, n_id: NId) -> bool {
        n_id < self.unmovable_nodes as NId
    }

    fn add_unch_n(&mut self, pos: Point2<f32>, cell_type: CellType) {
        self.add_node_of_type(pos, cell_type);
        // set the mass and radius
        // they will never be changed
        let p_node = self.physics_state.nodes.last_mut().unwrap();
        p_node.mass   = EMPTY_NODE_MASS * 4.;
        p_node.radius = EMPTY_NODE_RADIUS * 3.;
    }

    fn place_unchangeables(&mut self) {
        self.place_unmoveables();
        use CellType::*;

        let mut rng = rand::thread_rng();
        for _ in 0..3 {
            self.add_unch_n(Point2::new(rng.gen_range(100.0, 3740.0), rng.gen_range(100.0, 2060.0)), Wall);
        }

        {
            let node_len = self.node_count() as NId;
            let low = 0u16;
            for _ in 0..1 {
                let (i, j) = (rng.gen_range(low, node_len), rng.gen_range(low, node_len));
                self.add_edge(i, j);
            }
        }

        self.unchangeable_nodes = self.node_count();
        self.unchangeable_edges = self.edge_count();
    }

    fn place_unmoveables(&mut self) {
        use CellType::*;

        self.unmovable_nodes = self.node_count();
    }

    fn place_changeables(&mut self) {
        // add some nodes
        let mut rng = rand::thread_rng();

        for _ in 0..10 {
            self.add_node(Point2::new(rng.gen_range(100.0, 3740.0), rng.gen_range(100.0, 2060.0)));
        }
        for _ in 0..2 {
            self.add_node_of_type(Point2::new(rng.gen_range(100.0, 3740.0), rng.gen_range(100.0, 2060.0)), CellType::Producer);
        }
        for _ in 0..2 {
            self.add_node_of_type(Point2::new(rng.gen_range(100.0, 3740.0), rng.gen_range(100.0, 2060.0)), CellType::StartNode);
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
            let node_len = self.node_count() as NId;
            let low = self.unchangeable_nodes as u16;
            for _ in 0..20 {
                let (i, j) = (rng.gen_range(low, node_len), rng.gen_range(low, node_len));
                self.add_edge(i, j);
            }
        }

        // Now let's remove some stuff (for test purposes)
        /*
        {
            //state.remove_edge(0);
            for _ in 0..20 {
                state.remove_edge(rng.gen_range(0, state.physics_state.edge_count()) as EId);
            }
            for _ in 0..10 {
                state.remove_node(rng.gen_range(0, state.physics_state.node_count()) as NId);
            }
        }
        */
        // Remove all nodes without edges (for beauty purposes)
        {
            let mut i = self.unchangeable_nodes;
            while i < self.node_count() {
                if self.physics_state.node_at(i as NId).edge_indices.is_empty() {
                    self.remove_node(i as NId);
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
    }
}