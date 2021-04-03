use crate::{MainState, NId, EId};
use ggez::Context;
use ggez::nalgebra::Point2;
use rand::Rng;
use crate::game_mechanics::CellType;
use crate::physics::{EMPTY_NODE_MASS, EMPTY_NODE_RADIUS};
use ggez::graphics::set_screen_coordinates;

const LEVEL_SCALE_FACTOR: f32 = (3840 / 4) as f32;
const S: f32 = 1.65;
const M: f32 = 2.25;
const L: f32 = 3.5;
const XL: f32 = 5.;

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

    fn add_unch_n(&mut self, pos: Point2<f32>, cell_type: CellType, size_factor: f32) {
        self.add_node_of_type(pos, cell_type);
        // set the mass and radius
        // they will never be changed
        let p_node = self.physics_state.nodes.last_mut().unwrap();
        p_node.mass   = EMPTY_NODE_MASS * size_factor;
        p_node.radius = EMPTY_NODE_RADIUS * size_factor;
    }

    fn place_unchangeables(&mut self) {
        self.place_unmoveables();
        use CellType::*;
        let nodes_to_add = [
            ([2., 4.5],  Wall, M),
            ([3., 3.2],     Wall, M),
            ([5.7, 1.85],     Wall, M),
            ([8.5, 1.55],      Wall, M),
            ([11.2, 1.8],      Wall, M),
            ([13.8, 3.1],      Wall, M),
            ([14.5, 4.5],      Wall, M),
            ([13.8, 6.2],      Wall, M),
            ([11.3, 7.5],      Wall, M),
            ([8.2, 7.8],      Wall, M),
            ([5.5, 7.2],      Wall, M),
            ([2.9, 6.2],      Wall, M),

            ([0.2, 5.5],  Wall, L),
            ([-1., 4.6],  Wall, L),
            ([0.65, 3.1],  Wall, L),
            ([-1.3, 2.75],  Wall, L),
            ([2.4, 2.5],  Wall, L), // 20
            ([2.45, 1.],  Wall, L),
            ([4.1, -1.3],  Wall, L),
            ([6.2, 0.5],  Wall, L),
            ([7.8, -0.8],  Wall, L),
            ([9., -1.6],  Wall, L),
            ([10.8, 0.45],  Wall, L),
            ([14., -1.3],  Wall, L),
            ([14.2, 1.25],  Wall, L),
            ([18., -0.2],  Wall, L),
            ([17.8, 1.4],  Wall, L), // 30
            ([16., 2.6],  Wall, L),
            ([16.3, 4.4],  Wall, L),
            ([19., 3.2],  Wall, L),
            ([16.15, 6.45],  Wall, L),
            ([18., 7.],  Wall, L),
            ([13.15, 8.7],  Wall, L),
            ([13., 10.1],  Wall, L),
            ([9.2, 8.9],  Wall, L),
            ([7., 10.6],  Wall, L),
            ([5.7, 8.85],  Wall, L), // 40
            ([2.4, 7.8],  Wall, L),
            ([2., -1.1],  Wall, L),


            ([7.2, 4.4],  Wall, M),
            ([7.95, 4.5],  Wall, M),
            ([8.1, 5.1],  Wall, M),
            ([7.6, 5.4],  Wall, M),

            ([9., 5.45],  Wall, M),
            ([8.75, 5.05],  Wall, M),
            ([9.05, 4.52],  Wall, M),
            ([9.85, 4.5],  Wall, M), // 50

            ([8.1, 3.15],  Wall, M),
            ([9.2, 3.25],  Wall, M),
            ([8.85, 3.85],  Wall, M),
            ([8.3, 3.85],  Wall, M),

            ([7.6, 3.8],  Wall, S),
            ([9.5, 4.05],  Wall, S),
            ([8.35, 5.425],  Wall, S),
        ];

        for (point, c_type, size) in nodes_to_add.iter() {
            self.add_unch_n(Point2::<f32>::from(*point) * LEVEL_SCALE_FACTOR, *c_type, *size);
        }

        let edges_to_add = [
            (0, 19),
            (0, 18),
            (0, 21),
            (0, 22),
            (1, 26),
            (1, 28),
            (1, 31),
            (1, 30),
            (1, 29),
            (2, 36), // 10
            (2, 34),
            (2, 35),
            (2, 37),
            (3, 42),
            (3, 16),
            (3, 41),
            (3, 40),
            (3, 39),
            (4, 18),
            (4, 5),  // 20
            (4, 15),
            (4, 16),
            (5, 20),
            (5, 6),
            (6, 21),
            (6, 23),
            (6, 7),
            (7, 23),
            (7, 26),
            (7, 8),  // 30
            (8, 26),
            (8, 28),
            (8, 9),
            (9, 28),
            (9, 31),
            (9, 10),
            (10, 31),
            (10, 34),
            (10, 11),
            (11, 34), // 40
            (11, 36),
            (11, 12),
            (12, 36),
            (12, 38),
            (12, 13),
            (13, 38),
            (13, 40),
            (13, 14),
            (14, 40),
            (14, 41), // 50
            (14, 15),
            (15, 41),
            (15, 16),
            (16, 41),
            (16, 17),
            (16, 18),
            (17, 42),
            (17, 19),
            (18, 19),
            (18, 20), // 60
            (20, 21),
            (21, 22),
            (21, 23),
            (24, 22),
            (22, 25),
            (24, 23),
            (23, 26),
            (24, 27),
            (24, 25),
            (25, 27), // 70
            (27, 26),
            (26, 28),
            (27, 29),
            (28, 31),
            (29, 30),
            (29, 33),
            (30, 31),
            (30, 32),
            (31, 32),
            (32, 33), // 80
            (32, 35),
            (32, 34),
            (33, 35),
            (34, 36),
            (36, 38),
            (38, 37),
            (37, 39),
            (38, 40),
            (40, 39),
            (40, 41), // 90

            (43, 46),
            (43, 44),
            (44, 45),
            (46, 45),

            (47, 48),
            (48, 49),
            (49, 50),
            (47, 50),

            (51, 52), // 100 (due to a mistake)
            (52, 53),
            (53, 54),
            (54, 51),

            (51, 55),
            (55, 43),
            (55, 54),
            (55, 44), // 8

            (56, 53),
            (56, 52),
            (56, 49),
            (56, 50),

            (57, 46),
            (57, 45),
            (57, 48),
            (57, 47), // 15
        ];

        for (start, end) in edges_to_add.iter() {
            self.add_edge(*start as NId, *end as NId);
        }

        self.unchangeable_nodes = self.node_count();
        self.unchangeable_edges = self.edge_count();
    }

    fn place_unmoveables(&mut self) {
        use CellType::*;
        let nodes_to_add = [
            ([0f32, 0f32],  Wall, L),
            ([16., 0.], Wall, L),
            ([16., 9.], Wall, L),
            ([0., 9.],  Wall, L),
        ];

        for (point, c_type, size) in nodes_to_add.iter() {
            self.add_unch_n(Point2::<f32>::from(*point) * LEVEL_SCALE_FACTOR, *c_type, *size);
        }
        self.unmovable_nodes = self.node_count();
    }

    fn place_changeables(&mut self) {
        use CellType::*;
        let nodes_to_add = [
            ([4.2, 4.],  Producer),
            ([4., 3.5],  Basic),
            ([4.45, 3.1], StartNode), // 60

            ([10.2, 2.6],  Producer),
            ([11.2, 2.5],  Basic),
            ([11.4, 3.1],  StartNode),

            ([12.2, 5.55],  Producer),
            ([12.5, 6.0],  Basic),
            ([12.0, 6.2],  StartNode),

            ([6.5, 6.6],  Producer),
            ([5.7, 6.5],  Basic),
            ([5.5, 6.8], StartNode),

            ([8.5, 4.4], Basic),   // 70
        ];

        for (point, c_type) in nodes_to_add.iter() {
            self.add_node_of_type(Point2::<f32>::from(*point) * LEVEL_SCALE_FACTOR, *c_type);
        }

        let edges_to_add = [
            (58, 59),
            (59, 60),

            (61, 62),
            (62, 63),

            (64, 65),
            (65, 66),

            (67, 68),
            (68, 69),

            (70, 44),
            (70, 45),
            (70, 48),
            (70, 49),
            (70, 54),
            (70, 53),
            (70, 55),
            (70, 56),
            (70, 57),

            (14, 68),
            (15, 68),
            (5, 59),
            (4, 59),
            (8, 62),
            (9, 62),
            (11, 65),
            (12, 65),
        ];

/*
        let edges_to_add = [
            (0, 1),
            (1, 2),

            (3, 4),
            (4, 5),

            (6, 7),
            (7, 8),

            (9, 10),
            (10, 11),
        ];
*/


        for (start, end) in edges_to_add.iter() {
            self.add_edge(*start as NId, *end as NId);
        }

        /*
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
         */
    }
}