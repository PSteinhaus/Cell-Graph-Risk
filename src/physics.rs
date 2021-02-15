use ggez::nalgebra::{Point2, Vector2, distance, norm, clamp};
use crate::{ NId, EId };
use std::slice::{Iter, IterMut};
use crate::helpers::{intersect, orientation};
use smallvec::SmallVec;
use crate::game_mechanics::GameState;
use std::cmp::min;

const SPRING_CONST: f32 = 2.0;
const NODE_MASS: f32 = 20.0;    // constant for now
const NODE_FRICTION_FACTOR: f32 = 0.99;
const TENSION_FACTOR: f32 = 0.75; // = relaxed_length / distance between nodes

pub struct PhysicsState {
    pub nodes: Vec<Node>,
    pub edges: Vec<Edge>,
}

impl PhysicsState {
    pub fn new() -> PhysicsState {
        PhysicsState {
            nodes: Vec::new(),
            edges: Vec::new(),
        }
    }
    pub fn node_count(&self) -> usize { self.nodes.len() }
    pub fn edge_count(&self) -> usize { self.edges.len() }
    pub fn edge_at(&self, edge_index: EId) -> & Edge { &self.edges[usize::from(edge_index)] }
    pub fn edge_at_mut(&mut self, edge_index: EId) -> &mut Edge {
        &mut self.edges[usize::from(edge_index)]
    }
    pub fn node_at(&self, node_index: NId) -> &Node {
        &self.nodes[usize::from(node_index)]
    }
    pub fn node_at_mut(&mut self, node_index: NId) -> &mut Node {
        &mut self.nodes[usize::from(node_index)]
    }
    /// This is a somewhat ugly workaround for the fact that Rust does not offer partial borrowing
    pub fn data_mut(&mut self) -> (&mut [Node], &mut [Edge]) {
        (&mut self.nodes, &mut self.edges)
    }
    pub fn node_iter(&self) -> Iter<Node> { self.nodes.iter() }
    pub fn node_iter_mut(&mut self) -> IterMut<Node> { self.nodes.iter_mut() }
    pub fn edge_iter(&self) -> Iter<Edge> { self.edges.iter() }
    pub fn edge_iter_mut(&mut self) -> IterMut<Edge> { self.edges.iter_mut() }
    pub fn add_node(&mut self, position: Point2<f32>) {
        self.nodes.push(Node::new(position));
    }
    pub fn remove_node (&mut self, node_index: NId) {
        // first remove all edges to this node (no longer necessary because this is carried out earlier and more globally now)
        /*
        while !self.node_at(node_index).edge_indices.is_empty() {
            self.remove_edge(*self.node_at(node_index).edge_indices.last().unwrap());
        }
         */
        // then remove the node
        self.nodes.swap_remove(usize::from(node_index));
        // update the node index of all edges of the swapped node
        let new_length = self.nodes.len() as NId;
        if node_index < new_length
        {
            let swapped_node = self.node_at(node_index);
            for i in swapped_node.edge_indices.clone().iter() {
                self.edge_at_mut(*i).update_node_index(new_length, node_index);
            }
        }
    }
    pub fn edge_id_between(&self, n_id1: NId, n_id2: NId) -> Option<EId> {
        let mut e_id: Option<EId> = None;
        for edge_index in self.node_at(n_id1).edge_indices.iter() {
            if self.edge_at(*edge_index).other_node(n_id1) == n_id2 {
                e_id = Some(*edge_index);
                break;
            }
        }
        e_id
    }
    pub fn are_neighbors(&self, node1_index: NId, node2_index: NId) -> bool {
        // check the edges of the first node for the second node
        let mut are_neighbors = false;
        for edge_index in self.node_at(node1_index).edge_indices.iter() {
            if self.edge_at(*edge_index).other_node(node1_index) == node2_index {
                are_neighbors = true;
                break;
            }
        }
        are_neighbors
    }
    pub fn neighbors(&self, n_id: NId) -> NeighborNodeIterator {
        NeighborNodeIterator::new(n_id, &self.nodes[usize::from(n_id)], &self.edges)
    }
    pub fn edges_of_node(&'a mut self, n_id: NId) -> impl Iterator<Item=*mut Edge> + 'a {
        let e_ref: &'a mut Vec<Edge> = &mut self.edges;
        self.nodes[usize::from(n_id)].edge_indices.iter().map(move |e_id| &mut e_ref[usize::from(*e_id)] as *mut Edge)
    }
    pub fn can_add_edge(&self, node1_index: NId, node2_index: NId) -> bool {
        // check if such an edge already exists and whether the two nodes given are actually two different nodes
        // (but don't check whether they're in bounds, as that's unnecessary for my purposes)
        let arent_neighbors = node1_index != node2_index && !self.are_neighbors(node1_index, node2_index);
        // also check that there's no wall in the way
        if arent_neighbors {
            let n_pos_0 = self.node_at(node1_index).position;
            let n_pos_1 = self.node_at(node2_index).position;
            for edge in self.edges.iter() {
                if edge.is_wall() {
                    // if this edge contains one of the nodes that we try to connect to/from
                    // then we want to ignore any possible collisions
                    if !edge.node_indices.contains(&node1_index) &&
                        !edge.node_indices.contains(&node2_index)
                    {
                        let (n_id0, n_id1) = (edge.node_indices[0], edge.node_indices[1]);
                        let start = self.node_at(n_id0).position;
                        let end   = self.node_at(n_id1).position;
                        if intersect(start, end, n_pos_0, n_pos_1) {
                            return false;
                        }
                    }
                }
            }
            return true;
        } else {
            return false;
        }
    }
    pub fn distance(&self, node1_index: NId, node2_index: NId) -> f32 {
        let (node1, node2) = (self.node_at(node1_index), self.node_at(node2_index));
        distance(&node1.position, &node2.position)
    }
    pub fn add_edge(&mut self, node1_index: NId, node2_index: NId) {
        let (node1, node2) = (self.node_at(node1_index), self.node_at(node2_index));
        // we have to calc the distance between the nodes first
        let distance = distance(&node1.position, &node2.position);
        let new_edge = Edge::new(distance * TENSION_FACTOR, node1_index, node2_index);
        self.edges.push(new_edge);
        // tell the nodes that they now have a new edge
        let edge_index = (self.edges.len() - 1) as EId;
        self.node_at_mut(node1_index).add_edge(edge_index);
        self.node_at_mut(node2_index).add_edge(edge_index);
    }
    pub fn remove_edge(&mut self, edge_index: EId) {
        // update the edge-index collection of the nodes connected by the edge
        let (nodes, edges) = self.data_mut();
        let edge = &edges[usize::from(edge_index)];
        for i in 0..2 {
            let node_index = edge.node_indices[i];
            nodes[usize::from(node_index)].remove_edge(edge_index);   // second borrow here (safe though since remove_edge cannot invalidate the pointer)
        }
        // remove it from the collection
        self.edges.swap_remove(usize::from(edge_index));
        // if something else than the last edge was removed
        // update the edge index of all nodes of the swapped edge
        let new_length = self.edges.len() as EId;
        if edge_index < new_length
        {
            let swapped_edge = &self.edges[usize::from(edge_index)];
            for i in 0..2 {
                let node_edges = &mut self.nodes[usize::from(swapped_edge.node_indices[i])].edge_indices;
                let pos = node_edges.iter().position(|x| *x == new_length).unwrap();
                node_edges[pos] = edge_index;
            }
        }
    }
    /// returns whether the transformation was successful;
    /// it only adds to the vector if it is;
    pub fn turn_to_wall(&mut self, e_id: EId, edges_to_remove: &mut Vec<EId>) -> bool {
        let edge = &self.edges[usize::from(e_id)];
        let mut e_to_rem = SmallVec::<[EId; 32]>::new();
        // cut all edges intersecting this one
        let node0 = &self.nodes[usize::from(edge.node_indices[0])];
        let node1 = &self.nodes[usize::from(edge.node_indices[1])];
        let start   = node0.position;
        let end     = node1.position;
        let mut failure = false;
        for (other_e_id, other_edge) in self.edges.iter().enumerate() {
            let other_e_id = other_e_id as EId;
            // but don't cut any edges that are direct edges of one of the two nodes
            if node0.edge_indices.contains(&other_e_id) || node1.edge_indices.contains(&other_e_id) {
                continue;
            }
            // check whether this new wall would intersect any existing walls
            // if true then this cannot be turned into a wall
            let start2  = self.node_at(other_edge.node_indices[0]).position;
            let end2    = self.node_at(other_edge.node_indices[1]).position;
            if intersect(start, end, start2, end2) {
                if other_edge.is_wall() {
                    failure = true;
                    break;
                } else {
                    e_to_rem.push(other_e_id);
                }
            }
        }
        if failure {
            return false;
        } else {
            self.edges[usize::from(e_id)].e_type = EdgeType::Wall;
            for o_e_id in e_to_rem {
                if !edges_to_remove.contains(&o_e_id) {
                    edges_to_remove.push(o_e_id);
                }
            }
            true
        }
        // TODO: make the game state react to this by disabling all transport on that edge
    }
    /// advance the simulation dt seconds
    pub fn simulate_step(&mut self, dt: f32, prox_walls: &Vec<Vec<NId>>, edges_to_be_rem: &mut Vec<EId>) {
        let combined_factor = dt * SPRING_CONST / NODE_MASS;
        // calculate the node forces
        // also remove all edges for which the strain is too great
        for (e_id, edge) in self.edges.iter_mut().enumerate() {
            if edge.calc_force(&mut self.nodes, combined_factor, &prox_walls[e_id], dt) {
                edges_to_be_rem.push(e_id as EId);
            }
        }
        // apply them to the nodes and actually move them
        //let nodes_prt = &self.nodes as *const Vec<Node>;
        for (i, node) in self.nodes.iter_mut().enumerate() {
            // first save the old position
            //let old_pos = node.position;
            node.apply_forces(&self.edges, i as NId);
            node.apply_velocity();
            // check whether this node has trespassed a wall
            // for that go through all walls in your proximity
            //for wall_id in prox_walls[i].iter() {
            //    self.edges[usize::from(*wall_id)].trespass_check(i as NId, old_pos, node.position, unsafe{&*nodes_prt});
            //}
        }
    }

    pub fn edge_positions(&self, e_id: EId) -> (Point2<f32>, Point2<f32>) {
        let edge = self.edge_at(e_id);
        let start = self.node_at(edge.node_indices[0]).position;
        let end   = self.node_at(edge.node_indices[1]).position;

        (start, end)
    }

    pub fn edge_vec_2d(&self, e_id: EId, start_node: NId) -> Vector2<f32> {
        let edge = self.edge_at(e_id);
        let start = self.node_at(start_node).position;
        let end   = self.node_at(edge.other_node(start_node)).position;

        end - start
    }

    pub fn edge_length_id(&self, e_id: EId) -> f32 {
        self.edge_length(self.edge_at(e_id))
    }

    pub fn edge_length(&self, edge: &Edge) -> f32 {
        let start = self.node_at(edge.node_indices[0]);
        let end   = self.node_at(edge.node_indices[1]);

        (end.position - start.position).norm()
    }
}

pub struct Node {
    pub(crate) position: Point2<f32>,
    velocity: Vector2<f32>,
    pub(crate) edge_indices: Vec<EId>,
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.position == other.position
    }
}

pub struct NeighborNodeIterator<'a> {
    total_edges: &'a [Edge],
    node_id: NId,
    edge_iter: Iter<'a, EId>
}
impl NeighborNodeIterator<'_> {
    fn new<'a>(n_id: NId, node: &'a Node, total_edges: &'a [Edge]) -> NeighborNodeIterator<'a> { NeighborNodeIterator { total_edges, node_id: n_id, edge_iter: node.edge_indices.iter() } }
}

impl Iterator for NeighborNodeIterator<'_> {
    type Item = NId;

    fn next(&mut self) -> Option<NId> {
        if let Some(e_id) = self.edge_iter.next() {
            let next_neighbor = self.total_edges[usize::from(*e_id)].other_node(self.node_id);
            Some(next_neighbor)
        }
        else {
            None
        }
    }
}

impl Node {
    fn new(pos: Point2<f32>) -> Node {
        Node {
            position: pos,
            velocity: Vector2::new(0.0, 0.0),
            edge_indices: Vec::new(),
        }
    }
    fn add_edge(&mut self, edge_index: EId) {
        self.edge_indices.push(edge_index);
    }
    fn remove_edge(&mut self, edge_index: EId) {
        self.edge_indices.swap_remove(usize::from(self.edge_indices.iter().position(|x| *x == edge_index).unwrap()));
    }
    pub fn add_velocity(&mut self, vel_change: Vector2<f32>) {
        self.velocity += vel_change;
    }
    fn apply_forces(&mut self, edges: &Vec<Edge>, node_index: NId) {
        // first apply friction
        self.velocity *= NODE_FRICTION_FACTOR;
        // then apply the spring forces
        for i in self.edge_indices.iter() {
            self.velocity += edges[usize::from(*i)].force_step(node_index);
        }
    }
    fn apply_velocity(&mut self) {
        self.position += self.velocity;
    }
}

pub enum EdgeType {
    Normal,
    Wall,
}

pub struct Edge {
    relaxed_length: f32,
    pub(crate) e_type: EdgeType,
    pub(crate) node_indices: [NId; 2],  // the node for which the force is computed (the other needs to change the signs of it)
    pub(crate) velocity_change: Vector2<f32>, // velocity_change = difference_from_relaxed * spring_constant * dt / mass = F_spring/mass * dt = dv
}

impl PartialEq for Edge {
    fn eq(&self, other: &Self) -> bool {
        self.node_indices == other.node_indices
    }
}

impl Edge {
    fn new(relaxed_length: f32, node1_index: NId, node2_index: NId) -> Edge {
        Edge {
            relaxed_length,
            e_type: EdgeType::Normal,
            node_indices: [node1_index, node2_index],
            velocity_change: Vector2::new(0.0, 0.0),
        }
    }
    pub fn other_node(&self, n_id: NId) -> NId {
        if n_id == self.node_indices[0] {
            self.node_indices[1]
        } else {
            self.node_indices[0]
        }
    }
    pub fn contains_node(&self, n_id: NId) -> bool {
        n_id == self.node_indices[0] || n_id == self.node_indices[1]
    }
    /*
    fn trespass_check(&mut self, n_id: NId, old_pos: Point2<f32>, new_pos: Point2<f32>, nodes: &Vec<Node>) {
        // get your nodes
        let (node0, node1) = (&nodes[usize::from(self.node_indices[0])], &nodes[usize::from(self.node_indices[1])]);
        let (start, end) = (node0.position, node1.position);
        if intersect(start,
                     end,
                     old_pos,
                     new_pos) {
            // if the lines intersect the node has trespassed
            // so add it as a trespasser
            if let EdgeType::Wall(vec) = &mut self.e_type {
                // calculate its orientation to you
                let orientation = orientation(start, end, new_pos) as u8;
                vec.push((n_id, orientation));
            }
        }
    }
    */
    pub fn is_wall(&self) -> bool { if let EdgeType::Wall = self.e_type { true } else { false } }
    pub fn turn_to_normal(&mut self) {
        self.e_type = EdgeType::Normal;
    }
    pub fn shorten(&mut self, amount: f32) {
        const MIN_LENGTH: f32 = 150.0;
        if self.relaxed_length > MIN_LENGTH {
            self.relaxed_length -= amount;
            if self.relaxed_length < MIN_LENGTH {
                self.relaxed_length = MIN_LENGTH;
            }
        }
    }
    pub fn lengthen(&mut self, amount: f32) {
        const MAX_LENGTH: f32 = 1200.0;
        if self.relaxed_length < MAX_LENGTH {
            self.relaxed_length += amount;
            if self.relaxed_length > MAX_LENGTH {
                self.relaxed_length = MAX_LENGTH;
            }
        }
    }
    pub fn pos_in_edge(&self, n_id: NId) -> usize {
        self.node_indices.iter().position(|x| *x == n_id).unwrap()
    }

    /// reaches 1 when the length differs from the relaxed length by a certain percentage
    pub fn strain(&self, current_length: f32) -> f32 {
        Self::strain_static(current_length / self.relaxed_length, self.is_wall())
    }
    /// reaches 1 when the length differs from the relaxed length by a certain percentage
    pub fn strain_static(ratio_length_to_relaxed_length: f32, is_wall: bool) -> f32 {
        (ratio_length_to_relaxed_length - 1.).abs() * if is_wall { 2.5 } else { 1.5 }
    }

    /// Returns true if the strain is too great and the edge has to be removed.
    fn calc_force(&mut self, nodes: &mut Vec<Node>, combined_factor: f32, prox_wall: &Vec<NId>, dt: f32) -> bool {
        let (node1, node2) = (&mut nodes[usize::from(self.node_indices[0])] as *mut Node, &mut nodes[usize::from(self.node_indices[1])] as *mut Node);
        let (node1pos, node2pos) = unsafe { ((*node1).position, (*node2).position) };
        let vec: Vector2<f32> = node2pos - node1pos; // vector from node1 to node2
        let vec_norm = norm(&vec);
        // first check the strain and cut this edge if it's too much
        let ratio = vec_norm / self.relaxed_length;
        let is_wall = self.is_wall();
        if Self::strain_static(ratio, is_wall) >= 1. {
            // the strain is too big, cut this edge
            // also set the current force to 0 so that this edge will be ignored when calculating the node forces
            self.velocity_change = [0., 0.].into();
            return true;
        } else {
            // walls are stronger, meaning the force which they exert is stronger
            let scalar_force = (ratio - 1.) * combined_factor * 1024. * if is_wall { 4. } else { 1. };   // new elastic force
            //let scalar_force = (vec_norm - self.relaxed_length) * if is_wall { combined_factor * 8. } else { combined_factor }; // old spring based force
            let n_vec: Vector2<f32> = vec / vec_norm;
            self.velocity_change = n_vec * scalar_force;
            // if you're a wall apply force to all trespassing nodes (and your nodes as well)
            if is_wall {
                // calculate a vector perpendicular to you
                // so that it lies on your right side and is normalized
                let perp: Vector2<f32> = if n_vec.y == 0. {
                    [0., if n_vec.x.is_sign_positive() { -1. } else { 1.}].into()
                } else {
                    let x: f32 = if n_vec.y.is_sign_positive() { 1.} else { -1. };
                    let unnormalized_perp: Vector2<f32> = [x, x * (-n_vec.x) / n_vec.y].into();
                    unnormalized_perp / unnormalized_perp.norm()
                };
                // TODO: as soon as dynamic radius comes into play this has to be changed
                const RADIUS: f32 = 64.;
                let perp_with_rad = perp * RADIUS;
                for n_id in prox_wall.iter() {
                    let node = &mut nodes[usize::from(*n_id)];
                    // the problem of the approach used here is that once a node manages to cross the wall
                    // it will not be pushed out again (on the contrary, it will be pushed in!)
                    // this only becomes a problem when nodes are moving at high velocities though
                    // so as a workaround we'll just extrapolate its position a little bit backwards in time
                    // and use that as the current position instead
                    let real_pos = node.position;
                    let mut pos = real_pos;
                    const HIGH_VELOCITY: f32 = 16.;
                    let velocity_factor = node.velocity.x.abs() + node.velocity.y.abs();
                    if velocity_factor >= HIGH_VELOCITY {
                        //println!("HIGH VELOCITY! dt: {}", dt);
                        pos -= node.velocity * dt * 4.;
                    }
                    let orient = orientation(node1pos, node2pos, pos);
                    // if the line from the center of the node to the center + its radius in the direction of this wall
                    // intersects the wall, then this will be considered a collision
                    let perp_facing_the_wall = if orient <= 1 {-perp_with_rad} else {perp_with_rad};
                    if intersect(node1pos, node2pos, pos,real_pos + perp_facing_the_wall) {
                        //println!("INTERSECT! dt: {}", dt);
                        // find the point on the wall closest to the node
                        // (not really anymore, but at least calculate the parameter for finding it,
                        //  since we do need it to distribute the push-back)
                        // TODO: there's quite a bit numeric instability around the joints, so maybe find a way to stabilize it
                        let t = n_vec.x * (pos.x - node1pos.x) + n_vec.y * (pos.y - node1pos.y);
                        //let intersection = node1pos + (t * n_vec);
                        // calculate the push
                        // this actually needs quite a bit of fine tuning to work correctly
                        let push = -perp_facing_the_wall * combined_factor * (16. + velocity_factor * 4.);
                        //println!("t: {}", t);
                        //println!("push:  {:?}", push);
                        //println!("start: {:?}", node1pos);
                        //println!("end:   {:?}", node2pos);
                        //println!("pos:   {:?}", pos);
                        //println!("intersection: {:?}", intersection);
                        // and add it to the velocity of the node (thereby pushing it straight away from the wall)
                        node.velocity += push;
                        // now apply the inverted push to the nodes of the wall
                        // for that calculate how much of it each will get
                        let n2_factor: f32 = t.abs() / vec_norm;
                        let n1_factor: f32 = 1. - n2_factor;
                        // and then apply it
                        unsafe {
                            (*node1).velocity -= push * n1_factor;
                            (*node2).velocity -= push * n2_factor;
                        }
                    }
                }
            }
        }
        false
    }
    /// the vector that is to be applied to node
    fn force_step(&self, node_index: NId) -> Vector2<f32> {
        if node_index == self.node_indices[0] {
            self.velocity_change.clone() as Vector2<f32>
        }
        else {
            -self.velocity_change.clone() as Vector2<f32>
        }
    }
    fn update_node_index(&mut self, old_index: NId, new_index: NId) {
        if old_index == self.node_indices[0] {
            self.node_indices[0] = new_index;
        }
        else {
            self.node_indices[1] = new_index;
        }
    }
}
