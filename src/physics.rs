use ggez::nalgebra::{Point2, Vector2, distance, norm, normalize};
use ggez::graphics::DrawParam;
use crate::{ NId, EId };
use std::slice::{Iter, IterMut};

const SPRING_CONST: f32 = 2.0;
const NODE_MASS: f32 = 20.0;    // constant for now
const NODE_FRICTION_FACTOR: f32 = 0.99;
const TENSION_FACTOR: f32 = 0.75; // = relaxed_length / distance between nodes

pub struct PhysicsState {
    nodes: Vec<Node>,
    edges: Vec<Edge>,
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
        // first remove all edges to this node
        while !self.node_at(node_index).edge_indices.is_empty() {
            self.remove_edge(*self.node_at(node_index).edge_indices.last().unwrap());
        }
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
    pub fn can_add_edge(&self, node1_index: NId, node2_index: NId) -> bool {
        // check if such an edge already exists and whether the two nodes given are actually two different nodes
        // (but don't check whether they're in bounds, as that's unnecessary for my purposes)
        node1_index != node2_index && !self.are_neighbors(node1_index, node2_index)
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
        /*
        let edge = &self.edges[usize::from(edge_index)];
        for i in 0..2 {
            let node_index = edge.node_indices[i];
            self.nodes[usize::from(node_index)].remove_edge(edge_index);
        }
        */
        let (nodes, edges) = self.data_mut();
        let edge = &edges[usize::from(edge_index)];
        for i in 0..2 {
                let node_index = edge.node_indices[i];
                nodes[usize::from(node_index)].remove_edge(edge_index);   // second borrow here (safe though since remove_edge cannot invalidate the pointer)
        }
        /*
        let edge: *const Edge = self.edge_at(edge_index);
        for i in 0..2 {
            unsafe {    // might seem unnecessary, but Rust doesn't allow this loop else, because the edge borrow has to stay
                let node_index = (*edge).node_indices[i];
                self.node_at_mut(node_index).remove_edge(edge_index);   // second borrow here (safe though since remove_edge cannot invalidate the pointer)
            }
        }
        */
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
    /// advance the simulation dt seconds
    pub fn simulate_step(&mut self, dt: f32) {
        let combined_factor = dt * SPRING_CONST / NODE_MASS;
        //let world = current_world();
        // calculate the node forces
        for edge in self.edges.iter_mut() {
            edge.calc_force(&self.nodes, combined_factor);
        }
        // apply them to the nodes and actually move them
        for (i, node) in self.nodes.iter_mut().enumerate() {
            node.apply_forces(&self.edges, i as NId);
            node.apply_velocity();
        }
    }

    pub fn edge_vec_2d(&self, e_id: EId, start_node: NId) -> Vector2<f32> {
        let edge = self.edge_at(e_id);
        let start = self.node_at(start_node);
        let end = self.node_at(edge.other_node(start_node));

        end.position - start.position
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

pub struct Edge {
    relaxed_length: f32,
    pub(crate)node_indices: [NId; 2],  // the node for which the force is computed (the other needs to change the signs of it)
    pub(crate)velocity_change: Vector2<f32>, // velocity_change = difference_from_relaxed * spring_constant * dt / mass = F_spring/mass * dt = dv
}

impl PartialEq for Edge {
    fn eq(&self, other: &Self) -> bool {
        self.node_indices == other.node_indices
    }
}

impl Edge {
    fn new(relaxed_length: f32, node1_index: NId, node2_index: NId) -> Edge {
        const STRETCH_FACTOR: f32 = (4.0/3.0);
        Edge {
            relaxed_length,
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
        const MAX_LENGTH: f32 = 800.0;
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

    /// combined_factor is SPRING_CONST * dt / mass
    fn calc_force(&mut self, nodes: &Vec<Node>, combined_factor: f32) {
        let (node1pos, node2pos) = (&nodes[usize::from(self.node_indices[0])].position, &nodes[usize::from(self.node_indices[1])].position);
        let vec: Vector2<f32> = node2pos - node1pos; // vector from node1 to node2
        let norm = norm(&vec);
        let scalar_force = (norm - self.relaxed_length) * combined_factor;
        self.velocity_change = (vec / norm) * scalar_force;
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
