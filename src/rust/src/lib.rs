use extendr_api::prelude::*;
use rustworkx_core::petgraph::graph::{DiGraph, EdgeIndex, NodeIndex};
use rustworkx_core::petgraph::Direction;

/// The shared topology backing a `node_vec`/`edge_vec` pair (non-hyperedge
/// case only -- see `_dev/RUST_BACKEND.md`). Edges are stored as a plain
/// directed arc list regardless of `directed`; `directed` is metadata that
/// the query methods interpret, not a different graph type. This keeps the
/// object immutable and shareable: a `node_vec`, its `edges()` reorientation,
/// and any `edge_vec` sliced from it can all hold the same pointer.
///
/// @export
#[extendr]
struct GraphBackend {
    graph: DiGraph<(), ()>,
    directed: bool,
}

#[extendr]
impl GraphBackend {
    /// Build a graph on `n` nodes from 1-based `from`/`to` positions.
    /// Edges are added in input order and never removed afterwards, so
    /// edge ids (0-based internally, 1-based at the R boundary) stay stable
    /// and match the row order of the R-side edge attribute table.
    fn new(n: i32, from: Vec<i32>, to: Vec<i32>, directed: bool) -> Self {
        let n = if n > 0 { n as usize } else { 0 };
        let mut graph = DiGraph::<(), ()>::with_capacity(n, from.len());
        for _ in 0..n {
            graph.add_node(());
        }
        for (f, t) in from.iter().zip(to.iter()) {
            let fi = NodeIndex::new((*f - 1) as usize);
            let ti = NodeIndex::new((*t - 1) as usize);
            graph.add_edge(fi, ti, ());
        }
        GraphBackend { graph, directed }
    }

    fn n_nodes(&self) -> i32 {
        self.graph.node_count() as i32
    }

    fn n_edges(&self) -> i32 {
        self.graph.edge_count() as i32
    }

    fn is_directed(&self) -> bool {
        self.directed
    }

    /// 1-based neighbour positions of `node` (1-based). For an undirected
    /// graph `mode` is ignored and the symmetric neighbour set is always
    /// returned (a self-loop appears twice, once via each of the node's
    /// outgoing/incoming adjacency lists -- see the crate's tests). For a
    /// directed graph, `mode` is `"out"`, `"in"`, or `"all"` (both,
    /// concatenated). One entry per incident edge, not deduplicated, so
    /// `degree()` can just be `neighbors().len()`.
    fn neighbors(&self, node: i32, mode: &str) -> Vec<i32> {
        let idx = NodeIndex::new((node - 1) as usize);
        if !self.directed {
            return self.symmetric_neighbors(idx);
        }
        match mode {
            "out" => self.directed_neighbors(idx, Direction::Outgoing),
            "in" => self.directed_neighbors(idx, Direction::Incoming),
            "all" => self.symmetric_neighbors(idx),
            _ => panic!("`mode` must be one of \"out\", \"in\", \"all\", not \"{mode}\""),
        }
    }

    fn degree(&self, node: i32, mode: &str) -> i32 {
        self.neighbors(node, mode).len() as i32
    }

    /// Adjacency test. For an undirected graph, checks both orientations.
    fn has_edge(&self, from: i32, to: i32) -> bool {
        let fi = NodeIndex::new((from - 1) as usize);
        let ti = NodeIndex::new((to - 1) as usize);
        if self.graph.find_edge(fi, ti).is_some() {
            return true;
        }
        !self.directed && self.graph.find_edge(ti, fi).is_some()
    }

    /// All edges as 1-based `(from, to)` pairs, in construction/edge-id
    /// order. Backs `edge_vec`'s `format()`/`$from`/`$to` and `as.igraph()`
    /// -- no R-side edge table is needed for topology once this exists.
    fn edge_endpoints(&self) -> List {
        let m = self.graph.edge_count();
        let mut from: Vec<i32> = Vec::with_capacity(m);
        let mut to: Vec<i32> = Vec::with_capacity(m);
        for i in 0..m {
            let (a, b) = self
                .graph
                .edge_endpoints(EdgeIndex::new(i))
                .expect("edge index in range");
            from.push(a.index() as i32 + 1);
            to.push(b.index() as i32 + 1);
        }
        list!(from = from, to = to)
    }

    /// Induced-subgraph remap for a node set sliced/replicated according to
    /// `idx`: `idx[j]` is the 1-based *old* node position that *new* node
    /// `j` came from, `0` meaning "no source" (a new node with nothing
    /// mapping to it -- R's `NA_integer_` maps to this sentinel before the
    /// call, since it doesn't survive the R -> Rust integer conversion).
    /// Duplicate entries mean a replicated node. An edge is dropped if
    /// either endpoint has no surviving new position; an edge whose
    /// endpoint(s) were replicated is cloned once per combination.
    ///
    /// Returns `list(from, to, source_edge)`, all 1-based and in a stable
    /// order (new `to` slowest, new `from` fastest, matching
    /// `expand.grid(from, to)`'s order): `source_edge[k]` is the original
    /// edge id that surviving new edge `k` was cloned from, so the R side
    /// can carry edge attribute columns across replication with
    /// `edges[source_edge, ]`.
    fn induced_subgraph(&self, idx: Vec<i32>) -> List {
        let n_old = self.graph.node_count();
        let mut new_positions: Vec<Vec<i32>> = vec![Vec::new(); n_old];
        for (j, &p) in idx.iter().enumerate() {
            if p >= 1 && (p as usize) <= n_old {
                new_positions[(p - 1) as usize].push((j + 1) as i32);
            }
        }

        let mut new_from: Vec<i32> = Vec::new();
        let mut new_to: Vec<i32> = Vec::new();
        let mut source_edge: Vec<i32> = Vec::new();

        for i in 0..self.graph.edge_count() {
            let (a, b) = self
                .graph
                .edge_endpoints(EdgeIndex::new(i))
                .expect("edge index in range");
            let from_opts = &new_positions[a.index()];
            let to_opts = &new_positions[b.index()];
            if from_opts.is_empty() || to_opts.is_empty() {
                continue;
            }
            for &t in to_opts {
                for &f in from_opts {
                    new_from.push(f);
                    new_to.push(t);
                    source_edge.push((i + 1) as i32);
                }
            }
        }

        list!(from = new_from, to = new_to, source_edge = source_edge)
    }
}

impl GraphBackend {
    // The symmetric neighbour set, out edges then in edges. Deliberately
    // NOT `Graph::neighbors_undirected()`: that method special-cases a
    // self-loop to report it once (see its doc comment/impl -- it exists to
    // stop a *genuinely* undirected `petgraph` graph from double-reporting
    // one physical edge via both its incoming and outgoing adjacency
    // lists). Our graph is always the `Directed` petgraph type internally
    // (see the struct doc), so `neighbors_directed()` for each direction
    // never applies that skip (it only triggers when `self.graph.is_directed()`
    // is false, which for us is never), giving the doubled count the
    // undirected-self-loop convention wants. Confirmed by this file's tests
    // rather than assumed -- the two methods disagree on exactly this case.
    fn symmetric_neighbors(&self, idx: NodeIndex) -> Vec<i32> {
        let mut v = self.directed_neighbors(idx, Direction::Outgoing);
        v.extend(self.directed_neighbors(idx, Direction::Incoming));
        v
    }

    fn directed_neighbors(&self, idx: NodeIndex, dir: Direction) -> Vec<i32> {
        self.graph
            .neighbors_directed(idx, dir)
            .map(|n| n.index() as i32 + 1)
            .collect()
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod graphvec;
    impl GraphBackend;
}

#[cfg(test)]
mod tests {
    use super::*;

    // A `node_vec`/`edge_vec` sliced with `x[i]` relies on `edge_endpoints()`
    // enumerating edges in construction order -- confirm petgraph's
    // `EdgeIndex` really is stable, contiguous insertion order for a graph
    // that never removes an edge, rather than assuming it.
    #[test]
    fn edge_endpoints_preserve_construction_order() {
        test! {
            // Deliberately not sorted by either endpoint, so an accidental
            // internal reordering (e.g. by node) would be caught.
            let from = vec![3, 1, 2, 1];
            let to = vec![1, 2, 3, 3];
            let g = GraphBackend::new(3, from.clone(), to.clone(), true);
            let ends = g.edge_endpoints();
            let got_from: Vec<i32> = ends.dollar("from").unwrap().as_integer_vector().unwrap();
            let got_to: Vec<i32> = ends.dollar("to").unwrap().as_integer_vector().unwrap();
            assert_eq!(got_from, from);
            assert_eq!(got_to, to);
        }
    }

    // The undirected-degree convention a self-loop must satisfy (matching
    // `csr_build_undirected_cpp()`'s documented convention elsewhere in the
    // project): a loop counts *twice* towards degree. Confirm
    // `neighbors_undirected()` actually produces this on the always-directed
    // internal graph, rather than assuming it.
    #[test]
    fn self_loop_counts_twice_in_undirected_degree() {
        test! {
            // Node 1 has a self-loop and one ordinary edge to node 2.
            let g = GraphBackend::new(2, vec![1, 1], vec![1, 2], false);
            assert_eq!(g.degree(1, "all"), 3); // loop (2) + edge to 2 (1)
            assert_eq!(g.degree(2, "all"), 1);
            let mut ns = g.neighbors(1, "all");
            ns.sort();
            assert_eq!(ns, vec![1, 1, 2]); // order isn't a contract, only the multiset is
        }
    }

    #[test]
    fn directed_self_loop_counts_once_per_direction() {
        test! {
            let g = GraphBackend::new(1, vec![1], vec![1], true);
            assert_eq!(g.degree(1, "out"), 1);
            assert_eq!(g.degree(1, "in"), 1);
            assert_eq!(g.degree(1, "all"), 2);
        }
    }

    #[test]
    fn has_edge_checks_both_orientations_when_undirected() {
        test! {
            let g = GraphBackend::new(2, vec![1], vec![2], false);
            assert!(g.has_edge(1, 2));
            assert!(g.has_edge(2, 1));

            let gd = GraphBackend::new(2, vec![1], vec![2], true);
            assert!(gd.has_edge(1, 2));
            assert!(!gd.has_edge(2, 1));
        }
    }

    #[test]
    fn induced_subgraph_drops_dangling_and_clones_replicated() {
        test! {
            // Triangle 1-2-3 (edges 1->2, 2->3, 3->1); new nodes <- old 1, 1, 2
            // (node 1 replicated, node 3 dropped): edge 1->2 clones once per
            // replica of 1, edges touching 3 vanish.
            let g = GraphBackend::new(3, vec![1, 2, 3], vec![2, 3, 1], true);
            let remap = g.induced_subgraph(vec![1, 1, 2]);
            let from: Vec<i32> = remap.dollar("from").unwrap().as_integer_vector().unwrap();
            let to: Vec<i32> = remap.dollar("to").unwrap().as_integer_vector().unwrap();
            let source_edge: Vec<i32> = remap
                .dollar("source_edge")
                .unwrap()
                .as_integer_vector()
                .unwrap();
            // Old edge 1 (1->2) survives once per (new-from replica, new-to) combo:
            // old 1 maps to new {1, 2}, old 2 maps to new {3} -> two clones.
            assert_eq!(from, vec![1, 2]);
            assert_eq!(to, vec![3, 3]);
            assert_eq!(source_edge, vec![1, 1]);
        }
    }

    #[test]
    fn induced_subgraph_treats_zero_as_no_source() {
        test! {
            let g = GraphBackend::new(2, vec![1], vec![2], true);
            // New node 1 has no source (sentinel 0); new node 2 <- old node 2.
            let remap = g.induced_subgraph(vec![0, 2]);
            let from: Vec<i32> = remap.dollar("from").unwrap().as_integer_vector().unwrap();
            assert!(from.is_empty());
        }
    }
}
