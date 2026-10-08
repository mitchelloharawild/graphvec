use extendr_api::prelude::*;
use extendr_ffi::{R_ExternalPtrProtected, R_SetExternalPtrAddr, R_SetExternalPtrProtected};
use rustworkx_core::petgraph::csr::{Csr, NodeIndex as CsrNodeIndex};
use rustworkx_core::petgraph::graph::{EdgeIndex, Graph, NodeIndex};
use rustworkx_core::petgraph::matrix_graph::{MatrixGraph, NodeIndex as MatrixNodeIndex};
use rustworkx_core::petgraph::visit::{
    GraphBase, GraphProp, GraphRef, IntoNeighbors, IntoNeighborsDirected, IntoNodeIdentifiers,
    NodeCount, NodeIndexable, Visitable,
};
use rustworkx_core::petgraph::{Directed, Direction, EdgeType, Undirected};
use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::hash::{BuildHasher, Hasher};
use std::os::raw::c_void;
use std::sync::atomic::{AtomicU64, Ordering};

// Source of `GraphBackend::uid`: one fresh, never-reused number per graph
// built in this R session (see `uid()`), counting up from a random,
// per-session starting point so that graphs from different sessions (one
// saved with saveRDS(), one built after readRDS()) don't share a uid
// either. The randomness is std's own (`RandomState`'s OS-seeded keys), so
// R's RNG state (`.Random.seed`) is never touched. Kept to 53 bits, so a
// uid fits an R double exactly.
static NEXT_UID: AtomicU64 = AtomicU64::new(1);
static UID_BASE: AtomicU64 = AtomicU64::new(0);
const UID_MASK: u64 = (1 << 53) - 1;

fn next_uid() -> u64 {
    let mut base = UID_BASE.load(Ordering::Relaxed);
    if base == 0 {
        let random = RandomState::new().build_hasher().finish() | 1;
        base = match UID_BASE.compare_exchange(0, random, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => random,
            Err(current) => current,
        };
    }
    base.wrapping_add(NEXT_UID.fetch_add(1, Ordering::Relaxed)) & UID_MASK
}

/// Record the `list(n, from, to, directed, uid)` a `GraphBackend` was built
/// from in its external pointer's protected slot, which (unlike the Rust
/// graph the pointer addresses) survives `serialize()`/`saveRDS()`, so that
/// `graphvec_backend_revive()` can rebuild the graph after a reload. The
/// uid is also set as a `uid` attribute, which `identical()` compares
/// (unlike the protected slot): every reloaded pointer is null until it's
/// rebuilt, so without it any two reloaded graphs would be `identical()`.
/// @noRd
#[extendr]
fn graphvec_backend_set_source(mut graph: Robj, source: List) -> std::result::Result<(), String> {
    if !graph.is_external_pointer() {
        return Err("`graph` must be a `GraphBackend`.".to_string());
    }
    let uid = source.dollar("uid").map_err(|e| e.to_string())?;
    unsafe { R_SetExternalPtrProtected(graph.get(), source.get()) };
    graph.set_attrib("uid", uid).map_err(|e| e.to_string())?;
    Ok(())
}

/// Rebuild, in place, the Rust graph behind a `GraphBackend` external
/// pointer that came back null from `unserialize()`/`readRDS()` (or a
/// callr/future worker), from the source `graphvec_backend_set_source()`
/// recorded, keeping its uid. In place, so every R object sharing the
/// pointer sees the rebuilt graph. A live pointer, or one without a
/// recorded source, is left as it is.
///
/// The rebuilt graph is an ordinary extendr `GraphBackend` external
/// pointer of its own, whose address `graph` borrows; it's kept alive (and
/// is eventually freed, by its own finalizer) through `graph`'s protected
/// slot, so `graph` itself needs no finalizer and nothing here depends on
/// how extendr lays out the memory behind the address.
/// @noRd
#[extendr]
fn graphvec_backend_revive(graph: Robj) -> std::result::Result<(), String> {
    if !graph.is_external_pointer() {
        return Err("`graph` must be a `GraphBackend`.".to_string());
    }
    let sexp = unsafe { graph.get() };
    if !unsafe { extendr_api::R_ExternalPtrAddr(sexp) }.is_null() {
        return Ok(());
    }
    let source = unsafe { Robj::from_sexp(R_ExternalPtrProtected(sexp)) };
    let source = match List::try_from(source) {
        Ok(source) if source.len() >= 5 => source,
        _ => return Ok(()),
    };
    let field = |i: usize| source.elt(i).map_err(|e| e.to_string());
    let n = i32::try_from(field(0)?).map_err(|e| e.to_string())?;
    let from = Vec::<i32>::try_from(field(1)?).map_err(|e| e.to_string())?;
    let to = Vec::<i32>::try_from(field(2)?).map_err(|e| e.to_string())?;
    let directed = bool::try_from(field(3)?).map_err(|e| e.to_string())?;
    let uid = f64::try_from(field(4)?).map_err(|e| e.to_string())?;

    let mut backend = GraphBackend::new(n, from, to, directed)?;
    backend.uid = uid as u64;
    let fresh = Robj::from(backend);
    let mut values: Vec<Robj> = (0..5).map(|i| source.elt(i).unwrap()).collect();
    values.push(fresh.clone());
    let mut kept = List::from_names_and_values(["n", "from", "to", "directed", "uid", "backend"], values)
        .map_err(|e| e.to_string())?;
    unsafe {
        R_SetExternalPtrAddr(sexp, fresh.external_ptr_addr::<c_void>());
        R_SetExternalPtrProtected(sexp, kept.get_mut());
    }
    Ok(())
}

// One `Graph` instantiation, generic over `Ty: EdgeType`. `N`/`E` are both
// `()` (no weights needed -- presence alone is the payload) and `Ix` stays
// petgraph's own `graph::DefaultIx` (`u32`); the alias exists purely to
// name the two monomorphisations below without repeating them.
type GeneralGraphOf<Ty> = Graph<(), (), Ty>;

/// The two possible concrete `Graph` monomorphisations `GeneralData` can
/// hold -- `Directed` for a directed graph, `Undirected` for an undirected
/// one, chosen once at construction to match `GraphBackend::directed`
/// (`_dev/petgraph_data_types.md` S4's explicit "a real per-variant
/// implementation choice, not a rule to copy" note). Deliberately the same
/// wrapper-enum shape `DenseMatrix` below already uses for `MatrixGraph`,
/// and for the same reason: Rust has no way to make one struct field
/// "either of two concrete generic instantiations, decided at runtime"
/// without it, and `GeneralData` itself must stay a single, non-generic
/// type so `Repr::General` can hold it directly.
///
/// **Why an `Undirected` monomorphisation, rather than the always-`Directed`
/// storage `_dev/RUST_BACKEND.md` S1.1 originally specified.** petgraph and
/// rustworkx-core algorithms are generic over the `visit` traits and read a
/// graph's directedness off its *type*, through
/// `GraphProp::EdgeType`/`is_directed()` -- `degree_centrality()` branches
/// on exactly that (rustworkx-core 0.18.1 `centrality.rs:368`, read
/// directly). An undirected graph stored as `Graph<_, _, Directed>` would
/// therefore be walked as if its edges were one-way: on a `Directed` graph
/// `neighbors_directed(a, Outgoing)` follows only the outgoing adjacency
/// list, whereas on an `Undirected` one petgraph ignores the direction
/// argument entirely and returns every incident neighbour (petgraph 0.8.3
/// `graph_impl/mod.rs:930-938` -- `neighbors_directed()` narrows
/// `neighbors_undirected()`'s iterator only `if self.is_directed()`).
/// Storing the type the backend actually means is what makes a
/// trait-generic algorithm see the same graph this file's own
/// `neighbors()`/`degree()` already report -- `Repr::Dense` has worked this
/// way since it landed; this brings `Repr::General` into line.
enum GeneralGraph {
    Directed(GeneralGraphOf<Directed>),
    Undirected(GeneralGraphOf<Undirected>),
}

// All edges of one `Graph` monomorphisation as 1-based `(from, to)` pairs,
// in construction/edge-id order. `Graph` never renumbers an `EdgeIndex` for
// a graph that only ever appends (`_dev/petgraph_data_types.md` S4's
// edge-identity item, pinned by
// `edge_endpoints_preserve_construction_order`), and that is a property of
// `Graph`'s append-only edge array, not of `Ty` -- so the `Undirected`
// monomorphisation inherits it unchanged, `edge_endpoints()` returning the
// two endpoints in the order they were passed to `add_edge()`.
fn general_edge_list<Ty: EdgeType>(g: &GeneralGraphOf<Ty>) -> (Vec<i32>, Vec<i32>) {
    let m = g.edge_count();
    let mut from: Vec<i32> = Vec::with_capacity(m);
    let mut to: Vec<i32> = Vec::with_capacity(m);
    for i in 0..m {
        let (a, b) = g
            .edge_endpoints(EdgeIndex::new(i))
            .expect("edge index in range");
        from.push(a.index() as i32 + 1);
        to.push(b.index() as i32 + 1);
    }
    (from, to)
}

// Build one `Graph` instantiation from `from`/`to` (1-based), generic over
// `Ty: EdgeType` so the directed and undirected cases share one
// construction path -- the same shape `build_dense_matrix()` below has.
// Exactly one arc per supplied edge, never mirrored: an `Undirected`
// `Graph` stores an edge once and reports it from both endpoints itself
// (unlike `Csr`, whose bulk constructor does not -- `CsrData`'s doc
// comment).
fn build_general_graph<Ty: EdgeType>(n: usize, from: &[i32], to: &[i32]) -> GeneralGraphOf<Ty> {
    let mut g = GeneralGraphOf::<Ty>::with_capacity(n, from.len());
    for _ in 0..n {
        g.add_node(());
    }
    for (&f, &t) in from.iter().zip(to.iter()) {
        let fi = NodeIndex::new((f - 1) as usize);
        let ti = NodeIndex::new((t - 1) as usize);
        g.add_edge(fi, ti, ());
    }
    g
}

impl GeneralGraph {
    fn node_count(&self) -> usize {
        match self {
            GeneralGraph::Directed(g) => g.node_count(),
            GeneralGraph::Undirected(g) => g.node_count(),
        }
    }

    fn edge_count(&self) -> usize {
        match self {
            GeneralGraph::Directed(g) => g.edge_count(),
            GeneralGraph::Undirected(g) => g.edge_count(),
        }
    }

    fn edge_list(&self) -> (Vec<i32>, Vec<i32>) {
        match self {
            GeneralGraph::Directed(g) => general_edge_list(g),
            GeneralGraph::Undirected(g) => general_edge_list(g),
        }
    }

    // One edge of `edge_list()`, by 0-based edge id.
    fn edge_at(&self, i: usize) -> (i32, i32) {
        let (a, b) = match self {
            GeneralGraph::Directed(g) => g.edge_endpoints(EdgeIndex::new(i)),
            GeneralGraph::Undirected(g) => g.edge_endpoints(EdgeIndex::new(i)),
        }
        .expect("edge index in range");
        (a.index() as i32 + 1, b.index() as i32 + 1)
    }

    // The undirected neighbour set: one entry per incident edge, a
    // self-loop included exactly once. `neighbors_undirected()` walks both
    // adjacency lists but skips a self-loop on the incoming pass -- the
    // `edge.node[0] != self.skip_start` guard in `Neighbors::next()`
    // (petgraph 0.8.3 `graph_impl/mod.rs:1893-1912`), whose own comment
    // says so ("make sure we don't double count selfloops"). That guard is
    // independent of `Ty`, so both monomorphisations agree here and neither
    // needs a correction (unlike `CsrData::undirected_neighbors()`).
    fn undirected_neighbors(&self, idx: usize) -> Vec<i32> {
        let a = NodeIndex::new(idx);
        match self {
            GeneralGraph::Directed(g) => g
                .neighbors_undirected(a)
                .map(|n| n.index() as i32 + 1)
                .collect(),
            GeneralGraph::Undirected(g) => g
                .neighbors_undirected(a)
                .map(|n| n.index() as i32 + 1)
                .collect(),
        }
    }

    // Directed-mode "out"/"in" neighbours -- only ever called when
    // `GraphBackend::directed` is true, which is exactly when this is the
    // `Directed` arm (`GraphBackend::new()` chooses the two in lockstep,
    // the way `detect_dense()` does for `DenseMatrix`). The `Undirected`
    // arm is unreachable in practice; petgraph answers it as the full
    // incident set anyway (`neighbors_directed()` narrows its iterator only
    // `if self.is_directed()`, `graph_impl/mod.rs:930-938`), which is a
    // sensible answer rather than a panic if that invariant is ever broken
    // -- the same arrangement `DenseMatrix::directed_neighbors()` has.
    fn directed_neighbors(&self, idx: usize, dir: Direction) -> Vec<i32> {
        let a = NodeIndex::new(idx);
        match self {
            GeneralGraph::Directed(g) => g
                .neighbors_directed(a, dir)
                .map(|n| n.index() as i32 + 1)
                .collect(),
            GeneralGraph::Undirected(g) => g
                .neighbors_directed(a, dir)
                .map(|n| n.index() as i32 + 1)
                .collect(),
        }
    }

    // Adjacency test. No "check both orientations" branch is needed on the
    // `Undirected` arm: `Graph::find_edge()` dispatches to
    // `find_edge_undirected()` whenever `!self.is_directed()` (petgraph
    // 0.8.3 `graph_impl/mod.rs:1062-1070`), so it already agrees both ways
    // -- exactly why `Repr::Dense`'s arm needs no such branch either. That
    // check used to live in `GraphBackend::has_edge()` as `!self.directed
    // && find_edge(ti, fi)`, over always-`Directed` storage; the storage
    // now carries it.
    fn has_edge(&self, from: i32, to: i32) -> bool {
        let fi = NodeIndex::new((from - 1) as usize);
        let ti = NodeIndex::new((to - 1) as usize);
        match self {
            GeneralGraph::Directed(g) => g.find_edge(fi, ti).is_some(),
            GeneralGraph::Undirected(g) => g.find_edge(fi, ti).is_some(),
        }
    }
}

/// The general-purpose representation: an adjacency-list `Graph` -- in
/// whichever of `GeneralGraph`'s two monomorphisations matches the graph's
/// own directedness -- plus the per-node arc counts cached at construction
/// so `degree()` is O(1) instead of consuming `neighbors()`'s O(d) walk
/// (petgraph's `Graph` has no cached degree of its own -- confirmed against
/// its API, see `_dev/petgraph_data_types.md` S1).
///
/// The three caches are counted straight off the `from`/`to` arrays as
/// supplied to `new()` -- one increment per supplied arc, identical for
/// both monomorphisations -- so a self-loop contributes to both
/// `out_degree[i]` and `in_degree[i]`.
///
/// `self_loops[i]` additionally caches how many of those are self-loops
/// (`f == t`) at node `i` -- needed to keep `undirected_degree_at()` O(1):
/// `out_degree[i] + in_degree[i]` counts each self-loop *twice*, but the
/// undirected convention wants it counted once per self-loop edge (matching
/// `neighbors_undirected()`'s own behaviour, see
/// `GeneralGraph::undirected_neighbors()`'s comment), so `out_degree[i] +
/// in_degree[i] - self_loops[i]` is the O(1) formula for that -- confirmed
/// against the `neighbors_undirected()`-based iterator count by this file's
/// tests rather than assumed.
struct GeneralData {
    graph: GeneralGraph,
    out_degree: Vec<i32>,
    in_degree: Vec<i32>,
    self_loops: Vec<i32>,
}

// Node index width for `DenseData`'s `MatrixGraph`. petgraph's own default
// (`matrix_graph::DefaultIx = u16`) caps a graph at 65535 nodes -- far too
// small for graphvec's node counts -- so this is pinned to `u32` explicitly,
// matching `petgraph::graph::DefaultIx` (what `GeneralData`'s `DiGraph`
// already uses).
type DenseIx = u32;

// One `MatrixGraph` instantiation, generic over `Ty: EdgeType`
// (`_dev/petgraph_data_types.md` S4: "a real per-variant implementation
// choice" to store `Directed`/`Undirected` matching the graph's own flag,
// not a rule copied from `Repr::General`'s always-directed approach). `E`
// is `()` (no edge weights needed -- presence alone is the payload), so
// `Null` stays its default, `Option<()>` -- see `DenseData`'s doc comment
// for what that means for `_dev/petgraph_data_types.md` S2's bit-packing
// claim.
type DenseMatrixOf<Ty> = MatrixGraph<(), (), RandomState, Ty, Option<()>, DenseIx>;

/// The two possible concrete `MatrixGraph` monomorphisations `DenseData` can
/// hold -- `Directed` for a directed graph, `Undirected` for an undirected
/// one, chosen once at construction to match `GraphBackend::directed`
/// (`_dev/petgraph_data_types.md` S4's per-variant-storage note). Rust has
/// no way to make one struct field "either of two concrete generic
/// instantiations, decided at runtime" without this kind of small wrapper
/// enum; `DenseData` itself stays a single, non-generic type so `Repr::Dense`
/// can hold it directly, the same shape every other `Repr` variant has.
enum DenseMatrix {
    Directed(DenseMatrixOf<Directed>),
    Undirected(DenseMatrixOf<Undirected>),
}

impl DenseMatrix {
    // Directed-mode "out"/"in" neighbours -- only ever called when
    // `GraphBackend::directed` is true, which is exactly when this is the
    // `Directed` arm (see `detect_dense()`: the two are chosen in lockstep).
    // `MatrixGraph::neighbors_directed()` is only an inherent method on the
    // `Directed`-typed instantiation (confirmed from petgraph's own source --
    // no equivalent exists for `Undirected`), so the `Undirected` arm here is
    // unreachable in practice; it still returns a sensible (symmetric)
    // answer rather than panicking, in case that invariant is ever broken.
    fn directed_neighbors(&self, idx: usize, dir: Direction) -> Vec<i32> {
        let a = MatrixNodeIndex::new(idx);
        match self {
            DenseMatrix::Directed(m) => m
                .neighbors_directed(a, dir)
                .map(|n| n.index() as i32 + 1)
                .collect(),
            DenseMatrix::Undirected(m) => m.neighbors(a).map(|n| n.index() as i32 + 1).collect(),
        }
    }

    // The undirected neighbour set -- only ever called when
    // `GraphBackend::directed` is false, which is exactly when this is the
    // `Undirected` arm. Native `MatrixGraph::neighbors()` over a triangular
    // bit already reports a self-loop once (one bit, one appearance), not
    // twice -- no doubling to correct for here, unlike `Repr::General`'s
    // always-directed storage (see `GraphBackend::undirected_neighbors_at()`).
    fn undirected_neighbors(&self, idx: usize) -> Vec<i32> {
        let a = MatrixNodeIndex::new(idx);
        match self {
            DenseMatrix::Undirected(m) => m.neighbors(a).map(|n| n.index() as i32 + 1).collect(),
            DenseMatrix::Directed(m) => m
                .neighbors_directed(a, Direction::Outgoing)
                .map(|n| n.index() as i32 + 1)
                .collect(),
        }
    }
}

/// A borrowed, `Copy` view over `DenseData`'s `Undirected` `MatrixGraph`,
/// existing for exactly one reason: petgraph implements
/// `IntoNeighborsDirected` for `&MatrixGraph` **only** when `Ty = Directed`
/// (petgraph 0.8.3 `matrix_graph.rs:1381-1389` -- the impl names `Directed`
/// literally, unlike its `IntoNeighbors`/`IntoNodeIdentifiers`/`GraphProp`/
/// `Visitable`/`NodeIndexable`/`NodeCount` neighbours, which are all
/// `Ty: EdgeType`-generic). `MatrixGraph::neighbors_directed()` is likewise
/// an inherent method on the `Directed` instantiation alone. So an
/// undirected `Repr::Dense` graph satisfies every visit trait a
/// direction-aware rustworkx-core algorithm needs *except* that one, and
/// `core_number`/`degree_centrality` will not compile against it -- despite
/// `Repr::Dense` having stored the correct `Undirected` monomorphisation
/// since `08741ac`.
///
/// The gap is closed the way petgraph's own `Graph` closes it: for an
/// undirected graph `neighbors_directed()` ignores the direction argument
/// and returns every incident neighbour (`graph_impl/mod.rs:930-938`, where
/// `neighbors_directed()` narrows `neighbors_undirected()`'s iterator only
/// `if self.is_directed()`). Everything else forwards straight to the
/// wrapped `MatrixGraph`, `GraphProp::EdgeType` included -- it stays
/// `Undirected`, which is what makes `is_directed()` report the truth to a
/// trait-generic caller.
// `#[allow(dead_code)]`: constructed only by `with_graph_view!`, which no
// non-test caller has yet -- this is groundwork for delegating operations to
// rustworkx-core, exactly as `4d0799f`'s `CsrData` visit impls were, and the
// tests at the bottom of this file are what prove it works. Remove the
// attribute when the first operation is ported.
#[allow(dead_code)]
#[derive(Clone, Copy)]
struct UndirectedMatrix<'a>(&'a DenseMatrixOf<Undirected>);

impl GraphBase for UndirectedMatrix<'_> {
    type NodeId = <DenseMatrixOf<Undirected> as GraphBase>::NodeId;
    type EdgeId = <DenseMatrixOf<Undirected> as GraphBase>::EdgeId;
}

// `GraphRef: Copy + GraphBase` is a marker with no methods, but it is *not*
// blanket-implemented for every `Copy + GraphBase` type -- petgraph only has
// `impl<G> GraphRef for &G where G: GraphBase` plus one impl per adaptor
// (visit/mod.rs:106-108). `IntoNeighbors: GraphRef`, so a by-value view like
// this one has to say so itself.
impl GraphRef for UndirectedMatrix<'_> {}

impl NodeCount for UndirectedMatrix<'_> {
    fn node_count(&self) -> usize {
        NodeCount::node_count(self.0)
    }
}

impl GraphProp for UndirectedMatrix<'_> {
    type EdgeType = Undirected;
}

impl Visitable for UndirectedMatrix<'_> {
    type Map = <DenseMatrixOf<Undirected> as Visitable>::Map;
    fn visit_map(&self) -> Self::Map {
        self.0.visit_map()
    }
    fn reset_map(&self, map: &mut Self::Map) {
        self.0.reset_map(map)
    }
}

impl NodeIndexable for UndirectedMatrix<'_> {
    fn node_bound(&self) -> usize {
        self.0.node_bound()
    }
    fn to_index(&self, a: Self::NodeId) -> usize {
        self.0.to_index(a)
    }
    fn from_index(&self, i: usize) -> Self::NodeId {
        self.0.from_index(i)
    }
}

impl<'a> IntoNeighbors for UndirectedMatrix<'a> {
    type Neighbors = <&'a DenseMatrixOf<Undirected> as IntoNeighbors>::Neighbors;
    fn neighbors(self, a: Self::NodeId) -> Self::Neighbors {
        IntoNeighbors::neighbors(self.0, a)
    }
}

impl<'a> IntoNodeIdentifiers for UndirectedMatrix<'a> {
    type NodeIdentifiers = <&'a DenseMatrixOf<Undirected> as IntoNodeIdentifiers>::NodeIdentifiers;
    fn node_identifiers(self) -> Self::NodeIdentifiers {
        IntoNodeIdentifiers::node_identifiers(self.0)
    }
}

impl<'a> IntoNeighborsDirected for UndirectedMatrix<'a> {
    type NeighborsDirected = <&'a DenseMatrixOf<Undirected> as IntoNeighbors>::Neighbors;
    // `_d` is deliberately ignored -- see this type's doc comment: that is
    // precisely what petgraph's `Graph` does for an undirected graph, and
    // what `MatrixGraph<Undirected>` has no `neighbors_directed()` of its
    // own to do.
    fn neighbors_directed(self, a: Self::NodeId, _d: Direction) -> Self::NeighborsDirected {
        IntoNeighbors::neighbors(self.0, a)
    }
}

/// The dense representation (`_dev/petgraph_data_types.md` S2/S4 item 3): a
/// bit-matrix-backed `MatrixGraph`, chosen for graphs past the density
/// threshold in `detect_dense()` below with no duplicate edges.
///
/// **Why `from`/`to` are kept alongside `matrix` rather than answering
/// everything from the matrix alone**: `MatrixGraph::add_edge()`'s own doc
/// comment states it plainly -- "MatrixGraph does not allow adding parallel
/// (duplicate) edges," it panics if one already exists between two nodes --
/// so presence is keyed purely by the `(node, node)` pair, nothing else.
/// Unlike `Repr::General`'s `EdgeIndex` (naturally insertion-ordered),
/// `MatrixGraph` has no insertion-order edge enumeration at all to recover
/// original construction order from. The fix used here is the simple one
/// `_dev/petgraph_data_types.md` S4 calls out as sufficient when there's no
/// per-edge storage order to permute against in the first place: keep the
/// original `from`/`to` vectors verbatim (as supplied to `new()`) and answer
/// `edge_endpoints()`/`induced_subgraph()` from those directly, via
/// `GraphBackend::edge_list()` -- mirroring how `Repr::General` answers the
/// same two methods via `EdgeIndex` today, explicit storage instead of
/// relying on the matrix.
///
/// **`neighbors()` delegates straight to `matrix`'s own native queries**
/// (`DenseMatrix::directed_neighbors()`/`undirected_neighbors()` above), not
/// to `from`/`to` -- unlike an earlier version of this struct, which scanned
/// the retained vectors by hand specifically to force a self-loop to count
/// twice, matching `Repr::General`'s convention at the time. That convention
/// has since changed (see `symmetric_neighbors()` and
/// `GraphBackend::undirected_neighbors_at()`'s doc comments): a self-loop now
/// counts once everywhere, which is exactly what an `Undirected`-typed
/// `MatrixGraph` already does for free (one triangular bit per unordered
/// pair, one appearance), so there is no convention gap left to paper over
/// here -- `matrix` can just answer `neighbors()` directly.
///
/// **`degree()`'s *directed* ("out"/"in") case still uses `out_degree`/
/// `in_degree` below, not `matrix`** -- `MatrixGraph` has no cached degree
/// of its own (no `degree()` method at all, confirmed from its source), so
/// answering a directed degree query via `directed_neighbors(...).len()`
/// would silently regress back to the O(d)-via-allocated-`Vec` cost this
/// project's O(1)-where-possible bar (`_dev/petgraph_data_types.md` S1)
/// exists to rule out -- the same reasoning `GeneralData`'s own cache
/// exists for. The *undirected* ("all") case has no equivalent cache here
/// (`GraphBackend::undirected_degree_at()`'s `Repr::Dense` arm falls back to
/// counting `undirected_neighbors_at()`'s result, genuinely O(d)): unlike
/// out/in degree, a self-loop-correct O(1) undirected count would need its
/// own per-node self-loop cache the way `GeneralData::self_loops` provides,
/// which the density-threshold promotion path doesn't build -- left as-is
/// rather than adding that speculatively.
///
/// `from`/`to` are kept for edge identity (`edge_endpoints()`/
/// `induced_subgraph()`, per the paragraph above) -- not for neighbour
/// queries any more.
struct DenseData {
    matrix: DenseMatrix,
    from: Vec<i32>,
    to: Vec<i32>,
    out_degree: Vec<i32>,
    in_degree: Vec<i32>,
}

// Provisional density threshold, above which `detect_dense()` below
// promotes a graph to `Repr::Dense` provided it is also duplicate-free
// (`_dev/DATA.md` S3 step 4 / S6: "provisionally ~0.3...
// needs benchmarking" -- `_dev/petgraph_data_types.md` S5 says explicitly
// not to silently firm up a number DATA.md itself flags as unmeasured, so
// this stays exactly that provisional, a named constant with this comment
// rather than a bare literal).
const DENSE_THRESHOLD: f64 = 0.3;

// Whether `from`/`to` contains a repeated edge -- a repeated `(from, to)`
// ordered pair when directed, a repeated unordered pair when undirected
// (`_dev/petgraph_data_types.md` S4 item 3: `MatrixGraph::add_edge()`
// panics on exactly this, so `Repr::Dense` physically cannot hold such a
// graph). Checked with a canonicalised-pair `HashSet` in O(M).
fn has_duplicate_edges(from: &[i32], to: &[i32], directed: bool) -> bool {
    let mut seen: HashSet<(i32, i32)> = HashSet::with_capacity(from.len());
    for (&f, &t) in from.iter().zip(to.iter()) {
        let key = if directed || f <= t { (f, t) } else { (t, f) };
        if !seen.insert(key) {
            return true;
        }
    }
    false
}

// Reverse adjacency as a CSR pair, built by counting sort over 1-based
// `targets` (parallel to `sources`, same length): `ptr[i]..ptr[i+1]` slices
// `idx` down to the `sources` entries whose target is node `i+1`. Used by
// `detect_csr()` (`sources`/`targets` are simply `from`/`to`) to get a
// node's *reverse* adjacency from `Csr`'s own storage, which is only
// natively fast in the forward direction.
fn build_reverse_csr(n: usize, sources: &[i32], targets: &[i32]) -> (Vec<i32>, Vec<i32>) {
    let mut ptr = vec![0i32; n + 1];
    for &t in targets {
        ptr[t as usize] += 1;
    }
    for i in 0..n {
        ptr[i + 1] += ptr[i];
    }
    let mut cursor = ptr.clone();
    let mut idx = vec![0i32; targets.len()];
    for (&s, &t) in sources.iter().zip(targets.iter()) {
        let slot = cursor[(t - 1) as usize] as usize;
        idx[slot] = s;
        cursor[(t - 1) as usize] += 1;
    }
    (ptr, idx)
}

// Build one `MatrixGraph` instantiation from `from`/`to`, generic over
// `Ty: EdgeType` so the directed/undirected cases share one construction
// path (`detect_dense()` below picks which `Ty` to instantiate).
fn build_dense_matrix<Ty: EdgeType>(n: usize, from: &[i32], to: &[i32]) -> DenseMatrixOf<Ty> {
    let mut g = DenseMatrixOf::<Ty>::with_capacity(n);
    for _ in 0..n {
        g.add_node(());
    }
    for (&f, &t) in from.iter().zip(to.iter()) {
        let fi = MatrixNodeIndex::new((f - 1) as usize);
        let ti = MatrixNodeIndex::new((t - 1) as usize);
        g.add_edge(fi, ti, ());
    }
    g
}

/// `_dev/DATA.md` S3 step 4: past `DENSE_THRESHOLD` density (`M / (N choose
/// 2)`, the same formula regardless of `directed` -- ported verbatim, not
/// redesigned per-directedness, per `_dev/petgraph_data_types.md` S5's
/// "port... don't redesign") and with no duplicate edges (S4 item 3 --
/// `Repr::Dense` physically cannot hold a multigraph), promote to
/// `Repr::Dense`. `n < 2` guards `N choose 2 == 0` (no possible edges at
/// all -- skip Dense promotion rather than dividing by zero).
fn detect_dense(n: usize, from: &[i32], to: &[i32], directed: bool) -> Option<DenseData> {
    if n < 2 {
        return None;
    }
    let pairs = (n as f64) * ((n - 1) as f64) / 2.0;
    let density = (from.len() as f64) / pairs;
    if density <= DENSE_THRESHOLD {
        return None;
    }
    if has_duplicate_edges(from, to, directed) {
        return None;
    }

    let matrix = if directed {
        DenseMatrix::Directed(build_dense_matrix::<Directed>(n, from, to))
    } else {
        DenseMatrix::Undirected(build_dense_matrix::<Undirected>(n, from, to))
    };

    // O(1) directed degree cache -- see `DenseData`'s doc comment for why
    // this can't just be `directed_neighbors(...).len()` against `matrix`.
    let mut out_degree = vec![0i32; n];
    let mut in_degree = vec![0i32; n];
    for (&f, &t) in from.iter().zip(to.iter()) {
        out_degree[(f - 1) as usize] += 1;
        in_degree[(t - 1) as usize] += 1;
    }

    Some(DenseData {
        matrix,
        from: from.to_vec(),
        to: to.to_vec(),
        out_degree,
        in_degree,
    })
}

/// The general-purpose CSR representation (`_dev/petgraph_data_types.md`
/// S2/S6 item 4): a `petgraph::csr::Csr` built once at construction, chosen
/// by `detect_csr()` below for any graph that is not dense enough -- or
/// dense but with a duplicate edge (`Repr::Dense`) -- **and has no
/// duplicate edges of its own**.
///
/// **Deliberate deviation from S6 item 4's literal wording -- confirmed with
/// the user before starting, not a silent reinterpretation.** That section
/// describes this variant as *replacing* `Repr::General` outright, "the
/// general-purpose variant." Reading `Csr`'s 0.8.3 source directly (not from
/// memory) shows why that would be wrong: **`Csr` cannot represent a
/// multigraph.** `add_edge()` silently no-ops (returns `false`, easy to
/// miss) on a repeated `(a, b)` pair, and the bulk constructor
/// `from_sorted_edges()` errors on one (its `EdgesNotSorted` check requires
/// strictly increasing targets within a source row, which a duplicate
/// violates). `_dev/DATA.md`'s own cost table lists `edge_multiplicity` as
/// an operation the general backend must answer -- a parallel edge is a
/// legitimate case this project's data model supports, not one already
/// excluded upstream the way it is for `Repr::Dense`'s `MatrixGraph` (S4
/// item 3, same limitation, same fix, same `has_duplicate_edges()` gate).
/// So `Repr::General` stays exactly as it was: the always-correct fallback
/// for anything none of the more specific variants -- dense or this one --
/// can hold, multigraphs included. `Repr::Csr` is a genuinely new, narrower
/// bucket carved out of what `_dev/DATA.md` S3 step 5 described as one
/// homogeneous "otherwise" catch-all: duplicate-free, below-density-
/// threshold graphs go here; everything else that isn't dense (including
/// any graph with a duplicate edge) falls through to `Repr::General`. See
/// `GraphBackend::new()`'s selection order for where this split actually
/// happens.
///
/// **Directed-internal storage, matching `Repr::General`/`Repr::Dense`**:
/// built as `Csr<(), (), Directed>` regardless of `GraphBackend::directed`,
/// storing exactly the arcs supplied to `new()` (one per input edge, never
/// mirrored) -- not `Csr<_, _, Undirected>`. Checked directly against
/// `Csr::from_sorted_edges()`'s own doc comment: an `Undirected`-typed `Csr`
/// requires the input edge list to already contain *both* `(u, v)` and
/// `(v, u)` for every logical edge, which this project's undirected
/// convention (an edge stored once, `directed` interpreted by the query
/// methods) doesn't produce; `Csr::try_add_edge`'s own `a != b` guard also
/// shows a self-loop only gets mirrored once even under `Undirected` --
/// one more convention to fight rather than lean on. Storing directed arcs
/// and combining out+in for the symmetric view, exactly like `GeneralData`,
/// sidesteps all of that.
///
/// **This unlocks genuine O(1) directed degree without a separate cache**
/// -- unlike `Repr::Dense`, which has no O(1) primitive of `MatrixGraph`'s
/// own to lean on and so keeps an explicit `out_degree`/`in_degree` cache
/// alongside `matrix` instead (`DenseData`'s doc comment). `csr.out_degree()`
/// is petgraph's own O(1) row-length primitive; `in_ptr`/`in_idx` below are a
/// reverse CSR over the same edges, built once at construction, since `Csr`
/// only gives O(1) lookup in its one native (forward) direction. A
/// self-loop is stored once (one arc `a -> a`) but is counted once by
/// `out_degree` and once more by the reverse index's `in_degree` -- exactly
/// the doubling this project's undirected convention no longer wants (a
/// self-loop counts once, see `GraphBackend::undirected_neighbors_at()`'s
/// doc comment), so `undirected_neighbors()`/`undirected_degree()` below
/// correct for it explicitly -- the one variant that can't just delegate to
/// a native "undirected" primitive the way `Repr::General`
/// (`Graph::neighbors_undirected()`) and `Repr::Dense` (an `Undirected`-typed
/// `MatrixGraph`'s own bit storage) both can, since `Csr` has no such view
/// at all.
///
/// **Edge identity**: the same simple fix `Repr::Dense` uses, not the
/// "hidden permutation vector" `_dev/petgraph_data_types.md` S4 describes as
/// the fallback for a variant whose *natural* storage order differs from
/// input order -- which `Csr` genuinely is (it wants source-then-target
/// sorted storage, a real permutation of arbitrary construction order,
/// unlike `Repr::Dense`, which has no separate storage order to permute
/// against in the first place). `from`/`to` below are the original vectors
/// exactly as supplied to `new()`, kept verbatim alongside `csr`;
/// `edge_endpoints()`/`induced_subgraph()` answer from those via
/// `GraphBackend::edge_list()`, never from `csr` directly (which is built
/// from a *separately sorted clone*, see `build_csr()`). `csr` itself is
/// used for exactly two things: `has_edge()`'s adjacency test and the O(1)
/// degree/neighbour primitives above.
struct CsrData {
    csr: Csr<(), (), Directed>,
    from: Vec<i32>,
    to: Vec<i32>,
    // Reverse (in-)adjacency as a CSR pair, built once at construction over
    // the same edges `csr` already encodes -- see this struct's doc comment
    // ("genuine O(1) degree in both directions").
    in_ptr: Vec<i32>,
    in_idx: Vec<i32>,
}

impl CsrData {
    fn out_neighbors(&self, idx: usize) -> Vec<i32> {
        self.csr
            .neighbors_slice(idx as CsrNodeIndex)
            .iter()
            .map(|&n| n as i32 + 1)
            .collect()
    }

    fn in_neighbors(&self, idx: usize) -> Vec<i32> {
        let start = self.in_ptr[idx] as usize;
        let end = self.in_ptr[idx + 1] as usize;
        self.in_idx[start..end].to_vec()
    }

    fn out_degree(&self, idx: usize) -> i32 {
        self.csr.out_degree(idx as CsrNodeIndex) as i32
    }

    fn in_degree(&self, idx: usize) -> i32 {
        self.in_ptr[idx + 1] - self.in_ptr[idx]
    }

    // The undirected neighbour set: `out_neighbors()`/`in_neighbors()`
    // concatenated, then one self-loop appearance dropped if `idx` has one
    // -- `Csr` cannot hold a multigraph (`detect_csr()`'s
    // `has_duplicate_edges()` gate), so there is at most one such arc to
    // correct for, never more. Mirrors petgraph's own
    // `Graph::neighbors_undirected()` strategy (skip the self-loop on the
    // second/incoming pass, see `GeneralData`'s doc comment) by hand, since
    // `Csr` has no native undirected view to delegate to instead.
    fn undirected_neighbors(&self, idx: usize) -> Vec<i32> {
        let node = (idx + 1) as i32;
        let mut v = self.out_neighbors(idx);
        let mut incoming = self.in_neighbors(idx);
        if let Some(pos) = incoming.iter().position(|&n| n == node) {
            incoming.remove(pos);
        }
        v.extend(incoming);
        v
    }

    // O(1) companion to `undirected_neighbors()` above: `out_degree() +
    // in_degree()` counts a self-loop twice, so one is subtracted back off
    // when present (`contains_edge(a, a)` is `Csr`'s own O(1)-in-practice
    // adjacency test, already used by `has_edge()` below).
    fn undirected_degree(&self, idx: usize) -> i32 {
        let a = idx as CsrNodeIndex;
        let self_loop = i32::from(self.csr.contains_edge(a, a));
        self.out_degree(idx) + self.in_degree(idx) - self_loop
    }

    // `undirected_neighbors()`'s iterator form, in petgraph's own `NodeId`
    // currency rather than 1-based `i32` -- what `SymmetricCsr` below hands
    // to a trait-generic algorithm. Same multiset, same self-loop-once
    // convention; see `CsrSymmetricNeighbors`.
    #[allow(dead_code)] // see `UndirectedMatrix`'s note on this attribute
    fn symmetric_neighbors(&self, a: CsrNodeIndex) -> CsrSymmetricNeighbors<'_> {
        let idx = a as usize;
        let start = self.in_ptr[idx] as usize;
        let end = self.in_ptr[idx + 1] as usize;
        CsrSymmetricNeighbors {
            node: a,
            out: IntoNeighbors::neighbors(&self.csr, a),
            incoming: self.in_idx[start..end].iter(),
        }
    }

    fn has_edge(&self, from: i32, to: i32, directed: bool) -> bool {
        let fi = (from - 1) as CsrNodeIndex;
        let ti = (to - 1) as CsrNodeIndex;
        if self.csr.contains_edge(fi, ti) {
            return true;
        }
        // No storage-level "check both orientations" trick available here
        // (unlike `Repr::Dense`'s `Undirected`-typed `MatrixGraph`, which
        // stores one bit per unordered pair and so already agrees both
        // ways): `csr` is always `Directed`-typed storage of exactly the
        // supplied arcs (this struct's doc comment), so an undirected query
        // must explicitly try the reverse arc too, the same fallback
        // `Repr::General` uses over its own always-directed `DiGraph`.
        !directed && self.csr.contains_edge(ti, fi)
    }
}

// `rustworkx_core`/petgraph `visit`-trait coverage for `&CsrData`, so
// rustworkx-core algorithms can be called directly against a `Repr::Csr`
// graph instead of graphvec hand-porting them one at a time. petgraph's own
// `Csr` already implements `GraphBase`/`NodeCount`/`GraphProp`/`Visitable`/
// `NodeIndexable` (value-typed, `&self` methods) and `IntoNeighbors`/
// `IntoNodeIdentifiers` (reference-typed, `&'a Csr` -- `IntoNeighbors:
// GraphRef: Copy`, which `Csr` itself isn't) -- confirmed from petgraph
// 0.8.3's source, not assumed. What it does NOT implement, for any `Ty`,
// is `IntoNeighborsDirected`/`IntoEdgesDirected`: `Csr` has no reverse/
// incoming adjacency of its own at all (exactly why `CsrData` builds
// `in_ptr`/`in_idx` by hand above), so no directed rustworkx-core algorithm
// (`core_number`, direction-aware `degree_centrality`, DAG/topological
// algorithms, ...) can run against a bare `petgraph::csr::Csr`. The impls
// below close that gap for `CsrData` specifically, backed by the reverse
// index it already has. `IntoEdges`/`IntoEdgesDirected` (edge-*reference*
// access, not just neighbour node ids) are deliberately not implemented
// here -- they need `Data`/`EdgeRef` plumbing this crate's node/neighbour-
// based algorithms (the ones actually proven below) don't need, since
// `CsrData` carries no edge weights (`E = ()`) for a reference to expose
// beyond what `IntoNeighbors` already gives; add them if/when an
// edge-weighted algorithm needs them.
//
// Value-typed impls (`&self` methods) on `CsrData` itself, each forwarded
// straight to `self.csr`'s own impl of the same trait. petgraph's
// `delegate_impl` macro (confirmed from `visit/macros.rs`) automatically
// extends each of these to `&CsrData`/`&mut CsrData` too, so algorithms
// called with a `&CsrData` (the natural way to invoke one without moving
// the `Repr::Csr` it lives in) see them without a second impl block.
impl GraphBase for CsrData {
    type NodeId = CsrNodeIndex;
    type EdgeId = usize;
}

impl NodeCount for CsrData {
    fn node_count(&self) -> usize {
        self.csr.node_count()
    }
}

// The *directed* view. `CsrData::csr` is always `Directed`-typed storage,
// so this is the honest answer for a directed graph and a wrong one for an
// undirected graph -- `&CsrData` must therefore only be handed to a
// trait-generic algorithm when `GraphBackend::directed` is true. Use
// `SymmetricCsr` below for the undirected case; `with_graph_view!` picks
// between the two so no caller has to remember.
impl GraphProp for CsrData {
    type EdgeType = Directed;
}

impl Visitable for CsrData {
    type Map = <Csr<(), (), Directed> as Visitable>::Map;
    fn visit_map(&self) -> Self::Map {
        self.csr.visit_map()
    }
    fn reset_map(&self, map: &mut Self::Map) {
        self.csr.reset_map(map)
    }
}

impl NodeIndexable for CsrData {
    fn node_bound(&self) -> usize {
        self.csr.node_bound()
    }
    fn to_index(&self, a: Self::NodeId) -> usize {
        self.csr.to_index(a)
    }
    fn from_index(&self, i: usize) -> Self::NodeId {
        self.csr.from_index(i)
    }
}

// Reference-typed impls (`self` by value, over `&'a CsrData` -- a shared
// reference is `Copy`, satisfying `IntoNeighbors: GraphRef: Copy`, which
// `CsrData` itself isn't and shouldn't be). `IntoNeighbors`/
// `IntoNodeIdentifiers` forward to `&self.csr`'s own impls, unchanged;
// `IntoNeighborsDirected` is the new capability, `Outgoing` forwarded the
// same way and `Incoming` answered from `in_ptr`/`in_idx` -- the same
// arrays `CsrData::in_neighbors()` above already uses.
impl<'a> IntoNeighbors for &'a CsrData {
    type Neighbors = <&'a Csr<(), (), Directed> as IntoNeighbors>::Neighbors;
    fn neighbors(self, a: Self::NodeId) -> Self::Neighbors {
        (&self.csr).neighbors(a)
    }
}

impl<'a> IntoNodeIdentifiers for &'a CsrData {
    type NodeIdentifiers = <&'a Csr<(), (), Directed> as IntoNodeIdentifiers>::NodeIdentifiers;
    fn node_identifiers(self) -> Self::NodeIdentifiers {
        (&self.csr).node_identifiers()
    }
}

// A plain `fn` item (not a closure) so it coerces to a function pointer
// unconditionally, for `CsrDirectedNeighbors::In`'s iterator type below --
// `in_idx` holds 1-based external node ids (the same convention every other
// `Vec<i32>` in this file uses), petgraph's `NodeId` is 0-based.
fn in_idx_to_node_id(s: &i32) -> CsrNodeIndex {
    (*s - 1) as CsrNodeIndex
}

// The incoming-side iterator: a plain slice walk over `in_idx`'s 1-based
// external ids, converted to petgraph's 0-based `NodeId` -- distinct from
// the outgoing side's `Csr`-native iterator type, so `IntoNeighborsDirected`
// below wraps both in a small enum rather than trying to unify them.
enum CsrDirectedNeighbors<'a> {
    Out(<&'a Csr<(), (), Directed> as IntoNeighbors>::Neighbors),
    In(std::iter::Map<std::slice::Iter<'a, i32>, fn(&i32) -> CsrNodeIndex>),
}

impl Iterator for CsrDirectedNeighbors<'_> {
    type Item = CsrNodeIndex;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            CsrDirectedNeighbors::Out(it) => it.next(),
            CsrDirectedNeighbors::In(it) => it.next(),
        }
    }
}

impl<'a> IntoNeighborsDirected for &'a CsrData {
    type NeighborsDirected = CsrDirectedNeighbors<'a>;
    fn neighbors_directed(self, n: Self::NodeId, d: Direction) -> Self::NeighborsDirected {
        match d {
            Direction::Outgoing => CsrDirectedNeighbors::Out((&self.csr).neighbors(n)),
            Direction::Incoming => {
                let idx = n as usize;
                let start = self.in_ptr[idx] as usize;
                let end = self.in_ptr[idx + 1] as usize;
                CsrDirectedNeighbors::In(self.in_idx[start..end].iter().map(in_idx_to_node_id))
            }
        }
    }
}

// The symmetric-view iterator: every outgoing neighbour, then every
// incoming one except the self-loop. That skip is not an invention -- it is
// exactly what petgraph's own `Neighbors::next()` does for
// `Graph::neighbors_undirected()`, guarding its incoming pass with
// `edge.node[0] != self.skip_start` (petgraph 0.8.3
// `graph_impl/mod.rs:1893-1912`), and it is what makes a self-loop count
// once here as it does everywhere else in this file since `d11dfe9`.
// Skipping *every* matching entry rather than just the first is equivalent:
// `Csr` cannot hold a multigraph (`detect_csr()`'s `has_duplicate_edges()`
// gate), so at most one such arc exists.
// See `UndirectedMatrix`'s note on `#[allow(dead_code)]`.
#[allow(dead_code)]
struct CsrSymmetricNeighbors<'a> {
    node: CsrNodeIndex,
    out: <&'a Csr<(), (), Directed> as IntoNeighbors>::Neighbors,
    incoming: std::slice::Iter<'a, i32>,
}

impl Iterator for CsrSymmetricNeighbors<'_> {
    type Item = CsrNodeIndex;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(n) = self.out.next() {
            return Some(n);
        }
        for &s in self.incoming.by_ref() {
            // `in_idx` holds 1-based external node ids; petgraph's `NodeId`
            // is 0-based (see `in_idx_to_node_id()` above).
            let n = (s - 1) as CsrNodeIndex;
            if n != self.node {
                return Some(n);
            }
        }
        None
    }
}

/// A borrowed, `Copy`, **`Undirected`-typed** view over a `CsrData`.
///
/// **This is `Repr::Csr`'s deviation from the "store the directedness the
/// backend means" rule the other two variants now follow** (`GeneralGraph`,
/// `DenseMatrix`): `CsrData::csr` stays `Csr<(), (), Directed>` for an
/// undirected graph too, and this view supplies the symmetric adjacency a
/// trait-generic algorithm needs on top. Three things about petgraph 0.8.3's
/// `csr.rs`, all read directly rather than taken from `CsrData`'s older
/// summary of them, are why:
///
/// 1. `Csr::from_sorted_edges()`'s own doc comment (`csr.rs:176-178`): "When
///    constructing an **undirected** graph, edges have to be present in both
///    directions, i.e. `(u, v)` requires the sequence to also contain
///    `(v, u)`." It does no mirroring of its own -- it just pushes what it is
///    given into `column`. This project stores one arc per undirected edge,
///    so handing that list to an `Undirected`-typed `Csr` would silently
///    build a *half* graph, adjacency visible from one endpoint only.
/// 2. Pre-doubling the list to satisfy (1) fixes adjacency but breaks the
///    edge count: `from_sorted_edges()` does `self_.edge_count += 1` once per
///    *arc* pushed (`csr.rs:251`), while `edge_count()` returns that field
///    verbatim when `Ty = Undirected` (`csr.rs:271-277`). A bulk-built
///    undirected `Csr` therefore reports `2M` edges where the incremental
///    constructor reports `M` -- `try_add_edge()` increments once per logical
///    edge (`csr.rs:340-342`). Two constructors, two different answers for
///    the same graph; anything generic over `EdgeCount` sees the wrong one.
/// 3. The constructor that gets (1) and (2) right on its own is
///    `add_edge()`/`try_add_edge()`, which mirrors non-loop edges itself
///    (its `a != b` guard at `csr.rs:343` correctly leaves a self-loop
///    unmirrored). But petgraph's own doc puts building a whole graph that
///    way at **O(|V|·|E|)** -- each `add_edge_()` does a `column.insert()`
///    plus a walk over `row[a+1..]` (`csr.rs:351-372`). That is quadratic,
///    and cheap bulk construction is the entire reason `Repr::Csr` exists as
///    a separate variant from `Repr::General`.
///
/// **And not petgraph's own `visit::UndirectedAdaptor`**, which looks like
/// exactly this type. Two reasons, both from `visit/undirected_adaptor.rs`:
/// its `IntoNeighbors` is a bare
/// `neighbors_directed(Incoming).chain(neighbors_directed(Outgoing))` with
/// no self-loop guard (lines 14-24), so a loop would come back *twice* --
/// the doubled convention `d11dfe9` deliberately dropped; and it does not
/// implement `IntoNeighborsDirected` at all, so `core_number` and any other
/// algorithm bounded on it still would not compile. (It is also unusable for
/// the `MatrixGraph<Undirected>` gap above for a third reason: it *requires*
/// `G: IntoNeighborsDirected`, which is the very impl that is missing.)
///
/// So: directed arcs plus this view, rather than a representation that would
/// have to be either wrong or quadratic. The view reports
/// `GraphProp::EdgeType = Undirected`, so `is_directed()` still tells a
/// trait-generic caller the truth about the graph even though the storage
/// underneath it is `Directed` -- which is the property that actually
/// matters, and the one `_dev/petgraph_data_types.md` S4 asks for ("whichever
/// way a given variant takes it, the observable... semantics must still
/// match exactly").
// See `UndirectedMatrix`'s note on `#[allow(dead_code)]`.
#[allow(dead_code)]
#[derive(Clone, Copy)]
struct SymmetricCsr<'a>(&'a CsrData);

impl GraphBase for SymmetricCsr<'_> {
    type NodeId = <CsrData as GraphBase>::NodeId;
    type EdgeId = <CsrData as GraphBase>::EdgeId;
}

// See `UndirectedMatrix`'s `GraphRef` impl for why this is not automatic.
impl GraphRef for SymmetricCsr<'_> {}

impl NodeCount for SymmetricCsr<'_> {
    fn node_count(&self) -> usize {
        NodeCount::node_count(self.0)
    }
}

impl GraphProp for SymmetricCsr<'_> {
    type EdgeType = Undirected;
}

impl Visitable for SymmetricCsr<'_> {
    type Map = <CsrData as Visitable>::Map;
    fn visit_map(&self) -> Self::Map {
        self.0.visit_map()
    }
    fn reset_map(&self, map: &mut Self::Map) {
        self.0.reset_map(map)
    }
}

impl NodeIndexable for SymmetricCsr<'_> {
    fn node_bound(&self) -> usize {
        self.0.node_bound()
    }
    fn to_index(&self, a: Self::NodeId) -> usize {
        self.0.to_index(a)
    }
    fn from_index(&self, i: usize) -> Self::NodeId {
        self.0.from_index(i)
    }
}

impl<'a> IntoNodeIdentifiers for SymmetricCsr<'a> {
    type NodeIdentifiers = <&'a CsrData as IntoNodeIdentifiers>::NodeIdentifiers;
    fn node_identifiers(self) -> Self::NodeIdentifiers {
        IntoNodeIdentifiers::node_identifiers(self.0)
    }
}

impl<'a> IntoNeighbors for SymmetricCsr<'a> {
    type Neighbors = CsrSymmetricNeighbors<'a>;
    fn neighbors(self, a: Self::NodeId) -> Self::Neighbors {
        self.0.symmetric_neighbors(a)
    }
}

impl<'a> IntoNeighborsDirected for SymmetricCsr<'a> {
    type NeighborsDirected = CsrSymmetricNeighbors<'a>;
    // Direction ignored, matching what petgraph's `Graph` does for an
    // undirected graph (`graph_impl/mod.rs:930-938`) and what
    // `UndirectedMatrix` above does for `MatrixGraph<Undirected>`.
    fn neighbors_directed(self, a: Self::NodeId, _d: Direction) -> Self::NeighborsDirected {
        self.0.symmetric_neighbors(a)
    }
}

// Build a `Csr<(), (), Directed>` from `from`/`to` (original, 1-based,
// arbitrary order): sorts a *separate clone* of the 0-based pairs, since
// `Csr::from_sorted_edges()` wants source-then-target sorted, strictly
// increasing input (its own `EdgesNotSorted` check rejects a duplicate
// target for the same source) -- this is only ever called once
// `has_duplicate_edges()` has already ruled a duplicate out (`detect_csr()`
// below), so the sort can't turn up one `from_sorted_edges()` would reject.
// Sorting a clone rather than `from`/`to` themselves is what keeps
// `edge_endpoints()`/`induced_subgraph()` (via `GraphBackend::edge_list()`)
// answering in original construction order (S4's edge-identity item) --
// `csr` ends up a second, differently-ordered copy of the same edges, never
// the source of truth for enumeration.
fn build_csr(n: usize, from: &[i32], to: &[i32]) -> Csr<(), (), Directed> {
    let mut pairs: Vec<(u32, u32)> = from
        .iter()
        .zip(to.iter())
        .map(|(&f, &t)| ((f - 1) as u32, (t - 1) as u32))
        .collect();
    pairs.sort_unstable();
    let mut csr = Csr::<(), (), Directed>::from_sorted_edges(&pairs)
        .expect("pairs are pre-sorted and duplicate-free (has_duplicate_edges() already checked)");
    // `from_sorted_edges()` sizes the graph to `max_node_id + 1` -- it has
    // no way to know `n` beyond what the edges themselves imply -- so an
    // isolated highest-numbered node (or an empty edge list entirely) needs
    // padding up to `n` explicitly.
    while csr.node_count() < n {
        csr.add_node(());
    }
    csr
}

/// `_dev/petgraph_data_types.md` S4 item 4 (this file's deliberate deviation
/// from `_dev/DATA.md` S6 item 4 -- see `CsrData`'s doc comment): whether a
/// graph `detect_dense()` already rejected qualifies for `Repr::Csr` -- it
/// does unless it has a duplicate edge, the same `has_duplicate_edges()`
/// gate `detect_dense()` already uses (design recommendation 4: reuse it
/// rather than write a second, possibly-inconsistent check), since `Csr`
/// physically cannot hold a multigraph any more than `MatrixGraph` can.
/// Only called for graphs that already failed the density check, matching
/// this file's selection order (density/duplicate-gated dense, then
/// duplicate-gated CSR, then otherwise general).
fn detect_csr(n: usize, from: &[i32], to: &[i32], directed: bool) -> Option<CsrData> {
    if has_duplicate_edges(from, to, directed) {
        return None;
    }

    let csr = build_csr(n, from, to);

    // Reverse (in-)adjacency CSR over `from`/`to` -- see `build_reverse_csr()`'s
    // doc comment.
    let (in_ptr, in_idx) = build_reverse_csr(n, from, to);

    Some(CsrData {
        csr,
        from: from.to_vec(),
        to: to.to_vec(),
        in_ptr,
        in_idx,
    })
}

/// Which physical representation a `GraphBackend` picked for one graph
/// (`_dev/petgraph_data_types.md` S3). Private -- never `#[extendr]` itself,
/// matched inside every `GraphBackend` method so the R-visible class and
/// method set stay identical no matter which variant got chosen.
enum Repr {
    General(GeneralData),
    Dense(DenseData),
    Csr(CsrData),
}

// Run one expression against whichever concrete, correctly-directed
// petgraph view backs a `GraphBackend`, binding it to the named identifier:
//
// ```ignore
// let centrality = with_graph_view!(self, |g| {
//     rustworkx_core::centrality::degree_centrality(g, None)
// });
// ```
//
// This is the single place a rustworkx-core-backed operation has to touch
// per-representation code: write one function generic over petgraph's
// `visit` traits, call it here, done -- no hand-port per `Repr` variant,
// which is the whole point of every trait impl above.
//
// It has to be a macro rather than a method taking a closure: the arms bind
// `$g` to *different concrete types* (`&Graph<_, _, Directed>`,
// `&Graph<_, _, Undirected>`, `&MatrixGraph<..., Directed, ...>`,
// `UndirectedMatrix`, `&CsrData`, `SymmetricCsr`), so `$body` must be
// monomorphised once per arm. Rust closures are not generic over their
// argument type, so a `fn with_view<R>(&self, f: impl Fn(?) -> R)` cannot
// express this; a macro (or a trait with a generic method, which is the
// same thing with more ceremony) is the only way.
//
// Six arms, not three, because `GraphProp::EdgeType` is an associated type
// -- fixed at compile time -- while `directed` is a runtime flag, so each
// variant needs one arm per directedness. Two of the six exist only because
// petgraph leaves a gap: `UndirectedMatrix` (no `IntoNeighborsDirected` for
// `MatrixGraph<Undirected>`) and `SymmetricCsr` (no undirected `Csr` this
// project can build without either corrupting the graph or going
// quadratic). Both have doc comments giving the source citations.
// `#[allow(unused_macros)]`: nothing outside this file's tests calls it yet
// -- see `UndirectedMatrix`'s note on the matching `#[allow(dead_code)]`.
#[allow(unused_macros)]
macro_rules! with_graph_view {
    ($backend:expr, |$g:ident| $body:expr) => {{
        let backend: &GraphBackend = $backend;
        match &backend.repr {
            Repr::General(d) => match &d.graph {
                GeneralGraph::Directed(inner) => {
                    let $g = inner;
                    $body
                }
                GeneralGraph::Undirected(inner) => {
                    let $g = inner;
                    $body
                }
            },
            Repr::Dense(d) => match &d.matrix {
                DenseMatrix::Directed(inner) => {
                    let $g = inner;
                    $body
                }
                DenseMatrix::Undirected(inner) => {
                    let $g = UndirectedMatrix(inner);
                    $body
                }
            },
            Repr::Csr(d) => {
                if backend.directed {
                    let $g = d;
                    $body
                } else {
                    let $g = SymmetricCsr(d);
                    $body
                }
            }
        }
    }};
}

/// The shared topology backing a `node_vec`/`edge_vec` pair (non-hyperedge
/// case only -- see `_dev/RUST_BACKEND.md`). Wraps one of several physical
/// `Repr` variants, auto-selected at construction from graph shape
/// (`_dev/petgraph_data_types.md` S3/S5); `directed` is metadata every
/// variant's query methods interpret, not itself part of the shape
/// decision. This keeps the object immutable and shareable: a `node_vec`,
/// its `edges()` reorientation, and any `edge_vec` sliced from it can all
/// hold the same pointer.
///
/// @export
#[extendr]
struct GraphBackend {
    repr: Repr,
    directed: bool,
    uid: u64,
}

#[extendr]
impl GraphBackend {
    /// Build a graph on `n` nodes from 1-based `from`/`to` positions.
    /// Automatically picks the cheapest `Repr` the graph's shape qualifies
    /// for (`_dev/petgraph_data_types.md` S3/S5 -- this decision belongs
    /// here, not in R, so there is exactly one place shape detection can
    /// drift out of sync with the representation it feeds). Edges are added
    /// in input order and never removed afterwards, so edge ids (0-based
    /// internally, 1-based at the R boundary) stay stable and match the row
    /// order of the R-side edge attribute table, for every representation.
    ///
    /// Errors (an R error, not a crash) unless every `from`/`to` position is
    /// in `1..=n` and the two have the same length: an `NA` arrives as
    /// `i32::MIN`, and a position outside the graph would otherwise index
    /// out of bounds or, at 0 or below, wrap to a huge `usize` allocation.
    /// The R constructors check this first, with friendlier messages.
    fn new(n: i32, from: Vec<i32>, to: Vec<i32>, directed: bool) -> std::result::Result<Self, String> {
        let n = if n > 0 { n as usize } else { 0 };
        if from.len() != to.len() {
            return Err(format!(
                "`from` and `to` must have the same length, not {} and {}.",
                from.len(),
                to.len()
            ));
        }
        if let Some(&p) = from
            .iter()
            .chain(to.iter())
            .find(|&&p| p < 1 || p as usize > n)
        {
            let p = if p == i32::MIN { "NA".to_string() } else { p.to_string() };
            return Err(format!(
                "Edge endpoints must be node positions between 1 and {n}, not {p}."
            ));
        }

        if let Some(dense) = detect_dense(n, &from, &to, directed) {
            return Ok(GraphBackend {
                repr: Repr::Dense(dense),
                directed,
                uid: next_uid(),
            });
        }

        // `_dev/DATA.md` S3 step 5's "otherwise" catch-all splits in two
        // here, a deliberate deviation from `_dev/petgraph_data_types.md` S6
        // item 4's literal wording (`CsrData`'s doc comment has the full
        // reasoning, confirmed with the user before implementing): a graph
        // that reaches this point (not dense enough, or dense with a
        // duplicate) goes to `Repr::Csr` if it has no duplicate edges of its
        // own -- `Csr` cannot hold a multigraph any more than `MatrixGraph`
        // can -- and only falls through to `Repr::General` (unchanged, still
        // the always-correct fallback) when it does.
        if let Some(csr) = detect_csr(n, &from, &to, directed) {
            return Ok(GraphBackend {
                repr: Repr::Csr(csr),
                directed,
                uid: next_uid(),
            });
        }

        // `Directed`/`Undirected` picked in lockstep with `directed`, the
        // same way `detect_dense()` picks a `DenseMatrix` arm -- see
        // `GeneralGraph`'s doc comment for why the undirected case gets its
        // own monomorphisation rather than reusing directed storage.
        let graph = if directed {
            GeneralGraph::Directed(build_general_graph::<Directed>(n, &from, &to))
        } else {
            GeneralGraph::Undirected(build_general_graph::<Undirected>(n, &from, &to))
        };
        let mut out_degree = vec![0i32; n];
        let mut in_degree = vec![0i32; n];
        let mut self_loops = vec![0i32; n];
        for (f, t) in from.iter().zip(to.iter()) {
            out_degree[(*f - 1) as usize] += 1;
            in_degree[(*t - 1) as usize] += 1;
            if f == t {
                self_loops[(*f - 1) as usize] += 1;
            }
        }
        Ok(GraphBackend {
            repr: Repr::General(GeneralData {
                graph,
                out_degree,
                in_degree,
                self_loops,
            }),
            directed,
            uid: next_uid(),
        })
    }

    fn n_nodes(&self) -> i32 {
        match &self.repr {
            Repr::General(g) => g.graph.node_count() as i32,
            Repr::Dense(d) => match &d.matrix {
                DenseMatrix::Directed(m) => m.node_count() as i32,
                DenseMatrix::Undirected(m) => m.node_count() as i32,
            },
            Repr::Csr(c) => c.csr.node_count() as i32,
        }
    }

    fn n_edges(&self) -> i32 {
        match &self.repr {
            Repr::General(g) => g.graph.edge_count() as i32,
            Repr::Dense(d) => d.from.len() as i32,
            Repr::Csr(c) => c.from.len() as i32,
        }
    }

    fn is_directed(&self) -> bool {
        self.directed
    }

    /// A number unique to this graph among every graph built in the R
    /// session, never reused (unlike a memory address, which can be once a
    /// graph is garbage collected), and starting from a random point per
    /// session so it doesn't collide with a graph saved from another one.
    /// This is a graph's identity: two R objects are of the same graph
    /// exactly when their `GraphBackend`s have the same `uid()`, including
    /// after a `saveRDS()`/`readRDS()` round trip, which rebuilds the graph
    /// behind a new address but keeps its uid (`graphvec_backend_revive()`).
    /// A double, so it fits an R numeric exactly (it's kept to 53 bits).
    fn uid(&self) -> f64 {
        self.uid as f64
    }

    /// Which physical representation this backend picked, as a stable name
    /// (`"general"`, `"dense"`, `"csr"`) -- the one diagnostic entry point
    /// for "which `Repr` is this", replacing what used to be a separate
    /// `is_tree()`/`is_dense()`/`is_csr()` boolean per variant. That pattern
    /// grew one new `#[extendr]` method -- permanent, exported R API the
    /// moment it's added, per this file's own "the method set is the
    /// contract" principle -- for every future `Repr` addition; this single
    /// method's match arm count grows with `Repr` instead, so the R-visible
    /// surface stays fixed no matter how many representations `GraphBackend`
    /// eventually holds.
    fn repr_name(&self) -> String {
        match &self.repr {
            Repr::General(_) => "general",
            Repr::Dense(_) => "dense",
            Repr::Csr(_) => "csr",
        }
        .to_string()
    }

    /// Every node's degree, in node order -- `degree()` (below) for each
    /// node in one call, so `node_degree()` and the node predicates cross
    /// the R/Rust boundary once rather than once per node. Same `mode` and
    /// self-loop semantics as `degree()`, which it is defined by.
    fn degrees(&self, mode: &str) -> Vec<i32> {
        (1..=self.n_nodes()).map(|node| self.degree(node, mode)).collect()
    }

    /// The neighbours of every node in `nodes` (1-based, repeats allowed),
    /// each in increasing order, as one CSR pair `list(ptr, idx)`: node
    /// `nodes[k]`'s neighbours are `idx[(ptr[k] + 1):ptr[k + 1]]`, with
    /// `ptr` 0-based offsets of length `length(nodes) + 1`. One call for
    /// any number of query nodes (`node_neighbors()` splits it into its
    /// list result), with the same `mode`, self-loop and one-entry-per-edge
    /// semantics as `neighbors()`. Sorted here because `neighbors()`'s own
    /// order depends on which `Repr` the graph picked.
    fn neighbors_many(&self, nodes: Vec<i32>, mode: &str) -> List {
        let mut ptr: Vec<i32> = Vec::with_capacity(nodes.len() + 1);
        let mut idx: Vec<i32> = Vec::new();
        ptr.push(0);
        for &node in &nodes {
            let mut ns = self.neighbors(node, mode);
            ns.sort_unstable();
            idx.extend(ns);
            ptr.push(i32::try_from(idx.len()).expect("neighbour count fits an R integer"));
        }
        list!(ptr = ptr, idx = idx)
    }

    /// Adjacency test. For an undirected graph, checks both orientations.
    fn has_edge(&self, from: i32, to: i32) -> bool {
        self.node_index(from);
        self.node_index(to);
        match &self.repr {
            Repr::General(g) => g.graph.has_edge(from, to),
            Repr::Dense(d) => {
                // No "check both orientations" branch needed here either:
                // an `Undirected`-typed `MatrixGraph` stores one bit per
                // unordered pair (see `to_linearized_matrix_position()` in
                // petgraph's own source), so `has_edge(a, b)` and
                // `has_edge(b, a)` already agree for it. `Repr::General`
                // reaches the same conclusion by a different route --
                // `Graph::find_edge()`'s own undirected branch, see
                // `GeneralGraph::has_edge()`.
                let fi = MatrixNodeIndex::new((from - 1) as usize);
                let ti = MatrixNodeIndex::new((to - 1) as usize);
                match &d.matrix {
                    DenseMatrix::Directed(m) => m.has_edge(fi, ti),
                    DenseMatrix::Undirected(m) => m.has_edge(fi, ti),
                }
            }
            Repr::Csr(c) => c.has_edge(from, to, self.directed),
        }
    }

    /// Edges as 1-based `(from, to)` pairs: all of them in
    /// construction/edge-id order when `ids` is `NULL`, otherwise those of
    /// the 1-based edge ids `ids`, in `ids`' order (repeats allowed). An
    /// `NA` id is a missing edge, with `NA` endpoints. Backs `edge_vec`'s
    /// `format()`/`$from`/`$to` and `as.igraph()` -- no R-side edge table
    /// is needed for topology once this exists -- and lets a slice of a
    /// big graph read only its own edges' endpoints, O(length(ids)).
    fn edge_endpoints(&self, #[extendr(default = "NULL")] ids: Nullable<Vec<i32>>) -> List {
        let ids = match ids {
            Nullable::Null => {
                let (from, to) = self.edge_list();
                return list!(from = from, to = to);
            }
            Nullable::NotNull(ids) => ids,
        };
        let m = self.n_edges();
        let mut from: Vec<i32> = Vec::with_capacity(ids.len());
        let mut to: Vec<i32> = Vec::with_capacity(ids.len());
        for &id in &ids {
            // R's `NA_integer_` arrives as `i32::MIN`, and is the same bit
            // pattern going back out, so a missing edge stays `NA`.
            if id == i32::MIN {
                from.push(i32::MIN);
                to.push(i32::MIN);
                continue;
            }
            if id < 1 || id > m {
                panic!("`ids` must be edge positions between 1 and {m}, not {id}.");
            }
            let (a, b) = self.edge_at((id - 1) as usize);
            from.push(a);
            to.push(b);
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
    ///
    /// This is representation-independent: it works from `edge_list()`'s
    /// output alone (S4's edge-identity contract, already upheld there),
    /// never `self.repr` directly, so it needs no per-variant duplicate.
    /// The result is a plain from/to/source_edge list either way -- it does
    /// not construct a new `GraphBackend` itself (R reconstructs one from
    /// these lists via `new()`, confirmed by grepping `R/node_vec.R`'s
    /// `[.node_vec`), so the *new* backend's shape (which needn't match
    /// the old one -- both terms of the density ratio move here, `N`
    /// becoming `idx.len()` and `M` however many edges survived or were
    /// cloned, so e.g. dropping the centre of a `Repr::Dense` star strips
    /// every edge at once and leaves something far below
    /// `DENSE_THRESHOLD`, while dropping the isolated nodes that were
    /// holding a `Repr::Csr` graph under that threshold pushes what's left
    /// above it) is re-decided by `new()`'s own detection from scratch,
    /// same as it would be for any other from/to/directed input.
    fn induced_subgraph(&self, idx: Vec<i32>) -> List {
        let n_old = self.n_nodes() as usize;
        let (efrom, eto) = self.edge_list();

        let mut new_positions: Vec<Vec<i32>> = vec![Vec::new(); n_old];
        for (j, &p) in idx.iter().enumerate() {
            if p >= 1 && (p as usize) <= n_old {
                new_positions[(p - 1) as usize].push((j + 1) as i32);
            }
        }

        let mut new_from: Vec<i32> = Vec::new();
        let mut new_to: Vec<i32> = Vec::new();
        let mut source_edge: Vec<i32> = Vec::new();

        for (i, (&a, &b)) in efrom.iter().zip(eto.iter()).enumerate() {
            let from_opts = &new_positions[(a - 1) as usize];
            let to_opts = &new_positions[(b - 1) as usize];
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
    // 1-based neighbour positions of `node` (1-based). For an undirected
    // graph `mode` is ignored and the undirected neighbour set is always
    // returned: one entry per incident edge, a self-loop included exactly
    // once (see `undirected_neighbors_at()`'s doc comment for why, and this
    // file's tests). For a directed graph, `mode` is `"out"`, `"in"`, or
    // `"all"` (both, concatenated -- a directed self-loop counts once per
    // direction, so twice under `"all"`, unaffected by the undirected
    // convention above). One entry per incident edge, not deduplicated, so
    // `degree()` can just be `neighbors().len()`. Not an R method of its
    // own: R queries go through `neighbors_many()`.
    fn neighbors(&self, node: i32, mode: &str) -> Vec<i32> {
        let idx = self.node_index(node);
        if !self.directed {
            return self.undirected_neighbors_at(idx);
        }
        match mode {
            "out" => self.out_neighbors_at(idx),
            "in" => self.in_neighbors_at(idx),
            "all" => self.symmetric_neighbors(idx),
            _ => panic!("`mode` must be one of \"out\", \"in\", \"all\", not \"{mode}\""),
        }
    }

    // The degree of `node` (1-based): O(1) for `"out"`/`"in"`, and for
    // `"all"` on a *directed* graph (the construction-time caches sum
    // directly); O(d) for the undirected case
    // (see `undirected_degree_at()`'s doc comment for why that one can't
    // stay O(1) everywhere the way the old doubled-self-loop convention
    // let it). Must stay in exact agreement with `neighbors()`'s semantics
    // above (mode handling, panic on an invalid mode, self-loop counting)
    // -- see this file's tests. Not an R method of its own: R queries go
    // through `degrees()`.
    fn degree(&self, node: i32, mode: &str) -> i32 {
        let idx = self.node_index(node);
        if !self.directed {
            return self.undirected_degree_at(idx);
        }
        match mode {
            "out" => self.out_degree_at(idx),
            "in" => self.in_degree_at(idx),
            "all" => self.out_degree_at(idx) + self.in_degree_at(idx),
            _ => panic!("`mode` must be one of \"out\", \"in\", \"all\", not \"{mode}\""),
        }
    }

    // The 0-based index of 1-based position `node`, panicking (an R error,
    // through extendr) on a position outside the graph rather than indexing
    // out of bounds, or wrapping 0 or below to a huge `usize`. The R query
    // functions check their `i` first; this is the backstop.
    fn node_index(&self, node: i32) -> usize {
        let n = self.n_nodes();
        if node < 1 || node > n {
            panic!("`node` must be a node position between 1 and {n}, not {node}.");
        }
        (node - 1) as usize
    }

    // All edges as 1-based `(from, to)` pairs, in construction/edge-id
    // order -- the one place both `edge_endpoints()` and
    // `induced_subgraph()` read topology from, so every `Repr` variant
    // needs to get this right exactly once (`_dev/petgraph_data_types.md`
    // S4's edge-identity item) rather than per call site; `edge_at()`
    // below reads a single edge from the same storage.
    fn edge_list(&self) -> (Vec<i32>, Vec<i32>) {
        match &self.repr {
            Repr::General(g) => g.graph.edge_list(),
            Repr::Dense(d) => (d.from.clone(), d.to.clone()),
            Repr::Csr(c) => (c.from.clone(), c.to.clone()),
        }
    }

    // The 1-based `(from, to)` of 0-based edge id `i` (in range), as
    // `edge_list()` would give it, without materialising every edge.
    fn edge_at(&self, i: usize) -> (i32, i32) {
        match &self.repr {
            Repr::General(g) => g.graph.edge_at(i),
            Repr::Dense(d) => (d.from[i], d.to[i]),
            Repr::Csr(c) => (c.from[i], c.to[i]),
        }
    }

    // The symmetric neighbour set for a *directed* graph's `"all"` mode
    // ("out" edges then "in" edges, concatenated): a self-loop is one arc
    // `a -> a`, which shows up once via `out_neighbors_at()` and once more
    // via `in_neighbors_at()`, so it's counted (and listed) twice here --
    // deliberate and unchanged, `directed_self_loop_counts_once_per_direction`
    // pins this down. This is NOT the undirected convention (see
    // `undirected_neighbors_at()` below, used instead whenever
    // `!self.directed`): those two cases used to share this one function,
    // back when a self-loop counted twice everywhere; they no longer do
    // (`undirected_neighbors_at()`'s doc comment has the current
    // convention), so they're separate functions now.
    fn symmetric_neighbors(&self, idx: usize) -> Vec<i32> {
        let mut v = self.out_neighbors_at(idx);
        v.extend(self.in_neighbors_at(idx));
        v
    }

    // The undirected neighbour set: one entry per incident edge, a
    // self-loop included exactly *once* -- matching what every backing
    // petgraph type already does natively for an undirected query, rather
    // than the doubled count `symmetric_neighbors()` above still gives a
    // directed graph's `"all"` mode. Per-variant because the native
    // primitive to delegate to differs: `Graph::neighbors_undirected()`
    // explicitly skips a self-loop on its incoming pass (confirmed from
    // petgraph's own source, see `GeneralGraph::undirected_neighbors()`), an
    // `Undirected`-typed `MatrixGraph` never double-counts one in the first
    // place (one triangular bit, one appearance), and `Csr` -- always
    // `Directed`-typed internally, with no native undirected view at all
    // (`CsrData`'s doc comment) -- needs the one explicit correction of the
    // three, in `CsrData::undirected_neighbors()`.
    fn undirected_neighbors_at(&self, idx: usize) -> Vec<i32> {
        match &self.repr {
            Repr::General(g) => g.graph.undirected_neighbors(idx),
            Repr::Dense(d) => d.matrix.undirected_neighbors(idx),
            Repr::Csr(c) => c.undirected_neighbors(idx),
        }
    }

    // O(1) companion to `undirected_neighbors_at()` where the backing type
    // allows it (`Repr::General` via `GeneralData::self_loops`,
    // `Repr::Csr` via `CsrData::undirected_degree()`'s own O(1) formula);
    // `Repr::Dense` has no cached degree to correct in the first place
    // (`DenseData`'s doc comment) and so falls back to counting
    // `undirected_neighbors_at()`'s own result, O(d).
    fn undirected_degree_at(&self, idx: usize) -> i32 {
        match &self.repr {
            Repr::General(g) => g.out_degree[idx] + g.in_degree[idx] - g.self_loops[idx],
            Repr::Dense(_) => self.undirected_neighbors_at(idx).len() as i32,
            Repr::Csr(c) => c.undirected_degree(idx),
        }
    }

    fn out_neighbors_at(&self, idx: usize) -> Vec<i32> {
        match &self.repr {
            Repr::General(g) => g.graph.directed_neighbors(idx, Direction::Outgoing),
            Repr::Dense(d) => d.matrix.directed_neighbors(idx, Direction::Outgoing),
            Repr::Csr(c) => c.out_neighbors(idx),
        }
    }

    fn in_neighbors_at(&self, idx: usize) -> Vec<i32> {
        match &self.repr {
            Repr::General(g) => g.graph.directed_neighbors(idx, Direction::Incoming),
            Repr::Dense(d) => d.matrix.directed_neighbors(idx, Direction::Incoming),
            Repr::Csr(c) => c.in_neighbors(idx),
        }
    }

    fn out_degree_at(&self, idx: usize) -> i32 {
        match &self.repr {
            Repr::General(g) => g.out_degree[idx],
            Repr::Dense(d) => d.out_degree[idx],
            Repr::Csr(c) => c.out_degree(idx),
        }
    }

    fn in_degree_at(&self, idx: usize) -> i32 {
        match &self.repr {
            Repr::General(g) => g.in_degree[idx],
            Repr::Dense(d) => d.in_degree[idx],
            Repr::Csr(c) => c.in_degree(idx),
        }
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod graphvec;
    impl GraphBackend;
    fn graphvec_backend_set_source;
    fn graphvec_backend_revive;
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test-only shape predicates, mirroring `repr_name()`'s match arms but
    // as plain booleans for terser assertions below. Deliberately NOT
    // `#[extendr]` (this `impl` block lives inside `#[cfg(test)]`, so it
    // never compiles into the R-facing build) -- these used to be public
    // `is_tree()`/`is_dense()`/`is_csr()` methods on `GraphBackend` itself,
    // which meant every one was permanent, exported R API (extendr
    // generates an R wrapper per method in a `#[extendr] impl` block); one
    // stable `repr_name()` replaced them for any real R/cross-language use
    // (see its doc comment), and this crate-internal impl keeps the terser
    // `g.is_dense()` spelling for this file's own tests without paying that
    // cost again per variant.
    impl GraphBackend {
        fn is_dense(&self) -> bool {
            matches!(self.repr, Repr::Dense(_))
        }

        fn is_csr(&self) -> bool {
            matches!(self.repr, Repr::Csr(_))
        }
    }

    // Every graph gets its own identity, even when built from identical
    // input, so two separately built graphs never compare as the same.
    #[test]
    fn uid_is_unique_per_graph() {
        let a = GraphBackend::new(2, vec![1], vec![2], true).unwrap();
        let b = GraphBackend::new(2, vec![1], vec![2], true).unwrap();
        assert_ne!(a.uid(), b.uid());
        assert_eq!(a.uid(), a.uid());
        assert!(a.uid() < 2f64.powi(53) && a.uid() == a.uid().trunc());
    }

    // An endpoint outside `1..=n` (0, negative, `NA` as `i32::MIN`, or past
    // the last node) is an error, not a panic or a huge allocation.
    #[test]
    fn new_rejects_out_of_range_endpoints() {
        for bad in [0, -1, i32::MIN, 3] {
            assert!(GraphBackend::new(2, vec![bad], vec![1], true).is_err());
            assert!(GraphBackend::new(2, vec![1], vec![bad], false).is_err());
        }
        assert!(GraphBackend::new(2, vec![1, 2], vec![1], true).is_err());
        assert!(GraphBackend::new(2, vec![1, 2], vec![2, 2], true).is_ok());
    }

    // A `node_vec`/`edge_vec` sliced with `x[i]` relies on `edge_endpoints()`
    // enumerating edges in construction order -- confirm petgraph's
    // `EdgeIndex` really is stable, contiguous insertion order for a graph
    // that never removes an edge, rather than assuming it.
    #[test]
    fn edge_endpoints_preserve_construction_order() {
        test! {
            // Deliberately not sorted by either endpoint, so an accidental
            // internal reordering (e.g. by node) would be caught. Node 1 has
            // out-degree 2 here, picking Repr::General or Repr::Dense
            // (immaterial -- edge_list() is representation-independent).
            let from = vec![3, 1, 2, 1];
            let to = vec![1, 2, 3, 3];
            let g = GraphBackend::new(3, from.clone(), to.clone(), true).unwrap();
            let ends = g.edge_endpoints(Nullable::Null);
            let got_from: Vec<i32> = ends.dollar("from").unwrap().as_integer_vector().unwrap();
            let got_to: Vec<i32> = ends.dollar("to").unwrap().as_integer_vector().unwrap();
            assert_eq!(got_from, from);
            assert_eq!(got_to, to);
        }
    }

    // The undirected-degree convention a self-loop must satisfy: a loop
    // counts *once* (matching what every backing petgraph type already does
    // natively for an undirected query -- `GeneralData`'s doc comment).
    // Confirm `neighbors_undirected()` actually produces this on the
    // always-directed internal graph, rather than assuming it.
    #[test]
    fn self_loop_counts_once_in_undirected_degree() {
        test! {
            // Node 1 has a self-loop and one ordinary edge to node 2.
            let g = GraphBackend::new(2, vec![1, 1], vec![1, 2], false).unwrap();
            assert_eq!(g.degree(1, "all"), 2); // loop (1) + edge to 2 (1)
            assert_eq!(g.degree(2, "all"), 1);
            let mut ns = g.neighbors(1, "all");
            ns.sort();
            assert_eq!(ns, vec![1, 2]); // order isn't a contract, only the multiset is
        }
    }

    #[test]
    fn directed_self_loop_counts_once_per_direction() {
        test! {
            let g = GraphBackend::new(1, vec![1], vec![1], true).unwrap();
            assert_eq!(g.degree(1, "out"), 1);
            assert_eq!(g.degree(1, "in"), 1);
            assert_eq!(g.degree(1, "all"), 2);
        }
    }

    // `degree()` stopped being `neighbors().len()` when it moved to a
    // construction-time cache -- pin the two down as agreeing on every
    // mode, directed and undirected, so a future edit to one path can't
    // silently drift from the other.
    #[test]
    fn degree_matches_neighbors_len_every_mode() {
        test! {
            let gd = GraphBackend::new(3, vec![1, 1, 2], vec![1, 2, 3], true).unwrap();
            for node in 1..=3 {
                for mode in ["out", "in", "all"] {
                    assert_eq!(
                        gd.degree(node, mode),
                        gd.neighbors(node, mode).len() as i32
                    );
                }
            }

            let gu = GraphBackend::new(3, vec![1, 1, 2], vec![1, 2, 3], false).unwrap();
            for node in 1..=3 {
                // mode is ignored when undirected -- any value must agree.
                assert_eq!(gu.degree(node, "all"), gu.neighbors(node, "all").len() as i32);
            }
        }
    }

    // The vectorised methods R calls (`degrees()`, `neighbors_many()`,
    // `edge_endpoints(ids)`) are defined by the per-node/per-edge ones --
    // pin them as agreeing on every representation, mode and direction,
    // with repeated query positions and an `NA` edge id.
    #[test]
    fn vectorised_queries_match_per_node_ones() {
        test! {
            let shapes: [(i32, Vec<i32>, Vec<i32>, &str); 3] = [
                (3, vec![1, 1, 2, 3], vec![1, 2, 3, 1], "dense"),
                (8, vec![1, 1, 2, 5], vec![1, 2, 3, 1], "csr"),
                (3, vec![1, 1, 1, 3], vec![1, 2, 2, 1], "general"),
            ];
            for (n, from, to, repr) in shapes {
                for directed in [true, false] {
                    let g = GraphBackend::new(n, from.clone(), to.clone(), directed).unwrap();
                    assert_eq!(g.repr_name(), repr);
                    for mode in ["out", "in", "all"] {
                        let expected: Vec<i32> = (1..=n).map(|v| g.degree(v, mode)).collect();
                        assert_eq!(g.degrees(mode), expected);

                        let nodes = vec![2, 1, 2, n];
                        let res = g.neighbors_many(nodes.clone(), mode);
                        let ptr = res.dollar("ptr").unwrap().as_integer_vector().unwrap();
                        let idx = res.dollar("idx").unwrap().as_integer_vector().unwrap();
                        assert_eq!(ptr.len(), nodes.len() + 1);
                        for (k, &v) in nodes.iter().enumerate() {
                            let mut ns = g.neighbors(v, mode);
                            ns.sort();
                            assert_eq!(idx[ptr[k] as usize..ptr[k + 1] as usize], ns[..]);
                        }
                    }

                    let ids = vec![4, i32::MIN, 1, 4];
                    let ends = g.edge_endpoints(Nullable::NotNull(ids));
                    let got_from = ends.dollar("from").unwrap().as_integer_vector().unwrap();
                    let got_to = ends.dollar("to").unwrap().as_integer_vector().unwrap();
                    assert_eq!(got_from, vec![from[3], i32::MIN, from[0], from[3]]);
                    assert_eq!(got_to, vec![to[3], i32::MIN, to[0], to[3]]);
                }
            }
        }
    }

    #[test]
    #[should_panic(expected = "between 1 and 4")]
    fn edge_endpoints_rejects_out_of_range_ids() {
        let g = GraphBackend::new(2, vec![1, 1, 2, 2], vec![1, 2, 1, 2], true).unwrap();
        g.edge_endpoints(Nullable::NotNull(vec![1, 5]));
    }

    #[test]
    fn has_edge_checks_both_orientations_when_undirected() {
        test! {
            let g = GraphBackend::new(2, vec![1], vec![2], false).unwrap();
            assert!(g.has_edge(1, 2));
            assert!(g.has_edge(2, 1));

            let gd = GraphBackend::new(2, vec![1], vec![2], true).unwrap();
            assert!(gd.has_edge(1, 2));
            assert!(!gd.has_edge(2, 1));
        }
    }

    #[test]
    fn induced_subgraph_drops_dangling_and_clones_replicated() {
        test! {
            // Triangle 1-2-3 (edges 1->2, 2->3, 3->1); new nodes <- old 1, 1, 2
            // (node 1 replicated, node 3 dropped): edge 1->2 clones once per
            // replica of 1, edges touching 3 vanish. Density selection then
            // picks General or Dense, immaterial here: induced_subgraph() is
            // representation-independent, see its doc comment.
            let g = GraphBackend::new(3, vec![1, 2, 3], vec![2, 3, 1], true).unwrap();
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
            let g = GraphBackend::new(2, vec![1], vec![2], true).unwrap();
            // New node 1 has no source (sentinel 0); new node 2 <- old node 2.
            let remap = g.induced_subgraph(vec![0, 2]);
            let from: Vec<i32> = remap.dollar("from").unwrap().as_integer_vector().unwrap();
            assert!(from.is_empty());
        }
    }

    // -- Repr::General, undirected storage ---------------------------------
    //
    // `Repr::General` is only ever reached by a graph with a duplicate edge
    // (`GraphBackend::new()`'s selection order: dense, then CSR, then this
    // as the multigraph-capable fallback), so every input below supplies one
    // deliberately. An undirected such graph is now stored as
    // `Graph<(), (), Undirected>` rather than a `DiGraph` -- see
    // `GeneralGraph`'s doc comment -- and these pin the R-visible semantics
    // that must survive the swap unchanged.

    // The self-loop convention `d11dfe9` established, over the undirected
    // monomorphisation specifically: `neighbors_undirected()`'s self-loop
    // skip is `Ty`-independent (`Neighbors::next()`, petgraph 0.8.3
    // `graph_impl/mod.rs:1893-1912`), and `undirected_degree_at()`'s
    // `out + in - self_loops` cache formula must still agree with it. n=3
    // undirected: a self-loop on node 1 plus the edge (1,2) supplied twice.
    // Density = 3/(3 choose 2) = 1.0 > 0.3, but the duplicate rules out both
    // Dense and Csr -- picks General.
    #[test]
    fn general_undirected_self_loop_counts_once_in_undirected_degree() {
        test! {
            let g = GraphBackend::new(3, vec![1, 1, 1], vec![1, 2, 2], false).unwrap();
            assert!(!g.is_dense());
            assert!(!g.is_csr());
            // loop (1) + two parallel edges to node 2 (2)
            assert_eq!(g.degree(1, "all"), 3);
            assert_eq!(g.degree(2, "all"), 2);
            assert_eq!(g.degree(3, "all"), 0);
            let mut ns = g.neighbors(1, "all");
            ns.sort();
            assert_eq!(ns, vec![1, 2, 2]); // order isn't a contract, the multiset is
            for node in 1..=3 {
                assert_eq!(g.degree(node, "all"), g.neighbors(node, "all").len() as i32);
            }
        }
    }

    // `has_edge()` used to try the reverse arc itself for an undirected
    // `Repr::General`; the `Undirected` storage now answers both ways
    // natively via `Graph::find_edge()`'s own undirected branch
    // (`graph_impl/mod.rs:1062-1070`). Same observable behaviour either way
    // -- which is the point of this test.
    #[test]
    fn general_undirected_has_edge_checks_both_orientations() {
        test! {
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 2, 3], false).unwrap();
            assert!(!g.is_dense());
            assert!(!g.is_csr());
            assert!(g.has_edge(1, 2));
            assert!(g.has_edge(2, 1));
            assert!(g.has_edge(3, 2));
            assert!(!g.has_edge(1, 3));
        }
    }

    // Edge identity over the `Undirected` monomorphisation: `EdgeIndex` is
    // still append-only insertion order, and `edge_endpoints()` still
    // reports each edge's endpoints in the order they were supplied (not
    // canonicalised into some (min, max) form, which an undirected store
    // could plausibly have done). Deliberately scrambled, with a duplicate
    // to force General.
    #[test]
    fn general_undirected_edge_endpoints_preserve_construction_order() {
        test! {
            let from = vec![3, 1, 2, 1, 3];
            let to = vec![1, 2, 3, 2, 1];
            let g = GraphBackend::new(3, from.clone(), to.clone(), false).unwrap();
            assert!(!g.is_dense());
            assert!(!g.is_csr());
            let ends = g.edge_endpoints(Nullable::Null);
            let got_from: Vec<i32> = ends.dollar("from").unwrap().as_integer_vector().unwrap();
            let got_to: Vec<i32> = ends.dollar("to").unwrap().as_integer_vector().unwrap();
            assert_eq!(got_from, from);
            assert_eq!(got_to, to);
        }
    }

    // -- Repr::Dense -------------------------------------------------------
    //
    // Every S4 contract item (`_dev/petgraph_data_types.md`), covered for
    // Repr::Dense specifically -- none of it is inherited by assumption from
    // Repr::General's tests above.

    // Node identity: dense 1..N, stable, representation-independent -- holds
    // trivially here too, stated plainly per S4's own instruction rather
    // than skipped. A 4-node complete undirected graph:
    // density = 6 / (4 choose 2) = 6/6 = 1.0, comfortably past
    // DENSE_THRESHOLD, no duplicate edges -- picks Dense.
    #[test]
    fn dense_node_identity_is_dense_and_stable() {
        test! {
            let from = vec![1, 1, 1, 2, 2, 3];
            let to = vec![2, 3, 4, 3, 4, 4];
            let g = GraphBackend::new(4, from, to, false).unwrap();
            assert!(g.is_dense());
            assert_eq!(g.n_nodes(), 4);
            for node in 1..=4 {
                // every position answers queries independently of the others
                let _ = g.degree(node, "all");
                let _ = g.neighbors(node, "all");
            }
        }
    }

    // Edge identity / enumeration order -- the one real risk per S4.
    // `MatrixGraph` has no insertion-order edge enumeration of its own at
    // all (presence is keyed purely by the (node, node) pair -- see
    // `DenseData`'s doc comment), unlike Repr::General's insertion-ordered
    // `EdgeIndex`. `DenseData::from`/`to` exist specifically to answer this.
    // Mirrors `edge_endpoints_preserve_construction_order`, deliberately
    // scrambled (not sorted by either endpoint, and not node-order either)
    // so an accidental reordering would be caught. n=4 undirected, 3 edges:
    // density = 3 / (4 choose 2) = 3/6 = 0.5 > 0.3, no duplicate unordered
    // pairs -- picks Dense.
    #[test]
    fn dense_edge_endpoints_preserve_construction_order() {
        test! {
            let from = vec![3, 1, 4];
            let to = vec![1, 4, 2];
            let g = GraphBackend::new(4, from.clone(), to.clone(), false).unwrap();
            assert!(g.is_dense());
            let ends = g.edge_endpoints(Nullable::Null);
            let got_from: Vec<i32> = ends.dollar("from").unwrap().as_integer_vector().unwrap();
            let got_to: Vec<i32> = ends.dollar("to").unwrap().as_integer_vector().unwrap();
            assert_eq!(got_from, from);
            assert_eq!(got_to, to);
        }
    }

    // Self-loop/undirected-degree convention: a loop counts *once*, which
    // `Repr::Dense` gets natively for free -- an `Undirected`-typed
    // `MatrixGraph` stores one triangular bit per unordered pair, so
    // `matrix.neighbors()` (what `DenseMatrix::undirected_neighbors()`
    // delegates straight to, `DenseData`'s doc comment) already reports a
    // self-loop once, with no doubling to correct for the way
    // `Repr::General`/`Repr::Csr` need to. n=2 undirected, self-loop on
    // node 1 plus an ordinary edge to node 2 -- density = 2 / (2 choose 2)
    // = 2/1 = 2.0 > 0.3, no duplicate pairs ((1,1) and (1,2) are distinct
    // canonical keys) -- picks Dense.
    #[test]
    fn dense_self_loop_counts_once_in_undirected_degree() {
        test! {
            let g = GraphBackend::new(2, vec![1, 1], vec![1, 2], false).unwrap();
            assert!(g.is_dense());
            assert_eq!(g.degree(1, "all"), 2); // loop (1) + edge to 2 (1)
            assert_eq!(g.degree(2, "all"), 1);
            let mut ns = g.neighbors(1, "all");
            ns.sort();
            assert_eq!(ns, vec![1, 2]); // order isn't a contract, only the multiset is
        }
    }

    // `mode` semantics must match Repr::General exactly: "out"/"in"/"all"
    // for directed, invalid values panic identically. n=3 directed, edges
    // 1->2, 1->3, 2->3: density = 3 / (3 choose 2) = 3/3 = 1.0 > 0.3, no
    // duplicate ordered pairs -- picks Dense.
    #[test]
    fn dense_mode_semantics_match_other_variants() {
        test! {
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 3, 3], true).unwrap();
            assert!(g.is_dense());
            assert_eq!(g.degree(1, "out"), 2);
            assert_eq!(g.degree(1, "in"), 0);
            assert_eq!(g.degree(1, "all"), 2);
            assert_eq!(g.degree(2, "out"), 1);
            assert_eq!(g.degree(2, "in"), 1);
            assert_eq!(g.degree(2, "all"), 2);
            assert_eq!(g.degree(3, "out"), 0);
            assert_eq!(g.degree(3, "in"), 2);
            assert_eq!(g.degree(3, "all"), 2);
            for node in 1..=3 {
                for mode in ["out", "in", "all"] {
                    assert_eq!(
                        g.degree(node, mode),
                        g.neighbors(node, mode).len() as i32
                    );
                }
            }
        }
    }

    #[test]
    #[should_panic(expected = "`mode` must be one of \"out\", \"in\", \"all\", not \"sideways\"")]
    fn dense_neighbors_invalid_mode_panics_like_other_variants() {
        test! {
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 3, 3], true).unwrap();
            assert!(g.is_dense());
            g.neighbors(1, "sideways");
        }
    }

    #[test]
    #[should_panic(expected = "`mode` must be one of \"out\", \"in\", \"all\", not \"sideways\"")]
    fn dense_degree_invalid_mode_panics_like_other_variants() {
        test! {
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 3, 3], true).unwrap();
            assert!(g.is_dense());
            g.degree(1, "sideways");
        }
    }

    // induced_subgraph()'s replication/drop logic, mirroring
    // `induced_subgraph_drops_dangling_and_clones_replicated` /
    // `induced_subgraph_treats_zero_as_no_source` above, for a Dense-shaped
    // input, and confirming (as those tests' own comments already establish
    // for General) that induced_subgraph() never constructs a GraphBackend
    // itself -- it returns plain lists, representation-independent (see
    // `induced_subgraph()`'s doc comment and `edge_list()`, which
    // `Repr::Dense` participates in like every other variant).
    #[test]
    fn dense_induced_subgraph_drops_dangling_and_clones_replicated() {
        test! {
            // Triangle 1->2->3->1 (directed cycle); density = 3 / (3 choose
            // 2) = 3/3 = 1.0 > 0.3, no duplicates -- picks Dense.
            let g = GraphBackend::new(3, vec![1, 2, 3], vec![2, 3, 1], true).unwrap();
            assert!(g.is_dense());
            // New nodes <- old 1, 1, 2 (node 1 replicated, node 3 dropped):
            // edge 1->2 clones once per replica of 1, edges touching 3 vanish.
            let remap = g.induced_subgraph(vec![1, 1, 2]);
            let from: Vec<i32> = remap.dollar("from").unwrap().as_integer_vector().unwrap();
            let to: Vec<i32> = remap.dollar("to").unwrap().as_integer_vector().unwrap();
            let source_edge: Vec<i32> = remap
                .dollar("source_edge")
                .unwrap()
                .as_integer_vector()
                .unwrap();
            assert_eq!(from, vec![1, 2]);
            assert_eq!(to, vec![3, 3]);
            assert_eq!(source_edge, vec![1, 1]);
        }
    }

    #[test]
    fn dense_induced_subgraph_treats_zero_as_no_source() {
        test! {
            // n=2 undirected; density = 1/(2 choose 2) = 1/1 = 1.0 > 0.3,
            // no duplicates -- picks Dense.
            let g = GraphBackend::new(2, vec![1], vec![2], false).unwrap();
            assert!(g.is_dense());
            // New node 1 has no source (sentinel 0); new node 2 <- old node 2.
            let remap = g.induced_subgraph(vec![0, 2]);
            let from: Vec<i32> = remap.dollar("from").unwrap().as_integer_vector().unwrap();
            assert!(from.is_empty());
        }
    }

    // -- Dense selection logic --------------------------------------------

    #[test]
    fn dense_enough_duplicate_free_graph_picks_dense_repr() {
        test! {
            // Complete undirected triangle: density = 3/(3 choose 2) = 1.0.
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 3, 3], false).unwrap();
            assert!(g.is_dense());
        }
    }

    // Below DENSE_THRESHOLD (0.3): a 10-node directed cycle. M=10, N choose
    // 2 = 45, density = 10/45 ~= 0.222, below threshold, and no duplicate
    // edges -- picks Csr, not Dense.
    //
    // Renamed from `below_density_threshold_picks_general_repr` (this input
    // used to be General's example of the "otherwise" catch-all before
    // `Repr::Csr` existed): with the duplicate-free/duplicate split this
    // file's `Repr::Csr` deviation introduces (`CsrData`'s doc comment,
    // `GraphBackend::new()`'s selection-site comment), `Repr::General` is
    // now reached only when a graph *has* a duplicate edge -- see
    // `duplicate_edge_despite_density_falls_back_to_general_repr` below for
    // that case, and the "-- CSR selection logic --" tests further down for
    // the CSR/General split itself.
    #[test]
    fn below_density_threshold_picks_csr_repr() {
        test! {
            let from: Vec<i32> = (1..=10).collect();
            let to: Vec<i32> = (2..=10).chain(std::iter::once(1)).collect();
            let g = GraphBackend::new(10, from, to, true).unwrap();
            assert!(!g.is_dense());
            assert!(g.is_csr());
        }
    }

    // A duplicate edge despite being dense: falls back to Repr::General
    // regardless of density, since neither Repr::Dense nor (this file's
    // `Repr::Csr` addition) Repr::Csr can physically hold a multigraph
    // (`MatrixGraph::add_edge()` panics on a repeated pair, `Csr::add_edge()`
    // silently no-ops on one -- `_dev/petgraph_data_types.md` S4 item 3 and
    // `CsrData`'s doc comment respectively). n=3 undirected, edge (1,2)
    // supplied twice plus (2,3): density = 3/(3 choose 2) = 1.0 > 0.3 --
    // would otherwise qualify for Dense, but the duplicate gate forces
    // General instead (and would still force General even below the density
    // threshold, since the same gate applies to Csr -- see the "-- CSR
    // selection logic --" tests further down). Compare
    // `dense_enough_duplicate_free_graph_picks_dense_repr` above, same shape
    // minus the duplicate, which does pick Dense.
    #[test]
    fn duplicate_edge_despite_density_falls_back_to_general_repr() {
        test! {
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 2, 3], false).unwrap();
            assert!(!g.is_dense());
            assert!(!g.is_csr());
            // Still a perfectly ordinary graph otherwise -- General answers
            // has_edge()/degree() for it exactly as it would for any input.
            assert!(g.has_edge(1, 2));
            assert_eq!(g.degree(1, "all"), 2); // two (1,2) edges, both counted
        }
    }

    // -- Repr::Csr ---------------------------------------------------------
    //
    // Every S4 contract item (`_dev/petgraph_data_types.md`), covered for
    // Repr::Csr specifically -- none of it is inherited by assumption from
    // any other variant's tests above. See `CsrData`'s doc comment and
    // `GraphBackend::new()`'s selection-site comment for this file's
    // deliberate deviation from `_dev/DATA.md` S6 item 4's literal wording
    // (Repr::Csr as a new, narrower bucket alongside Repr::General, not a
    // replacement for it).

    // Node identity: dense 1..N, stable, representation-independent -- holds
    // trivially here too, stated plainly per S4's own instruction rather
    // than skipped. n=8 directed path 1->2->3->4->5->6 plus 1->3; density =
    // 6/(8 choose 2) = 6/28 ~= 0.214, below DENSE_THRESHOLD, no duplicate
    // edges -- picks Csr.
    #[test]
    fn csr_node_identity_is_dense_and_stable() {
        test! {
            let from = vec![1, 2, 3, 4, 5, 1];
            let to = vec![2, 3, 4, 5, 6, 3];
            let g = GraphBackend::new(8, from, to, true).unwrap();
            assert!(!g.is_dense());
            assert!(g.is_csr());
            assert_eq!(g.n_nodes(), 8);
            for node in 1..=8 {
                // every position answers queries independently of the others
                let _ = g.degree(node, "all");
                let _ = g.neighbors(node, "all");
            }
        }
    }

    // Edge identity / enumeration order -- the one real risk per S4, and the
    // risk this variant actually has for real (unlike Repr::Dense, which
    // has no separate storage order to permute against in the first place):
    // `Csr`'s native storage is source-then-target sorted, a real
    // permutation of arbitrary construction order. `CsrData::from`/`to`
    // exist specifically to answer this -- `csr` itself (built from a
    // *separately sorted clone*, `build_csr()`) is never consulted by
    // `edge_endpoints()`/`induced_subgraph()`. Deliberately scrambled input
    // (not sorted by either endpoint, and not node-order either) so an
    // accidental fall-through to `csr`'s own storage order would be caught.
    // n=8 directed, 6 edges (same shape as
    // `csr_node_identity_is_dense_and_stable`, deliberately reordered and
    // duplicate-free) -- density = 6/28 ~= 0.214 < 0.3, picks Csr.
    #[test]
    fn csr_edge_endpoints_preserve_construction_order() {
        test! {
            let from = vec![5, 1, 4, 1, 2, 3];
            let to = vec![6, 3, 5, 2, 3, 4];
            let g = GraphBackend::new(8, from.clone(), to.clone(), true).unwrap();
            assert!(g.is_csr());
            let ends = g.edge_endpoints(Nullable::Null);
            let got_from: Vec<i32> = ends.dollar("from").unwrap().as_integer_vector().unwrap();
            let got_to: Vec<i32> = ends.dollar("to").unwrap().as_integer_vector().unwrap();
            assert_eq!(got_from, from);
            assert_eq!(got_to, to);
        }
    }

    // Self-loop/undirected-degree convention: a loop counts *once*.
    // `CsrData`'s doc comment explains why this is the one variant that
    // can't just delegate to a native "undirected" primitive the way
    // `Repr::General`/`Repr::Dense` can (`Csr` has no such view at all) --
    // `undirected_neighbors()`/`undirected_degree()` correct for the raw
    // out+in doubling explicitly, and this test is what confirms that
    // correction actually lands, not just the arithmetic. n=6 undirected,
    // self-loop on node 1 plus an ordinary edge to node 2 (nodes 3-6
    // isolated, just to keep density below threshold): density = 2/(6
    // choose 2) = 2/15 ~= 0.133 < 0.3, no duplicate pairs ((1,1) and (1,2)
    // are distinct canonical keys) -- picks Csr.
    #[test]
    fn csr_self_loop_counts_once_in_undirected_degree() {
        test! {
            let g = GraphBackend::new(6, vec![1, 1], vec![1, 2], false).unwrap();
            assert!(g.is_csr());
            assert_eq!(g.degree(1, "all"), 2); // loop (1) + edge to 2 (1)
            assert_eq!(g.degree(2, "all"), 1);
            let mut ns = g.neighbors(1, "all");
            ns.sort();
            assert_eq!(ns, vec![1, 2]); // order isn't a contract, only the multiset is
        }
    }

    // `mode` semantics must match every other variant exactly: "out"/"in"/
    // "all" for directed, invalid values panic identically. n=8 directed,
    // 6 edges (same shape as `csr_node_identity_is_dense_and_stable`) --
    // density = 6/28 ~= 0.214 < 0.3, duplicate-free -- picks Csr.
    #[test]
    fn csr_mode_semantics_match_other_variants() {
        test! {
            let from = vec![1, 2, 3, 4, 5, 1];
            let to = vec![2, 3, 4, 5, 6, 3];
            let g = GraphBackend::new(8, from, to, true).unwrap();
            assert!(g.is_csr());
            assert_eq!(g.degree(1, "out"), 2); // 1->2, 1->3
            assert_eq!(g.degree(1, "in"), 0);
            assert_eq!(g.degree(1, "all"), 2);
            assert_eq!(g.degree(3, "out"), 1); // 3->4
            assert_eq!(g.degree(3, "in"), 2); // 2->3, 1->3
            assert_eq!(g.degree(3, "all"), 3);
            for node in 1..=8 {
                for mode in ["out", "in", "all"] {
                    assert_eq!(
                        g.degree(node, mode),
                        g.neighbors(node, mode).len() as i32
                    );
                }
            }
        }
    }

    #[test]
    #[should_panic(expected = "`mode` must be one of \"out\", \"in\", \"all\", not \"sideways\"")]
    fn csr_neighbors_invalid_mode_panics_like_other_variants() {
        test! {
            let from = vec![1, 2, 3, 4, 5, 1];
            let to = vec![2, 3, 4, 5, 6, 3];
            let g = GraphBackend::new(8, from, to, true).unwrap();
            assert!(g.is_csr());
            g.neighbors(1, "sideways");
        }
    }

    #[test]
    #[should_panic(expected = "`mode` must be one of \"out\", \"in\", \"all\", not \"sideways\"")]
    fn csr_degree_invalid_mode_panics_like_other_variants() {
        test! {
            let from = vec![1, 2, 3, 4, 5, 1];
            let to = vec![2, 3, 4, 5, 6, 3];
            let g = GraphBackend::new(8, from, to, true).unwrap();
            assert!(g.is_csr());
            g.degree(1, "sideways");
        }
    }

    // induced_subgraph()'s replication/drop logic, mirroring
    // `induced_subgraph_drops_dangling_and_clones_replicated` /
    // `induced_subgraph_treats_zero_as_no_source` above, for a Csr-shaped
    // input, and confirming (as those tests' own comments already establish
    // for General/Dense) that induced_subgraph() never constructs a
    // GraphBackend itself -- it returns plain lists, representation-
    // independent (see `induced_subgraph()`'s doc comment and `edge_list()`,
    // which `Repr::Csr` participates in like every other variant).
    #[test]
    fn csr_induced_subgraph_drops_dangling_and_clones_replicated() {
        test! {
            // 8-node directed path 1->2->3->4->5->6 plus 1->3; density =
            // 6/28 ~= 0.214 < 0.3, duplicate-free -- picks Csr.
            let from = vec![1, 2, 3, 4, 5, 1];
            let to = vec![2, 3, 4, 5, 6, 3];
            let g = GraphBackend::new(8, from, to, true).unwrap();
            assert!(g.is_csr());
            // New nodes <- old 1, 1, 2 (node 1 replicated, node 3 dropped):
            // edge 1->2 clones once per replica of 1; edges touching 3
            // (1->3, 3->4) vanish; edges among untouched old nodes (4,5,6,7,8)
            // vanish too since none of them have a new position.
            let remap = g.induced_subgraph(vec![1, 1, 2]);
            let from_out: Vec<i32> = remap.dollar("from").unwrap().as_integer_vector().unwrap();
            let to_out: Vec<i32> = remap.dollar("to").unwrap().as_integer_vector().unwrap();
            let source_edge: Vec<i32> = remap
                .dollar("source_edge")
                .unwrap()
                .as_integer_vector()
                .unwrap();
            // Old edge 1 (1->2) survives once per (new-from replica, new-to)
            // combo: old 1 maps to new {1, 2}, old 2 maps to new {3} -> two
            // clones.
            assert_eq!(from_out, vec![1, 2]);
            assert_eq!(to_out, vec![3, 3]);
            assert_eq!(source_edge, vec![1, 1]);
        }
    }

    #[test]
    fn csr_induced_subgraph_treats_zero_as_no_source() {
        test! {
            // n=6 undirected; density = 1/(6 choose 2)
            // = 1/15 ~= 0.067 < 0.3, no duplicates -- picks Csr.
            let g = GraphBackend::new(6, vec![1], vec![2], false).unwrap();
            assert!(g.is_csr());
            // New node 1 has no source (sentinel 0); new node 2 <- old node 2.
            let remap = g.induced_subgraph(vec![0, 2]);
            let from: Vec<i32> = remap.dollar("from").unwrap().as_integer_vector().unwrap();
            assert!(from.is_empty());
        }
    }

    // -- CSR selection logic ------------------------------------------------

    // A duplicate-free, below-density-threshold graph picks Repr::Csr --
    // same shape/reasoning as `below_density_threshold_picks_csr_repr`
    // above, restated here alongside the rest of the selection-order tests.
    #[test]
    fn duplicate_free_below_density_threshold_picks_csr_repr() {
        test! {
            let from = vec![1, 2, 3, 4, 5, 1];
            let to = vec![2, 3, 4, 5, 6, 3];
            let g = GraphBackend::new(8, from, to, true).unwrap();
            assert!(!g.is_dense());
            assert!(g.is_csr());
        }
    }

    // A graph with a duplicate edge, otherwise identical in shape to one
    // that would pick Csr, picks Repr::General instead -- `Csr::add_edge()`
    // silently no-ops on a repeated `(a, b)` pair and
    // `Csr::from_sorted_edges()` errors on one (`CsrData`'s doc comment), so
    // `detect_csr()` gates on the same `has_duplicate_edges()` check
    // `detect_dense()` already uses (design recommendation 4) rather than
    // ever attempting to build a `Csr` that can't hold the input. n=8
    // directed, same shape as `duplicate_free_below_density_threshold_picks_csr_repr`
    // but with 1->2 supplied twice: density unaffected by the duplicate
    // (still 6/28 < 0.3, using the distinct-pair count) -- would otherwise
    // qualify for Csr, but the duplicate forces General.
    #[test]
    fn duplicate_edge_below_density_threshold_picks_general_repr() {
        test! {
            let from = vec![1, 2, 3, 4, 5, 1, 1];
            let to = vec![2, 3, 4, 5, 6, 3, 2];
            let g = GraphBackend::new(8, from, to, true).unwrap();
            assert!(!g.is_dense());
            assert!(!g.is_csr());
            assert!(g.has_edge(1, 2));
            assert_eq!(g.degree(1, "out"), 3); // 1->2 (x2) + 1->3
        }
    }

    // A dense, duplicate-free graph picks Repr::Dense, not Repr::Csr --
    // confirms the ordering (density check, then CSR-eligibility, then
    // general fallback): the density check runs *before* the CSR check, so
    // a graph that clears the density threshold never reaches
    // `detect_csr()` at all. Same input as
    // `dense_enough_duplicate_free_graph_picks_dense_repr` above (complete
    // undirected triangle, density = 3/(3 choose 2) = 1.0 > 0.3).
    #[test]
    fn dense_duplicate_free_graph_picks_dense_not_csr_repr() {
        test! {
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 3, 3], false).unwrap();
            assert!(g.is_dense());
            assert!(!g.is_csr());
        }
    }

    // -- rustworkx-core algorithm interop ----------------------------------
    //
    // The actual point of every `visit`-trait impl above: proof that real
    // rustworkx-core algorithms run directly against each `Repr` variant,
    // not just that the trait impls happen to compile. Not a
    // `#[extendr]`-exposed capability yet (no R method calls any of this) --
    // this is Rust-internal evidence the trait coverage is real, ahead of any
    // graphvec operation actually being ported to delegate to it.
    //
    // Two algorithms, chosen because they exercise direction differently:
    //
    // - `centrality::degree_centrality(g, Some(dir))` reads
    //   `GraphProp::is_directed()` first and only honours `dir` when it is
    //   true, otherwise counting `neighbors()` (rustworkx-core 0.18.1
    //   `centrality.rs:368-390`). That makes it the sharpest available probe
    //   for "is this graph stored with the directedness it actually has": on
    //   an undirected graph the answer must be the *full* degree even though
    //   `Incoming` was asked for, and directed-typed storage would silently
    //   return in-degrees instead. Every undirected case below asserts the
    //   right one and names the wrong one.
    // - `connectivity::core_number(g)` needs `IntoNeighborsDirected` on
    //   `&G` in both directions no matter the graph's own directedness. It
    //   is what would not even compile for an undirected `Repr::Dense`
    //   without `UndirectedMatrix`, or for `Repr::Csr` without the impls
    //   from `4d0799f`/`SymmetricCsr`.
    //
    // Every case goes through `with_graph_view!`, so these also stand as the
    // worked examples of how a ported operation is meant to call in.

    // Small helper: `core_number()` returns a `DictMap` keyed by the graph's
    // own `NodeId` type, which differs per variant (`NodeIndex` for
    // `Graph`/`MatrixGraph`, a bare `u32` for `Csr`). Reduce it to a plain
    // per-position `Vec` via `NodeIndexable::to_index()` so the expectations
    // below can be written once, in graphvec's own 0-based node order.
    fn core_numbers(g: &GraphBackend) -> Vec<usize> {
        with_graph_view!(g, |view| {
            let cores = rustworkx_core::connectivity::core_number(view);
            let mut out = vec![0usize; g.n_nodes() as usize];
            for (node, k) in cores.iter() {
                out[view.to_index(*node)] = *k;
            }
            out
        })
    }

    // `degree_centrality(_, Some(dir))`, same reduction to node order.
    fn directed_degree_centrality(g: &GraphBackend, dir: Direction) -> Vec<f64> {
        with_graph_view!(g, |view| {
            rustworkx_core::centrality::degree_centrality(view, Some(dir))
        })
    }

    // -- Repr::General -----------------------------------------------------

    // n=4 directed, edges 1->2, 2->3, 3->4, 1->2 (again), 1->3. The repeated
    // (1,2) forces General past the density gate (5/(4 choose 2) = 0.83).
    #[test]
    fn general_directed_supports_rustworkx_core_algorithms() {
        test! {
            let g = GraphBackend::new(4, vec![1, 2, 3, 1, 1], vec![2, 3, 4, 2, 3], true).unwrap();
            assert!(!g.is_dense());
            assert!(!g.is_csr());

            // In-degrees, parallel edges counted separately (node 2 has two
            // incoming 1->2 arcs), over node_count - 1 = 3.
            let inc = directed_degree_centrality(&g, Direction::Incoming);
            for (node, &want) in [0.0, 2.0, 2.0, 1.0].iter().enumerate() {
                assert_eq!(inc[node], want / 3.0, "in-centrality of node {}", node + 1);
            }
            let out = directed_degree_centrality(&g, Direction::Outgoing);
            for (node, &want) in [3.0, 1.0, 1.0, 0.0].iter().enumerate() {
                assert_eq!(out[node], want / 3.0, "out-centrality of node {}", node + 1);
            }

            // core_number collects neighbours into a HashSet, so it sees the
            // simple graph 1-2, 2-3, 3-4, 1-3: node 4 (degree 1) peels off
            // as a 1-core, leaving the triangle 1-2-3 as a 2-core.
            assert_eq!(core_numbers(&g), vec![2, 2, 2, 1]);
        }
    }

    // The same five edges, undirected. This is the case that would be
    // silently wrong if `Repr::General` still stored an undirected graph as
    // a `DiGraph`: `degree_centrality(_, Some(Incoming))` would report
    // in-degrees [0, 2, 2, 1]/3, where the correct undirected answer counts
    // every incident edge.
    #[test]
    fn general_undirected_supports_rustworkx_core_algorithms() {
        test! {
            let g = GraphBackend::new(4, vec![1, 2, 3, 1, 1], vec![2, 3, 4, 2, 3], false).unwrap();
            assert!(!g.is_dense());
            assert!(!g.is_csr());

            let inc = directed_degree_centrality(&g, Direction::Incoming);
            // Node 1's answer is 3, not the 0 a `DiGraph` would have given.
            for (node, &want) in [3.0, 3.0, 3.0, 1.0].iter().enumerate() {
                assert_eq!(inc[node], want / 3.0, "centrality of node {}", node + 1);
            }
            // Direction is ignored entirely, so both agree ...
            assert_eq!(inc, directed_degree_centrality(&g, Direction::Outgoing));
            // ... and each is exactly this file's own `degree()`, which is
            // the property the whole exercise is about: the algorithm and
            // the R-visible primitive see one graph, not two.
            for node in 1..=4 {
                assert_eq!(inc[(node - 1) as usize], f64::from(g.degree(node, "all")) / 3.0);
            }

            assert_eq!(core_numbers(&g), vec![2, 2, 2, 1]);
        }
    }

    // -- Repr::Dense -------------------------------------------------------

    // n=4 directed, edges 1->2, 1->3, 2->3, 3->4: density = 4/(4 choose 2)
    // = 0.67 > 0.3, no duplicate ordered pairs -- picks Dense.
    #[test]
    fn dense_directed_supports_rustworkx_core_algorithms() {
        test! {
            let g = GraphBackend::new(4, vec![1, 1, 2, 3], vec![2, 3, 3, 4], true).unwrap();
            assert!(g.is_dense());

            let inc = directed_degree_centrality(&g, Direction::Incoming);
            for (node, &want) in [0.0, 1.0, 2.0, 1.0].iter().enumerate() {
                assert_eq!(inc[node], want / 3.0, "in-centrality of node {}", node + 1);
            }
            let out = directed_degree_centrality(&g, Direction::Outgoing);
            for (node, &want) in [2.0, 1.0, 1.0, 0.0].iter().enumerate() {
                assert_eq!(out[node], want / 3.0, "out-centrality of node {}", node + 1);
            }

            assert_eq!(core_numbers(&g), vec![2, 2, 2, 1]);
        }
    }

    // The same four edges, undirected -- `MatrixGraph<(), (), _, Undirected,
    // ...>` storage. Neither algorithm compiles for this variant without
    // `UndirectedMatrix`: petgraph implements `IntoNeighborsDirected` for
    // `&MatrixGraph` only when `Ty = Directed` (`matrix_graph.rs:1381-1389`).
    // A directed-typed dense store would answer the `Incoming` query with
    // [0, 1, 2, 1]/3 instead of the full degrees below.
    #[test]
    fn dense_undirected_supports_rustworkx_core_algorithms() {
        test! {
            let g = GraphBackend::new(4, vec![1, 1, 2, 3], vec![2, 3, 3, 4], false).unwrap();
            assert!(g.is_dense());

            let inc = directed_degree_centrality(&g, Direction::Incoming);
            // Node 1's answer is 2, not the 0 directed storage would give.
            for (node, &want) in [2.0, 2.0, 3.0, 1.0].iter().enumerate() {
                assert_eq!(inc[node], want / 3.0, "centrality of node {}", node + 1);
            }
            assert_eq!(inc, directed_degree_centrality(&g, Direction::Outgoing));
            for node in 1..=4 {
                assert_eq!(inc[(node - 1) as usize], f64::from(g.degree(node, "all")) / 3.0);
            }

            assert_eq!(core_numbers(&g), vec![2, 2, 2, 1]);
        }
    }

    // -- Repr::Csr ---------------------------------------------------------

    // n=8 directed path 1->2->3->4->5->6 plus 1->3; density = 6/28 ~= 0.214
    // < 0.3, duplicate-free -- picks Csr. Same shape as
    // `csr_node_identity_is_dense_and_stable`.
    //
    // Extends what `4d0799f` proved (`degree_centrality` against a bare
    // `&CsrData`) by routing through `with_graph_view!` and adding
    // `core_number`.
    #[test]
    fn csr_directed_supports_rustworkx_core_algorithms() {
        test! {
            let g = GraphBackend::new(8, vec![1, 2, 3, 4, 5, 1], vec![2, 3, 4, 5, 6, 3], true).unwrap();
            assert!(g.is_csr());

            // `Some(direction)` divides by `node_count - 1` regardless of
            // whether that matches the "complete graph" case (see
            // `degree_centrality`'s own source) -- node_count is 8 here, so
            // the expected denominator is 7 throughout.
            let out = directed_degree_centrality(&g, Direction::Outgoing);
            for (node, &want) in [2.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0].iter().enumerate() {
                assert_eq!(out[node], want / 7.0, "out-centrality of node {}", node + 1);
            }
            let inc = directed_degree_centrality(&g, Direction::Incoming);
            for (node, &want) in [0.0, 1.0, 2.0, 1.0, 1.0, 1.0, 0.0, 0.0].iter().enumerate() {
                assert_eq!(inc[node], want / 7.0, "in-centrality of node {}", node + 1);
            }

            // Triangle 1-2-3 with a tail 3-4-5-6 hanging off it, plus two
            // isolated nodes: the tail peels off as a 1-core, the isolated
            // pair never enters even the 1-core.
            assert_eq!(core_numbers(&g), vec![2, 2, 2, 1, 1, 1, 0, 0]);
        }
    }

    // n=8 undirected: path 1-2-3-4-5-6 plus a self-loop on node 1 (nodes 7
    // and 8 isolated, keeping density at 6/28 ~= 0.214 < 0.3; (1,1) and
    // (1,2) are distinct canonical keys, so it stays duplicate-free and
    // picks Csr).
    //
    // This is the case `SymmetricCsr` exists for, and it checks two things
    // directed storage would get wrong at once: the direction-blind answer
    // (`Incoming` on node 1 would be the single loop arc, not its full
    // degree of 2), and the self-loop convention -- petgraph's own
    // `UndirectedAdaptor` would count the loop twice here, giving node 1
    // a degree of 3. Both are pinned against `degree(node, "all")`.
    #[test]
    fn csr_undirected_supports_rustworkx_core_algorithms() {
        test! {
            let g = GraphBackend::new(8, vec![1, 1, 2, 3, 4, 5], vec![1, 2, 3, 4, 5, 6], false).unwrap();
            assert!(g.is_csr());

            let inc = directed_degree_centrality(&g, Direction::Incoming);
            let expected = [2.0, 2.0, 2.0, 2.0, 2.0, 1.0, 0.0, 0.0];
            for (node, &want) in expected.iter().enumerate() {
                assert_eq!(inc[node], want / 7.0, "centrality of node {}", node + 1);
            }
            assert_eq!(inc, directed_degree_centrality(&g, Direction::Outgoing));
            for node in 1..=8 {
                assert_eq!(inc[(node - 1) as usize], f64::from(g.degree(node, "all")) / 7.0);
            }

            // core_number's HashSet of neighbours keeps node 1's self-loop
            // as the entry `1` itself, so node 1's set is {1, 2} and the
            // whole path is a 1-core; the isolated nodes stay at 0.
            assert_eq!(core_numbers(&g), vec![1, 1, 1, 1, 1, 1, 0, 0]);
        }
    }
}
