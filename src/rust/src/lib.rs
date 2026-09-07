use extendr_api::prelude::*;
use rustworkx_core::petgraph::csr::{Csr, NodeIndex as CsrNodeIndex};
use rustworkx_core::petgraph::graph::{DiGraph, EdgeIndex, NodeIndex};
use rustworkx_core::petgraph::matrix_graph::{MatrixGraph, NodeIndex as MatrixNodeIndex};
use rustworkx_core::petgraph::{Directed, Direction, EdgeType, Undirected};
use std::collections::hash_map::RandomState;
use std::collections::HashSet;

/// The general-purpose representation: today's sole `Repr` variant, an
/// adjacency-list `Graph` plus the per-node arc counts cached at
/// construction so `degree()` is O(1) instead of consuming `neighbors()`'s
/// O(d) walk (petgraph's `Graph` has no cached degree of its own --
/// confirmed against its API, see `_dev/petgraph_data_types.md` S1).
/// Counted over the always-directed internal `graph` regardless of the
/// owning `GraphBackend::directed`, so a self-loop contributes to both and
/// `out_degree[i] + in_degree[i]` reproduces the doubled-self-loop
/// convention `neighbors()` already established (see `symmetric_neighbors()`
/// below and this file's tests).
struct GeneralData {
    graph: DiGraph<(), ()>,
    out_degree: Vec<i32>,
    in_degree: Vec<i32>,
}

/// The tree/forest representation (`_dev/petgraph_data_types.md` S2.3): a
/// parent-pointer vector, no separate edge table. `parent[i]` is the
/// 1-based parent position of node `i+1`, `0` for a root -- multiple zero
/// entries mean a forest (several root-ed trees over the same node set),
/// deliberately accepted, not just a single tree -- see this file's
/// `detect_tree()` doc comment for why.
///
/// A tree's storage alone can't answer `edge_endpoints()` in original
/// *construction* order (node order and input order are different things
/// once edges arrive out of node order) even though it can answer every
/// other query from `parent` alone -- `R/edge_vec.R`/`R/node_vec.R` set
/// `edge_id <- seq_len(n_edges)` once, right after construction, trusting
/// edge k of the backend to be the k-th `from`/`to` pair *as originally
/// supplied*, the same contract `_dev/petgraph_data_types.md` S4's
/// edge-identity item requires of every representation. `order` is exactly
/// that original `from` vector (each non-root node appears in it exactly
/// once, since a valid tree has out-degree <= 1 everywhere) kept around
/// for this one purpose; `to` for edge `k` is always recoverable as
/// `parent[order[k] - 1]`, so it isn't stored twice.
struct TreeData {
    parent: Vec<i32>,
    order: Vec<i32>,
    // Reverse (child) adjacency as a CSR pair, built once at construction
    // over the same N-1-or-fewer edges the parent vector already encodes
    // (`_dev/DATA.md` S2.3: "sorted by from, to already *is* this vector,
    // so no separate construction step exists" -- true for the forward
    // direction; the reverse direction is this). Without it, `neighbors(i,
    // "in")`/`degree(i, "in")` would have to scan all N parents on every
    // call, an O(N) regression `_dev/petgraph_data_types.md`'s O(1)-degree
    // bar (S1) rules out.
    children_ptr: Vec<i32>,
    children_idx: Vec<i32>,
}

impl TreeData {
    fn out_neighbors(&self, idx: usize) -> Vec<i32> {
        match self.parent[idx] {
            0 => Vec::new(),
            p => vec![p],
        }
    }

    fn in_neighbors(&self, idx: usize) -> Vec<i32> {
        let start = self.children_ptr[idx] as usize;
        let end = self.children_ptr[idx + 1] as usize;
        self.children_idx[start..end].to_vec()
    }

    fn out_degree(&self, idx: usize) -> i32 {
        if self.parent[idx] == 0 {
            0
        } else {
            1
        }
    }

    fn in_degree(&self, idx: usize) -> i32 {
        self.children_ptr[idx + 1] - self.children_ptr[idx]
    }
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

/// The dense representation (`_dev/petgraph_data_types.md` S2/S4 item 3): a
/// bit-matrix-backed `MatrixGraph`, chosen for graphs past the density
/// threshold in `detect_dense()` below with no duplicate edges.
///
/// **Why `from`/`to` are kept alongside `matrix` rather than answering
/// everything from the matrix alone**: `MatrixGraph::add_edge()`'s own doc
/// comment states it plainly -- "MatrixGraph does not allow adding parallel
/// (duplicate) edges," it panics if one already exists between two nodes --
/// so presence is keyed purely by the `(node, node)` pair, nothing else.
/// Unlike `Repr::General`'s `EdgeIndex` (naturally insertion-ordered) or
/// `Repr::Tree`'s `order` vector (S4 edge-identity item), `MatrixGraph` has
/// no insertion-order edge enumeration at all to recover original
/// construction order from. The fix used here is the simple one
/// `_dev/petgraph_data_types.md` S4 calls out as sufficient when there's no
/// per-edge storage order to permute against in the first place: keep the
/// original `from`/`to` vectors verbatim (as supplied to `new()`) and answer
/// `edge_endpoints()`/`induced_subgraph()` from those directly, via
/// `GraphBackend::edge_list()` -- mirroring how `Repr::General` answers the
/// same two methods via `EdgeIndex` today, explicit storage instead of
/// relying on the matrix.
///
/// **Why `degree()`/`neighbors()` are also computed from `from`/`to`
/// (`out_degree`/`in_degree` below), not from `matrix`'s own neighbour
/// iteration**: this project's self-loop/undirected-degree convention (a
/// loop counts twice, `_dev/petgraph_data_types.md` S4) falls out of
/// `Repr::General`'s *always-directed* internal storage almost by accident
/// -- a self-loop is stored once but shows up in both a node's outgoing and
/// incoming adjacency query. A `MatrixGraph<Undirected>` has no separate
/// out/in adjacency at all (one triangular bit per unordered pair), so
/// there's no doubling to inherit the same way; rather than trust whatever
/// `MatrixGraph::neighbors()`/`edges()` happens to do with that single bit
/// (S4 explicitly warns not to assume any petgraph type agrees with
/// `Graph`'s convention here, in either direction -- confirmed to actually
/// differ, see `dense_self_loop_counts_twice_in_undirected_degree`),
/// `degree()`/`neighbors()` are computed directly from the retained
/// `from`/`to` vectors, the same convention `GeneralData` establishes,
/// computed the same way (count/scan occurrences), just against explicit
/// vectors instead of always-directed petgraph storage.
///
/// `matrix` itself is used for exactly one thing: `has_edge()`'s O(1) bit
/// test (`_dev/DATA.md` S2.5's whole reason to promote to this backend).
struct DenseData {
    matrix: DenseMatrix,
    from: Vec<i32>,
    to: Vec<i32>,
    out_degree: Vec<i32>,
    in_degree: Vec<i32>,
}

impl DenseData {
    fn out_neighbors(&self, idx: usize) -> Vec<i32> {
        let node = (idx + 1) as i32;
        self.from
            .iter()
            .zip(self.to.iter())
            .filter(|&(&f, _)| f == node)
            .map(|(_, &t)| t)
            .collect()
    }

    fn in_neighbors(&self, idx: usize) -> Vec<i32> {
        let node = (idx + 1) as i32;
        self.from
            .iter()
            .zip(self.to.iter())
            .filter(|&(_, &t)| t == node)
            .map(|(&f, _)| f)
            .collect()
    }
}

// Provisional density threshold for promoting a non-tree-shaped graph to
// `Repr::Dense` (`_dev/DATA.md` S3 step 4 / S6: "provisionally ~0.3...
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
/// all -- skip Dense promotion rather than dividing by zero). Only called
/// for graphs `detect_tree()` already rejected, matching S3's order
/// (tree/forest check, then density check, then otherwise general).
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

    let mut out_degree = vec![0i32; n];
    let mut in_degree = vec![0i32; n];
    for (&f, &t) in from.iter().zip(to.iter()) {
        out_degree[(f - 1) as usize] += 1;
        in_degree[(t - 1) as usize] += 1;
    }

    let matrix = if directed {
        DenseMatrix::Directed(build_dense_matrix::<Directed>(n, from, to))
    } else {
        DenseMatrix::Undirected(build_dense_matrix::<Undirected>(n, from, to))
    };

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
/// by `detect_csr()` below for any graph that is not tree-shaped
/// (`Repr::Tree`), not dense enough -- or dense but with a duplicate edge
/// (`Repr::Dense`) -- **and has no duplicate edges of its own**.
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
/// for anything none of the more specific variants -- tree, dense, or this
/// one -- can hold, multigraphs included. `Repr::Csr` is a genuinely new,
/// narrower bucket carved out of what `_dev/DATA.md` S3 step 5 described as
/// one homogeneous "otherwise" catch-all: duplicate-free, non-tree,
/// below-density-threshold graphs go here; everything else that isn't
/// tree/dense (including any graph with a duplicate edge) falls through to
/// `Repr::General`. See `GraphBackend::new()`'s selection order for where
/// this split actually happens.
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
/// **This unlocks genuine O(1) degree in both directions** -- unlike
/// `Repr::Dense`, which had to fall back to scanning retained `from`/`to`
/// vectors because an `Undirected`-typed `MatrixGraph` has no separate
/// out/in adjacency to double through (S4's self-loop-convention risk).
/// `csr.out_degree()` is petgraph's own O(1) row-length primitive; `in_ptr`/
/// `in_idx` below are a reverse CSR over the same edges, built once at
/// construction -- the same pattern `TreeData::children_ptr`/
/// `children_idx` established for the tree variant's reverse (child)
/// direction, since `Csr` itself, like a parent-pointer vector, only gives
/// O(1) lookup in its one native (forward) direction. A self-loop is stored
/// once (one arc `a -> a`) but is counted once by `out_degree` and once by
/// the reverse index's `in_degree`, reproducing the doubled-self-loop
/// convention for free -- confirmed by
/// `csr_self_loop_counts_twice_in_undirected_degree`, not just assumed.
///
/// **Edge identity**: the same simple fix `Repr::Dense` uses, not the
/// "hidden permutation vector" `_dev/petgraph_data_types.md` S4 describes as
/// the fallback for a variant whose *natural* storage order differs from
/// input order -- which `Csr` genuinely is (it wants source-then-target
/// sorted storage, a real permutation of arbitrary construction order,
/// unlike `Repr::Tree`/`Repr::Dense`, which had no separate storage order to
/// permute against in the first place). `from`/`to` below are the original
/// vectors exactly as supplied to `new()`, kept verbatim alongside `csr`;
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
/// graph `detect_tree()` and `detect_dense()` already rejected qualifies for
/// `Repr::Csr` -- it does unless it has a duplicate edge, the same
/// `has_duplicate_edges()` gate `detect_dense()` already uses (design
/// recommendation 4: reuse it rather than write a second, possibly-
/// inconsistent check), since `Csr` physically cannot hold a multigraph any
/// more than `MatrixGraph` can. Only called for graphs that already failed
/// both earlier checks, matching this file's selection order (tree, then
/// density/duplicate-gated dense, then duplicate-gated CSR, then otherwise
/// general).
fn detect_csr(n: usize, from: &[i32], to: &[i32], directed: bool) -> Option<CsrData> {
    if has_duplicate_edges(from, to, directed) {
        return None;
    }

    let csr = build_csr(n, from, to);

    // Reverse (in-)adjacency CSR via counting sort over `to`, the same
    // technique `detect_tree()` uses to build `children_ptr`/`children_idx`
    // (generalised here: a node can have any number of in-edges, not at
    // most one).
    let mut in_ptr = vec![0i32; n + 1];
    for &t in to {
        in_ptr[t as usize] += 1;
    }
    for i in 0..n {
        in_ptr[i + 1] += in_ptr[i];
    }
    let mut cursor = in_ptr.clone();
    let mut in_idx = vec![0i32; from.len()];
    for (&f, &t) in from.iter().zip(to.iter()) {
        let slot = cursor[(t - 1) as usize] as usize;
        in_idx[slot] = f;
        cursor[(t - 1) as usize] += 1;
    }

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
    Tree(TreeData),
    Dense(DenseData),
    Csr(CsrData),
}

/// Directed, out-degree <= 1 for every node, no cycle -- accepts a *forest*
/// (several root-ed trees over the same node set), not only a single tree.
///
/// `_dev/DATA.md` states this two ways that read as in tension: S2.3's own
/// "Default for" bullet says "exactly one root", but S3's actual selection
/// algorithm -- the one `_dev/petgraph_data_types.md` S5 says to "port...
/// don't redesign" -- calls it a "tree/forest validator" with "one root per
/// component". Decision made here, stated plainly per
/// `_dev/petgraph_data_types.md` S5's own instruction: implement the forest
/// form. Reasons: (1) it's S3's literal wording, the section titled as the
/// authoritative selection algorithm; (2) a parent-pointer vector supports
/// it for free -- multiple `0` entries cost nothing extra to store or query,
/// there is no separate "single tree" representation being given up; (3) it
/// is a strict superset of the single-root form, so nothing that would have
/// qualified under a stricter reading is excluded, only more graphs
/// (legitimate forests) are additionally accepted. See this file's
/// `forest_with_two_roots_is_still_tree_shaped` test.
///
/// Cycle detection matters even though out-degree <= 1 alone looks
/// tree-like: a node whose out-edge chain (chasing `parent` repeatedly)
/// never reaches a `0` root, including a self-loop (`parent[i] == i + 1`),
/// is a cycle -- petgraph's `Graph` already accepts exactly this shape as a
/// perfectly ordinary directed graph (see the pre-existing
/// `induced_subgraph_drops_dangling_and_clones_replicated` test's 1->2->3->1
/// triangle), so it must be rejected here, not assumed away by the
/// out-degree check alone.
fn detect_tree(n: usize, from: &[i32], to: &[i32], directed: bool) -> Option<TreeData> {
    if !directed {
        return None;
    }

    let mut out_count = vec![0i32; n];
    let mut parent = vec![0i32; n];
    for (&f, &t) in from.iter().zip(to.iter()) {
        if f < 1 || (f as usize) > n || t < 1 || (t as usize) > n {
            return None;
        }
        let fi = (f - 1) as usize;
        out_count[fi] += 1;
        if out_count[fi] > 1 {
            return None;
        }
        parent[fi] = t;
    }

    // Cycle check: chase each node's parent chain, three-colouring as it
    // goes (0 unvisited, 1 in-progress-on-this-chase, 2 proven acyclic).
    // Revisiting a `1` node means the chain looped back on itself without
    // reaching a root -- a cycle. O(N) total: every node is chased at most
    // once as a fresh start, and a chain stops the moment it reaches
    // already-`2` ground, so no edge is walked more than twice.
    let mut state = vec![0u8; n];
    for start in 0..n {
        if state[start] != 0 {
            continue;
        }
        let mut path: Vec<usize> = Vec::new();
        let mut cur = start;
        loop {
            match state[cur] {
                0 => {
                    state[cur] = 1;
                    path.push(cur);
                    match parent[cur] {
                        0 => break,
                        p => cur = (p - 1) as usize,
                    }
                }
                1 => return None,
                _ => break,
            }
        }
        for node in path {
            state[node] = 2;
        }
    }

    // Reverse CSR (children) via counting sort over `parent`.
    let mut children_ptr = vec![0i32; n + 1];
    for &p in &parent {
        if p != 0 {
            children_ptr[p as usize] += 1;
        }
    }
    for i in 0..n {
        children_ptr[i + 1] += children_ptr[i];
    }
    let mut cursor = children_ptr.clone();
    let mut children_idx = vec![0i32; from.len()];
    for (node0, &p) in parent.iter().enumerate() {
        if p != 0 {
            let slot = cursor[(p - 1) as usize] as usize;
            children_idx[slot] = (node0 + 1) as i32;
            cursor[(p - 1) as usize] += 1;
        }
    }

    Some(TreeData {
        parent,
        order: from.to_vec(),
        children_ptr,
        children_idx,
    })
}

/// The shared topology backing a `node_vec`/`edge_vec` pair (non-hyperedge
/// case only -- see `_dev/RUST_BACKEND.md`). Wraps one of several physical
/// `Repr` variants, auto-selected at construction from graph shape
/// (`_dev/petgraph_data_types.md` S3/S5); `directed` is metadata every
/// variant's query methods interpret, not itself part of the shape
/// decision beyond gating which variants are eligible (only a directed
/// graph can be tree-shaped, see `detect_tree()`). This keeps the object
/// immutable and shareable: a `node_vec`, its `edges()` reorientation, and
/// any `edge_vec` sliced from it can all hold the same pointer.
///
/// @export
#[extendr]
struct GraphBackend {
    repr: Repr,
    directed: bool,
}

#[extendr]
impl GraphBackend {
    /// Build a graph on `n` nodes from 1-based `from`/`to` positions.
    /// Automatically picks the cheapest `Repr` the graph's shape qualifies
    /// for (`_dev/petgraph_data_types.md` S3/S5 -- this decision belongs
    /// here, not in R, so there is exactly one place shape detection can
    /// drift out of sync with the representation it feeds). For the
    /// general representation, edges are added in input order and never
    /// removed afterwards, so edge ids (0-based internally, 1-based at the
    /// R boundary) stay stable and match the row order of the R-side edge
    /// attribute table; for the tree representation, the same external
    /// contract is upheld via `TreeData::order` (see its doc comment).
    fn new(n: i32, from: Vec<i32>, to: Vec<i32>, directed: bool) -> Self {
        let n = if n > 0 { n as usize } else { 0 };

        if let Some(tree) = detect_tree(n, &from, &to, directed) {
            return GraphBackend {
                repr: Repr::Tree(tree),
                directed,
            };
        }

        if let Some(dense) = detect_dense(n, &from, &to, directed) {
            return GraphBackend {
                repr: Repr::Dense(dense),
                directed,
            };
        }

        // `_dev/DATA.md` S3 step 5's "otherwise" catch-all splits in two
        // here, a deliberate deviation from `_dev/petgraph_data_types.md` S6
        // item 4's literal wording (`CsrData`'s doc comment has the full
        // reasoning, confirmed with the user before implementing): a graph
        // that reaches this point (not tree-shaped, not dense enough or
        // dense-with-a-duplicate) goes to the new `Repr::Csr` if it has no
        // duplicate edges of its own -- `Csr` cannot hold a multigraph any
        // more than `MatrixGraph` can -- and only falls through to
        // `Repr::General` (unchanged, still the always-correct fallback)
        // when it does.
        if let Some(csr) = detect_csr(n, &from, &to, directed) {
            return GraphBackend {
                repr: Repr::Csr(csr),
                directed,
            };
        }

        let mut graph = DiGraph::<(), ()>::with_capacity(n, from.len());
        for _ in 0..n {
            graph.add_node(());
        }
        let mut out_degree = vec![0i32; n];
        let mut in_degree = vec![0i32; n];
        for (f, t) in from.iter().zip(to.iter()) {
            let fi = NodeIndex::new((*f - 1) as usize);
            let ti = NodeIndex::new((*t - 1) as usize);
            graph.add_edge(fi, ti, ());
            out_degree[(*f - 1) as usize] += 1;
            in_degree[(*t - 1) as usize] += 1;
        }
        GraphBackend {
            repr: Repr::General(GeneralData {
                graph,
                out_degree,
                in_degree,
            }),
            directed,
        }
    }

    fn n_nodes(&self) -> i32 {
        match &self.repr {
            Repr::General(g) => g.graph.node_count() as i32,
            Repr::Tree(t) => t.parent.len() as i32,
            Repr::Dense(d) => d.out_degree.len() as i32,
            Repr::Csr(c) => c.csr.node_count() as i32,
        }
    }

    fn n_edges(&self) -> i32 {
        match &self.repr {
            Repr::General(g) => g.graph.edge_count() as i32,
            Repr::Tree(t) => t.order.len() as i32,
            Repr::Dense(d) => d.from.len() as i32,
            Repr::Csr(c) => c.from.len() as i32,
        }
    }

    fn is_directed(&self) -> bool {
        self.directed
    }

    /// Whether this backend is tree/forest-shaped (`Repr::Tree`) -- the one
    /// thing R call sites (once any exist) need to check before calling
    /// `parent()`, per `_dev/petgraph_data_types.md` S3's suggestion of a
    /// single predicate rather than a per-variant method surface.
    fn is_tree(&self) -> bool {
        matches!(self.repr, Repr::Tree(_))
    }

    /// Whether this backend is dense-matrix-shaped (`Repr::Dense`) -- a
    /// test/diagnostic accessor mirroring `is_tree()`'s pattern (same
    /// reasoning: one predicate per variant, not a different method surface
    /// per shape). Not currently required by any `R/*.R` call site, added
    /// for the same reason `is_tree()` was: this file's tests need a way to
    /// confirm which `Repr` a given `new()` call picked.
    fn is_dense(&self) -> bool {
        matches!(self.repr, Repr::Dense(_))
    }

    /// Whether this backend is CSR-shaped (`Repr::Csr`) -- a test/diagnostic
    /// accessor mirroring `is_tree()`/`is_dense()`'s pattern, added for the
    /// same reason: this file's tests need a way to confirm which `Repr` a
    /// given `new()` call picked. Not currently required by any `R/*.R` call
    /// site.
    fn is_csr(&self) -> bool {
        matches!(self.repr, Repr::Csr(_))
    }

    /// The 1-based parent position of `node` (1-based); `0` means `node` is
    /// a root. Only defined when `is_tree()` is true -- `0` already means
    /// "root" for a real tree, so a non-tree variant returning `0` would be
    /// silently indistinguishable from a real answer rather than "not
    /// applicable"; `_dev/petgraph_data_types.md` S3 flags exactly this and
    /// suggests `panic!`/`NA_INTEGER` instead, which is what this does --
    /// mirroring this project's existing idiom for an operation an input
    /// shape doesn't support (e.g. `check_no_hyperedges()`'s
    /// `cli::cli_abort()`) rather than returning a value that looks valid
    /// but means something else per variant.
    fn parent(&self, node: i32) -> i32 {
        match &self.repr {
            Repr::Tree(t) => t.parent[(node - 1) as usize],
            Repr::General(_) | Repr::Dense(_) | Repr::Csr(_) => {
                panic!("`parent()` is only defined when `is_tree()` is TRUE")
            }
        }
    }

    /// 1-based neighbour positions of `node` (1-based). For an undirected
    /// graph `mode` is ignored and the symmetric neighbour set is always
    /// returned (a self-loop appears twice, once via each of the node's
    /// outgoing/incoming adjacency lists -- see the crate's tests). For a
    /// directed graph, `mode` is `"out"`, `"in"`, or `"all"` (both,
    /// concatenated). One entry per incident edge, not deduplicated, so
    /// `degree()` can just be `neighbors().len()`.
    fn neighbors(&self, node: i32, mode: &str) -> Vec<i32> {
        let idx = (node - 1) as usize;
        if !self.directed {
            return self.symmetric_neighbors(idx);
        }
        match mode {
            "out" => self.out_neighbors_at(idx),
            "in" => self.in_neighbors_at(idx),
            "all" => self.symmetric_neighbors(idx),
            _ => panic!("`mode` must be one of \"out\", \"in\", \"all\", not \"{mode}\""),
        }
    }

    /// O(1): a `ptr`-difference-style lookup, not a `neighbors().len()`
    /// walk, for either variant (the general representation's construction-
    /// time cache, or the tree representation's `parent`/reverse-CSR
    /// arrays). Must stay in exact agreement with `neighbors()`'s semantics
    /// above (mode handling, panic on an invalid mode, doubled self-loop)
    /// -- see this file's tests.
    fn degree(&self, node: i32, mode: &str) -> i32 {
        let idx = (node - 1) as usize;
        if !self.directed {
            return self.out_degree_at(idx) + self.in_degree_at(idx);
        }
        match mode {
            "out" => self.out_degree_at(idx),
            "in" => self.in_degree_at(idx),
            "all" => self.out_degree_at(idx) + self.in_degree_at(idx),
            _ => panic!("`mode` must be one of \"out\", \"in\", \"all\", not \"{mode}\""),
        }
    }

    /// Adjacency test. For an undirected graph, checks both orientations.
    fn has_edge(&self, from: i32, to: i32) -> bool {
        match &self.repr {
            Repr::General(g) => {
                let fi = NodeIndex::new((from - 1) as usize);
                let ti = NodeIndex::new((to - 1) as usize);
                if g.graph.find_edge(fi, ti).is_some() {
                    return true;
                }
                !self.directed && g.graph.find_edge(ti, fi).is_some()
            }
            Repr::Tree(t) => {
                // Tree is only ever selected when `directed` is true (see
                // `detect_tree()`), so there is no "check both
                // orientations" branch to mirror here -- a node has at
                // most one outgoing edge (to its parent) full stop.
                let fi = (from - 1) as usize;
                t.parent[fi] == to
            }
            Repr::Dense(d) => {
                // No "check both orientations" branch needed here either:
                // an `Undirected`-typed `MatrixGraph` stores one bit per
                // unordered pair (see `to_linearized_matrix_position()` in
                // petgraph's own source), so `has_edge(a, b)` and
                // `has_edge(b, a)` already agree for it -- unlike
                // `Repr::General`, whose internal storage is always
                // `Directed` regardless of `self.directed`.
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

    /// All edges as 1-based `(from, to)` pairs, in construction/edge-id
    /// order. Backs `edge_vec`'s `format()`/`$from`/`$to` and `as.igraph()`
    /// -- no R-side edge table is needed for topology once this exists.
    fn edge_endpoints(&self) -> List {
        let (from, to) = self.edge_list();
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
    /// `[.node_vec`), so the *new* backend's shape (which needn't match the
    /// old one -- an induced subgraph of a tree is not generally a tree,
    /// e.g. dropping a root splits it into a forest, or replication can
    /// reintroduce a cycle) is re-decided by `new()`'s own detection from
    /// scratch, same as it would be for any other from/to/directed input.
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
    // All edges as 1-based `(from, to)` pairs, in construction/edge-id
    // order -- the one place both `edge_endpoints()` and
    // `induced_subgraph()` read topology from, so every `Repr` variant
    // needs to get this right exactly once (`_dev/petgraph_data_types.md`
    // S4's edge-identity item) rather than per call site.
    fn edge_list(&self) -> (Vec<i32>, Vec<i32>) {
        match &self.repr {
            Repr::General(g) => {
                let m = g.graph.edge_count();
                let mut from: Vec<i32> = Vec::with_capacity(m);
                let mut to: Vec<i32> = Vec::with_capacity(m);
                for i in 0..m {
                    let (a, b) = g
                        .graph
                        .edge_endpoints(EdgeIndex::new(i))
                        .expect("edge index in range");
                    from.push(a.index() as i32 + 1);
                    to.push(b.index() as i32 + 1);
                }
                (from, to)
            }
            Repr::Tree(t) => {
                let to: Vec<i32> = t
                    .order
                    .iter()
                    .map(|&c| t.parent[(c - 1) as usize])
                    .collect();
                (t.order.clone(), to)
            }
            Repr::Dense(d) => (d.from.clone(), d.to.clone()),
            Repr::Csr(c) => (c.from.clone(), c.to.clone()),
        }
    }

    // The symmetric neighbour set, out edges then in edges. Deliberately
    // NOT `Graph::neighbors_undirected()`: that method special-cases a
    // self-loop to report it once (see its doc comment/impl -- it exists to
    // stop a *genuinely* undirected `petgraph` graph from double-reporting
    // one physical edge via both its incoming and outgoing adjacency
    // lists). Our general representation is always the `Directed` petgraph
    // type internally (see `GeneralData`), so `neighbors_directed()` for
    // each direction never applies that skip (it only triggers when
    // `self.graph.is_directed()` is false, which for us is never), giving
    // the doubled count the undirected-self-loop convention wants.
    // Confirmed by this file's tests rather than assumed -- the two methods
    // disagree on exactly this case. The tree representation has no
    // self-loop case at all (`detect_tree()` rejects any cycle, a self-loop
    // included), so this doubling never arises there -- see this file's
    // `tree_shaped_graph_has_no_self_loop_case` test.
    fn symmetric_neighbors(&self, idx: usize) -> Vec<i32> {
        let mut v = self.out_neighbors_at(idx);
        v.extend(self.in_neighbors_at(idx));
        v
    }

    fn out_neighbors_at(&self, idx: usize) -> Vec<i32> {
        match &self.repr {
            Repr::General(g) => Self::directed_neighbors(g, NodeIndex::new(idx), Direction::Outgoing),
            Repr::Tree(t) => t.out_neighbors(idx),
            Repr::Dense(d) => d.out_neighbors(idx),
            Repr::Csr(c) => c.out_neighbors(idx),
        }
    }

    fn in_neighbors_at(&self, idx: usize) -> Vec<i32> {
        match &self.repr {
            Repr::General(g) => Self::directed_neighbors(g, NodeIndex::new(idx), Direction::Incoming),
            Repr::Tree(t) => t.in_neighbors(idx),
            Repr::Dense(d) => d.in_neighbors(idx),
            Repr::Csr(c) => c.in_neighbors(idx),
        }
    }

    fn out_degree_at(&self, idx: usize) -> i32 {
        match &self.repr {
            Repr::General(g) => g.out_degree[idx],
            Repr::Tree(t) => t.out_degree(idx),
            Repr::Dense(d) => d.out_degree[idx],
            Repr::Csr(c) => c.out_degree(idx),
        }
    }

    fn in_degree_at(&self, idx: usize) -> i32 {
        match &self.repr {
            Repr::General(g) => g.in_degree[idx],
            Repr::Tree(t) => t.in_degree(idx),
            Repr::Dense(d) => d.in_degree[idx],
            Repr::Csr(c) => c.in_degree(idx),
        }
    }

    fn directed_neighbors(g: &GeneralData, idx: NodeIndex, dir: Direction) -> Vec<i32> {
        g.graph
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
            assert!(!g.is_tree()); // sanity: this input isn't tree-shaped (node 1 has out-degree 2)
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

    // `degree()` stopped being `neighbors().len()` when it moved to a
    // construction-time cache -- pin the two down as agreeing on every
    // mode, directed and undirected, so a future edit to one path can't
    // silently drift from the other.
    #[test]
    fn degree_matches_neighbors_len_every_mode() {
        test! {
            let gd = GraphBackend::new(3, vec![1, 1, 2], vec![1, 2, 3], true);
            for node in 1..=3 {
                for mode in ["out", "in", "all"] {
                    assert_eq!(
                        gd.degree(node, mode),
                        gd.neighbors(node, mode).len() as i32
                    );
                }
            }

            let gu = GraphBackend::new(3, vec![1, 1, 2], vec![1, 2, 3], false);
            for node in 1..=3 {
                // mode is ignored when undirected -- any value must agree.
                assert_eq!(gu.degree(node, "all"), gu.neighbors(node, "all").len() as i32);
            }
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
            // replica of 1, edges touching 3 vanish. Also a cycle, so this is
            // never Tree-shaped -- confirmed below (density selection then
            // picks General or Dense, immaterial here: induced_subgraph() is
            // representation-independent, see its doc comment).
            let g = GraphBackend::new(3, vec![1, 2, 3], vec![2, 3, 1], true);
            assert!(!g.is_tree());
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

    // -- Repr::Tree ----------------------------------------------------
    //
    // Every S4 contract item (`_dev/petgraph_data_types.md`), covered for
    // Repr::Tree specifically -- none of it is inherited by assumption from
    // Repr::General's tests above.

    // Node identity: dense 1..N, stable, representation-independent. This
    // holds trivially for a tree (the R constructor hands over exactly N
    // node positions and TreeData never reorders or compacts them -- same
    // as General, see `_dev/petgraph_data_types.md` S4's first bullet), but
    // stating it plainly rather than skipping it per that section's own
    // instruction: n_nodes() reports N and every node 1..N is independently
    // addressable, replicated or not, root or not.
    #[test]
    fn tree_node_identity_is_dense_and_stable() {
        test! {
            // 4-node, 2-root forest: 1 and 2 are roots; 3's parent is 1,
            // 4's parent is 2.
            let g = GraphBackend::new(4, vec![3, 4], vec![1, 2], true);
            assert!(g.is_tree());
            assert_eq!(g.n_nodes(), 4);
            for node in 1..=4 {
                // every position answers queries independently of the others
                let _ = g.parent(node);
                let _ = g.degree(node, "all");
            }
            assert_eq!(g.parent(1), 0);
            assert_eq!(g.parent(2), 0);
            assert_eq!(g.parent(3), 1);
            assert_eq!(g.parent(4), 2);
        }
    }

    // Edge identity / enumeration order -- the one real risk per S4: a
    // tree's own storage is naturally node-ordered (parent[i] indexed by
    // node), which is *not* generally construction order once edges arrive
    // out of node order, unlike Repr::General's insertion-order EdgeIndex.
    // `TreeData::order` exists specifically to bridge this. Mirrors
    // `edge_endpoints_preserve_construction_order` above, deliberately with
    // edges out of node order so an accidental node-order reconstruction
    // (e.g. naively porting DATA.md S2.3's `from = seq_len(N)[-roots]`
    // formula, which is node-ordered) would be caught.
    #[test]
    fn tree_edge_endpoints_preserve_construction_order() {
        test! {
            // Node 3's parent (edge 3->1) is supplied before node 1's own
            // out-edge... except node 1 is a root here, so use a 4-node
            // chain-like tree instead and still scramble input order:
            // edges (child -> parent): 4->2, 2->1, 3->1. Node order would
            // be [2, 3, 4]; construction order is [4, 2, 3].
            let from = vec![4, 2, 3];
            let to = vec![2, 1, 1];
            let g = GraphBackend::new(4, from.clone(), to.clone(), true);
            assert!(g.is_tree());
            let ends = g.edge_endpoints();
            let got_from: Vec<i32> = ends.dollar("from").unwrap().as_integer_vector().unwrap();
            let got_to: Vec<i32> = ends.dollar("to").unwrap().as_integer_vector().unwrap();
            assert_eq!(got_from, from);
            assert_eq!(got_to, to);
        }
    }

    // A tree by definition has no self-loops (a self-loop is a cycle,
    // rejected by `detect_tree()`) and, since `detect_tree()` requires
    // `directed`, no undirected case either -- confirmed explicitly here
    // rather than left unstated, per S4's self-loop-convention bullet.
    #[test]
    fn tree_shaped_graph_has_no_self_loop_case() {
        test! {
            // A self-loop is a 1-node cycle: rejected, falls back to General.
            let looped = GraphBackend::new(1, vec![1], vec![1], true);
            assert!(!looped.is_tree());

            // Undirected is never tree-shaped, even if the underlying
            // shape (out-degree <= 1, acyclic) would otherwise qualify.
            let undirected = GraphBackend::new(2, vec![1], vec![2], false);
            assert!(!undirected.is_tree());
        }
    }

    // Repr::Tree's degree()/neighbors() must agree with what Repr::General
    // would compute for the *same edge set* -- checked here against
    // manually-counted expected values (in/out-degree by counting
    // occurrences in from/to directly) rather than against a forced-General
    // instance of the same tree-shaped input, since shape selection is
    // automatic and not possible to override from outside `new()`.
    #[test]
    fn tree_degree_matches_general_convention_for_same_edges() {
        test! {
            // 5-node, 1-root tree: 1 is root; 2,3 -> 1; 4,5 -> 2.
            let from = vec![2, 3, 4, 5];
            let to = vec![1, 1, 2, 2];
            let g = GraphBackend::new(5, from.clone(), to.clone(), true);
            assert!(g.is_tree());
            for node in 1..=5i32 {
                let expected_out = from.iter().filter(|&&f| f == node).count() as i32;
                let expected_in = to.iter().filter(|&&t| t == node).count() as i32;
                assert_eq!(g.degree(node, "out"), expected_out, "out-degree of {node}");
                assert_eq!(g.degree(node, "in"), expected_in, "in-degree of {node}");
                assert_eq!(g.degree(node, "all"), expected_out + expected_in, "total degree of {node}");
                assert_eq!(g.neighbors(node, "out").len() as i32, expected_out);
                assert_eq!(g.neighbors(node, "in").len() as i32, expected_in);
            }
        }
    }

    // `mode` semantics must match Repr::General exactly: "out"/"in"/"all",
    // and an invalid mode panics with the identical message (both variants
    // share the same match arm in `neighbors()`/`degree()`, so this is
    // mostly a structural guarantee, but S4 asks for it tested, not
    // assumed).
    #[test]
    #[should_panic(expected = "`mode` must be one of \"out\", \"in\", \"all\", not \"sideways\"")]
    fn tree_neighbors_invalid_mode_panics_like_general() {
        test! {
            let g = GraphBackend::new(2, vec![1], vec![2], true);
            assert!(g.is_tree());
            g.neighbors(1, "sideways");
        }
    }

    #[test]
    #[should_panic(expected = "`mode` must be one of \"out\", \"in\", \"all\", not \"sideways\"")]
    fn tree_degree_invalid_mode_panics_like_general() {
        test! {
            let g = GraphBackend::new(2, vec![1], vec![2], true);
            assert!(g.is_tree());
            g.degree(1, "sideways");
        }
    }

    // induced_subgraph()'s replication/drop logic, mirroring
    // `induced_subgraph_drops_dangling_and_clones_replicated` /
    // `induced_subgraph_treats_zero_as_no_source` above, for a tree-shaped
    // input. Note (stated per this task's own ask): the result is not
    // itself required to be tree-shaped -- dropping the root of a tree
    // splits it into a dangling-free forest of the remaining subtrees, or
    // (as here) can leave a plain from/to/source_edge list that a fresh
    // `new()` call would reclassify entirely independently; this method
    // itself is representation-agnostic (see `edge_list()`).
    #[test]
    fn tree_induced_subgraph_drops_dangling_and_clones_replicated() {
        test! {
            // 3-node chain: 1 is root; 2->1; 3->2.
            let g = GraphBackend::new(3, vec![2, 3], vec![1, 2], true);
            assert!(g.is_tree());
            // New nodes <- old 2, 2, 3 (node 2 replicated, node 1/root dropped):
            // edge 2->1 vanishes (1 has no new position), edge 3->2 clones once
            // per replica of 2.
            let remap = g.induced_subgraph(vec![2, 2, 3]);
            let from: Vec<i32> = remap.dollar("from").unwrap().as_integer_vector().unwrap();
            let to: Vec<i32> = remap.dollar("to").unwrap().as_integer_vector().unwrap();
            let source_edge: Vec<i32> = remap
                .dollar("source_edge")
                .unwrap()
                .as_integer_vector()
                .unwrap();
            // Old edge 2 (3->2) survives once per (new-from replica, new-to) combo:
            // old 3 maps to new {3}, old 2 maps to new {1, 2} -> two clones.
            assert_eq!(from, vec![3, 3]);
            assert_eq!(to.len(), 2); // clones to both replicas of node 2
            assert_eq!(source_edge, vec![2, 2]);
            let mut sorted_to = to.clone();
            sorted_to.sort();
            assert_eq!(sorted_to, vec![1, 2]);
        }
    }

    #[test]
    fn tree_induced_subgraph_treats_zero_as_no_source() {
        test! {
            let g = GraphBackend::new(2, vec![1], vec![2], true); // 1's parent is 2
            assert!(g.is_tree());
            let remap = g.induced_subgraph(vec![0, 2]);
            let from: Vec<i32> = remap.dollar("from").unwrap().as_integer_vector().unwrap();
            assert!(from.is_empty());
        }
    }

    // -- Shape detection itself ------------------------------------------

    #[test]
    fn tree_shaped_graph_picks_tree_repr() {
        test! {
            let g = GraphBackend::new(3, vec![1, 2], vec![2, 3], true);
            assert!(g.is_tree());
        }
    }

    // A second root does NOT disqualify a graph from Repr::Tree in this
    // implementation -- deliberate (see `detect_tree()`'s doc comment):
    // the storage is a forest representation, and this is the concrete
    // case that distinguishes "forest" from "single tree only".
    #[test]
    fn forest_with_two_roots_is_still_tree_shaped() {
        test! {
            // Two separate one-edge trees over the same 4-node set:
            // 1 is root of {1, 2}; 3 is root of {3, 4}.
            let g = GraphBackend::new(4, vec![2, 4], vec![1, 3], true);
            assert!(g.is_tree());
            assert_eq!(g.parent(1), 0);
            assert_eq!(g.parent(2), 1);
            assert_eq!(g.parent(3), 0);
            assert_eq!(g.parent(4), 3);
        }
    }

    #[test]
    fn node_with_out_degree_two_picks_non_tree_repr() {
        test! {
            // Node 1 has two outgoing edges (1->2, 1->3): not tree-shaped.
            // (Density selection then decides General vs Dense -- immaterial
            // to this test, which only checks the tree disqualification.)
            let g = GraphBackend::new(3, vec![1, 1], vec![2, 3], true);
            assert!(!g.is_tree());
        }
    }

    #[test]
    fn a_cycle_picks_non_tree_repr() {
        test! {
            // A cycle disqualifies tree detection regardless of which
            // non-tree variant density selection then picks.
            let g = GraphBackend::new(3, vec![1, 2, 3], vec![2, 3, 1], true);
            assert!(!g.is_tree());
        }
    }

    #[test]
    fn undirected_graph_never_picks_tree_repr() {
        test! {
            // Otherwise tree-shaped (out-degree <= 1, acyclic), but undirected.
            let g = GraphBackend::new(3, vec![1, 2], vec![2, 3], false);
            assert!(!g.is_tree());
        }
    }

    // `parent()` on a non-tree backend: explicit panic, not a silent
    // sentinel (see `GraphBackend::parent()`'s doc comment for why 0 would
    // be the wrong choice here). Same panic for General and Dense alike
    // (see `GraphBackend::parent()`'s `Repr::General(_) | Repr::Dense(_)`
    // arm), so this doesn't need to pin down which of the two a cycle picks.
    #[test]
    #[should_panic(expected = "`parent()` is only defined when `is_tree()` is TRUE")]
    fn parent_panics_on_non_tree_repr() {
        test! {
            let g = GraphBackend::new(3, vec![1, 2, 3], vec![2, 3, 1], true); // a cycle
            assert!(!g.is_tree());
            g.parent(1);
        }
    }

    // -- Repr::Dense -----------------------------------------------------
    //
    // Every S4 contract item (`_dev/petgraph_data_types.md`), covered for
    // Repr::Dense specifically -- none of it is inherited by assumption from
    // Repr::General's or Repr::Tree's tests above.

    // Node identity: dense 1..N, stable, representation-independent -- holds
    // trivially here too (same reasoning as
    // `tree_node_identity_is_dense_and_stable`), stated plainly per S4's own
    // instruction rather than skipped. A 4-node complete undirected graph:
    // density = 6 / (4 choose 2) = 6/6 = 1.0, comfortably past
    // DENSE_THRESHOLD, no duplicate edges -- picks Dense.
    #[test]
    fn dense_node_identity_is_dense_and_stable() {
        test! {
            let from = vec![1, 1, 1, 2, 2, 3];
            let to = vec![2, 3, 4, 3, 4, 4];
            let g = GraphBackend::new(4, from, to, false);
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
    // `EdgeIndex` or Repr::Tree's `order` vector. `DenseData::from`/`to`
    // exist specifically to answer this. Mirrors
    // `edge_endpoints_preserve_construction_order`/
    // `tree_edge_endpoints_preserve_construction_order`, deliberately
    // scrambled (not sorted by either endpoint, and not node-order either)
    // so an accidental reordering would be caught. n=4 undirected (never
    // tree-shaped), 3 edges: density = 3 / (4 choose 2) = 3/6 = 0.5 > 0.3,
    // no duplicate unordered pairs -- picks Dense.
    #[test]
    fn dense_edge_endpoints_preserve_construction_order() {
        test! {
            let from = vec![3, 1, 4];
            let to = vec![1, 4, 2];
            let g = GraphBackend::new(4, from.clone(), to.clone(), false);
            assert!(g.is_dense());
            let ends = g.edge_endpoints();
            let got_from: Vec<i32> = ends.dollar("from").unwrap().as_integer_vector().unwrap();
            let got_to: Vec<i32> = ends.dollar("to").unwrap().as_integer_vector().unwrap();
            assert_eq!(got_from, from);
            assert_eq!(got_to, to);
        }
    }

    // Self-loop/undirected-degree convention: S4 explicitly warns not to
    // assume `MatrixGraph` agrees with `Graph`'s "a loop counts twice"
    // convention (established for Repr::General by
    // `self_loop_counts_twice_in_undirected_degree`), in either direction.
    // `Repr::Dense` doesn't inherit the convention from `MatrixGraph`'s own
    // neighbour iteration at all -- `DenseData::out_neighbors`/`in_neighbors`
    // compute it from the retained `from`/`to` vectors instead, the same way
    // `GeneralData` does over its always-directed storage (see `DenseData`'s
    // doc comment for why: an `Undirected`-typed `MatrixGraph` has no
    // separate out/in adjacency to double-count through in the first place).
    // This test confirms/establishes the result of that deliberate choice:
    // n=2 undirected, self-loop on node 1 plus an ordinary edge to node 2 --
    // density = 2 / (2 choose 2) = 2/1 = 2.0 > 0.3, no duplicate pairs
    // ((1,1) and (1,2) are distinct canonical keys) -- picks Dense.
    #[test]
    fn dense_self_loop_counts_twice_in_undirected_degree() {
        test! {
            let g = GraphBackend::new(2, vec![1, 1], vec![1, 2], false);
            assert!(g.is_dense());
            assert_eq!(g.degree(1, "all"), 3); // loop (2) + edge to 2 (1)
            assert_eq!(g.degree(2, "all"), 1);
            let mut ns = g.neighbors(1, "all");
            ns.sort();
            assert_eq!(ns, vec![1, 1, 2]); // order isn't a contract, only the multiset is
        }
    }

    // `mode` semantics must match Repr::General/Repr::Tree exactly:
    // "out"/"in"/"all" for directed, invalid values panic identically.
    // n=3 directed, edges 1->2, 1->3, 2->3: node 1 has out-degree 2 (not
    // tree-shaped); density = 3 / (3 choose 2) = 3/3 = 1.0 > 0.3, no
    // duplicate ordered pairs -- picks Dense.
    #[test]
    fn dense_mode_semantics_match_other_variants() {
        test! {
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 3, 3], true);
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
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 3, 3], true);
            assert!(g.is_dense());
            g.neighbors(1, "sideways");
        }
    }

    #[test]
    #[should_panic(expected = "`mode` must be one of \"out\", \"in\", \"all\", not \"sideways\"")]
    fn dense_degree_invalid_mode_panics_like_other_variants() {
        test! {
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 3, 3], true);
            assert!(g.is_dense());
            g.degree(1, "sideways");
        }
    }

    // induced_subgraph()'s replication/drop logic, mirroring
    // `induced_subgraph_drops_dangling_and_clones_replicated` /
    // `induced_subgraph_treats_zero_as_no_source` above, for a Dense-shaped
    // input, and confirming (as those tests' own comments already establish
    // for General/Tree) that induced_subgraph() never constructs a
    // GraphBackend itself -- it returns plain lists, representation-
    // independent (see `induced_subgraph()`'s doc comment and `edge_list()`,
    // which `Repr::Dense` participates in like every other variant).
    #[test]
    fn dense_induced_subgraph_drops_dangling_and_clones_replicated() {
        test! {
            // Triangle 1->2->3->1 (directed cycle, so never Tree-shaped);
            // density = 3 / (3 choose 2) = 3/3 = 1.0 > 0.3, no duplicates --
            // picks Dense.
            let g = GraphBackend::new(3, vec![1, 2, 3], vec![2, 3, 1], true);
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
            // n=2 undirected (never tree-shaped); density = 1/(2 choose 2)
            // = 1/1 = 1.0 > 0.3, no duplicates -- picks Dense.
            let g = GraphBackend::new(2, vec![1], vec![2], false);
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
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 3, 3], false);
            assert!(!g.is_tree());
            assert!(g.is_dense());
        }
    }

    // Below DENSE_THRESHOLD (0.3): a 10-node directed cycle has out-degree
    // exactly 1 everywhere, which looks tree-like, but it never reaches a
    // root (`detect_tree()` rejects it as a cycle -- see its own doc
    // comment), so it falls through to the density check same as any other
    // non-tree graph. M=10, N choose 2 = 45, density = 10/45 ~= 0.222, below
    // threshold, and no duplicate edges -- picks Csr, not Dense.
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
            let g = GraphBackend::new(10, from, to, true);
            assert!(!g.is_tree());
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
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 2, 3], false);
            assert!(!g.is_tree());
            assert!(!g.is_dense());
            assert!(!g.is_csr());
            // Still a perfectly ordinary graph otherwise -- General answers
            // has_edge()/degree() for it exactly as it would for any input.
            assert!(g.has_edge(1, 2));
            assert_eq!(g.degree(1, "all"), 2); // two (1,2) edges, both counted
        }
    }

    // Tree/forest check happens strictly before the density check (S3's
    // order: tree/forest validator, then density, then otherwise general --
    // `_dev/petgraph_data_types.md` S5 "port... don't redesign"). This graph
    // is tree-shaped AND would also clear DENSE_THRESHOLD if the density
    // check ran on it (density = 1/(2 choose 2) = 1.0 > 0.3) -- proving the
    // tree check really does short-circuit before density is ever
    // evaluated, not just that both checks happen to agree. Also never
    // reaches the CSR check for the same reason (tree wins first).
    #[test]
    fn tree_shaped_graph_never_reaches_density_check() {
        test! {
            let g = GraphBackend::new(2, vec![2], vec![1], true);
            assert!(g.is_tree());
            assert!(!g.is_dense());
            assert!(!g.is_csr());
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
    // trivially here too (same reasoning as
    // `tree_node_identity_is_dense_and_stable`/
    // `dense_node_identity_is_dense_and_stable`), stated plainly per S4's
    // own instruction rather than skipped. n=8 directed path 1->2->3->4->5->6
    // plus 1->3 (node 1 has out-degree 2, so never tree-shaped); density =
    // 6/(8 choose 2) = 6/28 ~= 0.214, below DENSE_THRESHOLD, no duplicate
    // edges -- picks Csr.
    #[test]
    fn csr_node_identity_is_dense_and_stable() {
        test! {
            let from = vec![1, 2, 3, 4, 5, 1];
            let to = vec![2, 3, 4, 5, 6, 3];
            let g = GraphBackend::new(8, from, to, true);
            assert!(!g.is_tree());
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
    // risk this variant actually has for real (unlike Repr::Tree/Repr::Dense,
    // which had no separate storage order to permute against in the first
    // place): `Csr`'s native storage is source-then-target sorted, a real
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
            let g = GraphBackend::new(8, from.clone(), to.clone(), true);
            assert!(g.is_csr());
            let ends = g.edge_endpoints();
            let got_from: Vec<i32> = ends.dollar("from").unwrap().as_integer_vector().unwrap();
            let got_to: Vec<i32> = ends.dollar("to").unwrap().as_integer_vector().unwrap();
            assert_eq!(got_from, from);
            assert_eq!(got_to, to);
        }
    }

    // Self-loop/undirected-degree convention: S4 explicitly warns not to
    // assume any petgraph type agrees with `Graph`'s "a loop counts twice"
    // convention (established for Repr::General by
    // `self_loop_counts_twice_in_undirected_degree`) in either direction --
    // confirmed to actually differ for Repr::Dense's `MatrixGraph`
    // (`dense_self_loop_counts_twice_in_undirected_degree`'s comment), so
    // Repr::Csr gets its own check rather than inheriting the assumption.
    // `CsrData`'s doc comment predicts this holds "for free" here (a
    // self-loop is one arc `a -> a`, counted once by `csr.out_degree()` and
    // once more by the reverse (`in_ptr`/`in_idx`) index) -- this test is
    // what actually confirms that prediction rather than trusting the
    // arithmetic. n=6 undirected, self-loop on node 1 plus an ordinary edge
    // to node 2 (nodes 3-6 isolated, just to keep density below threshold):
    // density = 2/(6 choose 2) = 2/15 ~= 0.133 < 0.3, no duplicate pairs
    // ((1,1) and (1,2) are distinct canonical keys) -- picks Csr.
    #[test]
    fn csr_self_loop_counts_twice_in_undirected_degree() {
        test! {
            let g = GraphBackend::new(6, vec![1, 1], vec![1, 2], false);
            assert!(g.is_csr());
            assert_eq!(g.degree(1, "all"), 3); // loop (2) + edge to 2 (1)
            assert_eq!(g.degree(2, "all"), 1);
            let mut ns = g.neighbors(1, "all");
            ns.sort();
            assert_eq!(ns, vec![1, 1, 2]); // order isn't a contract, only the multiset is
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
            let g = GraphBackend::new(8, from, to, true);
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
            let g = GraphBackend::new(8, from, to, true);
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
            let g = GraphBackend::new(8, from, to, true);
            assert!(g.is_csr());
            g.degree(1, "sideways");
        }
    }

    // `parent()` panics on Repr::Csr the same way it does on Repr::General/
    // Repr::Dense (see `GraphBackend::parent()`'s `Repr::General(_) |
    // Repr::Dense(_) | Repr::Csr(_)` arm).
    #[test]
    #[should_panic(expected = "`parent()` is only defined when `is_tree()` is TRUE")]
    fn csr_parent_panics() {
        test! {
            let from = vec![1, 2, 3, 4, 5, 1];
            let to = vec![2, 3, 4, 5, 6, 3];
            let g = GraphBackend::new(8, from, to, true);
            assert!(g.is_csr());
            g.parent(1);
        }
    }

    // induced_subgraph()'s replication/drop logic, mirroring
    // `induced_subgraph_drops_dangling_and_clones_replicated` /
    // `induced_subgraph_treats_zero_as_no_source` above, for a Csr-shaped
    // input, and confirming (as those tests' own comments already establish
    // for General/Tree/Dense) that induced_subgraph() never constructs a
    // GraphBackend itself -- it returns plain lists, representation-
    // independent (see `induced_subgraph()`'s doc comment and `edge_list()`,
    // which `Repr::Csr` participates in like every other variant).
    #[test]
    fn csr_induced_subgraph_drops_dangling_and_clones_replicated() {
        test! {
            // 8-node directed path 1->2->3->4->5->6 plus 1->3 (never
            // Tree-shaped: node 1 has out-degree 2); density = 6/28 ~= 0.214
            // < 0.3, duplicate-free -- picks Csr.
            let from = vec![1, 2, 3, 4, 5, 1];
            let to = vec![2, 3, 4, 5, 6, 3];
            let g = GraphBackend::new(8, from, to, true);
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
            // n=6 undirected (never tree-shaped); density = 1/(6 choose 2)
            // = 1/15 ~= 0.067 < 0.3, no duplicates -- picks Csr.
            let g = GraphBackend::new(6, vec![1], vec![2], false);
            assert!(g.is_csr());
            // New node 1 has no source (sentinel 0); new node 2 <- old node 2.
            let remap = g.induced_subgraph(vec![0, 2]);
            let from: Vec<i32> = remap.dollar("from").unwrap().as_integer_vector().unwrap();
            assert!(from.is_empty());
        }
    }

    // -- CSR selection logic ------------------------------------------------

    // A duplicate-free, non-tree, below-density-threshold graph picks
    // Repr::Csr -- same shape/reasoning as `below_density_threshold_picks_csr_repr`
    // above, restated here alongside the rest of the selection-order tests.
    #[test]
    fn duplicate_free_below_density_threshold_picks_csr_repr() {
        test! {
            let from = vec![1, 2, 3, 4, 5, 1];
            let to = vec![2, 3, 4, 5, 6, 3];
            let g = GraphBackend::new(8, from, to, true);
            assert!(!g.is_tree());
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
            let g = GraphBackend::new(8, from, to, true);
            assert!(!g.is_tree());
            assert!(!g.is_dense());
            assert!(!g.is_csr());
            assert!(g.has_edge(1, 2));
            assert_eq!(g.degree(1, "out"), 3); // 1->2 (x2) + 1->3
        }
    }

    // A dense, duplicate-free graph picks Repr::Dense, not Repr::Csr --
    // confirms the ordering (tree check, then density check, then
    // CSR-eligibility, then general fallback): the density check runs
    // *before* the CSR check, so a graph that clears the density threshold
    // never reaches `detect_csr()` at all. Same input as
    // `dense_enough_duplicate_free_graph_picks_dense_repr` above (complete
    // undirected triangle, density = 3/(3 choose 2) = 1.0 > 0.3).
    #[test]
    fn dense_duplicate_free_graph_picks_dense_not_csr_repr() {
        test! {
            let g = GraphBackend::new(3, vec![1, 1, 2], vec![2, 3, 3], false);
            assert!(!g.is_tree());
            assert!(g.is_dense());
            assert!(!g.is_csr());
        }
    }

    // A tree-shaped graph never reaches the CSR check at all -- the tree
    // check runs first and short-circuits (same ordering point
    // `tree_shaped_graph_never_reaches_density_check` above makes for the
    // density check). This graph is tree-shaped AND would also be
    // duplicate-free and below the density threshold if either later check
    // ran on it, proving the tree check really does short-circuit before
    // the CSR check is ever evaluated, not just that the checks happen to
    // agree.
    #[test]
    fn tree_shaped_graph_never_reaches_csr_check() {
        test! {
            let g = GraphBackend::new(3, vec![1, 2], vec![2, 3], true);
            assert!(g.is_tree());
            assert!(!g.is_dense());
            assert!(!g.is_csr());
        }
    }
}
