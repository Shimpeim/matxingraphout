# CLAUDE.md — matxingraphout dev context

## Package overview

**matxingraphout** converts an adjacency matrix + node-property table into SVG,
Graphviz DOT, and Mermaid flowchart outputs. No external R package dependencies.

- Version: 0.3.5
- GitHub: https://github.com/Shimpeim/matxingraphout
- Test suite: 73 tests, all passing (run with `devtools::test()`)

---

## File map

| File | Role |
|---|---|
| `R/graph_to_outputs.R` | Main exported function `graph_to_outputs()`; topology analysis; layout engines; DOT/Mermaid builders; calls SVG builder |
| `R/svg_builder.R` | SVG rendering: node shapes, edge arcs, arrowheads, labels |
| `man/graph_to_outputs.Rd` | Roxygen-generated docs (do not edit by hand) |
| `tests/testthat/test-graph_to_outputs.R` | Full test suite |

---

## Key architecture decisions

### Two matrices: `adj_matrix` vs `adj_overlay`

- `adj_matrix` — structural edges: used for topology analysis, layout, and eigenvector centrality
- `adj_overlay` — annotation edges: drawn on top (dashed by default), **excluded** from all topology analysis and centrality calculations
- Both are rendered in SVG; only `adj_matrix` affects graph classification and layout

### Edge curvature (`edge_curvature` / `overlay_edge_curvature`)

Values: `"auto"` (default) or `"straight"`.

**`"auto"` algorithm:**
1. Compute eigenvector centrality on `adj_matrix` only (symmetrised, power iteration ≤ 200 steps) via `.eigenvector_centrality(adj_matrix)` — internal helper in `graph_to_outputs.R`
2. Identify hub node O = node with highest centrality score
3. For each edge P1→P2: fit the circumscribed circle of triangle O–P1–P2; draw the arc of that circle from P1 to P2 that avoids O
4. Falls back to straight line when O, P1, P2 are collinear (no finite circle)
5. `overlay_edge_curvature` applies the same logic independently to overlay edges

**Where it lives in `svg_builder.R`:**
- `radial_center` is computed from hub node position and passed globally
- Arc drawing is handled inside the edge-rendering loop using the circumscribed-circle formula
- The `.arc_origin()` helper was **removed** in v0.3.5; curvature logic is inline

### Layout modes

`layout` argument: `"manual"` (default), `"auto"`, `"sunburst"`, `"tree"`, `"bipartite"`, `"circular"`.

- `"auto"` chooses based on topology: sunburst for shallow DAGs with high branching, tree for other acyclic, bipartite for non-acyclic bipartite, circular otherwise
- `"manual"` requires `x`, `y` columns in `node_props`
- Other modes compute positions automatically; `x`/`y` columns optional

### Node shapes

`"rect"`, `"rounded"`, `"circle"`, `"ellipse"`, `"diamond"`

### Colour column aliasing

`node_props` accepts both `"colour"` and `"color"` (and `"fontcolour"` / `"fontcolor"`).

---

## Function signature (v0.3.5)

```r
graph_to_outputs(
  adj_matrix,
  node_props,
  directed           = TRUE,
  svg_file           = "graph.svg",
  dot_file           = "graph.dot",
  mermaid_file       = "graph.mmd",
  svg_padding        = 40,
  default_width      = 100,
  default_height     = 44,
  default_fontsize   = 12,
  default_fontcolour = "#222222",
  default_stroke     = "#333333",
  edge_colour        = "#444444",
  edge_width         = 1.5,
  adj_overlay        = NULL,
  overlay_edge_colour= "#999999",
  overlay_edge_width = 1,
  overlay_edge_style = "dashed",     # "dashed" or "solid"
  layout             = "manual",
  sunburst_max_depth = 3L,
  sunburst_min_branching = 3,
  circle_r           = NULL,
  circle_cx          = NULL,
  circle_cy          = NULL,
  edge_curvature         = "auto",   # "auto" or "straight"
  overlay_edge_curvature = "auto"
)
```

Return value: invisible named list with `$svg`, `$dot`, `$mermaid`, `$topology`.

---

## Topology analysis fields (`$topology`)

`type`, `recommended_layout`, `max_depth`, `avg_branching_factor`,
`n_nodes`, `n_edges`, `density`, `is_acyclic`, `is_weakly_connected`,
`is_strongly_connected`, `is_bipartite`, `is_tree`, `is_forest`,
`n_strongly_connected_components`, `root_nodes`, `leaf_nodes`,
`in_degree`, `out_degree`

---

## Workflow notes

- Roxygen docs: regenerate with `devtools::document()` after editing `@param` blocks
- Tests: `devtools::test()` or `testthat::test_file("tests/testthat/test-graph_to_outputs.R")`
- No `Imports:` in DESCRIPTION — zero external dependencies is a design constraint
- When adding arguments: update function signature, roxygen block, and tests

---

## Recent changes (v0.3.5)

- Added `edge_curvature` and `overlay_edge_curvature` arguments
- Arc origin set to hub node (highest eigenvector centrality on `adj_matrix` only)
- Removed `arc_n` argument (earlier centroid-of-N-nearest-nodes approach, abandoned)
- Removed `.arc_origin()` helper; curvature logic is now inline in `svg_builder.R`
- `adj_overlay` edges excluded from centrality calculation (confirmed correct)
