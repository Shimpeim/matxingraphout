# CLAUDE.md — matxingraphout dev context

## Package overview

**matxingraphout** converts an adjacency matrix + node-property table into SVG,
Graphviz DOT, and Mermaid flowchart outputs. No external R package dependencies.

- Version: 0.3.8
- GitHub: https://github.com/Shimpeim/matxingraphout
- Test suite: 86 tests, all passing (run with `devtools::test()`)

---

## File map

| File | Role |
|---|---|
| `R/graph_to_outputs.R` | Main exported function `graph_to_outputs()`; topology analysis; layout engines; calls SVG/DOT/Mermaid builders |
| `R/svg_builder.R` | SVG rendering: node shapes, edge arcs, arrowheads, labels, legend; `.dasharray()`, `.ep_lookup()` helpers |
| `R/format_builders.R` | DOT and Mermaid output builders; `.dot_build()`, `.mmd_build()`, `.dot_lt()` |
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

**`"auto"` algorithm — two modes, selected by whether `centroids` is supplied:**

**Mode A — centroid mode** (`centroids` is a non-empty data.frame):
1. For each edge P1→P2, compute midpoint M = ((P1x+P2x)/2, (P1y+P2y)/2)
2. Find the centroid C with minimum Euclidean distance to M
3. Use C as arc origin O for that edge's circumscribed-circle arc
4. Falls back to straight line when O, P1, P2 are collinear
5. Both structural and overlay edges use the same centroid set (each independently picks its nearest centroid)

**Mode B — hub mode** (`centroids` is NULL, legacy default):
1. Compute eigenvector centrality on `adj_matrix` only (symmetrised, power iteration ≤ 200 steps) via `.eigenvector_centrality(adj_matrix)`
2. Identify hub node O = node with highest centrality score
3. Use O as global arc origin for all edges
4. Falls back to straight line when O, P1, P2 are collinear

**Where it lives:**
- `graph_to_outputs.R`: decides mode (centroids vs hub), passes both `radial_center` and `centroids` to `.svg_build()`; only one is non-NULL at a time
- `svg_builder.R`: per-edge dispatch — `use_centroids` flag selects nearest-centroid lookup vs global `rc_sx/rc_sy`; logic is in-line in both edge loops (structural and overlay)

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

## Function signature (v0.3.8)

```r
graph_to_outputs(
  adj_matrix,
  node_props,
  directed           = TRUE,
  svg_file           = "graph.svg",
  clean_svg_file     = "graph_clean.svg",  # 2nd SVG: centroid markers at opacity 0
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
  overlay_edge_curvature = "auto",
  centroids              = NULL,     # data.frame(label, x, y) or NULL → hub mode
  show_centroids         = TRUE,     # draw centroid crosshair markers in SVG
  edge_labels            = NULL,     # character matrix — explicit labels for structural edges
  overlay_edge_labels    = NULL,     # character matrix — explicit labels for overlay edges
  edge_props             = NULL,     # data.frame(weight, colour, width, linetype, label)
  overlay_edge_props     = NULL,     # same structure for overlay edges
  show_legend            = FALSE,    # append SVG legend block below graph
  legend_node_shape      = NULL,     # data.frame(shape, label) for legend shape section
  legend_node_colour     = NULL,     # data.frame(colour, label) for legend colour section
  legend_title           = "Legend"
)
```

Return value: invisible named list with `$svg`, `$dot`, `$mermaid`, `$clean_svg`, `$topology`, `$canvas`
- `$clean_svg` — same as `$svg` but centroid marker `<g>` elements have `opacity="0"` (for clean export)
- `$canvas = list(xlo, ylo)` — canvas-to-original-coordinate offsets for Shiny

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

## Recent changes (v0.3.8)

- Added `edge_props` parameter: `data.frame(weight, colour, width, linetype, label)`. Maps `adj_matrix` weight values to per-edge visual properties. Unmatched weights fall back to `edge_colour`/`edge_width`/solid. Linetype values: `"solid"` (default), `"dashed"`, `"dotted"`, `"longdash"`, `"twodash"`. Rendered in SVG (with correct `stroke-dasharray` and per-colour arrowhead markers), DOT, and Mermaid.
- Added `overlay_edge_props` parameter: same structure, applied to overlay edges.
- Added `show_legend` parameter (`FALSE`): when `TRUE`, appends an SVG legend block below the graph canvas. Legend sections: node shapes (`legend_node_shape`), node colours (`legend_node_colour`), edge types (auto-derived from `edge_props`). Canvas height extended by `leg_h` before emitting SVG header.
- Added `legend_node_shape` parameter: `data.frame(shape, label)` for legend shape section.
- Added `legend_node_colour` parameter: `data.frame(colour, label)` for legend colour section.
- Added `legend_title` parameter (default `"Legend"`): title text for legend block.
- `svg_builder.R`: added `.dasharray(lt)` helper (linetype → `stroke-dasharray`); added `.ep_lookup(ep, v, def_col, def_w, def_lt)` helper; extra arrowhead markers generated as `ah-COLORHEX` / `ahov-COLORHEX` for non-default colours.
- `format_builders.R`: added `.dot_lt()` helper; per-edge DOT attrs (`color`, `penwidth`, `style`); non-solid Mermaid operator (`-.->`/`-.-`) for overlay.
- Shiny app: new "Structural Edge Properties" and "Overlay Edge Properties" DT panels (columns: weight, colour, width, linetype, label); CSV import for both; Legend section in Settings with shape/colour DT tables and auto-populate buttons (`auto_ls`, `auto_lc`); `rcode_out` includes `edge_props` block and legend args.
- Test suite: 86 tests (5 new edge_props/legend tests added).

## Previous changes (v0.3.7)

- Added `edge_labels` parameter: character matrix (same dims as `adj_matrix`). Non-NA/non-empty cell `[i,j]` provides an explicit text label for that structural edge; takes priority over the weight-based annotation (weight shown only when `edge_labels` is NULL or the cell is empty/NA and weight ≠ 1). Rendered in SVG, DOT, and Mermaid.
- Added `overlay_edge_labels` parameter: same semantics for overlay edges.
- Shiny app: new "Edge Labels" panel with text-input matrix grid (up to 15 nodes) and CSV import
- Shiny app: `\n` typed in any node Label cell is converted to an actual newline before rendering (multi-line node labels); usage note shown in the Nodes panel
- Shiny app: new "Import CSV" panel with five file inputs:
  - **Node properties**: standard CSV with headers matching `node_props` column names
  - **Adjacency / Overlay matrix**: first column = from-node IDs, remaining column headers = to-node IDs; numeric values
  - **Edge labels**: same matrix CSV format but string cell values
  - **Settings**: 2-column CSV (`Setting`, `Value`) for all layout/style parameters
- Shiny app: `output$rcode_out` now includes the `edge_labels` matrix block when labels are defined
- Test suite: 78 tests (5 new edge-label tests added)

## Previous changes (v0.3.6)

- Added `centroids` parameter to `graph_to_outputs()` and `.svg_build()`
- When `centroids` is a non-empty data.frame(label, x, y): each edge picks its nearest centroid (by midpoint distance) as arc origin → per-region curvature
- When `centroids` is NULL: falls back to eigenvector-centrality hub node (legacy)
- Both structural and overlay edges use the same centroid set independently
- Added `show_centroids = TRUE` parameter: draws red crosshair circle markers at centroid positions in the SVG (each with `data-centroid-idx` attribute for JS interaction)
- Return value now includes `$canvas = list(xlo, ylo)` — the canvas coordinate offsets needed to convert SVG click positions back to original node-coordinate space
- Shiny app: new "Centroids" DT panel with add/remove; R Code tab outputs centroid data.frame when defined
- Added `clean_svg_file = "graph_clean.svg"` parameter: writes a second SVG identical to the main one but with all centroid markers at `opacity="0"` (via `gsub` on the SVG string); also returned as `$clean_svg` in the list
- Shiny app: "Download SVG (no centroids)" button serves the clean SVG
- Shiny app: interactive centroid placement via mouse click on rendered SVG
  - "Place centroid" button (toggle): click SVG → adds centroid row, draws temporary JS-side marker
  - "Remove centroid" button (toggle): click existing centroid marker → removes row
  - Rulers drawn on X and Y axes of graph using `<canvas>` elements (HiDPI-aware, labeled with original coordinates)
  - `canvas_offset` custom message handler sends `xlo/ylo` after each render; `MutationObserver` on `#svg-inner` triggers ruler update + click handler reattachment

## Previous changes (v0.3.5)

- Added `edge_curvature` and `overlay_edge_curvature` arguments
- Arc origin set to hub node (highest eigenvector centrality on `adj_matrix` only)
- Removed `arc_n` argument (earlier centroid-of-N-nearest-nodes approach, abandoned)
- Removed `.arc_origin()` helper; curvature logic is now inline in `svg_builder.R`
- `adj_overlay` edges excluded from centrality calculation (confirmed correct)
