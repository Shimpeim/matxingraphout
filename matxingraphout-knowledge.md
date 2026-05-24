# matxingraphout — Knowledge & Skills Reference

**Version:** 0.3.11 · **GitHub:** https://github.com/Shimpeim/matxingraphout
**Test suite:** 92 tests (run with `devtools::test()`)
**Dependencies:** zero (base R only; `shiny` + `DT` are optional Suggests)

---

## 1. Package purpose

Converts an **adjacency matrix** + **node-property table** into three outputs simultaneously:

| Output | File argument | Return slot |
|---|---|---|
| SVG graph | `svg_file` | `$svg` |
| SVG (clean — centroid markers hidden) | `clean_svg_file` | `$clean_svg` |
| Graphviz DOT source | `dot_file` | `$dot` |
| Mermaid flowchart source | `mermaid_file` | `$mermaid` |

The function also returns `$topology` (graph-theoretic analysis) and `$canvas` (coordinate offsets for Shiny).

---

## 2. Source file map

| File | Role |
|---|---|
| `R/graph_to_outputs.R` | Main exported function; topology analysis; layout engine dispatch; calls SVG/DOT/Mermaid builders |
| `R/svg_builder.R` | SVG rendering: node shapes, edge arcs, arrowheads, labels, legend; `.dasharray()`, `.ep_lookup()` helpers |
| `R/format_builders.R` | DOT + Mermaid builders; `.dot_build()`, `.mmd_build()`, `.dot_lt()` helpers |
| `R/run_app.R` | `run_app()` launcher for the Shiny interface |
| `inst/shinyapp/app.R` | Full Shiny app (UI + server) |
| `man/graph_to_outputs.Rd` | Roxygen-generated docs — do **not** edit by hand |
| `tests/testthat/test-graph_to_outputs.R` | Full test suite |

---

## 3. Function signature

```r
graph_to_outputs(
  # ── Required ──────────────────────────────────────────────────
  adj_matrix,                        # numeric matrix, square, named rows+cols
  node_props,                        # data.frame — see §4

  # ── Output files ──────────────────────────────────────────────
  directed           = TRUE,
  svg_file           = "graph.svg",
  clean_svg_file     = "graph_clean.svg",
  dot_file           = "graph.dot",
  mermaid_file       = "graph.mmd",

  # ── Canvas / typography ────────────────────────────────────────
  svg_padding        = 40,
  default_width      = 100,
  default_height     = 44,
  default_fontsize   = 12,
  default_fontcolour = "#222222",
  default_stroke     = "#333333",

  # ── Structural edges ──────────────────────────────────────────
  edge_colour        = "#444444",
  edge_width         = 1.5,
  edge_curvature     = "auto",       # "auto" | "straight"
  edge_labels        = NULL,         # character matrix, same dims as adj_matrix
  edge_props         = NULL,         # data.frame(weight, colour, width, linetype, label)

  # ── Overlay edges ─────────────────────────────────────────────
  adj_overlay             = NULL,
  overlay_edge_colour     = "#999999",
  overlay_edge_width      = 1,
  overlay_edge_style      = "dashed",    # "dashed" | "solid"
  overlay_edge_curvature  = "auto",
  overlay_edge_labels     = NULL,
  overlay_edge_props      = NULL,

  # ── Layout ────────────────────────────────────────────────────
  layout             = "manual",     # see §6
  sunburst_max_depth = 3L,
  sunburst_min_branching = 3,
  circle_r           = NULL,
  circle_cx          = NULL,
  circle_cy          = NULL,

  # ── Curvature / centroids ──────────────────────────────────────
  centroids          = NULL,         # data.frame(label, x, y) or NULL → hub mode
  show_centroids     = TRUE,

  # ── Legend ────────────────────────────────────────────────────
  show_legend        = FALSE,
  legend_node_shape  = NULL,         # data.frame(shape, label)
  legend_node_colour = NULL,         # data.frame(colour, label)
  legend_title       = "Legend"
)
```

### Return value

```r
invisible(list(
  svg       = "<svg>…</svg>",          # full SVG string
  clean_svg = "<svg>…</svg>",          # same but centroid markers opacity="0"
  dot       = "digraph { … }",
  mermaid   = "flowchart LR\n…",
  topology  = list(…),                 # see §9
  canvas    = list(xlo = …, ylo = …)  # coordinate offsets for Shiny click mapping
))
```

---

## 4. `node_props` columns

| Column | Required | Type | Notes |
|---|---|---|---|
| `id` | yes | character | Must match `rownames(adj_matrix)` and `colnames(adj_matrix)` |
| `label` | no | character | Display text; `\n` in value → line break in SVG |
| `shape` | no | character | `"rect"` (default) · `"rounded"` · `"circle"` · `"ellipse"` · `"diamond"` |
| `colour` / `color` | no | hex string | Node fill colour (both spellings accepted) |
| `fontcolour` / `fontcolor` | no | hex string | Label font colour (both spellings accepted) |
| `stroke` | no | hex string | Border colour |
| `fontsize` | no | numeric | Per-node font size override |
| `width` | no | numeric | Node width in SVG units |
| `height` | no | numeric | Node height in SVG units |
| `x` | required for `layout="manual"` | numeric | x-coordinate (SVG/user space) |
| `y` | required for `layout="manual"` | numeric | y-coordinate (SVG/user space) |

**Colour aliasing:** `colour`/`color` and `fontcolour`/`fontcolor` are interchangeable — the package aliases them internally.

---

## 5. Two-matrix architecture

| Matrix | Role |
|---|---|
| `adj_matrix` | **Structural edges** — used for topology analysis, eigenvector centrality, and layout engine |
| `adj_overlay` | **Annotation edges** — rendered on top (dashed by default), **excluded** from all topology analysis and centrality |

Only `adj_matrix` determines graph classification and automatic layout. Both are rendered in SVG.

**Matrix format:** square numeric matrix; `rownames` = from-nodes, `colnames` = to-nodes; cell value = edge weight (0 = no edge, non-zero = edge; weight value available to `edge_props` lookup).

---

## 6. Layout modes

| `layout` value | Description |
|---|---|
| `"manual"` *(default)* | Uses `x`, `y` columns in `node_props`; coordinates are taken as-is |
| `"auto"` | Selects automatically: sunburst for shallow DAGs with high branching; tree for other acyclic; bipartite for non-acyclic bipartite; circular otherwise |
| `"sunburst"` | Radial tree from root; depth controlled by `sunburst_max_depth`, branching by `sunburst_min_branching` |
| `"tree"` | Layered top-down tree layout |
| `"bipartite"` | Two-column bipartite layout |
| `"circular"` | Nodes equally spaced on a circle; `circle_r`, `circle_cx`, `circle_cy` override radius/centre |

For non-manual layouts, `x`/`y` in `node_props` are optional and ignored.

---

## 7. Edge curvature algorithm

### `edge_curvature` / `overlay_edge_curvature`

Values: `"straight"` → straight lines; `"auto"` → arc (default).

#### Mode A — centroid mode (when `centroids` is a non-empty data.frame)

For each edge P1→P2:
1. Compute midpoint M = ((P1x+P2x)/2, (P1y+P2y)/2)
2. Find the centroid C with minimum Euclidean distance to M
3. Use C as the arc origin O for that edge's circumscribed-circle arc
4. Falls back to straight line when O, P1, P2 are collinear

Both structural and overlay edges independently pick the nearest centroid from the same `centroids` set.

#### Mode B — hub mode (when `centroids` is NULL, legacy default)

1. Compute eigenvector centrality on `adj_matrix` only (symmetrised; power iteration ≤ 200 steps) via `.eigenvector_centrality()`
2. Identify hub node O = node with highest centrality
3. Use O as global arc origin for **all** edges
4. Falls back to straight line when O, P1, P2 are collinear

**Implementation location:** mode selection in `graph_to_outputs.R`; per-edge arc drawing inline in `svg_builder.R`.

### `centroids` parameter

```r
centroids = data.frame(
  label = c("Centre A", "Centre B"),  # display label (optional)
  x     = c(300, 700),
  y     = c(400, 400)
)
```

`show_centroids = TRUE` (default) draws red crosshair circle markers at centroid positions in the SVG; each has a `data-centroid-idx` attribute for JS interaction. `$clean_svg` has all centroid markers at `opacity="0"`.

---

## 8. `edge_props` / `overlay_edge_props`

Maps adjacency-matrix **weight values** to per-edge visual properties.

```r
edge_props = data.frame(
  weight   = c(1, 2, 3),          # must match values in adj_matrix
  colour   = c("#444444", "#e74c3c", "#2ecc71"),
  width    = c(1.5, 2.5, 2.0),
  linetype = c("solid", "dashed", "dotted"),
  label    = c("weak", "strong", "medium")   # edge label text (optional)
)
```

**Linetype values:** `"solid"` · `"dashed"` · `"dotted"` · `"longdash"` · `"twodash"`

**Lookup logic:** helper `.ep_lookup(ep, v, def_col, def_w, def_lt)` in `svg_builder.R`:
- If `edge_props` is NULL or weight not found → falls back to `edge_colour`, `edge_width`, solid
- Non-default colours generate extra `<marker>` elements as `ah-COLORHEX` / `ahov-COLORHEX`

**DOT/Mermaid:** per-edge `color`, `penwidth`, `style` attributes in DOT; `-.->`/`-.-` operator for non-solid Mermaid edges.

---

## 9. `edge_labels` / `overlay_edge_labels`

Character matrix with same dimensions as `adj_matrix`:

```r
edge_labels <- matrix("", nrow = n, ncol = n,
                      dimnames = list(rownames(adj_matrix), colnames(adj_matrix)))
edge_labels["A", "B"] <- "influences"
edge_labels["B", "C"] <- "critiques\non realism"   # \n → line break in SVG
```

Priority rule: explicit `edge_labels[i,j]` text takes precedence over weight-based annotation. Weight is shown only when `edge_labels` is NULL or cell is empty/NA **and** weight ≠ 1.

---

## 10. Legend

```r
show_legend        = TRUE,
legend_title       = "Legend",
legend_node_shape  = data.frame(
  shape = c("rounded", "diamond"),
  label = c("Thinker / position", "Key publication")
),
legend_node_colour = data.frame(
  colour = c("#e8f4fd", "#e8f5e9", "#fce4ec"),
  label  = c("Analytic epistemology", "Social epistemology", "Feminist epistemology")
)
```

Legend sections rendered in SVG:
1. **Node shapes** — from `legend_node_shape`
2. **Node colours** — from `legend_node_colour`
3. **Edge types** — auto-derived from `edge_props` (linetype + colour samples)

Canvas height is extended by `leg_h` before the SVG header is emitted.

---

## 11. Topology analysis (`$topology`)

| Field | Type | Meaning |
|---|---|---|
| `type` | character | Graph type label (e.g. `"DAG"`, `"cyclic"`, `"tree"`) |
| `recommended_layout` | character | Suggested `layout` value |
| `n_nodes` | integer | Number of nodes |
| `n_edges` | integer | Number of structural edges |
| `density` | numeric | Edge density |
| `max_depth` | integer | Maximum depth from roots |
| `avg_branching_factor` | numeric | Average out-degree of non-leaf nodes |
| `is_acyclic` | logical | TRUE if DAG |
| `is_weakly_connected` | logical | |
| `is_strongly_connected` | logical | |
| `is_bipartite` | logical | |
| `is_tree` | logical | |
| `is_forest` | logical | |
| `n_strongly_connected_components` | integer | |
| `root_nodes` | character vector | Nodes with in-degree 0 |
| `leaf_nodes` | character vector | Nodes with out-degree 0 |
| `in_degree` | named integer vector | Per-node in-degree |
| `out_degree` | named integer vector | Per-node out-degree |

Topology analysis runs on `adj_matrix` only — `adj_overlay` is excluded.

---

## 12. Shiny app

Launch with:
```r
matxingraphout::run_app()
# or from package root:
shiny::runApp("inst/shinyapp/app.R")
```

### UI panels

| Panel / tab | Function |
|---|---|
| **Nodes** | DT table: id, label, shape, colour, fontcolour, stroke, x, y, width, height. `\n` in Label cell → line break. Add/remove row buttons. |
| **Adjacency** | Numeric matrix grid (up to 15 nodes) |
| **Overlay** | Enabled via checkbox; same matrix grid for `adj_overlay` |
| **Edge Labels** | String matrix grid for `edge_labels` (same dims); CSV import |
| **Edge Properties** | DT table for `edge_props` (weight, colour, width, linetype, label); CSV import |
| **Overlay Edge Properties** | DT table for `overlay_edge_props`; CSV import |
| **Centroids** | DT table (label, x, y); add/remove; interactive click-to-place on rendered SVG |
| **Settings** | directed, layout, padding, defaults, edge style, curvature, overlay style |
| **Legend** | show_legend checkbox, legend_title, legend_node_shape DT, legend_node_colour DT, auto-populate buttons |
| **Import CSV** | Batch upload + individual file inputs (see §13) |
| **R Code** | Live-generated `graph_to_outputs()` call reproducing current state |
| **SVG Preview** | Rendered output; rulers on x/y axes (HiDPI canvas elements) |
| **Downloads** | SVG, clean SVG (no centroid markers), DOT, Mermaid |

### Interactive centroid placement

- "Place centroid" toggle button → click anywhere on SVG → centroid added at that position (JS-side marker drawn instantly; row added to centroid DT)
- "Remove centroid" toggle button → click existing crosshair marker → row removed
- `canvas_offset` Shiny custom message carries `xlo`/`ylo` after each render
- `MutationObserver` on `#svg-inner` reattaches click handlers and refreshes rulers on every re-render

---

## 13. CSV import — formats and file suffix routing

### Batch import (multi-file upload)

Upload multiple CSVs at once via the **batch file input**. Files are routed by filename suffix (case-insensitive):

| Filename suffix | Loaded as |
|---|---|
| `_adj.csv` | Adjacency matrix |
| `_node_props.csv` | Node properties |
| `_edge_labels.csv` | Edge label matrix |
| `_edge_props.csv` or `edge_props.csv` | Structural edge properties |
| `_overlay.csv` | Overlay adjacency matrix |
| `_overlay_edge_props.csv` | Overlay edge properties |
| `_settings.csv` | Settings key-value table |
| `_legend_shapes.csv` | Legend node shapes |
| `_legend_colours.csv` or `_legend_colors.csv` | Legend node colours |

Settings are applied first, then node_props, then matrices, to ensure correct node count before matrix parsing.

### `_node_props.csv`

Standard CSV with headers matching `node_props` column names:

```
id,label,shape,colour,fontcolour,stroke,x,y,width,height
A,"Node A\nLine 2",rounded,#e8f4fd,#222222,#a0c8e8,100,200,160,50
B,"Node B",diamond,#fce4ec,#222222,#e8a8b8,300,200,160,50
```

- `\n` in label cells → line break in SVG (converted by app before rendering)
- `colour`/`color` and `fontcolour`/`fontcolor` both accepted

### `_adj.csv` / `_overlay.csv`

Matrix CSV — **first column** = from-node IDs, **remaining column headers** = to-node IDs:

```
,A,B,C,D
A,0,1,0,0
B,0,0,1,1
C,0,0,0,0
D,0,0,1,0
```

- Cell values: numeric (0 = no edge; non-zero = edge weight, used by `edge_props` lookup)
- First cell (top-left) is empty or any value — ignored

### `_edge_labels.csv`

Same matrix layout as `_adj.csv` but string cell values:

```
,A,B,C,D
A,,"influences",,
B,,,"critiques\non realism","builds on"
C,,,,
D,,,"extends",
```

- Empty cells → no label
- `\n` in cell values → multi-line edge label in SVG
- Must be CSV-quoted if the label contains commas

### `_settings.csv`

Two-column CSV (`Setting`, `Value`):

```
Setting,Value
layout,manual
directed,TRUE
default_width,160
default_height,50
default_fontsize,10
default_fontcolour,#222222
default_stroke,#cccccc
edge_colour,#666666
edge_width,1.2
show_legend,TRUE
legend_title,My Graph Legend
```

Supported `Setting` keys: `layout`, `directed`, `svg_padding`, `default_width`, `default_height`, `default_fontsize`, `default_fontcolour`, `default_stroke`, `edge_colour`, `edge_width`, `overlay_edge_colour`, `overlay_edge_width`, `overlay_edge_style`, `edge_curvature`, `overlay_edge_curvature`, `show_legend`, `legend_title`

### `_edge_props.csv` / `_overlay_edge_props.csv`

```
weight,colour,width,linetype,label
1,#444444,1.5,solid,
2,#e74c3c,2.5,dashed,strong link
3,#2ecc71,2.0,dotted,weak link
```

All columns required; `label` can be empty. `linetype` values: `solid`, `dashed`, `dotted`, `longdash`, `twodash`.

### `_legend_shapes.csv`

```
shape,label
rounded,Thinker / theoretical position
diamond,Key publication
```

### `_legend_colours.csv`

```
colour,label
#e8f4fd,Analytic epistemology (Gettier · Code · Sosa)
#e8f5e9,Social epistemology (Goldman · Longino · Kitcher)
#fce4ec,Feminist epistemology (Harding · Haraway · Fricker)
```

---

## 14. Multi-line labels

Both node labels and edge labels support line breaks via literal `\n` (two characters: backslash + n):

- **In the Shiny UI:** type `\n` in any node Label cell → converted to actual newline before rendering
- **In CSV files:** write `\n` as a literal string in the cell value (no quoting needed unless the cell also contains commas)
- **In R code:** use `\n` inside a character string: `"Line 1\nLine 2"`
- **In SVG output:** rendered as `<tspan>` elements stacked vertically within a `<text>` element

---

## 15. Typical R workflow

```r
library(matxingraphout)

# Build adjacency matrix
nodes <- c("A", "B", "C", "D")
adj <- matrix(0, nrow=4, ncol=4, dimnames=list(nodes, nodes))
adj["A","B"] <- 1; adj["B","C"] <- 2; adj["B","D"] <- 1; adj["C","D"] <- 1

# Node properties
np <- data.frame(
  id         = nodes,
  label      = c("Start\nNode A", "Hub B", "Node C", "End D"),
  shape      = c("rounded","rounded","rounded","rounded"),
  colour     = c("#e8f4fd","#e8f5e9","#fce4ec","#fff3e0"),
  fontcolour = "#222222",
  stroke     = "#999999",
  x          = c(100, 300, 500, 500),
  y          = c(300, 300, 200, 400),
  width      = 140,
  height     = 50,
  stringsAsFactors = FALSE
)

# Edge labels
el <- matrix("", 4, 4, dimnames=list(nodes,nodes))
el["A","B"] <- "initiates"
el["B","C"] <- "leads to\nphase 2"

# Edge visual properties (keyed to weight values)
ep <- data.frame(
  weight   = c(1, 2),
  colour   = c("#444444", "#e74c3c"),
  width    = c(1.5, 2.5),
  linetype = c("solid", "dashed"),
  label    = c("", "strong")
)

# Run
out <- graph_to_outputs(
  adj_matrix   = adj,
  node_props   = np,
  directed     = TRUE,
  layout       = "manual",
  edge_labels  = el,
  edge_props   = ep,
  show_legend  = TRUE,
  legend_title = "My Graph",
  legend_node_shape  = data.frame(shape="rounded", label="Node"),
  legend_node_colour = data.frame(
    colour = c("#e8f4fd","#e8f5e9","#fce4ec","#fff3e0"),
    label  = c("Group A","Group B","Group C","Group D")
  )
)

# Access outputs
cat(out$dot)
writeLines(out$svg, "graph.svg")
out$topology$type          # e.g. "DAG"
out$topology$root_nodes    # "A"
```

---

## 16. Developer workflow

```r
# Regenerate docs after editing @param blocks in R/graph_to_outputs.R
devtools::document()

# Run all tests
devtools::test()

# Run single test file
testthat::test_file("tests/testthat/test-graph_to_outputs.R")

# Install from GitHub
remotes::install_github("Shimpeim/matxingraphout", force = TRUE)
# If lock-directory error: remove /path/to/R/library/00LOCK-matxingraphout
```

**Design constraints:**
- Zero `Imports:` in DESCRIPTION — no external runtime dependencies
- When adding a parameter: update (1) function signature, (2) roxygen `@param` block, (3) test suite, (4) Shiny app UI/server, (5) `output$rcode_out` code generation, (6) `_settings.csv` parsing if it's a settings-level parameter

---

## 17. Known edge cases and fixes

| Issue | Resolution |
|---|---|
| Multi-line edge labels with `\n` not breaking in SVG | Bug fixed in `svg_builder.R` (v0.3.11): edge label text now split on `\n` and rendered as stacked `<tspan>` elements, same as node labels |
| Shiny: edge labels CSV import silently dropped after any node-list change | Bug fixed in `app.R`: `sync_ids()` was resizing `rv$adj` and `rv$overlay` but not `rv$edge_labels`; after a node add/remove/re-import the dim check failed and the fallback grid loop found nothing (no grid exists for n > 15) → labels lost. Fixed by adding `resize_label_matrix()` and calling it from `sync_ids()`. |
| Shiny: manual grid edits to edge labels ignored after CSV import (n ≤ 15) | Bug fixed in `app.R`: the render button used `rv$edge_labels` directly when its dims matched, skipping grid inputs entirely. Rewritten to mirror the adj matrix pattern — grid inputs are read first; `rv$edge_labels` fills in only where the grid input is absent or empty. |
| Dropbox sync modifies file between Read and Write | Write complete content to `/tmp/file.md` via Write tool, then `cp` to Dropbox destination — bypasses mtime check |
| `00LOCK-matxingraphout` error on `install_github` | `rm -rf /path/to/R/library/00LOCK-matxingraphout` |
| `colour`/`color` column both supplied in node_props | Package aliases internally — last one wins; use consistently |
| `adj_overlay` edges appearing in centrality calc | Confirmed excluded: `.eigenvector_centrality()` receives `adj_matrix` only |
| Edge labels CSV column misalignment in 22×30+ matrices | Use Python `csv.writer` to generate large matrices programmatically rather than hand-counting commas |

---

## 18. Example CSV file set (naming convention)

For a project named `my_graph`, upload all at once as a batch:

```
my_graph_node_props.csv        ← node properties table
my_graph_adj.csv               ← structural adjacency matrix
my_graph_edge_labels.csv       ← edge label matrix
my_graph_edge_props.csv        ← edge visual properties (weight → style)
my_graph_overlay.csv           ← overlay adjacency matrix (optional)
my_graph_overlay_edge_props.csv← overlay edge properties (optional)
my_graph_settings.csv          ← layout/style settings
my_graph_legend_shapes.csv     ← legend: node shapes
my_graph_legend_colours.csv    ← legend: node colours
```
