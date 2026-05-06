#' Convert an adjacency matrix and node properties to SVG, DOT, and Mermaid
#'
#' Takes a square adjacency matrix and a node-property table and writes (or
#' returns) three graph representations: a rendered SVG file, Graphviz DOT
#' source, and Mermaid flowchart source.  No external package dependencies.
#'
#' @param adj_matrix Square numeric matrix (n x n).  Row i -> Col j = directed
#'   edge i->j.  Non-zero values are edges; values other than 1 are treated as
#'   weights and annotated on the edge.
#' @param node_props `data.frame` with the following columns:
#'   \describe{
#'     \item{id}{Node identifier matching `rownames`/`colnames` of `adj_matrix`.}
#'     \item{x,y}{Centre coordinates in SVG pixel space (y increases downward).}
#'     \item{shape}{One of `"rect"`, `"rounded"`, `"circle"`, `"ellipse"`,
#'       `"diamond"`.}
#'     \item{colour}{Fill colour as a hex string or R colour name (e.g.
#'       `"#e8f0fe"`).  Also accepted as `"color"`.}
#'     \item{label}{Display text.  Use `"\n"` for multi-line labels.}
#'     \item{width,height}{*(optional)* Node size in pixels.}
#'     \item{fontsize}{*(optional)* Label font size in pixels.}
#'     \item{fontcolour}{*(optional)* Label text colour. Also `"fontcolor"`.}
#'     \item{stroke}{*(optional)* Node border colour.}
#'   }
#' @param directed Logical. `TRUE` (default) produces a directed graph with
#'   arrowheads.
#' @param svg_file Output path for the SVG file.  `NULL` = do not write.
#' @param dot_file Output path for the DOT file.  `NULL` = do not write.
#' @param mermaid_file Output path for the Mermaid `.mmd` file.
#'   `NULL` = do not write.
#' @param svg_padding Extra whitespace (px) added around the content when
#'   computing the SVG canvas size.
#' @param default_width Default node width in pixels (used when `width` column
#'   is absent or `NA`).
#' @param default_height Default node height in pixels.
#' @param default_fontsize Default label font size in pixels.
#' @param default_fontcolour Default label text colour.
#' @param default_stroke Default node border colour.
#' @param edge_colour Colour applied to all edges and arrowheads.
#' @param edge_width Stroke width for edges in pixels.
#' @param adj_overlay Optional square numeric matrix of the same dimensions as
#'   `adj_matrix`.  Non-zero entries add extra edges drawn on top of the
#'   structural edges but **excluded from topology analysis**.  Useful for
#'   annotations, cross-links, or relationships that should not influence the
#'   graph classification or automatic layout.  `NULL` (default) = no overlay.
#' @param overlay_edge_colour Colour for overlay edges.  Default `"#999999"`.
#' @param overlay_edge_width Stroke width for overlay edges in pixels.
#'   Default `1.0`.
#' @param overlay_edge_style Line style for overlay edges: `"dashed"` (default)
#'   or `"solid"`.
#' @param layout Node positioning mode:
#'   \describe{
#'     \item{`"manual"`}{(default) Uses the `x`/`y` columns of `node_props`
#'       as supplied.}
#'     \item{`"auto"`}{Chooses the best layout from the topological analysis:
#'       `"tree"` for acyclic graphs, `"bipartite"` for non-acyclic bipartite
#'       graphs, `"circular"` otherwise.  The chosen name is reported in a
#'       console message and stored in `result$topology$recommended_layout`.}
#'     \item{`"tree"`}{Hierarchical top-down layout.  Ranks nodes by longest
#'       path from root(s); orders siblings by barycenter of parent positions.
#'       Suitable for trees, forests, and DAGs.}
#'     \item{`"bipartite"`}{Two-column layout: one partition on the left,
#'       the other on the right.  Suitable for bipartite graphs.}
#'     \item{`"sunburst"`}{Radial hierarchical layout.  Root(s) at the centre;
#'       each successive rank placed on a larger concentric circle; angular
#'       span of each node is proportional to its subtree leaf count.
#'       Recommended automatically for shallow DAGs with high branching
#'       (see `sunburst_max_depth` and `sunburst_min_branching`).}
#'     \item{`"circular"`}{Equal angular intervals around a circle, starting
#'       at the top (12 o'clock) proceeding clockwise.  Suitable for cyclic
#'       or densely connected graphs.}
#'   }
#' @param sunburst_max_depth Integer. Maximum DAG depth for `"auto"` to
#'   recommend `"sunburst"` over `"tree"`.  Default `3`.
#' @param sunburst_min_branching Numeric. Minimum average branching factor
#'   (mean out-degree of non-leaf nodes) for `"auto"` to recommend `"sunburst"`.
#'   Default `3`.
#' @param circle_r Radius of the circle in pixels when `layout = "circular"`.
#'   `NULL` (default) auto-sizes to `node_spacing * n / (2 * pi)` where
#'   `node_spacing = max(default_width, default_height) * 1.5`.
#' @param circle_cx,circle_cy Centre of the circle in SVG pixel space when
#'   `layout = "circular"`.  Both default to `circle_r + svg_padding +
#'   max(default_width, default_height) / 2` so the full circle fits inside the
#'   canvas with padding.
#'
#' @return Invisibly: a named list with elements `svg`, `dot`, `mermaid`, and
#'   `topology`.  The first three are character strings containing the full
#'   source of each format.  `topology` is itself a named list with elements:
#'   \describe{
#'     \item{type}{Character. One of `"tree"`, `"forest (multiple trees)"`,
#'       `"DAG — hierarchical"`, `"DAG — disconnected"`,
#'       `"strongly connected (cyclic)"`, `"weakly connected (cyclic)"`,
#'       `"disconnected (cyclic)"`.}
#'     \item{recommended_layout}{Character. The layout that `"auto"` would
#'       choose: `"sunburst"`, `"tree"`, `"bipartite"`, or `"circular"`.}
#'     \item{max_depth}{Integer. Longest directed path from any root.
#'       `NA` for cyclic graphs.}
#'     \item{avg_branching_factor}{Numeric. Mean out-degree of non-leaf nodes.
#'       `NA` if all nodes are leaves.}
#'     \item{n_nodes, n_edges}{Integer counts.}
#'     \item{density}{Edge density: `n_edges / (n * (n - 1))`.}
#'     \item{is_acyclic}{Logical. `TRUE` if the graph contains no directed cycle.}
#'     \item{is_weakly_connected}{Logical. `TRUE` if the underlying undirected
#'       graph is connected.}
#'     \item{is_strongly_connected}{Logical. `TRUE` if every node is reachable
#'       from every other node following directed edges.}
#'     \item{is_bipartite}{Logical. `TRUE` if nodes can be 2-coloured such that
#'       no edge connects two nodes of the same colour.}
#'     \item{is_tree}{Logical. DAG + weakly connected + single root +
#'       every non-root has in-degree 1.}
#'     \item{is_forest}{Logical. DAG + every node has in-degree ≤ 1.}
#'     \item{n_strongly_connected_components}{Integer. Number of SCCs
#'       (Kosaraju's algorithm).}
#'     \item{root_nodes}{Character vector of node IDs with in-degree 0.}
#'     \item{leaf_nodes}{Character vector of node IDs with out-degree 0.}
#'     \item{in_degree, out_degree}{Named integer vectors, one value per node.}
#'   }
#'
#' @examples
#' ids <- c("A", "B", "C")
#' adj <- matrix(
#'   c(0, 1, 1,
#'     0, 0, 1,
#'     0, 0, 0),
#'   nrow = 3, byrow = TRUE,
#'   dimnames = list(ids, ids)
#' )
#' nodes <- data.frame(
#'   id     = ids,
#'   x      = c(150,  50, 250),
#'   y      = c( 60, 200, 200),
#'   shape  = c("diamond", "rect", "rect"),
#'   colour = c("#fff8e1", "#e8f0fe", "#e8f0fe"),
#'   label  = ids,
#'   stringsAsFactors = FALSE
#' )
#' result <- graph_to_outputs(
#'   adj_matrix   = adj,
#'   node_props   = nodes,
#'   svg_file     = NULL,
#'   dot_file     = NULL,
#'   mermaid_file = NULL
#' )
#' cat(result$mermaid)
#'
#' @export
graph_to_outputs <- function(
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
    edge_colour         = "#444444",
    edge_width          = 1.5,
    adj_overlay         = NULL,
    overlay_edge_colour = "#999999",
    overlay_edge_width  = 1.0,
    overlay_edge_style  = "dashed",
    layout                  = "manual",
    sunburst_max_depth      = 3L,
    sunburst_min_branching  = 3,
    circle_r                = NULL,
    circle_cx               = NULL,
    circle_cy               = NULL
) {

  # ── 0. Validate & normalise inputs ─────────────────────────────────────────
  stopifnot(
    is.matrix(adj_matrix),
    nrow(adj_matrix) == ncol(adj_matrix),
    is.data.frame(node_props)
  )
  n <- nrow(adj_matrix)

  # Accept both American and British spelling
  names(node_props) <- tolower(names(node_props))
  for (pair in list(c("color", "colour"), c("fontcolor", "fontcolour"))) {
    old <- pair[1]; new <- pair[2]
    if (old %in% names(node_props) && !new %in% names(node_props))
      names(node_props)[names(node_props) == old] <- new
  }

  missing_cols <- setdiff(c("id", "x", "y", "shape", "colour", "label"),
                          names(node_props))
  if (length(missing_cols))
    stop("node_props missing column(s): ", paste(missing_cols, collapse = ", "))

  node_props$id <- as.character(node_props$id)

  # Establish canonical node IDs from adj_matrix rownames
  node_ids <- rownames(adj_matrix)
  if (is.null(node_ids)) {
    if (nrow(node_props) != n)
      stop("adj_matrix has no rownames; node_props must have exactly ", n,
           " rows (one per node).")
    node_ids <- node_props$id[seq_len(n)]
    rownames(adj_matrix) <- colnames(adj_matrix) <- node_ids
  }

  idx <- match(node_ids, node_props$id)
  if (anyNA(idx))
    stop("node_props$id entries missing for node(s): ",
         paste(node_ids[is.na(idx)], collapse = ", "))
  node_props <- node_props[idx, ]
  rownames(node_props) <- NULL

  # Fill optional columns with defaults
  .fill <- function(col, def) {
    if (!col %in% names(node_props)) node_props[[col]] <<- def
    node_props[[col]][is.na(node_props[[col]])] <<- def
  }
  .fill("width",      default_width)
  .fill("height",     default_height)
  .fill("fontsize",   default_fontsize)
  .fill("fontcolour", default_fontcolour)
  .fill("stroke",     default_stroke)

  for (col in c("x", "y", "width", "height", "fontsize"))
    node_props[[col]] <- as.numeric(node_props[[col]])

  # ── 0a. Validate overlay matrix ────────────────────────────────────────────
  if (!is.null(adj_overlay)) {
    if (!is.matrix(adj_overlay) ||
        !identical(dim(adj_overlay), dim(adj_matrix)))
      stop("adj_overlay must be a matrix with the same dimensions as adj_matrix.")
    if (!is.null(rownames(adj_matrix)) &&
        !identical(dimnames(adj_overlay), dimnames(adj_matrix)))
      stop("adj_overlay must have the same row/col names as adj_matrix.")
    overlay_edge_style <- match.arg(overlay_edge_style, c("dashed", "solid"))
  }

  # ── 0b. Topological analysis ────────────────────────────────────────────────
  topo <- .graph_topology(adj_matrix,
                          sunburst_max_depth, sunburst_min_branching)
  message("Graph topology : ", topo$type,
          "  [nodes=", topo$n_nodes, ", edges=", topo$n_edges,
          ", density=", topo$density, "]")
  message("Recommended layout: ", topo$recommended_layout)

  # ── 0c. Layout (overrides x / y for non-manual modes) ──────────────────────
  layout <- match.arg(layout, c("manual", "auto", "sunburst", "tree", "bipartite", "circular"))
  if (layout == "auto") {
    layout <- topo$recommended_layout
    message("Auto layout selected: '", layout, "'")
  }
  if (layout == "circular") {
    node_spacing <- max(default_width, default_height) * 1.5
    if (is.null(circle_r))
      circle_r <- node_spacing * n / (2 * pi)
    half_node <- max(default_width, default_height) / 2
    if (is.null(circle_cx)) circle_cx <- circle_r + svg_padding + half_node
    if (is.null(circle_cy)) circle_cy <- circle_r + svg_padding + half_node
    angles <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)] - pi / 2
    node_props$x <- circle_cx + circle_r * cos(angles)
    node_props$y <- circle_cy + circle_r * sin(angles)
  } else if (layout == "sunburst") {
    node_props <- .layout_sunburst(adj_matrix, node_props,
                                   default_width, default_height, svg_padding)
  } else if (layout == "tree") {
    node_props <- .layout_tree(adj_matrix, node_props,
                               default_width, default_height, svg_padding)
  } else if (layout == "bipartite") {
    node_props <- .layout_bipartite(adj_matrix, node_props,
                                    default_width, default_height, svg_padding)
  }

  # ── 1–3. Build representations ─────────────────────────────────────────────
  svg_str <- .svg_build(adj_matrix, node_props, directed,
                        svg_padding, edge_colour, edge_width,
                        adj_overlay, overlay_edge_colour,
                        overlay_edge_width, overlay_edge_style)
  dot_str <- .dot_build(adj_matrix, node_props, directed,
                        adj_overlay, overlay_edge_colour,
                        overlay_edge_width, overlay_edge_style)
  mmd_str <- .mmd_build(adj_matrix, node_props, directed,
                        adj_overlay, overlay_edge_style)

  # ── 4. Write files ─────────────────────────────────────────────────────────
  .write_if <- function(path, content, label) {
    if (!is.null(path)) {
      writeLines(content, path)
      message(label, " written \u2192 ",
              normalizePath(path, mustWork = FALSE))
    }
  }
  .write_if(svg_file,     svg_str, "SVG")
  .write_if(dot_file,     dot_str, "DOT")
  .write_if(mermaid_file, mmd_str, "Mermaid")

  invisible(list(svg = svg_str, dot = dot_str, mermaid = mmd_str,
                 topology = topo))
}

# ── Layout helpers ───────────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.layout_sunburst <- function(adj_matrix, node_props,
                              default_width, default_height, svg_padding) {
  n <- nrow(adj_matrix)
  A <- adj_matrix != 0

  # ── Rank via longest-path topological DP ─────────────────────────────────
  in_copy  <- as.integer(colSums(A))
  queue    <- which(in_copy == 0L)
  topo_ord <- integer(0)
  while (length(queue)) {
    v <- queue[1L]; queue <- queue[-1L]
    topo_ord <- c(topo_ord, v)
    for (w in which(A[v, ])) {
      in_copy[w] <- in_copy[w] - 1L
      if (in_copy[w] == 0L) queue <- c(queue, w)
    }
  }
  rank <- integer(n)
  for (v in topo_ord)
    for (w in which(A[v, ]))
      rank[w] <- max(rank[w], rank[v] + 1L)

  # ── Leaf count per subtree (bottom-up in reverse topological order) ───────
  leaf_cnt <- integer(n)
  leaf_cnt[rowSums(A) == 0L] <- 1L
  for (v in rev(topo_ord)) {
    ch <- which(A[v, ])
    if (length(ch)) leaf_cnt[v] <- sum(leaf_cnt[ch])
  }

  # ── Angular ranges (top-down): each node gets a slice ∝ its leaf count ───
  ang_start <- numeric(n);  ang_end <- numeric(n)
  roots       <- which(colSums(A) == 0L)
  total_leaves <- sum(leaf_cnt[roots])
  cur <- 0
  for (r in roots) {
    ang_start[r] <- cur
    ang_end[r]   <- cur + leaf_cnt[r] / total_leaves * 2 * pi
    cur          <- ang_end[r]
  }
  for (v in topo_ord) {
    ch <- which(A[v, ])
    if (!length(ch)) next
    cur <- ang_start[v]
    for (w in ch) {
      span         <- ang_end[v] - ang_start[v]
      ang_start[w] <- cur
      ang_end[w]   <- cur + leaf_cnt[w] / sum(leaf_cnt[ch]) * span
      cur          <- ang_end[w]
    }
  }

  # ── Radii: rank 0 = 0 (centre), each rank adds one ring ──────────────────
  nw       <- if ("width"  %in% names(node_props)) node_props$width  else default_width
  nh       <- if ("height" %in% names(node_props)) node_props$height else default_height
  ring_gap <- max(max(nw, na.rm = TRUE), max(nh, na.rm = TRUE)) * 2.2
  radii    <- rank * ring_gap

  outer_r  <- max(radii) + max(max(nw, na.rm = TRUE), max(nh, na.rm = TRUE))
  cx       <- svg_padding + outer_r
  cy       <- svg_padding + outer_r

  # ── Convert to Cartesian (start at 12 o'clock) ───────────────────────────
  mid_ang      <- (ang_start + ang_end) / 2 - pi / 2
  node_props$x <- cx + radii * cos(mid_ang)
  node_props$y <- cy + radii * sin(mid_ang)
  node_props
}

#' @keywords internal
#' @noRd
.layout_tree <- function(adj_matrix, node_props,
                         default_width, default_height, svg_padding) {
  n <- nrow(adj_matrix)
  A <- adj_matrix != 0

  # ── Rank: longest path from root via topological sort (Kahn + DP) ────────
  in_copy  <- as.integer(colSums(A))
  queue    <- which(in_copy == 0L)
  topo_ord <- integer(0)
  while (length(queue)) {
    v <- queue[1L]; queue <- queue[-1L]
    topo_ord <- c(topo_ord, v)
    for (w in which(A[v, ])) {
      in_copy[w] <- in_copy[w] - 1L
      if (in_copy[w] == 0L) queue <- c(queue, w)
    }
  }
  rank <- integer(n)
  for (v in topo_ord)
    for (w in which(A[v, ]))
      rank[w] <- max(rank[w], rank[v] + 1L)

  # ── x-slot: barycenter heuristic, rank by rank top-down ──────────────────
  x_slot  <- numeric(n)
  n_ranks <- max(rank) + 1L
  for (r in seq_len(n_ranks) - 1L) {
    members <- which(rank == r)
    if (!length(members)) next
    score <- vapply(members, function(v) {
      preds <- which(A[, v] != 0)
      if (!length(preds)) 0 else mean(x_slot[preds])
    }, numeric(1L))
    x_slot[members[order(score)]] <- seq_along(members)
  }

  # ── Convert to pixel coordinates ─────────────────────────────────────────
  nw     <- if ("width"  %in% names(node_props)) node_props$width  else default_width
  nh     <- if ("height" %in% names(node_props)) node_props$height else default_height
  h_gap  <- max(nw, na.rm = TRUE) * 1.6
  v_gap  <- max(nh, na.rm = TRUE) * 2.2
  half_w <- max(nw, na.rm = TRUE) / 2
  half_h <- max(nh, na.rm = TRUE) / 2

  node_props$x <- svg_padding + half_w + (x_slot - 1) * h_gap
  node_props$y <- svg_padding + half_h + rank * v_gap
  node_props
}

#' @keywords internal
#' @noRd
.layout_bipartite <- function(adj_matrix, node_props,
                               default_width, default_height, svg_padding) {
  n     <- nrow(adj_matrix)
  A_und <- (adj_matrix != 0) | t(adj_matrix != 0)

  # ── 2-colouring BFS ───────────────────────────────────────────────────────
  color <- rep(-1L, n)
  for (s in seq_len(n)) {
    if (color[s] != -1L) next
    color[s] <- 0L; queue <- s
    while (length(queue)) {
      v <- queue[1L]; queue <- queue[-1L]
      for (w in which(A_und[v, ])) {
        if (color[w] == -1L) { color[w] <- 1L - color[v]; queue <- c(queue, w) }
      }
    }
  }
  color[color == -1L] <- 0L

  # ── Column placement ──────────────────────────────────────────────────────
  nw      <- if ("width"  %in% names(node_props)) node_props$width  else default_width
  nh      <- if ("height" %in% names(node_props)) node_props$height else default_height
  h_gap   <- max(nh, na.rm = TRUE) * 1.8
  col_gap <- max(nw, na.rm = TRUE) * 5.0
  half_w  <- max(nw, na.rm = TRUE) / 2
  half_h  <- max(nh, na.rm = TRUE) / 2

  left  <- which(color == 0L)
  right <- which(color == 1L)
  node_props$x[left]  <- svg_padding + half_w
  node_props$x[right] <- svg_padding + half_w + col_gap
  node_props$y[left]  <- svg_padding + half_h + (seq_along(left)  - 1L) * h_gap
  node_props$y[right] <- svg_padding + half_h + (seq_along(right) - 1L) * h_gap
  node_props
}

# ── Topological analysis helper ──────────────────────────────────────────────

#' @keywords internal
#' @noRd
.graph_topology <- function(adj_matrix,
                            sunburst_max_depth     = 3L,
                            sunburst_min_branching = 3) {

  n   <- nrow(adj_matrix)
  ids <- rownames(adj_matrix)
  A   <- adj_matrix != 0          # logical adjacency matrix

  out_deg <- rowSums(A)
  in_deg  <- colSums(A)
  n_edges <- sum(A)

  # ── Cycle detection: DFS with 3-state colouring (0=new, 1=open, 2=done) ──
  .has_cycle <- function() {
    state <- integer(n)
    found <- FALSE
    .dfs <- function(v) {
      if (found) return()
      state[v] <<- 1L
      for (w in which(A[v, ])) {
        if      (state[w] == 1L) { found <<- TRUE; return() }
        else if (state[w] == 0L) .dfs(w)
      }
      state[v] <<- 2L
    }
    for (i in seq_len(n)) if (state[i] == 0L) .dfs(i)
    found
  }
  has_cycle <- .has_cycle()

  # ── Weak connectivity: BFS on symmetrised adjacency ───────────────────────
  A_und   <- A | t(A)
  visited <- logical(n)
  queue   <- 1L;  visited[1L] <- TRUE
  while (length(queue)) {
    v     <- queue[1L]; queue <- queue[-1L]
    nbs   <- which(A_und[v, ] & !visited)
    visited[nbs] <- TRUE
    queue <- c(queue, nbs)
  }
  weakly_connected <- all(visited)

  # ── Strongly connected components: Kosaraju's algorithm ──────────────────
  vis1 <- logical(n);  finish <- integer(0)
  .dfs1 <- function(v) {
    vis1[v] <<- TRUE
    for (w in which(A[v, ])) if (!vis1[w]) .dfs1(w)
    finish <<- c(finish, v)
  }
  for (i in seq_len(n)) if (!vis1[i]) .dfs1(i)

  A_T  <- t(A);  vis2 <- logical(n);  scc <- integer(n);  k <- 0L
  .dfs2 <- function(v, id) {
    vis2[v] <<- TRUE;  scc[v] <<- id
    for (w in which(A_T[v, ])) if (!vis2[w]) .dfs2(w, id)
  }
  for (v in rev(finish)) if (!vis2[v]) { k <- k + 1L; .dfs2(v, k) }

  # ── Bipartite check: 2-colouring BFS ─────────────────────────────────────
  color     <- rep(-1L, n)
  bipartite <- TRUE
  for (s in seq_len(n)) {
    if (color[s] != -1L || !bipartite) next
    color[s] <- 0L;  queue <- s
    while (length(queue) && bipartite) {
      v <- queue[1L]; queue <- queue[-1L]
      for (w in which(A_und[v, ])) {
        if      (color[w] == -1L)      { color[w] <- 1L - color[v]; queue <- c(queue, w) }
        else if (color[w] == color[v]) { bipartite <- FALSE }
      }
    }
  }

  # ── Derived flags ─────────────────────────────────────────────────────────
  roots     <- which(in_deg  == 0L)
  leaves    <- which(out_deg == 0L)
  is_dag    <- !has_cycle
  is_tree   <- is_dag && weakly_connected &&
               length(roots) == 1L && all(in_deg[-roots] == 1L)
  is_forest <- is_dag && all(in_deg <= 1L)

  topo_type <- if      (is_tree)                  "tree"
               else if (is_forest)                "forest (multiple trees)"
               else if (is_dag && weakly_connected) "DAG \u2014 hierarchical"
               else if (is_dag)                   "DAG \u2014 disconnected"
               else if (k == 1L)                  "strongly connected (cyclic)"
               else if (weakly_connected)         "weakly connected (cyclic)"
               else                               "disconnected (cyclic)"

  # ── Depth and branching factor (only meaningful for DAGs) ────────────────
  if (is_dag) {
    in_copy2 <- as.integer(in_deg)
    queue_t  <- which(in_copy2 == 0L)
    topo2    <- integer(0)
    while (length(queue_t)) {
      v <- queue_t[1L]; queue_t <- queue_t[-1L]
      topo2 <- c(topo2, v)
      for (w in which(A[v, ])) {
        in_copy2[w] <- in_copy2[w] - 1L
        if (in_copy2[w] == 0L) queue_t <- c(queue_t, w)
      }
    }
    node_depth <- integer(n)
    for (v in topo2)
      for (w in which(A[v, ]))
        node_depth[w] <- max(node_depth[w], node_depth[v] + 1L)
    max_depth <- max(node_depth)
  } else {
    max_depth <- NA_integer_
  }

  non_leaves <- which(out_deg > 0L)
  avg_branching_factor <- if (length(non_leaves) > 0L)
    round(mean(out_deg[non_leaves]), 2) else NA_real_

  # ── Layout recommendation ─────────────────────────────────────────────────
  rec_layout <- if (!is_dag) {
    if (bipartite) "bipartite" else "circular"
  } else if (!is.na(max_depth) && max_depth <= sunburst_max_depth &&
             !is.na(avg_branching_factor) && avg_branching_factor >= sunburst_min_branching) {
    "sunburst"
  } else {
    "tree"
  }

  list(
    type                           = topo_type,
    recommended_layout             = rec_layout,
    max_depth                      = max_depth,
    avg_branching_factor           = avg_branching_factor,
    n_nodes                        = n,
    n_edges                        = n_edges,
    density                        = round(n_edges / max(1L, n * (n - 1L)), 4),
    is_acyclic                     = is_dag,
    is_weakly_connected            = weakly_connected,
    is_strongly_connected          = (k == 1L),
    is_bipartite                   = bipartite,
    is_tree                        = is_tree,
    is_forest                      = is_forest,
    n_strongly_connected_components = k,
    root_nodes                     = ids[roots],
    leaf_nodes                     = ids[leaves],
    in_degree                      = setNames(in_deg,  ids),
    out_degree                     = setNames(out_deg, ids)
  )
}
