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
#'     \item{x,y}{Centre coordinates in SVG pixel space (y increases downward).
#'       \emph{Required only when} `layout = "manual"`; omit freely when using
#'       any other layout mode.}
#'     \item{shape}{One of `"rect"`, `"rounded"`, `"circle"`, `"ellipse"`,
#'       `"diamond"`.}
#'     \item{colour}{Fill colour as a hex string or R colour name (e.g.
#'       `"#e8f0fe"`).  Also accepted as `"color"`.}
#'     \item{label}{Display text.  Use `"\n"` for multi-line labels.}
#'     \item{width,height}{*(optional)* Node size in pixels.}
#'     \item{fontsize}{*(optional)* Label font size in pixels.}
#'     \item{fontcolour}{*(optional)* Label text colour. Also `"fontcolor"`.}
#'     \item{stroke}{*(optional)* Node border colour.}
#'     \item{hierarchy_rank}{*(optional, integer)* Override the automatically
#'       computed rank (y-level) in `layout = "tree"`.  Non-NA values take
#'       precedence over topology-derived rank; NA leaves rank computed
#'       automatically.  Ignored for all other layouts.}
#'   }
#' @param directed Logical. `TRUE` (default) produces a directed graph with
#'   arrowheads.
#' @param svg_file Output path for the SVG file.  `NULL` = do not write.
#' @param clean_svg_file Character. Path for a second SVG output identical to
#'   the main SVG but with centroid markers at 0 percent opacity.  Set to
#'   `NULL` to suppress this output.
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
#' @param node_spacing Positive numeric. Global spacing multiplier applied to
#'   all automatic layouts (`"circular"`, `"sunburst"`, `"tree"`,
#'   `"bipartite"`).  Values greater than 1 spread nodes further apart;
#'   values less than 1 (e.g. `0.6`) pack them closer together.
#'   Default `1.0`.  Has no effect on `"manual"` layout.
#' @param sunburst_sort_children Logical. When `TRUE` (default), siblings are
#'   reordered within each parent's angular slice so that the child with the
#'   largest subtree (most leaf descendants) occupies the central angle, with
#'   progressively smaller siblings fanning outward on both sides.  Set to
#'   `FALSE` to preserve adjacency-matrix column order.
#' @param circle_r Radius of the circle in pixels when `layout = "circular"`.
#'   `NULL` (default) auto-sizes to `node_spacing * n / (2 * pi)` where
#'   `node_spacing = max(default_width, default_height) * 1.5`.
#' @param circle_cx,circle_cy Centre of the circle in SVG pixel space when
#'   `layout = "circular"`.  Both default to `circle_r + svg_padding +
#'   max(default_width, default_height) / 2` so the full circle fits inside the
#'   canvas with padding.
#' @param edge_curvature Controls arc routing for **structural edges**
#'   (`adj_matrix`).  Works for any layout mode:
#'   \describe{
#'     \item{`"auto"`}{(default) Draws each edge as the arc of the
#'       circumscribed circle of O--P1--P2 that avoids O, where O is the
#'       position of the node with the highest eigenvector centrality score
#'       (power-iteration on the symmetrised adjacency matrix).  This single
#'       global origin gives curvature that follows the structural centre of
#'       the graph.  Falls back to a straight line when the three points are
#'       collinear (no finite circle exists).}
#'     \item{`"straight"`}{Always draws straight lines.}
#'   }
#' @param overlay_edge_curvature Controls arc routing for **overlay edges**
#'   (`adj_overlay`) independently of `edge_curvature`.  Same values:
#'   `"auto"` (default) or `"straight"`.  Has no effect when `adj_overlay` is
#'   `NULL`.
#' @param centroids Optional `data.frame` with columns `x` and `y` (numeric,
#'   SVG pixel space) that defines one or more curvature-origin points.
#'   An optional `label` column is accepted but not used in calculations.
#'   \describe{
#'     \item{When `NULL` (default)}{Arc curvature (for both structural and
#'       overlay edges) is anchored at the node with the highest eigenvector
#'       centrality score -- the existing global-hub behaviour.}
#'     \item{When non-empty}{Each edge independently selects the centroid
#'       nearest to its **midpoint** (Euclidean distance) and uses that point
#'       as the arc origin O in the circumscribed-circle calculation.  This
#'       lets different regions of the diagram have distinct curvature
#'       orientations.  An empty data.frame is treated as `NULL`.}
#'   }
#' @param show_centroids Logical.  When `TRUE` (default) and `centroids` is
#'   non-`NULL`, centroid markers (crosshair circles) are drawn on the SVG to
#'   show their positions.  Set to `FALSE` to suppress markers.
#' @param edge_labels Optional character matrix with the same dimensions as
#'   `adj_matrix`.  Each non-`NA`, non-empty cell provides an explicit text
#'   label for the corresponding structural edge.  When `NULL` (default) or
#'   for any cell that is `NA`/empty, the label falls back to the edge weight
#'   value if that weight is not 1, or no label otherwise.
#' @param overlay_edge_labels Optional character matrix with the same
#'   dimensions as `adj_overlay`.  Same semantics as `edge_labels` but for
#'   overlay edges.  Ignored when `adj_overlay` is `NULL`.
#' @param edge_props Optional `data.frame` mapping structural edge weight values
#'   to visual properties.  Recognised columns:
#'   \describe{
#'     \item{`weight`}{Numeric (required).  The adj_matrix cell value this row
#'       applies to.}
#'     \item{`colour`}{Character.  Edge stroke colour.  Defaults to
#'       `edge_colour` when absent or `NA`.}
#'     \item{`width`}{Numeric.  Stroke width in pixels.  Defaults to
#'       `edge_width` when absent or `NA`.}
#'     \item{`linetype`}{Character.  One of `"solid"` (default), `"dashed"`,
#'       `"dotted"`, `"longdash"`, `"twodash"`.}
#'     \item{`label`}{Character.  Legend entry text for this weight/style.
#'       Used when `show_legend = TRUE`.}
#'   }
#'   When `NULL` (default), all structural edges use `edge_colour`,
#'   `edge_width`, and solid linetype.  Weights not present in the table fall
#'   back to those global defaults.
#' @param overlay_edge_props Optional `data.frame` with the same columns as
#'   `edge_props` but applied to overlay edges.  Weights not present fall back
#'   to `overlay_edge_colour`, `overlay_edge_width`, `overlay_edge_style`.
#' @param show_legend Logical.  When `TRUE`, an SVG legend is appended below
#'   the graph explaining node shapes, node colours, and edge linetypes.
#'   Default `FALSE`.
#' @param legend_node_shape Named character vector mapping shape names to
#'   display labels, e.g. `c(rect = "Intervention", diamond = "Outcome")`.
#'   Used only when `show_legend = TRUE`.  `NULL` = no shape section in legend.
#' @param legend_node_colour Named character vector mapping colour hex strings
#'   to display labels, e.g. `c("#e8f0fe" = "Category A")`.
#'   Used only when `show_legend = TRUE`.  `NULL` = no colour section in legend.
#' @param legend_title Character string used as the legend title when
#'   `show_legend = TRUE`.  Default `"Legend"`.
#'
#' @return Invisibly: a named list with elements `svg`, `dot`, `mermaid`,
#'   `clean_svg`, and `topology`.  The first three are character strings containing the full
#'   source of each format.  `topology` is itself a named list with elements:
#'   \describe{
#'     \item{type}{Character. One of `"tree"`, `"forest (multiple trees)"`,
#'       `"DAG -- hierarchical"`, `"DAG -- disconnected"`,
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
#'     \item{is_forest}{Logical. DAG + every node has in-degree <= 1.}
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
    clean_svg_file     = "graph_clean.svg",
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
    node_spacing            = 1.0,
    sunburst_sort_children  = TRUE,
    circle_r                = NULL,
    circle_cx               = NULL,
    circle_cy               = NULL,
    edge_curvature          = "auto",
    overlay_edge_curvature  = "auto",
    centroids               = NULL,
    show_centroids          = TRUE,
    edge_labels             = NULL,
    overlay_edge_labels     = NULL,
    edge_props              = NULL,
    overlay_edge_props      = NULL,
    show_legend             = FALSE,
    legend_node_shape       = NULL,
    legend_node_colour      = NULL,
    legend_title            = "Legend"
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

  layout                 <- match.arg(layout, c("manual", "auto", "sunburst",
                                                "tree", "bipartite", "circular"))
  edge_curvature         <- match.arg(edge_curvature,         c("auto", "straight"))
  overlay_edge_curvature <- match.arg(overlay_edge_curvature, c("auto", "straight"))

  # x/y are only required for manual layout; other layouts compute them
  req_xy       <- layout == "manual"
  required_cols <- c("id", if (req_xy) c("x", "y"), "shape", "colour", "label")
  missing_cols  <- setdiff(required_cols, names(node_props))
  if (length(missing_cols))
    stop("node_props missing column(s): ", paste(missing_cols, collapse = ", "))

  # Add placeholder x/y so downstream code always finds them
  if (!"x" %in% names(node_props)) node_props$x <- 0
  if (!"y" %in% names(node_props)) node_props$y <- 0

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

  # ── 0a. Validate centroids ─────────────────────────────────────────────────
  if (!is.null(centroids)) {
    if (!is.data.frame(centroids) || !all(c("x", "y") %in% names(centroids)))
      stop("centroids must be a data.frame with columns 'x' and 'y'.")
    centroids$x <- as.numeric(centroids$x)
    centroids$y <- as.numeric(centroids$y)
    if (nrow(centroids) == 0L) centroids <- NULL   # empty df → NULL (hub mode)
  }

  # ── 0b. Validate overlay matrix ────────────────────────────────────────────
  if (!is.null(adj_overlay)) {
    if (!is.matrix(adj_overlay) ||
        !identical(dim(adj_overlay), dim(adj_matrix)))
      stop("adj_overlay must be a matrix with the same dimensions as adj_matrix.")
    if (!is.null(rownames(adj_matrix)) &&
        !identical(dimnames(adj_overlay), dimnames(adj_matrix)))
      stop("adj_overlay must have the same row/col names as adj_matrix.")
    overlay_edge_style <- match.arg(overlay_edge_style, c("dashed", "solid"))
  }

  # ── 0c. Validate edge label matrices ────────────────────────────────────────
  if (!is.null(edge_labels)) {
    if (!is.matrix(edge_labels) ||
        !identical(dim(edge_labels), dim(adj_matrix)))
      stop("edge_labels must be a character matrix with the same dimensions as adj_matrix.")
    edge_labels <- matrix(as.character(edge_labels), nrow = nrow(edge_labels),
                          ncol = ncol(edge_labels),
                          dimnames = dimnames(edge_labels))
  }
  if (!is.null(overlay_edge_labels)) {
    if (!is.matrix(overlay_edge_labels) ||
        !identical(dim(overlay_edge_labels), dim(adj_matrix)))
      stop("overlay_edge_labels must be a character matrix with the same dimensions as adj_matrix.")
    overlay_edge_labels <- matrix(as.character(overlay_edge_labels),
                                  nrow = nrow(overlay_edge_labels),
                                  ncol = ncol(overlay_edge_labels),
                                  dimnames = dimnames(overlay_edge_labels))
  }

  # ── 0d. Validate edge_props / overlay_edge_props ────────────────────────────
  .validate_edge_props <- function(ep, nm) {
    if (is.null(ep)) return(NULL)
    if (!is.data.frame(ep) || !"weight" %in% names(ep))
      stop(nm, " must be a data.frame with at least a 'weight' column.")
    ep$weight <- as.numeric(ep$weight)
    if ("colour"   %in% names(ep)) ep$colour   <- as.character(ep$colour)
    if ("width"    %in% names(ep)) ep$width     <- as.numeric(ep$width)
    if ("linetype" %in% names(ep)) ep$linetype  <- as.character(ep$linetype)
    if ("label"    %in% names(ep)) ep$label     <- as.character(ep$label)
    ep
  }
  edge_props         <- .validate_edge_props(edge_props,         "edge_props")
  overlay_edge_props <- .validate_edge_props(overlay_edge_props, "overlay_edge_props")

  # ── 0b. Topological analysis ────────────────────────────────────────────────
  topo <- .graph_topology(adj_matrix,
                          sunburst_max_depth, sunburst_min_branching)
  message("Graph topology : ", topo$type,
          "  [nodes=", topo$n_nodes, ", edges=", topo$n_edges,
          ", density=", topo$density, "]")
  message("Recommended layout: ", topo$recommended_layout)

  # ── 0c. Layout (overrides x / y for non-manual modes) ──────────────────────
  if (layout == "auto") {
    layout <- topo$recommended_layout
    message("Auto layout selected: '", layout, "'")
  }
  if (layout == "circular") {
    circ_spacing <- max(default_width, default_height) * 1.5 * node_spacing
    if (is.null(circle_r))
      circle_r <- circ_spacing * n / (2 * pi)
    half_node <- max(default_width, default_height) / 2
    if (is.null(circle_cx)) circle_cx <- circle_r + svg_padding + half_node
    if (is.null(circle_cy)) circle_cy <- circle_r + svg_padding + half_node
    angles <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)] - pi / 2
    node_props$x <- circle_cx + circle_r * cos(angles)
    node_props$y <- circle_cy + circle_r * sin(angles)
  } else if (layout == "sunburst") {
    node_props <- .layout_sunburst(adj_matrix, node_props,
                                   default_width, default_height, svg_padding,
                                   sunburst_sort_children, node_spacing)
  } else if (layout == "tree") {
    node_props <- .layout_tree(adj_matrix, node_props,
                               default_width, default_height, svg_padding,
                               node_spacing)
  } else if (layout == "bipartite") {
    node_props <- .layout_bipartite(adj_matrix, node_props,
                                    default_width, default_height, svg_padding,
                                    node_spacing)
  }

  # Canvas extents — returned so interactive clients can convert between
  # SVG canvas space and the original coordinate space used by node_props.
  canvas_xlo <- min(node_props$x - node_props$width  / 2) - svg_padding
  canvas_ylo <- min(node_props$y - node_props$height / 2) - svg_padding

  # ── Arc origin ───────────────────────────────────────────────────────────────
  # Priority: user-supplied centroids > eigenvector-centrality hub node.
  #
  # When centroids is non-NULL, each edge independently picks its nearest
  # centroid as arc origin O; radial_center is not needed.
  # When centroids is NULL, the single global hub node (highest eigenvector
  # centrality on adj_matrix) is used for all edges — the legacy behaviour.
  need_arc <- edge_curvature != "straight" || overlay_edge_curvature != "straight"
  if (!is.null(centroids) && need_arc) {
    radial_center <- NULL        # svg_build will use centroids instead
  } else if (need_arc) {
    ec  <- .eigenvector_centrality(adj_matrix)
    hub <- which.max(ec)
    radial_center <- c(node_props$x[hub], node_props$y[hub])
  } else {
    radial_center <- NULL
  }

  # ── 1–3. Build representations ─────────────────────────────────────────────
  svg_str <- .svg_build(adj_matrix, node_props, directed,
                        svg_padding, edge_colour, edge_width,
                        adj_overlay, overlay_edge_colour,
                        overlay_edge_width, overlay_edge_style,
                        radial_center          = radial_center,
                        centroids              = centroids,
                        edge_curvature         = edge_curvature,
                        overlay_edge_curvature = overlay_edge_curvature,
                        show_centroids         = show_centroids,
                        edge_labels            = edge_labels,
                        overlay_edge_labels    = overlay_edge_labels,
                        edge_props             = edge_props,
                        overlay_edge_props     = overlay_edge_props,
                        show_legend            = show_legend,
                        legend_node_shape      = legend_node_shape,
                        legend_node_colour     = legend_node_colour,
                        legend_title           = legend_title)
  dot_str <- .dot_build(adj_matrix, node_props, directed,
                        adj_overlay, overlay_edge_colour,
                        overlay_edge_width, overlay_edge_style,
                        edge_labels         = edge_labels,
                        overlay_edge_labels = overlay_edge_labels,
                        edge_props          = edge_props,
                        overlay_edge_props  = overlay_edge_props)
  mmd_str <- .mmd_build(adj_matrix, node_props, directed,
                        adj_overlay, overlay_edge_style,
                        edge_labels         = edge_labels,
                        overlay_edge_labels = overlay_edge_labels,
                        edge_props          = edge_props,
                        overlay_edge_props  = overlay_edge_props)

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

  # write clean SVG (centroid markers at 0% opacity)
  clean_svg <- gsub(
    '<g class="centroid-marker"',
    '<g class="centroid-marker" opacity="0"',
    svg_str, fixed = TRUE
  )
  if (!is.null(clean_svg_file) && nzchar(clean_svg_file)) {
    writeLines(clean_svg, con = clean_svg_file)
  }

  invisible(list(svg = svg_str, dot = dot_str, mermaid = mmd_str,
                 clean_svg = clean_svg,
                 topology = topo,
                 canvas   = list(xlo = canvas_xlo, ylo = canvas_ylo)))
}

# ── Layout helpers ───────────────────────────────────────────────────────────

# Returns an integer vector of component IDs (1-based) for each node.
.find_components <- function(A_und) {
  n    <- nrow(A_und)
  comp <- integer(n)
  cid  <- 0L
  for (s in seq_len(n)) {
    if (comp[s] != 0L) next
    cid <- cid + 1L
    q   <- s
    while (length(q)) {
      v <- q[1L]; q <- q[-1L]
      if (comp[v] != 0L) next
      comp[v] <- cid
      q <- c(q, which(A_und[v, ] & comp == 0L))
    }
  }
  comp
}

# Rank a single-component subgraph (directed longest-path, then BFS fallback).
# Returns list(rank, A) where A is the (possibly rebuilt BFS-tree) adjacency.
.rank_component <- function(A_c) {
  m <- nrow(A_c)
  in_copy  <- as.integer(colSums(A_c))
  queue    <- which(in_copy == 0L)
  topo_ord <- integer(0)
  while (length(queue)) {
    v <- queue[1L]; queue <- queue[-1L]
    topo_ord <- c(topo_ord, v)
    for (w in which(A_c[v, ])) {
      in_copy[w] <- in_copy[w] - 1L
      if (in_copy[w] == 0L) queue <- c(queue, w)
    }
  }
  rank_c <- integer(m)
  for (v in topo_ord)
    for (w in which(A_c[v, ]))
      rank_c[w] <- max(rank_c[w], rank_c[v] + 1L)

  in_deg_c <- as.integer(colSums(A_c))
  if (any(rank_c == 0L & in_deg_c > 0L)) {
    Au_c  <- A_c | t(A_c)
    root  <- which.max(rowSums(Au_c))
    rank_c[] <- NA_integer_
    rank_c[root] <- 0L
    bfs_q <- root
    while (length(bfs_q)) {
      v      <- bfs_q[1L]; bfs_q <- bfs_q[-1L]
      nbrs   <- which(Au_c[v, ] & is.na(rank_c))
      rank_c[nbrs] <- rank_c[v] + 1L
      bfs_q  <- c(bfs_q, nbrs)
    }
    rank_c[is.na(rank_c)] <- max(rank_c, na.rm = TRUE) + 1L
    A_c <- Au_c & outer(rank_c, rank_c, function(r, s) s == r + 1L)
  }
  list(rank = rank_c, A = A_c)
}

#' @keywords internal
#' @noRd
.layout_sunburst <- function(adj_matrix, node_props,
                              default_width, default_height, svg_padding,
                              sort_children = TRUE, node_spacing = 1.0) {
  n  <- nrow(adj_matrix)
  A  <- adj_matrix != 0
  Au <- A | t(A)

  nw      <- if ("width"  %in% names(node_props)) node_props$width  else default_width
  nh      <- if ("height" %in% names(node_props)) node_props$height else default_height
  nw_max  <- if (any(is.finite(nw))) max(nw, na.rm = TRUE) else default_width
  nh_max  <- if (any(is.finite(nh))) max(nh, na.rm = TRUE) else default_height
  ring_gap <- max(nw_max, nh_max) * 2.2 * node_spacing
  node_pad <- max(nw_max, nh_max)
  comp_gap <- node_pad * 2.0 * node_spacing   # horizontal gap between components

  comp    <- .find_components(Au)
  n_comps <- max(comp)

  x_out <- numeric(n)
  y_out <- numeric(n)

  # Zigzag helper (needs leaf_cnt in scope)
  .zigzag <- function(idx, lc) {
    m <- length(idx)
    if (m <= 1L) return(idx)
    ord <- idx[order(lc[idx], decreasing = TRUE)]
    out <- integer(m); mid <- ceiling(m / 2L); r <- mid + 1L; l <- mid - 1L
    out[mid] <- ord[1L]
    for (k in seq(2L, m)) {
      if (k %% 2L == 0L) { out[r] <- ord[k]; r <- r + 1L }
      else                { out[l] <- ord[k]; l <- l - 1L }
    }
    out
  }

  # First pass: compute outer_r per component so we can set a shared cy
  outer_rs <- numeric(n_comps)
  for (ci in seq_len(n_comps)) {
    idx   <- which(comp == ci)
    rc    <- .rank_component(A[idx, idx, drop = FALSE])
    outer_rs[ci] <- max(rc$rank) * ring_gap + node_pad
  }
  max_outer_r <- max(outer_rs)
  cy <- svg_padding + max_outer_r

  # Second pass: full layout per component, placed side by side
  cx_cursor <- svg_padding
  for (ci in seq_len(n_comps)) {
    idx   <- which(comp == ci)
    m     <- length(idx)
    rc    <- .rank_component(A[idx, idx, drop = FALSE])
    rank_c <- rc$rank
    A_c    <- rc$A

    # Recompute topo_ord for this component's (possibly rebuilt) A_c
    in_copy2 <- as.integer(colSums(A_c))
    topo_c   <- integer(0)
    queue2   <- which(in_copy2 == 0L)
    while (length(queue2)) {
      v <- queue2[1L]; queue2 <- queue2[-1L]
      topo_c <- c(topo_c, v)
      for (w in which(A_c[v, ])) {
        in_copy2[w] <- in_copy2[w] - 1L
        if (in_copy2[w] == 0L) queue2 <- c(queue2, w)
      }
    }

    leaf_c <- integer(m)
    leaf_c[rowSums(A_c) == 0L] <- 1L
    for (v in rev(topo_c)) {
      ch <- which(A_c[v, ])
      if (length(ch)) leaf_c[v] <- sum(leaf_c[ch])
    }
    leaf_c[leaf_c == 0L] <- 1L

    roots_c <- which(colSums(A_c) == 0L)
    if (!length(roots_c)) roots_c <- which(rank_c == min(rank_c))
    total_lv <- sum(leaf_c[roots_c])
    if (total_lv == 0L) { leaf_c[] <- 1L; total_lv <- m }
    if (sort_children) roots_c <- .zigzag(roots_c, leaf_c)

    ang_start_c <- numeric(m); ang_end_c <- numeric(m)
    cur <- 0
    for (r in roots_c) {
      ang_start_c[r] <- cur
      ang_end_c[r]   <- cur + leaf_c[r] / total_lv * 2 * pi
      cur            <- ang_end_c[r]
    }
    for (v in topo_c) {
      ch <- which(A_c[v, ])
      if (!length(ch)) next
      if (sort_children) ch <- .zigzag(ch, leaf_c)
      cur <- ang_start_c[v]
      for (w in ch) {
        span           <- ang_end_c[v] - ang_start_c[v]
        ang_start_c[w] <- cur
        ang_end_c[w]   <- cur + leaf_c[w] / sum(leaf_c[ch]) * span
        cur            <- ang_end_c[w]
      }
    }

    outer_r_c <- outer_rs[ci]
    cx_c      <- cx_cursor + outer_r_c
    radii_c   <- rank_c * ring_gap
    mid_ang_c <- (ang_start_c + ang_end_c) / 2 - pi / 2
    x_out[idx] <- cx_c + radii_c * cos(mid_ang_c)
    y_out[idx] <- cy   + radii_c * sin(mid_ang_c)

    cx_cursor <- cx_cursor + 2 * outer_r_c + comp_gap
  }

  node_props$x <- x_out
  node_props$y <- y_out
  node_props
}

#' @keywords internal
#' @noRd
.layout_tree <- function(adj_matrix, node_props,
                         default_width, default_height, svg_padding,
                         node_spacing = 1.0) {
  n  <- nrow(adj_matrix)
  A  <- adj_matrix != 0
  Au <- A | t(A)

  nw      <- if ("width"  %in% names(node_props)) node_props$width  else default_width
  nh      <- if ("height" %in% names(node_props)) node_props$height else default_height
  nw_max  <- if (any(is.finite(nw))) max(nw, na.rm = TRUE) else default_width
  nh_max  <- if (any(is.finite(nh))) max(nh, na.rm = TRUE) else default_height
  h_gap   <- nw_max * 1.6 * node_spacing
  v_gap   <- nh_max * 2.2 * node_spacing
  half_w  <- nw_max / 2
  half_h  <- nh_max / 2
  comp_gap <- nw_max * 3.0 * node_spacing   # horizontal gap between components

  comp    <- .find_components(Au)
  n_comps <- max(comp)

  x_out <- numeric(n)
  y_out <- numeric(n)
  x_cursor <- svg_padding + half_w   # left edge for next component

  for (ci in seq_len(n_comps)) {
    idx  <- which(comp == ci)
    m    <- length(idx)
    rc   <- .rank_component(A[idx, idx, drop = FALSE])
    rank_c <- rc$rank
    A_c    <- rc$A

    # User-defined hierarchy_rank override
    if ("hierarchy_rank" %in% names(node_props)) {
      user_rank_c <- suppressWarnings(as.integer(node_props$hierarchy_rank[idx]))
      if (any(!is.na(user_rank_c))) {
        rank_c[!is.na(user_rank_c)] <- user_rank_c[!is.na(user_rank_c)]
        Au_c <- A_c | t(A_c)
        A_c  <- Au_c & outer(rank_c, rank_c, function(r, s) s == r + 1L)
      }
    }

    # x-slot: barycenter heuristic within this component
    x_slot_c <- numeric(m)
    n_ranks_c <- max(rank_c) + 1L
    for (r in seq_len(n_ranks_c) - 1L) {
      members_c <- which(rank_c == r)
      if (!length(members_c)) next
      score_c <- vapply(members_c, function(v) {
        preds <- which(A_c[, v] != 0)
        if (!length(preds)) 0 else mean(x_slot_c[preds])
      }, numeric(1L))
      x_slot_c[members_c[order(score_c)]] <- seq_along(members_c)
    }

    # Centre each rank (pyramid shape)
    rank_counts_c <- vapply(seq_len(n_ranks_c) - 1L,
                            function(r) sum(rank_c == r), integer(1L))
    max_k_c       <- max(rank_counts_c)
    rank_offset_c <- (max_k_c - rank_counts_c) / 2 * h_gap

    x_out[idx] <- x_cursor + (x_slot_c - 1L) * h_gap + rank_offset_c[rank_c + 1L]
    y_out[idx] <- svg_padding + half_h + rank_c * v_gap

    comp_width <- (max_k_c - 1L) * h_gap
    x_cursor   <- x_cursor + comp_width + comp_gap
  }

  node_props$x <- x_out
  node_props$y <- y_out
  node_props
}

#' @keywords internal
#' @noRd
.layout_bipartite <- function(adj_matrix, node_props,
                               default_width, default_height, svg_padding,
                               node_spacing = 1.0) {
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
  nw_max  <- if (any(is.finite(nw))) max(nw, na.rm = TRUE) else default_width
  nh_max  <- if (any(is.finite(nh))) max(nh, na.rm = TRUE) else default_height
  h_gap   <- nh_max * 1.8 * node_spacing
  col_gap <- nw_max * 5.0 * node_spacing
  half_w  <- nw_max / 2
  half_h  <- nh_max / 2

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

# ── Eigenvector centrality helper ─────────────────────────────────────────────

#' Eigenvector centrality via power iteration (base R, no dependencies)
#'
#' Computes the principal eigenvector of the symmetrised adjacency matrix
#' A_sym = (A != 0) | t(A != 0) using up to 200 iterations of power iteration
#' with L-inf normalisation.  Converges when the maximum element-wise change
#' between successive iterates falls below 1e-9.
#'
#' For disconnected graphs only the largest connected component receives
#' non-zero scores; isolated nodes remain at zero.  When the graph has no
#' edges at all (A_sym is all-zero), every node gets score 1/n so that
#' `which.max` returns the first node rather than erroring.
#'
#' @return Named numeric vector of centrality scores, one per node, in the
#'   same order as `rownames(adj_matrix)`.
#'
#' @keywords internal
#' @noRd
.eigenvector_centrality <- function(adj_matrix) {
  n     <- nrow(adj_matrix)
  A     <- adj_matrix != 0
  A_sym <- A | t(A)               # symmetrise: treat directed edges as undirected

  v <- rep(1 / n, n)
  for (iter in seq_len(200L)) {
    v_new <- as.numeric(A_sym %*% v)
    norm  <- max(abs(v_new))
    if (norm < 1e-12) break       # no edges — all scores stay equal
    v_new <- v_new / norm
    if (max(abs(v_new - v)) < 1e-9) { v <- v_new; break }
    v <- v_new
  }
  setNames(v, rownames(adj_matrix))
}
