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
#'
#' @return Invisibly: a named list with elements `svg`, `dot`, and `mermaid`,
#'   each a single character string containing the full source of that format.
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
    edge_colour        = "#444444",
    edge_width         = 1.5
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

  # ── 1–3. Build representations ─────────────────────────────────────────────
  svg_str <- .svg_build(adj_matrix, node_props, directed,
                        svg_padding, edge_colour, edge_width)
  dot_str <- .dot_build(adj_matrix, node_props, directed)
  mmd_str <- .mmd_build(adj_matrix, node_props, directed)

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

  invisible(list(svg = svg_str, dot = dot_str, mermaid = mmd_str))
}
