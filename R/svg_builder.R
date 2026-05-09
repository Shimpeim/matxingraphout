# Internal functions for SVG rendering.
# None of these are exported; users interact only via graph_to_outputs().


# Convert linetype name to SVG stroke-dasharray value
.dasharray <- function(lt) {
  switch(tolower(trimws(as.character(lt))),
    dashed   = "6,3",
    dotted   = "2,3",
    longdash = "12,5",
    twodash  = "10,4,2,4",
    ""   # solid or unknown: no dasharray
  )
}


# ── Main SVG builder ──────────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.svg_build <- function(adj, np, directed, pad, ecol, ew,
                       adj_ov = NULL, ovcol = "#999999",
                       ovw = 1.0, ovstyle = "dashed",
                       radial_center = NULL,
                       centroids = NULL,
                       edge_curvature = "auto",
                       overlay_edge_curvature = "auto",
                       show_centroids = FALSE,
                       edge_labels = NULL,
                       overlay_edge_labels = NULL,
                       edge_props = NULL, overlay_edge_props = NULL,
                       show_legend = FALSE, legend_node_shape = NULL,
                       legend_node_colour = NULL, legend_title = "Legend") {
  n <- nrow(adj)

  # Canvas extents from node bounding boxes + padding
  xlo <- min(np$x - np$width  / 2) - pad
  xhi <- max(np$x + np$width  / 2) + pad
  ylo <- min(np$y - np$height / 2) - pad
  yhi <- max(np$y + np$height / 2) + pad
  W   <- xhi - xlo
  H   <- yhi - ylo

  # Shift node centres so the canvas starts at (0, 0)
  np$sx <- np$x - xlo
  np$sy <- np$y - ylo

  # Arc-origin mode flags (shifted to canvas coordinates).
  # Two modes, mutually exclusive:
  #   centroid mode  — per-edge: nearest user centroid to the edge midpoint
  #   hub mode       — global:   eigenvector-centrality hub node (radial_center)
  has_rc        <- !is.null(radial_center)
  rc_sx         <- if (has_rc) radial_center[1] - xlo else 0
  rc_sy         <- if (has_rc) radial_center[2] - ylo else 0

  centroids_sh  <- NULL
  use_centroids <- !is.null(centroids) && nrow(centroids) > 0L
  if (use_centroids) {
    centroids_sh <- data.frame(
      x = as.numeric(centroids$x) - xlo,
      y = as.numeric(centroids$y) - ylo
    )
  }

  # In hub mode, pre-compute whether arc routing is active for each edge type
  use_arc    <- !use_centroids && has_rc && edge_curvature         != "straight"
  use_arc_ov <- !use_centroids && has_rc && overlay_edge_curvature != "straight"

  # ── Auto-scale canvas to eliminate label overlaps ─────────────────────────
  # Iteratively expand the node layout (scale canvas coords from centroid)
  # until no edge label overlaps any other label or any node bounding box.
  # Stops after 15 iterations (~15 % growth per step → up to ~8× expansion).
  for (.auto_iter in seq_len(15L)) {
    .boxes <- .collect_label_bboxes(
      np, adj, adj_ov, edge_labels, overlay_edge_labels,
      edge_curvature, overlay_edge_curvature,
      use_centroids, centroids_sh, use_arc, use_arc_ov,
      rc_sx, rc_sy, directed)
    if (!.has_label_overlaps(.boxes)) break

    .STEP <- 1.15
    .cx   <- mean(np$sx);  .cy <- mean(np$sy)
    np$sx <- .cx + (np$sx - .cx) * .STEP
    np$sy <- .cy + (np$sy - .cy) * .STEP

    # Re-anchor so canvas left/top edge stays at pad
    .dx <- min(np$sx - np$width  / 2) - pad
    .dy <- min(np$sy - np$height / 2) - pad
    np$sx <- np$sx - .dx
    np$sy <- np$sy - .dy

    if (use_centroids && !is.null(centroids_sh) && nrow(centroids_sh) > 0L) {
      centroids_sh$x <- .cx + (centroids_sh$x - .cx) * .STEP - .dx
      centroids_sh$y <- .cy + (centroids_sh$y - .cy) * .STEP - .dy
    }
    if (has_rc) {
      rc_sx <- .cx + (rc_sx - .cx) * .STEP - .dx
      rc_sy <- .cy + (rc_sy - .cy) * .STEP - .dy
    }

    W <- max(np$sx + np$width  / 2) + pad
    H <- max(np$sy + np$height / 2) + pad
  }

  # ── Edge-props style lookup ──────────────────────────────────────────────────
  # Returns list(colour, width, linetype) for a given weight value
  .ep_lookup <- function(ep, v, def_col, def_w, def_lt = "solid") {
    if (!is.null(ep)) {
      idx <- which(ep$weight == v)
      if (length(idx) > 0L) {
        row <- ep[idx[1L], ]
        col <- if ("colour"   %in% names(row) && !is.na(row$colour))   row$colour   else def_col
        w   <- if ("width"    %in% names(row) && !is.na(row$width))    row$width    else def_w
        lt  <- if ("linetype" %in% names(row) && !is.na(row$linetype)) row$linetype else def_lt
        return(list(colour = col, width = w, linetype = lt))
      }
    }
    list(colour = def_col, width = def_w, linetype = def_lt)
  }

  # Collect all unique colours that will appear on structural edges
  # (needed to generate per-colour arrowhead markers)
  all_struct_cols <- ecol
  if (!is.null(edge_props) && "colour" %in% names(edge_props))
    all_struct_cols <- unique(c(ecol, na.omit(edge_props$colour)))
  all_ov_cols <- ovcol
  if (!is.null(adj_ov) && !is.null(overlay_edge_props) && "colour" %in% names(overlay_edge_props))
    all_ov_cols <- unique(c(ovcol, na.omit(overlay_edge_props$colour)))

  # ── Pre-compute legend items ─────────────────────────────────────────────────
  LEG_PAD   <- 12L   # padding around legend box
  LEG_ROW_H <- 24L   # height per legend row
  LEG_W     <- 230L  # legend box width
  LEG_TITLE_H <- 22L # legend title row height
  LEG_ICON_W  <- 36L # icon area width in each row
  LEG_GAP     <- 10L # gap between graph and legend box

  legend_items <- list()
  if (isTRUE(show_legend)) {
    if (!is.null(legend_node_shape) && length(legend_node_shape) > 0L) {
      legend_items <- c(legend_items, list(list(section = "Shapes")))
      for (sh in names(legend_node_shape))
        legend_items <- c(legend_items,
          list(list(type = "shape", shape = sh,
                    label = as.character(legend_node_shape[[sh]]))))
    }
    if (!is.null(legend_node_colour) && length(legend_node_colour) > 0L) {
      legend_items <- c(legend_items, list(list(section = "Node colours")))
      for (co in names(legend_node_colour))
        legend_items <- c(legend_items,
          list(list(type = "colour", colour = co,
                    label = as.character(legend_node_colour[[co]]))))
    }
    if (!is.null(edge_props) && "label" %in% names(edge_props)) {
      edge_rows <- edge_props[!is.na(edge_props$label) & nzchar(trimws(edge_props$label)), ]
      if (nrow(edge_rows) > 0L) {
        legend_items <- c(legend_items, list(list(section = "Edge types")))
        for (k in seq_len(nrow(edge_rows))) {
          es <- .ep_lookup(edge_props, edge_rows$weight[k], ecol, ew)
          legend_items <- c(legend_items,
            list(list(type = "edge", colour = es$colour, width = es$width,
                      linetype = es$linetype, label = trimws(edge_rows$label[k]))))
        }
      }
    }
    if (!is.null(overlay_edge_props) && "label" %in% names(overlay_edge_props)) {
      ov_rows <- overlay_edge_props[!is.na(overlay_edge_props$label) &
                                     nzchar(trimws(overlay_edge_props$label)), ]
      if (nrow(ov_rows) > 0L) {
        if (!any(vapply(legend_items, function(x) isTRUE(x$section == "Edge types"), logical(1L))))
          legend_items <- c(legend_items, list(list(section = "Edge types")))
        for (k in seq_len(nrow(ov_rows))) {
          es <- .ep_lookup(overlay_edge_props, ov_rows$weight[k], ovcol, ovw)
          legend_items <- c(legend_items,
            list(list(type = "edge", colour = es$colour, width = es$width,
                      linetype = es$linetype, label = trimws(ov_rows$label[k]))))
        }
      }
    }
  }
  n_legend <- length(legend_items)
  # count only non-section rows for height
  n_data_rows <- if (n_legend == 0L) 0L else
    sum(vapply(legend_items, function(x) is.null(x$section), logical(1L)))
  n_sec_rows  <- n_legend - n_data_rows
  leg_content_h <- if (n_legend > 0L)
    LEG_TITLE_H + n_data_rows * LEG_ROW_H + n_sec_rows * 18L + LEG_PAD * 2L
  else 0L
  H_total <- H + if (leg_content_h > 0L) leg_content_h + LEG_GAP else 0L

  buf <- character(0)
  .emit <- function(...) buf <<- c(buf, paste0(...))

  # ── Header ─────────────────────────────────────────────────────────────────
  .emit('<?xml version="1.0" encoding="UTF-8"?>')
  .emit('<svg xmlns="http://www.w3.org/2000/svg"',
        ' width="',  round(W), '" height="', round(H_total), '"',
        ' viewBox="0 0 ', round(W), ' ', round(H_total), '"',
        ' overflow="visible">')

  if (directed) {
    .emit('  <defs>')
    # Default structural arrowhead
    .emit('    <marker id="arrowhead" markerWidth="10" markerHeight="7"',
          ' refX="9" refY="3.5" orient="auto">',
          '<polygon points="0 0,10 3.5,0 7" fill="', ecol, '"/></marker>')
    # Extra structural arrowheads for non-default colours from edge_props
    for (col in setdiff(all_struct_cols, ecol)) {
      safe_id <- gsub("[^A-Za-z0-9]", "", col)
      .emit('    <marker id="ah-', safe_id, '" markerWidth="10" markerHeight="7"',
            ' refX="9" refY="3.5" orient="auto">',
            '<polygon points="0 0,10 3.5,0 7" fill="', col, '"/></marker>')
    }
    if (!is.null(adj_ov)) {
      # Default overlay arrowhead
      .emit('    <marker id="arrowhead-ov" markerWidth="10" markerHeight="7"',
            ' refX="9" refY="3.5" orient="auto">',
            '<polygon points="0 0,10 3.5,0 7" fill="', ovcol, '"/></marker>')
      # Extra overlay arrowheads
      for (col in setdiff(all_ov_cols, ovcol)) {
        safe_id <- gsub("[^A-Za-z0-9]", "", col)
        .emit('    <marker id="ahov-', safe_id, '" markerWidth="10" markerHeight="7"',
              ' refX="9" refY="3.5" orient="auto">',
              '<polygon points="0 0,10 3.5,0 7" fill="', col, '"/></marker>')
      }
    }
    .emit('  </defs>')
  }

  # Default edge CSS (used when edge_props has no match for a weight)
  edge_css_default <- sprintf('stroke="%s" stroke-width="%g" fill="none"', ecol, ew)
  mar_attr_default <- if (directed) ' marker-end="url(#arrowhead)"' else ''

  # ── Edges ──────────────────────────────────────────────────────────────────
  .emit('  <!-- edges -->')
  done <- matrix(FALSE, n, n)

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      v <- adj[i, j]
      if (v == 0 || (!directed && done[j, i])) next

      ax <- np$sx[i];  ay <- np$sy[i]
      bx <- np$sx[j];  by <- np$sy[j]

      if (i == j) {
        # Self-loop: cubic bezier curling above-right of the node
        rx <- np$width[i]  / 2;  ry <- np$height[i] / 2
        ox <- ax + rx * 1.4;    oy <- ay - ry * 1.4
        .emit('  <path d="M ', round(ax + rx * .5, 1), ',', round(ay - ry, 1),
              ' C ', round(ox, 1), ',', round(ay - ry, 1),
              ' ',  round(ox, 1), ',', round(oy, 1),
              ' ',  round(ax + rx, 1), ',', round(ay, 1), '"',
              mar_attr_default, ' ', edge_css_default, '/>')
      } else {
        from <- .boundary_pt(ax, ay, bx - ax, by - ay,
                             np$shape[i], np$width[i], np$height[i])
        to   <- .boundary_pt(bx, by, ax - bx, ay - by,
                             np$shape[j], np$width[j], np$height[j])

        # Pull endpoint back 2 px so the arrowhead clears the node fill
        if (directed) {
          d <- sqrt((to[1] - from[1])^2 + (to[2] - from[2])^2)
          if (d > 2) {
            ux <- (to[1] - from[1]) / d;  uy <- (to[2] - from[2]) / d
            to <- to - c(ux, uy) * 2
          }
        }

        # Per-edge visual style
        es       <- .ep_lookup(edge_props, v, ecol, ew, "solid")
        da       <- .dasharray(es$linetype)
        edge_css_this <- sprintf('stroke="%s" stroke-width="%g" fill="none"%s',
                                 es$colour, es$width,
                                 if (nzchar(da)) paste0(' stroke-dasharray="', da, '"') else "")
        if (directed) {
          safe_col <- gsub("[^A-Za-z0-9]", "", es$colour)
          mar_attr_this <- if (identical(es$colour, ecol))
            ' marker-end="url(#arrowhead)"'
          else
            paste0(' marker-end="url(#ah-', safe_col, ')"')
        } else {
          mar_attr_this <- ''
        }

        arc_origin <- NULL
        if (edge_curvature == "straight") {
          arc <- NULL
        } else if (use_centroids) {
          cmx   <- (from[1] + to[1]) / 2
          cmy   <- (from[2] + to[2]) / 2
          dists <- (centroids_sh$x - cmx)^2 + (centroids_sh$y - cmy)^2
          nc    <- centroids_sh[which.min(dists), , drop = FALSE]
          arc_origin <- c(nc$x, nc$y)
          arc   <- .circumcircle_arc_svg(from[1] - nc$x, from[2] - nc$y,
                                         to[1]   - nc$x, to[2]   - nc$y)
        } else if (use_arc) {
          arc_origin <- c(rc_sx, rc_sy)
          arc   <- .circumcircle_arc_svg(from[1] - rc_sx, from[2] - rc_sy,
                                         to[1]   - rc_sx, to[2]   - rc_sy)
        } else {
          arc <- NULL
        }

        if (!is.null(arc)) {
          .emit('  <path d="M ', round(from[1], 1), ',', round(from[2], 1),
                ' A ', arc$R, ',', arc$R, ' 0 ',
                arc$large_arc, ',', arc$sweep,
                ' ', round(to[1], 1), ',', round(to[2], 1), '"',
                mar_attr_this, ' ', edge_css_this, '/>')
        } else {
          .emit('  <line x1="', round(from[1], 1), '" y1="', round(from[2], 1),
                '" x2="', round(to[1], 1), '" y2="', round(to[2], 1), '"',
                mar_attr_this, ' ', edge_css_this, '/>')
        }

        # Edge label
        {
          lbl_txt <- NULL
          if (!is.null(edge_labels) &&
              !is.na(edge_labels[i, j]) &&
              nzchar(trimws(edge_labels[i, j])))
            lbl_txt <- trimws(edge_labels[i, j])
          else if (v != 1)
            lbl_txt <- format(v, trim = TRUE)
          if (!is.null(lbl_txt)) {
            .lf <- if (directed) 0.4 else 0.5
            if (!is.null(arc) && !is.null(arc_origin)) {
              lp <- .arc_label_pt(from, to, arc_origin, frac = .lf)
              lbl_x <- lp[1]; lbl_y <- lp[2]
            } else {
              lbl_x <- from[1] + .lf * (to[1] - from[1])
              lbl_y <- from[2] + .lf * (to[2] - from[2])
            }
            .emit('  <text x="', round(lbl_x, 1), '" y="', round(lbl_y, 1), '"',
                  ' text-anchor="middle" dominant-baseline="auto"',
                  ' dy="-4" font-size="10" fill="', es$colour, '"',
                  ' font-family="Helvetica,Arial,sans-serif">',
                  .xml_esc(lbl_txt), '</text>')
          }
        }
      }

      done[i, j] <- TRUE
    }
  }

  # ── Overlay edges (drawn after structural edges, before nodes) ─────────────
  if (!is.null(adj_ov)) {
    .emit('  <!-- overlay edges -->')
    done_ov  <- matrix(FALSE, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        v <- adj_ov[i, j]
        if (v == 0 || (!directed && done_ov[j, i])) next
        ax <- np$sx[i];  ay <- np$sy[i]
        bx <- np$sx[j];  by <- np$sy[j]
        if (i == j) {
          rx <- np$width[i]  / 2;  ry <- np$height[i] / 2
          ox <- ax + rx * 1.4;    oy <- ay - ry * 1.4
          # Self-loop uses global overlay defaults
          ov_dash_sl  <- if (ovstyle == "dashed") ' stroke-dasharray="5,3"' else ''
          ov_css_sl   <- sprintf('stroke="%s" stroke-width="%g" fill="none"%s',
                                 ovcol, ovw, ov_dash_sl)
          ov_mar_sl   <- if (directed) ' marker-end="url(#arrowhead-ov)"' else ''
          .emit('  <path d="M ', round(ax + rx * .5, 1), ',', round(ay - ry, 1),
                ' C ', round(ox, 1), ',', round(ay - ry, 1),
                ' ',  round(ox, 1), ',', round(oy, 1),
                ' ',  round(ax + rx, 1), ',', round(ay, 1), '"',
                ov_mar_sl, ' ', ov_css_sl, '/>')
        } else {
          from <- .boundary_pt(ax, ay, bx - ax, by - ay,
                               np$shape[i], np$width[i], np$height[i])
          to   <- .boundary_pt(bx, by, ax - bx, ay - by,
                               np$shape[j], np$width[j], np$height[j])
          if (directed) {
            d <- sqrt((to[1] - from[1])^2 + (to[2] - from[2])^2)
            if (d > 2) {
              ux <- (to[1] - from[1]) / d;  uy <- (to[2] - from[2]) / d
              to <- to - c(ux, uy) * 2
            }
          }

          # Per-overlay-edge visual style
          ovs     <- .ep_lookup(overlay_edge_props, v, ovcol, ovw, ovstyle)
          ov_da   <- .dasharray(ovs$linetype)
          ov_css_this <- sprintf('stroke="%s" stroke-width="%g" fill="none"%s',
                                 ovs$colour, ovs$width,
                                 if (nzchar(ov_da)) paste0(' stroke-dasharray="', ov_da, '"') else "")
          if (directed) {
            safe_ov_col <- gsub("[^A-Za-z0-9]", "", ovs$colour)
            ov_mar_this <- if (identical(ovs$colour, ovcol))
              ' marker-end="url(#arrowhead-ov)"'
            else
              paste0(' marker-end="url(#ahov-', safe_ov_col, ')"')
          } else {
            ov_mar_this <- ''
          }

          arc_ov_origin <- NULL
          if (overlay_edge_curvature == "straight") {
            arc_ov <- NULL
          } else if (use_centroids) {
            cmx_ov <- (from[1] + to[1]) / 2
            cmy_ov <- (from[2] + to[2]) / 2
            dists  <- (centroids_sh$x - cmx_ov)^2 + (centroids_sh$y - cmy_ov)^2
            nc_ov  <- centroids_sh[which.min(dists), , drop = FALSE]
            arc_ov_origin <- c(nc_ov$x, nc_ov$y)
            arc_ov <- .circumcircle_arc_svg(from[1] - nc_ov$x, from[2] - nc_ov$y,
                                            to[1]   - nc_ov$x, to[2]   - nc_ov$y)
          } else if (use_arc_ov) {
            arc_ov_origin <- c(rc_sx, rc_sy)
            arc_ov <- .circumcircle_arc_svg(from[1] - rc_sx, from[2] - rc_sy,
                                            to[1]   - rc_sx, to[2]   - rc_sy)
          } else {
            arc_ov <- NULL
          }

          if (!is.null(arc_ov)) {
            .emit('  <path d="M ', round(from[1], 1), ',', round(from[2], 1),
                  ' A ', arc_ov$R, ',', arc_ov$R, ' 0 ',
                  arc_ov$large_arc, ',', arc_ov$sweep,
                  ' ', round(to[1], 1), ',', round(to[2], 1), '"',
                  ov_mar_this, ' ', ov_css_this, '/>')
          } else {
            .emit('  <line x1="', round(from[1], 1), '" y1="', round(from[2], 1),
                  '" x2="', round(to[1], 1), '" y2="', round(to[2], 1), '"',
                  ov_mar_this, ' ', ov_css_this, '/>')
          }

          # Overlay edge label
          {
            ov_lbl_txt <- NULL
            if (!is.null(overlay_edge_labels) &&
                !is.na(overlay_edge_labels[i, j]) &&
                nzchar(trimws(overlay_edge_labels[i, j])))
              ov_lbl_txt <- trimws(overlay_edge_labels[i, j])
            else if (v != 1)
              ov_lbl_txt <- format(v, trim = TRUE)
            if (!is.null(ov_lbl_txt)) {
              .lf_ov <- if (directed) 0.4 else 0.5
              if (!is.null(arc_ov) && !is.null(arc_ov_origin)) {
                lp_ov  <- .arc_label_pt(from, to, arc_ov_origin, frac = .lf_ov)
                ov_lbl_x <- lp_ov[1]; ov_lbl_y <- lp_ov[2]
              } else {
                ov_lbl_x <- from[1] + .lf_ov * (to[1] - from[1])
                ov_lbl_y <- from[2] + .lf_ov * (to[2] - from[2])
              }
              .emit('  <text x="', round(ov_lbl_x, 1), '" y="', round(ov_lbl_y, 1), '"',
                    ' text-anchor="middle" dominant-baseline="auto"',
                    ' dy="-4" font-size="10" fill="', ovs$colour, '"',
                    ' font-family="Helvetica,Arial,sans-serif">',
                    .xml_esc(ov_lbl_txt), '</text>')
            }
          }
          done_ov[i, j] <- TRUE
        }
      }
    }
  }

  # ── Nodes ──────────────────────────────────────────────────────────────────
  .emit('  <!-- nodes -->')

  for (i in seq_len(n)) {
    cx   <- np$sx[i];  cy  <- np$sy[i]
    w    <- np$width[i];   h  <- np$height[i]
    shp  <- tolower(trimws(np$shape[i]))
    natt <- sprintf('fill="%s" stroke="%s" stroke-width="1.5"',
                    np$colour[i], np$stroke[i])

    switch(shp,
      circle = .emit(
        '  <circle cx="', round(cx, 1), '" cy="', round(cy, 1),
        '" r="', round(min(w, h) / 2, 1), '" ', natt, '/>'
      ),
      ellipse = .emit(
        '  <ellipse cx="', round(cx, 1), '" cy="', round(cy, 1),
        '" rx="', round(w / 2, 1), '" ry="', round(h / 2, 1), '" ', natt, '/>'
      ),
      diamond = {
        pts <- paste(
          c(round(cx,       1), round(cx + w/2, 1),
            round(cx,       1), round(cx - w/2, 1)),
          c(round(cy - h/2, 1), round(cy,       1),
            round(cy + h/2, 1), round(cy,       1)),
          sep = ",", collapse = " "
        )
        .emit('  <polygon points="', pts, '" ', natt, '/>')
      },
      rounded = .emit(
        '  <rect x="', round(cx - w/2, 1), '" y="', round(cy - h/2, 1),
        '" width="', round(w, 1), '" height="', round(h, 1),
        '" rx="', round(min(w, h) * .22, 1), '" ', natt, '/>'
      ),
      # default: plain rectangle
      .emit(
        '  <rect x="', round(cx - w/2, 1), '" y="', round(cy - h/2, 1),
        '" width="', round(w, 1), '" height="', round(h, 1), '" ', natt, '/>'
      )
    )

    # Label — multi-line via "\n"
    lbl_lines <- strsplit(as.character(np$label[i]), "\n", fixed = TRUE)[[1]]
    nl  <- length(lbl_lines)
    lh  <- np$fontsize[i] * 1.35
    y0  <- cy - (nl - 1) * lh / 2

    for (k in seq_along(lbl_lines)) {
      .emit('  <text x="', round(cx, 1), '" y="', round(y0 + (k - 1) * lh, 1),
            '" text-anchor="middle" dominant-baseline="middle"',
            ' font-size="', np$fontsize[i], '" fill="', np$fontcolour[i], '"',
            ' font-family="Helvetica,Arial,sans-serif">',
            .xml_esc(lbl_lines[k]), '</text>')
    }
  }

  # ── Centroid markers ────────────────────────────────────────────────────────
  if (show_centroids && use_centroids) {
    .emit('  <!-- centroid markers -->')
    for (k in seq_len(nrow(centroids_sh))) {
      cx_k <- round(centroids_sh$x[k], 1)
      cy_k <- round(centroids_sh$y[k], 1)
      lbl  <- if ("label" %in% names(centroids) &&
                   !is.na(centroids$label[k]) &&
                   nzchar(trimws(centroids$label[k])))
                .xml_esc(as.character(centroids$label[k]))
              else paste0("C", k)
      .emit('  <g class="centroid-marker" data-centroid-idx="', k - 1L, '" style="cursor:pointer">')
      .emit('    <circle cx="', cx_k, '" cy="', cy_k, '" r="12"',
            ' fill="#e53e3e" fill-opacity="0.65"',
            ' stroke="#e53e3e" stroke-width="2"/>')
      .emit('    <line x1="', cx_k - 16, '" y1="', cy_k,
            '" x2="', cx_k + 16, '" y2="', cy_k,
            '" stroke="#e53e3e" stroke-width="2"/>')
      .emit('    <line x1="', cx_k, '" y1="', cy_k - 16,
            '" x2="', cx_k, '" y2="', cy_k + 16,
            '" stroke="#e53e3e" stroke-width="2"/>')
      .emit('    <text x="', cx_k + 13, '" y="', cy_k - 4,
            '" font-size="10" fill="#e53e3e"',
            ' font-family="Helvetica,Arial,sans-serif">', lbl, '</text>')
      .emit('  </g>')
    }
  }

  # ── Legend ──────────────────────────────────────────────────────────────────
  if (n_legend > 0L) {
    lx  <- LEG_PAD                 # legend box left x
    ly  <- H + LEG_GAP             # legend box top y (below graph)
    lbw <- min(LEG_W, round(W - 2 * LEG_PAD))  # box width capped to canvas
    .emit('  <!-- legend -->')
    .emit('  <rect x="', lx, '" y="', round(ly), '" width="', lbw,
          '" height="', round(leg_content_h),
          '" fill="white" stroke="#cbd5e0" stroke-width="1" rx="4"/>')
    # Title
    .emit('  <text x="', lx + LEG_PAD, '" y="', round(ly + LEG_TITLE_H - 5),
          '" font-size="11" font-weight="bold" fill="#2d3748"',
          ' font-family="Helvetica,Arial,sans-serif">',
          .xml_esc(legend_title), '</text>')
    .emit('  <line x1="', lx + 4, '" y1="', round(ly + LEG_TITLE_H),
          '" x2="', lx + lbw - 4, '" y2="', round(ly + LEG_TITLE_H),
          '" stroke="#e2e8f0" stroke-width="1"/>')

    row_y <- ly + LEG_TITLE_H + 4L
    for (item in legend_items) {
      if (!is.null(item$section)) {
        # Section header
        .emit('  <text x="', lx + LEG_PAD, '" y="', round(row_y + 13),
              '" font-size="9" font-weight="bold" fill="#718096"',
              ' font-family="Helvetica,Arial,sans-serif" text-transform="uppercase">',
              .xml_esc(toupper(item$section)), '</text>')
        row_y <- row_y + 18L
        next
      }
      icon_cx <- lx + LEG_PAD + LEG_ICON_W / 2
      text_x  <- lx + LEG_PAD + LEG_ICON_W + 4L
      row_cy  <- row_y + LEG_ROW_H / 2

      if (item$type == "shape") {
        sh <- tolower(trimws(item$shape))
        sw <- 24L; sh_h <- 14L
        natt <- 'fill="#e8f0fe" stroke="#4a5568" stroke-width="1"'
        switch(sh,
          circle  = .emit('  <circle cx="', round(icon_cx), '" cy="', round(row_cy),
                          '" r="8" ', natt, '/>'),
          ellipse = .emit('  <ellipse cx="', round(icon_cx), '" cy="', round(row_cy),
                          '" rx="12" ry="7" ', natt, '/>'),
          diamond = .emit('  <polygon points="',
                          round(icon_cx), ',', round(row_cy - 8),  ' ',
                          round(icon_cx + 12), ',', round(row_cy),  ' ',
                          round(icon_cx), ',', round(row_cy + 8),  ' ',
                          round(icon_cx - 12), ',', round(row_cy), '" ', natt, '/>'),
          rounded = .emit('  <rect x="', round(icon_cx - 12), '" y="', round(row_cy - 7),
                          '" width="24" height="14" rx="4" ', natt, '/>'),
          .emit('  <rect x="', round(icon_cx - 12), '" y="', round(row_cy - 7),
                '" width="24" height="14" ', natt, '/>')
        )
      } else if (item$type == "colour") {
        .emit('  <rect x="', round(icon_cx - 10), '" y="', round(row_cy - 7),
              '" width="20" height="14" fill="', item$colour,
              '" stroke="#4a5568" stroke-width="1" rx="2"/>')
      } else if (item$type == "edge") {
        da <- .dasharray(item$linetype)
        ecs <- sprintf('stroke="%s" stroke-width="%g" fill="none"%s',
                       item$colour, item$width,
                       if (nzchar(da)) paste0(' stroke-dasharray="', da, '"') else "")
        .emit('  <line x1="', round(icon_cx - 14), '" y1="', round(row_cy),
              '" x2="', round(icon_cx + 14), '" y2="', round(row_cy),
              '" ', ecs, '/>')
        if (directed)
          .emit('  <polygon points="',
                round(icon_cx + 14), ',', round(row_cy), ' ',
                round(icon_cx + 9),  ',', round(row_cy - 3), ' ',
                round(icon_cx + 9),  ',', round(row_cy + 3),
                '" fill="', item$colour, '"/>')
      }

      .emit('  <text x="', text_x, '" y="', round(row_cy + 4),
            '" font-size="10" fill="#2d3748"',
            ' font-family="Helvetica,Arial,sans-serif">',
            .xml_esc(item$label), '</text>')
      row_y <- row_y + LEG_ROW_H
    }
  }

  .emit('</svg>')
  paste(buf, collapse = "\n")
}


# ── Geometry helper ───────────────────────────────────────────────────────────

#' Point where a ray from a node centre exits the node boundary
#'
#' Given a node centred at (cx, cy) and a direction vector (dx, dy), returns
#' the point on the node's boundary shape that the ray first reaches.
#' Used internally to trim edge endpoints so lines meet the shape edge, not
#' the centre.
#'
#' @keywords internal
#' @noRd
.boundary_pt <- function(cx, cy, dx, dy, shape, w, h) {
  len <- sqrt(dx^2 + dy^2)
  if (len < 1e-9) return(c(cx, cy))
  ux  <- dx / len;  uy <- dy / len
  shp <- tolower(trimws(shape))

  t <- switch(shp,
    circle  = min(w, h) / 2,

    ellipse = {
      a <- w / 2;  b <- h / 2
      1 / sqrt((ux / a)^2 + (uy / b)^2)
    },

    diamond = {
      # Boundary equation: |x / (w/2)| + |y / (h/2)| = 1
      1 / (abs(ux) / (w / 2) + abs(uy) / (h / 2) + 1e-12)
    },

    {  # rect / rounded — same rectangular bounding box
      tx <- if (abs(ux) > 1e-9) (w / 2) / abs(ux) else Inf
      ty <- if (abs(uy) > 1e-9) (h / 2) / abs(uy) else Inf
      min(tx, ty)
    }
  )
  c(cx + ux * t, cy + uy * t)
}


# ── XML escaping ──────────────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.xml_esc <- function(s) {
  s <- gsub("&",  "&amp;",  s, fixed = TRUE)
  s <- gsub("<",  "&lt;",   s, fixed = TRUE)
  s <- gsub(">",  "&gt;",   s, fixed = TRUE)
  s <- gsub('"',  "&quot;", s, fixed = TRUE)
  s
}


# ── Circumscribed-circle arc helper ───────────────────────────────────────────

#' SVG arc parameters for the circumscribed circle of O, P1, P2
#'
#' ## Geometric rationale
#'
#' In radial layouts (sunburst, circular) every edge connects two nodes that
#' both lie on concentric circles around a common centre O.  A straight line
#' between distant nodes cuts through the interior of the diagram and visually
#' crosses the centre.  Routing each edge along the arc of the **circumscribed
#' circle** of the triangle formed by O, P1, and P2 keeps the edge at a
#' natural distance from O, producing curved lines that follow the radial
#' structure of the layout.
#'
#' ## Circle-centre derivation
#'
#' The circumscribed circle passes through O = (0, 0), P1 = (x1, y1), and
#' P2 = (x2, y2).  Its centre (cxc, cyc) satisfies three equal-distance
#' constraints.  Setting |centre - O|^2 = |centre - P1|^2 and
#' |centre - O|^2 = |centre - P2|^2 yields a 2x2 linear system whose solution
#' (via Cramer's rule) is:
#'
#'   denom = 2 * (x1*y2 - y1*x2)           # twice the signed area of the triangle
#'   cxc   = (y2*|P1|^2 - y1*|P2|^2) / denom
#'   cyc   = (x1*|P2|^2 - x2*|P1|^2) / denom
#'   R     = |centre - O| = sqrt(cxc^2 + cyc^2)
#'
#' When `denom ~= 0` the three points are collinear (no finite circle exists);
#' the function returns `NULL` and the caller draws a straight line instead.
#'
#' ## Arc-flag selection
#'
#' The circumscribed circle has two arcs from P1 to P2: one passes through O,
#' the other does not.  We always want the arc that avoids O.
#'
#' 1. Express the angles of P1, P2, and O on the circle as th1, th2, th0.
#' 2. The clockwise arc from P1 to P2 spans `delta_cw = (th2 - th1) mod 2*pi`
#'    radians.
#' 3. O lies on the CW arc iff `(th0 - th1) mod 2*pi <= delta_cw`.
#' 4. If O is on the CW arc, take the CCW arc (SVG sweep-flag = 0).
#'    Otherwise take the CW arc (SVG sweep-flag = 1).
#' 5. SVG large-arc-flag = 1 when the chosen arc spans more than pi radians.
#'
#' @keywords internal
#' @noRd
.circumcircle_arc_svg <- function(x1, y1, x2, y2) {
  # Circumscribed circle of triangle O=(0,0), P1, P2
  # denom = 2 × signed area of the triangle; zero means collinear
  denom <- 2 * (x1 * y2 - y1 * x2)
  if (abs(denom) < 1e-9) return(NULL)   # collinear → straight line

  r1sq <- x1^2 + y1^2
  r2sq <- x2^2 + y2^2
  # Circle centre via Cramer's rule (see derivation above)
  cxc  <- (y2 * r1sq - y1 * r2sq) / denom
  cyc  <- (x1 * r2sq - x2 * r1sq) / denom
  R    <- sqrt(cxc^2 + cyc^2)   # radius = distance from centre to O

  # Normalise angle to [0, 2π)
  n2pi <- function(a) ((a %% (2 * pi)) + 2 * pi) %% (2 * pi)

  th1 <- n2pi(atan2(y1 - cyc, x1 - cxc))   # angle of P1 on the circle
  th2 <- n2pi(atan2(y2 - cyc, x2 - cxc))   # angle of P2
  th0 <- n2pi(atan2(    -cyc,     -cxc))    # angle of O

  # Clockwise arc from P1 to P2 spans delta_cw radians
  delta_cw <- n2pi(th2 - th1)

  # Does O lie on the CW arc P1→P2?
  # O is on the CW arc iff its angular offset from P1 (going CW) ≤ delta_cw
  cw_has_O <- n2pi(th0 - th1) <= delta_cw

  if (cw_has_O) {
    # Take the CCW arc to avoid O (SVG sweep-flag = 0 means CCW)
    sweep <- 0L
    span  <- 2 * pi - delta_cw
  } else {
    # Take the CW arc (SVG sweep-flag = 1 means CW)
    sweep <- 1L
    span  <- delta_cw
  }

  list(
    R         = round(R, 2),
    large_arc = if (span > pi) 1L else 0L,   # SVG large-arc-flag
    sweep     = sweep
  )
}


# ── Label-overlap detection helpers ───────────────────────────────────────────

# Collect bounding boxes for all edge labels (label bboxes) and all nodes
# (node bboxes) in canvas coordinates.  Returns list(labels, nodes) where
# each element is a list of c(x1,y1,x2,y2) rectangles.
#
# Label bbox uses dominant-baseline="auto" dy="-4" convention: the text sits
# above its y-coordinate, so the box spans [y-LABEL_H, y].
#
# @keywords internal
# @noRd
.collect_label_bboxes <- function(np, adj, adj_ov,
                                   edge_labels, overlay_edge_labels,
                                   edge_curvature, overlay_edge_curvature,
                                   use_centroids, centroids_sh,
                                   use_arc, use_arc_ov,
                                   rc_sx, rc_sy, directed) {
  n       <- nrow(adj)
  CHAR_W  <- 5.5   # approx px per character at 10 px font
  LABEL_H <- 14    # total label height (ascender + descender + dy offset)
  MARGIN  <- 2     # extra padding on all sides of each label bbox

  lbl_pt <- function(from, to, curvature, arc_flag) {
    .lf <- if (directed) 0.4 else 0.5
    if (curvature == "straight" || !arc_flag)
      return(from + .lf * (to - from))
    if (use_centroids && !is.null(centroids_sh) && nrow(centroids_sh) > 0L) {
      cmx <- (from[1] + to[1]) / 2;  cmy <- (from[2] + to[2]) / 2
      dst <- (centroids_sh$x - cmx)^2 + (centroids_sh$y - cmy)^2
      nc  <- centroids_sh[which.min(dst), , drop = FALSE]
      return(.arc_label_pt(from, to, c(nc$x, nc$y), frac = .lf))
    }
    .arc_label_pt(from, to, c(rc_sx, rc_sy), frac = .lf)
  }

  get_lbl <- function(i, j, v, lmat) {
    if (!is.null(lmat) && i <= nrow(lmat) && j <= ncol(lmat) &&
        !is.na(lmat[i, j]) && nzchar(trimws(lmat[i, j])))
      return(trimws(lmat[i, j]))
    if (v != 1) return(format(v, trim = TRUE))
    NULL
  }

  label_bboxes <- list()
  add_lbl <- function(x, y, text) {
    w  <- nchar(text) * CHAR_W + 2 * MARGIN
    label_bboxes[[length(label_bboxes) + 1L]] <<-
      c(x - w / 2, y - LABEL_H - MARGIN, x + w / 2, y + MARGIN)
  }

  # Structural edge labels
  done <- matrix(FALSE, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) {
    v <- adj[i, j]
    if (v == 0 || (!directed && done[j, i]) || i == j) next
    done[i, j] <- TRUE
    lbl <- get_lbl(i, j, v, edge_labels)
    if (is.null(lbl)) next
    from <- .boundary_pt(np$sx[i], np$sy[i],
                          np$sx[j] - np$sx[i], np$sy[j] - np$sy[i],
                          np$shape[i], np$width[i], np$height[i])
    to   <- .boundary_pt(np$sx[j], np$sy[j],
                          np$sx[i] - np$sx[j], np$sy[i] - np$sy[j],
                          np$shape[j], np$width[j], np$height[j])
    pt   <- lbl_pt(from, to, edge_curvature, use_arc)
    add_lbl(pt[1], pt[2], lbl)
  }

  # Overlay edge labels
  if (!is.null(adj_ov)) {
    done_ov <- matrix(FALSE, n, n)
    for (i in seq_len(n)) for (j in seq_len(n)) {
      v <- adj_ov[i, j]
      if (v == 0 || (!directed && done_ov[j, i]) || i == j) next
      done_ov[i, j] <- TRUE
      lbl <- get_lbl(i, j, v, overlay_edge_labels)
      if (is.null(lbl)) next
      from <- .boundary_pt(np$sx[i], np$sy[i],
                            np$sx[j] - np$sx[i], np$sy[j] - np$sy[i],
                            np$shape[i], np$width[i], np$height[i])
      to   <- .boundary_pt(np$sx[j], np$sy[j],
                            np$sx[i] - np$sx[j], np$sy[i] - np$sy[j],
                            np$shape[j], np$width[j], np$height[j])
      pt   <- lbl_pt(from, to, overlay_edge_curvature, use_arc_ov)
      add_lbl(pt[1], pt[2], lbl)
    }
  }

  # Node bounding boxes
  node_bboxes <- lapply(seq_len(n), function(i)
    c(np$sx[i] - np$width[i]  / 2,  np$sy[i] - np$height[i] / 2,
      np$sx[i] + np$width[i]  / 2,  np$sy[i] + np$height[i] / 2))

  list(labels = label_bboxes, nodes = node_bboxes)
}


# TRUE iff any label bbox overlaps any other label bbox or any node bbox.
# @keywords internal
# @noRd
.has_label_overlaps <- function(boxes) {
  lbls  <- boxes$labels
  nodes <- boxes$nodes
  if (length(lbls) == 0L) return(FALSE)

  ov1d <- function(a1, a2, b1, b2) a1 < b2 && a2 > b1
  ov2d <- function(a, b)
    ov1d(a[1], a[3], b[1], b[3]) && ov1d(a[2], a[4], b[2], b[4])

  for (lb in lbls) for (nb in nodes)  if (ov2d(lb, nb)) return(TRUE)
  nl <- length(lbls)
  if (nl > 1L)
    for (i in seq_len(nl - 1L)) for (j in seq(i + 1L, nl))
      if (ov2d(lbls[[i]], lbls[[j]])) return(TRUE)
  FALSE
}


# ── Arc label-point helper ─────────────────────────────────────────────────────

#' Midpoint of the drawn arc between two edge endpoints.
#'
#' Replicates the circumcircle construction used by `.circumcircle_arc_svg()`
#' to find the circle centre, then returns the canvas point at the mid-angle
#' of the chosen arc (the arc that avoids the arc-origin O).  Falls back to
#' the chord midpoint when O, from, to are collinear (same condition that
#' makes `.circumcircle_arc_svg()` return NULL).
#'
#' @param from  Numeric(2) -- start endpoint in canvas coords.
#' @param to    Numeric(2) -- end endpoint in canvas coords.
#' @param origin Numeric(2) -- arc origin O in canvas coords
#'   (nearest centroid or hub node).
#' @return Numeric(2) canvas coordinates of the arc midpoint.
#' @keywords internal
#' @noRd
.arc_label_pt <- function(from, to, origin, frac = 0.5) {
  x1 <- from[1] - origin[1];  y1 <- from[2] - origin[2]
  x2 <- to[1]   - origin[1];  y2 <- to[2]   - origin[2]

  denom <- 2 * (x1 * y2 - y1 * x2)
  if (abs(denom) < 1e-9) return((from + to) / 2)   # collinear → chord mid

  r1sq <- x1^2 + y1^2;  r2sq <- x2^2 + y2^2
  cxc  <- (y2 * r1sq - y1 * r2sq) / denom
  cyc  <- (x1 * r2sq - x2 * r1sq) / denom
  R    <- sqrt(cxc^2 + cyc^2)

  # Circle centre in canvas coordinates
  cx <- origin[1] + cxc;  cy <- origin[2] + cyc

  n2pi <- function(a) ((a %% (2 * pi)) + 2 * pi) %% (2 * pi)
  th1  <- n2pi(atan2(from[2]     - cy, from[1]     - cx))
  th2  <- n2pi(atan2(to[2]       - cy, to[1]       - cx))
  th0  <- n2pi(atan2(origin[2]   - cy, origin[1]   - cx))

  # Same arc-selection logic as .circumcircle_arc_svg:
  # take the arc that avoids origin O.
  delta_cw <- n2pi(th2 - th1)
  cw_has_O <- n2pi(th0 - th1) <= delta_cw

  if (cw_has_O) {
    # Chosen arc is CCW (sweep=0); angle goes backwards from th1
    span   <- 2 * pi - delta_cw
    th_mid <- n2pi(th1 - frac * span)
  } else {
    # Chosen arc is CW (sweep=1); angle advances from th1
    span   <- delta_cw
    th_mid <- n2pi(th1 + frac * span)
  }

  c(cx + R * cos(th_mid), cy + R * sin(th_mid))
}
