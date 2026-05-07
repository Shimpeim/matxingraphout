# Internal functions for SVG rendering.
# None of these are exported; users interact only via graph_to_outputs().


# ── Main SVG builder ──────────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.svg_build <- function(adj, np, directed, pad, ecol, ew,
                       adj_ov = NULL, ovcol = "#999999",
                       ovw = 1.0, ovstyle = "dashed",
                       radial_center = NULL,
                       centroids = NULL,
                       edge_curvature = "auto",
                       overlay_edge_curvature = "auto") {
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

  buf <- character(0)
  .emit <- function(...) buf <<- c(buf, paste0(...))

  # ── Header ─────────────────────────────────────────────────────────────────
  .emit('<?xml version="1.0" encoding="UTF-8"?>')
  .emit('<svg xmlns="http://www.w3.org/2000/svg"',
        ' width="',  round(W), '" height="', round(H), '"',
        ' viewBox="0 0 ', round(W), ' ', round(H), '">')

  if (directed) {
    .emit('  <defs>')
    .emit('    <marker id="arrowhead" markerWidth="10" markerHeight="7"',
          ' refX="9" refY="3.5" orient="auto">')
    .emit('      <polygon points="0 0,10 3.5,0 7" fill="', ecol, '"/>')
    .emit('    </marker>')
    if (!is.null(adj_ov))
      .emit('    <marker id="arrowhead-ov" markerWidth="10" markerHeight="7"',
            ' refX="9" refY="3.5" orient="auto">',
            '<polygon points="0 0,10 3.5,0 7" fill="', ovcol, '"/></marker>')
    .emit('  </defs>')
  }

  edge_css <- sprintf('stroke="%s" stroke-width="%g" fill="none"', ecol, ew)
  mar_attr <- if (directed) ' marker-end="url(#arrowhead)"' else ''

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
              mar_attr, ' ', edge_css, '/>')
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

        arc <- if (edge_curvature == "straight") {
          NULL
        } else if (use_centroids) {
          mx    <- (from[1] + to[1]) / 2
          my    <- (from[2] + to[2]) / 2
          dists <- (centroids_sh$x - mx)^2 + (centroids_sh$y - my)^2
          nc    <- centroids_sh[which.min(dists), , drop = FALSE]
          .circumcircle_arc_svg(from[1] - nc$x, from[2] - nc$y,
                                to[1]   - nc$x, to[2]   - nc$y)
        } else if (use_arc) {
          .circumcircle_arc_svg(from[1] - rc_sx, from[2] - rc_sy,
                                to[1]   - rc_sx, to[2]   - rc_sy)
        } else {
          NULL
        }

        if (!is.null(arc)) {
          .emit('  <path d="M ', round(from[1], 1), ',', round(from[2], 1),
                ' A ', arc$R, ',', arc$R, ' 0 ',
                arc$large_arc, ',', arc$sweep,
                ' ', round(to[1], 1), ',', round(to[2], 1), '"',
                mar_attr, ' ', edge_css, '/>')
        } else {
          .emit('  <line x1="', round(from[1], 1), '" y1="', round(from[2], 1),
                '" x2="', round(to[1], 1), '" y2="', round(to[2], 1), '"',
                mar_attr, ' ', edge_css, '/>')
        }

        # Annotate non-binary edge weights
        if (v != 1) {
          mx <- (from[1] + to[1]) / 2
          my <- (from[2] + to[2]) / 2
          .emit('  <text x="', round(mx, 1), '" y="', round(my, 1), '"',
                ' text-anchor="middle" dominant-baseline="auto"',
                ' dy="-4" font-size="10" fill="', ecol, '"',
                ' font-family="Helvetica,Arial,sans-serif">',
                .xml_esc(format(v, trim = TRUE)), '</text>')
        }
      }

      done[i, j] <- TRUE
    }
  }

  # ── Overlay edges (drawn after structural edges, before nodes) ─────────────
  if (!is.null(adj_ov)) {
    ov_dash  <- if (ovstyle == "dashed") ' stroke-dasharray="5,3"' else ''
    ov_css   <- sprintf('stroke="%s" stroke-width="%g" fill="none"%s',
                        ovcol, ovw, ov_dash)
    ov_mar   <- if (directed) ' marker-end="url(#arrowhead-ov)"' else ''
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
          .emit('  <path d="M ', round(ax + rx * .5, 1), ',', round(ay - ry, 1),
                ' C ', round(ox, 1), ',', round(ay - ry, 1),
                ' ',  round(ox, 1), ',', round(oy, 1),
                ' ',  round(ax + rx, 1), ',', round(ay, 1), '"',
                ov_mar, ' ', ov_css, '/>')
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
          arc_ov <- if (overlay_edge_curvature == "straight") {
            NULL
          } else if (use_centroids) {
            mx    <- (from[1] + to[1]) / 2
            my    <- (from[2] + to[2]) / 2
            dists <- (centroids_sh$x - mx)^2 + (centroids_sh$y - my)^2
            nc    <- centroids_sh[which.min(dists), , drop = FALSE]
            .circumcircle_arc_svg(from[1] - nc$x, from[2] - nc$y,
                                  to[1]   - nc$x, to[2]   - nc$y)
          } else if (use_arc_ov) {
            .circumcircle_arc_svg(from[1] - rc_sx, from[2] - rc_sy,
                                  to[1]   - rc_sx, to[2]   - rc_sy)
          } else {
            NULL
          }

          if (!is.null(arc_ov)) {
            .emit('  <path d="M ', round(from[1], 1), ',', round(from[2], 1),
                  ' A ', arc_ov$R, ',', arc_ov$R, ' 0 ',
                  arc_ov$large_arc, ',', arc_ov$sweep,
                  ' ', round(to[1], 1), ',', round(to[2], 1), '"',
                  ov_mar, ' ', ov_css, '/>')
          } else {
            .emit('  <line x1="', round(from[1], 1), '" y1="', round(from[2], 1),
                  '" x2="', round(to[1], 1), '" y2="', round(to[2], 1), '"',
                  ov_mar, ' ', ov_css, '/>')
          }
        }
        done_ov[i, j] <- TRUE
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
#' constraints.  Setting |centre − O|² = |centre − P1|² and
#' |centre − O|² = |centre − P2|² yields a 2×2 linear system whose solution
#' (via Cramer's rule) is:
#'
#'   denom = 2 * (x1*y2 − y1*x2)           # twice the signed area of the triangle
#'   cxc   = (y2*|P1|² − y1*|P2|²) / denom
#'   cyc   = (x1*|P2|² − x2*|P1|²) / denom
#'   R     = |centre − O| = sqrt(cxc² + cyc²)
#'
#' When `denom ≈ 0` the three points are collinear (no finite circle exists);
#' the function returns `NULL` and the caller draws a straight line instead.
#'
#' ## Arc-flag selection
#'
#' The circumscribed circle has two arcs from P1 to P2: one passes through O,
#' the other does not.  We always want the arc that avoids O.
#'
#' 1. Express the angles of P1, P2, and O on the circle as th1, th2, th0.
#' 2. The clockwise arc from P1 to P2 spans `delta_cw = (th2 − th1) mod 2π`
#'    radians.
#' 3. O lies on the CW arc iff `(th0 − th1) mod 2π ≤ delta_cw`.
#' 4. If O is on the CW arc, take the CCW arc (SVG sweep-flag = 0).
#'    Otherwise take the CW arc (SVG sweep-flag = 1).
#' 5. SVG large-arc-flag = 1 when the chosen arc spans more than π radians.
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
