# Internal functions for SVG rendering.
# None of these are exported; users interact only via graph_to_outputs().


# ── Main SVG builder ──────────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.svg_build <- function(adj, np, directed, pad, ecol, ew) {
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

        .emit('  <line x1="', round(from[1], 1), '" y1="', round(from[2], 1),
              '" x2="', round(to[1], 1), '" y2="', round(to[2], 1), '"',
              mar_attr, ' ', edge_css, '/>')

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
