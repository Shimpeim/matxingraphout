library(shiny)
library(matxingraphout)

# ── helpers ───────────────────────────────────────────────────────────────────

default_nodes <- function() {
  data.frame(
    id         = c("A", "B", "C"),
    label      = c("A", "B", "C"),
    shape      = c("diamond", "rect", "rect"),
    colour     = c("#fff8e1", "#e8f0fe", "#e8f0fe"),
    x          = c(150,  50, 250),
    y          = c( 60, 200, 200),
    width      = c(NA_real_,      NA_real_,      NA_real_),
    height     = c(NA_real_,      NA_real_,      NA_real_),
    fontsize   = c(NA_real_,      NA_real_,      NA_real_),
    fontcolour = c(NA_character_, NA_character_, NA_character_),
    stroke     = c(NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
}

default_adj <- function(ids) {
  n <- length(ids)
  m <- matrix(0, n, n, dimnames = list(ids, ids))
  if (n >= 3) { m[1L, 2L] <- 1; m[1L, 3L] <- 1; m[2L, 3L] <- 1 }
  m
}

resize_matrix <- function(old_m, new_ids) {
  n     <- length(new_ids)
  new_m <- matrix(0, n, n, dimnames = list(new_ids, new_ids))
  shared <- intersect(rownames(old_m), new_ids)
  if (length(shared) > 0L)
    new_m[shared, shared] <- old_m[shared, shared]
  new_m
}

# Parse a square adjacency/label matrix CSV.
# Expected: first column = row IDs (no header or blank header),
#           remaining column headers = col IDs,
#           cells = values (numeric for adj, character for labels).
parse_matrix_csv <- function(path, numeric = TRUE) {
  df  <- read.csv(path, header = TRUE, check.names = FALSE,
                  stringsAsFactors = FALSE)
  row_ids <- as.character(df[[1]])
  col_ids <- colnames(df)[-1]
  mat <- as.matrix(df[, -1, drop = FALSE])
  rownames(mat) <- row_ids
  colnames(mat) <- col_ids
  if (numeric) storage.mode(mat) <- "numeric"
  mat
}

# Parse a node properties CSV; returns a data.frame with standard columns.
parse_node_csv <- function(path) {
  df <- read.csv(path, header = TRUE, check.names = FALSE,
                 stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  for (pair in list(c("color", "colour"), c("fontcolor", "fontcolour"))) {
    old <- pair[1]; new <- pair[2]
    if (old %in% names(df) && !new %in% names(df))
      names(df)[names(df) == old] <- new
  }
  for (col in c("id", "label", "shape", "colour", "fontcolour", "stroke"))
    if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
  for (col in c("x", "y", "width", "height", "fontsize"))
    if (col %in% names(df)) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  if (!"label"  %in% names(df) && "id" %in% names(df)) df$label  <- df$id
  if (!"shape"  %in% names(df)) df$shape  <- "rect"
  if (!"colour" %in% names(df)) df$colour <- "#e8f0fe"
  for (col in c("x","y","width","height","fontsize","fontcolour","stroke"))
    if (!col %in% names(df)) df[[col]] <- NA
  df
}

# Parse a settings CSV (2 columns: Setting, Value).
# Returns a named list.
parse_settings_csv <- function(path) {
  df <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
  names(df) <- tolower(trimws(names(df)))
  if (!all(c("setting","value") %in% names(df))) return(list())
  setNames(as.list(trimws(df$value)), trimws(df$setting))
}

# ── CSS ───────────────────────────────────────────────────────────────────────

APP_CSS <- "
body { background:#f0f2f5; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif; }
.app-header { padding:14px 0 10px; border-bottom:1px solid #e2e8f0; margin-bottom:16px; }
.app-title  { font-size:20px; font-weight:700; color:#2d3748; display:inline; }
.app-badge  { font-size:11px; font-weight:600; background:#4299e1; color:white;
              border-radius:10px; padding:2px 8px; margin-left:8px; vertical-align:middle; }
.panel-box  { background:white; border-radius:8px; border:1px solid #e2e8f0;
              padding:16px; margin-bottom:12px; }
.panel-box h5 { font-size:11px; font-weight:700; color:#718096; text-transform:uppercase;
                letter-spacing:.06em; margin:0 0 12px; padding-bottom:7px;
                border-bottom:1px solid #edf2f7; }
.render-btn { width:100%; padding:11px; font-size:14px; font-weight:600;
              background:#4299e1; border-color:#3182ce; color:white;
              border-radius:6px; border:none; cursor:pointer; margin-top:4px; }
.render-btn:hover { background:#3182ce; }
.matrix-wrap { overflow-x:auto; padding-bottom:4px; }
.matrix-table { border-collapse:collapse; font-size:11px; }
.matrix-table .hdr { background:#edf2f7; font-weight:600; padding:4px 6px;
                     text-align:center; min-width:54px; color:#4a5568; }
.matrix-table .row-hdr { background:#edf2f7; font-weight:600; padding:4px 10px 4px 6px;
                          text-align:right; color:#4a5568; white-space:nowrap; }
.matrix-table .cell { padding:2px; }
.matrix-table .cell input[type='number'] {
  width:54px !important; text-align:center !important;
  padding:3px 2px !important; font-size:12px !important;
  border:1px solid #cbd5e0 !important; border-radius:4px !important; }
.matrix-table .cell.diag input[type='number'] {
  background:#f7fafc !important; color:#a0aec0 !important; }
.matrix-table .cell input[type='text'] {
  width:54px !important; text-align:center !important;
  padding:3px 2px !important; font-size:12px !important;
  border:1px solid #cbd5e0 !important; border-radius:4px !important; }
.svg-container { background:#f7fafc; border-radius:6px; border:1px solid #e2e8f0;
                 min-height:280px; overflow:auto; text-align:center; padding:16px; }
.svg-placeholder { color:#a0aec0; margin-top:90px; font-size:14px; }
.code-block { font-size:12px; font-family:'SFMono-Regular',Consolas,monospace;
              max-height:460px; overflow-y:auto; white-space:pre; }
.error-box  { background:#fff5f5; border:1px solid #fc8181; border-radius:6px;
              padding:12px; color:#c53030; font-size:13px; margin-top:8px; }
.dl-row     { display:flex; gap:8px; margin-bottom:10px; }
.topo-table td { font-size:13px; padding:4px 8px; }
.topo-table td:first-child { font-weight:500; color:#4a5568; width:210px; }
.topo-table td:last-child  { font-family:monospace; color:#2d3748; }
label       { font-size:12px !important; font-weight:500 !important; color:#4a5568 !important; }
.form-control { font-size:13px !important; }
.shiny-input-container { margin-bottom:8px; }
.mode-active { background:#c53030 !important; border-color:#9b2c2c !important; color:white !important; }
#coord-display { font-size:11px; color:#718096; font-family:monospace; min-width:220px; padding:2px 0; }
.ruler-x-row { display:flex; flex-direction:row; }
.ruler-corner { width:30px; height:30px; flex-shrink:0; background:#edf2f7;
                border-right:1px solid #cbd5e0; border-bottom:1px solid #cbd5e0; }
#ruler-x { flex:1; display:block; }
.ruler-svg-row { display:flex; flex-direction:row; }
#ruler-y { flex-shrink:0; width:30px; display:block; }
#svg-inner { flex:1; background:#f7fafc; border:1px solid #e2e8f0;
             border-left:none; border-radius:0 6px 6px 6px;
             min-height:280px; overflow:auto; text-align:center; padding:12px; }
"

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(
    tags$style(APP_CSS),
    tags$script(HTML("
(function(){'use strict';

var mode = null;
var xlo = 0, ylo = 0;

Shiny.addCustomMessageHandler('canvas_offset', function(msg) {
  xlo = msg.xlo;
  ylo = msg.ylo;
  updateRulers();
  setTimeout(function(){ reattach(); }, 60);
});

$(document).on('click', '#btn-add-centroid', function() {
  if (mode === 'add') {
    mode = null;
    $(this).removeClass('mode-active');
  } else {
    mode = 'add';
    $(this).addClass('mode-active');
    $('#btn-remove-centroid').removeClass('mode-active');
  }
  updateCursors();
});

$(document).on('click', '#btn-remove-centroid', function() {
  if (mode === 'remove') {
    mode = null;
    $(this).removeClass('mode-active');
  } else {
    mode = 'remove';
    $(this).addClass('mode-active');
    $('#btn-add-centroid').removeClass('mode-active');
  }
  updateCursors();
});

function getSvg() {
  var inner = document.getElementById('svg-inner');
  if (!inner) return null;
  return inner.querySelector('svg');
}

function svgPos(e, svg) {
  var r   = svg.getBoundingClientRect();
  var vb  = svg.viewBox.baseVal;
  var scaleX = vb.width  / r.width;
  var scaleY = vb.height / r.height;
  var cx = (e.clientX - r.left)  * scaleX;
  var cy = (e.clientY - r.top)   * scaleY;
  return { cx: cx, cy: cy };
}

function toOrig(cx, cy) {
  return { x: Math.round(cx + xlo), y: Math.round(cy + ylo) };
}

function reattach() {
  var svg = getSvg();
  if (!svg) return;

  svg.onmousemove = function(e) {
    var pos  = svgPos(e, svg);
    var orig = toOrig(pos.cx, pos.cy);
    var el   = document.getElementById('coord-display');
    if (el) el.textContent = 'x = ' + orig.x + '   y = ' + orig.y;
  };

  svg.onmouseleave = function() {
    var el = document.getElementById('coord-display');
    if (el) el.textContent = 'Hover over the graph for coordinates';
  };

  svg.onclick = function(e) {
    if (mode === 'add') {
      var pos  = svgPos(e, svg);
      var orig = toOrig(pos.cx, pos.cy);
      Shiny.setInputValue('centroid_click', { x: orig.x, y: orig.y }, { priority: 'event' });
      drawTempMarker(svg, pos.cx, pos.cy);
    }
  };

  svg.querySelectorAll('g[data-centroid-idx]').forEach(function(g) {
    g.onclick = function(e) {
      e.stopPropagation(); // always block svg.onclick to prevent adding centroid on top
      if (mode === 'remove') {
        var idx = parseInt(g.getAttribute('data-centroid-idx'), 10);
        Shiny.setInputValue('centroid_remove_idx', idx, { priority: 'event' });
        g.parentNode.removeChild(g);
      }
    };
  });
  updateCursors();
}

function updateCursors() {
  var svg = getSvg();
  if (!svg) return;
  svg.querySelectorAll('g[data-centroid-idx]').forEach(function(g) {
    g.style.cursor = (mode === 'remove') ? 'pointer' : 'default';
  });
}

function drawTempMarker(svg, cx, cy) {
  var ns = 'http://www.w3.org/2000/svg';
  var g  = document.createElementNS(ns, 'g');
  g.setAttribute('class', 'temp-centroid-marker');

  var circle = document.createElementNS(ns, 'circle');
  circle.setAttribute('cx', cx);
  circle.setAttribute('cy', cy);
  circle.setAttribute('r',  '9');
  circle.setAttribute('fill', '#e53e3e');
  circle.setAttribute('fill-opacity', '0.25');
  circle.setAttribute('stroke', '#e53e3e');
  circle.setAttribute('stroke-width', '1.5');
  g.appendChild(circle);

  var lh = document.createElementNS(ns, 'line');
  lh.setAttribute('x1', cx - 12);
  lh.setAttribute('y1', cy);
  lh.setAttribute('x2', cx + 12);
  lh.setAttribute('y2', cy);
  lh.setAttribute('stroke', '#e53e3e');
  lh.setAttribute('stroke-width', '1.5');
  g.appendChild(lh);

  var lv = document.createElementNS(ns, 'line');
  lv.setAttribute('x1', cx);
  lv.setAttribute('y1', cy - 12);
  lv.setAttribute('x2', cx);
  lv.setAttribute('y2', cy + 12);
  lv.setAttribute('stroke', '#e53e3e');
  lv.setAttribute('stroke-width', '1.5');
  g.appendChild(lv);

  svg.appendChild(g);
}

function niceStep(range, maxTicks) {
  var rough = range / maxTicks;
  var mag   = Math.pow(10, Math.floor(Math.log10(rough)));
  var steps = [1, 2, 5, 10];
  for (var i = 0; i < steps.length; i++) {
    if (steps[i] * mag >= rough) return steps[i] * mag;
  }
  return steps[steps.length - 1] * mag;
}

function updateRulers() {
  var svg = getSvg();
  if (!svg) return;
  var r   = svg.getBoundingClientRect();
  var vb  = svg.viewBox.baseVal;
  var dpr = window.devicePixelRatio || 1;

  // X ruler
  var cx = document.getElementById('ruler-x');
  if (cx) {
    cx.style.width  = r.width + 'px';
    cx.style.height = '30px';
    cx.width  = Math.round(r.width  * dpr);
    cx.height = Math.round(30       * dpr);
    var ctx = cx.getContext('2d');
    ctx.scale(dpr, dpr);
    ctx.fillStyle = '#edf2f7';
    ctx.fillRect(0, 0, r.width, 30);
    var scaleX = vb.width / r.width;
    var step   = niceStep(vb.width, Math.max(2, Math.floor(r.width / 60)));
    var uStart = Math.ceil((-xlo) / step) * step;
    ctx.strokeStyle = '#a0aec0';
    ctx.fillStyle   = '#4a5568';
    ctx.font        = '9px sans-serif';
    ctx.textAlign   = 'center';
    ctx.lineWidth   = 1;
    for (var u = uStart; u <= vb.width + (-xlo); u += step) {
      var px = (u - (-xlo)) / scaleX;
      if (px < 0 || px > r.width) continue;
      ctx.beginPath();
      ctx.moveTo(px, 18);
      ctx.lineTo(px, 30);
      ctx.stroke();
      ctx.fillText(Math.round(u + xlo), px, 14);
    }
    ctx.strokeStyle = '#cbd5e0';
    ctx.lineWidth   = 1;
    ctx.beginPath();
    ctx.moveTo(0, 29);
    ctx.lineTo(r.width, 29);
    ctx.stroke();
  }

  // Y ruler
  var cy = document.getElementById('ruler-y');
  if (cy) {
    cy.style.width  = '30px';
    cy.style.height = r.height + 'px';
    cy.width  = Math.round(30       * dpr);
    cy.height = Math.round(r.height * dpr);
    var ctx2 = cy.getContext('2d');
    ctx2.scale(dpr, dpr);
    ctx2.fillStyle = '#edf2f7';
    ctx2.fillRect(0, 0, 30, r.height);
    var scaleY = vb.height / r.height;
    var stepY  = niceStep(vb.height, Math.max(2, Math.floor(r.height / 60)));
    var vStart = Math.ceil((-ylo) / stepY) * stepY;
    ctx2.strokeStyle = '#a0aec0';
    ctx2.fillStyle   = '#4a5568';
    ctx2.font        = '9px sans-serif';
    ctx2.lineWidth   = 1;
    for (var v = vStart; v <= vb.height + (-ylo); v += stepY) {
      var py = (v - (-ylo)) / scaleY;
      if (py < 0 || py > r.height) continue;
      ctx2.beginPath();
      ctx2.moveTo(18, py);
      ctx2.lineTo(30, py);
      ctx2.stroke();
      ctx2.save();
      ctx2.translate(14, py);
      ctx2.rotate(-Math.PI / 2);
      ctx2.textAlign = 'center';
      ctx2.fillText(Math.round(v + ylo), 0, 0);
      ctx2.restore();
    }
    ctx2.strokeStyle = '#cbd5e0';
    ctx2.lineWidth   = 1;
    ctx2.beginPath();
    ctx2.moveTo(29, 0);
    ctx2.lineTo(29, r.height);
    ctx2.stroke();
  }
}

var _observer = new MutationObserver(function(mutations) {
  for (var i = 0; i < mutations.length; i++) {
    var added = mutations[i].addedNodes;
    for (var j = 0; j < added.length; j++) {
      if (added[j].nodeName && added[j].nodeName.toLowerCase() === 'svg') {
        setTimeout(function(){ reattach(); }, 60);
        return;
      }
    }
  }
});

document.addEventListener('DOMContentLoaded', function() {
  var inner = document.getElementById('svg-inner');
  if (inner) {
    _observer.observe(inner, { childList: true, subtree: true });
  }
});

})();
    "))
  ),

  tags$div(class = "app-header",
    tags$span(class = "app-title", "matxingraphout"),
    tags$span(class = "app-badge", "Interactive Editor")
  ),

  fluidRow(

    # ── LEFT : input panels ─────────────────────────────────────────────────
    column(5,

      # Nodes ---
      tags$div(class = "panel-box",
        tags$h5("Nodes"),
        tags$p(tags$small(style = "color:#718096",
          "Use ", tags$code("\\n"), " in the Label cell for multi-line node labels.")),
        DT::DTOutput("node_table"),
        tags$br(),
        fluidRow(
          column(6, actionButton("add_node", "\u002b Add node",
                                 class = "btn-sm btn-default", width = "100%")),
          column(6, actionButton("del_node", "\u2212 Delete selected",
                                 class = "btn-sm btn-danger",  width = "100%"))
        )
      ),

      # Structural matrix ---
      tags$div(class = "panel-box",
        tags$h5("Adjacency matrix — structural edges"),
        tags$p(tags$small(style = "color:#718096",
          "Row \u2192 Col = edge weight (0\u202f=\u202fno edge, 1\u202f=\u202funweighted, >1\u202f=\u202fweighted).")),
        tags$div(class = "matrix-wrap", uiOutput("adj_matrix_ui"))
      ),

      # Overlay matrix ---
      tags$div(class = "panel-box",
        tags$h5("Overlay matrix — annotation edges"),
        checkboxInput("use_overlay", "Enable overlay edges", value = FALSE),
        conditionalPanel("input.use_overlay",
          tags$p(tags$small(style = "color:#718096",
            "Drawn on top of structural edges; excluded from topology analysis.")),
          tags$div(class = "matrix-wrap", uiOutput("ovl_matrix_ui"))
        )
      ),

      # Edge Properties ---
      tags$div(class = "panel-box",
        tags$h5("Structural edge properties"),
        tags$p(tags$small(style = "color:#718096",
          "Map edge weight values to colour, width, linetype, and legend label.",
          " Weights not listed use the global edge settings.")),
        DT::DTOutput("edge_props_table"),
        tags$br(),
        fluidRow(
          column(6, actionButton("add_ep",  "\u002b Add row",
                                 class = "btn-sm btn-default", width = "100%")),
          column(6, actionButton("del_ep",  "\u2212 Delete selected",
                                 class = "btn-sm btn-danger",  width = "100%"))
        )
      ),

      conditionalPanel("input.use_overlay",
        tags$div(class = "panel-box",
          tags$h5("Overlay edge properties"),
          tags$p(tags$small(style = "color:#718096",
            "Same as above but for overlay edges.")),
          DT::DTOutput("overlay_ep_table"),
          tags$br(),
          fluidRow(
            column(6, actionButton("add_ovep", "\u002b Add row",
                                   class = "btn-sm btn-default", width = "100%")),
            column(6, actionButton("del_ovep", "\u2212 Delete selected",
                                   class = "btn-sm btn-danger",  width = "100%"))
          )
        )
      ),

      # Centroids ---
      tags$div(class = "panel-box",
        tags$h5("Centroids \u2014 arc curvature origins"),
        tags$p(tags$small(style = "color:#718096",
          "Each edge curves toward its nearest centroid (edge midpoint distance).",
          " When the table is empty, the eigenvector-centrality hub node is used.")),
        DT::DTOutput("centroid_table"),
        tags$br(),
        fluidRow(
          column(6, actionButton("add_centroid", "\u002b Add centroid",
                                 class = "btn-sm btn-default", width = "100%")),
          column(6, actionButton("del_centroid", "\u2212 Delete selected",
                                 class = "btn-sm btn-danger",  width = "100%"))
        )
      ),

      # Edge Labels ---
      tags$div(class = "panel-box",
        tags$h5("Edge labels"),
        tags$p(tags$small(style = "color:#718096",
          "Optional text labels on edges. Empty cell = use weight value (if \u2260 1) or no label.",
          " In the Nodes table, use ", tags$code("\\n"), " in Label for multi-line node labels.")),
        tags$div(class = "matrix-wrap", uiOutput("edge_label_matrix_ui"))
      ),

      # Settings ---
      tags$div(class = "panel-box",
        tags$h5("Settings"),

        fluidRow(
          column(6, checkboxInput("directed", "Directed graph", value = TRUE)),
          column(6, selectInput("layout", "Layout", width = "100%",
            choices  = c("auto", "manual", "tree", "sunburst", "bipartite", "circular"),
            selected = "auto"))
        ),

        conditionalPanel("input.layout === 'manual'",
          tags$p(style = "color:#e53e3e; font-size:12px; margin-bottom:6px",
            "\u26a0 Manual layout requires x and y columns in the Nodes table.")
        ),

        conditionalPanel("input.layout === 'auto' || input.layout === 'sunburst'",
          fluidRow(
            column(6, numericInput("sunburst_max_depth",     "Sunburst max depth",    value = 3, min = 1, step = 1)),
            column(6, numericInput("sunburst_min_branching", "Sunburst min branching",value = 3, min = 0.5, step = 0.5))
          )
        ),

        conditionalPanel("input.layout === 'auto' || input.layout === 'circular'",
          fluidRow(
            column(4, numericInput("circle_r",  "Circle radius (0\u202f=\u202fauto)", value = 0, min = 0, step = 20)),
            column(4, numericInput("circle_cx", "Centre X (0\u202f=\u202fauto)",       value = 0, min = 0, step = 20)),
            column(4, numericInput("circle_cy", "Centre Y (0\u202f=\u202fauto)",       value = 0, min = 0, step = 20))
          )
        ),

        tags$hr(style = "border-color:#edf2f7; margin:10px 0"),
        tags$small(tags$b("Structural edges")),
        fluidRow(
          column(4, textInput("edge_colour",    "Colour",    value = "#444444")),
          column(4, numericInput("edge_width",  "Width (px)", value = 1.5, min = 0.5, step = 0.5)),
          column(4, selectInput("edge_curvature", "Curvature",
                                choices = c("auto", "straight"), selected = "auto"))
        ),

        conditionalPanel("input.use_overlay",
          tags$small(tags$b("Overlay edges")),
          fluidRow(
            column(3, textInput("ovl_colour",  "Colour",    value = "#999999")),
            column(3, numericInput("ovl_width","Width (px)", value = 1.0, min = 0.5, step = 0.5)),
            column(3, selectInput("ovl_style", "Style",
                                  choices = c("dashed", "solid"), selected = "dashed")),
            column(3, selectInput("ovl_curvature", "Curvature",
                                  choices = c("auto", "straight"), selected = "auto"))
          )
        ),

        tags$hr(style = "border-color:#edf2f7; margin:10px 0"),
        tags$small(tags$b("Node defaults")),
        fluidRow(
          column(3, numericInput("default_width",     "Width (px)",   value = 100, min = 20, step = 5)),
          column(3, numericInput("default_height",    "Height (px)",  value = 44,  min = 10, step = 2)),
          column(3, numericInput("default_fontsize",  "Font size",    value = 12,  min = 6,  step = 1)),
          column(3, numericInput("svg_padding",       "SVG padding",  value = 40,  min = 0,  step = 5))
        ),
        fluidRow(
          column(6, textInput("default_fontcolour", "Font colour",   value = "#222222")),
          column(6, textInput("default_stroke",     "Border colour", value = "#333333"))
        ),
        tags$hr(style = "border-color:#edf2f7; margin:10px 0"),
        tags$small(tags$b("Legend")),
        fluidRow(
          column(6, checkboxInput("show_legend", "Show legend", value = FALSE)),
          column(6, textInput("legend_title", "Legend title", value = "Legend"))
        ),
        conditionalPanel("input.show_legend",
          tags$p(tags$small(style = "color:#718096",
            "Node shape legend (shape \u2192 label):")),
          DT::DTOutput("legend_shapes_table"),
          tags$br(),
          fluidRow(
            column(5, actionButton("add_ls",  "\u002b Add",    class = "btn-sm btn-default", width="100%")),
            column(5, actionButton("del_ls",  "\u2212 Remove", class = "btn-sm btn-danger",  width="100%")),
            column(2, actionButton("auto_ls", "\u21ba",        class = "btn-sm btn-default", width="100%",
                                   title = "Auto-populate from node shapes"))
          ),
          tags$br(),
          tags$p(tags$small(style = "color:#718096",
            "Node colour legend (hex \u2192 label):")),
          DT::DTOutput("legend_colours_table"),
          tags$br(),
          fluidRow(
            column(5, actionButton("add_lc",  "\u002b Add",    class = "btn-sm btn-default", width="100%")),
            column(5, actionButton("del_lc",  "\u2212 Remove", class = "btn-sm btn-danger",  width="100%")),
            column(2, actionButton("auto_lc", "\u21ba",        class = "btn-sm btn-default", width="100%",
                                   title = "Auto-populate from node colours"))
          )
        )
      ),

      # Render button ---
      tags$button(id = "render_btn", class = "btn render-btn",
        onclick = "Shiny.setInputValue('render_btn', Math.random())",
        "\u25b6\u2002Render Graph"),

      # Import CSV ---
      tags$div(class = "panel-box", style = "margin-top:12px",
        tags$h5("Import CSV"),
        tags$p(tags$small(style = "color:#718096",
          "CSV formats: ",
          tags$b("Nodes"), " — columns: id, label, shape, colour, x, y, [width, height, fontsize, fontcolour, stroke]. ",
          tags$b("Matrix"), " — first column = from-node IDs, remaining column headers = to-node IDs; numeric values. ",
          tags$b("Edge labels"), " — same format as matrix but cells are label strings. ",
          tags$b("Settings"), " — two columns: Setting, Value."
        )),
        tags$p(tags$small(tags$b("Batch upload:"),
          " select all CSVs for one story at once.",
          " Files are routed by suffix: ",
          tags$code("_adj"), ", ",
          tags$code("_node_props"), ", ",
          tags$code("_edge_labels"), ", ",
          tags$code("_edge_props"), ", ",
          tags$code("_overlay"), ", ",
          tags$code("_settings"), ", ",
          tags$code("_legend_shapes"), ", ",
          tags$code("_legend_colours"), "."
        )),
        fileInput("csv_batch", NULL, multiple = TRUE, accept = ".csv",
                  buttonLabel = "Batch upload\u2026",
                  placeholder  = "e.g. mononoke_adj.csv + mononoke_node_props.csv + \u2026"),
        tags$hr(style = "border-color:#edf2f7; margin:6px 0 10px"),
        fileInput("csv_nodes",   "Node properties (.csv)", accept = ".csv",
                  placeholder = "node_props.csv"),
        fileInput("csv_adj",     "Adjacency matrix (.csv)", accept = ".csv",
                  placeholder = "adj.csv"),
        fileInput("csv_edgelbl", "Edge labels (.csv)", accept = ".csv",
                  placeholder = "edge_labels.csv"),
        fileInput("csv_overlay", "Overlay matrix (.csv)", accept = ".csv",
                  placeholder = "overlay.csv"),
        fileInput("csv_settings","Settings (.csv)", accept = ".csv",
                  placeholder = "settings.csv"),
        fileInput("csv_edge_props",    "Edge properties (.csv)", accept = ".csv",
                  placeholder = "edge_props.csv"),
        fileInput("csv_overlay_props", "Overlay edge properties (.csv)", accept = ".csv",
                  placeholder = "overlay_edge_props.csv"),
        fileInput("csv_legend_shapes",  "Legend shapes (.csv)", accept = ".csv",
                  placeholder = "legend_shapes.csv"),
        fileInput("csv_legend_colours", "Legend colours (.csv)", accept = ".csv",
                  placeholder = "legend_colours.csv")
      )
    ),

    # ── RIGHT : output panels ───────────────────────────────────────────────
    column(7,
      tags$div(class = "panel-box",
        tabsetPanel(id = "out_tabs",

          tabPanel("SVG",
            br(),
            tags$div(class = "dl-row", style = "align-items:center; flex-wrap:wrap; gap:6px;",
              downloadButton("dl_svg", "Download SVG", class = "btn-sm btn-default"),
              downloadButton("dl_svg_clean", "Download SVG (no centroids)", class = "btn-sm btn-default"),
              tags$button(id = "btn-add-centroid",    class = "btn btn-sm btn-default",
                          title = "Click on the graph to place a centroid",
                          HTML("&#10010;&nbsp;Place centroid")),
              tags$button(id = "btn-remove-centroid", class = "btn btn-sm btn-default",
                          title = "Click a centroid marker to remove it",
                          HTML("&times;&nbsp;Remove centroid")),
              tags$span(id = "coord-display", "Hover over the graph for coordinates")
            ),
            tags$div(class = "ruler-x-row",
              tags$div(class = "ruler-corner"),
              tags$canvas(id = "ruler-x")
            ),
            tags$div(class = "ruler-svg-row",
              tags$canvas(id = "ruler-y"),
              tags$div(id = "svg-inner", uiOutput("svg_out"))
            )
          ),

          tabPanel("DOT",
            br(),
            tags$div(class = "dl-row",
              downloadButton("dl_dot", "Download .dot", class = "btn-sm btn-default")
            ),
            verbatimTextOutput("dot_out")
          ),

          tabPanel("Mermaid",
            br(),
            tags$div(class = "dl-row",
              downloadButton("dl_mmd", "Download .mmd", class = "btn-sm btn-default")
            ),
            verbatimTextOutput("mermaid_out")
          ),

          tabPanel("Topology",
            br(),
            tableOutput("topo_out")
          ),

          tabPanel("R Code",
            br(),
            verbatimTextOutput("rcode_out")
          )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  rv <- reactiveValues(
    nodes            = default_nodes(),
    adj              = default_adj(c("A", "B", "C")),
    overlay          = matrix(0, 3L, 3L,
                              dimnames = list(c("A","B","C"), c("A","B","C"))),
    edge_labels      = NULL,
    edge_labels_arg  = NULL,
    centroids        = data.frame(label = character(0), x = numeric(0), y = numeric(0),
                                  stringsAsFactors = FALSE),
    edge_props         = data.frame(weight=numeric(0), colour=character(0),
                                     width=numeric(0), linetype=character(0),
                                     label=character(0), stringsAsFactors=FALSE),
    overlay_edge_props = data.frame(weight=numeric(0), colour=character(0),
                                     width=numeric(0), linetype=character(0),
                                     label=character(0), stringsAsFactors=FALSE),
    legend_shapes      = data.frame(shape=character(0), label=character(0),
                                     stringsAsFactors=FALSE),
    legend_colours     = data.frame(colour=character(0), label=character(0),
                                     stringsAsFactors=FALSE),
    import_ver       = 0L,
    result           = NULL,
    error            = NULL
  )

  # ── Node table ────────────────────────────────────────────────────────────

  output$node_table <- DT::renderDT({
    DT::datatable(
      rv$nodes,
      rownames  = FALSE,
      editable  = list(target = "cell"),
      selection = "multiple",
      options   = list(
        pageLength = 25,
        dom        = "t",
        scrollX    = TRUE,
        columnDefs = list(
          list(width = "56px",  targets = c(4L, 5L, 6L, 7L, 8L)),  # x y w h fontsize
          list(width = "80px",  targets = c(2L, 3L)),               # shape, colour
          list(width = "70px",  targets = c(9L, 10L))               # fontcolour, stroke
        )
      )
    )
  }, server = FALSE)

  # Sync DT cell edits back to rv$nodes
  observeEvent(input$node_table_cell_edit, {
    info <- input$node_table_cell_edit
    col  <- info$col + 1L       # DT: 0-indexed → R: 1-indexed
    row  <- info$row
    rv$nodes[row, col] <- tryCatch(
      type.convert(as.character(info$value), as.is = TRUE),
      error = function(e) info$value
    )
    sync_ids()
  })

  sync_ids <- function() {
    ids        <- rv$nodes$id
    rv$adj     <- resize_matrix(rv$adj,     ids)
    rv$overlay <- resize_matrix(rv$overlay, ids)
  }

  # Add node
  observeEvent(input$add_node, {
    existing <- rv$nodes$id
    i        <- length(existing) + 1L
    new_id   <- paste0("N", i)
    while (new_id %in% existing) new_id <- paste0("N", i <- i + 1L)

    rv$nodes <- rbind(rv$nodes, data.frame(
      id = new_id, label = new_id, shape = "rect", colour = "#e8f0fe",
      x = NA_real_, y = NA_real_, width = NA_real_, height = NA_real_,
      fontsize = NA_real_, fontcolour = NA_character_, stroke = NA_character_,
      stringsAsFactors = FALSE
    ))
    sync_ids()
  })

  # Delete selected nodes
  observeEvent(input$del_node, {
    sel <- input$node_table_rows_selected
    if (length(sel) > 0L) {
      rv$nodes <- rv$nodes[-sel, , drop = FALSE]
      sync_ids()
    }
  })

  # ── Matrix grid renderer ──────────────────────────────────────────────────

  make_matrix_ui <- function(ids, prefix) {
    n <- length(ids)
    if (n == 0L)
      return(tags$p(tags$small(style = "color:#718096", "No nodes defined.")))
    if (n > 15L)
      return(tags$p(tags$small(style = "color:#e53e3e",
        "Grid display limited to 15 nodes. Larger matrices can be set via R code.")))

    m <- if (prefix == "adj") isolate(rv$adj) else isolate(rv$overlay)

    header <- tags$tr(
      tags$th(""),
      lapply(ids, function(id) tags$th(class = "hdr", id))
    )

    body_rows <- lapply(seq_len(n), function(i) {
      tags$tr(
        tags$td(class = "row-hdr", ids[i]),
        lapply(seq_len(n), function(j) {
          val <- if (!is.null(m) && nrow(m) >= i && ncol(m) >= j) m[i, j] else 0
          tags$td(
            class = paste0("cell", if (i == j) " diag"),
            numericInput(paste0(prefix, "_", i, "_", j),
                         label = NULL, value = val, min = 0, step = 1,
                         width = "54px")
          )
        })
      )
    })

    tags$table(class = "matrix-table",
      tags$thead(header),
      tags$tbody(body_rows)
    )
  }

  output$adj_matrix_ui <- renderUI({ rv$import_ver; make_matrix_ui(rv$nodes$id, "adj") })
  output$ovl_matrix_ui <- renderUI({ rv$import_ver; make_matrix_ui(rv$nodes$id, "ovl") })

  make_label_matrix_ui <- function(ids, prefix) {
    n <- length(ids)
    if (n == 0L)
      return(tags$p(tags$small(style = "color:#718096", "No nodes defined.")))
    if (n > 15L)
      return(tags$p(tags$small(style = "color:#e53e3e",
        "Grid display limited to 15 nodes. Use CSV import for larger matrices.")))

    m <- isolate(rv$edge_labels)

    header <- tags$tr(
      tags$th(""),
      lapply(ids, function(id) tags$th(class = "hdr", id))
    )
    body_rows <- lapply(seq_len(n), function(i) {
      tags$tr(
        tags$td(class = "row-hdr", ids[i]),
        lapply(seq_len(n), function(j) {
          val <- if (!is.null(m) && nrow(m) >= i && ncol(m) >= j &&
                     !is.na(m[i, j])) m[i, j] else ""
          tags$td(
            class = paste0("cell", if (i == j) " diag"),
            textInput(paste0(prefix, "_", i, "_", j),
                      label = NULL, value = val,
                      width = "54px",
                      placeholder = "")
          )
        })
      )
    })
    tags$table(class = "matrix-table",
      tags$thead(header),
      tags$tbody(body_rows)
    )
  }

  # ── Centroid table ────────────────────────────────────────────────────────

  output$centroid_table <- DT::renderDT({
    DT::datatable(
      rv$centroids,
      rownames  = FALSE,
      editable  = list(target = "cell"),
      selection = "multiple",
      options   = list(
        pageLength = 10,
        dom        = "t",
        scrollX    = TRUE,
        columnDefs = list(
          list(width = "80px",  targets = c(1L, 2L)),   # x, y
          list(width = "100px", targets = 0L)            # label
        )
      )
    )
  }, server = FALSE)

  observeEvent(input$centroid_table_cell_edit, {
    info <- input$centroid_table_cell_edit
    rv$centroids[info$row, info$col + 1L] <- tryCatch(
      type.convert(as.character(info$value), as.is = TRUE),
      error = function(e) info$value
    )
  })

  observeEvent(input$add_centroid, {
    i <- nrow(rv$centroids) + 1L
    rv$centroids <- rbind(rv$centroids, data.frame(
      label = paste0("C", i),
      x     = 200,
      y     = 200,
      stringsAsFactors = FALSE
    ))
  })

  observeEvent(input$del_centroid, {
    sel <- input$centroid_table_rows_selected
    if (length(sel) > 0L)
      rv$centroids <- rv$centroids[-sel, , drop = FALSE]
  })

  output$edge_label_matrix_ui <- renderUI({
    rv$import_ver
    make_label_matrix_ui(rv$nodes$id, "elbl")
  })

  # ── Edge props tables ─────────────────────────────────────────────────────

  output$edge_props_table <- DT::renderDT({
    DT::datatable(rv$edge_props, rownames = FALSE,
                  editable = list(target = "cell"), selection = "multiple",
                  options = list(pageLength = 10, dom = "t", scrollX = TRUE,
                    columnDefs = list(list(width = "60px", targets = c(0L,2L)),
                                      list(width = "80px", targets = c(1L,3L,4L)))))
  }, server = FALSE)

  observeEvent(input$edge_props_table_cell_edit, {
    info <- input$edge_props_table_cell_edit
    rv$edge_props[info$row, info$col + 1L] <- tryCatch(
      type.convert(as.character(info$value), as.is = TRUE), error = function(e) info$value)
  })
  observeEvent(input$add_ep, {
    rv$edge_props <- rbind(rv$edge_props, data.frame(
      weight=1, colour=NA_character_, width=NA_real_,
      linetype="solid", label=NA_character_, stringsAsFactors=FALSE))
  })
  observeEvent(input$del_ep, {
    sel <- input$edge_props_table_rows_selected
    if (length(sel)) rv$edge_props <- rv$edge_props[-sel, , drop=FALSE]
  })

  output$overlay_ep_table <- DT::renderDT({
    DT::datatable(rv$overlay_edge_props, rownames = FALSE,
                  editable = list(target = "cell"), selection = "multiple",
                  options = list(pageLength = 10, dom = "t", scrollX = TRUE,
                    columnDefs = list(list(width = "60px", targets = c(0L,2L)),
                                      list(width = "80px", targets = c(1L,3L,4L)))))
  }, server = FALSE)

  observeEvent(input$overlay_ep_table_cell_edit, {
    info <- input$overlay_ep_table_cell_edit
    rv$overlay_edge_props[info$row, info$col + 1L] <- tryCatch(
      type.convert(as.character(info$value), as.is = TRUE), error = function(e) info$value)
  })
  observeEvent(input$add_ovep, {
    rv$overlay_edge_props <- rbind(rv$overlay_edge_props, data.frame(
      weight=1, colour=NA_character_, width=NA_real_,
      linetype="solid", label=NA_character_, stringsAsFactors=FALSE))
  })
  observeEvent(input$del_ovep, {
    sel <- input$overlay_ep_table_rows_selected
    if (length(sel)) rv$overlay_edge_props <- rv$overlay_edge_props[-sel, , drop=FALSE]
  })

  output$legend_shapes_table <- DT::renderDT({
    DT::datatable(rv$legend_shapes, rownames = FALSE,
                  editable = list(target = "cell"), selection = "multiple",
                  options = list(pageLength = 10, dom = "t", scrollX = TRUE))
  }, server = FALSE)

  observeEvent(input$legend_shapes_table_cell_edit, {
    info <- input$legend_shapes_table_cell_edit
    rv$legend_shapes[info$row, info$col + 1L] <- as.character(info$value)
  })
  observeEvent(input$add_ls, {
    rv$legend_shapes <- rbind(rv$legend_shapes,
      data.frame(shape="rect", label="", stringsAsFactors=FALSE))
  })
  observeEvent(input$del_ls, {
    sel <- input$legend_shapes_table_rows_selected
    if (length(sel)) rv$legend_shapes <- rv$legend_shapes[-sel, , drop=FALSE]
  })
  observeEvent(input$auto_ls, {
    shapes <- unique(rv$nodes$shape)
    rv$legend_shapes <- data.frame(shape=shapes,
      label=vapply(shapes, function(s) paste0(toupper(substr(s,1,1)), substr(s,2,nchar(s))),
                   character(1L)),
      stringsAsFactors=FALSE)
  })

  output$legend_colours_table <- DT::renderDT({
    DT::datatable(rv$legend_colours, rownames = FALSE,
                  editable = list(target = "cell"), selection = "multiple",
                  options = list(pageLength = 10, dom = "t", scrollX = TRUE))
  }, server = FALSE)

  observeEvent(input$legend_colours_table_cell_edit, {
    info <- input$legend_colours_table_cell_edit
    rv$legend_colours[info$row, info$col + 1L] <- as.character(info$value)
  })
  observeEvent(input$add_lc, {
    rv$legend_colours <- rbind(rv$legend_colours,
      data.frame(colour="#e8f0fe", label="", stringsAsFactors=FALSE))
  })
  observeEvent(input$del_lc, {
    sel <- input$legend_colours_table_rows_selected
    if (length(sel)) rv$legend_colours <- rv$legend_colours[-sel, , drop=FALSE]
  })
  observeEvent(input$auto_lc, {
    colours <- unique(rv$nodes$colour)
    colours <- colours[!is.na(colours)]
    rv$legend_colours <- data.frame(colour=colours, label="",
                                     stringsAsFactors=FALSE)
  })

  # ── CSV import ────────────────────────────────────────────────────────────

  # ── Batch upload: route files by filename suffix ─────────────────────────
  observeEvent(input$csv_batch, {
    req(input$csv_batch)
    files   <- input$csv_batch   # data.frame: name, size, type, datapath
    has_sfx <- function(nm, sfx) endsWith(tolower(trimws(nm)), tolower(sfx))

    # Helper: apply a parsed settings list to UI inputs
    apply_settings <- function(s) {
      if (!is.null(s$directed))
        updateCheckboxInput(session, "directed",
          value = tolower(trimws(s$directed)) %in% c("true","1","yes"))
      if (!is.null(s$layout))
        updateSelectInput(session, "layout", selected = s$layout)
      if (!is.null(s$edge_colour))
        updateTextInput(session, "edge_colour", value = s$edge_colour)
      if (!is.null(s$edge_width))
        updateNumericInput(session, "edge_width", value = as.numeric(s$edge_width))
      if (!is.null(s$edge_curvature))
        updateSelectInput(session, "edge_curvature", selected = s$edge_curvature)
      if (!is.null(s$default_width))
        updateNumericInput(session, "default_width",    value = as.numeric(s$default_width))
      if (!is.null(s$default_height))
        updateNumericInput(session, "default_height",   value = as.numeric(s$default_height))
      if (!is.null(s$default_fontsize))
        updateNumericInput(session, "default_fontsize", value = as.numeric(s$default_fontsize))
      if (!is.null(s$default_fontcolour))
        updateTextInput(session, "default_fontcolour",  value = s$default_fontcolour)
      if (!is.null(s$default_stroke))
        updateTextInput(session, "default_stroke",      value = s$default_stroke)
      if (!is.null(s$svg_padding))
        updateNumericInput(session, "svg_padding",      value = as.numeric(s$svg_padding))
      if (!is.null(s$show_legend))
        updateCheckboxInput(session, "show_legend",
          value = tolower(trimws(s$show_legend)) %in% c("true","1","yes"))
      if (!is.null(s$legend_title))
        updateTextInput(session, "legend_title", value = s$legend_title)
    }

    # Pass 1 — settings (must come first so layout/style UI is ready)
    for (i in seq_len(nrow(files))) {
      if (has_sfx(files$name[i], "_settings.csv")) {
        s <- tryCatch(parse_settings_csv(files$datapath[i]), error = function(e) NULL)
        if (!is.null(s)) apply_settings(s)
      }
    }

    # Pass 2 — node properties
    for (i in seq_len(nrow(files))) {
      if (has_sfx(files$name[i], "_node_props.csv")) {
        df <- tryCatch(parse_node_csv(files$datapath[i]), error = function(e) NULL)
        if (!is.null(df) && "id" %in% names(df)) {
          rv$nodes <- df
          sync_ids()
          rv$import_ver <- rv$import_ver + 1L
        }
      }
    }

    # Pass 3 — adjacency matrix (may also seed node table if no node_props present)
    for (i in seq_len(nrow(files))) {
      if (has_sfx(files$name[i], "_adj.csv")) {
        mat <- tryCatch(parse_matrix_csv(files$datapath[i], numeric = TRUE),
                        error = function(e) NULL)
        if (!is.null(mat)) {
          rv$adj  <- mat
          new_ids <- rownames(mat)
          if (!identical(new_ids, rv$nodes$id)) {
            kept     <- rv$nodes[rv$nodes$id %in% new_ids, , drop = FALSE]
            new_rows <- setdiff(new_ids, kept$id)
            if (length(new_rows)) {
              extra <- data.frame(
                id = new_rows, label = new_rows, shape = "rect", colour = "#e8f0fe",
                x = NA_real_, y = NA_real_, width = NA_real_, height = NA_real_,
                fontsize = NA_real_, fontcolour = NA_character_, stroke = NA_character_,
                stringsAsFactors = FALSE)
              kept <- rbind(kept, extra)
            }
            kept     <- kept[match(new_ids, kept$id), , drop = FALSE]
            rv$nodes <- kept
          }
          sync_ids()
          rv$import_ver <- rv$import_ver + 1L
        }
      }
    }

    # Pass 4 — edge labels
    for (i in seq_len(nrow(files))) {
      if (has_sfx(files$name[i], "_edge_labels.csv")) {
        mat <- tryCatch(parse_matrix_csv(files$datapath[i], numeric = FALSE),
                        error = function(e) NULL)
        if (!is.null(mat)) {
          rv$edge_labels <- mat
          rv$import_ver  <- rv$import_ver + 1L
        }
      }
    }

    # Pass 5 — structural edge properties
    for (i in seq_len(nrow(files))) {
      nm <- files$name[i]
      if (has_sfx(nm, "_edge_props.csv") || tolower(trimws(nm)) == "edge_props.csv") {
        df <- tryCatch(read.csv(files$datapath[i], stringsAsFactors = FALSE),
                       error = function(e) NULL)
        if (!is.null(df) && "weight" %in% tolower(names(df))) {
          names(df) <- tolower(names(df))
          rv$edge_props <- df
        }
      }
    }

    # Pass 6 — overlay matrix
    for (i in seq_len(nrow(files))) {
      if (has_sfx(files$name[i], "_overlay.csv")) {
        mat <- tryCatch(parse_matrix_csv(files$datapath[i], numeric = TRUE),
                        error = function(e) NULL)
        if (!is.null(mat)) {
          rv$overlay <- mat
          updateCheckboxInput(session, "use_overlay", value = TRUE)
          rv$import_ver <- rv$import_ver + 1L
        }
      }
    }

    # Pass 7 — overlay edge properties
    for (i in seq_len(nrow(files))) {
      if (has_sfx(files$name[i], "_overlay_edge_props.csv")) {
        df <- tryCatch(read.csv(files$datapath[i], stringsAsFactors = FALSE),
                       error = function(e) NULL)
        if (!is.null(df) && "weight" %in% tolower(names(df))) {
          names(df) <- tolower(names(df))
          rv$overlay_edge_props <- df
        }
      }
    }

    # Pass 8 — legend shapes
    for (i in seq_len(nrow(files))) {
      if (has_sfx(files$name[i], "_legend_shapes.csv")) {
        df <- tryCatch({
          d <- read.csv(files$datapath[i], header = TRUE, stringsAsFactors = FALSE)
          names(d) <- tolower(trimws(names(d)))
          d
        }, error = function(e) NULL)
        if (!is.null(df) && all(c("shape","label") %in% names(df)))
          rv$legend_shapes <- df[, c("shape","label"), drop = FALSE]
      }
    }

    # Pass 9 — legend colours
    for (i in seq_len(nrow(files))) {
      nm <- files$name[i]
      if (has_sfx(nm, "_legend_colours.csv") || has_sfx(nm, "_legend_colors.csv")) {
        df <- tryCatch({
          d <- read.csv(files$datapath[i], header = TRUE, stringsAsFactors = FALSE)
          names(d) <- tolower(trimws(names(d)))
          if ("color" %in% names(d) && !"colour" %in% names(d))
            names(d)[names(d) == "color"] <- "colour"
          d
        }, error = function(e) NULL)
        if (!is.null(df) && all(c("colour","label") %in% names(df)))
          rv$legend_colours <- df[, c("colour","label"), drop = FALSE]
      }
    }

    rv$error <- NULL
  })

  observeEvent(input$csv_nodes, {
    req(input$csv_nodes)
    df <- tryCatch(parse_node_csv(input$csv_nodes$datapath),
                   error = function(e) { rv$error <- paste("Node CSV:", e$message); NULL })
    if (is.null(df)) return()
    req("id" %in% names(df))
    rv$nodes <- df
    sync_ids()
    rv$import_ver <- rv$import_ver + 1L
    rv$error <- NULL
  })

  observeEvent(input$csv_adj, {
    req(input$csv_adj)
    mat <- tryCatch(parse_matrix_csv(input$csv_adj$datapath, numeric = TRUE),
                    error = function(e) { rv$error <- paste("Adj CSV:", e$message); NULL })
    if (is.null(mat)) return()
    rv$adj   <- mat
    # Sync node table to matrix IDs
    new_ids  <- rownames(mat)
    if (!identical(new_ids, rv$nodes$id)) {
      kept <- rv$nodes[rv$nodes$id %in% new_ids, , drop = FALSE]
      new_rows <- setdiff(new_ids, kept$id)
      if (length(new_rows)) {
        extra <- data.frame(
          id = new_rows, label = new_rows, shape = "rect", colour = "#e8f0fe",
          x = NA_real_, y = NA_real_, width = NA_real_, height = NA_real_,
          fontsize = NA_real_, fontcolour = NA_character_, stroke = NA_character_,
          stringsAsFactors = FALSE)
        kept <- rbind(kept, extra)
      }
      kept <- kept[match(new_ids, kept$id), , drop = FALSE]
      rv$nodes <- kept
    }
    sync_ids()
    rv$import_ver <- rv$import_ver + 1L
    rv$error <- NULL
  })

  observeEvent(input$csv_edgelbl, {
    req(input$csv_edgelbl)
    mat <- tryCatch(parse_matrix_csv(input$csv_edgelbl$datapath, numeric = FALSE),
                    error = function(e) { rv$error <- paste("Edge label CSV:", e$message); NULL })
    if (is.null(mat)) return()
    rv$edge_labels <- mat
    rv$import_ver <- rv$import_ver + 1L
    rv$error <- NULL
  })

  observeEvent(input$csv_overlay, {
    req(input$csv_overlay)
    mat <- tryCatch(parse_matrix_csv(input$csv_overlay$datapath, numeric = TRUE),
                    error = function(e) { rv$error <- paste("Overlay CSV:", e$message); NULL })
    if (is.null(mat)) return()
    rv$overlay <- mat
    rv$import_ver <- rv$import_ver + 1L
    rv$error   <- NULL
  })

  observeEvent(input$csv_edge_props, {
    req(input$csv_edge_props)
    df <- tryCatch(read.csv(input$csv_edge_props$datapath, stringsAsFactors=FALSE),
                   error = function(e) { rv$error <- paste("Edge props CSV:", e$message); NULL })
    if (is.null(df) || !"weight" %in% tolower(names(df))) return()
    names(df) <- tolower(names(df))
    rv$edge_props <- df
    rv$error <- NULL
  })

  observeEvent(input$csv_overlay_props, {
    req(input$csv_overlay_props)
    df <- tryCatch(read.csv(input$csv_overlay_props$datapath, stringsAsFactors=FALSE),
                   error = function(e) { rv$error <- paste("Overlay edge props CSV:", e$message); NULL })
    if (is.null(df) || !"weight" %in% tolower(names(df))) return()
    names(df) <- tolower(names(df))
    rv$overlay_edge_props <- df
    rv$error <- NULL
  })

  observeEvent(input$csv_legend_shapes, {
    req(input$csv_legend_shapes)
    df <- tryCatch({
      d <- read.csv(input$csv_legend_shapes$datapath, header = TRUE,
                    stringsAsFactors = FALSE)
      names(d) <- tolower(trimws(names(d)))
      d
    }, error = function(e) { rv$error <- paste("Legend shapes CSV:", e$message); NULL })
    if (is.null(df)) return()
    if (!all(c("shape","label") %in% names(df))) {
      rv$error <- "Legend shapes CSV must have columns: shape, label"
      return()
    }
    rv$legend_shapes <- df[, c("shape","label"), drop = FALSE]
    rv$error <- NULL
  })

  observeEvent(input$csv_legend_colours, {
    req(input$csv_legend_colours)
    df <- tryCatch({
      d <- read.csv(input$csv_legend_colours$datapath, header = TRUE,
                    stringsAsFactors = FALSE)
      names(d) <- tolower(trimws(names(d)))
      if ("color" %in% names(d) && !"colour" %in% names(d))
        names(d)[names(d) == "color"] <- "colour"
      d
    }, error = function(e) { rv$error <- paste("Legend colours CSV:", e$message); NULL })
    if (is.null(df)) return()
    if (!all(c("colour","label") %in% names(df))) {
      rv$error <- "Legend colours CSV must have columns: colour (or color), label"
      return()
    }
    rv$legend_colours <- df[, c("colour","label"), drop = FALSE]
    rv$error <- NULL
  })

  observeEvent(input$csv_settings, {
    req(input$csv_settings)
    s <- tryCatch(parse_settings_csv(input$csv_settings$datapath),
                  error = function(e) { rv$error <- paste("Settings CSV:", e$message); NULL })
    if (is.null(s)) return()
    if (!is.null(s$directed))
      updateCheckboxInput(session, "directed",
                          value = tolower(trimws(s$directed)) %in% c("true","1","yes"))
    if (!is.null(s$layout))
      updateSelectInput(session, "layout", selected = s$layout)
    if (!is.null(s$edge_colour))
      updateTextInput(session, "edge_colour", value = s$edge_colour)
    if (!is.null(s$edge_width))
      updateNumericInput(session, "edge_width",  value = as.numeric(s$edge_width))
    if (!is.null(s$edge_curvature))
      updateSelectInput(session, "edge_curvature", selected = s$edge_curvature)
    if (!is.null(s$use_overlay))
      updateCheckboxInput(session, "use_overlay",
                          value = tolower(trimws(s$use_overlay)) %in% c("true","1","yes"))
    if (!is.null(s$overlay_edge_colour))
      updateTextInput(session, "ovl_colour", value = s$overlay_edge_colour)
    if (!is.null(s$overlay_edge_width))
      updateNumericInput(session, "ovl_width", value = as.numeric(s$overlay_edge_width))
    if (!is.null(s$overlay_edge_style))
      updateSelectInput(session, "ovl_style", selected = s$overlay_edge_style)
    if (!is.null(s$overlay_edge_curvature))
      updateSelectInput(session, "ovl_curvature", selected = s$overlay_edge_curvature)
    if (!is.null(s$default_width))
      updateNumericInput(session, "default_width",   value = as.numeric(s$default_width))
    if (!is.null(s$default_height))
      updateNumericInput(session, "default_height",  value = as.numeric(s$default_height))
    if (!is.null(s$default_fontsize))
      updateNumericInput(session, "default_fontsize",value = as.numeric(s$default_fontsize))
    if (!is.null(s$default_fontcolour))
      updateTextInput(session, "default_fontcolour", value = s$default_fontcolour)
    if (!is.null(s$default_stroke))
      updateTextInput(session, "default_stroke",     value = s$default_stroke)
    if (!is.null(s$svg_padding))
      updateNumericInput(session, "svg_padding",     value = as.numeric(s$svg_padding))
    if (!is.null(s$sunburst_max_depth))
      updateNumericInput(session, "sunburst_max_depth",     value = as.numeric(s$sunburst_max_depth))
    if (!is.null(s$sunburst_min_branching))
      updateNumericInput(session, "sunburst_min_branching", value = as.numeric(s$sunburst_min_branching))
    if (!is.null(s$circle_r))
      updateNumericInput(session, "circle_r",  value = as.numeric(s$circle_r))
    if (!is.null(s$circle_cx))
      updateNumericInput(session, "circle_cx", value = as.numeric(s$circle_cx))
    if (!is.null(s$circle_cy))
      updateNumericInput(session, "circle_cy", value = as.numeric(s$circle_cy))
    if (!is.null(s$show_legend))
      updateCheckboxInput(session, "show_legend",
                          value = tolower(trimws(s$show_legend)) %in% c("true","1","yes"))
    if (!is.null(s$legend_title))
      updateTextInput(session, "legend_title", value = s$legend_title)
    rv$error <- NULL
  })

  # ── Render ────────────────────────────────────────────────────────────────

  observeEvent(input$render_btn, {
    rv$error  <- NULL
    rv$result <- NULL

    np  <- rv$nodes
    ids <- np$id
    n   <- length(ids)

    if (n < 2L) { rv$error <- "At least 2 nodes are required."; return() }

    # Read structural matrix from inputs; fall back to rv$adj when inputs are
    # not yet registered (e.g. immediately after a CSV import repopulates the UI)
    has_rv_adj <- !is.null(rv$adj) &&
                  identical(dim(rv$adj), c(n, n)) &&
                  identical(rownames(rv$adj), ids)
    adj <- matrix(0, n, n, dimnames = list(ids, ids))
    for (i in seq_len(n)) for (j in seq_len(n)) {
      v <- input[[paste0("adj_", i, "_", j)]]
      if (!is.null(v) && !is.na(v)) adj[i, j] <- v
      else if (has_rv_adj)          adj[i, j] <- rv$adj[i, j]
    }
    rv$adj <- adj

    # Read overlay matrix (if enabled)
    overlay <- NULL
    if (isTRUE(input$use_overlay)) {
      ovl <- matrix(0, n, n, dimnames = list(ids, ids))
      for (i in seq_len(n)) for (j in seq_len(n)) {
        v <- input[[paste0("ovl_", i, "_", j)]]
        if (!is.null(v) && !is.na(v)) ovl[i, j] <- v
      }
      rv$overlay <- ovl
      overlay <- ovl
    }

    # Coerce node-prop columns to correct types
    for (col in c("fontcolour", "stroke"))
      np[[col]][ np[[col]] %in% c("NA", "") ] <- NA_character_
    for (col in c("x", "y", "width", "height", "fontsize"))
      np[[col]] <- suppressWarnings(as.numeric(np[[col]]))

    # Convert literal \n in labels to actual newlines for multi-line SVG text
    np$label <- gsub("\\n", "\n", np$label, fixed = TRUE)

    # Circle layout: treat 0 as NULL (auto)
    circle_r  <- if (isTRUE(input$circle_r  == 0)) NULL else input$circle_r
    circle_cx <- if (isTRUE(input$circle_cx == 0)) NULL else input$circle_cx
    circle_cy <- if (isTRUE(input$circle_cy == 0)) NULL else input$circle_cy

    # Read edge labels from grid inputs (if any cell is non-empty)
    edge_labels_arg <- NULL
    ids_for_lbl <- ids
    n_lbl <- length(ids_for_lbl)
    if (!is.null(rv$edge_labels) &&
        identical(dim(rv$edge_labels), c(n_lbl, n_lbl))) {
      edge_labels_arg <- rv$edge_labels
    } else {
      # Try reading from UI grid
      elbl_m <- matrix("", n_lbl, n_lbl, dimnames = list(ids_for_lbl, ids_for_lbl))
      any_label <- FALSE
      for (i in seq_len(n_lbl)) for (j in seq_len(n_lbl)) {
        v <- input[[paste0("elbl_", i, "_", j)]]
        if (!is.null(v) && nzchar(trimws(v))) {
          elbl_m[i, j] <- trimws(v)
          any_label <- TRUE
        }
      }
      if (any_label) edge_labels_arg <- elbl_m
    }
    rv$edge_labels_arg <- edge_labels_arg

    # Centroids: empty table → NULL (falls back to eigenvector hub mode)
    centroids_arg <- if (nrow(rv$centroids) > 0L) {
      df   <- rv$centroids
      df$x <- suppressWarnings(as.numeric(df$x))
      df$y <- suppressWarnings(as.numeric(df$y))
      df
    } else {
      NULL
    }

    # Edge props: empty table → NULL
    ep_arg <- if (nrow(rv$edge_props) > 0L) rv$edge_props else NULL
    ovep_arg <- if (nrow(rv$overlay_edge_props) > 0L) rv$overlay_edge_props else NULL

    # Legend vectors
    lns_arg <- if (nrow(rv$legend_shapes) > 0L) {
      setNames(rv$legend_shapes$label, rv$legend_shapes$shape)
    } else NULL
    lnc_arg <- if (nrow(rv$legend_colours) > 0L) {
      setNames(rv$legend_colours$label, rv$legend_colours$colour)
    } else NULL

    rv$ep_arg  <- ep_arg
    rv$lns_arg <- lns_arg
    rv$lnc_arg <- lnc_arg

    result <- tryCatch(
      graph_to_outputs(
        adj_matrix             = adj,
        node_props             = np,
        directed               = isTRUE(input$directed),
        svg_file               = NULL,
        dot_file               = NULL,
        mermaid_file           = NULL,
        layout                 = input$layout,
        edge_colour            = input$edge_colour,
        edge_width             = input$edge_width,
        edge_curvature         = input$edge_curvature,
        adj_overlay            = overlay,
        overlay_edge_colour    = input$ovl_colour,
        overlay_edge_width     = input$ovl_width,
        overlay_edge_style     = input$ovl_style,
        overlay_edge_curvature = input$ovl_curvature,
        default_width          = input$default_width,
        default_height         = input$default_height,
        default_fontsize       = input$default_fontsize,
        default_fontcolour     = input$default_fontcolour,
        default_stroke         = input$default_stroke,
        svg_padding            = input$svg_padding,
        sunburst_max_depth     = as.integer(input$sunburst_max_depth),
        sunburst_min_branching = input$sunburst_min_branching,
        circle_r               = circle_r,
        circle_cx              = circle_cx,
        circle_cy              = circle_cy,
        centroids              = centroids_arg,
        edge_labels            = edge_labels_arg,
        edge_props             = ep_arg,
        overlay_edge_props     = ovep_arg,
        show_legend            = isTRUE(input$show_legend),
        legend_node_shape      = lns_arg,
        legend_node_colour     = lnc_arg,
        legend_title           = input$legend_title
      ),
      error = function(e) e
    )

    if (inherits(result, "error")) {
      rv$error  <- conditionMessage(result)
    } else {
      rv$result <- result
      session$sendCustomMessage("canvas_offset", list(
        xlo = result$canvas$xlo,
        ylo = result$canvas$ylo
      ))
    }
  })

  # ── Output renderers ──────────────────────────────────────────────────────

  output$svg_out <- renderUI({
    if (!is.null(rv$error))
      return(tags$div(class = "error-box", tags$b("Error: "), rv$error))
    if (is.null(rv$result))
      return(tags$p(style = "color:#a0aec0; margin-top:80px; font-size:14px;",
        "\u25b6\u2002Click \u201cRender Graph\u201d to generate output."))
    HTML(rv$result$svg)
  })

  output$dot_out <- renderText({
    if (is.null(rv$result)) return("# Click \u201cRender Graph\u201d to generate DOT.")
    rv$result$dot
  })

  output$mermaid_out <- renderText({
    if (is.null(rv$result)) return("# Click \u201cRender Graph\u201d to generate Mermaid.")
    rv$result$mermaid
  })

  output$topo_out <- renderTable({
    req(rv$result)
    topo <- rv$result$topology

    fmt_bool <- function(x) if (isTRUE(x)) "TRUE" else "FALSE"
    fmt_na   <- function(x, fmt = "%s") if (is.na(x)) "NA" else sprintf(fmt, x)

    rows <- list(
      c("Graph type",              topo$type),
      c("Recommended layout",      topo$recommended_layout),
      c("Nodes",                   as.character(topo$n_nodes)),
      c("Edges",                   as.character(topo$n_edges)),
      c("Density",                 fmt_na(topo$density,              "%.4f")),
      c("Max depth",               fmt_na(topo$max_depth,            "%d")),
      c("Avg branching factor",    fmt_na(topo$avg_branching_factor, "%.2f")),
      c("Acyclic",                 fmt_bool(topo$is_acyclic)),
      c("Weakly connected",        fmt_bool(topo$is_weakly_connected)),
      c("Strongly connected",      fmt_bool(topo$is_strongly_connected)),
      c("Bipartite",               fmt_bool(topo$is_bipartite)),
      c("Tree",                    fmt_bool(topo$is_tree)),
      c("Forest",                  fmt_bool(topo$is_forest)),
      c("SCCs",                    as.character(topo$n_strongly_connected_components)),
      c("Root nodes",              paste(topo$root_nodes, collapse = ", ")),
      c("Leaf nodes",              paste(topo$leaf_nodes,  collapse = ", "))
    )

    data.frame(
      Property = vapply(rows, `[[`, character(1L), 1L),
      Value    = vapply(rows, `[[`, character(1L), 2L),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = FALSE,
     spacing = "xs", colnames = FALSE, width = "100%")

  output$rcode_out <- renderText({
    req(rv$result)
    np      <- rv$nodes
    ids     <- np$id
    adj     <- rv$adj
    n       <- length(ids)
    cen     <- rv$centroids
    ep_arg  <- rv$ep_arg
    lns_arg <- rv$lns_arg
    lnc_arg <- rv$lnc_arg

    fmtv <- function(x, q = FALSE) {
      s <- vapply(x, function(v) {
        if (is.na(v)) "NA" else if (q) paste0('"', v, '"') else as.character(v)
      }, character(1L))
      paste(s, collapse = ", ")
    }

    mat_rows <- apply(adj, 1L, function(r) paste(r, collapse = ", "))
    mat_body <- paste0("    ", mat_rows, collapse = ",\n")

    # Centroids block (only when table is non-empty)
    cen_block <- if (nrow(cen) > 0L) {
      cen$x <- suppressWarnings(as.numeric(cen$x))
      cen$y <- suppressWarnings(as.numeric(cen$y))
      paste0(
        "centroids <- data.frame(\n",
        "  label = c(", fmtv(cen$label, q = TRUE), "),\n",
        "  x     = c(", fmtv(cen$x), "),\n",
        "  y     = c(", fmtv(cen$y), "),\n",
        "  stringsAsFactors = FALSE\n",
        ")\n\n"
      )
    } else ""

    cen_arg <- if (nrow(cen) > 0L) "  centroids      = centroids,\n" else
      "  # centroids = NULL  # (uses eigenvector-centrality hub)\n"

    # Edge labels block
    elbl_block <- ""
    elbl_arg   <- "  # edge_labels = NULL  # (no edge labels)\n"
    edge_labels_arg <- rv$edge_labels_arg
    if (!is.null(edge_labels_arg)) {
      nr <- nrow(edge_labels_arg)
      nc <- ncol(edge_labels_arg)
      cells <- apply(edge_labels_arg, 2L, function(col)
        paste0('  c(', paste0('"', col, '"', collapse = ", "), ')')
      )
      elbl_block <- paste0(
        "edge_labels <- matrix(\n  c(\n",
        paste(cells, collapse = ",\n"), "\n  ),\n",
        "  nrow = ", nr, ", byrow = FALSE,\n",
        "  dimnames = list(ids, ids)\n)\n\n"
      )
      elbl_arg <- "  edge_labels       = edge_labels,\n"
    }

    # Edge props block
    ep_block <- ""
    ep_arg_code <- "  # edge_props = NULL,\n"
    if (!is.null(ep_arg)) {
      ep_block <- paste0(
        "edge_props <- data.frame(\n",
        "  weight   = c(", paste(ep_arg$weight, collapse = ", "), "),\n",
        "  colour   = c(", paste0('"', ep_arg$colour, '"', collapse = ", "), "),\n",
        "  width    = c(", paste(ep_arg$width,  collapse = ", "), "),\n",
        "  linetype = c(", paste0('"', ep_arg$linetype, '"', collapse = ", "), "),\n",
        "  label    = c(", paste0('"', ep_arg$label, '"',  collapse = ", "), "),\n",
        "  stringsAsFactors = FALSE\n)\n\n"
      )
      ep_arg_code <- "  edge_props         = edge_props,\n"
    }

    # Legend args
    leg_args_code <- ""
    if (isTRUE(input$show_legend)) {
      leg_args_code <- paste0(
        "  show_legend        = TRUE,\n",
        "  legend_title       = \"", input$legend_title, "\",\n"
      )
      if (!is.null(lns_arg))
        leg_args_code <- paste0(leg_args_code,
          "  legend_node_shape  = c(", paste0(names(lns_arg), ' = "', lns_arg, '"', collapse=", "), "),\n")
      if (!is.null(lnc_arg))
        leg_args_code <- paste0(leg_args_code,
          "  legend_node_colour = c(", paste0('"', names(lnc_arg), '" = "', lnc_arg, '"', collapse=", "), "),\n")
    }

    paste0(
      "library(matxingraphout)\n\n",
      "ids <- c(", fmtv(ids, q = TRUE), ")\n\n",
      "adj <- matrix(\n  c(\n", mat_body, "\n  ),\n",
      "  nrow = ", n, ", byrow = TRUE,\n",
      "  dimnames = list(ids, ids)\n)\n\n",
      "nodes <- data.frame(\n",
      "  id        = c(", fmtv(np$id,    q = TRUE), "),\n",
      "  label     = c(", fmtv(np$label, q = TRUE), "),\n",
      "  shape     = c(", fmtv(np$shape, q = TRUE), "),\n",
      "  colour    = c(", fmtv(np$colour,q = TRUE), "),\n",
      "  x         = c(", fmtv(np$x),    "),\n",
      "  y         = c(", fmtv(np$y),    "),\n",
      "  stringsAsFactors = FALSE\n)\n\n",
      cen_block,
      elbl_block,
      ep_block,
      "result <- graph_to_outputs(\n",
      "  adj_matrix     = adj,\n",
      "  node_props     = nodes,\n",
      "  directed       = ", tolower(as.character(isTRUE(input$directed))), ",\n",
      "  layout         = \"", input$layout, "\",\n",
      "  edge_colour    = \"", input$edge_colour, "\",\n",
      "  edge_width     = ", input$edge_width, ",\n",
      "  edge_curvature = \"", input$edge_curvature, "\",\n",
      cen_arg,
      elbl_arg,
      ep_arg_code,
      leg_args_code,
      "  svg_file       = \"graph.svg\",\n",
      "  dot_file       = \"graph.dot\",\n",
      "  mermaid_file   = \"graph.mmd\"\n",
      ")\n"
    )
  })

  # ── Downloads ──────────────────────────────────────────────────────────────

  output$dl_svg <- downloadHandler(
    filename = "graph.svg",
    content  = function(f) { req(rv$result); writeLines(rv$result$svg,     f) }
  )
  output$dl_svg_clean <- downloadHandler(
    filename = "graph_clean.svg",
    content  = function(f) {
      req(rv$result)
      clean_svg <- gsub(
        '<g class="centroid-marker"',
        '<g class="centroid-marker" opacity="0"',
        rv$result$svg, fixed = TRUE
      )
      writeLines(clean_svg, f)
    }
  )
  output$dl_dot <- downloadHandler(
    filename = "graph.dot",
    content  = function(f) { req(rv$result); writeLines(rv$result$dot,     f) }
  )
  output$dl_mmd <- downloadHandler(
    filename = "graph.mmd",
    content  = function(f) { req(rv$result); writeLines(rv$result$mermaid, f) }
  )

  # Centroid placed via mouse click on SVG
  observeEvent(input$centroid_click, {
    click <- input$centroid_click
    if (is.null(click$x) || is.null(click$y)) return()
    i <- nrow(rv$centroids) + 1L
    rv$centroids <- rbind(rv$centroids, data.frame(
      label = paste0("C", i),
      x     = round(as.numeric(click$x)),
      y     = round(as.numeric(click$y)),
      stringsAsFactors = FALSE
    ))
  })

  # Centroid removed by clicking its marker on SVG (0-based index from JS)
  observeEvent(input$centroid_remove_idx, {
    idx <- input$centroid_remove_idx
    if (is.null(idx)) return()
    r <- as.integer(idx) + 1L   # convert 0-based JS index to 1-based R index
    if (r >= 1L && r <= nrow(rv$centroids))
      rv$centroids <- rv$centroids[-r, , drop = FALSE]
  })
}

# ── launch ────────────────────────────────────────────────────────────────────

shinyApp(ui, server)
