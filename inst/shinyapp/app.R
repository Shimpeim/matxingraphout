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
    g.style.cursor = (mode === 'remove') ? 'pointer' : 'default';
    g.onclick = function(e) {
      if (mode === 'remove') {
        var idx = parseInt(g.getAttribute('data-centroid-idx'), 10);
        Shiny.setInputValue('centroid_remove_idx', idx, { priority: 'event' });
        g.parentNode.removeChild(g);
        e.stopPropagation();
      }
    };
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
        )
      ),

      # Render button ---
      tags$button(id = "render_btn", class = "btn render-btn",
        onclick = "Shiny.setInputValue('render_btn', Math.random())",
        "\u25b6\u2002Render Graph")
    ),

    # ── RIGHT : output panels ───────────────────────────────────────────────
    column(7,
      tags$div(class = "panel-box",
        tabsetPanel(id = "out_tabs",

          tabPanel("SVG",
            br(),
            tags$div(class = "dl-row", style = "align-items:center; flex-wrap:wrap; gap:6px;",
              downloadButton("dl_svg", "Download SVG", class = "btn-sm btn-default"),
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
    nodes     = default_nodes(),
    adj       = default_adj(c("A", "B", "C")),
    overlay   = matrix(0, 3L, 3L,
                       dimnames = list(c("A","B","C"), c("A","B","C"))),
    centroids = data.frame(label = character(0), x = numeric(0), y = numeric(0),
                           stringsAsFactors = FALSE),
    result    = NULL,
    error     = NULL
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

  output$adj_matrix_ui <- renderUI({ make_matrix_ui(rv$nodes$id, "adj") })
  output$ovl_matrix_ui <- renderUI({ make_matrix_ui(rv$nodes$id, "ovl") })

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

  # ── Render ────────────────────────────────────────────────────────────────

  observeEvent(input$render_btn, {
    rv$error  <- NULL
    rv$result <- NULL

    np  <- rv$nodes
    ids <- np$id
    n   <- length(ids)

    if (n < 2L) { rv$error <- "At least 2 nodes are required."; return() }

    # Read structural matrix from inputs
    adj <- matrix(0, n, n, dimnames = list(ids, ids))
    for (i in seq_len(n)) for (j in seq_len(n)) {
      v <- input[[paste0("adj_", i, "_", j)]]
      if (!is.null(v) && !is.na(v)) adj[i, j] <- v
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

    # Circle layout: treat 0 as NULL (auto)
    circle_r  <- if (isTRUE(input$circle_r  == 0)) NULL else input$circle_r
    circle_cx <- if (isTRUE(input$circle_cx == 0)) NULL else input$circle_cx
    circle_cy <- if (isTRUE(input$circle_cy == 0)) NULL else input$circle_cy

    # Centroids: empty table → NULL (falls back to eigenvector hub mode)
    centroids_arg <- if (nrow(rv$centroids) > 0L) {
      df   <- rv$centroids
      df$x <- suppressWarnings(as.numeric(df$x))
      df$y <- suppressWarnings(as.numeric(df$y))
      df
    } else {
      NULL
    }

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
        centroids              = centroids_arg
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
    np   <- rv$nodes
    ids  <- np$id
    adj  <- rv$adj
    n    <- length(ids)
    cen  <- rv$centroids

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
      "result <- graph_to_outputs(\n",
      "  adj_matrix     = adj,\n",
      "  node_props     = nodes,\n",
      "  directed       = ", tolower(as.character(isTRUE(input$directed))), ",\n",
      "  layout         = \"", input$layout, "\",\n",
      "  edge_colour    = \"", input$edge_colour, "\",\n",
      "  edge_width     = ", input$edge_width, ",\n",
      "  edge_curvature = \"", input$edge_curvature, "\",\n",
      cen_arg,
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
