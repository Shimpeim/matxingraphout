# ── Shared fixtures ───────────────────────────────────────────────────────────

# Minimal 2-node graph (A → B)
.ids2 <- c("A", "B")
.adj2 <- matrix(c(0, 1, 0, 0), 2, 2, dimnames = list(.ids2, .ids2))
.nodes2 <- data.frame(
  id = .ids2, x = c(100, 300), y = c(150, 150),
  shape = c("rect", "circle"),
  colour = c("#e8f0fe", "#fde8ec"),
  label = .ids2, stringsAsFactors = FALSE
)

# NIHR figure 10 — 23-node shallow tree (depth 2, high branching)
.node_ids <- paste0("N", 1:23)
.n <- length(.node_ids)
.adj_nihr <- matrix(0L, nrow = .n, ncol = .n,
                    dimnames = list(.node_ids, .node_ids))
.adj_nihr["N1", c("N2","N3","N4","N5","N6")]          <- 1L
.adj_nihr["N2", c("N7","N8","N9")]                     <- 1L
.adj_nihr["N3", c("N10","N11","N12","N13")]            <- 1L
.adj_nihr["N4", c("N14","N15","N16","N17")]            <- 1L
.adj_nihr["N5", c("N18","N19","N20")]                  <- 1L
.adj_nihr["N6", c("N21","N22","N23")]                  <- 1L

.nodes_nihr <- data.frame(
  id = .node_ids,
  label = c(
    "Candidacy and\ndiagnostic processes",
    "Diagnostic\nshock",
    "Biographical and\nrelational disruption",
    "Biographical and\nrelational erosion",
    "Biographical and\nrelational fracture",
    "Biographical and\nrelational repair",
    "Responding to\nexistential threat",
    "Experiences of distress\nand personal risk",
    "Information-seeking/\nextend understanding",
    "Mobilisation of\ncaregiver contributions",
    "Struggles over care\nand access to services",
    "Relations with health\nprofessionals and services",
    "Decisions and\ndecisional conflicts",
    "Responding to role\nstrain and restrictions",
    "Transfers of responsibilities\nto caregivers",
    "Negotiations\nwithin families",
    "Manage diminishing\nhorizons over time",
    "Managing symptoms\nand disease progression",
    "Mitigating social\ndislocation",
    "Restrictions on service\nusers and caregivers",
    "Acquisition of skills\nin self-management",
    "Seeking social\n(re)integration",
    "Controlled disclosure and\nmanagement of stigma"
  ),
  x = c(500,
        100, 300, 500, 700, 900,
        100, 100, 100,
        300, 300, 300, 300,
        500, 500, 500, 500,
        700, 700, 700,
        900, 900, 900),
  y = c(60,
        200, 200, 200, 200, 200,
        380, 500, 620,
        380, 500, 620, 740,
        380, 500, 620, 740,
        380, 500, 620,
        380, 500, 620),
  shape  = "rounded",
  colour = c("#d0e4f7", rep("#e8f4f8", 5), rep("#ffffff", 17)),
  width  = 140, height = 55, fontsize = 10,
  stringsAsFactors = FALSE
)

# Simple cycle: A → B → C → A
.ids_cyc <- c("A", "B", "C")
.adj_cyc <- matrix(
  c(0,1,0, 0,0,1, 1,0,0), 3, 3, byrow = TRUE,
  dimnames = list(.ids_cyc, .ids_cyc)
)
.nodes_cyc <- data.frame(
  id = .ids_cyc, x = c(100,200,150), y = c(100,100,200),
  shape = "circle", colour = "#e8f0fe", label = .ids_cyc,
  stringsAsFactors = FALSE
)

# Bipartite graph: {A,B} → {C,D}
.ids_bip <- c("A","B","C","D")
.adj_bip <- matrix(
  c(0,0,1,1, 0,0,1,1, 0,0,0,0, 0,0,0,0), 4, 4, byrow = TRUE,
  dimnames = list(.ids_bip, .ids_bip)
)
.nodes_bip <- data.frame(
  id = .ids_bip, x = c(100,100,300,300), y = c(100,200,100,200),
  shape = "rect", colour = "#e8f0fe", label = .ids_bip,
  stringsAsFactors = FALSE
)


# ── 1. Return structure ───────────────────────────────────────────────────────

test_that("returns a list with svg, dot, mermaid, topology", {
  res <- graph_to_outputs(.adj2, .nodes2,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_type(res, "list")
  expect_named(res, c("svg", "dot", "mermaid", "topology"))
})

test_that("svg, dot, mermaid elements are non-empty character strings", {
  res <- graph_to_outputs(.adj2, .nodes2,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_type(res$svg,     "character")
  expect_type(res$dot,     "character")
  expect_type(res$mermaid, "character")
  expect_gt(nchar(res$svg),     0)
  expect_gt(nchar(res$dot),     0)
  expect_gt(nchar(res$mermaid), 0)
})


# ── 2. Input validation ───────────────────────────────────────────────────────

test_that("stops on missing required node_props columns", {
  bad <- data.frame(id = .ids2, x = 1, y = 1)
  expect_error(
    graph_to_outputs(.adj2, bad, svg_file = NULL,
                     dot_file = NULL, mermaid_file = NULL),
    "missing column"
  )
})

test_that("stops on non-square adj_matrix", {
  expect_error(
    graph_to_outputs(matrix(1:6, 2, 3), .nodes2,
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})

test_that("stops when node_props ids do not match adj_matrix rownames", {
  bad_nodes <- .nodes2
  bad_nodes$id <- c("X", "Y")
  expect_error(
    graph_to_outputs(.adj2, bad_nodes,
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})


# ── 3. Topology — NIHR tree ───────────────────────────────────────────────────

test_that("NIHR graph is classified as a tree", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  topo <- res$topology
  expect_equal(topo$type,      "tree")
  expect_true (topo$is_acyclic)
  expect_true (topo$is_tree)
  expect_true (topo$is_forest)   # a tree is a connected forest (in_deg ≤ 1 for all)
  expect_true (topo$is_weakly_connected)
  expect_false(topo$is_strongly_connected)
  expect_equal(topo$n_nodes, 23L)
  expect_equal(topo$n_edges, 22L)
  expect_equal(topo$root_nodes, "N1")
  expect_length(topo$leaf_nodes, 17L)
})

test_that("NIHR topology has correct depth and branching", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  topo <- res$topology
  expect_equal(topo$max_depth, 2L)
  expect_gt(topo$avg_branching_factor, 3)
})

test_that("NIHR topology recommends sunburst (depth 2, branching > 3)", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_equal(res$topology$recommended_layout, "sunburst")
})


# ── 4. Topology — cyclic graph ────────────────────────────────────────────────

test_that("cycle graph is classified as cyclic", {
  res <- graph_to_outputs(.adj_cyc, .nodes_cyc,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  topo <- res$topology
  expect_false(topo$is_acyclic)
  expect_false(topo$is_tree)
  expect_true (topo$is_strongly_connected)
  expect_true (topo$is_weakly_connected)
  expect_true (is.na(topo$max_depth))
})

test_that("cycle graph recommends circular layout", {
  res <- graph_to_outputs(.adj_cyc, .nodes_cyc,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_equal(res$topology$recommended_layout, "circular")
})


# ── 5. Topology — bipartite graph ─────────────────────────────────────────────

test_that("bipartite graph is detected correctly", {
  res <- graph_to_outputs(.adj_bip, .nodes_bip,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_true(res$topology$is_bipartite)
})

test_that("acyclic bipartite graph recommends tree (not bipartite layout)", {
  # The bipartite fixture is also a DAG, so tree takes priority over bipartite
  res <- graph_to_outputs(.adj_bip, .nodes_bip,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_match(res$topology$recommended_layout, "tree|sunburst")
})


# ── 6. Layout modes ───────────────────────────────────────────────────────────

test_that("layout = 'auto' runs without error on NIHR data", {
  expect_no_error(
    graph_to_outputs(.adj_nihr, .nodes_nihr, layout = "auto",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})

test_that("layout = 'sunburst' runs without error on NIHR data", {
  expect_no_error(
    graph_to_outputs(.adj_nihr, .nodes_nihr, layout = "sunburst",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})

test_that("layout = 'tree' runs without error on NIHR data", {
  expect_no_error(
    graph_to_outputs(.adj_nihr, .nodes_nihr, layout = "tree",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})

test_that("layout = 'circular' runs without error on NIHR data", {
  expect_no_error(
    graph_to_outputs(.adj_nihr, .nodes_nihr, layout = "circular",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})

test_that("layout = 'bipartite' runs without error on bipartite data", {
  expect_no_error(
    graph_to_outputs(.adj_bip, .nodes_bip, layout = "bipartite",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})

test_that("invalid layout value raises an error", {
  expect_error(
    graph_to_outputs(.adj2, .nodes2, layout = "radial",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})


# ── 7. sunburst threshold parameters ─────────────────────────────────────────

test_that("sunburst_max_depth = 0 forces tree recommendation for NIHR data", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          sunburst_max_depth = 0L,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_equal(res$topology$recommended_layout, "tree")
})

test_that("sunburst_min_branching = 99 forces tree recommendation for NIHR data", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          sunburst_min_branching = 99,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_equal(res$topology$recommended_layout, "tree")
})

test_that("relaxed thresholds preserve sunburst recommendation for NIHR data", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          sunburst_max_depth = 5L, sunburst_min_branching = 2,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_equal(res$topology$recommended_layout, "sunburst")
})


# ── 8. File writing ───────────────────────────────────────────────────────────

test_that("SVG file is written to disk when svg_file is set", {
  tmp <- tempfile(fileext = ".svg")
  on.exit(unlink(tmp))
  graph_to_outputs(.adj2, .nodes2,
                   svg_file = tmp, dot_file = NULL, mermaid_file = NULL)
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
})

# ── 9. node_props without x / y ──────────────────────────────────────────────

.nodes_nihr_noxy <- .nodes_nihr[, setdiff(names(.nodes_nihr), c("x", "y"))]

test_that("node_props without x/y is accepted for auto layout", {
  expect_no_error(
    graph_to_outputs(.adj_nihr, .nodes_nihr_noxy, layout = "auto",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})

test_that("node_props without x/y is accepted for tree layout", {
  expect_no_error(
    graph_to_outputs(.adj_nihr, .nodes_nihr_noxy, layout = "tree",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})

test_that("node_props without x/y is accepted for sunburst layout", {
  expect_no_error(
    graph_to_outputs(.adj_nihr, .nodes_nihr_noxy, layout = "sunburst",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})

test_that("node_props without x/y is accepted for circular layout", {
  expect_no_error(
    graph_to_outputs(.adj_nihr, .nodes_nihr_noxy, layout = "circular",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})

test_that("node_props without x/y raises an error for manual layout", {
  expect_error(
    graph_to_outputs(.adj_nihr, .nodes_nihr_noxy, layout = "manual",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL),
    "missing column"
  )
})

# ── 10. Overlay matrix ────────────────────────────────────────────────────────

# Overlay: add a cross-link N7 → N21 (not in structural matrix)
.adj_ov <- matrix(0L, .n, .n, dimnames = list(.node_ids, .node_ids))
.adj_ov["N7", "N21"] <- 1L

test_that("adj_overlay is accepted and does not change topology", {
  res_plain   <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                                  svg_file = NULL, dot_file = NULL,
                                  mermaid_file = NULL)
  res_overlay <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                                  adj_overlay = .adj_ov,
                                  svg_file = NULL, dot_file = NULL,
                                  mermaid_file = NULL)
  expect_equal(res_plain$topology,   res_overlay$topology)
  expect_equal(res_plain$topology$n_edges, 22L)
})

test_that("overlay edge appears in SVG output", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          adj_overlay = .adj_ov,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_true(grepl("overlay edges", res$svg, fixed = TRUE))
  expect_true(grepl("arrowhead-ov",  res$svg, fixed = TRUE))
})

test_that("overlay edge appears in DOT output", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          adj_overlay = .adj_ov,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_true(grepl("overlay edges", res$dot, fixed = TRUE))
  expect_true(grepl("dashed",        res$dot, fixed = TRUE))
})

test_that("overlay edge appears in Mermaid output as dashed arrow", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          adj_overlay = .adj_ov,
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_true(grepl("Overlay edges", res$mermaid, fixed = TRUE))  # section renamed to 10
  expect_true(grepl("-.->",          res$mermaid, fixed = TRUE))
})

test_that("overlay_edge_style = 'solid' produces solid lines in SVG", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          adj_overlay = .adj_ov,
                          overlay_edge_style = "solid",
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_false(grepl("stroke-dasharray", res$svg, fixed = TRUE))
})

test_that("adj_overlay with wrong dimensions raises an error", {
  bad_ov <- matrix(0L, 3, 3)
  expect_error(
    graph_to_outputs(.adj_nihr, .nodes_nihr,
                     adj_overlay = bad_ov,
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})

test_that("no files are written when all file args are NULL", {
  before <- list.files(tempdir())
  graph_to_outputs(.adj2, .nodes2,
                   svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  after <- list.files(tempdir())
  expect_equal(before, after)
})

# ── 11. edge_curvature ─────────────────────────────────────────────────────────

test_that("edge_curvature='auto' with circular layout produces SVG arc paths", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          layout = "circular",
                          edge_curvature = "auto",
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  # At least one arc path element should be present
  expect_true(grepl("<path d=\"M ", res$svg, fixed = TRUE))
  expect_true(grepl(" A ", res$svg, fixed = TRUE))
})

test_that("edge_curvature='straight' with circular layout produces only lines", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          layout = "circular",
                          edge_curvature = "straight",
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  # Edges should be <line> elements, not arc paths
  expect_true(grepl("<line ", res$svg, fixed = TRUE))
  # Arc flag ' A ' should not appear in edge paths (self-loops use C, not A)
  edge_section <- sub("<!-- nodes -->.*", "", res$svg)
  expect_false(grepl(" A [0-9]", edge_section))
})

test_that("edge_curvature='auto' with sunburst layout produces SVG arc paths", {
  res <- graph_to_outputs(.adj_nihr, .nodes_nihr,
                          layout = "sunburst",
                          edge_curvature = "auto",
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  expect_true(grepl(" A ", res$svg, fixed = TRUE))
})

test_that("edge_curvature='auto' with manual layout produces straight lines", {
  res <- graph_to_outputs(.adj2, .nodes2,
                          layout = "manual",
                          edge_curvature = "auto",
                          svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  # No radial centre is set for manual layout → straight lines only
  edge_section <- sub("<!-- nodes -->.*", "", res$svg)
  expect_false(grepl(" A [0-9]", edge_section))
})

test_that("edge_curvature rejects unknown values", {
  expect_error(
    graph_to_outputs(.adj2, .nodes2,
                     edge_curvature = "curved",
                     svg_file = NULL, dot_file = NULL, mermaid_file = NULL)
  )
})
