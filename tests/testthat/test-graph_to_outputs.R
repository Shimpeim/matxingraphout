test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

ids <- c("A", "B")
adj <- matrix(c(0,1,0,0), 2, 2, dimnames = list(ids, ids))
nodes <- data.frame(
  id = ids, x = c(100,300), y = c(150,150),
  shape = c("rect","circle"),
  colour = c("#e8f0fe","#fde8ec"),
  label = ids, stringsAsFactors = FALSE
)

test_that("returns list with svg, dot, mermaid", {
  res <- graph_to_outputs(adj, nodes,
                          svg_file=NULL, dot_file=NULL, mermaid_file=NULL)
  expect_type(res, "list")
  expect_named(res, c("svg","dot","mermaid"))
})

test_that("stops on missing required columns", {
  bad <- data.frame(id=ids, x=1, y=1)
  expect_error(graph_to_outputs(adj, bad), "missing column")
})

