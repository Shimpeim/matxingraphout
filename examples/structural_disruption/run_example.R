library(matxingraphout)

nodes <- read.csv("nodes.csv", stringsAsFactors = FALSE)

adj_raw <- read.csv("adj_matrix.csv", row.names = 1, check.names = FALSE)
adj <- as.matrix(adj_raw)

result <- graph_to_outputs(
  adj_matrix   = adj,
  node_props   = nodes,
  directed     = TRUE,
  layout       = "tree",
  svg_file     = "structural_disruption.svg",
  dot_file     = "structural_disruption.dot",
  mermaid_file = "structural_disruption.mmd"
)

cat("Graph type:", result$topology$type, "\n")
cat("Nodes:", result$topology$n_nodes, " Edges:", result$topology$n_edges, "\n")
