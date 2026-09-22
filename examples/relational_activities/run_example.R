library(matxingraphout)

nodes <- read.csv("nodes.csv", stringsAsFactors = FALSE)

adj_raw <- read.csv("adj_matrix.csv", row.names = 1, check.names = FALSE)
adj <- as.matrix(adj_raw)

result <- graph_to_outputs(
  adj_matrix   = adj,
  node_props   = nodes,
  directed     = TRUE,
  layout       = "tree",
  svg_file     = "relational_activities.svg",
  dot_file     = "relational_activities.dot",
  mermaid_file = "relational_activities.mmd"
)

cat("Graph type:", result$topology$type, "\n")
cat("Nodes:", result$topology$n_nodes, " Edges:", result$topology$n_edges, "\n")
