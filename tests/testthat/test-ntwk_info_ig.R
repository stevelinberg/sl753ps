# Set up the igraph version of the Florentine wedding dataset for testing

data(flo)
# If the matrix is symmetric, it is undirected.
# https://mathworld.wolfram.com/UndirectedGraph.html
flo_ig <- igraph::graph_from_adjacency_matrix(
  flo, mode = ifelse(isSymmetric(flo), "undirected", "directed"))

# Connect the unconnected igraph nodes to themselves
zero_degree_node_indices <- which(igraph::degree(flo_ig) == 0)
# # rep(..., each = 2) turns c(1, 2, 3) into c(1, 1, 2, 2, 3, 3) for add_edges
if (length(zero_degree_node_indices) > 0) {
  flo_ig <- igraph::add_edges(
    flo_ig, rep(zero_degree_node_indices, each = 2))
}

test_that("ntwk_info_ig returns 16 named vertices for flo_ig", {
  flo_ig_info <- ntwk_info_ig(flo_ig)
  expect_equal(length(flo_ig_info$ni$name), 16)
})
