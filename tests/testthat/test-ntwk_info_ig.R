# Set up the igraph version of the Florentine wedding dataset for testing

library(network)
data(flo)
# If the matrix is symmetric, it is undirected.
# https://mathworld.wolfram.com/UndirectedGraph.html
flo_ig <- igraph::graph_from_adjacency_matrix(
  flo, mode = ifelse(isSymmetric(flo), "undirected", "directed"))

flo_ig <- connect_isolates_ig(flo_ig)

test_that("ntwk_info_ig returns 16 named vertices for flo_ig", {
  flo_ig_info <- ntwk_info_ig(flo_ig)
  expect_equal(length(flo_ig_info$ni$name), 16)
})
