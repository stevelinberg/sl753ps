#' Connect isolated nodes to themselves in a statnet graph
#'
#' @param ntwk_sna
#'
#' @return ntwk_sna
#' @export
#'
#' @examples
#' \dontrun {
#' flo_stat <- connect_isolates_sna(flo_stat)
#' }
connect_isolates_sna <- function(ntwk_sna) {
  if (!network::is.network(ntwk_sna)) stop("ntwk_sna is not a network object")

  zero_degree_node_indices <- sna::isolates(ntwk_sna)
  if (length(zero_degree_node_indices) > 0) {
    ntwk_sna <- network::add.edges(
      ntwk_sna, zero_degree_node_indices, zero_degree_node_indices
    )
  }

  return(ntwk_sna)
}

#' Connect isolated nodes to themselves in an igraph graph
#'
#' @param ntwk_ig
#'
#' @return ntwk_ig
#' @export
#'
#' @examples
#' \dontrun {
#' flo_ig <- connect_isolates_sna(flo_ig)
#' }
connect_isolates_ig <- function(ntwk_ig) {
  if (!igraph::is.igraph(ntwk_ig)) stop("ntwk_ig is not an igraph object")

  zero_degree_node_indices <- which(igraph::degree(ntwk_ig) == 0)
  # # rep(..., each = 2) turns c(1, 2, 3) into c(1, 1, 2, 2, 3, 3) for add_edges
  if (length(zero_degree_node_indices) > 0) {
    ntwk_ig <- igraph::add_edges(
      ntwk_ig, rep(zero_degree_node_indices, each = 2)
    )
  }

  return(ntwk_ig)
}
