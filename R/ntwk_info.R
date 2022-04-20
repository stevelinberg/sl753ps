ntwk_info_names <- c(
  "ni", "is_directed", "is_bipartite", "is_weighted",
  "is_connected"
)

#' Return information about an iGraph network object
#'
#' @param ntwk_ig the iGraph network object
#' @param loops_p value for igraph::degree `loops` parameter
#'
#' @return a list of network information
#' @export
#'
#' @examples
#' \dontrun{
#' x <- ntwk_info_ig(flomarr.ig)
#' flo_dir <- x$is_directed
#' }
ntwk_info_ig <- function(ntwk_ig, loops_p = FALSE) {
  if (!igraph::is.igraph(ntwk_ig)) stop("ntwk_ig is not an igraph object")
  ntwk_directed <- igraph::is_directed(ntwk_ig)
  nodeinfo <- data.frame(
    name      = igraph::V(ntwk_ig)$name,
    totdegree = igraph::degree(ntwk_ig, loops = loops_p),
    indegree  = igraph::degree(ntwk_ig, mode = "in", loops = loops_p),
    outdegree = igraph::degree(ntwk_ig, mode = "out", loops = loops_p),
    eigen     = igraph::centr_eigen(ntwk_ig, directed = ntwk_directed, scale = TRUE)$vector,
    bonanich  = igraph::power_centrality(ntwk_ig),
    # weight attribute will be used automatically if it exists
    centr_clo = igraph::closeness(ntwk_ig, normalized = TRUE),
    centr_btw = igraph::betweenness(ntwk_ig, directed = ntwk_directed, weights = NA),
    burt      = igraph::constraint(ntwk_ig)
  )
  nodeinfo$centr_clo <- nodeinfo$centr_clo / max(nodeinfo$centr_clo)

  output_list <-
    list(
      nodeinfo,
      ntwk_directed,
      igraph::is_bipartite(ntwk_ig),
      igraph::is_weighted(ntwk_ig),
      igraph::is_connected(ntwk_ig)
    )
  names(output_list) <- ntwk_info_names
  return(output_list)
}

#' Return information about a statnet network object
#'
#' @param ntwk_sna the statnet network object
#'
#' @return a list of network information
#' @importFrom network %v%
#' @export
#'
#' @examples
#' \dontrun{
#' x <- ntwk_info_sna(flomarr.sna)
#' flo_dir <- x$is_directed
#' }
ntwk_info_sna <- function(ntwk_sna) {
  if (!network::is.network(ntwk_sna)) stop("ntwk_sna is not a network object")

  ntwk_directed <- ntwk_sna$gal$directed
  gmode_p <- ifelse(ntwk_directed, "digraph", "graph")
  ntwk_connected <- sna::components(ntwk_sna) == 1

  # if there is exactly one edge attribute not named "na", it is the weight
  # attribute (regardless of its name)
  ntwk_edge_attrs <- network::list.edge.attributes(ntwk_sna)
  ntwk_edge_attrs <- ntwk_edge_attrs[ntwk_edge_attrs != "na"]
  ntwk_weighted <- length(ntwk_edge_attrs) > 0
  # ntwk_weight_name <- ifelse(ntwk_weighted, ntwk_edge_attrs[1], NA)

  nodeinfo <- data.frame(
    name = ntwk_sna %v% "vertex.names",
    totdegree = sna::degree(ntwk_sna, gmode = gmode_p),
    indegree = sna::degree(ntwk_sna, gmode = gmode_p, cmode = "indegree"),
    outdegree = sna::degree(ntwk_sna, gmode = gmode_p, cmode = "outdegree"),
    # Note: we might not want to use weights here if we want to stay consistent
    # with igraph.
    eigen = sna::evcent(ntwk_sna, ignore.eval = ntwk_weighted, rescale = FALSE),
    bonanich = sna::bonpow(ntwk_sna),
    centr_clo = sna::closeness(ntwk_sna,
      gmode = gmode_p,
      ignore.eval = FALSE,
      rescale = FALSE,
      cmode = ifelse(ntwk_connected,
                     ifelse(ntwk_directed, "suminvdir", "suminvundir"),
                     # As per sna::connect, if the network is disconnected,
                     # use Gil-Schmidt closeness calculation.
                     "gil-schmidt")
    ),
    centr_btw = sna::betweenness(ntwk_sna, gmode = gmode_p)
  )
  # Scale the eigenvector values to 0-1 to match the igraph values.
  # styler: off goddammit I know what I'm doing
  nodeinfo$eigen     <- nodeinfo$eigen     / max(nodeinfo$eigen)
  nodeinfo$centr_clo <- nodeinfo$centr_clo / max(nodeinfo$centr_clo)
  # styler: on

  output_list <-
    list(
      nodeinfo,
      ntwk_directed,
      ntwk_sna$gal$bipartite,
      ntwk_weighted,
      ntwk_connected
    )
  names(output_list) <- ntwk_info_names
  return(output_list)
}
