#' Title
#'
#' @param ig_nodes a list of igraph nodes from `ntwk_info_ig`
#' @param sna_nodes a list of statnet nodes from `ntwk_info_sna`
#' @param col_name the name of the column to create data pairs for
#'
#' @return a list of paired vectors of values, suitable to pass to `plot_ntwk_vector_pairs`
#' @export
#'
#' @examples
ntwk_vector_pairs <- function(ig_nodes, sna_nodes, col_name) {
  temp_df <- data.frame(
    name    = ig_nodes$name,
    igraph  =  ig_nodes[, col_name],
    sna     = sna_nodes[, col_name]
  )
  output_list <- list(col_name, temp_df)
  names(output_list) <- c("col_name", "df")
  return(output_list)
}

#' Title
#'
#' @param ntwk_list a list of vector pairs from `newk_vector_pairs`
#' @importFrom dplyr %>%
#'
#' @return
#' @export
#'
#' @examples
plot_ntwk_vector_pairs <- function(ntwk_list) {
  df_tall <- ntwk_list$df %>%
    tidyr::pivot_longer(!name, names_to = "package", values_to = ntwk_list$col_name)

  ggplot2::ggplot(data = df_tall, ggplot2::aes(name, .data[[ntwk_list$col_name]], fill = package)) +
    ggplot2::geom_col(position = "dodge", color = "black") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal()
}
