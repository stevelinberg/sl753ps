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

plot_ntwk_vector_pairs <- function(ntwk_list) {
  df_tall <- ntwk_list$df %>%
    tidyr::pivot_longer(!name, names_to = "package", values_to = ntwk_list$col_name)

  ggplot(data = df_tall, aes(name, .data[[ntwk_list$col_name]], fill = package)) +
    geom_col(position = "dodge", color = "black") +
    coord_flip() +
    theme_minimal()
}
