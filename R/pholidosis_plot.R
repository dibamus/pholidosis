#' pholidosis_plot
#'
#' Provides a quick way to plot a readable pholidosis network.
#'
#' This function uses the ggraph package to estimate a good layout for a
#' pholidosis network
#' different edges, and different edge weights.
#' @import ggraph
#' @import tidygraph
#' @importFrom igraph delete_vertices
#' @param g An igraph object: the pholidosis network you want to plot
#' @param title A character vector: the title for the graph
#' $param layout a layout function
#' @return a ggplot plot of the pholidosis network
#' @examples
#' pth <- system.file("extdata", "DibamidaeDemo.xlsx", package = "pholidosis")
#' net <- excel_to_network(pth)
#' pholidosis_plot(net[[1]], title = "Anelytropsis papillosus pholidosis network")
#' @export
pholidosis_plot <- function(g, title = NULL, layout = 'stress', ...){ #can pass other arguments for plotting
  if(any(V(g)$unplotted)){
    g <- delete_vertices(g, which(V(g)$unplotted))
  }
  ggraph(g, layout = layout) +
    ggtitle(title) +
    geom_edge_link(aes(edge_width = 0.5+ (weight/2)),linetype = "solid", alpha = 1) +
    geom_node_point(aes(
      colour = vertextype), size = 5) +
    suppressWarnings(geom_node_text(aes(label = gsub("[^0-9]","",name)))) +
    scale_edge_width_identity() +
    theme_graph()
}
