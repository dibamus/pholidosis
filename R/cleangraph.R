#' Clean a graph
#' Tidy up a graph that's been spit out of an automated process.
#' @param g An igraph object.
#' @importFrom igraph as_edgelist graph_from_edgelist
#' @return An igraph object, constructed from the edgelist of g
#' @export.
cleangraph <- function(g) {as_edgelist(g) %>%
    paste0 %>%
    matrix(ncol =2) %>%
    graph_from_edgelist(directed = FALSE)}
