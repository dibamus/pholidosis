#' List the neighboring vertices
#'
#' get a list of all vertices which v1 neighbors
#' @param v1 A character vector, a named vertex in a network
#' @param g An n * 2 character matrix: the edgelist for the network.
#' @return A character vector naming all the vertices neighboring v1.
#' @export

neighborlist <- function(v1,g){
  edges <- g[apply(g, MARGIN = 1, FUN = function(x){
    v1 %in% x }),]
  vertices <- unique(unlist(edges))
  vertices[-which(vertices == v1)]
}
