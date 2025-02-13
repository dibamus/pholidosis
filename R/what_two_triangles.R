#this function takes two vertices (that make an edge) in a graph
#and returns a pair of vertices that each of the original vertices connects to,
#therfore finding what 2 triangles in the graph that edge is a part of

#' what_two_triangles
#'
#' In a fully triangulated graph, any edge is part of 2 triangles.
#' For problematic edges, we want to know which 2 triangles those are.
#'
#' This function takes a graph and vertex names of an edge and returns
#' vertex names for the 2 vertices which complete the 2 adjacent triangles
#'
#' @importFrom igraph all_simple_paths
#' @param g An igraph object; the graph in which the target edge exists
#' @param v1 A string: the name of the first vertex of the target edge
#' @param v2 A string: the name of the second vertex of the target edge
#' @return A vector of two vertex names
#'
#' @examples
#' g <- graph_from_literal(A-B-C-D-A,A-C)
#' tri_tips<- what_two_triangles(g, "A","C")
#' tri_tips

what_two_triangles <- function(graph, v1, v2){
  paths <- all_simple_paths(graph, v1,v2)
  #return the paths of length 3
  p2 <- paths[which(sapply(paths,length)==3)]

  mx <- sapply(p2, names)

  return(mx[!(mx %in% c(v1,v2))])
}
