#' Get just the largest fully-connected subgraph of a graph
#' @import igraph
#' @param g An igraph object.
#' @return An igraph object containing only the largest fully-connected subgraph of g
#' @export
clean_isolates <- function(g){
  #Find different subgraphs
  clu <- components(g, mode = "weak")
  if (clu$no == 1){
    return(g)
  }
  else{
    primary <- which(clu$csize == max(clu$csize))
    return(subgraph(g,which(clu$membership == primary)))
  }
}
