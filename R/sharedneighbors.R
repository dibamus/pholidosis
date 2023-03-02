#' Do vertices share neighbors?
#'
#' Find  all the vertices that both vertices in an edge connect to.
#' @param edge A character vector of length 2: an edge in a network.
#' @param g1.E An n * 2 character matrix: the edgelist for the network.
#' @return A character vector of all the vertices that both vertices of edge connect to

sharedneighbors <- function(edge,g1.E){
  #edge must be in the form c(v1,v2)
  #g1.E is a graph as an edgelist
  v1.n <- neighborlist(edge[1],g1.E)
  v2.n <- neighborlist(edge[2],g1.E)

  g1.sn <- intersect(v1.n,v2.n)

  if(length(g1.sn) < 2){
    return(c(NA,NA))
  }
  else{
    return(g1.sn)
  }

}
