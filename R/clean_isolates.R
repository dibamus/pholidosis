#' clean_isolates
#'
#' Get just the largest fully-connected subgraph of a graph
#'
#' this ensures a graph added to your dataset is fully connected (there is a
#' path from every vertex to every other vertex in the graph). It is helpful to
#' use this when importing graphs from adjacency matrix spreadsheets where there
#' may be unused columns and rows which create isolated vertices
#'
#' @importFrom igraph components subgraph
#' @param g An igraph object.
#' @return An igraph object containing only the largest fully-connected subgraph of g
#'
#' @examples
#' g <- graph_from_edgelist(matrix(c("B","C","C","D","D","A","B","D","E","E"), ncol = 2, byrow = TRUE))
#'
#' plot(g) ## notice that E is isolated
#'
#' g <- clean_isolates(g)
#'
#' plot(g) ## the isolated vertex has been removed
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
