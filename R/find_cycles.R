#' Find cycles in a graph
#' find and list all cycles (rings of vertices) in a graph larger than a given
#' number of vertices.
#' @param g An igraph object.
#' @param minlength An integer; the length of the shortest desired cycle. Set by default to 5
#' @importFrom igraph neighbors all_simple_paths
#' @return A list of cycles as vertex paths through the graph g
#' @examples
#' find_cycles(simpleGs$g6)
#' find_cycles(simpleGs$g6, minlength = 4)
#' @export

find_cycles<- function(g, minlength = 5){
  Cycles <- NULL
  for(v1 in V(g)) {
    for(v2 in neighbors(g, v1, mode="all")) {
      Cycles <- c(Cycles,
                 lapply(all_simple_paths(g, v2,v1, mode="all"), function(p) c(v1,p)))
    }
  }
  LongCycles <- Cycles[which(sapply(Cycles, length) >= minlength)]
  uniqueCycles <- LongCycles[!duplicated(lapply(LongCycles,
                                                FUN = function(x){(sort(unique(x)))}))]
  return(uniqueCycles)
}
