#' Set up a graph with everything needed to plot out a comprehensible lizard.
#' @param graph An igraph object, the scale network to be set up.
#' @param firstscale A character scalar, the name of a vertex in graph that should
#'    be used for anterior-posterior ordination. Usually, the most anterior scale
#'    on the lizar's head. Therefore, this is set by default to 'rostral'
#' @return An igraph object with the following vertex attributes:
#'    str - the strength of the vertex
#'    deg - the degree of the vertex
#'    scaletype - the general scale type the vertex corresponds to; the name, ignoring
#'        any ordination or enumeration
#'    pos - distance from the firstscale
#'    side - 0 for scales on the midline; 1 for Right side; -1 for left side
#' @export
lizard_setup <- function(graph, firstscale = 'rostral'){

  V(graph)$str <- igraph::strength(graph) # access strength (sum of vertex weights) for each node
  V(graph)$deg <- igraph::degree(graph)

  #store general scale name (name stripped of any numbers, R/L indicators, underscores)
  V(graph)$scaletype <- str_replace_all(names(V(graph)), "[0123456789_RL]","")


  #ORDINATION
  V(graph)$pos <- distances(graph, weights = NA)[,firstscale] #ordinate nodes (anterior (0) to posterior (+n))
  # set siding info: -1 for left
  V(graph)$side <- rep(0, length(V(graph)))
  V(graph)$side[grep("L_",V(graph)$name)] <- -1
  V(graph)$side[grep("R_",V(graph)$name)] <- 1

  return(graph)
}
