#' lizardSetup
#'
#' Set up a graph with everything needed to plot out a comprehensible lizard.
#'
#' This assigns vertex properties based on scales and re-aligns edge properties
#' so that the edge weights c(1,2,3) become c(1,1.5,2)
#'
#' @importFrom igraph V E strength degree
#'
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
#' @examples
#' library(readxl)
#' filepath <- system.file("extdata", "DibamidaeDemo.xlsx", package = "pholidosis")
#' anelytropsis.adj <- read_excel(filepath, sheet = 1)
#'
#' # verify that all column and row names have matches
#' anelytropsis.adj <- verifyMatrix(anelytropsis.adj) # they do,
#' # and now there are row names
#'
#' anelytropsis.net <- scaleNetwork(anelytropsis.adj, lizard = FALSE)
#' vertex_attr(anelytropsis.net) #vertices only have a name attribute
#'
#' anelytropsis.net <- lizardSetup(anelytropsis.net) #now each vertex has name
#' # strength, degree, scaletype, position, and side attributes!
#'
#' @export
lizard_setup <- function(graph, firstscale = 'rostral'){

  V(graph)$str <- igraph::strength(graph) # access strength (sum of vertex weights) for each node
  V(graph)$deg <- igraph::degree(graph)

  E(graph)$weight <- sapply(E(graph)$weight, FUN = function(x){(x + 1)/2})

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
