#' Find an edge
#'
#' Does a given edge exist in an edgelist?
#' @param edge A character vector of length 2, the names of the vertices of the edge.
#' @param edgelist A n * 2 character matrix, an edgelist for a graph.
#' @return a scalar, either the index of the row of edge in edgelist, or NA if
#'    the edge cannot be found in the edgelist
#'@examples
#' data("simpleGs")
#' find_edge(c('B','D'),as_edgelist(simpleGs$g1)) ##neither the edge "B-D" nor
#' ## the edge "D-B" is present in g1
#' find_edge(c('B','D'),as_edgelist(simpleGs$g2) # the edge "B-D" (or the edge
#' ## "D-B") is the 5th edge in g1
#'
#' @export
find_edge <- function(edge,edgelist){ #does an edge exist in an edgelist?
  edge <- unlist(edge)

  index <- apply(edgelist, MARGIN = 1, FUN = function(x){setequal(edge,x)}) %>%
    which
  if(length(index) == 0){ #NO
    return(NA)
  }
  else{return(index)} # YES, here's where it is!
}
