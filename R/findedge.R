#' Find an edge
#'
#' Does a given edge exist in an edgelist?
#' @param edge A character vector of length 2, the names of the vertices of the edge.
#' @param edgelist A n * 2 character matrix, an edgelist for a graph.
#' @return a scalar, either the index of the row of edge in edgelist, or NA if
#'    the edge cannot be found in the edgelist
#' @export
findedge <- function(edge,edgelist){ #does an edge exist in an edgelist?
  edge <- unlist(edge)

  index <- apply(edgelist, MARGIN = 1, FUN = function(x){setequal(edge,x)}) %>%
    which
  if(length(index) == 0){ #NO
    return(NA)
  }
  else{return(index)} # YES, here's where it is!
}
