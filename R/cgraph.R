#' cgraph - compare graphs
#'
#' Check for similarities between graphs
#'
#' Which of the edges of g1 can be found in g2, and to which edges do they correspond?
#'
#' @param g1.E An n * 2 character matrix: the edgelist for g1.
#' @param g2.E An m * 2 character matrix: the edgelist for g2.
#' @return An integer vector of length n, one entry per row (edge) og g1.E.
#'  The entry for each index is equal to the index of the row in g2.E where
#'  the equivalent edge is found.
#'
#' @examples
#' data('simpleGs)
#' g1 <- as_edgelist(simpleGs$g1)
#' g2 <- as_edgelist(simpleGs$g2)
#'
#' comp <- cgraph(g1,g2)
#'
#' comp ## edge 1 of g1 corresponds to edge 1 of g2,
#' ## edge 2 of g1 corresponds to edge 2 of g2, etc,
#' ## edge 5 of g1 does not have a corresponding edge in g2
#'
#' @export
cgraph <- function(g1.E,g2.E){ # are the edges of g1 in g2?
  x <- c(1:dim(g1.E)[1])
  sapply(x, FUN = function(x){
    findedge(g1.E[x,],g2.E)
  })
}
