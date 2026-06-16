#' Generate distance matrix from gl
#' @noMd
#' @import utils
#' @param gl A list of graphs.
#' @return An n x n x 4 matrix, where n is the number of graphs in gl
#' the first slice [,,1] contains the weight distance of shared edges
#' the second slice [,,2] contains the vertex distance
#' the third slice [,,3] contains the distance moved by "wandering" vertices
#'the fourth slice [,,4] contains the flip distance
#'
#' @examples
#' data('simpleGs')
#'
#' dmat <- net_dist_mat_2(simpleGs)
#'
#' dmat ##view the difference matrices
#' @export
#This function generates a distance matrix of distances between any two graphs in a list of graphs
net_dist_mat_2 <- function(gl){
  l <- length(gl)
  dmat <- array(data = NA, dim = c(l,l,4),
                dimnames = list(names(gl),
                                names(gl),
                                c("edge weight distance",
                                  "vertex distance",
                                  "wander distance",
                                  "flip distance")))
  pairs <- t(combn(l, 2)) #get matrix of all possible pairs of numbers from 1-l

  for (i in 1:dim(pairs)[1]){ #for each row in pairs matrix
    # measure distance between those nodes
    # place distances in correct spot (lower triangle) of distance matrix
    print(paste(names(gl)[pairs[i,2]],"vs",names(gl)[pairs[i,1]]))
    dmat[pairs[i,2],pairs[i,1],] <- ged(gl[[pairs[i,1]]], gl[[pairs[i,2]]])$distances
  }
  dmat
}
