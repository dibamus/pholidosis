#' Generate distance matrix from gl
#' @import utils
#' @param gl A list of graphs.
#' @return An n x n x 3 matrix, where n is the number of graphs in gl
#' the first slice [,,1] contains the weight difference of shared edges
#' the second slice [,,2] contains the number of edge-changes among shared scales
#' the third slice [,,3] contains the number of un-shared scales
#' @export
#This function generates a distance matrix of distances between any two graphs in a list of graphs
netDistMat <- function(gl){
  l <- length(gl)
  dmat <- array(data = NA, dim = c(l,l,3),
                dimnames = list(names(gl),
                                names(gl),
                                c("edge weight difference",
                                  "number of edge changes among shared scales",
                                  "difference in scale count")))
  pairs <- t(combn(l, 2)) #get matrix of all possible pairs of numbers from 1-l

  for (i in 1:dim(pairs)[1]){ #for each row in pairs matrix
    # measure distance between those nodes
    # place distances in correct spot (lower triangle) of distance matrix
    print(paste(names(gl)[pairs[i,2]],"vs",names(gl)[pairs[i,1]]))
    dmat[pairs[i,2],pairs[i,1],] <- pnet_distance(gl[[pairs[i,1]]], gl[[pairs[i,2]]])[1:3]
  }
  dmat
}
