#Generate distance matrix from graphs
netDistMat <- function(graphs){
  l <- length(graphs)
  dmat <- array(data = NA, dim = c(l,l,3), 
                dimnames = list(names(graphs),
                                names(graphs),
                                c("weight difference of shared edges","number of unshared edges","full distance")))
  pairs <- combinations(l, 2) #get matrix of all possible pairs of numbers from 1-l
  
  for (i in 1:dim(pairs)[1]){ #for each row in pairs matrix
    # measure distance between those nodes
    # place distances in correct spot (lower triangle) of distance matrix
    #right now this only keeps track of the first 3 distance measures
    dmat[pairs[i,2],pairs[i,1],] <- pnet_distance(graphs[[pairs[i,1]]], graphs[[pairs[i,2]]])[1:3]
  }
  dmat
}
