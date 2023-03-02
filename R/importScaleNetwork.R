#' This loads a dataframe (df) of scale relationships as an igraph object
#' @import igraph stringr tidyverse
#' @param df A data frame, the scale table.
#' @param checkAsymmetry A logical scalar, if TRUE, the function checks whether
#' df is a symmetric matrix
#' @param verbose A logical scalar. If TRUE, the function prints errors when they occur.
#' @return An igraph object, the scale network.

scaleNetwork <-  function(df, checkAsymmetry = FALSE, verbose = FALSE){
  mat <-as.matrix(df) # read in adjacency matrix file
  mat[is.na(mat)] <- 0 # set all NA links to 0

  sym <- TRUE #assume matrix is symmetric (or it doesn't need to be)

  if(checkAsymmetry){ #if you do want to check your work, this does it
    sym <- isSymmetric(mat)
  }

  #check if column and row names match
  if(any(row.names(mat) != colnames(mat))){ #if the row and column names don't match
    #tell the user which ones don't match

    if(verbose) {cat("One or more row names do not match the column names:\n")
    cat(row.names(mat)[which(row.names(mat) != colnames(mat))],"\n\n")}

    #then return
    return()
  }


  # check if matrix is empty:
  if(sum(mat) ==0) {
    make_empty_graph()
    print("graph is empty")
  }

  #if matrix is not empty
  else{
    if (sym) { # yes, it is symmetric - complete the import

      graph <- graph_from_adjacency_matrix(weighted = T, adjmatrix = mat, mode = "undirected")
      graph <- clean_isolates(graph) #get rid of any isolated vertices


      graph <- lizard_setup(graph)

      if(verbose){cat(" converted")}

      return(graph) #return the graph object
    }

    else { #the matrix is not symmetric - correct it
      if(verbose){cat("asymmetric matrix","\n",
          "Coordinates of asymmertic entries:","\n",
          which(mat != t(mat), arr.ind = TRUE),"\n")}
      }
  }
}
