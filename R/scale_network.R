#' scale_network
#'
#' This loads a dataframe (df) of scale relationships as an igraph object
#'
#' See excel_to_network for importing networks from excel sheets, lizard_setup
#' for handling the assignment of vertex attributes, and verify_matrix for
#' a system handling the individual problems with improperly formatted dataframes
#' or dataframes with bad (non-numeric) entries.
#'
#' If you use the 1-2-3 unfused-partially fused-fused numeric scale in your
#' matrix, lizard_setup will change your edge weights to 1-1.5-2 to make scale
#' subtractions and scale fusions numerically equivalent for graph_exit_distance
#' measurement.
#'
#' @import stringr tidyverse
#' @importFrom igraph graph_from_adjacency_matrix E
#' @param df A data frame, the scale table.
#' @param checkAsymmetry A logical scalar, if TRUE, the function checks whether
#' df is a symmetric matrix
#' @param verbose A logical scalar. If TRUE, the function prints errors when they occur.
#' @param setup_fun A setup function th help plot the netowrk. See the "lizard_setup" function for an example
#' @return An igraph object: If df is valid according to verify_matrix,
#' the pholidosis network. If df is not a valid matrix, an empty graph.
#'
#' @examples
#' library(readxl)
#' filepath <- system.file("extdata", "DibamidaeDemo.xlsx", package = "pholidosis")
#' anelytropsis.adj <- read_xlsx(filepath, sheet = 1, col_names = TRUE)
#' anelytropsis.net <- scale_network(anelytropsis.adj)
#'
#' @export

scale_network <-  function(df, checkAsymmetry = FALSE, verbose = FALSE, setup_fun = lizard_setup){

  if(any(colnames(df) == "bad.input.matrix")){ # verify_matrix found a matrix error
    if(verbose){cat(" not converted")}
    return(empty_graph())
  }

  mat <-as.matrix(df) # read in adjacency matrix file
  mat[is.na(mat)] <- 0 # set all NA links to 0

  sym <- TRUE #assume matrix is symmetric (or it doesn't need to be)

  if(checkAsymmetry){ #if you do want to check your work, this does it
    sym <- isSymmetric(mat)
  }

  #check if column and row names match
  if(any(row.names(mat) != colnames(mat))){ #if the row and column names don't match
    #tell the user which ones don't match

    if(verbose) {warning("One or more row names do not match the column names:\n",
                         row.names(mat)[which(row.names(mat) != colnames(mat))],"\n")}

    #then return
    return()
  }


  # check if matrix is empty:
  if(sum(mat) ==0) {
    warning("graph is empty\n")
    return(empty_graph())

  }

  #if matrix is not empty
  else{
    if (sym) { # yes, it is symmetric - complete the import

      graph <- graph_from_adjacency_matrix(adjmatrix = mat, weighted = T, mode = "max")
      graph <- clean_isolates(graph) #get rid of any isolated vertices

      #update the edge weights - they should range from 1-2, not 1-3
	    E(graph)$weight <- sapply(E(graph)$weight, FUN = function(x){(x + 1)/2})

      #apply setup function to the graph to produce additional vertex and edge attributes
      graph <- setup_fun(graph)

      if(verbose){cat(" converted")}

      return(graph) #return the graph object
    }

    else { #the matrix is not symmetric - correct it
      if(verbose){warning("asymmetric matrix","\n",
          "Coordinates of asymmertic entries:","\n",
          which(mat != t(mat), arr.ind = TRUE),"\n")}
      }
  }
}
