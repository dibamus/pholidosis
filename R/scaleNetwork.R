#' scaleNetwork
#'
#' This loads a dataframe (df) of scale relationships as an igraph object
#'
#' See excel_to_network for importing networks from excel sheets, lizard_setup
#' for handling the assignment of vertex attributes, and verifyMatrix for
#' a system handling the individual problems with improperly formatted dataframes
#' or dataframes with bad (non-numeric) entries.
#'
#' If you use the 1-2-3 unfused-partially fused-fused numeric scale in your
#' matrix, lizard_setup will change your edge weights to 1-1.5-2 to make scale
#' subtractions and scale fusions numerically equivalent for graphEditDistance
#' measurement.
#'
#' @import stringr tidyverse
#' @importFrom igraph graph_from_adjacency_matrix E
#' @param df A data frame, the scale table.
#' @param checkAsymmetry A logical scalar, if TRUE, the function checks whether
#' df is a symmetric matrix
#' @param verbose A logical scalar. If TRUE, the function prints errors when they occur.
#' @param setup_fun A setup funtion th help plot the netowrk. See the "lizard_setup" function for an example
#' @return An igraph object, the scale network.
#'
#' @examples
#' library(readxl)
#' filepath <- system.file("extdata", "DibamidaeDemo.xlsx", package = "pholidosis")
#' anelytropsis.adj <- read_xlsx(filepath, sheet = 1, col_names = TRUE)
#' anelytropsis.net <- scaleNetwork(anelytropsis.adj)
#'
#' @export

scaleNetwork <-  function(df, checkAsymmetry = FALSE, verbose = FALSE, setup_fun = lizard_setup){
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

      if(length(V(graph)) != (3*length(E(graph)))-6){

        print("graph is not spherical: E != 3V-6")
        print("loading graph for visualization only - NOT suitable for comparisons")
      }

      #update the edge weights - they should range from 1-2, not 1-3
	    E(graph)$weight <- sapply(E(graph)$weight, FUN = function(x){(x + 1)/2})

      #apply setup function to the graph to produce additional vertex and edge attributes
      graph <- setup_fun(graph)

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
