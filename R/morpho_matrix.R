#' Generate a morphological character matrix from a list of graphs
#' @import tidyverse dplyr
#' @param gl A list of graphs
#' @param custom A boolean; whether the matrix contains values other than 1, 1.5, and 2
#' @return
#' A n*e matrix, where n is the number of graphs in gl,
#' and e is the total number of unique edges in gl.
#' An element, [n1,e1] is the weight of edge e1 in graph n1.
#'
#' @examples
#' data("simpleGs")
#' mat <- morpho_matrix(simpleGs)
#' mat
#'
#' @export

morpho_matrix <- function(gl, custom = FALSE){
  #first, make a matrix of all unique edges in the dataset
  alledges <- function(gl, f = matrix(nrow = 1, ncol = 2), i = 1){
    el <- as_edgelist(gl[[i]])

    newedges <- which(is.na(compare_graphs(el, f)))
    f <- rbind(f, el[newedges,])

    if(i == length(gl)){
      return(data.frame(f[-1,]))
    }
    else{
      alledges(gl, f, i=i+1)
    }
  }

  #call that function, add a column to name each character
  #`cur_data()` was deprecated in dplyr 1.1.0
  # use pick() instead
  totalEdges <- alledges(gl) %>% mutate(character = paste(cur_data()[[1]],"+",cur_data()[[2]]))

  for(i in 1:length(gl)){

    weights <- E(gl[[i]])$weight
    edgelist <- as_edgelist(gl[[i]])

    states <- apply(totalEdges[,1:2], MARGIN = 1, FUN =
                       function(x){
                         index <- find_edge(x, edgelist)
                         if(!is.na(index)){
                           index <- weights[index]
                         }
                         return(index)
                       })
    totalEdges[,i+3] <- states
  }

  # format this as a traditional character matrix
  charMat <- t(totalEdges[,-c(1:3)])

  colnames(charMat) <- totalEdges$character

  rownames(charMat) <- names(gl)

  if(custom == FALSE){
    charMat <- charMat*2-1
    charMat[which(is.na(charMat))] <- 0

  }
  return(charMat)
}
