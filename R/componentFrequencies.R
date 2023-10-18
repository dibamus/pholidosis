#' Vertex and Edge Consistency
#' How often does each edge and each vertex (scale) appear in a dataset?
#' @param charMat A character matrix created by the "morphoMatrix" function
#' @importFrom igraph as_edgelist
#' @importFrom stringr str_split
#' @return A list of two lists: "EdgeConsistency," the proportion of species in
#' which each edge is present, and ScaleConsistency," the proportion of species
#' in which each vertex (scale) is present.
#'
#' @examples
#' data("simpleGs)
#' mat <- morphoMatrix(simpleGs)
#' freq <- componentFrequencies(mat)
#' freq
#'
#' @export

componentFrequencies <- function(charMat){
  require(stringr)
  #how consistently does each edge appear in the dataset?
  edgeCons <- apply(charMat, MARGIN = 2, FUN = function(x){1 -(length(which(is.na(x)))/dim(charMat)[1])})

  #how consistently does each scale appear in the dataset?
  scaleNames <- function(edgelist){unique(unlist(
    sapply(edgelist,str_split, pattern = " \\+ ")))}

  MatScales <- scaleNames(dimnames(charMat)[2][[1]])

  ScalePresences <- sapply(dimnames(charMat)[1][[1]], function(x){
    names <- scaleNames(names(charMat[x,][which(!is.na(charMat[x,]))]))
    sapply(MatScales, function(x){x %in% names})
  }) %>% t

  scaleCons <- apply(ScalePresences, MARGIN = 2, FUN = function(x){
    length(which(x))/dim(ScalePresences)[1]
  })

  return(list("EdgeConsistency"= edgeCons,"ScaleConsistency" = scaleCons))
}
