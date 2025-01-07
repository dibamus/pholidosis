#' component_frequencies
#'
#' Vertex and Edge Consistency
#'
#' How often does each edge and each vertex appear in a dataset?
#'
#' @param charMat A character matrix created by the "morpho_matrix" function
#' @importFrom igraph as_edgelist
#' @importFrom stringr str_split
#' @return A list of two lists: "EdgeConsistency," the proportion of species in
#' which each edge is present, and VertexConsistency," the proportion of species
#' in which each vertex is present.
#'
#' @examples
#' data("simpleGs")
#' mat <- morpho_matrix(simpleGs)
#' freq <- component_frequencies(mat)
#' freq
#'
#' @export

component_frequencies <- function(charMat){
  require(stringr)
  #how consistently does each edge appear in the dataset?
  edgeCons <- apply(charMat, MARGIN = 2, FUN = function(x){1 -(length(which(is.na(x)))/dim(charMat)[1])})

  #how consistently does each vertex appear in the dataset?
  vertexNames <- function(edgelist){unique(unlist(
    sapply(edgelist,str_split, pattern = " \\+ ")))}

  MatVerts <- vertexNames(dimnames(charMat)[2][[1]])

  vertexPresences <- sapply(dimnames(charMat)[1][[1]], function(x){
    names <- vertexNames(names(charMat[x,][which(!is.na(charMat[x,]))]))
    sapply(MatVerts, function(x){x %in% names})
  }) %>% t

  vertexCons <- apply(vertexPresences, MARGIN = 2, FUN = function(x){
    length(which(x))/dim(vertexPresences)[1]
  })

  return(list("EdgeConsistency"= edgeCons,"VertexConsistency" = vertexCons))
}
