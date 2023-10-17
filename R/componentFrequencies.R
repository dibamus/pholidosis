#Scale and Edge Consistency

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
