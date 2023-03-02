# Generate character matrix of edges from a list of graphs

# This funciton takes a list of scale graphs and produces a morphological data 
# matrix wherein each edge in the graph is a trait.

morphoMatrix <- function(tgs){
  #first, make a matrix of all unique edges in the dataset
  alledges <- function(tgs, f = matrix(nrow = 1, ncol = 2), i = 1){
    el <- as_edgelist(tgs[[i]])
    
    newedges <- which(is.na(cgraph(el, f)))
    f <- rbind(f, el[newedges,])
    
    if(i == length(tgs)){
      return(data.frame(f[-1,]))
    }
    else{
      alledges(tgs, f, i=i+1)
    }
  }
  
  #call that function, add a column to name each character  
  totalEdges <- alledges(tgs) %>% mutate(character = paste(X1,"+",X2))
    
  for(i in 1:length(tgs)){
    
    weights <- E(tgs[[i]])$weight
    edgelist <- as_edgelist(tgs[[i]])
    
    states <- apply(totalEdges[,1:2], MARGIN = 1, FUN = 
                       function(x){
                         index <- findedge(x, edgelist)
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
  
  rownames(charMat) <- names(tgs)
  
  return(charMat)
}
