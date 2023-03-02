clean_isolates <- function(g){
  require('igraph')
  #Find different subgraphs
  clu <- components(g, mode = "weak")
  if (clu$no == 1){
    return(g)
  }
  else{
    primary <- which(clu$csize == max(clu$csize))
    return(subgraph(g,which(clu$membership == primary)))
  }
}
