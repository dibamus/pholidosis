#edge_utils

# tidy up a graph that's been spit out of an automated process
cleangraph <- function(g) {as_edgelist(g) %>%
    paste0 %>%
    matrix(nc =2) %>%
    graph_from_edgelist(directed = FALSE)}

findedge <- function(edge,edgelist){ #does an edge exist in an edgelist?
  edge <- unlist(edge)

  index <- apply(edgelist, MARGIN = 1, FUN = function(x){setequal(edge,x)}) %>%
    which
  if(length(index) == 0){ #NO
    return(NA)
  }
  else{return(index)} # YES, here's where it is!
}

cgraph <- function(g1.E,g2.E){ # are the edges of g1 in g2?
  x <- c(1:dim(g1.E)[1])
  sapply(x, FUN = function(x){
    findedge(g1.E[x,],g2.E)
  })
}

neighborlist <- function(v1,g){
  edges <- g[apply(g, MARGIN = 1, FUN = function(x){
    v1 %in% x }),]
  vertices <- unique(unlist(edges))
  vertices[-which(vertices == v1)]
}

sharedneighbors <- function(edge,g1.E){
  #edge must be in the form c(v1,v2)
  #g1.E is a graph as an edgelist
  v1.n <- neighborlist(edge[1],g1.E)
  v2.n <- neighborlist(edge[2],g1.E)

  g1.sn <- intersect(v1.n,v2.n)

  if(length(g1.sn) < 2){
    return(c(NA,NA))
  }
  else{
    return(g1.sn)
  }

}

# # Perimeter Detection Method ####
# # this detects which scales are on the edge of a scale arrangement,
# #
#
# # requires neighborlist
# edged <- function(v,g){ #detects whether nodes are on the perimeter
#   #get list of neighbors
#   ns <- neighborlist(v,g)
#
#   #in the graph, which edges belong only to v's neighbors?
#   hits <- sapply(1:dim(g)[1], function(x){
#     all(g[x,] %in% ns)
#   })
#
#   if(any(hits) == FALSE){ #the node is not surrounded by any immediate edges
#     perim <- TRUE
#   }
#   else{
#     #make a subgraph of only v's neigbors
#     sg <- g[hits,, drop = F]
#
#     # if the graph is cyclic, the maximum shortest path between two vertices will
#     # equal half or less than half of the total number of edges
#
#     # if the graph is linear, it means v cannot be encircled by its neighbors
#     # therefore, this will report it as FALSE, i.e., not at the perimeter
#     perim <- max(shortest.paths(graph_from_edgelist(sg))) > dim(sg)[1]/2
#   }
#
#   return(perim)
#
# }
#
# perimeter <- function(g){
#   V(g)$perimeter <- sapply(V(g)$name,
#                            function(x){
#                              edged(x,as_edgelist(g))
#                            })
#   return(g)
# }
#
# #find perimeter nodes
# isperimeter <- function(x,g){
#   per <- all(x %in% V(g)$name[which(V(g)$perimeter)])
#   if(per){
#     per <- !(sharedneighbors(x, as_edgelist(g)) %>% is.na %>% all)
#   }
#   return(per)
# }

# comparing two graphs


