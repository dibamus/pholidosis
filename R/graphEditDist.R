#network comparison: venn apporach
graphEditDist <- function(g1,g2){
  
  ##### 1 - Generate Edgelists ####
  g1.E <- as_edgelist(g1)
  
  g2.E <- as_edgelist(g2)
  
  #Which vertices in g1 and g2 are not shared?
  uniquevertices <- list(g1 = setdiff(V(g1)$name,V(g2)$name),
                         g2 = setdiff(V(g2)$name,V(g1)$name))
  
  # remove an edge (e) from a graph, and then find the new shortest path between
  # those 2 vertices
  spa <- function(df,g){
    if(dim(df)[1] ==0 | length(g) ==0){
      return(NA)
    }
    
    l <- list()
    for(i in 1:dim(df)[1]){
      if(all(df[i,1:2] %in% V(g)$name)){
        x <- shortest_paths(graph = g,
                            from = which(V(g)$name == df[i,1]),
                            to = which(V(g)$name == df[i,2]),
                            output = "vpath")$vpath[[1]]
        vs <- as_ids(x)
        nv <- vs[which(!(vs %in% df[i,1:2]))]
        if(length(nv)==0){
          nv <- NA
        }
        l[[i]] <- nv
      }
      else{
        l[[i]] <- NA
      }
    }
    return(l)
  }
  
  #set up a data frame to compare the edges of a graph to the topology of another graph
  gAnalysis <- function(graph, comparisongraph){
    uV <- list(g1 = c(setdiff(V(graph)$name,V(comparisongraph)$name),"dummy name"),
               g2 = c(setdiff(V(comparisongraph)$name,V(graph)$name),"dummy name"))
    E <- list(g1 = as_edgelist(graph),
              g2 = as_edgelist(comparisongraph))
    
    df <- data.frame(as_edgelist(graph))
    df$matched <- !is.na(cgraph(E$g1,E$g2))
    
    #for unmatched edges: can you find a way between the nodes via the unmatched
    # edges in the comparison graph?
    df$altpath <- NA
    df$altpath[!df$matched] <- spa(df[!df$matched,1:2],
                                   cleangraph(difference(graph,comparisongraph)))
    
    df$uniqueVs <- apply(df[,1:2], MARGIN = 1, function(x){any(x %in%uV$g1)})
    df$altpath_allmissing <- sapply(df$altpath, FUN = function(x){all(x %in% uV$g2)})
    df$topochange <- NA
    #which edges of the graph cannot be resolved thorugh adding or subtracting vertices 
    # to/from the comparison graph?
    
    #an edge is unresolved if:
    df$unresolved <- !df$uniqueVs & #it does not contain unique vertices
      !df$altpath_allmissing & #there is no alternative path through only unique vertices
      !df$matched #it is not contained in the other graph
    return(df)
  }
  
  g1.df <- gAnalysis(g1,g2)
  g2.df <- gAnalysis(g2,g1)
  
  # cut the df to just the edges that remain different after vertex changes are accounted for
  g1.unresolved <- g1.df[g1.df$unresolved,]
  g2.unresolved <- g2.df[g2.df$unresolved,]
  
  g1.E.VofI <- data.frame(g1.E[apply(g1.E, MARGIN = 1, 
                                     FUN = function(x){any(x %in% unique(unlist(g1.unresolved[,1:2])))}),])
  g2.E.VofI <- data.frame(g2.E[apply(g2.E, MARGIN = 1, 
                                     FUN = function(x){any(x %in% unique(unlist(g2.unresolved[,1:2])))}),])
  
  ####
  
  #does an edge make up a single connection between two subgraphs?
  singleconnection <- function(e,el){ 
    el <- el[,1:2]
    i <- findedge(e,el)
    el <- el[-i,]
    
    #are all the scales in edge e still present in the new edgelist
    
    if(!all(unlist(e) %in% unlist(el))){#not all scales present
      #this is a single connection
      sp <- T
    }
    else{ 
      #make a graph from the new edgelist
      g<- graph_from_edgelist(as.matrix(el),directed = FALSE)
      
      #find the shortest path between the vertices of e in g 
      sp <- shortest_paths(graph = g,
                           from = which(V(g)$name == as.character(e[1])),
                           to = which(V(g)$name == as.character(e[2])),
                           output = "vpath")$vpath[[1]]
      #if shortest_paths can't find a connection, sp is character(0)
      #if sp has a length greater than 4, the original edge is not part of any triangles
      # - since it is not part of a triangle, it can't be involved in any tight 
      #   substitution groups
      if (length(sp)==0 | length(sp)>4){
        sp <- T
      }
      else(sp <- F)
    }
    
    return(sp)
  }
  
  # find the areas of a graph that include & surround unresolved edges 
  problemareas <- list(g1 = g1.E.VofI[!apply(g1.E.VofI, MARGIN = 1, FUN = singleconnection, el = g1.E.VofI),],
                       g2 = g2.E.VofI[!apply(g2.E.VofI, MARGIN = 1, FUN = singleconnection, el = g2.E.VofI),])
  
  # which edges are shared in the "problemareas" 
  # these should resolve to complete cycles (polygons) surrounding unresolved edges
  problemPolys <- graph_from_edgelist(as.matrix(intersect(problemareas$g1,problemareas$g2)),
                                      directed = F)
  
  if(length(problemPolys)!=0){
    # if an edge not in problemPolys can be replaced with a path in problempolys 
    # (using spa), then it has a substitute in the other graph
    g1.df$topochange[g1.df$unresolved] <- spa(g1.df[g1.df$unresolved,],problemPolys)
    g1.df$unresolved[!is.na(g1.df$topochange)] <- FALSE
    
    g2.df$topochange[g2.df$unresolved] <- spa(g2.df[g2.df$unresolved,],problemPolys)
    g2.df$unresolved[!is.na(g2.df$topochange)] <- FALSE
  }
  
  
  
  # Tally changes
  nodeChanges <- sum(sapply(uniquevertices,length))
  topoSwaps <- length(which(!is.na(g1.df$topochange)))
  newEdges <- sum(length(which(g1.df$unresolved)),length(which(g2.df$unresolved)))
  
  return(list(
    g1.df = g1.df,
    g2.df = g2.df,
    distances = list(node = nodeChanges, swap = topoSwaps, edge = newEdges)
  ))
  
}

#This function generates a distance matrix of distances between any two graphs in a list of graphs
netDistMat <- function(graphs){
  l <- length(graphs)
  dmat <- array(data = NA, dim = c(l,l,3), 
                dimnames = list(names(graphs),
                                names(graphs),
                                c("weight difference of shared edges",
                                  "number of edge changes among shared scales",
                                  "difference in scale count")))
  pairs <- combinations(l, 2) #get matrix of all possible pairs of numbers from 1-l
  
  for (i in 1:dim(pairs)[1]){ #for each row in pairs matrix
    # measure distance between those nodes
    # place distances in correct spot (lower triangle) of distance matrix
    print(paste(names(graphs)[pairs[i,2]],"vs",names(graphs)[pairs[i,1]]))
    dmat[pairs[i,2],pairs[i,1],] <- pnet_distance(graphs[[pairs[i,1]]], graphs[[pairs[i,2]]])[1:3] #right now this only keeps track of the first 3 distance measures
  }
  dmat
}

neighborsimilarity <- function(v){
  
  sapply(c(1:length(V.shared)), function(x){
    sum(gC[v,] & gC[V.shared[x],])}) %>%
    
    setNames(V.shared)
}

pnet_distance <- function(g1,g2, truncate = TRUE){
  ed <- graphEditDist(g1,g2)
  g1.E <- data.frame(as_edgelist(g1),weight = E(g1)$weight, indep.weight = (E(g1)$weight -1)/2)
  g2.E <- data.frame(as_edgelist(g2),weight = E(g2)$weight, indep.weight = (E(g2)$weight -1)/2)
  
  g1.E$match <- !is.na(cgraph(g1.E[,1:2],g2.E[,1:2]))
  g2.E$match <- !is.na(cgraph(g2.E[,1:2],g1.E[,1:2]))
  
  g1.E$indep.weight[g1.E$match] <- 0
  g2.E$indep.weight[g2.E$match] <- 0
  
  g.U <- g1.E[g1.E$match,]
  
  g.U$g2weight <- apply(g.U[,1:2], 
                        MARGIN = 1, 
                        function(x){
                          E(g2)$weight[findedge(x, g2.E[,1:2])]
                        })
  #make column of weight differences of shared edges
  # a change of weight = 1 to weight = 3 (unfused to fully fused) counts as 1 change
  # a change of weight = 1 to weight = 2 (unfused to semi-fused) counts as .5 change
  g.U$wDiff <- (g.U$weight - g.U$g2weight)/2
  
  distances <- c(w = sum(g.U$wDiff) + sum(g1.E$indep.weight) + sum(g2.E$indep.weight),
                 e = ed$distances$swap + ed$distances$edge,
                 f = ed$distances$node)
  names(distances) <- c("weight difference of shared edges",
                        "number of edge changes among shared scales",
                        "difference in scale count")
  if(truncate){
    return(distances)
  }
  else{
    return(list(
      topo = ed,
      measure = distances
    ))
  }
  
}