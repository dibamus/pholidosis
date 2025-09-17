#' graph_edit_distance
#'
#' Graph edit distance, number of topological changes between two planar
#' networks with mostly homologous vertices. This does not measure differences
#' in vertex or edge attributes (i.e., edge weight differences).
#'
#' This function takes two graphs and measures edit distance between them,
#' returning
#' @import stringr tidyverse igraph
#' @param g1 An igraph object; the first graph.
#' @param g2 An igraph object; the second graph.
#' @param verbose logical; whether to print a data frame of matching cycles, helpful for debugging
#' @return A list containing
#'    1) g1.df - A data frame of topological changes to g1 and how they were resolved
#'    2) g2.df - A data frame of topological changes to g2 and how they were resolved
#'    3) distances - a list containing
#'        node - the number of un-shared vertices between g1 and g2
#'        swap - the number of "edge swaps" between g1 and g2
#'        edge - the number of non-"edge swap" unshared edges between g1 and g2
#'
#' @examples
#' data("simpleGs")
#' dist <- graph_edit_distance(simpleGs$g1,simplegs$g2)
#' dist
#'
#' @export
graph_edit_distance <- function(g1,g2, verbose = FALSE){

  ##### 1 - Generate Edgelists ####
  g1.E <- as_edgelist(g1)

  g2.E <- as_edgelist(g2)

  #Which vertices in g1 and g2 are not shared?
  uniquevertices <- list(g1 = setdiff(V(g1)$name,V(g2)$name),
                         g2 = setdiff(V(g2)$name,V(g1)$name))

  # remove an edge (e) from a graph, and then find the new shortest path between
  # those 2 vertices

  spa <- function(df,g,uniqueV){
    if(dim(df)[1] ==0 | length(g) ==0){
      return(NA)
    }

    l <- list()
    for(i in 1:dim(df)[1]){
      #here is where I should pare down g

      if(all(df[i,1:2] %in% V(g)$name)){
        #make sure to trim shared vertices out of g if indicated
        if(length(uniqueV)!=0){
          g3 <- delete_vertices(g, v = which(!(V(g)$name %in% c(df[i,1:2],uniqueV))))
        }
        else{
          g3 <- g
        }
        #and continue
        x <- suppressWarnings( # some vertices it tries may be unreachable, but that's ok
          shortest_paths(graph = g3,
                            from = which(V(g3)$name == df[i,1]),
                            to = which(V(g3)$name == df[i,2]),
                            output = "vpath")$vpath[[1]])
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
    df$matched <- !is.na(compare_graphs(E$g1,E$g2))
    df$uniqueVs <- apply(df[,1:2], MARGIN = 1, function(x){any(x %in%uV$g1)})

    #for unmatched edges: can you find a way between the nodes via the unmatched
    # edges in the comparison graph?
    df$altpath <- NA
    df$altpath[!df$matched & !df$uniqueVs] <- spa(df[(!df$matched & !df$uniqueVs),1:2],
                                                  # jan 6 - removed this line
                                                  #cleangraph(difference(comparisongraph, graph)),
                                                  as_undirected(difference(comparisongraph, graph), mode = 'collapse'),
                                                  uV$g2)

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

  if(!any(c(g1.df$unresolved,g2.df$unresolved))){ #no unresolved edges?
    #great, carry on
    return(list(
      g1.df = g1.df,
      g2.df = g2.df,
      distances = list(node = sum(sapply(uniquevertices,length)),
                       swap = 0,
                       edge = 0)
    ))
  }

  else{

  # cut the df to just the edges that remain different after vertex changes are accounted for
  g1.unresolved <- g1.df[g1.df$unresolved,]
  g2.unresolved <- g2.df[g2.df$unresolved,]

  unrs.V <- unique(c(unlist(g1.unresolved[,1:2]),unlist(g2.unresolved[,1:2])))

  g1.cycles <- subgraph(g1,match(unrs.V,V(g1)$name)) #get the cycles around unmatched edges
  g2.cycles <- subgraph(g2,match(unrs.V,V(g2)$name))

  #With those data, find the triangles surrounding the unresolved edges
  # using what_two_triangles() and find any systems of connected edges
  # that might be made up of unresolved edges.

  #step 1 - find the triangles
  g1.unresolved <- cbind(g1.unresolved, sapply(
    1:dim(g1.unresolved)[1],
    FUN = function(i) {
      what_two_triangles(g1.cycles, g1.unresolved$X1[i], g1.unresolved$X2[i])
    }
  )%>%t())

  #step 2 - find adjacent problem edges using get_sisteredges
  g1.unresolved <- cbind(g1.unresolved, get_sisteredges(g1.unresolved[,c(1,2,9,10)]))

  #step 3 - list out all of the vertices involved in a system of adjacent problem edges
  g1.unresolved$groupnodes <- lapply(1:dim(g1.unresolved)[1], function(x){
    g1.unresolved[g1.unresolved$sisters[[x]],c(1,2,9,10)] %>%
      unlist() %>%
      unique() %>% sort() %>% paste(collapse = "-")
  })
  g1.unresolved$groupnodes <- unlist(g1.unresolved$groupnodes)

  #Repeat for g2
  g2.unresolved <- cbind(g2.unresolved, sapply(
    1:dim(g2.unresolved)[1],
    FUN = function(i) {
      what_two_triangles(g2.cycles, g2.unresolved$X1[i], g2.unresolved$X2[i])
    }
  )%>%t())

  g2.unresolved <- cbind(g2.unresolved, get_sisteredges(g2.unresolved[,c(1,2,9,10)]))

  g2.unresolved$groupnodes <- lapply(1:dim(g2.unresolved)[1], function(x){
    g2.unresolved[g2.unresolved$sisters[[x]],c(1,2,9,10)] %>%
      unlist() %>%
      unique() %>% sort() %>% paste(collapse = "-")
  })
  g2.unresolved$groupnodes <- unlist(g2.unresolved$groupnodes)

  topo.summary <- data.frame(table(sort(unlist(g1.unresolved$groupnodes))),
                                       table(sort(unlist(g2.unresolved$groupnodes))))

  topo.summary[,1] <- as.character(topo.summary[,1])
  topo.summary[,3] <- as.character(topo.summary[,3])

  if(verbose) {
    print(topo.summary)
  }

  # do we find all of the same problem cycles and problem cycle frequencies in g1 and g2
  if(all(topo.summary[,1] == topo.summary[,3]) &&
     all(topo.summary[,2] == topo.summary[,4])){
    topochanges <- sum(table(sort(unlist(g1.unresolved$groupnodes))))
  }
  else{
    warning("Graphs are not topologically comparable; topo changes will be NA.")
    topochanges <- NA
  }

  #Add the cycle #s to the graph dfs
  g1.df[dimnames(g1.unresolved)[[1]],"topochange"] <- g1.unresolved$groupnodes
  g2.df[dimnames(g2.unresolved)[[1]],"topochange"] <- g2.unresolved$groupnodes

  # Tally changes
  nodeChanges <- sum(sapply(uniquevertices,length))
  #topoSwaps <- sum(cyclelengths$topochanges)
  topoSwaps <- topochanges
  newEdges <- 0

  return(list(
    g1.df = g1.df,
    g2.df = g2.df,
    distances = list(node = nodeChanges, swap = topoSwaps, edge = newEdges)
  ))
}
}
