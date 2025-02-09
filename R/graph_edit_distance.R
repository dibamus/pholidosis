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
graph_edit_distance <- function(g1,g2){

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
                                                  igraph::as_undirected(difference(comparisongraph, graph), mode = 'collapse'),
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



  #### BEGIN ALTERNATIVE APPROACH ####
  # This approach relies on a fully-triangulated graph
  # It detects cycles by creating a subgraph containing all of the vertices
  # that are involved in the unmatched edges.
  # Since both input graphs are fully triangulated, all vertices in the cycle
  # that surrounds swapped edges must be represented by either the ends of
  # edges in g1 or the ends of edges in g2.
  # this is stable because there are no "perimeters" to the scale network. The
  # network is spherical, so even if, for instance, SL1 & SL3 are touching,
  # in the other network there was an SL2-mouth connection, so the topology
  # is the same as an edge swap between all scales.

  unrs.V <- unique(c(unlist(g1.unresolved[,1:2]),unlist(g2.unresolved[,1:2])))

  g1.cycles <- subgraph(g1,match(unrs.V,V(g1)$name)) #get the cycles around unmatched edges
  g2.cycles <- subgraph(g2,match(unrs.V,V(g2)$name))

  g1.cycles <- delete_edges(g1.cycles, #remove the unresolved edges
                                   get_edge_ids(g1.cycles,
                                                as.matrix(g1.unresolved[,1:2]) %>%
                                                  t() %>% c())
                                   )
  g2.cycles <- delete_edges(g2.cycles,
                            get_edge_ids(g2.cycles,
                                         as.matrix(g2.unresolved[,1:2]) %>%
                                           t() %>% c())
  )

  consensus.cycles <- g1.cycles + g2.cycles

  cl <- find_cycles(consensus.cycles, minlength = 4) %>%
    lapply(FUN = function(x){x[-1]}) #remove the first vertex of each cycle (unnamed)

  # #tester
  # cl <- list(c(a = 1, b=2, c=3, d=4),
  #            c(e=5,f=6,g=7,h=8),
  #            c(a = 1, b=2, c=3, d=4,e=5,f=6,g=7,h=8),
  #            c(a = 1, b=2, c=3, d=4,e=5,f=6,g=7,h=8))

  cyclelengths <- data.frame(n = 1:length(cl), length = sapply(cl,length), matches = "none") %>%
    arrange(length)

  # There may be larger cycles in the graph, produced when 2 cycles touch.
  # scanning through these later will double-count edges, since they're part of
  # the small (true) cycle as well as the large (composite) one.
  # so let's get rid of any cycles that contain all of the vertices in another cycle

  if(length(cl) == 1){ #if there's only one cycle, don't even bother
    rmCy = FALSE
  }

  else{
    rmCy <- sapply(length(cl):2, function(x){ #starting from the longest cycle
      nm <- names(cl[[cyclelengths$n[x]]])

      # are any of the other cycles contained in it?
      any(sapply((x-1):1, function(y){#search through the smaller cycles
        if(x == y){
          return(FALSE) # cycle y is NOT contained in cycle x
        }
        snm <- names(cl[[cyclelengths$n[y]]])
        if(all(snm %in% nm)){
          return(TRUE) # cycle y IS contained in cycle x
        }
        else{return(FALSE)} # cycle y is NOT contained in cycle x
      }))
    })
    # rmCy is missing an entry for the smallest cycle
    rmCy <- append(rmCy,FALSE)
  }

  #and it's reversed relative to the order of cyclelengths
  # let's set that straight add the info about which cycles are composite to cyclelengths
  cyclelengths$composite <- rev(rmCy)

  #ok, let's get rid of these composite cycles
  cyclelengths <- cyclelengths[!cyclelengths$composite,]


  # Now I need to write a for loop that matches these cycles to edges from
  # g1.unresolved and g2.unresolved.
  # A cycle is cleared once all of its vertices are matched to a vertex from
  # one of those dfs.
  # the number of changes represented by each cycle is equal to the number of
  # edges from g1 (or g2) that are matched to it
   g1.unresolved$cycle <- NA
   g2.unresolved$cycle <- NA

  cyclelengths$matches <- sapply(1:dim(cyclelengths)[1],function(x){
    cycle <- cl[[x]]
    vn <- names(cycle)
    matched <- rep("", length(vn))

    for (i in 1:dim(g1.unresolved)[1]){
      if(all(g1.unresolved[i,1:2] %in% vn)){
        vn.remainder <- vn[!(vn %in% g1.unresolved[i,1:2])]
        matched[which(vn %in% g1.unresolved[i,1:2])] <- paste0("g1","-",i)
        g1.unresolved$cycle[i] <<- x
        #the match is in row i of g1.unresolved
        for (j in 1:dim(g2.unresolved)[1]){
          if(all(g2.unresolved[j,1:2] %in% vn.remainder)){
            vn.remainder <- vn.remainder[!(vn.remainder %in% g2.unresolved[j,1:2])]
            g2.unresolved$cycle[j] <<- x
            #the match is in row j of g2.unresolved
            matched[which(vn %in% g2.unresolved[j,1:2])] <- paste0("g2","-",j)
          }
        }
      }
    }
    return(list(matched))
  })

  # note that
  # 1) a vertex may be matched to more than one edge
  # 2) the returned list includes only the most recent vertices to be matched.

  #Add the cycle #s to the graph dfs
  g1.df[dimnames(g1.unresolved)[[1]],"topochange"] <- g1.unresolved$cycle
  g2.df[dimnames(g2.unresolved)[[1]],"topochange"] <- g2.unresolved$cycle

  #count the topo changes
  # the number of topo changes within a cycle is equal to the cycle length minus 3

  cyclelengths$topochanges <- cyclelengths$length - 3

  # Tally changes
  nodeChanges <- sum(sapply(uniquevertices,length))
  topoSwaps <- sum(cyclelengths$topochanges)
  newEdges <- 0

  return(list(
    g1.df = g1.df,
    g2.df = g2.df,
    distances = list(node = nodeChanges, swap = topoSwaps, edge = newEdges)
  ))
}
}
