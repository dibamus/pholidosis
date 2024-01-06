#' graphEditDist
#'
#' Graph edit distance, number of topological changes between two planar
#' networks with mostly homologous vertices. This does not measure differences
#' in vertex or edge attributes (i.e., edge weight differences).
#'
#' This function takes two graphs and measures edit distance between them,
#' returning
#' @import stringr tidyverse
#' @importFrom igraph V E graph_from_edgelist as_edgelist shortest_paths
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
#' dist <- graphEditDist(simpleGs$g1,simplegs$g2)
#' dist
#'
#' @export
graphEditDist <- function(g1,g2){

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
        x <- shortest_paths(graph = g3,
                            from = which(V(g3)$name == df[i,1]),
                            to = which(V(g3)$name == df[i,2]),
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
    df$uniqueVs <- apply(df[,1:2], MARGIN = 1, function(x){any(x %in%uV$g1)})

    #for unmatched edges: can you find a way between the nodes via the unmatched
    # edges in the comparison graph?
    df$altpath <- NA
    df$altpath[!df$matched & !df$uniqueVs] <- spa(df[(!df$matched & !df$uniqueVs),1:2],
                                                  cleangraph(difference(comparisongraph, graph)),
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
      sp <- suppressWarnings({ #the shortest_paths function will likely find
        # cases where there is no connection between the two vertices. This is
        # a feature, (we want to know if there is no path), so I elect to
        # suppress the following warning message, which tells the user when no
        # path can be found:
        # "In shortest_paths(graph = g,
        # from = which(V(g)$name == as.character(e[1])),  :
        # At core/paths/unweighted.c:368 : Couldn't reach some vertices."

        shortest_paths(graph = g,
                       from = which(V(g)$name == as.character(e[1])),
                       to = which(V(g)$name == as.character(e[2])),
                       output = "vpath")$vpath[[1]]
      })
      #if shortest_paths can't find a connection, sp is character(0)
      #
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

  #Make a graph of all the problem areas minus the unresolved edges.

  #combine problem areas into a single edgelist (the perimeter)
  allprob <- rbind(
    problemareas$g1,
    problemareas$g2)

  #find which edges are unresolved so they can be excluded from the perimeter
  unrs <- rbind(g1.df[which(g1.df$unresolved),],
                g2.df[which(g2.df$unresolved),])
  unrs$graph <- c(rep("g1", times = length(which(g1.df$unresolved))),
                  rep("g2", times = length(which(g2.df$unresolved)))
                  )
  combined <- NA

  if(!all(is.na(unrs))){ #remove those unwanted edges from the edgelist
    combined <- allprob[is.na(cgraph(allprob, unrs[,1:2])),]
  }
  problemPolys <- NULL

  if(!all(is.na(combined))){
    #make this edgelist into a graph
    #delete any duplicated edges using "simplify()"
    problemPolys <- simplify(graph_from_edgelist(as.matrix(combined), directed = F))
    if(length(problemPolys)!=0){
      # if an edge not in problemPolys can be replaced with a path in problempolys
      # (using spa), then it has a substitute in the other graph
      # not necessarily due to an edge swap, it can be due to simply adding an edge
      g1.df$topochange[g1.df$unresolved] <- spa(g1.df[g1.df$unresolved,],
                                                problemPolys,
                                                uniqueV = NULL)
      g1.df$unresolved[!is.na(g1.df$topochange)] <- FALSE

      g2.df$topochange[g2.df$unresolved] <- spa(g2.df[g2.df$unresolved,],
                                                problemPolys,
                                                uniqueV = NULL)
      g2.df$unresolved[!is.na(g2.df$topochange)] <- FALSE
    }
  }

  #Initialize a list of topo changes
  tc <- list(swaps = 0, other = 0)

  if(!is.null(problemPolys)){
    sharedcycles <- findCycles(problemPolys) # find cycles in problempolys

    if(length(sharedcycles)==0){
      tc$other = dim(unrs)[1] # if no cycles, no swaps, so all the topo changes are "other"
    }

    #NOTE: I am not providing a full implementation of this now, just an estimator
    # the code below estimates which edges are toposwaps -
    #it is not a complete solution but should work for most cases now
    else{

      rmCy <- sapply(sharedcycles, function(x){ # find cycles that do not contain other cycles
        sum(sapply(sharedcycles, function(y){all(y %in% x)}))
      }) <= 2 # every cycle should only contain all the elements of itself
      sharedcycles <- sharedcycles[rmCy]

      unrs$cycles <- NA
      unrs$potswap <- NA

      for(i in 1:dim(unrs)[1]){
        e <- unrs[i,1:2]
        incy <- sapply(sharedcycles, function(x){all(e %in% names(x))})
        unrs$cycles[i] <- list(incy)
      }
      #matrix - rows are the edges in question, columns are the cycles

      #NOTE: this does not account for edges that may be a part of more than one cycle,
      # but for now it should be fine

      cyclematrix <- matrix(unlist(unrs$cycles), nrow = dim(unrs)[1], byrow = TRUE)
      for( k in 1:dim(unrs)[1]){
        if(unrs$graph[k] == "g1"){gind =1}
        else(gind = -1)
        cyclematrix[k,][which(cyclematrix[k,]==1)] <-gind}
      cyclematrix <- cyclematrix[,
                                 order(sapply(1:dim(cyclematrix)[2],
                                              function(x){sum(cyclematrix[,x] == 0)}))]
      #At this point, some rows in cyclematrix will have several entries
      #so which cycles should we assign those edges to?

      cmcheck <- matrix(NA, nrow = dim(cyclematrix)[1], ncol= dim(cyclematrix)[2])

      for(i in 1:dim(cyclematrix)[2]){
        for(j in 1:dim(cyclematrix)[1]){
          if(cyclematrix[j,i] == 0){}
          else{
            value <- cyclematrix[j,i]
            match <- which(cyclematrix[,i] == value*-1)[1]
            if(is.na(match)){}
            else{
              cmcheck[j,i] <- match
              cmcheck[match, i] <- j
            }
          }
        }
      }

      cmmatch <- !t(apply(cmcheck, MARGIN = 1, function(x){!is.na(x) *!duplicated(x)}))

      countmatched <- apply(cmmatch, MARGIN = 1, sum)

      if(any(countmatched>1)){
        print("Warning: uncertain subgraph swap assignments - topo changes may be overcounted")
      }
      cyclematrix <- cyclematrix * cmmatch

      cycleresults <- data.frame(cycle = paste0("c",1:dim(cyclematrix)[2]))
      cycleresults$unpartnered <- abs(apply(cyclematrix,
                                            MARGIN = 2, sum))
      cycleresults$partnerpairs <- apply(cyclematrix,
                                         MARGIN = 2,
                                         FUN = function(x){
                                           min(c(sum(x == -1),
                                                 sum(x == 1)))
                                         })
      tc$swaps <- sum(cycleresults$partnerpairs)
      tc$other <- sum(cycleresults$unpartnered)}


  }

    # how to avoid double-counting when a cycle appears in more than one sharedcycle?
    # total count for outputs must equal the number of unmatched edges
    # start scanning the smallest sharedcycles, then move out to the larger ones
    # don't overwrite.
    # the return value is
    #list(swaps= a, other = b), where a + b = # unmatched edges from unrs

  # Tally changes
  nodeChanges <- sum(sapply(uniquevertices,length))
  topoSwaps <- tc$swaps
  newEdges <- tc$other

  return(list(
    g1.df = g1.df,
    g2.df = g2.df,
    distances = list(node = nodeChanges, swap = topoSwaps, edge = newEdges)
  ))

}
