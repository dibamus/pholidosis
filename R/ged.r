#' ged
#'
#' Graph edit distance, number of topological changes between two planar
#' networks with mostly homologous vertices. This measures vertex insertion/deletion
#' vertex fusion/fission, and topological changes between the subgrpahs of
#' g1 and g2 which share vertices
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
#'      weight distance - difference in weights of all shared edges in g1 and g2
#'      vertex distance- distance in vertices (number of unshared edges
#'      attached to unshared vertices) between g1 and g2
#'      wanderer distance - distance traveled by all wandering vertices
#'      flip distance - minimum edge flips among induced subgraphs
#'
#'
#' @examples
#' data("simpleGs")
#' dist <- graph_edit_distance(simpleGs$g1,simplegs$g2)
#' dist
#'
#' @export
#'

ged <- function(g1,g2, verbose = FALSE){
  #### basic checks ####
  # Make graphs directed
  if(any(c(is_directed(g1),is_directed(g2)))){
    message("One or both graphs have directed edges. Converting to undirected for measurement.")
    g1 <- as_undirected(g1)
    g2 <- as_undirected(g2)
  }

  #are graphs maximaly planar?
  mp <- function(g){
    length(E(g1)) == 3*length(V(g))-6
  }
  if(!mp(g1)){warning("g1 is not maximally planar. Edit distance may be incorrectly estimated")}
  if(!mp(g2)){warning("g2 is not maximally planar. Edit distance may be incorrectly estimated")}



  #### STEP 1 - Vertex changes ####

  ## 1.A identify fusions/fissions among shared edges
  g1.E <- data.frame(as_edgelist(g1),weight = E(g1)$weight, indep.weight = (E(g1)$weight -1))
  g2.E <- data.frame(as_edgelist(g2),weight = E(g2)$weight, indep.weight = (E(g2)$weight -1))


  #are the edges of each graph matched in the other?
  g1.E$match <- !is.na(compare_graphs(g1.E[,1:2],g2.E[,1:2]))
  g2.E$match <- !is.na(compare_graphs(g2.E[,1:2],g1.E[,1:2]))

  #get union of G1 & G2 edges
  g.U <- g1.E[g1.E$match,]

  #what is the weight of each edge in g2
  g.U$g2weight <- apply(g.U[,1:2],
                        MARGIN = 1,
                        function(x){
                          E(g2)$weight[find_edge(x, g2.E[,1:2])]
                        })

  g.U$diff <- abs(g.U$weight - g.U$g2weight)

  #Since full fusions are given edge weight 2
  #and half-fusions are given edge weight 3
  #the difference of 2 is a full fusion/fission
  #right now, we are working with a model in which the fusion/fission cost
  #is equal to an edge addition, which is equivalent to 4 edge flips
  fusion.fission.cost = 4

  # so 1 unit of fusion/fission per edge weight difference of 2
  g.U$weight.cost = g.U$diff * (fusion.fission.cost/2)

  # Store edge weight distance
  weight.distance = sum(g.U$weight.cost)


  ## 1.B identify un-shared vertices
  shared.V <- intersect(names(V(g1)), names(V(g2)))

  #get vectors of degrees of all unshared vertices
  Dg1 <- degree(g1)[!(names(V(g1)) %in% shared.V)]
  Dg2 <- degree(g2)[!(names(V(g2)) %in% shared.V)]

  #quantify the edge changes
  quantify.vertex <- function(vector){
    if(length(vector == 0)){
      return(vector)
    }
    else{ #for all vertexes of degree 4 or more, the cost is 4
      vector[vector>3] <- 4
      return(vector)
    }
  }
  #combine quantified vectors for unshared vertices
  quant.verts <- c(quantify.vertex(Dg1), quantify.vertex(Dg2))

  vertex.distance <- sum(quant.verts)

  ## 1.C cleanup: make edge tables

  #Which vertices in g1 and g2 are not shared?
  uniquevertices <- list(g1 = setdiff(V(g1)$name,V(g2)$name),
                         g2 = setdiff(V(g2)$name,V(g1)$name))

  # spa function
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

    #Which vertices do not share edges between the two graphs?
    df$wanderingvertex <- NA
    df$wandercycle <- NA
    df$wanderlength <- NA
    df$wander_resolutions <- NA

    #which unmatched vertices are not in matched vertices?
    #unmatched vertices are vertices corresponding with unmatched edges
    #that are not themselves unique to the graph
    unmatchedvertices <-  unique(unlist(df[!df$matched & !df$uniqueVs,c("X1","X2")]))
    wanderingvertices <- unmatchedvertices[!unmatchedvertices %in%
                                             unique(unlist(df[df$matched,c("X1","X2")]))]
    for(i in wanderingvertices){
      wandind <- which(i == df[,c("X1","X2")], arr.ind = TRUE)
      df$wanderingvertex[wandind[,1]] <- i
    }

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

  wanderer.distances = 0
  flip.distances = 0

  if(any(c(g1.df$unresolved,g2.df$unresolved))){ #any unresolved edges?

    #### WHERE ARE THE UNRESOLVED EDGES? ####
    # cut the df to just the edges that remain different after vertex changes are accounted for
    g1.unresolved <- g1.df[g1.df$unresolved,]
    g2.unresolved <- g2.df[g2.df$unresolved,]

    #### WHERE ARE THE EDGES RESOLVED BY UNSHARED VERTICES? ####
    # which edges in each graph are substituted by multiple edges through unique vertices in the other?
    g1.replacementedges <- g1.df[g1.df$altpath_allmissing,]
    g2.replacementedges <- g2.df[g2.df$altpath_allmissing,]


    #### STEP 2 - Identify wandering vertices ####

    if(any(!is.na(g1.df$wanderingvertex))){
      ## 2.A begin only if wandering vertices have been identified already
      # what are the wandering vertices?
      wanderers <- unique(na.exclude(g1.df$wanderingvertex))

      ## 2.B Add wandering vertices from g2 into g1 as "vertex-wanderer"
      #new graph is g1.extrawand

      #add wandering vertex into expanded graph
      g1.extrawand <- add_vertices(g1,length(wanderers))
      #give it a new name
      V(g1.extrawand)$name[is.na(V(g1.extrawand)$name)] <- paste0(wanderers,"+wanderer")

      #find edges for wanderer in g2
      wandererlist <- as.list(wanderers)

      #add all edges
      for(i in wandererlist){
        v_name <- paste0(i,"+wanderer") #match name given above
        #find edges in g2
        alt_edges <- as.matrix(g2.df[which(g2.df$wanderingvertex == i),c("X1","X2")])
        alt_edges[alt_edges == i] <- v_name
        alt_edges <- matrix(match(alt_edges, V(g1.extrawand)$name), ncol =2)

        #add_edges wants a vertex sequence, not a matrix
        #since matrices are read column-first, the matrix should be flipped- t()
        g1.extrawand <- add_edges(g1.extrawand, t(alt_edges))
      }

      #fix weights, make graph undirected

      E(g1.extrawand)$weight[is.na(E(g1.extrawand)$weight)]<-1
      g1.extrawand <- as_undirected(g1.extrawand)

      ## 2.C Find shortest cycle containing both vertex and vertex-wanderer

      for(i in wandererlist){
        v_name <- paste0(i,"+wanderer")
        wandpath <- all_shortest_paths(g1.extrawand,
                                   from = which(V(g1.extrawand)$name  == i),
                                   to = which(V(g1.extrawand)$name == v_name),
                                   mode = "all")

        #for each of these paths, find the cycle containing that path

        paths <- wandpath$vpaths

        surroundingcycles <- data.frame(cycle = NA,
                                        resolutions = NA,
                                        n_resolutions = NA,
                                        wander_length = NA)

        for(k in paths){
          #list just the vertices between the two wanderer positions
          intermediate_vs <- k[c(2:(length(k)-1))]

          #get all neighboring vertices
          neigh <- sapply(intermediate_vs, function(x){
            neighbors(g1.extrawand, V(g1.extrawand)[V(g1.extrawand) == x])
          })

          #get all neighbors of these vertices
          neighbors_only <- unique(unlist(neigh))[!(unique(unlist(neigh)) %in% intermediate_vs)]

          #make graph of just neighbors
          neighbor_cycle <- induced.subgraph(g1.extrawand, neighbors_only)

          #all shortest paths when these vertices are gone
          alt_paths <- suppressWarnings(all_shortest_paths(neighbor_cycle,
                                          from = which(V(neighbor_cycle)$name  == i),
                                          to = which(V(neighbor_cycle)$name == v_name),
                                          mode = "all"))

          #if no alternative paths were found
          #(not possible in fully triangulated graph, but possible in test graphs)
          #
          if(length(alt_paths$vpaths) == 0){
            #do not add any data to surroundingcycles
          }

          else{
            #there should be 1 or 2 alt paths, with a minimal length
            min.altpathlength <- length(alt_paths$vpaths[[1]])-2

            #look for flipped edges between shortest path and altpath
            for(y in alt_paths$vpaths){
              #get all vertex names involved
              allintvert <- unique(c(V(neighbor_cycle)$name[unlist(y)],
                                     V(g1.extrawand)$name[intermediate_vs]))
              #check for unresolved edges in g2 that span these vertices
              #also check for edges in g1 that span these vertices
              #(ignore edges assoc. with wanderers)
              candidate_spanners <- rbind(g2.unresolved[which(is.na(g2.unresolved$wanderingvertex)),
                                                  c("X1","X2")] %>% as.matrix(,ncol = 2),
                                          g1.unresolved[which(is.na(g1.unresolved$wanderingvertex)),
                                                        c("X1","X2")] %>% as.matrix(,ncol = 2))



              #get a matrix of unresolved edges (vertex-vertex) that span these vertices
              spans <- candidate_spanners[apply(candidate_spanners, MARGIN = 1,
                                                function(x){all(x %in% allintvert)}),
                                          , #include full subset of columns
                                          drop = FALSE] #keep matrix format
              #write out a string of spans
              spans_string <- apply(spans,
                                    MARGIN = 1,
                                    paste0, collapse = "-") %>% paste0(collapse = "&")

              #count up the number of unresolved edges that this path would help resolve
              nspans <- dim(spans)[1]

              #add cycle to surroundingcycles
              surroundingcycles <- rbind(surroundingcycles,
                                         data.frame(cycle = paste0(sort(allintvert),
                                                                   collapse = "-"),
                                                    resolutions = spans_string,
                                                    n_resolutions = nspans,
                                                    #wander length is equal to the size of the cycle minus 2
                                                    # alt path and intermediate vertices exclude the wanderer
                                                    # so altpath + intermediate + 1 = cycle -1
                                                    wander_length = min.altpathlength + length(intermediate_vs) -1
                                         ))
            }
          }
        }

        #remove NA row from surrounding cycles df
        surroundingcycles <- surroundingcycles[-1,]

        #filter out duplicates
        surroundingcycles <- surroundingcycles[match(unique(surroundingcycles$cycle),
                                                     surroundingcycles$cycle),]

        #the best option minimizes wander length and maximizes resolving other cycles
        surroundingcycles$score <- surroundingcycles$wander_length - surroundingcycles$n_resolutions

        bestcycle <- surroundingcycles[surroundingcycles$score == min(surroundingcycles$score),]

        #mark soultion of cycle
        g1.unresolved[g1.unresolved$wanderingvertex %in% i, "wandercycle"] <- bestcycle$cycle
        g2.unresolved[g2.unresolved$wanderingvertex %in% i, "wandercycle"] <- bestcycle$cycle

        #note wander length (edit distance)
        g1.unresolved[g1.unresolved$wanderingvertex %in% i, "wanderlength"] <- bestcycle$wander_length
        g2.unresolved[g2.unresolved$wanderingvertex %in% i, "wanderlength"] <- bestcycle$wander_length

        #note resolutions
        g1.unresolved[g1.unresolved$wanderingvertex %in% i, "wander_resolutions"] <- bestcycle$resolutions
        g2.unresolved[g2.unresolved$wanderingvertex %in% i, "wander_resolutions"] <- bestcycle$resolutions

        #note that these edges are resolved
        g1.unresolved[g1.unresolved$wanderingvertex %in% i, "unresolved"] <- FALSE
        g2.unresolved[g2.unresolved$wanderingvertex %in% i, "unresolved"] <- FALSE

        #mark other edges which were resolved by this as resolved


        #figure out which edges are resolved by a cycle
        resolved_edges <- str_split(
          #wander resolutions are the same for g1 and g2
          unique(na.exclude(g1.unresolved$wander_resolutions)), pattern = "&") %>% unlist() %>% #split edges
          str_split(pattern = "-") %>% unlist() %>% #split vertices
          matrix(, ncol = 2, byrow = TRUE) #reconstitute

        # apply resolutions in g2 & g1
        for(k in 1:dim(resolved_edges)[2]){
          x <- resolved_edges[k,]
          edge_text  <- paste0(x, collapse = "-")
          mat1 <- as.matrix(g1.unresolved[,c("X1","X2")])
          mat2 <- as.matrix(g2.unresolved[,c("X1","X2")])

          mat1_matches <- apply(mat1, MARGIN = 1, function(y){
            all(x %in% y)})
          mat2_matches <- apply(mat2, MARGIN = 1, function(y){
            all(x %in% y)})

          if(any(mat1_matches)){
            g1.unresolved[mat1_matches,"wander_resolutions"]<- edge_text
            g1.unresolved[mat1_matches,"unresolved"]<- FALSE
          }
          if(any(mat2_matches)){
            g2.unresolved[mat2_matches,"wander_resolutions"]<- edge_text
            g2.unresolved[mat2_matches,"unresolved"]<- FALSE
          }
        }



      }

      #from the updated "unresolved" data frames
      #get the total length of wander paths
      wanderer.distances <- g1.unresolved[na.exclude(unique(g1.unresolved$wandercycle)) %>%
                                            match(g1.unresolved$wandercycle),
                                          "wanderlength"] %>% sum()
      #each edge crossing the wanderer path must be swapped and then swapped back
      #so multiply the wanderer distance by 2
      wanderer.distances <- wanderer.distances*2

      # update data frames
      g1.df[as.numeric(rownames(g1.unresolved)),colnames(g1.unresolved)] <- g1.unresolved
      g2.df[as.numeric(rownames(g2.unresolved)),colnames(g2.unresolved)] <- g2.unresolved
    }


    #### STEP 3 - Measure flip distance ####
    #if more unresolved edges, measure flip distance
    if(any(c(g1.df$unresolved,g2.df$unresolved))){
      #any unresolved edges?

      ## 3.A find all flipping cycles



      # This algorithm compares subgraphs of g1 & g2
      # in order to find shared cycles within which there have been edge swaps
      #
      # if g2 has a vertex that g1 lacks inserted into one of these cycles
      # then the cycle will not be found in both graphs.
      # so, we need to add an edge into g2 that completes that cycle
      # which has been interrupted by the insertion of an extra vertex in g2

      # and vice versa

      add_homologous_edge <- function(graph,edgematrix){
        #graph is a graph
        #edgematrix is an n x 2 matrix of edges between named (not numbered) vertices
        #substitute numbers (needed by igraph) for names (input to function)
        # transpose matrix; matrices are read by column first
        # so the transposed matrix looks like
        # v1 Vn  Vn3 ...
        # v2 Vn2 Vn4 ...

        edgematrix.numeric <- t(matrix(match(edgematrix, names(V(graph))), ncol =2))

        #add in those edges
        add_edges(graph, edgematrix.numeric)

      }

      g1.mod <- add_homologous_edge(g1, as.matrix(g2.replacementedges[,c("X1","X2")]))
      g2.mod <- add_homologous_edge(g2, as.matrix(g1.replacementedges[,c("X1","X2")]))


      #### FIND THE CYCLES OF EDGES SURROUNDING UNSHARED EDGES ####
      unrs.V <- unique(c(unlist(g1.unresolved[,1:2]),unlist(g2.unresolved[,1:2])))

      g1.cycles <- subgraph(g1.mod,match(unrs.V,V(g1)$name)) #get the cycles around unmatched edges
      g2.cycles <- subgraph(g2.mod,match(unrs.V,V(g2)$name))

      #With those data, find the triangles surrounding the unresolved edges
      # using what_two_triangles() and find any systems of connected edges
      # that might be made up of unresolved edges.

      #step 1 - find the triangles
      # NOTE: this may fail on directed graphs.
      g1.unresolved <- cbind(g1.unresolved, sapply(
        1:dim(g1.unresolved)[1],
        FUN = function(i) {
          what_two_triangles(g1.cycles, g1.unresolved$X1[i], g1.unresolved$X2[i])
        }
      )%>%t())

      #these are useful columns for get_sisteredges
      sist_cols <- c("X1","X2","1","2")

      #step 2 - find adjacent problem edges using get_sisteredges
      g1.unresolved <- cbind(g1.unresolved, get_sisteredges(g1.unresolved[,sist_cols])) # HARD CODED INDICES NEED TO CHANGE

      #step 3 - list out all of the vertices involved in a system of adjacent problem edges
      g1.unresolved$groupnodes <- lapply(1:dim(g1.unresolved)[1], function(x){
        g1.unresolved[g1.unresolved$sisters[[x]],sist_cols] %>% # HARD CODED INDICES NEED TO CHANGE

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

      g2.unresolved <- cbind(g2.unresolved, get_sisteredges(g2.unresolved[,sist_cols])) # HARD CODED INDICES NEED TO CHANGE


      g2.unresolved$groupnodes <- lapply(1:dim(g2.unresolved)[1], function(x){
        g2.unresolved[g2.unresolved$sisters[[x]],sist_cols] %>% # HARD CODED INDICES NEED TO CHANGE

          unlist() %>%
          unique() %>% sort() %>% paste(collapse = "-")
      })
      g2.unresolved$groupnodes <- unlist(g2.unresolved$groupnodes)

      topo.summary <- data.frame(table(sort(unlist(g1.unresolved$groupnodes))),
                                 table(sort(unlist(g2.unresolved$groupnodes))))

      topo.summary[,1] <- as.character(topo.summary[,1])
      topo.summary[,3] <- as.character(topo.summary[,3])

      cycles <- unlist(unique(c(topo.summary[,c(1,3)])))

      #recover a correct order of vertces in a cycle
      get_cycle <- function(verts){ #verts is a vector of vertrices
        v_ind <- match(verts, V(g1.mod)$name)

        #this needs to include edges that are matched via an altpath
        #so I use g1.mod as the template graph
        sg <- induced.subgraph(g1.mod,v_ind)

        internaledges <- c()

        for(i in 1:dim(g1.table)[1]){ #make vector of internal edge indices
          ed <- g1.table[i,1:2]
          internaledges <- c(internaledges,find_edge(ed,as_edgelist(sg)))
        }
        sg <- delete_edges(sg, internaledges) #remove those internal edges
        return(sg)
      }

      # make a vector of cycles wiht correclty ordered vertices
      cyc_ordered <- cycles #the correct orders will be the names

      for(i in 1:length(cycles)){
        cyc <- cycles[[i]]
        verts <- str_split(cyc,"-")[[1]]
        g1.table <- g1.unresolved[which(g1.unresolved$groupnodes == cyc),]

        ring <- get_cycle(verts)
        #find all paths from one vertex of an outer edge to its partner vertex
        ringpaths <- all_simple_paths(ring,
                                      from = match(as_edgelist(ring),names(V(ring)))[1],
                                      to = match(as_edgelist(ring),names(V(ring)))[2],)

        ringorder <- as.numeric(ringpaths[[which(sapply(ringpaths, length) !=2)]])

        ring_ordination <- V(ring)$name[ringorder]

        ring_string <- paste(ring_ordination, collapse = "-")

        names(cyc_ordered)[i]<- ring_string
      }


      #fix groupnodes column
      g1.unresolved$groupnodes <- names(cyc_ordered)[match(g1.unresolved$groupnodes,cyc_ordered)]
      g2.unresolved$groupnodes <- names(cyc_ordered)[match(g2.unresolved$groupnodes,cyc_ordered)]

      ## 3.B find flip distance for each cycle

      #set up a vector to store flip distances
      cyc.flip.dist <- setNames(rep(NA, length(cycles)), cycles)

      #find flip distance for each cycle
      for(i in 1:length(cycles)){
        cycle <- cycles[i]

        #ordinate vertices
        #split up the cycle into idifidual vertices
        ord <- setNames(1:length(str_split(cycle,"-")[[1]]),
                        str_split(cycle,"-")[[1]])

        # for small cycles, flip distance is known arithmetically
        # so for these cycles we can stop once we know the cycle size
        # this will probably account for the majority of flip cycles in a dataset
        if(length(ord) %in% c(4,5)){
          flip.dist <- length(ord)-3
          cyc.flip.dist[i] <- flip.dist

        }

        #for bigger cycles, we will use a brute-force breadth-first search
        else{
          #get matrix of edges from g1
          emg1 <- as.matrix(g1.unresolved[g1.unresolved$groupnodes == cycle,
                                          c("X1","X2") #find correct edges
          ])

          # ordinate edges from g1 into list
          start.g1edges <- lapply(1:(length(ord)-3),
                                  function(x){
                                    match(#match the edges in this cycle to numbers from the ordination
                                      emg1[x,], # select edge
                                      names(ord)) -1 #replace vertex names with numbers, starting from zero
                                  })

          emg2 <- as.matrix(g2.unresolved[g2.unresolved$groupnodes == cycle,
                                          c("X1","X2") #find correct edges
          ])

          # ordinate edges from g2 into list
          goal.g2edges <- lapply(1:(length(ord)-3),
                                 function(x){
                                   match(#match the edges in this cycle to numbers from the ordination
                                     emg2[x,], # select edge
                                     names(ord)) -1 #replace vertex names with numbers, starting from zero
                                 })

          #measure edge flips
          flip.dist <- flip_distance(start.g1edges,
                                     goal.g2edges,
                                     n = length(ord))

          cyc.flip.dist[i] <- flip.dist
        }



      }

      #check for Inf values

      inf.check <- is.infinite(cyc.flip.dist)
      if(any(inf.check)){
        print(paste("no solution found for cycle(s)", cycles[which(inf.check)],
                    ": flip distance set to 2000"))
        flip.distance <- 2000
      }

      #update the g1 & g2 data frames
      else{
        g1.df[as.numeric(rownames(g1.unresolved)),]$unresolved <- FALSE
        g1.df[as.numeric(rownames(g1.unresolved)),]$topochange <- "flip cycle"
        g2.df[as.numeric(rownames(g2.unresolved)),]$unresolved <- FALSE
        g2.df[as.numeric(rownames(g2.unresolved)),]$topochange <- "flip cycle"
        flip.distances <- sum(cyc.flip.dist)
      }


    }

    }

  #### STEP 4 - return


  return(list(
    g1.df = g1.df,
    g2.df = g2.df,
    distances = c(weight.distance = weight.distance,
                  vertex.distance = vertex.distance,
                  wanderer.distances = wanderer.distances,
                  flip.distances = flip.distances)
  ))

}
