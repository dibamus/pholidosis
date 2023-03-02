require("igraph")
#require("ggraph")
require("stringr")
#source("Scripts/clean_isolates.R")

# This loads a dataframe (df) of scale relationships as an igraph

scaleNetwork <-  function(df, checkAsymmetry = FALSE, verbose = FALSE,
                          firstscale = "rostral",
                          lastscale = "mental"){
  mat <-as.matrix(df) # read in adjacency matrix file
  mat[is.na(mat)] <- 0 # set all NA links to 0

  sym <- TRUE #assume matrix is symmetric (or it doesn't need to be)

  if(checkAsymmetry){ #if you do want to check your work, this does it
    sym <- isSymmetric(mat)
  }

  #check if column and row names match
  if(any(row.names(mat) != colnames(mat))){ #if the row and column names don't match
    #tell the user which ones don't match

    if(verbose) {cat("One or more row names do not match the column names:\n")
    cat(row.names(mat)[which(row.names(mat) != colnames(mat))],"\n\n")}

    #then return
    return()
  }


  # check if matrix is empty:
  if(sum(mat) ==0) {
    make_empty_graph()
    print("graph is empty")
  }

  #if matrix is not empty
  else{
    if (sym) { # yes, it is symmetric - complete the import

      graph <- graph_from_adjacency_matrix(weighted = T, adjmatrix = mat, mode = "undirected")
      graph <- clean_isolates(graph) #get rid of any isolated vertices


      graph <- lizard_setup(graph)

      ####LAYOUT####

      lay <- create_layout(graph, layout = "linear") #construct layout for plotting
      #X COORD
      lay[,"x"] <- V(graph)$pos # set x position as "pos" variable
      lay[which(lay[,"name"] == lastscale),"x"] <- lay[which(lay[,"name"] == lastscale),"x"] -1 # bump mental forward in layout

      # Y COORD
      depth <- max(V(graph)$pos)

      for (i in 0:depth) { #this function generates a y-coordinate for each scale
        #based off of its centrality in its positional (ant/post) subgraph
        subnet <- induced_subgraph(graph, which(V(graph)$pos==i))
        rowcent <- V(subnet)$side * (1/subgraph_centrality(subnet))
        rowcent <- sort(rowcent)
        # reassign rowcent values to whole #s
        vec <- c(1:length(rowcent))

        rowcent[1:length(rowcent)] <- vec - ceiling(length(vec)/2)
        #center rows with even # of scales
        if (length(rowcent) %% 2 == 0) {
          rowcent <- rowcent - 0.5
        }

        V(graph)$lat[match(names(rowcent),V(graph)$name)] <- rowcent/(max(rowcent) + 1)
      }
      lay[,"y"] <- V(graph)$lat
      lay[which(lay[,"name"] == lastscale),"y"] <- 0

      graph$layout <- lay #assign the graph a layout property

      if(verbose){cat("✓ converted")}

      return(graph) #return the graph object
    }

    else { #the matrix is not symmetric - correct it
      if(verbose){cat("asymmetric matrix","\n",
          "Coordinates of asymmertic entries:","\n",
          which(mat != t(mat), arr.ind = TRUE),"\n")}
      }
  }
}

#set up a graph with everything needed to plot out a comprehensible lizard
lizard_setup <- function(graph){
  graph <- perimeter(graph)
  V(graph)$str <- igraph::strength(graph) # access strength (sum of vertex weights) for each node
  V(graph)$deg <- igraph::degree(graph)

  #store general scale name (name stripped of any numbers, R/L indicators, underscores)
  V(graph)$scaletype <- str_replace_all(names(V(graph)), "[0123456789_RL]","")


  #ORDINATION
  V(graph)$pos <- distances(graph, weights = NA)[,firstscale] #ordinate nodes (anterior (0) to posterior (+n))
  # set siding info: -1 for left
  V(graph)$side <- rep(0, length(V(graph)))
  V(graph)$side[grep("L_",V(graph)$name)] <- -1
  V(graph)$side[grep("R_",V(graph)$name)] <- 1

  return(graph)
}
