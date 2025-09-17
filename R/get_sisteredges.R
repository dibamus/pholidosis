#' get_sisteredges
#'
#' For a set of problematic edges and their
#'
#' This function takes a graph and vertex names of an edge and returns
#' vertex names for the 2 vertices which complete the 2 adjacent triangles
#'
#' @importFrom igraph dfs graph_from_adjacency_matrix
#' @importFrom dplyr %>%
#' @param input_matrix an n × 4 matrix where the rows correspond to
#' problematic edges in a graph and the 4 columns contain the 4 vertices which
#' complete the 2 triangles surrounding the problem edge
#' @return A data frame of information about the groupings of problem edges
#'
#' @examples
#' input <- matrix(
#' c("a","c","b","e",
#' "c","e","a","d"), nrow = 2, byrow = TRUE)
#' get_sisteredges(input)

get_sisteredges <- function(input_matrix){
  l <- dim(input_matrix)[1]

  adjacent <- sapply(1:l, FUN = function(r){
    sapply(1:l, FUN = function(r2){
      all(input_matrix[r2,1:2] %in% input_matrix[r,])
    })
  })

  diag(adjacent) <- F

  #figure out which lizard graph edges (nodes in adj_graph) are connected
  adj_graph <- graph_from_adjacency_matrix(adjacent)

  sisters <- data.frame(edge = 1:l)
  sisters$sisters <- sapply(1:l, function(x){
    #Do a depth-first search of the adjacency graph to find connected edges
    list(dfs(adj_graph, x, unreachable = F)$order %>% sort())})
  sisters$sistergroup <- sisters$sisters %>% paste()
  sisters$sister_edges <- sapply(1:l, function(x){
    rows <- which(sisters$sistergroup == sisters$sistergroup[x])
    input_matrix[rows,1:2] %>% c() %>% paste0(collapse = "-")
  })
  sisters$groupsize <- sapply(1:l, function(x){
    length(which(sisters$sistergroup == sisters$sistergroup[x]))
  })
  return(sisters)
}
