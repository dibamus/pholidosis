#' Measures the full graph edit distance  between two graphs
#' @import igraph
#' @param g1 An igraph object; the first graph.
#' @param g2 An igraph object; the second graph.
#' @param truncate A logical scalar. If TRUE (default), the function returns only
#'   the three distance measures. If false, it returns the distance measures and
#'   the data frames built by graphEditDist.
#' @return Either a named vector of length 3, containing:
#'   1) the weight difference of shared edges
#'   2) the number of edge changes among shared scales
#'   3) the number of unshared scales
#'   between graphs g1 and g2.
#'   OR
#'   a list containing:
#'   topo - the distance measures and data frames built by graphEditDist
#'   measure - the named vector described above.
#'
#' @export
pnet_distance <- function(g1,g2, truncate = TRUE){
  ed <- graphEditDist(g1,g2)
  g1.E <- data.frame(as_edgelist(g1),weight = E(g1)$weight, indep.weight = (E(g1)$weight -1))
  g2.E <- data.frame(as_edgelist(g2),weight = E(g2)$weight, indep.weight = (E(g2)$weight -1))

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
  # make column of weight differences of shared edges 
  # this is now accounted for in lizard_setup.R
  # a change of weight = 1 to weight = 3 (unfused to fully fused) counts as 1 change
  # a change of weight = 1 to weight = 2 (unfused to semi-fused) counts as .5 change
  g.U$wDiff <- (g.U$weight - g.U$g2weight) # this is now accounted for in lizard_setup.R

  distances <- c(w = sum(abs(g.U$wDiff)) + sum(abs(g1.E$indep.weight)) + sum(abs(g2.E$indep.weight)),
                 e = ed$distances$swap + ed$distances$edge,
                 f = ed$distances$node)
  names(distances) <- c("weight difference",
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
