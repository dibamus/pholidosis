#' Six simple scale networks
#'
#' This dataset contains simple networks for use in demonstrating the "pholidosis" package
#'
#' @format A list of six graphs
library('igraph')

sg1 <- graph_from_edgelist(matrix(c("A","B","B","C","C","D","D","A","A","C"), ncol = 2, byrow = TRUE))
E(sg1)$weight <- c(1,1,1,1,1)

sg2 <- graph_from_edgelist(matrix(c("A","B","B","C","C","D","D","A","B","D"), ncol = 2, byrow = TRUE))
E(sg2)$weight <- c(1,1,1,1,1)

sg3 <- graph_from_edgelist(matrix(c("A","B","B","C","C","D","D","A","A","C"), ncol = 2, byrow = TRUE))
E(sg3)$weight <- c(1,1,1,1,3) #the "AC" edge has a weight of 3, indicating the scales A & C have fused

sg4 <- graph_from_edgelist(matrix(c("A","B","B","C","C","D","D","A","A","C","A","E"), ncol = 2, byrow = TRUE))
E(sg4)$weight <- c(1,1,1,1,1,1)

sg5 <- graph_from_edgelist(matrix(c("A","B","B","C","C","D","D","A","A","C","A","E","B","E"), ncol = 2, byrow = TRUE))
E(sg5)$weight <- c(1,1,1,1,1,1,1)

sg6 <- graph_from_edgelist(matrix(c("E","C","B","C","C","D","D","A","B","D","A","E","B","E"), ncol = 2, byrow = TRUE))
E(sg6)$weight <- c(1,1,1,1,1,1,1)

simpleGs <- list("g1"=sg1,"g2"=sg2,"g3"=sg3,"g4"=sg4,"g5"=sg5,"g6"=sg6)

