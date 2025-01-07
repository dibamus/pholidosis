test_that("recognizes shared edges", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  g1 <- as_edgelist(simpleGs$g1)
  g2 <- as_edgelist(simpleGs$g2)
  g4 <- as_edgelist(simpleGs$g4)
  expect_equal(compare_graphs(g1,g2),c(1,2,3,4,NA))
  expect_equal(compare_graphs(g4,g1),c(1,2,3,4,5,NA))
})

test_that("does not drop repeateded edges", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  g1 <- as_edgelist(simpleGs$g1)
  gE <- rbind(g1,g1[1:2,])
  expect_equal(compare_graphs(gE,g1),c(1,2,3,4,5,1,2))
})

test_that("returns NA for NA matrices", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  g1 <- as_edgelist(simpleGs$g1)
  gEmpty <- matrix(nrow = 2, ncol = 2)

  expect_equal(compare_graphs(gEmpty,g1),c(NA,NA))
  expect_equal(compare_graphs(g1,gEmpty),c(NA,NA,NA,NA,NA))
})

test_that("throws error when given graph",{
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  g1 <- as_edgelist(simpleGs$g1)
  expect_error(compare_graphs(g1, simpleGs[[2]]),"One or more arguments is not coercible to a matrix.")
})

test_that("throws error when wrong dimensions",{ # not sure why this isn't working...
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  g1 <- as_edgelist(simpleGs$g1)
  gWide <- cbind(as_edgelist(simpleGs$g1),1:5)
  expect_error(compare_graphs(g1, gWide),"One or more arguments is not an n*2 edge list.")
})
