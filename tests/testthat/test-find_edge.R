test_that("absent edges return NA", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(find_edge(c('B','D'),as_edgelist(simpleGs$g1)),NA)
})

test_that("agnostic to vertex order", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(find_edge(c('C','D'),as_edgelist(simpleGs$g1)) ==
                 find_edge(c('D','C'),as_edgelist(simpleGs$g1)), TRUE)
})

test_that("returns correct row", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(find_edge(c('C','D'),as_edgelist(simpleGs$g1)), 3)
})

# Could use more tests, but this is unlikely to be called by users.
# Integral to the package but its behavior is pretty settled.
