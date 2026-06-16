test_that("recognizes and fixes undirected graphs", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_message(ged(simpleGs$g1,simpleGs$g2),
                 "One or both graphs have directed edges. Converting to undirected for measurement.")
})

test_that("recognizes and warns of mon-mp graphs", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_warning(ged(simpleGs$g1,simpleGs$g2),
                 "g1 is not maximally planar. Edit distance may be incorrectly estimated")
})

test_that("correctly measures 1-swap", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(ged(simpleGs$g1,simpleGs$g2)$distances,
               #make sure no distances but the swap
               setNames(c(0,0,0,1), nm = c("weight.distance",
                                           "vertex.distance",
                                           "wanderer.distances",
                                           "flip.distances")))
})

test_that("correctly measures fusion", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(ged(simpleGs$g1,simpleGs$g3)$distances,
               #make sure no distances but the swap
               setNames(c(4,0,0,0), nm = c("weight.distance",
                                           "vertex.distance",
                                           "wanderer.distances",
                                           "flip.distances")))
})

test_that("correctly measures fusion and swap", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(ged(simpleGs$g2,simpleGs$g3)$distances,
               #make sure no distances but the swap
               setNames(c(0,0,0,1), nm = c("weight.distance",
                                           "vertex.distance",
                                           "wanderer.distances",
                                           "flip.distances")))
})

test_that("simple wanderer works", {
  library('igraph')
  data('simplewanderers', package = 'pholidosis')
  expect_equal(ged(simplewanderers$g1,simplewanderers$g2)$distances,
               #make sure no distances but the swap
               setNames(c(0,0,6,0), nm = c("weight.distance",
                                           "vertex.distance",
                                           "wanderer.distances",
                                           "flip.distances")))
})
