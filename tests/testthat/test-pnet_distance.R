test_that("correctly measures added vertex", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(pnet_distance(simpleGs$g1,simpleGs$g3),
               c("weight difference" = 2,
                    "number of edge changes among shared scales" = 0,
                    "difference in scale count" = 0))
})

test_that("correctly measures no change", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(pnet_distance(simpleGs$g1,simpleGs$g1),
               c("weight difference" = 0,
                    "number of edge changes among shared scales" = 0,
                    "difference in scale count" = 0))
})

test_that("correctly measures g2 vs g3 change", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(pnet_distance(simpleGs$g2,simpleGs$g3),
               c("weight difference" = 2,
                 "number of edge changes among shared scales" = 1,
                 "difference in scale count" = 0))
})
# this should fail since these graphs are not spherical& fully triangulated
test_that("correctly measures g1 vs g6 change", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_error(pnet_distance(simpleGs$g1,simpleGs$g6),"Error in `data.frame(n = 1:length(cl), length = sapply(cl, length), matches = \"none\")`: arguments imply differing number of rows: 2, 0, 1")
})
