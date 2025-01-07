test_that("produces correct sampleGs matrix", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_snapshot(net_dist_mat(simpleGs))
})
