test_that("correctly counts frequencies", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  mat <- morphoMatrix(simpleGs)
  exp_Edges <- setNames(c(5/6, 6/6, 6/6, 6/6,
                          4/6, 2/6, 3/6, 2/6, 1/6),colnames(mat))
  exp_Vertices <- setNames(c(1.0, 1.0, 1.0, 1.0, 0.5),
                           c("A","b","C","D","E"))
  expect_equal(componentFrequencies(mat), list(EdgeConsistency = exp_Edges,
                                               VertexConsistency = exp_vertices))

})
