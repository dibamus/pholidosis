test_that("correctly counts edge frequencies", {
  data('simpleGs', package = 'pholidosis')
  mat <- morphoMatrix(simpleGs)
  exp_Edges <- setNames(c(5/6, 6/6, 6/6, 6/6,
                          4/6, 2/6, 3/6, 2/6, 1/6),
                        colnames(mat))
  expect_equal(component_frequencies(mat)$EdgeConsistency,exp_Edges)
})

test_that("correctly counts vertex frequencies", {
  data('simpleGs', package = 'pholidosis')
  mat <- morphoMatrix(simpleGs)
  exp_Vertices <- setNames(c(1.0, 1.0, 1.0, 1.0, 0.5),
                           c("A","B","C","D","E"))
  expect_equal(component_frequencies(mat)$VertexConsistency, exp_Vertices)
})

# Might want more later, but this is not an essential function.
