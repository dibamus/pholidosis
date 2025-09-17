test_that("produces correct character matrix", {
  suppressMessages(library('igraph'))
  data('simpleGs', package = 'pholidosis')
  expect_snapshot(morpho_matrix(simpleGs))
})
