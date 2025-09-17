test_that("recognizes simplest edge swap", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(graph_edit_distance(simpleGs$g1, simpleGs$g2)$distances,
               list(node = 0, swap = 1, edge = 0))
})

test_that("correctly measures zero distance", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(graph_edit_distance(simpleGs$g1, simpleGs$g3)$distances,
               list(node = 0, swap = 0, edge = 0))
})

test_that("correctly measures added vertex", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(graph_edit_distance(simpleGs$g1, simpleGs$g4)$distances,
               list(node = 1, swap = 0, edge = 0))
})

test_that("correctly measures 2nd degree edge swap", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(graph_edit_distance(simpleGs$g5, simpleGs$g6)$distances,
               list(node = 0, swap = 2, edge = 0))
})

test_that("correctly measures g2-g3 distance", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_equal(graph_edit_distance(simpleGs$g2, simpleGs$g3)$distances,
               list(node = 0, swap = 1, edge = 0))
})

test_that("produces complete g1 data frame", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_snapshot(graph_edit_distance(simpleGs$g5, simpleGs$g6)$g1.df)
})

test_that("produces complete g2 data frame", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  expect_snapshot(graph_edit_distance(simpleGs$g5, simpleGs$g6)$g2.df)
})
