test_that("cleans graph", {
  library('igraph')
  g <- graph_from_edgelist(matrix(c("B","C","C","D","D","A","B","D","E","E"), ncol = 2, byrow = TRUE))
  gExpected <- g <- graph_from_edgelist(matrix(c("B","C","C","D","D","A","B","D"), ncol = 2, byrow = TRUE))
  expect_equal(clean_isolates(g),gExpected)
  expect_equal(gExpected,gExpected)
}
)
