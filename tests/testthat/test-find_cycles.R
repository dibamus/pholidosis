test_that("finds cycles of 5 or larger (defalut)", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  cycles_biggerthan5_g1 <- list(setNames(c(1,2,3,4,1),
                                         c("","B","C","D","A")))

  expect_equal(find_cycles(simpleGs$g1),cycles_biggerthan5_g1)
})

test_that("finds cycles of 3 or larger (defalut)", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')
  cycles_biggerthan3_g1 <- list(setNames(c(1,2,1),
                                         c("","B","A")),
                                setNames(c(1,2,3,1),
                                         c("","B","C","A")),
                                setNames(c(1,2,3,4,1),
                                         c("","B","C","D","A")),
                                setNames(c(1,3,1),
                                         c("","C","A")),
                                setNames(c(1,3,4,1),
                                         c("","C","D","A")),
                                setNames(c(1,4,1),
                                         c("","D","A")),
                                setNames(c(2,3,2),
                                         c("","C","B")),
                                setNames(c(3,4,3),
                                         c("","D","C")))

  expect_equal(find_cycles(simpleGs$g1, 3),cycles_biggerthan3_g1)
})

test_that("returns empty list when no cycles present", {
  library('igraph')
  data('simpleGs', package = 'pholidosis')

  expect_equal(find_cycles(simpleGs$g1,6),list())
})
