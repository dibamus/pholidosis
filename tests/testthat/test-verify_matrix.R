test_that("warns of mismatched column & row names", {
  library('readxl')
  sheets <- system.file("extdata","bad_matrix.xlsx", package = "pholidosis")
  mat.1 <- read_excel(sheets, sheet = 1)
  #snap <- system.file("testthat","")
  expect_snapshot(verify_matrix(mat.1))
})

test_that("warns of non-square matrix", {
  library('readxl')
  sheets <- system.file("extdata","bad_matrix.xlsx", package = "pholidosis")
  mat.4 <- read_excel(sheets, sheet = 4)
  mat.nsq <- rbind(mat.4, mat.4[1:2,])
  mat.nsq[dim(mat.nsq)[1]-1,1] <- "extra.row"
  mat.nsq[dim(mat.nsq)[1],1] <- ""
  expect_snapshot(verify_matrix(mat.nsq))
})

test_that("warns of uninterpretable inputs", {
  library('readxl')
  sheets <- system.file("extdata","bad_matrix.xlsx", package = "pholidosis")
  mat.2 <- read_excel(sheets, sheet = 2)
  #snap <- system.file("testthat","")
  expect_snapshot(verify_matrix(mat.2))
})

test_that("warns of non-triangulated network", {
  library('readxl')
  sheets <- system.file("extdata","bad_matrix.xlsx", package = "pholidosis")
  mat.3 <- read_excel(sheets, sheet = 3)
  #snap <- system.file("testthat","")
  expect_snapshot(verify_matrix(mat.3))
})

test_that("correctly loads example", {
  library('readxl')
  sheets <- system.file("extdata","bad_matrix.xlsx", package = "pholidosis")
  mat.4 <- read_excel(sheets, sheet = 4)
  #snap <- system.file("testthat","")
  expect_snapshot(verify_matrix(mat.4))
})
