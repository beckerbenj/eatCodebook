
#input <- readRDS("tests/testthat/helper_inputForDescriptives_clean.RDS")
input <- readRDS("helper_inputForDescriptives_clean.RDS")

test_that("for data.frame", {
  out <- createStructure(input)
  expect_equal(names(out), c("Titel", "Ebene"))
})

test_that("for list", {
  l <- list(dat1 = input, dat2 = input)
  out <- createStructure(l)
  expect_equal(names(out), c("dat1", "dat2"))
  expect_equal(names(out[[1]]), c("Titel", "Ebene"))
})
