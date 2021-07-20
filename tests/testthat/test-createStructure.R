
#varInfo <- readRDS("tests/testthat/helper_varinfo_pisa.RDS")
varInfo <- readRDS("helper_varinfo_pisa.RDS")

test_that("for data.frame", {
  out <- createStructure(varInfo)
  expect_equal(names(out), c("Titel", "Ebene"))
})

test_that("for list", {
  l <- list(dat1 = varInfo, dat2 = varInfo)
  out <- createStructure(l)
  expect_equal(names(out), c("dat1", "dat2"))
  expect_equal(names(out[[1]]), c("Titel", "Ebene"))
})
