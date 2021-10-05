
test_that("no input", {
  out <- createStructure()
  expect_equal(names(out), c("Titel", "Ebene"))
  expect_equal(nrow(out), 1)
  expect_equal(out[1, 1], NA)
  expect_equal(out[1, 2], NA)
})

#varInfo <- getVarInfo("tests/testthat/helper_varInfo.xlsx")
varInfo <- getVarInfo("helper_varInfo.xlsx")
varInfo[[1]]$Gliederung <- c("1.01", "1.03", "1.02")

test_that("for data.frame", {
  out <- createStructure(varInfo)
  expect_equal(out[[1]]$Ebene, c("1.01", "1.02", "1.03"))
})

test_that("for list", {
  l <- list(dat1 = varInfo[[1]], dat2 = varInfo[[1]])
  out <- createStructure(l)
  expect_equal(out$dat1$Ebene, c("1.01", "1.02", "1.03"))
  expect_equal(out$dat2$Ebene, c("1.01", "1.02", "1.03"))
})
