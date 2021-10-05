
test_that("data.frame", {
  # out <- getMissings("tests/testthat/helper_varInfo.xlsx")
  out <- getExcel("helper_varInfo.xlsx")
  expect_true(is.data.frame(out))
})


test_that("list format", {
  # out <- getMissings("tests/testthat/helper_missings.xlsx")
  out <- getExcel("helper_missings.xlsx")
  expect_equal(length(out), 2)
  expect_equal(names(out), c("dat1", "dat2"))
})
