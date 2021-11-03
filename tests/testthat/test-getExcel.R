
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

test_that("list with funList", {
  # out <- getExcel("tests/testthat/helper_getExcel.xlsx", funList = funList)
  funList <- list(fun1 = function(x) {
    x$v1 + x$v2
    },
    fun2 = function(x) mean(x)
  )
  out <- getExcel("helper_getExcel.xlsx", funList = funList)
  expect_equal(out, list(dat1 = 3, dat2 = 3.5))
})
