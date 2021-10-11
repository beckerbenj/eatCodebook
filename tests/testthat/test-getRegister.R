
test_that("Errors", {
  #getRegister("tests/testthat/helper_register_err1.xlsx")
  expect_error(getRegister("helper_register_err1.xlsx"), "Keywords are missing.")
  expect_error(getRegister("helper_register_err2.xlsx"), "Keyword 'keyword1' is not assigned in sheet 'df2'.")
})

test_that("regular loading", {
  # out <- getRegister("tests/testthat/helper_register.xlsx")
  out <- getRegister("helper_register.xlsx")
  expect_equal(names(out), c("df1", "df2"))
  expect_equal(names(out[[1]]), c("Nr.", "varName", "varLabel", "keyword1", "keyword2"))
  expect_equal(names(out[[2]]), c("Nr.", "varName", "varLabel", "keyword1", "keyword2", "keyword3"))
})
