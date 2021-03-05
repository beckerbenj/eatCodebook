
test <- ds.einlesen("tests/testthat/helper_spss.sav")

test_that("data import", {

  expect_equal(2 * 2, 4)
})
