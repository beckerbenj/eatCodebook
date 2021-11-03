
test_that("process meta data", {
  #out <- makeMetadata("tests/testthat/helper_meta.xlsx")
  out <- makeMetadata("helper_meta.xlsx")
  expect_equal(out[1], "\\Title{Codebook Test}")
  expect_equal(out[4], "\\Subject{test}")

})
