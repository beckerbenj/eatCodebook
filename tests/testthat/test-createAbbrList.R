
test_that("with defaults", {
  out <- createAbbrList()
  expect_equal(length(out), 2)
})
