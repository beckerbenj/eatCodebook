

test_that("createandExecuteFunctionCalls", {
  input <- list(x = list(mtcars$mpg, mtcars$mpg), "trim" = list(0, 0.5))
  out <- createAndExecuteFunctionCalls("mean", input)
  expect_equal(out, list(20.090625, 19.2))
  expect_equal(names(out), NULL)
})

test_that("createandExecuteFunctionCalls with named list", {
  input <- list(x = list(vec1 = mtcars$mpg, vec2 = mtcars$mpg), "trim" = list(0, 0.5))
  out <- createAndExecuteFunctionCalls("mean", input)
  expect_equal(out, list(20.090625, 19.2))
  expect_equal(names(out), c("vec1", "vec2"))
})
