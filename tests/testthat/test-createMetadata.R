test_that("create meta data", {
  out <- createMetadata()
  expect_equal(names(out), c("Title", "Author", "Keywords", "Subject"))
  expect_equal(nrow(out), 0)
})
