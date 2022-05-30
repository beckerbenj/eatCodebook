test_that("special signs", {
  out <- sonderzeichen.aufbereiten(c("a", "b", "x%", "\\", "ab#a_"))
  expect_equal(out, c("a", "b", "x\\%", "\\", "ab\\#a\\_"))
})
