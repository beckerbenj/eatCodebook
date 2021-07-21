

test_that("create lit info", {
  out <- createLitInfo()
  expect_equal(names(out), c("Kurzangabe", "Langangabe", "in_Literaturverzeichnis"))
})
