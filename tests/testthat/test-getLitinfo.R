test_that("create lit info", {
  out <- createLitinfo()
  expect_equal(names(out), c("Kurzangabe", "Langangabe", "in_Literaturverzeichnis"))
})
