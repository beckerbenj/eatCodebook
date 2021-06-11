
test_that("regular creation", {
  out <- createStructure(c("df1", "df2"))
  expect_equal(names(out), c("df1", "df2"))
  expect_equal(names(out[[1]]), c("Titel", "Ebene"))
})


test_that("regular loading", {
  out <- getStructure("helper_structure.xlsx")
  expect_equal(names(out), c("df1", "df2"))
  expect_equal(names(out[[1]]), c("Titel", "Ebene"))
})
