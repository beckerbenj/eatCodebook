
## currently not used
#abbrList <- createAbbrList()
#eatAnalysis::write_xlsx(abbrList, filePath = "tests/testthat/helper_abbrList")

test_that("with defaults", {
  expect_error(getAbbrList("helper_abbrList.xlsx"), "'getAbbrList' is deprecated, use 'makeAbbrList' instead.")
  # out <- getAbbrList("helper_abbrList.xlsx")
  # expect_equal(length(out), 2)
  # expect_equal(names(out), c("Statistische Formelzeichen", "Akronyme"))
  # expect_equal(dim(out[[2]]), c(1, 2))
})
