

#varInfo3 <- varInfo2 <- varInfo <- getVarInfo("tests/testthat/helper_varInfo.xlsx")
varInfo3 <- varInfo2 <- varInfo <- getVarInfo("helper_varInfo.xlsx")

test_that("multiplication works", {
  out <- createChapters(varInfo)
  expect_equal(out, data.frame(dataName = "dat", chapterName = NA, stringsAsFactors = FALSE))
})

test_that("multiplication works", {
  varInfo_list <- list(dat1 = varInfo, dat2 = varInfo)
  out <- createChapters(varInfo_list)
  expect_equal(out, data.frame(dataName = c("dat1", "dat2"), chapterName = NA, stringsAsFactors = FALSE))
})

