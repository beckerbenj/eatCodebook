

test_that("create lit info", {
  out <- createLitInfo()
  expect_equal(names(out), c("Kurzangabe", "Langangabe", "in_Literaturverzeichnis"))
})

#varInfo3 <- varInfo2 <- varInfo <- getVarInfo("tests/testthat/helper_varInfo.xlsx")
varInfo3 <- varInfo2 <- varInfo <- getVarInfo("helper_varInfo.xlsx")
varInfo[[1]]$QuelleSH <- c("Author1", "Author3", "Author2")
varInfo3[[1]]$QuelleSH <- c("Author1", "-", "Author2")

test_that("create lit info with simple input", {
  out <- createLitInfo(varInfo)
  expect_equal(out$Kurzangabe, c("Author1", "Author2", "Author3"))
})

test_that("create lit info with no input", {
  out <- createLitInfo(varInfo2)
  expect_equal(out$Kurzangabe, c("-"))
})

test_that("create lit info with mixed input", {
  out <- createLitInfo(varInfo3)
  expect_equal(out$Kurzangabe, c("Author1", "Author2"))
})

test_that("create lit info with list input", {
  varInfo_list <- list(dat1 = varInfo[[1]], dat2 = varInfo[[1]])
  out <- createLitInfo(varInfo_list)
  expect_equal(out$Kurzangabe, c("Author1", "Author2", "Author3"))
})
