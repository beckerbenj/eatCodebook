
#dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")
dfSAV <- eatGADS::import_spss("helper_spss.sav")

test_that("with pisa", {
  out <- createVarueInfo(eatGADS::pisa)
  expect_equal(out$Var.Name, eatGADS::namesGADS(eatGADS::pisa))
  expect_equal(out$in.DS.und.SH[1], "ja")
  expect_equal(out$LabelSH[1], "Student-ID")
  expect_equal(out$LabelSH, out$Titel)
})

test_that("with list", {
  l1 <- list(pisa = eatGADS::pisa, other = dfSAV)
  out <- createVarueInfo(l1)
  expect_equal(names(out), c("pisa", "other"))
  expect_equal(out[[1]]$Var.Name, eatGADS::namesGADS(eatGADS::pisa))
  expect_equal(out[[1]]$LabelSH, out[[1]]$Titel)
  expect_equal(out[[2]]$Var.Name, eatGADS::namesGADS(dfSAV))
})
