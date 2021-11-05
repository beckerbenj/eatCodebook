
#dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")
dfSAV <- eatGADS::import_spss("helper_spss.sav")

#gads <- readRDS("tests/testthat/helper_scaleDF.RDS")
gads <- readRDS("helper_scaleDF.RDS")

test_that("with pisa", {
  suppressMessages(input4descr <- createInputForDescriptives(eatGADS::pisa, impExpr = "Plausible Value"))
  out <- createVarInfo(eatGADS::pisa, input4descr)
  expect_true(all(eatGADS::namesGADS(eatGADS::pisa) %in% out$Var.Name))
  expect_true(all(c("ma_pooled", "rea_pooled", "sci_pooled") %in% out$Var.Name))
  expect_equal(out$in.DS.und.SH[1], "ja")
  expect_equal(out$Unterteilung.im.Skalenhandbuch, rep(NA, nrow(out)))
  expect_equal(sum(out$in.DS.und.SH == "sh"), 3)
  expect_equal(sum(out$in.DS.und.SH == "ds"), 3 * 5)
  expect_equal(out$LabelSH[1], "Student-ID")
  expect_equal(out$LabelSH, out$Titel)
  expect_equal(unique(out$Gliederung), "-")
})

test_that("with list", {
  l1 <- list(pisa = eatGADS::pisa, other = dfSAV)
  suppressMessages(inputList <- createInputForDescriptives(l1, impExpr = "Plausible Value"))
  out <- createVarInfo(l1, inputList)
  expect_equal(names(out), c("pisa", "other"))
  expect_true(all(eatGADS::namesGADS(eatGADS::pisa) %in% out[[1]]$Var.Name))
  expect_true(all(c("ma_pooled", "rea_pooled", "sci_pooled") %in% out[[1]]$Var.Name))
  expect_equal(out[[1]]$LabelSH, out[[1]]$Titel)
  expect_equal(out[[2]]$Var.Name, eatGADS::namesGADS(dfSAV))
})

test_that("input validation with list", {
  l1 <- list(pisa = eatGADS::pisa, other = dfSAV)
  suppressMessages(inputList <- createInputForDescriptives(l1, impExpr = "Plausible Value"))
  expect_error(createVarInfo(l1, inputList[1]),
              "'GADSdat' and 'inputForDescriptives' lists have different lengths.")

})
