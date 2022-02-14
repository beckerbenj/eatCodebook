
#dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")
dfSAV <- eatGADS::import_spss("helper_spss.sav")

#dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")

test_that("normal", {
  dfSAV2 <- eatGADS::changeMissings(dfSAV, "VAR2", value = 2, missings = "miss")
  input4descr <- createInputForDescriptives(dfSAV2)

  out <- createMissings(dfSAV2, inputForDescriptives = input4descr)
  expect_equal(names(out), c("Var.name", "Wert", "missing", "LabelSH", "Zeilenumbruch_vor_Wert"))
  expect_equal(out$Zeilenumbruch_vor_Wert, rep("nein", 2))
  expect_equal(out$missing, c("nein", "ja"))

  input4descr_pisa <- createInputForDescriptives(eatGADS::pisa)
  out2 <- createMissings(eatGADS::pisa, input4descr_pisa)
  expect_equal(names(out2), c("Var.name", "Wert", "missing", "LabelSH", "Zeilenumbruch_vor_Wert"))
  expect_equal(out2$Zeilenumbruch_vor_Wert, rep("nein", 442))
})

test_that("with pooled variables", {
  reading_vars <- paste0("rea_pv", 1:5)
  suppressMessages(small_pisa <- eatGADS::extractVars(eatGADS::pisa, c("idstud", reading_vars)))
  small_pisa_withMeta <- small_pisa_noMeta <- small_pisa

  input4descr1 <- createInputForDescriptives(small_pisa_noMeta, impExpr = "Plausible Value")
  varInfo1 <- createVarInfo(small_pisa_withMeta, input4descr1)

  out <- createMissings(small_pisa_noMeta, input4descr1)
  expect_equal(names(out), c("Var.name", "Wert", "missing", "LabelSH", "Zeilenumbruch_vor_Wert"))
  expect_equal(nrow(out), 0)

  for(i in reading_vars) {
    small_pisa_withMeta <- eatGADS::changeValLabels(small_pisa_withMeta, varName = i, value = 10, valLabel = "some label")
  }
  input4descr2 <- createInputForDescriptives(small_pisa_withMeta, impExpr = "Plausible Value")
  varInfo <- createVarInfo(small_pisa_withMeta, input4descr2)

  out <- createMissings(small_pisa_withMeta, input4descr2)
  expect_equal(names(out), c("Var.name", "Wert", "missing", "LabelSH", "Zeilenumbruch_vor_Wert"))
  expect_equal(out$Zeilenumbruch_vor_Wert, rep("nein", 6))
  expect_equal(out$missing, rep("nein", 6))
})
