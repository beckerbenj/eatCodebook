
#dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")
dfSAV <- eatGADS::import_spss("helper_spss.sav")

#dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")

test_that("with pooled variables", {
  reading_vars <- paste0("rea_pv", 1:5)
  suppressMessages(small_pisa <- eatGADS::extractVars(eatGADS::pisa, c("idstud", reading_vars)))
  for(i in reading_vars) {
    small_pisa <- eatGADS::changeValLabels(small_pisa, varName = i, value = 10, valLabel = "some label")
  }
  input4descr <- createInputForDescriptives(small_pisa, impExpr = "Plausible Value")
  varInfo <- createVarInfo(small_pisa, input4descr)

  out <- createMissings(small_pisa, input4descr)
  expect_equal(names(out), c("Var.name", "Wert", "missing", "LabelSH", "Zeilenumbruch_vor_Wert"))
  expect_equal(out$Zeilenumbruch_vor_Wert, rep("nein", 2))
  expect_equal(out$missing, c("nein", "ja"))

  out2 <- createMissings(eatGADS::pisa)
  expect_equal(names(out2), c("Var.name", "Wert", "missing", "LabelSH", "Zeilenumbruch_vor_Wert"))
  expect_equal(out2$Zeilenumbruch_vor_Wert, rep("nein", 442))
})


test_that("normal", {
  input4descr <- createInputForDescriptives(dfSAV)

  dfSAV2 <- eatGADS::changeMissings(dfSAV, "VAR2", value = 2, missings = "miss")
  out <- createMissings(dfSAV2)
  expect_equal(names(out), c("Var.name", "Wert", "missing", "LabelSH", "Zeilenumbruch_vor_Wert"))
  expect_equal(out$Zeilenumbruch_vor_Wert, rep("nein", 2))
  expect_equal(out$missing, c("nein", "ja"))

  out2 <- createMissings(eatGADS::pisa)
  expect_equal(names(out2), c("Var.name", "Wert", "missing", "LabelSH", "Zeilenumbruch_vor_Wert"))
  expect_equal(out2$Zeilenumbruch_vor_Wert, rep("nein", 442))
})

