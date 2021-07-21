
#dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")
dfSAV <- eatGADS::import_spss("helper_spss.sav")

test_that("normal", {
  dfSAV2 <- eatGADS::changeMissings(dfSAV, "VAR2", value = 2, missings = "miss")
  out <- createMissings(dfSAV2)
  expect_equal(names(out), c("varName", "Wert", "missing", "Label", "Zeilenumbruch_vor_Wert"))
  expect_equal(out$Zeilenumbruch_vor_Wert, rep("nein", 2))
  expect_equal(out$missing, c("nein", "ja"))

  out2 <- createMissings(eatGADS::pisa)
  expect_equal(names(out2), c("varName", "Wert", "missing", "Label", "Zeilenumbruch_vor_Wert"))
  expect_equal(out2$Zeilenumbruch_vor_Wert, rep("nein", 442))
})

