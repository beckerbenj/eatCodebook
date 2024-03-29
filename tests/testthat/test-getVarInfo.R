

test_that("transform char vector", {
  expect_equal(replaceNASignes(c("1", "a", "b")),
               c("1", "a", "b"))
  expect_equal(replaceNASignes(c("1", "", "b", "NA", "-")),
               c("1", "-", "b", "-", "-"))
  expect_equal(replaceNASignes(c("1 ", " ", "b", "NA ", "-")),
               c("1 ", "-", "b", "NA ", "-"))
})

test_that("prepare structure", {
  out <- prepareStructure(struc = rep(NA, 3), varNames = paste0("var", 1:3), in.DS.und.SH = rep("ja", 3))
  expect_equal(out, rep("1.1", 3))

  out2 <- prepareStructure(struc = rep("-", 3), varNames = paste0("var", 1:3), in.DS.und.SH = rep("ja", 3))
  expect_equal(out2, rep("1.1", 3))

  out3 <- prepareStructure(struc = c("1.1", "1.1", "2.0"), varNames = paste0("var", 1:3), in.DS.und.SH = rep("ja", 3))
  expect_equal(out3, c("1.1", "1.1", "2.0"))

  expect_error(prepareStructure(struc = c("1.1", NA, NA), varNames = paste0("var", 1:3), in.DS.und.SH = rep("ja", 3)),
               "The following variable(s) should be in the codebook but contain no valid 'Gliederungspunkt' ('number.number'): var2, var3",
               fixed = TRUE)

  out4 <- prepareStructure(struc = c("1.1", "-", "2.0"), varNames = paste0("var", 1:3), in.DS.und.SH = c("ja", "nein", "ja"))
  expect_equal(out4, c("1.1", NA, "2.0"))

})

test_that("get simple varue info", {
  #out <- getVarInfo("tests/testthat/helper_varInfo.xlsx")
  out <- getVarInfo("helper_varInfo.xlsx")
  expect_equal(names(out)[1], "Var.Name")
  expect_equal(out$LabelSH[1], "Variable 1")
  expect_equal(out$Anmerkung.Var[1], "-")
  expect_equal(out$Reihenfolge, c("0", "0", "0"))
})

test_that("no structure", {
  #out <- getVarInfo("tests/testthat/helper_varInfo_nostruc.xlsx")
  out <- getVarInfo("helper_varInfo_nostruc.xlsx")
  expect_equal(names(out)[1], "Var.Name")
  expect_equal(out$LabelSH[1], "Variable 1")
  expect_equal(out$Anmerkung.Var[1], "-")
  expect_equal(out$Reihenfolge, c("0", "0", "0"))
  expect_equal(out$Gliederung, c("1.1", "1.1", "1.1"))
})

test_that("unsorted", {
  #out <- getVarInfo("tests/testthat/helper_varInfo_unsorted.xlsx")
  out <- getVarInfo("helper_varInfo_unsorted.xlsx")
  expect_equal(out$Var.Name, c("VAR1", "VAR2", "VAR4", "VAR5", "VAR3"))
  expect_equal(out$Reihenfolge, c("0", "0", "1", "2", "0"))
  expect_equal(out$Gliederung, c("1.01", "1.02", "1.03", "1.03", "2.01"))
})


test_that("check_varInfo", {
  #varInfo <- getVarInfo("tests/testthat/helper_varInfo.xlsx")
  varInfo1 <- varInfo2 <- varInfo3 <- varInfo4 <- varInfo5 <- varInfo6 <- getVarInfo("helper_varInfo.xlsx")

  names(varInfo1)[1] <- "varName"
  expect_error(check_varInfo(varInfo1), "Malformed column names in 'varInfo'.")
  varInfo2[1, "in.DS.und.SH"] <- "-"
  expect_error(check_varInfo(varInfo2), "Invalid values in 'in.DS.und.SH' column in 'varInfo'.")
  varInfo3[2, "Titel"] <- NA
  expect_error(check_varInfo(varInfo3), "Missing values in 'Titel' column in 'varInfo'.")
  varInfo4[2, "Titel"] <- NA
  varInfo4[2, "in.DS.und.SH"] <- "nein"
  expect_silent(check_varInfo(varInfo4))
})


### currently the ordering follows specific defaults; somewhen redo and document?
# test_that("ordering of additional variables", {
#   #out <- getVarInfo("tests/testthat/helper_varInfo_bgm.xlsx")
#   out <- getVarInfo("helper_varInfo_bgm.xlsx")
#   expect_equal(names(out)[1], "Var.Name")
#   expect_equal(out$LabelSH[1], "Variable 1")
#   expect_equal(out$Anmerkung.Var[1], "-")
#   expect_equal(out$Reihenfolge, c("0", "0", "0"))
# })


# sheets.varue.info <- c("sfb_tr_ach","lfb","slfb")
# fbshort <- c("sfb" ,"lfb" ,"slfb" )
# names(sheets.varue.info) <- fbshort
# varue.cols <- c("Var.Name" , "in.DS.und.SH" , "Layout" , "LabelSH" , "Anmerkung.Var" , "Gliederung" , "Reihenfolge" , "Titel" , "rekodiert","QuelleSH" , "Instruktionen" , "Hintergrundmodell", "HGM.Variable.erstellt.aus", "HGM.Reihenfolge", "intern.extern", "Seitenumbruch.im.Inhaltsverzeichnis" ) # Spalten der Variableninformationen
# varue.info <- get.varue.info("other_code/varue.xlsx", sheets=sheets.varue.info, fbshort=fbshort , varue.cols=varue.cols)
#
# str(varue.info$slfb)
# table(varue.info$sfb$in.DS.und.SH)
