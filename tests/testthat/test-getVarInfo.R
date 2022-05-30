

test_that("transform char vector", {
  expect_equal(replaceNASignes(c("1", "a", "b")),
               c("1", "a", "b"))
  expect_equal(replaceNASignes(c("1", "", "b", "NA", "-")),
               c("1", "-", "b", "-", "-"))
  expect_equal(replaceNASignes(c("1 ", " ", "b", "NA ", "-")),
               c("1 ", "-", "b", "NA ", "-"))
})

test_that("get simple varue info", {
  #out <- getVarInfo("tests/testthat/helper_varInfo.xlsx")
  out <- getVarInfo("helper_varInfo.xlsx")
  expect_equal(names(out)[1], "Var.Name")
  expect_equal(out$LabelSH[1], "Variable 1")
  expect_equal(out$Anmerkung.Var[1], "-")
  expect_equal(out$Reihenfolge, c("0", "0", "0"))
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



# sheets.varue.info <- c("sfb_tr_ach","lfb","slfb")
# fbshort <- c("sfb" ,"lfb" ,"slfb" )
# names(sheets.varue.info) <- fbshort
# varue.cols <- c("Var.Name" , "in.DS.und.SH" , "Layout" , "LabelSH" , "Anmerkung.Var" , "Gliederung" , "Reihenfolge" , "Titel" , "rekodiert","QuelleSH" , "Instruktionen" , "Hintergrundmodell", "HGM.Variable.erstellt.aus", "HGM.Reihenfolge", "intern.extern", "Seitenumbruch.im.Inhaltsverzeichnis" ) # Spalten der Variableninformationen
# varue.info <- get.varue.info("other_code/varue.xlsx", sheets=sheets.varue.info, fbshort=fbshort , varue.cols=varue.cols)
#
# str(varue.info$slfb)
# table(varue.info$sfb$in.DS.und.SH)
