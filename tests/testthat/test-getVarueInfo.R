

# dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")
# varueInfo <- createVarueInfo(dfSAV)
# out$Anmerkung.Var[1] <- "NA"
# eatAnalysis::write_xlsx(varueInfo, "tests/testthat/helper_varueInfo.xlsx", row.names = FALSE)


test_that("multiplication works", {
  #out <- getVarueInfo("tests/testthat/helper_varueInfo.xlsx")
  out <- getVarueInfo("helper_varueInfo.xlsx")
  expect_equal(names(out[[1]])[1], "Var.Name")
  expect_equal(out[[1]]$LabelSH[1], "Variable 1")
  expect_equal(out[[1]]$Anmerkung.Var[1], "-")

})


# sheets.varue.info <- c("sfb_tr_ach","lfb","slfb")
# fbshort <- c("sfb" ,"lfb" ,"slfb" )
# names(sheets.varue.info) <- fbshort
# varue.cols <- c("Var.Name" , "in.DS.und.SH" , "Layout" , "LabelSH" , "Anmerkung.Var" , "Gliederung" , "Reihenfolge" , "Titel" , "rekodiert","QuelleSH" , "Instruktionen" , "Hintergrundmodell", "HGM.Variable.erstellt.aus", "HGM.Reihenfolge", "intern.extern", "Seitenumbruch.im.Inhaltsverzeichnis" ) # Spalten der Variableninformationen
# varue.info <- get.varue.info("other_code/varue.xlsx", sheets=sheets.varue.info, fbshort=fbshort , varue.cols=varue.cols)
#
# str(varue.info$slfb)
# table(varue.info$sfb$in.DS.und.SH)
