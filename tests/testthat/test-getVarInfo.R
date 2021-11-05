

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
