

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# sheets.varue.info <- c("sfb_tr_ach","lfb","slfb")
# fbshort <- c("sfb" ,"lfb" ,"slfb" )
# names(sheets.varue.info) <- fbshort
# varue.cols <- c("Var.Name" , "in.DS.und.SH" , "Layout" , "LabelSH" , "Anmerkung.Var" , "Gliederung" , "Reihenfolge" , "Titel" , "rekodiert","QuelleSH" , "Instruktionen" , "Hintergrundmodell", "HGM.Variable.erstellt.aus", "HGM.Reihenfolge", "intern.extern", "Seitenumbruch.im.Inhaltsverzeichnis" ) # Spalten der Variableninformationen
# varue.info <- get.varue.info("other_code/varue.xlsx", sheets=sheets.varue.info, fbshort=fbshort , varue.cols=varue.cols)
#
# str(varue.info$slfb)
# table(varue.info$sfb$in.DS.und.SH)
