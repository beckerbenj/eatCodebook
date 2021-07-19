

test_that("multiplication works", {
  out <- getMissings("helper_missings.xlsx")
  expect_equal(length(out), 2)
  expect_equal(names(out), c("dat1", "dat2"))
})



# varue.file <- "other_code/Varue.xlsx"
# fbshort <- c("sfb" ,"lfb" ,"slfb" )
# sheets.varue.missings <- c("VarValues_sfbtr42","VarValue_leh","VarValue_pri")
# names(sheets.varue.missings) <- fbshort
# miss.cols <- c( "Var.name" , "Wert" , "missing" , "LabelSH", "Zeilenumbruch.vor.Wert") # Spalten in der Werteübersicht
#
# ds.file <- c("other_code/sfb_aufb_neu_v08.sav",
#              "other_code/lfb_aufb_neu_v04.sav",
#              "other_code/slfb_aufb_neu_v03.sav")
#
# names(ds.file) <- fbshort
# ds <- get.ds(ds.file , fbshort) # faster: new get.ds
#
# varue.cols <- c("Var.Name" , "in.DS.und.SH" , "Layout" , "LabelSH" , "Anmerkung.Var" , "Gliederung" , "Reihenfolge" , "Titel" , "rekodiert","QuelleSH" , "Instruktionen" , "Hintergrundmodell", "HGM.Variable.erstellt.aus", "HGM.Reihenfolge", "intern.extern", "Seitenumbruch.im.Inhaltsverzeichnis" ) # Spalten der Variableninformationen
# sheets.varue.info <- c("sfb_tr_ach","lfb","slfb")
# names(sheets.varue.info) <- fbshort
# varue.info <- get.varue.info(varue.file , sheets=sheets.varue.info, fbshort=fbshort , varue.cols=varue.cols)
#
#
# varue.missings <- get.varue.missings(varue.file=varue.file , sheets=sheets.varue.missings, fbshort=fbshort , miss.cols=miss.cols , varue.info=varue.info , Gesamtdatensatz=ds)
#
# saveRDS(varue.missings, "other_code/missing_aufbereitet_bt16.RDS")
#
# out <- getMissings("other_code/missings.xlsx")
# varue.missings <- readRDS("other_code/missing_aufbereitet_bt16.RDS")
#
# all.equal(out$VarValue_pri, varue.missings$slfb)
# all.equal(out$VarValue_leh, varue.missings$lfb)
# all.equal(out$VarValues_sfbtr42, varue.missings$sfb)
#
# str(varue.missings)
