### tests fuer die gesamte calculateDescriptives-Funktion

### Achtung: wenn alle Probleme behoben sind, hier "suppressWarnings" entfernen und die
### Warnungen explizit abfangen (die soll es ja geben)

load(test_path("helper_varinfo_vorlage.rda"))
test_that("descriptives scale", {
  # create GADSdat
  file <- system.file("extdata", "example1.sav", package = "eatCodebook")
  gd   <- eatGADS::import_spss(file)                                          ### inputliste erzeugen
  vari <- suppressWarnings(createInputForDescriptives(gd, varNameSeparatorImp = NULL, impExpr = "plausible value", scaleExpr = "Skalenwert", verbose = FALSE))
  ### hier gibt es eine wichtige warnmeldung, die beruecksichtigt werden muss,
  ### dass das skalenniveau der items, die zu einer skala gehoeren, einheitlich sein muss
  ### korrektur von hand
  vari[grep("skala1item", vari[,"varName"]),"scale"] <- "ordinal"
  expect_equal(dim(vari), c(22,7))                                            ### data.frame mit 22 Zeilen, 7 Spalten
  tab  <- table(vari[,"type"])
  expect_equal(names(tab), c("item", "scale", "variable"))                    ### drei eintraege sollen vorkommen
  expect_equal(as.vector(tab), c(3,1,18))                                     ### scale nur einmal, variable 18-mal, item 3-mal
  vari[which(vari[,"varName"] == "skalenwert_fake"),"type"] <- "scale"        ### ein eintrag in der varinfo muss jetzt haendisch geaendert werden (das geschieht spaeter fuer das

  vari[vari$varName %in% paste0("pv_kat", 1:5), "group"] <- "pv_kat"
  warns <- capture_warnings(res  <- suppressMessages(calculateDescriptives(gd, vari, verbose = FALSE)))### tatsaechliche Skalenhandbuch bei Bedarf in Excel)
})


test_that("create descriptives", {
    clean2 <- eatGADS::import_spss(test_path("helper_clean2.sav"))
    expect_warning(clean_input2 <- createInputForDescriptives(clean2),
               "Identification of fake scales cannot be done completely automatically. Please check if the assignment of which items belong to a common scale is correct.")
    deskr <- calculateDescriptives(clean2, inputForDescriptives = clean_input2)
    deskr2<- readRDS("helper_descriptives.RDS")
    lapply(names(deskr), FUN = function(nam) {expect_equal(deskr[[nam]], deskr2[[nam]])})
})

