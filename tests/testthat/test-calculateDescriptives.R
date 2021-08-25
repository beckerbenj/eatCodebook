### tests fuer die gesamte createInputForDescriptives- und calculateDescriptives-Funktion

### Achtung: wenn alle Probleme behoben sind, hier "suppressWarnings" entfernen und die
### Warnungen explizit abfangen (die soll es ja geben)

#load("tests/testthat/helper_varinfo_vorlage.rda")
load("helper_varinfo_vorlage.rda")
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
  expect_equal(names(tab), c("scale", "variable"))                            ### beide eintraege sollen vorkommen
  expect_equal(as.vector(tab), c(1,21))                                       ### scale nur einmal, variable 21-mal
  vari[which(vari[,"varName"] == "skalenwert_fake"),"type"] <- "scale"        ### ein eintrag in der varinfo muss jetzt haendisch geaendert werden (das geschieht spaeter fuer das
  res  <- suppressMessages(calculateDescriptives(gd, vari))                   ### tatsaechliche Skalenhandbuch bei Bedarf in Excel)
})
