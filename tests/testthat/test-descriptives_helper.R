

#load("q:/BT2016/BT/02_Organisation/81_StuMi_Arbeitsordner/Skalenhandbuch/03_Kennwerte/Kennwerte_sfb.rdata")
#Kennwertedatensatz$TR_SEX
#kennwerte.kategorial(ds$sfb$TR_SEX, c(-94,-99))

test_that("descriptives categorical", {
  df <- data.frame(id = 1:5, v1 = c(1, 3, -99, -98, NA))
  value_table <- data.frame(value = c(1, 2, 3, -98, -99),
                            missings = c("valid", "valid", "valid", "miss", "miss"),
                            stringsAsFactors = FALSE)
  out <- suppressWarnings(kennwerte.kategorial(df$v1, value_table))
  expect_equal(out[["N.valid"]], "2")
  expect_equal(out[["N.total"]], "5")
 expect_equal(out[["1.valid"]], "50.0")
  expect_equal(out[["2.valid"]], "0.0")
  expect_equal(out[["3.valid"]], "50.0")
  expect_equal(out[["-98.valid"]], "\\multic{--}")
  expect_equal(out[["1.total"]], "20.0")
  expect_equal(out[["2.total"]], "0.0")
  expect_equal(out[["3.total"]], "20.0")
  expect_equal(out[["-99.total"]], "20.0")
  expect_equal(out[["-99.totalabs"]], "1")
  expect_equal(out[["sysmis.valid"]], "\\multic{--}")
  expect_equal(out[["sysmis.total"]], "20.0")
  expect_equal(out[["sysmis.totalabs"]], "1")
  expect_equal(names(out)[3:7], c("1.valid", "2.valid", "3.valid", "sysmis.valid", "-98.valid"))
})

test_that("descriptives categorical with character", {
  value_table <- data.frame(value = c("a", "b", -98, -99),
                            missings = c("valid", "valid", "miss", "miss"),
                            stringsAsFactors = FALSE)
  out <- kennwerte.kategorial(c("a", "b", "b", -99, -98, NA), value_table)
  expect_equal(out[["N.valid"]], "3")
  expect_equal(out[["N.total"]], "6")
  expect_equal(out[["a.valid"]], "33.3")
  expect_equal(out[["b.valid"]], "66.7")
  expect_equal(out[["-98.valid"]], "\\multic{--}")
  expect_equal(out[["a.total"]], "16.7")
  expect_equal(out[["b.total"]], "33.3")
  expect_equal(out[["-99.total"]], "16.7")
  expect_equal(out[["-99.totalabs"]], "1")
  expect_equal(out[["sysmis.valid"]], "\\multic{--}")
  expect_equal(out[["sysmis.total"]], "16.7")
  expect_equal(out[["sysmis.totalabs"]], "1")
  expect_equal(names(out)[3:6], c("a.valid", "b.valid", "sysmis.valid", "-98.valid"))
})


test_that("descriptives categorical no labels", {
  value_table <- data.frame(value = c(-98, -99),
                            missings = c("miss", "miss"),
                            stringsAsFactors = FALSE)
  out <- suppressWarnings(kennwerte.kategorial(c(-99, -98, 1, 5), value_table))
  expect_equal(out[["N.valid"]], "2")
  expect_equal(out[["N.total"]], "4")
  expect_equal(out[["1.valid"]], "50.0")
  expect_equal(out[["5.valid"]], "50.0")
  expect_equal(out[["-98.valid"]], "\\multic{--}")
  expect_equal(out[["1.total"]], "25.0")
  expect_equal(out[["5.total"]], "25.0")
  expect_equal(out[["-99.total"]], "25.0")
  expect_equal(out[["-99.totalabs"]], "1")
  expect_equal(out[["sysmis.valid"]], "\\multic{--}")
  expect_equal(out[["sysmis.total"]], "0.0")
  expect_equal(out[["sysmis.totalabs"]], "0")
  expect_equal(names(out)[3:6], c("1.valid", "5.valid", "sysmis.valid", "-98.valid"))
})

test_that("descriptives ordinal", {
  value_table <- data.frame(value = c(1, 2, 3, -98, -99),
                            missings = c("valid", "valid", "valid", "miss", "miss"),
                            stringsAsFactors = FALSE)
  out <- suppressWarnings(kennwerte.ordinal(c(3, 3, 1, 2, -99), value_table))
  expect_equal(out[["N.valid"]], "4")
  expect_equal(out[["N.total"]], "5")
  expect_equal(out[["mean.valid"]], "2.25")
  expect_equal(out[["sd.valid"]], "0.96")
  expect_equal(out[["1.valid"]], "25.0")
  expect_equal(out[["2.valid"]], "25.0")
  expect_equal(out[["3.valid"]], "50.0")
  expect_equal(out[["-98.valid"]], "\\multic{--}")
  expect_equal(out[["1.total"]], "20.0")
  expect_equal(out[["-98.total"]], "0.0")
  expect_equal(out[["-99.total"]], "20.0")
  expect_equal(out[["-99.totalabs"]], "1")
  expect_equal(out[["sysmis.valid"]], "\\multic{--}")
  expect_equal(out[["sysmis.total"]], "0.0")
  expect_equal(out[["sysmis.totalabs"]], "0")
  expect_equal(names(out)[3:5], c("mean.valid", "sd.valid", "1.valid"))
})

test_that("descriptives ordinal scale", {
  value_table <- data.frame(value = c(1, 2, 3, -98, -99),
                            missings = c("valid", "valid", "valid", "miss", "miss"),
                            stringsAsFactors = FALSE)
  out <- suppressWarnings(kennwerte.ordinal.skala(c(3, 3, 1, 2, -99), value_table))
  expect_equal(out[["N.valid"]], "4")
  expect_equal(out[["N.total"]], "5")
  expect_equal(out[["mean.valid"]], "2.25")
  expect_equal(out[["sd.valid"]], "0.96")
  expect_true(!"1.valid" %in% names(out))
  expect_true(!"-98.valid" %in% names(out))
  expect_equal(out[["1.total"]], "20.0")
  expect_equal(out[["-98.total"]], "0.0")
  expect_equal(out[["-99.total"]], "20.0")
  expect_equal(out[["-99.totalabs"]], "1")
  expect_true(!"sysmis.valid" %in% names(out))
  expect_equal(out[["sysmis.total"]], "0.0")
  expect_equal(out[["sysmis.totalabs"]], "0")
  expect_equal(names(out)[3:5], c("mean.valid", "sd.valid", "1.total"))
})

test_that("descriptives metric", {
  value_table <- data.frame(value = c(1, 2, 3, -98, -99),
                            missings = c("valid", "valid", "valid", "miss", "miss"),
                            stringsAsFactors = FALSE)
  out <- kennwerte.metrisch(c(3, 3, 1, 2, -99), value_table)
  expect_equal(out[["N.valid"]], "4")
  expect_equal(out[["mean.valid"]], "2.25")
  expect_equal(out[["sd.valid"]], "0.96")
  expect_equal(out[["min.valid"]], "1.0")
  expect_equal(out[["max.valid"]], "3.0")
  expect_equal(out[["sysmis.totalabs"]], "0")
  expect_equal(names(out), c("N.valid", "mean.valid", "sd.valid", "min.valid", "max.valid", "sysmis.totalabs"))
})


### probleme

test_that("descriptives dummy", {
  df <- data.frame(id = 1:5, v1 = c(3, 3, 1, 2, -99))
  value_table <- data.frame(value = c(1, 2, 3, -98, -99),
                            missings = c("valid", "valid", "valid", "miss", "miss"),
                            stringsAsFactors = FALSE)
  out <- suppressWarnings(kennwerte.ordinal.skala(df[,"v1"], value_table))
})


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




