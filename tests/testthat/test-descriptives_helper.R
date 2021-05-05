

#load("q:/BT2016/BT/02_Organisation/81_StuMi_Arbeitsordner/Skalenhandbuch/03_Kennwerte/Kennwerte_sfb.rdata")
#Kennwertedatensatz$TR_SEX
#kennwerte.kategorial(ds$sfb$TR_SEX, c(-94,-99))

test_that("descriptives categorical", {
  df <- data.frame(id = 1:5, v1 = c(1, 3, -99, -98, NA))
  value_table <- data.frame(value = c(1, 2, 3, -98, -99),
                            missings = c("valid", "valid", "valid", "miss", "miss"),
                            stringsAsFactors = FALSE)
  out <- kennwerte.kategorial(df$v1, value_table)
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
  expect_equal(names(out)[3:7], c("1.valid", "2.valid", "3.valid", "-98.valid", "-99.valid"))
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
  expect_equal(names(out)[3:6], c("a.valid", "b.valid", "-98.valid", "-99.valid"))
})


test_that("descriptives categorical no labels", {
  value_table <- data.frame(value = c(-98, -99),
                            missings = c("miss", "miss"),
                            stringsAsFactors = FALSE)
  out <- kennwerte.kategorial(c(-99, -98, 1, 5), value_table)
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
  expect_equal(names(out)[3:6], c("1.valid", "5.valid", "-98.valid", "-99.valid"))
})

test_that("descriptives ordinal", {
  value_table <- data.frame(value = c(1, 2, 3, -98, -99),
                            missings = c("valid", "valid", "valid", "miss", "miss"),
                            stringsAsFactors = FALSE)
  out <- kennwerte.ordinal(c(3, 3, 1, 2, -99), value_table)

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
  out <- kennwerte.ordinal.skala(c(3, 3, 1, 2, -99), value_table)

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


test_that("descriptives dummy", {
  df <- data.frame(id = 1:5, v1 = c(3, 3, 1, 2, -99))
  value_table <- data.frame(Wert = c(1, 2, 3, -98, -99),
                            Var.name = "v1",
                            missing = c("nein", "nein", "nein", "ja", "ja"),
                            stringsAsFactors = FALSE)
  out <- kennwerte.ordinal.skala("v1", value_table, df)
})

# test fuer kennwerte.skala
# originalobjekte mit felix alter syntax erzeugt und im Paketverzeichnis unter tests/testthat gespeichert
load("out.rda")
load("dat.rda")
load("results_gepoolt_metrisch.rda")
load("kennwerte.skala.fake.rda")
test_that("descriptives scale", {
  skalen.info <- data.frame ( Var.Name = "DM_erfahrung", Quelle = "sfb", Items.der.Skala = paste("Semz19_", letters[1:4], sep="", collapse=", ") , stringsAsFactors = FALSE)
  value_table <- data.frame(value = c(1, 2, 3, 4, -98, -99),
                            missings = c("valid", "valid", "valid", "valid", "miss", "miss"),
                            stringsAsFactors = FALSE)
  varue_missings <- data.frame ( "Var.name" = paste("Semz19_", letters[1:4], sep="", collapse=", "), Wert = rep(c(-98, -99), 3), missing = "ja", stringsAsFactors = FALSE)
  out1 <- kennwerte.skala(dat=dat, scaleCol = "DM_erfahrung", c("Semz19_a", "Semz19_b", "Semz19_c", "Semz19_d"), missingValues = c(-98,-99))
  expect_equal(out, out1)
  out2 <- kennwerte.skala.fake(dat=dat, variableCols = c("Semz19_a", "Semz19_b", "Semz19_c", "Semz19_d"), missingValues = c(-98,-99))
  expect_equal(out2, ret2)
  out3 <- kennwerte.gepoolt.metrisch ( name="DM_erfahrung" , id.fb="IDSTUD" , Gesamtdatensatz=dat, skalen.info=skalen.info)
  expect_equal(out3, results.gepoolt.metrisch)
})




