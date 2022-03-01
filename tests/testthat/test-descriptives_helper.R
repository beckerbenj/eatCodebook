

#load("q:/BT2016/BT/02_Organisation/81_StuMi_Arbeitsordner/Skalenhandbuch/03_Kennwerte/Kennwerte_sfb.rdata")
#Kennwertedatensatz$TR_SEX
#kennwerte.kategorial(ds$sfb$TR_SEX, c(-94,-99))

file <- system.file("extdata", "example1_clean.sav", package = "eatCodebook")
gd   <- eatGADS::import_spss(file)

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

test_that("descriptives pooled categorical", {
  imputed_cols <- data.frame(imp1 = c(1, 2, 1, 2), imp2 = c(2, 2, 1, 1))
  out <- kennwerte.gepoolt.kategorial(imputed_cols, imputedVariableCols = c("imp1", "imp2"), verbose = FALSE)
  expect_equal(out[["N.valid"]], "4")
  expect_equal(out[["1.valid"]], "50.0")
  expect_equal(out[["2.valid"]], "50.0")
  expect_equal(out[["1.totalabs"]], "2")
    expect_equal(out[["sysmis.totalabs"]], "0")
  expect_equal(names(out), c("N.valid", "N.total", "1.valid", "2.valid", "sysmis.valid", "1.total", "2.total", "sysmis.total", "1.totalabs", "2.totalabs", "sysmis.totalabs"))

  imputed_cols <- data.frame(imp1 = c(1, 2, 1, NA), imp2 = c(2, 2, 1, NA))
  suppressWarnings(out2 <- kennwerte.gepoolt.kategorial(imputed_cols, imputedVariableCols = c("imp1", "imp2"), verbose = FALSE))
  expect_equal(out2[["N.valid"]], "3")
  expect_equal(out2[["1.valid"]], "50.0")
  expect_equal(out2[["2.valid"]], "50.0")
  expect_equal(out2[["1.totalabs"]], "2")
  expect_equal(out2[["sysmis.totalabs"]], "1")
  expect_equal(names(out2), c("N.valid", "N.total", "1.valid", "2.valid", "sysmis.valid", "1.total", "2.total", "sysmis.total", "1.totalabs", "2.totalabs", "sysmis.totalabs"))
})

test_that("descriptives metric scale", {
  inputForDescr <- createInputForDescriptives(gd)
  sub_inputForDescr <- inputForDescr[inputForDescr$group == "skala1", ]
  out <- kennwerte.skala(gd, sub.inputForDescriptives = sub_inputForDescr, verbose = FALSE)
  expect_equal(out[[1]][["N.valid"]], "9")
  expect_equal(out[[1]][["mean.valid"]], "2.52")
  expect_equal(out[[1]][["sd.valid"]], "0.53")
  expect_equal(out[[1]][["min.valid"]], "1.7")
  expect_equal(out[[1]][["max.valid"]], "3.3")
  expect_equal(out[[1]][["sysmis.totalabs"]], "0")
  expect_equal(out[[1]][["alpha"]], "-.68")

  expect_equal(as.character(out[[2]][1, ]), rep("9", 3))
  expect_equal(as.character(out[[2]]["mean.valid", ]), c("2.78", "2.11", "2.89"))
  expect_equal(as.character(out[[2]]["cor.valid", ]), c(".00", ".61", ".09"))
})


