
#dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")
dfSAV <- eatGADS::import_spss("helper_spss.sav")
#gads <- readRDS("tests/testthat/helper_scaleDF.RDS")
gads <- readRDS("helper_scaleDF.RDS")
#netw_g <- readRDS("tests/testthat/helper_data_netw.RDS")
netw_g <- readRDS("helper_data_netw.RDS")
#netw_input <- readRDS("tests/testthat/helper_inputForDescriptives_netw.RDS")
netw_input <- readRDS("helper_inputForDescriptives_netw.RDS")

test_that("insertRow", {
# tbd
  comp1 <- rbind(mtcars[1:3, ], mtcars[1:3, ], mtcars[4:nrow(mtcars), ])
  out1 <- insertRows(mtcars, mtcars[1:3, ], 4)
  rownames(comp1) <- rownames(out1) <- NULL
  expect_equal(out1, comp1)

  comp2 <- rbind(mtcars[1:7, ], mtcars[5, ], mtcars[8:nrow(mtcars), ])
  out2 <- insertRows(mtcars, mtcars[5, ], 8)
  rownames(comp2) <- rownames(out2) <- NULL
  expect_equal(out2, comp2)
})


test_that("with pisa", {
  suppressMessages(input4descr <- createInputForDescriptives(eatGADS::pisa, impExpr = "Plausible Value"))
  out <- createVarInfo(eatGADS::pisa, input4descr)
  expect_true(all(eatGADS::namesGADS(eatGADS::pisa) %in% out$Var.Name))
  expect_true(all(c("ma_pooled", "rea_pooled", "sci_pooled") %in% out$Var.Name))
  expect_equal(out$in.DS.und.SH[1], "ja")
  expect_equal(out$Reihenfolge, rep(NA, nrow(out)))
  expect_equal(out$Unterteilung.im.Skalenhandbuch, rep(NA, nrow(out)))
  expect_equal(sum(out$in.DS.und.SH == "sh"), 3)
  expect_equal(sum(out$in.DS.und.SH == "ds"), 3 * 5)
  expect_equal(out$LabelSH[1], "Student-ID")
  expect_equal(unique(out$Gliederung), "-")

  expect_equal(out[119:124, ]$in.DS.und.SH, c("sh", rep("ds", 5)))
  expect_equal(out[119:124, ]$Titel, c(NA, rep("-", 5)))
  expect_equal(out[119, ]$LabelSH, NA_character_)
  expect_equal(out[120, ]$LabelSH, "Math Achievement (Plausible Value 1) (T1)")
  #expect_equal(out$Titel, out$LabelSH)
})

test_that("with network data", {
  out <- createVarInfo(netw_g, netw_input)
  expect_equal(out$Var.Name, c("id", "friend", "friend_1", "friend_2"))
  expect_equal(out$in.DS.und.SH, c("ja", "sh", "ds", "ds"))
})

test_that("with scale data", {
  outpu <- capture_output(suppressMessages(input <- createInputForDescriptives(gads)))
  out <- createVarInfo(gads, input)
  expect_equal(out$Var.Name, c("id", "constr_1", "constr_2", "constr_3", "constr"))
  expect_equal(out$in.DS.und.SH, c("ja", "ds", "ds", "ds", "ja"))
  expect_equal(out$Titel, c(NA, "-", "-", "-", "Skala Construct"))
  expect_equal(out$Gliederung, rep("-", 5))
})

test_that("with list", {
  l1 <- list(pisa = eatGADS::pisa, other = dfSAV)
  suppressMessages(inputList <- createInputForDescriptives(l1, impExpr = "Plausible Value"))
  out <- createVarInfo(l1, inputList)
  expect_equal(names(out), c("pisa", "other"))
  expect_true(all(eatGADS::namesGADS(eatGADS::pisa) %in% out[[1]]$Var.Name))
  expect_true(all(c("ma_pooled", "rea_pooled", "sci_pooled") %in% out[[1]]$Var.Name))
  expect_equal(out[[2]]$Var.Name, eatGADS::namesGADS(dfSAV))
})

test_that("input validation with list", {
  l1 <- list(pisa = eatGADS::pisa, other = dfSAV)
  suppressMessages(inputList <- createInputForDescriptives(l1, impExpr = "Plausible Value"))
  expect_error(createVarInfo(l1, inputList[1]),
              "'GADSdat' and 'inputForDescriptives' lists have different lengths.")

})


