
#gads <- readRDS("tests/testthat/helper_scaleDF.RDS")
gads <- readRDS("helper_scaleDF.RDS")
#gads_catPV <- eatGADS::import_spss("tests/testthat/helper_clean.sav")
gads_catPV <- eatGADS::import_spss("helper_clean.sav")
#input_catPV <- readRDS("tests/testthat/helper_inputForDescriptives_clean.RDS")
input_catPV <- readRDS("helper_inputForDescriptives_clean.RDS")
#varInfo_catPV <- readRDS("tests/testthat/helper_varInfo_clean.RDS")
varInfo_catPV <- readRDS("helper_varInfo_clean.RDS")


test_that("infer layout errors", {
  outpu <- capture_output(suppressMessages(input <- createInputForDescriptives(gads)))
  varInfo <- createVarInfo(gads, input)
  expect_error(inferLayout(varInfo, GADSdat = gads, inputForDescriptives = input),
               "'format' information is missing in 'GADSdat' for variable id.")

})

test_that("infer layout scale", {
  suppressMessages(gads2 <- eatGADS::checkFormat(gads))
  #gads2$labels
  outpu <- capture_output(suppressMessages(input <- createInputForDescriptives(gads2)))
  input$scale[1] <- NA
  varInfo <- createVarInfo(gads2, input)
  out <- inferLayout(varInfo, GADSdat = gads2, inputForDescriptives = input)
  expect_equal(out$Layout, c(0, NA, NA, NA, 5))
})

test_that("infer layout pisa", {
  #suppressMessages(gads2 <- eatGADS::checkFormat(gads))
  #gads2$labels
  outpu <- capture_output(suppressMessages(input <- createInputForDescriptives(eatGADS::pisa, impExpr = "Plausible Value")))
  #input$scale[1] <- NA
  varInfo <- createVarInfo(eatGADS::pisa, input)
  out <- inferLayout(varInfo, GADSdat = eatGADS::pisa, inputForDescriptives = input)
  expect_equal(out$Layout[119:124], c(6, NA, NA, NA, NA, NA))
})

test_that("infer layout imputed categorical var", {
  out <- inferLayout(varInfo_catPV, GADSdat = gads_catPV, inputForDescriptives = input_catPV)
  expect_equal(out$Layout[15], c(7))
})

test_that("infer layout list", {
  suppressMessages(gads2 <- eatGADS::checkFormat(gads))
  gadsList <- list(pisa = eatGADS::pisa, gads = gads2)
  outpu <- capture_output(suppressMessages(input <- createInputForDescriptives(gadsList, impExpr = "Plausible Value")))
  input$gads$scale[1] <- NA
  varInfo <- createVarInfo(gadsList, input)
  out <- inferLayout(varInfo, GADSdat = gadsList, inputForDescriptives = input)
  expect_equal(length(out), 2)
  expect_equal(out$pisa$Layout[119:124], c(6, NA, NA, NA, NA, NA))
  expect_equal(out$gads$Layout, c(0, NA, NA, NA, 5))
})
