
gads <- readRDS(test_path("helper_scaleDF.RDS"))
gads_catPV <- eatGADS::import_spss(test_path("helper_clean.sav"))
input_catPV <- readRDS(test_path("helper_inputForDescriptives_clean.RDS"))
varInfo_catPV <- readRDS(test_path("helper_varInfo_clean.RDS"))

full_varInfo_path <- system.file("extdata", "example_varInfo.xlsx", package = "eatCodebook")
full_varInfo <- getVarInfo(full_varInfo_path)
full_example_path <- system.file("extdata", "example1_clean.sav", package = "eatCodebook")
full_example <- eatGADS::import_spss(full_example_path)
full_input <- createInputForDescriptives(full_example)



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

test_that("infer layout pseudo scale", {
  suppressMessages(gads_b <- eatGADS::removeVars(gads, "constr"))
  suppressMessages(gads2 <- eatGADS::checkFormat(gads_b))
  #gads2$labels
  suppressWarnings(input <- createInputForDescriptives(gads2, fakeItemExpr = "Construct"))
  input$scale[1] <- NA
  input$group[2:4] <- "constr"
  varInfo <- createVarInfo(gads2, input)
  out <- inferLayout(varInfo, GADSdat = gads2, inputForDescriptives = input)
  expect_equal(out$Layout, c(0, 5, NA, NA, NA))
})

test_that("infer layout pseudo ordinal scale", {
  suppressMessages(gads_b <- eatGADS::removeVars(gads, "constr"))
  suppressMessages(gads2 <- eatGADS::checkFormat(gads_b))
  #gads2$labels
  input <- createInputForDescriptives(gads2)
  input$scale[1] <- NA
  input$scale[2:4] <- "ordinal"
  input$group[2:4] <- "constr"
  varInfo <- createVarInfo(gads2, input)
  out <- inferLayout(varInfo, GADSdat = gads2, inputForDescriptives = input)
  expect_equal(out$Layout, c(0, 11, NA, NA, NA))
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
  expect_equal(out$Layout[15], c(11))
})

test_that("infer layout full example", {
  out <- inferLayout(full_varInfo, GADSdat = full_example, inputForDescriptives = full_input)
  expect_equal(out$Layout[1:5], c(1, 0, 4, 3, 1)) # normal variables
  expect_equal(out$Layout[c(10, 16, 22)], c(6, 11, 7)) # imputed variables
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
