
#gads <- readRDS("tests/testthat/helper_scaleDF.RDS")
gads <- readRDS("helper_scaleDF.RDS")

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
