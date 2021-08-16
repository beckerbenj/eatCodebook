
#varInfo_pisa <- readRDS("tests/testthat/helper_varinfo_pisa.RDS")
varInfo_pisa <- readRDS("helper_varinfo_pisa.RDS")
#gads <- readRDS("tests/testthat/helper_scaleDF.RDS")
gads <- readRDS("helper_scaleDF.RDS")
varInfo_scale <-  createInputForDescriptives(gads, verbose = FALSE)


test_that("single scale", {
  out <- createScaleInfo(varInfo_scale)
  expect_equal(nrow(out), 1)
  expect_equal(out$varName, "constr")
  expect_equal(out$Items_der_Skala, "constr_1,constr_2,constr_3")
})

test_that("no scales", {
  out <- createScaleInfo(varInfo_pisa)
  expect_equal(nrow(out), 0)
})

test_that("with input as list", {
  l1 <- list(pisa = varInfo_pisa, other = varInfo_scale)
  out <- createScaleInfo(l1)
  expect_equal(nrow(out), 1)
  expect_equal(out$Quelle, "other")

  l2 <- list(other1 = varInfo_scale, other2 = varInfo_scale)
  out2 <- createScaleInfo(l2)
  expect_equal(nrow(out2), 2)
  expect_equal(out2$Quelle, c("other1", "other2"))
})

