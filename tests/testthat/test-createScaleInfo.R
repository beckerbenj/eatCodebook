
#input_pisa <- readRDS("tests/testthat/helper_inputForDescriptives_pisa.RDS")
input_pisa <- readRDS("helper_inputForDescriptives_pisa.RDS")
#input_clean <- readRDS("tests/testthat/helper_inputForDescriptives_clean.RDS")
input_clean <- readRDS("helper_inputForDescriptives_clean.RDS")

test_that("single scale", {
  out <- createScaleInfo(input_clean)
  expect_equal(nrow(out), 1)
  expect_equal(out$varName, "skala1")
  expect_equal(out$Items_der_Skala, "constr_1,constr_2,constr_3")
})

test_that("no scales", {
  out <- createScaleInfo(input_pisa)
  expect_equal(nrow(out), 0)
})

test_that("with input as list", {
  l1 <- list(pisa = input_pisa, other = input_clean)
  out <- createScaleInfo(l1)
  expect_equal(nrow(out), 1)
  expect_equal(out$Quelle, "other")

  l2 <- list(other1 = input_pisa, other2 = input_clean)
  out2 <- createScaleInfo(l2)
  expect_equal(nrow(out2), 2)
  expect_equal(out2$Quelle, c("other1", "other2"))
})

