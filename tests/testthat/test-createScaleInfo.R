
#input_pisa <- readRDS("tests/testthat/helper_inputForDescriptives_pisa.RDS")
input_pisa <- readRDS("helper_inputForDescriptives_pisa.RDS")
#input_clean <- readRDS("tests/testthat/helper_inputForDescriptives_clean.RDS")
input_clean <- readRDS("helper_inputForDescriptives_clean.RDS")
#input_imputedScale <- readRDS("tests/testthat/helper_inputedForDescriptives_imputedScale.RDS")
input_imputedScale <- readRDS("helper_inputedForDescriptives_imputedScale.RDS")


test_that("single scale", {
  out <- createScaleInfo(input_clean)
  expect_equal(nrow(out), 2)
  expect_equal(out$varName, c("skala1", "skalenwert_fake"))
  expect_equal(out$Items_der_Skala, c("skala1_item1,skala1_item2,skala1_item3", ""))
})

test_that("no scales", {
  out <- createScaleInfo(input_pisa)
  expect_equal(nrow(out), 0)
})

test_that("with input as list", {
  l1 <- list(pisa = input_pisa, other = input_clean)
  out <- createScaleInfo(l1)
  expect_equal(nrow(out), 2)
  expect_equal(out$Quelle, rep("other",2))

  l2 <- list(other1 = input_clean, other2 = input_clean)
  out2 <- createScaleInfo(l2)
  expect_equal(nrow(out2), 4)
  expect_equal(out2$Quelle, rep(c("other1", "other2"), each=2))
})

test_that("imputed scale", {
  out <- createScaleInfo(input_imputedScale)
  expect_equal(nrow(out), 1)
  expect_equal(out$varName, "Sinmo_pooled")
})
