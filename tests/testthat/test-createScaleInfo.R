
input_pisa <- readRDS(test_path("helper_inputForDescriptives_pisa.RDS"))
input_clean <- readRDS(test_path("helper_inputForDescriptives_clean.RDS"))
input_clean2 <- readRDS(test_path("helper_inputForDescriptives_clean2.RDS"))
input_imputedScale <- readRDS(test_path("helper_inputedForDescriptives_imputedScale.RDS"))


test_that("single scale", {
  out <- createScaleInfo(input_clean)
  expect_equal(nrow(out), 3)
  expect_equal(out$varName, c("skala1", "pv_pooled", "pv_kat_pooled"))
  expect_equal(out$Items_der_Skala[1], c("skala1_item1,skala1_item2,skala1_item3"))
  expect_equal(out$Items_der_Skala[2], "")
  expect_equal(out$Imputationen[2], paste(paste0("pv_", 1:5), collapse = ","))
})

test_that("fake scale", {
  out <- createScaleInfo(input_clean2)
  expect_equal(nrow(out), 4)
  expect_equal(out$Items_der_Skala[2], paste(paste0("skala_fake_item", 1:3), collapse = ","))
})


test_that("no scales, only imputed", {
  out <- createScaleInfo(input_pisa)
  expect_equal(nrow(out), 3)
  expect_equal(out$Imputationen[1], c("ma_pv1,ma_pv2,ma_pv3,ma_pv4,ma_pv5"))
  expect_equal(out$Imputationen[2], c("rea_pv1,rea_pv2,rea_pv3,rea_pv4,rea_pv5"))
})

test_that("with input as list", {
  l1 <- list(pisa = input_pisa, other = input_clean)
  out <- createScaleInfo(l1)
  expect_equal(names(out), c("varName", "Quelle", "Anzahl_valider_Werte", "Items_der_Skala", "Imputationen"))
  expect_equal(nrow(out), 6)
  expect_equal(out$Quelle, c(rep("pisa", 3), rep("other", 3)))

  l2 <- list(other1 = input_clean, other2 = input_clean)
  out2 <- createScaleInfo(l2)
  expect_equal(nrow(out2), 6)
  expect_equal(out2$Quelle, rep(c("other1", "other2"), each = 3))
})

test_that("imputed scale", {
  out <- createScaleInfo(input_imputedScale)
  expect_equal(nrow(out), 1)
  expect_equal(out$varName, "Sinmo_pooled")
})
