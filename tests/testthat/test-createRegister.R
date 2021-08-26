
#input_pisa <- readRDS("tests/testthat/helper_inputForDescriptives_pisa.RDS")
input_pisa <- readRDS("helper_inputForDescriptives_pisa.RDS")

#input_clean <- readRDS("tests/testthat/helper_inputForDescriptives_clean.RDS")
input_clean <- readRDS("helper_inputForDescriptives_clean.RDS")


test_that("for standard data.frame", {
  out <- createRegister(input_clean, keywordList = c("KW1", "KW2"))
  expect_equal(names(out), c("Nr", "varName", "KW1", "KW2"))
  expect_equal(out$Nr, seq(length(unique(input_clean$group))))
  expect_equal(out$varName, unique(input_clean$group))
  expect_equal(out$KW1, rep(NA, 22))
  expect_equal(out$KW2, rep(NA, 22))
})


test_that("for pisa data.frame", {
  out <- createRegister(varInfo_pisa, keywordList = c("KW1", "KW2"))
  expect_equal(names(out), c("Nr", "varName", "KW1", "KW2"))
  expect_equal(out$Nr, 1:121)
  expect_equal(out$varName[119:121], c("ma_imputed", "rea_imputed", "sci_imputed"))
})
