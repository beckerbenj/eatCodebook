
#input_pisa <- readRDS("tests/testthat/helper_inputForDescriptives_pisa.RDS")
input_pisa <- readRDS("helper_inputForDescriptives_pisa.RDS")

#input_clean <- readRDS("tests/testthat/helper_inputForDescriptives_clean.RDS")
input_clean <- readRDS("helper_inputForDescriptives_clean.RDS")


test_that("for standard data.frame", {
  out <- createRegister(input_clean, keywordList = c("KW1", "KW2"))
  vars_in_cb <- length(unique(input_clean$group))
  expect_equal(names(out), c("Nr", "varName", "KW1", "KW2"))
  expect_equal(out$Nr, seq(vars_in_cb))
  expect_equal(out$varName, unique(input_clean$group))
  expect_equal(out$KW1, rep(NA, vars_in_cb))
  expect_equal(out$KW2, rep(NA, vars_in_cb))
})


test_that("for pisa data.frame", {
  out <- createRegister(input_pisa, keywordList = c("KW1", "KW2"))
  expect_equal(names(out), c("Nr", "varName", "KW1", "KW2"))
  expect_equal(out$Nr, 1:121)
  expect_equal(out$varName[119:121], c("ma_pooled", "rea_pooled", "sci_pooled"))
})
