
#varInfo <- readRDS("tests/testthat/helper_varinfo_pisa.RDS")
varInfo <- readRDS("helper_varinfo_pisa.RDS")

test_that("for data.frame", {
  out <- createRegister(varInfo, keywordList = c("KW1", "KW2"))
  expect_equal(names(out), c("Nr", "varName", "KW1", "KW2"))
  expect_equal(out$Nr, 1:121)
  expect_equal(out$Nr, 1:121)
  expect_equal(out$varName[119:121], c("ma_imputed", "rea_imputed", "sci_imputed"))
})
