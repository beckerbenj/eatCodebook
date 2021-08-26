
#varInfo_pisa <- readRDS("tests/testthat/helper_varinfo_pisa.RDS")
varInfo_pisa <- readRDS("helper_varinfo_pisa.RDS")

#load("tests/testthat/helper_varinfo.rda")
load("helper_varinfo.rda")

test_that("for standard data.frame", {
  out <- createRegister(varinfo, keywordList = c("KW1", "KW2"))
  expect_equal(names(out), c("Nr", "varName", "KW1", "KW2"))
  expect_equal(out$Nr, seq(length(unique(varinfo$group))))
  expect_equal(out$varName, unique(varinfo$group))
  expect_equal(out$KW1, rep(NA, 7))
  expect_equal(out$KW2, rep(NA, 7))
})


test_that("for pisa data.frame", {
  out <- createRegister(varInfo_pisa, keywordList = c("KW1", "KW2"))
  expect_equal(names(out), c("Nr", "varName", "KW1", "KW2"))
  expect_equal(out$Nr, 1:121)
  expect_equal(out$varName[119:121], c("ma_imputed", "rea_imputed", "sci_imputed"))
})
