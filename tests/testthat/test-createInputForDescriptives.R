


test_that("with pisa data", {
  out <- createInputForDescriptives(eatGADS::pisa, impExpr = "Plausible Value")
  expect_equal(names(out), c("varName", "varLabel", "imp", "type", "scale", "group"))
  expect_equal(unique(out$type), "variable")
})


df <- data.frame(id = 1:4,
           constr_1 = c(3, 2, 1, 4),
           constr_2 = c(3, 2, 1, 4),
           constr_3 = c(3, 2, 1, 4),
           constr = c(3, 2, 1, 4))
gads <- eatGADS::import_DF(df)
gads <- eatGADS::changeVarLabels(gads, varName = c("constr_1", "constr_2", "constr_3", "constr"),
                        c("Construct Item 1", "Construct Item 2", "Construct Item 3", "Skala Construct"))
#saveRDS(gads, "tests/testthat/helper_scaleDF.RDS")


test_that("with scale", {
  out <-  createInputForDescriptives(gads, verbose = FALSE)
  expect_equal(names(out), c("varName", "varLabel", "imp", "type", "scale", "group"))
  expect_equal(out$type, c("scale", rep("variable", 4)))
  expect_equal(unique(out$imp), c(FALSE))
})
