
#input <- readRDS("tests/testthat/helper_inputForDescriptives_clean.RDS")
#input <- readRDS("c:/Diskdrv/Winword/Psycho/IQB/Repositories/eatCodebook/tests/testthat/helper_inputForDescriptives_clean.RDS")
input <- readRDS("helper_inputForDescriptives_clean.RDS")

#dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")
#dfSAV <- eatGADS::import_spss("c:/Diskdrv/Winword/Psycho/IQB/Repositories/eatCodebook/tests/testthat/helper_spss.sav")
dfSAV <- eatGADS::import_spss("helper_spss.sav")

test_that("with pisa data", {
  messages <- capture_messages(out <- createInputForDescriptives(eatGADS::pisa, impExpr = "Plausible Value", nCatsForOrdinal = c(2:7)))
  expect_equal(names(out), c("varName", "varLabel", "format", "imp", "type", "scale", "group"))
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
  mess <- capture_messages(out <-  createInputForDescriptives(gads, verbose = FALSE))
  expect_equal(names(out), c("varName", "varLabel", "format", "imp", "type", "scale", "group"))
  expect_equal(out$type, c(rep("variable", 4), "scale"))
  expect_equal(unique(out$imp), c(FALSE))
})

test_that("check inputForDescriptives", {
  input5 <- input4 <- input3 <- input2 <- input1 <- input
  expect_silent(check_inputForDescriptives(input))
  input2 <- input2[, -2]
  expect_error(check_inputForDescriptives(input2),
               "The column names of 'inputForDescriptives' need to be: 'varName', 'varLabel', 'format', 'imp', 'type', 'scale', 'group'.")
  input3[2, "imp"] <- "a"
  expect_error(check_inputForDescriptives(input3),
               "The column 'imp' in 'inputForDescriptives' must be logical.")
  input4[2, "type"] <- "a"
  expect_error(check_inputForDescriptives(input4),
               "The column 'type' in 'inputForDescriptives' can only contain the entries 'variable' and 'scale'.")
  input5[2, "scale"] <- "a"
  expect_error(check_inputForDescriptives(input5),
               "The column 'scale' in 'inputForDescriptives' can only contain the entries 'numeric', 'ordinal', 'nominal'.")
})

test_that("list input and default options", {
  l <- list(dat1 = dfSAV, dat2 = dfSAV)
  out <- createInputForDescriptives(l)
  expect_equal(names(out), c("dat1", "dat2"))
})


gd1   <- eatGADS::import_spss("example1_clean.sav")

test_that("checkItemScaleConsistency", {
  inputForDescr2 <- inputForDescr <- createInputForDescriptives(gd1)
  inputForDescr[6:8,"scale"] <- "nominal"
  expect_error(out <- capture_output(checkItemScaleConsistency(inputForDescr)))

  inputForDescr2[7,"scale"] <- NA
  expect_error(out <- capture_output(checkItemScaleConsistency(inputForDescr2)))
})
