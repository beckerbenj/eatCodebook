

test_that("check scaleInfo", {
  expect_error(check_scaleInfo(2),
               "'scaleInfo' needs to be a data.frame.")
  expect_error(check_scaleInfo(mtcars),
               "The column names of 'scaleInfo' need to be: 'varName', 'Anzahl_valider_Werte', 'Items_der_Skala', 'Imputationen'.")
})

test_that("get scale info", {
  out <- getScaleInfo(test_path("helper_scaleInfo.xlsx"))

  out2 <- getScaleInfo(test_path("helper_scaleInfo2.xlsx"))
  expect_equal(length(out2), 2)
  expect_equal(names(out2), c("data1", "data2"))
})
