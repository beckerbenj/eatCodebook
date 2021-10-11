

test_that("check scaleInfo", {
  expect_error(check_scaleInfo(2),
               "'scaleInfo' needs to be a data.frame.")
  expect_error(check_scaleInfo(mtcars),
               "The column names of 'scaleInfo' need to be: 'varName', 'Quelle', 'Anzahl_valider_Werte', 'Quelle'.")
})
