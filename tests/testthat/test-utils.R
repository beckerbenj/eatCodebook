
test_that("quotation marks", {
  out <- subQuotationMarks(c('This contains no quotation marks.', 'This contains "quotation" marks.',
                             'This contains "multiple" "quotation" "marks".'))
  expect_equal(out[1], 'This contains no quotation marks.')
  expect_equal(out[2], 'This contains \\glqq quotation\\grqq{} marks.')
  expect_equal(out[3], 'This contains \\glqq multiple\\grqq{} \\glqq quotation\\grqq{} \\glqq marks\\grqq{}.')

  expect_error(subQuotationMarks(c('This is a "problem.')),
                                 'Detected an uneven number of quotation marks in: This is a "problem.')
  })


test_that("special signs", {
  out <- sonderzeichen.aufbereiten(c("a", "b", "x%", "\\", "ab#a_"))
  expect_equal(out, c("a", "b", "x\\%", "\\", "ab\\#a\\_"))
})


test_that("Latex length", {
  out1 <- Latex.length("Test", bold = FALSE)
  expect_equal(out1, 18.85557, tolerance = 0.0001)
  out2 <- Latex.length("Test", bold = TRUE)
  expect_equal(out2, 22.07811, tolerance = 0.0001)

  out3 <- Latex.length("T\U00E4st", bold = FALSE)
  expect_equal(out3, 18.85557, tolerance = 0.0001)
  out4 <- Latex.length("T\U00E4st", bold = TRUE)
  expect_equal(out4, 22.75271, tolerance = 0.0001)

  out5 <- Latex.length("\U00DC_ber", bold = FALSE)
  expect_equal(out5, 27.36385, tolerance = 0.0001)
  out6 <- Latex.length("\U00DC_ber", bold = TRUE)
  expect_equal(out6, 32.11164, tolerance = 0.0001)
})
