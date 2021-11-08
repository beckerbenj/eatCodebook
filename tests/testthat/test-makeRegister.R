
test_that("regular makeRegister", {
  # reg <- getRegister("tests/testthat/helper_register.xlsx")
  reg <- getRegister("helper_register.xlsx")
  mess <- capture_messages(out <- makeRegister(fblong = "Data set 1", fb.akt = "df1", varue.reg = reg[["df1"]], double.vars = character()))

  expect_equal(mess[1], "Register (df1) - Schlagwort: keyword1\n")
  expect_equal(mess[2], "Register (df1) - Schlagwort: keyword2\n")

  expect_equal(out[1], "\\phantomsection")
  expect_equal(out[2], "\\section*{Register: Data set 1}")
  expect_equal(out[13], "\\pagebreak")
})
