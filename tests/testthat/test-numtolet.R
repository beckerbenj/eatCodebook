
test_that("numtolet", {
  out <- numtolet("var1", fb = "Data set", double.vars = c())
  expect_equal(out, "varNUMBERONE")

  out2 <- numtolet("var5", fb = "Data set", double.vars = c())
  expect_equal(out2, "varNUMBERFIVE")

  out3 <- numtolet("var5", fb = "Data set", double.vars = c("var5"))
  expect_equal(out3, "varNUMBERFIVEdataSPACEset")

  out4 <- numtolet("var5 \U00DF", fb = "Data set", double.vars = c("var5"))
  expect_equal(out4, "varNUMBERFIVESPACEss")
})
