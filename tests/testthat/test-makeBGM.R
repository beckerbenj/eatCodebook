
#dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")
dfSAV <- eatGADS::import_spss("helper_spss.sav")


test_that("single data.frame", {
  suppressMessages(input4descr <- createInputForDescriptives(dfSAV))
  varueInfo <- createVarInfo(dfSAV, input4descr)
  out <- makeBGM(varueInfo)
  expect_equal(out, character())

  varueInfo2 <- varueInfo
  varueInfo2$Hintergrundmodell[1:2] <- c("ja", "ja")
  expect_message(out <- makeBGM(varueInfo2),
  "Da keine numerische Angaben vorliegen, wird nichts an der Reihenfolge geaendert.")
  expect_equal(out[1], "\\clearpage")
  expect_equal(out[17], "VAR1 & \\multil{-} & Variable 1 \\\\")
  expect_equal(out[18], "VAR2 & \\multil{-} & Variable 2 \\\\")
  expect_equal(out[20], "\\end{xltabular}")
})


test_that("multiple data.frame", {
  l <- list(dat1 = dfSAV, dat2 = dfSAV)
  suppressMessages(input4descr_l <- createInputForDescriptives(l))
  varueInfo_l <- createVarInfo(l, input4descr_l)
  out <- makeBGM(varueInfo_l)
  expect_equal(out, character())

  varueInfo_l2 <- varueInfo_l
  varueInfo_l2$dat1$Hintergrundmodell[1] <- "ja"
  varueInfo_l2$dat2$Hintergrundmodell[2] <- "ja"
  expect_message(out <- makeBGM(varueInfo_l2),
                 "Da keine numerische Angaben vorliegen, wird nichts an der Reihenfolge geaendert.")
  expect_equal(out[1], "\\clearpage")
  expect_equal(out[17], "VAR1 & \\multil{-} & Variable 1 \\\\")
  expect_equal(out[18], "VAR2 & \\multil{-} & Variable 2 \\\\")
})
