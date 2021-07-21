

test_that("normal", {
  litInfo <- createLitInfo()
  litInfo[1, ] <- c("Some Book", "Soem long info", "yes")
  litInfo[2, ] <- c("Some Book", "Soem long info", "yes")
  out <- makeLit(litInfo)
  expect_equal(length(out), 12)
  expect_equal(out[1], "\\phantomsection" )
  expect_equal(out[12], "\\pagebreak")
  expect_equal(out[10], "\\lititem{Soem long info}" )
})


# litInfo <- getLitInfo("other_code/literatur.xlsx")
# test <- makeLit(litInfo)
#
# lit.cols <- c( "Kurzangabe" , "Langangabe" , "in.Literaturverzeichnis" ) # Spalten im Literatur-Reiter
# sheets.varue.lit <- "Literatur"
# varue.lit <- get.lit.info(varue.file= "other_code/varue.xlsx" , sheets=sheets.varue.lit , lit.cols=lit.cols)
# literatur <- make.lit(varue.lit)
#
# all.equal(test, literatur)
