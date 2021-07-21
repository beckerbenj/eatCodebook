


test_that("multiplication works", {
  #out <- makeAbbrList("tests/testthat/helper_abbrList.xlsx")
  out <- makeAbbrList("helper_abbrList.xlsx")
  expect_equal(length(out), 22)
  expect_equal(out[1], "\\clearpage")
  expect_equal(out[22], "\\end{longtabu}\n")
  expect_equal(out[20], "Min. & Minimum\\\\")
  expect_equal(out[12], "\\textbf{Symbol} & \\textbf{Bedeutung}\\\\")
})



# test <- makeAbbrList(filePath = "other_code/abkuerzungsverzeichnis.xlsx")
# abkuerzverz <- makeAbkVerz(varue.file="other_code/varue.xlsx",
#                            sheets=c("Akronyme" , "Statistische Formelzeichen") ,
#                            headings=list("Akronyme"=c( "Abkürzung", "Bedeutung") ,
#                                          "Statistische Formelzeichen"=c( "Symbol", "Bedeutung")) ,
#                            captions=list("Akronyme"=c( "Abkürzungen") , "Statistische Formelzeichen"=c( "Statistische Formelzeichen"))  )
#
# all.equal(test, abkuerzverz)
#
# View(data.frame(neu = test, alt = abkuerzverz))
