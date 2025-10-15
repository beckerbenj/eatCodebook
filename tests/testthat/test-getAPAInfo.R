
### test if proper sheet is selected (BT examples)

test_that("Import Excel with one sheet, select proper sheet", {
  references <- getAPAInfo("helper_getAPAInfo_1sheet.xlsx")

  expect_true(is.data.frame(references))
  expect_true(identical(names(references), c("Kurzangabe", "Langangabe")))
})

test_that("Import Excel with two sheets, select proper sheet", {
  references <- getAPAInfo("helper_getAPAInfo_2sheets.xlsx")

  expect_true(is.data.frame(references))
  expect_true(identical(names(references), c("Kurzangabe", "Langangabe")))
})

### tests for improper Excel files

test_that("Import Excel without reference sheet", {
  expect_error(references <- getAPAInfo("helper_getAPAInfo_error1.xlsx"))
})

test_that("Import Excel with two reference sheets", {
  # file with at least 3 sheets, two reference sheets
  expect_error(references <- getAPAInfo("helper_getAPAInfo_error2.xlsx"))
  # file with just two reference sheets
  references <- getAPAInfo("helper_getAPAInfo_error3.xlsx")
  ## selects first sheet
  ref <- c("Hertel, S., Hochweber, J., Mildner, D., Steinert, B. & Jude, N. (2014). \\textit{PISA 2009 Skalenhandbuch.} Waxmann. \\urlstyle{same}\\url{https://doi.org/10.25656/01:9554}",
           "Lütke, B., Paetsch, J. & Dubiel, S. (2017-2019). \\textit{Selbsteingeschätztes Wissen im Bereich Sprachbildung} (unveröffentlicht). Projekt: Sprachsensibles Unterrichten fördern – Sprachliche Bildung systemisch im Vorbereitungsdienst implementieren. Bericht der wissenschaftlichen Begleitforschung.")
  expect_equal(references$Langangabe, ref)
})

### test output for getAPAInfo

test_that("test output for getAPAInfo", {
  references <- getAPAInfo("helper_getAPAInfo_2sheets.xlsx")
  # str of data frame
  expect_true(checkmate::test_data_frame(references, types = "character", ncols = 2))
  expect_true(identical(names(references), c("Kurzangabe", "Langangabe")))
  # references
  ref <- c("Hertel, S., Hochweber, J., Mildner, D., Steinert, B. & Jude, N. (2014). \\textit{PISA 2009 Skalenhandbuch.} Waxmann. \\urstyle{same}\\url{https://doi.org/10.25656/01:9554}",
           "Lütke, B., Paetsch, J. & Dubiel, S. (2017-2019). \\textit{Selbsteingeschätztes Wissen im Bereich Sprachbildung} (unveröffentlicht). Projekt: Sprachsensibles Unterrichten fördern – Sprachliche Bildung systemisch im Vorbereitungsdienst implementieren. Bericht der wissenschaftlichen Begleitforschung.")
  expect_equal(references$Langangabe, ref)
})


### test addLatex_italic and addLatex_URL

test_that("test addLatex_italic", {
  string <- "example"
  string_latex <- addLatex_italic(string)
  expect_equal(string_latex, "\\textit{example}")

  vector <- c("example", "example")
  vector_latex <- addLatex_italic(vector)
  expect_equal(vector_latex, c("\\textit{example}", "\\textit{example}"))
})

test_that("test addLatex_URL", {
  link <- "http://thisisalink.de"
  link_latex <- addLatex_URL(link)
  expect_equal(link_latex, "\\urstyle{same}\\url{http://thisisalink.de}")

  vector <- c("http://thisisalink.de", "https://thisisalink.de")
  vector_latex <- addLatex_URL(vector)
  expect_equal(vector_latex, c("\\urstyle{same}\\url{http://thisisalink.de}", "\\urstyle{same}\\url{https://thisisalink.de}"))
})


