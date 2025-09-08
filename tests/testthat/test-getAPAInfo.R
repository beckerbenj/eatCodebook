
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
  expect_error(references <- getAPAInfo("helper_getAPAInfo_error2.xlsx"))
  expect_access(references <- getAPAInfo("helper_getAPAInfo_error3.xlsx"))
})

### test output for getAPAInfo

references <- getAPAInfo("helper_getAPAInfo_2sheets.xlsx")

test_that("test output for getAPAInfo", {
  references <- getAPAInfo("helper_getAPAInfo_2sheets.xlsx")

  expect_true(test_data_frame(references, types = "character", ncols = 2))
  expect_true(identical(names(references), c("Kurzangabe", "Langangabe")))

  # maybe add tests for the strings/latex code?
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


