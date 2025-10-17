

gd_path <- system.file("extdata", "example1_clean.sav", package = "eatCodebook")
gd_gads   <- eatGADS::import_spss(gd_path)
gd <- eatGADS::extractData(gd_gads)

inputForDescr <- createInputForDescriptives(gd_gads, verbose = FALSE)
# Hotfix (see mail to SW):
inputForDescr[inputForDescr$imp, "type"] <- "variable"
suppressWarnings(descr  <- calculateDescriptives(gd_gads, inputForDescr, verbose = FALSE))

varInfo_path <- system.file("extdata", "example_varInfo.xlsx", package = "eatCodebook")
varInfo <- getVarInfo(varInfo_path)
varMiss_path <- system.file("extdata", "example_miss.xlsx", package = "eatCodebook")
varMiss <- getMissings(varMiss_path)


test_that("simple description for ID", {
  varInfo[1, "LabelSH"] <- "SuS-ID"
  out <- table_description_variable(name =  "ID", varue.info = varInfo, var.typ = "ID-Variable")
  expect_equal(out[1], "\\begin{tabnormallong}{Beschreibung der Variable}")
  expect_equal(out[2], "Variablenname:&ID\\\\")
  expect_equal(out[3], "Label:&SuS-ID\\\\")
  expect_equal(out[4], "\\end{tabnormallong}")
})


test_that("description for ordinal variable with source", {
  out <- table_description_variable(name =  "varOrdinal", varue.info = varInfo, varue.missings = varMiss, var.typ = "Numerisch", werte = descr$varOrdinal)
  expect_equal(out[1], "\\begin{tabnormallong}{Beschreibung der Variable}")
  expect_equal(out[2], "Variablenname:&varOrdinal\\\\")
  expect_equal(out[3], "Label:&ordinale Beispielvariable, Kompetenzstufe\\\\")
  expect_equal(out[4], "Quelle:&Mueller (2019)\\\\")
  expect_equal(out[5], "Kategorien:& 1~$=$~\\textit{sehr schlecht}; 2~$=$~\\textit{schlecht}; 3~$=$~\\textit{gut}; 4~$=$~\\textit{sehr gut}\\\\")
  expect_equal(out[6], "")
  expect_equal(out[7], "\\end{tabnormallong}")
})


test_that("simple description for metric variable with missings", {
  out <- table_description_variable(name = "varMetrisch", varue.info = varInfo, varue.missings = varMiss, var.typ="Numerisch", Gesamtdatensatz = gd,
                           werte = descr$varMetrisch , skala.items=NULL, show.kategorien=FALSE)
  expect_equal(out[1], "\\begin{tabnormallong}{Beschreibung der Variable}")
  expect_equal(out[2], "Variablenname:&varMetrisch\\\\")
  expect_equal(out[3], "Label:&metrische Beispielvariable, Kompetenzwert\\\\")
  expect_equal(out[4], "Fehlende Werte:& -98~$=$~\\textit{omission}; -99~$=$~\\textit{not reached}\\\\")
  expect_equal(out[5], "\\end{tabnormallong}")
})


test_that("simple description for pooled metric variable", {
  out <- table_description_variable(name = "pv_pooled", varue.info = varInfo, varue.missings = varMiss, var.typ="Numerisch", Gesamtdatensatz = gd,
                           werte = descr$pv_pooled, imputations = paste0("pv_", 1:5), gepoolt = TRUE)
  expect_equal(out[1], "\\begin{tabnormallong}{Beschreibung der Variable}")
  expect_equal(out[2], "Variablenname:&pv\\_pooled\\\\")
  expect_equal(out[3], "Label:&NA\\\\")
  expect_equal(out[4], "Anzahl der Imputationen: & 5\\\\")
  expect_equal(out[5], "\\end{tabnormallong}")
})

test_that("simple description for pooled categorical variable", {
  out <- table_description_variable(name = "pvkat_pooled", varue.info = varInfo, varue.missings = varMiss, var.typ="Numerisch", Gesamtdatensatz = gd,
                           werte = descr$pv_pooled, imputations = paste0("pvkat_", 1:5), gepoolt = TRUE)
  expect_equal(out[1], "\\begin{tabnormallong}{Beschreibung der Variable}")
  expect_equal(out[2], "Variablenname:&pvkat\\_pooled\\\\")
  expect_equal(out[3], "Label:&NA\\\\")
  expect_equal(out[4], "Anzahl der Imputationen: & 5\\\\")
  expect_equal(out[5], "Kategorien:& 1~$=$~\\textit{Kompetenzstufe 1}; 2~$=$~\\textit{Kompetenzstufe 2}; 3~$=$~\\textit{Kompetenzstufe 3}; 4~$=$~\\textit{Kompetenzstufe 4}; 5~$=$~\\textit{Kompetenzstufe 5}\\\\")
  expect_equal(out[6], "Fehlende Werte:& .~$=$~\\textit{kein Dateneintrag}\\\\")
  expect_equal(out[7], "\\end{tabnormallong}")
})

