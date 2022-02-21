

gd_path <- system.file("extdata", "example1_clean.sav", package = "eatCodebook")
gd   <- eatGADS::import_spss(gd_path)

inputForDescr <- createInputForDescriptives(gd, verbose = FALSE)
suppressWarnings(descr  <- calculateDescriptives(gd, inputForDescr, verbose = FALSE))

varInfo_path <- system.file("extdata", "example_varInfo.xlsx", package = "eatCodebook")
varInfo <- getVarInfo(varInfo_path)
varMiss_path <- system.file("extdata", "example_miss.xlsx", package = "eatCodebook")
varMiss <- getMissings(varMiss_path)


test_that("simple description for ID", {
  varInfo[1, "LabelSH"] <- "SuS-ID"
  out <- table.descriptive(name =  "ID", varue.info = varInfo, var.typ = "ID-Variable")
  expect_equal(out[1], "\\begin{tabnormallong}{Beschreibung der Variable}")
  expect_equal(out[2], "Variablenname:&ID\\\\")
  expect_equal(out[3], "Label:&SuS-ID\\\\")
  expect_equal(out[4], "\\end{tabnormallong}")
})


test_that("description for ordinal variable with source", {
  out <- table.descriptive(name =  "varOrdinal", varue.info = varInfo, varue.missings = varMiss, var.typ = "Numerisch", werte = descr$varOrdinal)
  expect_equal(out[1], "\\begin{tabnormallong}{Beschreibung der Variable}")
  expect_equal(out[2], "Variablenname:&varOrdinal\\\\")
  expect_equal(out[3], "Label:&ordinale Beispielvariable, Kompetenzstufe\\\\")
  expect_equal(out[4], "Quelle:&Mueller (2019)\\\\")
  expect_equal(out[5], "Kategorien:& 1~$=$~\\textit{sehr schlecht}; 2~$=$~\\textit{schlecht}; 3~$=$~\\textit{gut}; 4~$=$~\\textit{sehr gut}\\\\")
  expect_equal(out[6], "")
  expect_equal(out[7], "\\end{tabnormallong}")
})


test_that("simple description for metric variable with missings", {
  out <- table.descriptive(name = "varMetrisch", varue.info = varInfo, varue.missings = varMiss, var.typ="Numerisch", Gesamtdatensatz = gd,
                           werte = descr$varMetrisch , skala.items=NULL, show.kategorien=FALSE)

  ## wie funktioniert das mit Missings??
  expect_equal(out[1], "\\begin{tabnormallong}{Beschreibung der Variable}")
  expect_equal(out[2], "Variablenname:&varMetrisch\\\\")
  expect_equal(out[3], "Label:&metrische Beispielvariable, Kompetenzwert\\\\")
  expect_equal(out[4], "\\end{tabnormallong}")
})


test_that("simple description for scale variable", {
  out_scale <- table.descriptive(name = "skala1", varue.info = varInfo, varue.missings = varMiss, var.typ="Numerisch", Gesamtdatensatz = gd,
                           werte = descr$skala1 , skala.items = paste0("skala1_item", 1:3), show.kategorien=FALSE)
  expect_equal(out_scale[1], "\\begin{tabnormallong}{Beschreibung der Variable}")
  expect_equal(out_scale[2], "Variablenname:&skala1\\\\")
  expect_equal(out_scale[3], "Label:&Skala: Likert-Skalenwert\\\\")
  expect_equal(out_scale[4], "Anzahl der Items: & 3\\\\")
  expect_equal(out_scale[5], "\\end{tabnormallong}")

  out_items <- table.descriptive(name = paste0("skala1_item", 1:3), varue.info = varInfo, varue.missings = varMiss, var.typ="Numerisch", Gesamtdatensatz = gd,
                                 werte = as.data.frame(descr$skala1[[2]]))
  expect_equal(out_items[1], "\\begin{tabnormallong}{Beschreibung der Items}")
  expect_equal(out_items[2], "Kategorien:& 1~$=$~\\textit{stimme nicht zu}; 2~$=$~\\textit{stimme etwas zu}; 3~$=$~\\textit{stimme zu}; 4~$=$~\\textit{stimme voll zu}\\\\")
  expect_equal(out_items[3], "")
  expect_equal(out_items[4], "\\end{tabnormallong}")
  expect_equal(out_items[5], "\\begin{tabcoloredNoCaption}{lX}")
  expect_equal(out_items[6], "\\textbf{Variablen} & \\textbf{Labels} \\\\")
  expect_equal(out_items[7], "\\midrule")
  expect_equal(out_items[8], "skala1\\_item1 & Likert-Skalenindikator\\\\")
  expect_equal(out_items[11], "\\bottomrule")
  expect_equal(out_items[12], "\\end{tabcoloredNoCaption}")
})

test_that("simple description for pooled metric variable", {
  out <- table.descriptive(name = "pv_pooled", varue.info = varInfo, varue.missings = varMiss, var.typ="Numerisch", Gesamtdatensatz = gd,
                           werte = descr$pv_pooled, skala.items = paste0("pv_", 1:5), gepoolt = TRUE)
  expect_equal(out[1], "\\begin{tabnormallong}{Beschreibung der Variable}")
  expect_equal(out[2], "Variablenname:&pv\\_pooled\\\\")
  expect_equal(out[3], "Label:&NA\\\\")
  expect_equal(out[4], "Anzahl der Imputationen: & 5\\\\")
  expect_equal(out[5], "\\end{tabnormallong}")
})

test_that("simple description for pooled categorical variable", {
  out <- table.descriptive(name = "pvkat_pooled", varue.info = varInfo, varue.missings = varMiss, var.typ="Numerisch", Gesamtdatensatz = gd,
                           werte = descr$pv_pooled, skala.items = paste0("pvkat_", 1:5), gepoolt = TRUE)
  expect_equal(out[1], "\\begin{tabnormallong}{Beschreibung der Variable}")
  expect_equal(out[2], "Variablenname:&pvkat\\_pooled\\\\")
  expect_equal(out[3], "Label:&NA\\\\")
  expect_equal(out[4], "Anzahl der Imputationen: & 5\\\\")
  expect_equal(out[5], "Kategorien:& 1~$=$~\\textit{Kompetenzstufe 1}; 2~$=$~\\textit{Kompetenzstufe 2}; 3~$=$~\\textit{Kompetenzstufe 3}; 4~$=$~\\textit{Kompetenzstufe 4}; 5~$=$~\\textit{Kompetenzstufe 5}\\\\")
  expect_equal(out[6], "")
  expect_equal(out[7], "\\end{tabnormallong}")
})

