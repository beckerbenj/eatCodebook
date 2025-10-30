

df <- data.frame(
  ID = c("P1","P2","P3","P4","P5","P6","P7","P8","P9"),
  IDSCH = c(1001,1001,1001,1002,1002,1002,1003,1003,1003),
  varMetrisch = c(518.727356,521.720458,-99.000000,522.990187,362.098329,-98.000000,409.618053,445.110438,609.070510),
  varOrdinal = c(3,2,3,3,4,3,1,4,3),
  varCat = c("c","b","c","c","c","c","b","d","a"),
  skala1_item1 = c(1,3,3,2,2,2,4,4,4),
  skala1_item2 = c(3,1,3,2,4,2,2,1,1),
  skala1_item3 = c(1,4,1,2,4,3,3,4,4),
  skala1 = c(1.666667,2.666667,2.333333,2.000000,3.333333,2.333333,2.333333,3.000000,3.000000),
  pv_1 = c(-0.542021,0.871749,1.050498,-0.558275,0.707330,0.138531,0.732479,2.285707,NA),
  pv_2 = c(-0.318104,0.732479,1.130451,-1.456950,0.633771,0.164844,0.746879,1.824546,NA),
  pv_3 = c(-1.562190,0.406087,0.514336,0.191893,-0.641168,0.732479,0.670173,1.889183,NA),
  pv_4 = c(-0.682624,1.731707,-0.029477,0.088180,0.048499,0.453846,0.703055,2.373361,NA),
  pv_5 = c(-1.367413,1.113409,0.991823,-0.989473,0.410356,1.433651,0.911091,2.316563,NA),
  pvord_1 = c(3,3,2,2,3,4,5,3,NA),
  pvord_2 = c(4,5,2,3,2,4,4,2,NA),
  pvord_3 = c(3,5,3,3,2,4,4,3,NA),
  pvord_4 = c(4,4,2,3,1,3,5,2,NA),
  pvord_5 = c(3,3,2,2,2,4,4,2,NA),
  pvkat_1 = c(3,1,2,2,3,4,5,6,NA),
  pvkat_2 = c(4,5,1,3,2,4,4,6,NA),
  pvkat_3 = c(3,5,3,1,2,4,4,6,NA),
  pvkat_4 = c(4,4,2,3,1,3,5,6,NA),
  pvkat_5 = c(3,3,2,1,2,4,4,6,NA),
  stringsAsFactors = FALSE
)

varLabels <- data.frame(
  varName = names(df),
  varLabel = c(
    NA,
    "Schul-ID",
    "metrische Beispielvariable, Kompetenzwert",
    "ordinale Beispielvariable, Kompetenzstufe",
    "nominale Beispielvariable",
    "Likert-Skalenindikator 1",
    "Likert-Skalenindikator 2",
    "Likert-Skalenindikator 3",
    "Skala: Likert-Skalenwert",
    "IMPUTATION 1: plausible value",
    "IMPUTATION 2: plausible value",
    "IMPUTATION 3: plausible value",
    "IMPUTATION 4: plausible value",
    "IMPUTATION 5: plausible value",
    "IMPUTATION 1: Kompetenzstufe des plausible value",
    "IMPUTATION 2: Kompetenzstufe des plausible value",
    "IMPUTATION 3: Kompetenzstufe des plausible value",
    "IMPUTATION 4: Kompetenzstufe des plausible value",
    "IMPUTATION 5: Kompetenzstufe des plausible value",
    "IMPUTATION 1: Kompetenzkategorie des plausible value",
    "IMPUTATION 2: Kompetenzkategorie des plausible value",
    "IMPUTATION 3: Kompetenzkategorie des plausible value",
    "IMPUTATION 4: Kompetenzkategorie des plausible value",
    "IMPUTATION 5: Kompetenzkategorie des plausible value"
  )
)

valLabels <- data.frame(
  varName = c(
    "varMetrisch", "varMetrisch",
    "varOrdinal", "varOrdinal", "varOrdinal", "varOrdinal",
    "skala1_item1", "skala1_item1", "skala1_item1", "skala1_item1",
    "skala1_item2", "skala1_item2", "skala1_item2", "skala1_item2",
    "skala1_item3", "skala1_item3", "skala1_item3", "skala1_item3",
    "pvord_1", "pvord_1", "pvord_1", "pvord_1", "pvord_1",
    "pvord_2", "pvord_2", "pvord_2", "pvord_2", "pvord_2",
    "pvord_3", "pvord_3", "pvord_3", "pvord_3", "pvord_3",
    "pvord_4", "pvord_4", "pvord_4", "pvord_4", "pvord_4",
    "pvord_5", "pvord_5", "pvord_5", "pvord_5", "pvord_5",
    "pvkat_1", "pvkat_1", "pvkat_1", "pvkat_1", "pvkat_1", "pvkat_1",
    "pvkat_2", "pvkat_2", "pvkat_2", "pvkat_2", "pvkat_2", "pvkat_2",
    "pvkat_3", "pvkat_3", "pvkat_3", "pvkat_3", "pvkat_3", "pvkat_3",
    "pvkat_4", "pvkat_4", "pvkat_4", "pvkat_4", "pvkat_4", "pvkat_4",
    "pvkat_5", "pvkat_5", "pvkat_5", "pvkat_5", "pvkat_5", "pvkat_5"
  ),
  value = c(
    -99, -98,
    1, 2, 3, 4,
    1, 2, 3, 4,
    1, 2, 3, 4,
    1, 2, 3, 4,
    1, 2, 3, 4, 5,
    1, 2, 3, 4, 5,
    1, 2, 3, 4, 5,
    1, 2, 3, 4, 5,
    1, 2, 3, 4, 5,
    1, 2, 3, 4, 5, 6,
    1, 2, 3, 4, 5, 6,
    1, 2, 3, 4, 5, 6,
    1, 2, 3, 4, 5, 6,
    1, 2, 3, 4, 5, 6
  ),
  valLabel = c(
    "not reached", "omission",
    "sehr schlecht", "schlecht", "gut", "sehr gut",
    "stimme nicht zu", "stimme etwas zu", "stimme zu", "stimme voll zu",
    "stimme nicht zu", "stimme etwas zu", "stimme zu", "stimme voll zu",
    "stimme nicht zu", "stimme etwas zu", "stimme zu", "stimme voll zu",
    "Kompetenzstufe 1", "Kompetenzstufe 2", "Kompetenzstufe 3", "Kompetenzstufe 4", "Kompetenzstufe 5",
    "Kompetenzstufe 1", "Kompetenzstufe 2", "Kompetenzstufe 3", "Kompetenzstufe 4", "Kompetenzstufe 5",
    "Kompetenzstufe 1", "Kompetenzstufe 2", "Kompetenzstufe 3", "Kompetenzstufe 4", "Kompetenzstufe 5",
    "Kompetenzstufe 1", "Kompetenzstufe 2", "Kompetenzstufe 3", "Kompetenzstufe 4", "Kompetenzstufe 5",
    "Kompetenzstufe 1", "Kompetenzstufe 2", "Kompetenzstufe 3", "Kompetenzstufe 4", "Kompetenzstufe 5",
    "Kompetenzstufe 1", "Kompetenzstufe 2", "Kompetenzstufe 3", "Kompetenzstufe 4", "Kompetenzstufe 5", "Kompetenzstufe 6",
    "Kompetenzstufe 1", "Kompetenzstufe 2", "Kompetenzstufe 3", "Kompetenzstufe 4", "Kompetenzstufe 5", "Kompetenzstufe 6",
    "Kompetenzstufe 1", "Kompetenzstufe 2", "Kompetenzstufe 3", "Kompetenzstufe 4", "Kompetenzstufe 5", "Kompetenzstufe 6",
    "Kompetenzstufe 1", "Kompetenzstufe 2", "Kompetenzstufe 3", "Kompetenzstufe 4", "Kompetenzstufe 5", "Kompetenzstufe 6",
    "Kompetenzstufe 1", "Kompetenzstufe 2", "Kompetenzstufe 3", "Kompetenzstufe 4", "Kompetenzstufe 5", "Kompetenzstufe 6"
  ), missings = c(
    rep("miss", 2),
    rep("valid", 71)
  )
)

gd <- eatGADS::import_raw(df = df, varLabels = varLabels, valLabels = valLabels)
gd2 <- eatGADS::checkFormat(gd)

eatGADS::write_spss(gd2, filePath = "inst/extdata/example1_clean.sav")

