### Achtung: wenn alle Probleme behoben sind, hier "suppressWarnings" entfernen und die
### Warnungen explizit abfangen (die soll es ja geben)

load(test_path("helper_varinfo_vorlage.rda"))
file <- system.file("extdata", "example1_clean.sav", package = "eatCodebook")
gd   <- eatGADS::import_spss(file)                                          
vari <- createInputForDescriptives(gd, impExpr = "plausible value", scaleExpr = "Skalenwert", verbose = FALSE)

vari[grepl("pvkat", vari$varName), "scale"] <- "nominal"

test_that("descriptives scale", {
  suppressWarnings(res  <- calculateDescriptives(gd, vari, verbose = FALSE))
  expect_equal(names(res), c("pv_pooled", "pvkat_pooled", "pvord_pooled", "skala1", "varMetrisch", "varOrdinal"))
  expect_equal(names(res$pv_pooled), c("N.valid", "mean.valid", "sd.valid", "max.valid", "min.valid", "sysmis.totalabs"))
  expect_equal(names(res$pvkat_pooled), c("N.valid", "N.total", "1.valid", "2.valid", "3.valid", "4.valid", "5.valid", "6.valid", "sysmis.valid", 
                                   "1.total", "2.total", "3.total", "4.total", "5.total" , "6.total", "sysmis.total", 
                                   "1.totalabs", "2.totalabs", "3.totalabs", "4.totalabs", "5.totalabs", "6.totalabs", "sysmis.totalabs"))
  expect_equal(names(res$pvord_pooled), c("N.valid", "N.total", "mean.valid", "sd.valid",
                                   "1.valid", "2.valid", "3.valid", "4.valid", "5.valid", "sysmis.valid", 
                                   "1.total", "2.total", "3.total", "4.total", "5.total", "sysmis.total", 
                                   "1.totalabs", "2.totalabs", "3.totalabs", "4.totalabs", "5.totalabs", "sysmis.totalabs"))
})



clean2 <- eatGADS::import_spss(test_path("helper_clean2.sav"))
deskr2 <- readRDS(test_path("helper_descriptives.RDS"))

test_that("create descriptives", {
    expect_warning(clean_input2 <- createInputForDescriptives(clean2),
               "Identification of fake scales cannot be done completely automatically. Please check if the assignment of which items belong to a common scale is correct.")
    deskr <- calculateDescriptives(clean2, inputForDescriptives = clean_input2)
    lapply(names(deskr), FUN = function(nam) {
      expect_equal(deskr[[nam]], deskr2[[nam]], label = nam)
      })
})

