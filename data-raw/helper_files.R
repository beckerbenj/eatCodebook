
pisa_input <- createInputForDescriptives(eatGADS::pisa, impExpr = "Plausible Value")
saveRDS(pisa_input, "tests/testthat/helper_inputForDescriptives_pisa.RDS")

clean <- eatGADS::import_spss("tests/testthat/helper_clean.sav")
clean_input <- createInputForDescriptives(clean)
saveRDS(clean_input, "tests/testthat/helper_inputForDescriptives_clean.RDS")


# usethis::use_data(DATASET, overwrite = TRUE)

# Excel helper
# -----------------------------------------------------------------------------------
missings <- data.frame(Var.name = c("v1", "v2"),
                       Wert = c(-99, 1),
                       missing = c("ja", "nein"),
                       LabelSH = c("Auslassen einer Frage", "eins"),
                       Zeilenumbruch_vor_Wert = c("nein", "nein"))
eatAnalysis::write_xlsx(list(dat1 = missings, dat2 = missings), filePath = "tests/testthat/helper_missings.xlsx", row.names = FALSE)


#
imputed_scale <- input_descriptives$sus[input_descriptives$sus$group == "Sinmo_pooled", ]
saveRDS(imputed_scale, "tests/testthat/helper_inputedForDescriptives_imputedScale.RDS")


