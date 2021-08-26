
pisa_input <- createInputForDescriptives(eatGADS::pisa)
saveRDS(pisa_input, "tests/testthat/helper_inputForDescriptives_pisa.RDS")

clean <- eatGADS::import_spss("tests/testthat/helper_clean.sav")
clean_input <- createInputForDescriptives(clean)
saveRDS(clean_input, "tests/testthat/helper_inputForDescriptives_clean.RDS")


# usethis::use_data(DATASET, overwrite = TRUE)
