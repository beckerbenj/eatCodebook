
pisa_input <- createInputForDescriptives(eatGADS::pisa, impExpr = "Plausible Value")
saveRDS(pisa_input, "tests/testthat/helper_inputForDescriptives_pisa.RDS")

clean <- eatGADS::import_spss("tests/testthat/helper_clean.sav")
clean_input <- createInputForDescriptives(clean, fakeItemExpr = "gibt es nicht")
clean_varinfo <- createVarInfo(clean, inputForDescriptives = clean_input)
saveRDS(clean_input, "tests/testthat/helper_inputForDescriptives_clean.RDS")
saveRDS(clean_varinfo, "tests/testthat/helper_varInfo_clean.RDS")

clean2 <- eatGADS::import_spss("tests/testthat/helper_clean2.sav")
clean2_input <- createInputForDescriptives(clean2)
clean2_varinfo <- createVarInfo(clean2, inputForDescriptives = clean2_input)
saveRDS(clean2_input, "tests/testthat/helper_inputForDescriptives_clean2.RDS")
saveRDS(clean2_varinfo, "tests/testthat/helper_varInfo_clean2.RDS")


# usethis::use_data(DATASET, overwrite = TRUE)

# Excel helper
# -----------------------------------------------------------------------------------
## missings
missings <- data.frame(Var.name = c("v1", "v2"),
                       Wert = c(-99, 1),
                       missing = c("ja", "nein"),
                       LabelSH = c("Auslassen einer Frage", "eins"),
                       Zeilenumbruch_vor_Wert = c("nein", "nein"))
eatAnalysis::write_xlsx(list(dat1 = missings, dat2 = missings), filePath = "tests/testthat/helper_missings.xlsx", row.names = FALSE)

## varInfo
dfSAV <- eatGADS::import_spss("tests/testthat/helper_spss.sav")
dfSAV_input <- createInputForDescriptives(dfSAV)
varInfo <- createVarInfo(dfSAV, dfSAV_input)
varInfo$Unterteilung.im.Skalenhandbuch <- c("Teil 1a", "Teil 1b", "Teil 2")
varInfo$Gliederung <- c("1.01", "1.02", "2.01")
#varInfo$Anmerkung.Var[1] <- "NA"
eatAnalysis::write_xlsx(varInfo, "tests/testthat/helper_varInfo.xlsx", row.names = FALSE)


# inputfordescr
imputed_scale <- input_descriptives$sus[input_descriptives$sus$group == "Sinmo_pooled", ]
saveRDS(imputed_scale, "tests/testthat/helper_inputedForDescriptives_imputedScale.RDS")


# data sets
# -----------------------------------------------------------------------------------
netw <- data.frame(id = 1:2,
                    friend_1 = c(0, 1),
                    friend_2 = c(0, 0))
netzw_g <- eatGADS::import_DF(netw)
input_netw <- createInputForDescriptives(netzw_g)
input_netw[2:3, "group"] <- "friend"
input_netw[2:3, "scale"] <- NA
descr_netw <- calculateDescriptives(netw_g, input_netw)

saveRDS(netw_g, "tests/testthat/helper_data_netw.RDS")
saveRDS(input_netw, "tests/testthat/helper_inputForDescriptives_netw.RDS")
