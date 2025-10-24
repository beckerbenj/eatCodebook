
pisa_input <- createInputForDescriptives(eatGADS::pisa, impExpr = "Plausible Value")
saveRDS(pisa_input, "tests/testthat/helper_inputForDescriptives_pisa.RDS")

clean <- eatGADS::import_spss("tests/testthat/helper_clean.sav")
clean_input <- createInputForDescriptives(clean, fakeItemExpr = "gibt es nicht")
# Hotfix (see mail to SW):
clean_input[clean_input$imp, "type"] <- "variable"
clean_varinfo <- createVarInfo(clean, inputForDescriptives = clean_input)
saveRDS(clean_input, "tests/testthat/helper_inputForDescriptives_clean.RDS")
saveRDS(clean_varinfo, "tests/testthat/helper_varInfo_clean.RDS")

clean2 <- eatGADS::import_spss("tests/testthat/helper_clean2.sav")
clean_input2 <- createInputForDescriptives(clean2)
# Hotfix (see mail to SW):
clean_input2[clean_input2$imp, "type"] <- "variable"
clean_varinfo2 <- createVarInfo(clean2, inputForDescriptives = clean_input2)
saveRDS(clean_input2, "tests/testthat/helper_inputForDescriptives_clean2.RDS")
saveRDS(clean_varinfo2, "tests/testthat/helper_varInfo_clean2.RDS")


# usethis::use_data(DATASET, overwrite = TRUE)

# Excel helper
# -----------------------------------------------------------------------------------
## scaleInfo
scaleInfo_df <- createScaleInfo(clean_input)
eatAnalysis::write_xlsx(scaleInfo_df, filePath = "tests/testthat/helper_scaleInfo.xlsx", row.names = FALSE)
scaleInfo_list <- createScaleInfo(list(data1=clean_input, data2=clean_input))
eatAnalysis::write_xlsx(scaleInfo_list, filePath = "tests/testthat/helper_scaleInfo2.xlsx", row.names = FALSE)
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
