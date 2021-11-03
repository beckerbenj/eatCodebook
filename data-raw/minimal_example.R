
# Minimal full example
# -----------------------------------------------------------------------------------
file <- system.file("extdata", "example1.sav", package = "eatCodebook")
gd   <- eatGADS::import_spss(file)                                          ### inputliste erzeugen

str(gd)

## Descriptives
inputForDescr <- createInputForDescriptives(gd, varNameSeparatorImp = NULL, impExpr = "plausible value", scaleExpr = "Skalenwert")
inputForDescr[grep("skala1item", inputForDescr[,"varName"]),"scale"] <- "ordinal"
inputForDescr[which(inputForDescr[,"varName"] == "skalenwert_fake"),"type"] <- "scale"        ### ein eintrag in der varinfo muss jetzt haendisch geaendert werden (das geschieht spaeter fuer das
descr  <- calculateDescriptives(gd, inputForDescr)

## ## prepare everything else
miss <- createMissings(gd)
eatAnalysis::write_xlsx(miss, "inst/extdata/example_miss.xlsx", row.names = FALSE)
miss_final <- getMissings("inst/extdata/example_miss.xlsx")


varInfo <- createVarInfo(gd, inputForDescriptives = inputForDescr)
varInfo[3, "QuelleSH"] <- "Mueller (2019)"
varInfo[c(2, 11:14), "Hintergrundmodell"] <- "ja"
eatAnalysis::write_xlsx(varInfo, "inst/extdata/example_varInfo.xlsx", row.names = FALSE)
varInfo_final <- getVarInfo("inst/extdata/example_varInfo.xlsx")

struc <- createStructure(varInfo_final)
eatAnalysis::write_xlsx(struc, "inst/extdata/example_struc.xlsx", row.names = FALSE)
struc_final <- getStructure("inst/extdata/example_struc.xlsx")

scaleInfo <- createScaleInfo(inputForDescr)

register <- createRegister(inputForDescr, keywordList = c("kw1", "kw2"))


abbrList <- createAbbrList()
abbrList[[1]][1, ] <- c("MW", "Mittelwert")
abbrList[[2]][1, ] <- c("M", "Mittelwert")
eatAnalysis::write_xlsx(abbrList, "inst/extdata/example_abbrList.xlsx", row.names = FALSE)

litInfo <- createLitInfo(varInfo)

## make-steps
cover <- makeCover(maintitle = "Study of Achievement", subtitle = "Codebook of Study of Achievement",
                   authors = "Some Person", addAuthors = "With the help of some other persons",
                   schriftenreihe = "Book 9 of Studies of Achievement")

abbr <- makeAbbrList("inst/extdata/example_abbrList.xlsx")

lit <- makeLit(litInfo)

hint <- makeBGM
