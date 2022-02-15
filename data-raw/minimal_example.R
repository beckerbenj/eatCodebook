
# Minimal full example
# -----------------------------------------------------------------------------------
#file <- system.file("extdata", "example1_clean.sav", package = "eatCodebook")
file <- "inst/extdata/example1_clean.sav"
gd   <- eatGADS::import_spss(file)                                          ### inputliste erzeugen

str(gd)

## Descriptives
inputForDescr <- createInputForDescriptives(gd)
descr  <- calculateDescriptives(gd, inputForDescr)

## Missings/Line Breaks
miss <- createMissings(gd, inputForDescriptives = inputForDescr)
#eatAnalysis::write_xlsx(miss, "inst/extdata/example_miss.xlsx", row.names = FALSE)
miss_final <- getMissings("inst/extdata/example_miss.xlsx")

## Varinfo
varInfo <- createVarInfo(gd, inputForDescriptives = inputForDescr)
# Gliederung
# Instruktion und Quellen (Beispiele rein)
varInfo[3, "QuelleSH"] <- "Mueller (2019)"
varInfo[c(2, 3, 8), "Hintergrundmodell"] <- "ja"
varInfo[c(1), "Titel"] <- "Schueler-ID"
varInfo[, "Unterteilung.im.Skalenhandbuch"] <- c(rep("BG", 4), rep("Scale", 4), rep("PVs", 12))
varInfo[, "Gliederung"] <- c(rep("1.1", 4), rep("1.2", 4), rep("2.1", 12))
varInfo[c(9, 15), "Titel"] <- c("Plausible Value", "categorical plausible value")
#eatAnalysis::write_xlsx(varInfo, "inst/extdata/example_varInfo.xlsx", row.names = FALSE)
varInfo_final <- getVarInfo("inst/extdata/example_varInfo.xlsx")
varInfo_final2 <- inferLayout(varInfo_final, GADSdat = gd, inputForDescriptives = inputForDescr)

## Structure
struc <- createStructure(varInfo_final)
struc[c(1, 4), "Titel"] <- c("Background", "Competences")
# Hier Oberkapitel einfuegen
#eatAnalysis::write_xlsx(struc, "inst/extdata/example_struc.xlsx", row.names = FALSE)
struc_final <- getStructure("inst/extdata/example_struc.xlsx")

## Scale Infos
scaleInfo <- createScaleInfo(inputForDescr)
#eatAnalysis::write_xlsx(scaleInfo, "inst/extdata/example_scaleInfo.xlsx", row.names = FALSE)
scaleInfo_final <- getScaleInfo("inst/extdata/example_scaleInfo.xlsx")
# workaround, should be done automatically, but currently problem with non list inputs for createScaleInfo
scaleInfo_final$Quelle <- "dat"

## Register
register <- createRegister(inputForDescr, keywordList = c("kw1", "kw2"))
register[c(1, 3, 4), "kw1"] <- "x"
register[c(7), "kw2"] <- "x"
#eatAnalysis::write_xlsx(list(dat = register), "inst/extdata/example_register.xlsx", row.names = FALSE)
register_final <- getRegister("inst/extdata/example_register.xlsx")

## Abbrevtiation list
abbrList <- createAbbrList()
abbrList[[1]][1, ] <- c("MW", "Mittelwert")
abbrList[[2]][1, ] <- c("M", "Mittelwert")
#eatAnalysis::write_xlsx(abbrList, "inst/extdata/example_abbrList.xlsx", row.names = FALSE)

## Literature
litInfo <- createLitInfo(varInfo)
litInfo[, 3] <- "ja"
litInfo[, 2] <- "Mueller, M. (2020). Titel."
#eatAnalysis::write_xlsx(litInfo, "inst/extdata/example_litInfo.xlsx", row.names = FALSE)
litInfo_final <- getLitInfo("inst/extdata/example_litInfo.xlsx")
# vlt. getLitInfo ueberarbeiten, sodass in_Litverzeichnis obsolet wird (also doppelte Langangabe rausnehmen)

# meta data
meta <- createMetadata()
meta[1, "Title"] <- "Codebook Test"
meta[1, "Author"] <- "Anna Muster"
meta[1, "Keywords"] <- "lsa, education"
meta[1, "Subject"] <- "test"
#eatAnalysis::write_xlsx(meta, "inst/extdata/example_meta.xlsx", row.names = FALSE)


## Make-steps
# --------------------------------------------------
cover <- makeCover(maintitle = "Study of Achievement", subtitle = "Codebook of Study of Achievement",
                   authors = "Some Person", addAuthors = "With the help of some other persons",
                   schriftenreihe = "Book 9 of Studies of Achievement")

abbr <- makeAbbrList("inst/extdata/example_abbrList.xlsx")

lit <- makeLit(litInfo_final)

hint <- makeBGM(varInfo_final)

meta_final <- makeMetadata("inst/extdata/example_meta.xlsx")

# Inkonsistenzen in make vs get ueberarbeiten

## Sonstiges Zeug von Felix
# --------------------------------------------------
# SH-Variablen
variablen.all <- varInfo_final2[varInfo_final2$in.DS.und.SH %in% c("ja", "sh"), "Var.Name"]

# ID-Variablen
id <- c(dat = "id")


## Codebook (tbd)
# --------------------------------------------------
codebook(varue.info = varInfo_final2, varue.missings = miss_final, varue.gliederung = struc_final, skalen.info = scaleInfo_final,
         varue.reg = register_final, make.reg = NULL, Gesamtdatensatz = gd, Kennwertedatensatz = descr, variablen = variablen.all,
         id = id, fbshort = "", fblong = "", deckblatt = "", intro = "", literatur = lit, abkuerzverz = abbr, hintmod = hint,
         lastpage = "")

####################
struc_final2 <- struc_final
names(struc_final2) <- "dat"

# try to hotfix (maybe kennwerte needs a data.frame sometimes?)
#str(descr$skala1[[2]])
descr$skala1[[2]] <- as.data.frame(descr$skala1[[2]])

latex_skript <- codebook(varue.info = list(dat = varInfo_final2), varue.missings = list(dat = miss_final), varue.gliederung = struc_final2,
         skalen.info = scaleInfo_final,
         varue.reg = register_final, make.reg = NULL, Gesamtdatensatz = list(dat = eatGADS::extractData(gd)),
         Kennwertedatensatz = list(dat = descr),
         variablen = list(dat = variablen.all),
         id = id, fbshort = "dat", fblong = "dat", deckblatt = "", intro = "", literatur = lit, abkuerzverz = abbr, hintmod = hint,
         lastpage = "")

#### SKRIPTE SCHREIBEN ####
write.table(latex_skript , file = "other_code/minimal_example/minimal_example.tex" , fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )
write.table(meta_final , file = "other_code/minimal_example/minimal_example_meta.xmpdata", fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )




## Ueberlegungen
# --------------------------------------------------
# getExcel umschreiben, sodass im Zweifelsfall doch immer eine Liste (bei Input df -> Laenge 1 rauskommt?)
# mit Sebastian klaeren


#load("q:/BT2016/BT/02_Organisation/81_StuMi_Arbeitsordner/Skalenhandbuch/03_Kennwerte/Kennwerte_sfb.rdata")
Kennwertedatensatz$stufe_deu_hoeren_gepoolt
Kennwertedatensatz$stufe_deu_hoeren_gepoolt

# mit JB sprechen, ohjeohje!
load("q:/BT2018/BT/90_Skalenhandbuch/07_SH_Erstellung/Minimalbeispiel/Erstellung_alte_syntax/03_Kennwerte/Kennwerte_minimal.rdata")
Kennwertedatensatz$pvkat_1
#Kennwertedatensatz$
