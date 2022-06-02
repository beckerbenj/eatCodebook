
# Minimal full example
# -----------------------------------------------------------------------------------
#file <- system.file("extdata", "example1_clean.sav", package = "eatCodebook")
file1 <- "inst/extdata/example1_clean.sav"
file2 <- "inst/extdata/example1_clean_school.sav"
gd1   <- eatGADS::import_spss(file1)
gd2   <- eatGADS::import_spss(file2)

dat_list <- list(Students = gd1, Schools = gd2)

## Descriptives
inputForDescr <- createInputForDescriptives(dat_list)
descr  <- calculateDescriptives(dat_list, inputForDescr)

## Missings/Line Breaks
miss <- createMissings(dat_list, inputForDescriptives = inputForDescr)
#eatAnalysis::write_xlsx(miss, "inst/extdata/example2_miss.xlsx", row.names = FALSE)
miss_final <- getMissings("inst/extdata/example2_miss.xlsx")

## Varinfo
varInfo <- createVarInfo(dat_list, inputForDescriptives = inputForDescr)
# Gliederung
# Instruktion und Quellen (Beispiele rein)
varInfo[["Students"]][3, "QuelleSH"] <- "Mueller (2019)"
varInfo[["Students"]][c(2, 3, 8), "Hintergrundmodell"] <- "ja"
varInfo[["Students"]][c(1, 2), "Titel"] <- c("Schueler-ID", "School-ID")
varInfo[["Students"]][, "Unterteilung.im.Skalenhandbuch"] <- c(rep("BG", 5), rep("Scale", 4), rep("PVs", 12))
varInfo[["Students"]][, "Gliederung"] <- c(rep("1.1", 5), rep("1.2", 4), rep("2.1", 12))
varInfo[["Students"]][c(10, 16), "Titel"] <- c("Plausible Value", "categorical plausible value")
varInfo[["Schools"]][c(1), "Titel"] <- c("School-ID")
varInfo[["Schools"]][, "Gliederung"] <- c("1.1", "1.2", "1.2")
varInfo[["Schools"]][, "Unterteilung.im.Skalenhandbuch"] <- c(rep("Tracking", 1), rep("BG", 2))
#eatAnalysis::write_xlsx(varInfo, "inst/extdata/example2_varInfo.xlsx", row.names = FALSE)
varInfo_final <- getVarInfo("inst/extdata/example2_varInfo.xlsx")
varInfo_final2 <- inferLayout(varInfo_final, GADSdat = dat_list, inputForDescriptives = inputForDescr)

## Structure
struc <- createStructure(varInfo_final)
struc[["Students"]][c(1, 4), "Titel"] <- c("Background", "Competences")
struc[["Schools"]][c(1), "Titel"] <- c("School Background")
# Hier Oberkapitel einfuegen
#eatAnalysis::write_xlsx(struc, "inst/extdata/example2_struc.xlsx", row.names = FALSE)
struc_final <- getStructure("inst/extdata/example2_struc.xlsx")

## Scale Infos
scaleInfo <- createScaleInfo(inputForDescr)
#eatAnalysis::write_xlsx(scaleInfo, "inst/extdata/example2_scaleInfo.xlsx", row.names = FALSE)
scaleInfo_final <- getScaleInfo("inst/extdata/example2_scaleInfo.xlsx")

## Register
register <- createRegister(inputForDescr, keywordList = list(c("kw1", "kw2"), c("kw3", "kw4")))
register[["Students"]][c(1, 3, 4), "kw1"] <- "x"
register[["Students"]][c(7), "kw2"] <- "x"
register[["Schools"]][c(2), "kw3"] <- "x"
register[["Schools"]][c(1), "kw4"] <- "x"
#eatAnalysis::write_xlsx(register, "inst/extdata/example2_register.xlsx", row.names = FALSE)
register_final <- getRegister("inst/extdata/example2_register.xlsx")

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

# cover page
cover <- makeCover(logoFile = "q:/BT2016/BT/02_Organisation/81_StuMi_Arbeitsordner/Skalenhandbuch/Vorlagen/Grafiken/iqb-logo.jpg",
                   maintitle = "Study of Achievement",
                   subtitle = "Codebook of Study of Achievement",
                   authors = "Some Person",
                   addAuthors = "With the help of some other persons",
                   schriftenreihe = "Book 9 of Studies of Achievement",
                   bibinfo = "test")

# meta data
meta <- createMetadata()
meta[1, "Title"] <- "Codebook Test"
meta[1, "Author"] <- "Anna Muster"
meta[1, "Keywords"] <- "lsa, education"
meta[1, "Subject"] <- "test"
#eatAnalysis::write_xlsx(meta, "inst/extdata/example_meta.xlsx", row.names = FALSE)


## Make-steps
# --------------------------------------------------
cover <- makeCover(logoFile = "q:/BT2016/BT/02_Organisation/81_StuMi_Arbeitsordner/Skalenhandbuch/Vorlagen/Grafiken/iqb-logo.jpg",
                   maintitle = "Study of Achievement", subtitle = "Codebook of Study of Achievement",
                   authors = "Some Person", addAuthors = "With the help of some other persons",
                   schriftenreihe = "Book 9 of Studies of Achievement", bibinfo = "test")

abbr <- makeAbbrList("inst/extdata/example_abbrList.xlsx")

lit <- makeLit(litInfo_final)

hint <- makeBGM(varInfo_final)

meta_final <- makeMetadata("inst/extdata/example_meta.xlsx")

# Inkonsistenzen in make vs get ueberarbeiten


## Chapters
# --------------------------------------------------
chapters <- createChapters(varInfo_final2)
chapters[, 2] <- c("SFB", "SLFB")
chapters <- chapters[c(2, 1), ]
#eatAnalysis::write_xlsx(list(dat = chapters), "inst/extdata/example2_chapters.xlsx", row.names = FALSE)
chapters_final <- getChapters("inst/extdata/example2_chapters.xlsx")



## Codebook
# --------------------------------------------------
# try to hotfix (maybe kennwerte needs a data.frame sometimes?) -> fix this after JB has fixed the example with Felix's syntax
#str(descr$skala1[[2]])
descr2 <- descr
descr2[["Students"]]$skala1[[2]] <- as.data.frame(descr2[["Students"]]$skala1[[2]])


latex_skript <- codebook(varInfo = varInfo_final2, missings = miss_final, struc = struc_final,
                         scaleInfo = scaleInfo_final, register = register_final, make.reg = NULL, dat = lapply(dat_list, eatGADS::extractData),
                         Kennwertedatensatz = descr2, chapters = chapters_final,
                         deckblatt = cover, intro = "", literatur = lit, abkuerzverz = abbr, hintmod = hint,
                         lastpage = "")

#### SKRIPTE SCHREIBEN ####
write.table(latex_skript , file = "other_code/minimal_example/minimal_example2.tex" , fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )
write.table(meta_final , file = "other_code/minimal_example/minimal_example2_meta.xmpdata", fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )




## Ueberlegungen
# --------------------------------------------------
# getExcel umschreiben, sodass im Zweifelsfall doch immer eine Liste (bei Input df -> Laenge 1 rauskommt?)
# mit Sebastian klaeren


# mit JB sprechen
load("q:/BT2018/BT/90_Skalenhandbuch/07_SH_Erstellung/Minimalbeispiel/Erstellung_alte_syntax/03_Kennwerte/Kennwerte_minimal.rdata")
Kennwertedatensatz$pvkat_1
#Kennwertedatensatz$

#load("q:/BT2016/BT/02_Organisation/81_StuMi_Arbeitsordner/Skalenhandbuch/03_Kennwerte/Kennwerte_sfb.rdata")
str(descr$skala1)
str(Kennwertedatensatz$Salgsf)
#Kennwertedatensatz$Salgsf
Kennwertedatensatz$stufe_deu_hoeren_gepoolt



