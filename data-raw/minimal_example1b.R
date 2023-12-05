
# Minimal full example
# -----------------------------------------------------------------------------------
#file <- system.file("extdata", "example2_clean.sav", package = "eatCodebook")
file <- "inst/extdata/example2_clean.sav"
gd   <- eatGADS::import_spss(file)

str(gd)

## Descriptives
inputForDescr <- createInputForDescriptives(gd)
descr  <- calculateDescriptives(gd, inputForDescr)

## Missings/Line Breaks
miss <- createMissings(gd, inputForDescriptives = inputForDescr)
#eatAnalysis::write_xlsx(miss, "inst/extdata/example1b_miss.xlsx", row.names = FALSE)
miss_final <- getMissings("inst/extdata/example1b_miss.xlsx")

## Varinfo
varInfo <- createVarInfo(gd, inputForDescriptives = inputForDescr)
# Gliederung
# Instruktion und Quellen (Beispiele rein)
varInfo[4, "QuelleSH"] <- "Mueller (2019)"
varInfo[c(3, 4, 9), "Hintergrundmodell"] <- "ja"
varInfo[c(1, 2), "Titel"] <- c("Schueler-ID", "School-ID")
varInfo[, "Unterteilung.im.Skalenhandbuch"] <- c(rep("BG", 5), rep("Scale", 8), rep("PVs", 12))
varInfo[, "Gliederung"] <- c(rep("1.1", 5), rep("1.2", 8), rep("2.1", 12))
varInfo[c(10, 14, 20), "Titel"] <- c("Fake Skala", "Plausible Value", "categorical plausible value")
#eatAnalysis::write_xlsx(varInfo, "inst/extdata/example1b_varInfo.xlsx", row.names = FALSE)
varInfo_final <- getVarInfo("inst/extdata/example1b_varInfo.xlsx")
varInfo_final2 <- inferLayout(varInfo_final, GADSdat = gd, inputForDescriptives = inputForDescr)

## Structure
struc <- createStructure(varInfo_final)
struc[c(1, 4), "Titel"] <- c("Background", "Competences")
# Hier Oberkapitel einfuegen
#eatAnalysis::write_xlsx(struc, "inst/extdata/example_struc.xlsx", row.names = FALSE)
struc_final <- getStructure("inst/extdata/example_struc.xlsx")

## Scale Infos
scaleInfo <- createScaleInfo(inputForDescr)
#eatAnalysis::write_xlsx(scaleInfo, "inst/extdata/example1b_scaleInfo.xlsx", row.names = FALSE)
scaleInfo_final <- getScaleInfo("inst/extdata/example1b_scaleInfo.xlsx")
# workaround, should be done automatically, but currently problem with non list inputs for createScaleInfo
scaleInfo_final$Quelle <- "dat"

## Register
register <- createRegister(inputForDescr, keywordList = c("kw1", "kw2"))
register[c(1, 4, 5), "kw1"] <- "x"
register[c(8), "kw2"] <- "x"
#eatAnalysis::write_xlsx(list(dat = register), "inst/extdata/example1b_register.xlsx", row.names = FALSE)
register_final <- getRegister("inst/extdata/example1b_register.xlsx")

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
chapters[, 2] <- "Datensatz"
#eatAnalysis::write_xlsx(list(dat = chapters), "inst/extdata/example_chapters.xlsx", row.names = FALSE)
chapters_final <- getChapters("inst/extdata/example_chapters.xlsx")


## Codebook
# --------------------------------------------------
# try to hotfix (maybe kennwerte needs a data.frame sometimes?) -> fix this after JB has fixed the example with Felix's syntax
#str(descr$skala1[[2]])
descr2 <- descr
descr2$skala1[[2]] <- as.data.frame(descr2$skala1[[2]])


latex_skript <- codebook(varInfo = varInfo_final2, missings = miss_final, struc = struc_final,
                         scaleInfo = scaleInfo_final, register = register_final, dat = eatGADS::extractData(gd),
                         Kennwertedatensatz = descr2, chapters = chapters_final,
                         deckblatt = cover, intro = "", literatur = lit, abkuerzverz = abbr, hintmod = hint,
                         lastpage = "")

#### SKRIPTE SCHREIBEN ####
write.table(latex_skript , file = "other_code/minimal_example/minimal_example1b.tex" , fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )
write.table(meta_final , file = "other_code/minimal_example/minimal_example_meta.xmpdata", fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )




## Codebook ohne Register
# --------------------------------------------------
latex_skript_noreg <- codebook(varInfo = varInfo_final2, missings = miss_final, struc = struc_final2,
                         scaleInfo = scaleInfo_final, register = register_final, dat = eatGADS::extractData(gd),
                         Kennwertedatensatz = descr2, chapters = chapters_final,
                         deckblatt = cover, intro = "", literatur = lit, abkuerzverz = abbr, hintmod = hint,
                         lastpage = "")

#### SKRIPTE SCHREIBEN ####
write.table(latex_skript_noreg, file = "other_code/minimal_example/minimal_example_noregister.tex" , fileEncoding="UTF-8" ,
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



