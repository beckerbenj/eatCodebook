
# Minimal full example
# -----------------------------------------------------------------------------------
gd   <- eatGADS::import_DF(iris)
gd <- eatGADS::checkFormat(gd)

## Descriptives
inputForDescr <- createInputForDescriptives(gd)
descr  <- calculateDescriptives(gd, inputForDescr)

## Missings/Line Breaks
miss <- createMissings(gd, inputForDescriptives = inputForDescr)
writeExcel(miss, "inst/extdata/example_iris_miss.xlsx", row.names = FALSE)
miss_final <- getMissings("inst/extdata/example_iris_miss.xlsx")

## Varinfo
varInfo <- createVarInfo(gd, inputForDescriptives = inputForDescr)
varInfo[, "Titel"] <- c("Sepal Length", "Sepal Width", "Pepal Length", "Pepal Width", "Species")
varInfo[, "Gliederung"] <- c(rep("1.1", 2), rep("1.2", 2), rep("2.1", 1))
writeExcel(varInfo, "inst/extdata/example_iris_varInfo.xlsx", row.names = FALSE)
varInfo_final <- getVarInfo("inst/extdata/example_iris_varInfo.xlsx")
varInfo_final2 <- inferLayout(varInfo_final, GADSdat = gd, inputForDescriptives = inputForDescr)

## Structure
struc <- createStructure(varInfo_final)
struc[c(1, 4), "Titel"] <- c("Metrics", "Species Information")
# Hier Oberkapitel einfuegen
writeExcel(struc, "inst/extdata/example_iris_struc.xlsx", row.names = FALSE)
struc_final <- getStructure("inst/extdata/example_iris_struc.xlsx")

## Scale Infos
scaleInfo <- createScaleInfo(inputForDescr)
writeExcel(scaleInfo, "inst/extdata/example_iris_scaleInfo.xlsx", row.names = FALSE)
scaleInfo_final <- getScaleInfo("inst/extdata/example_iris_scaleInfo.xlsx")

# cover page
cover <- makeCover(maintitle = "Study of Achievement",
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
writeExcel(meta, "inst/extdata/example_iris_meta.xlsx", row.names = FALSE)
meta_final <- makeMetadata("inst/extdata/example_iris_meta.xlsx")

# Inkonsistenzen in make vs get ueberarbeiten

## Chapters
# --------------------------------------------------
chapters <- createChapters(varInfo_final2)
chapters[, 2] <- "Datensatz"
writeExcel(list(dat = chapters), "inst/extdata/example_iris_chapters.xlsx", row.names = FALSE)
chapters_final <- getChapters("inst/extdata/example_iris_chapters.xlsx")


## Codebook
# --------------------------------------------------
latex_skript <- codebook(varInfo = varInfo_final2, missings = miss_final, struc = struc_final,
                         scaleInfo = scaleInfo_final, dat = eatGADS::extractData(gd),
                         Kennwertedatensatz = descr, chapters = chapters_final)

#### SKRIPTE SCHREIBEN ####
write.table(latex_skript , file = "other_code/minimal_example.tex" , fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )
write.table(meta_final , file = "other_code/minimal_example_meta.xmpdata", fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )

## Codebook with predefined cover
# --------------------------------------------------
pdf_cover <- '\\includepdf[pages=-]{q:/BT2018/BT/90_Skalenhandbuch/08_Einleitungstexte/cover_page.pdf}'

latex_skript <- codebook(varInfo = varInfo_final2, missings = miss_final, struc = struc_final,
                         scaleInfo = scaleInfo_final, dat = eatGADS::extractData(gd),
                         Kennwertedatensatz = descr, chapters = chapters_final,
                         deckblatt = pdf_cover)

#### SKRIPTE SCHREIBEN ####
write.table(latex_skript , file = "other_code/minimal_example_pdf_cover.tex" , fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )
write.table(meta_final , file = "other_code/minimal_example_meta.xmpdata", fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )


