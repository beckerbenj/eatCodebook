#' Datensätze einlesen und alle Objekte/Exceldateien fürs Skalenhandbuch erstellen
#' aufwändigere Bearbeitung in seperaten .R Dateien für die Übersicht:
#' kennwerte, varinfo, gliederung, literatur.
#' kleinere Änderungen direkt im Skript

### setup ----------------------------------------------------------------------
#remotes::install_github("beckerbenj/eatCodebook")
#remotes::install_github("beckerbenj/eatAnalysis")
#install.packages("eatGADS")
library(eatCodebook)
library(eatGADS)
library(eatAnalysis)

### 1. Datensätze laden --------------------------------------------------------
### ----------------------------------------------------------------------------

#' Daten mit `readRDS()` oder `import_spss()` einlesen
#' Daten liegen idR auf `Q:\\BT20xx\\BT\\90_Skalenhandbuch` als `.sav`Datei

#daten1 <- import_spss()
#daten2 <- import_spss()
#daten3 <- import_spss()

#' alle Datensätze in einer Liste `datenliste` speichern
datenliste <- list(daten1 = daten1,
                   daten2 = daten2,
                   daten3 = daten3)

### 2. Kennwertdatensätze (InputForDescriptives) -------------------------------
### ----------------------------------------------------------------------------

#' für jeden Datensatz ein `descriptives_bsp` Objekt erstellen mit `createInputForDescriptives()`
#' es als Excel speichern mit `eatAnalysis::write_excel()` als `descriptives_bsp.xlsx`
#' zur Bearbeitung ggf. neue .R Datei erstellen, s. `.\\inst\\shb_templates\\SH_kennwerte.R`
#' wieder einlesen in Objekt `descriptives_bsp_bearbeitet`
#' Skalenkonsistenz checken mit `checkSacleConcsistency()` und in Objekt `check_scale_bsp` speichern
#' Kennwerte berechnen mit `calculateDescriptives()` und in Objekt `kennwerte_bsp` speichern
#' nachdem alle Kennwertdatensätze und InputForDescriptives erstellt wurden:
#' 2 Listen erstellen `kennwerte` und `input_descriptives`,
#' diese 2 Listen als `.RDS` Dateien speichern und wieder einlesen

# Beispiel: daten1 -------------------------------------------------------------

## Inputfordescriptives erstellen
descriptives_bsp <- createInputForDescriptives(GADSdat = daten1, nCatsForOrdinal = 4)
## Excel erstellen
#eatAnalysis::write_excel(df_list = list(BspName = descriptives_bsp), row.names = FALSE, filePath = ".\\07_SH_Erstellung\\descriptives_bsp.xlsx")

#' ggf. `descriptives_bsp.xlsx` bearbeiten (manuell od. via R)

## bearbeiteten Datensatz wieder einlesen
descriptives_bsp_bearbeitet <- getInputForDescriptives(".\\07_SH_Erstellung\\descriptives_bsp_bearbeitet.xlsx")

## ggf. Skalenkonsistenz checken
check_scale_bsp <- checkScaleConsistency(daten1, descriptives_bsp_bearbeitet, 1:nrow(descriptives_bsp_bearbeitet))

## Kennwerte berechnen und in eigenem Objekt speichern
kennwerte_bsp <- calculateDescriptives(GADSdat = daten1, inputForDescriptives = descriptives_bsp_bearbeitet,
                                            showCallOnly = FALSE)


# neue Kennwertedatensätze in Listen speichern ---------------------------------

# Kennwerte aus calculateDescriptives()
kennwerte <- list(daten1 = kennwerte_daten1,
                  daten2 = kennwerte_daten2,
                  daten3 = kennwerte_daten3)
# bearbeiteten Input aus der erstellten Excel
input_descriptives <- list(daten1 = descriptives_daten1_bearbeitet,
                           daten2 = descriptives_daten2_bearbeitet,
                           daten3 = descriptives_daten3_bearbeitet)

## Listen jeweils als RDS Dateien speichern
#saveRDS(kennwerte, ".\\07_SH_Erstellung\\kennwerte.RDS")
#saveRDS(input_descriptives, ".\\07_SH_Erstellung\\input_descriptives.RDS")

## und wieder neu laden ##
#kennwerte <- readRDS(".\\07_SH_Erstellung\\kennwerte.RDS")
#input_descriptives <- readRDS(".\\07_SH_Erstellung\\input_descriptives.RDS")


### 3. Missingsobjekt ----------------------------------------------------------
### ----------------------------------------------------------------------------

#' `missings` Objekt erstellen und in Excel speichern. idR keine Bearbeitung.

missings <- createMissings(datenliste, input_descriptives)
#eatAnalysis::write_xlsx(df_list = missings, row.names = FALSE, filePath = ".\\07_SH_Erstellung\\missings.xlsx")

#missings <- getMissings(".\\07_SH_Erstellung\\missings.xlsx")

### 4. Skalen ------------------------------------------------------------------
### ----------------------------------------------------------------------------

#' `skalen` Objekt erstellen und in Excel speichern.
#' ggf. bearbeiten

skalen <- createScaleInfo(input_descriptives)
#eatAnalysis::write_xlsx(df_list = skalen, row.names = FALSE, filePath = ".\\07_SH_Erstellung\\skalen.xlsx")

# ggf. Skalen bearbeiten -----------------

skalen <- getScaleInfo(".\\07_SH_Erstellung\\skalen.xlsx")

### 5. Abkürzungsverzeichnis (abbr) --------------------------------------------
### ----------------------------------------------------------------------------

#' `abbr_list` erstellen und in Excel speichern. ggf. bearbeiten
#' kann idR von anderen BTs übernommen werden
#' Aus der Exceldatei LaTeX Syntax fürs Abkürzungsverzeichnis erstellen mit `makeAbbrList()`

abbr_list <- createAbbrList()
#eatAnalysis::write_xlsx(df_list = abbr_list, row.names = FALSE, filePath = ".\\07_SH_Erstellung\\abkuerzung.xlsx")

# erstellt LaTeX Syntax
abbr_list <- makeAbbrList(".\\07_SH_Erstellung\\abkuerzung.xlsx")


### 6. varInfo -----------------------------------------------------------------
### ----------------------------------------------------------------------------

#' Ein `varinfo` Objekt erstellen für alle Datensätze gemeinsam
#' Layout hinzufügen mit `inferLayout()` - muss nicht jedes mal ausgeführt werden.
#' Objekt als Excel speichern `varinfo.xlsx` und wieder einlesen mit `getVarInfo()`

# varinfo erstellen
varinfo <- createVarInfo(datenliste, input_descriptives)

# Layout
#varinfo <- inferLayout(varinfo, datenliste, input_descriptives)

# varinfo speichern/laden
#eatAnalysis::write_xlsx(df_list = varinfo, row.names = FALSE, filePath = ".\\07_SH_Erstellung\\varinfo.xlsx")
#varinfo <- getVarInfo(".\\07_SH_Erstellung\\varinfo.xlsx")

# varinfo bearbeiten -----------------------------------------------------------

#' am besten neue .R Datei erstellen, s. `.\\inst\\shb_templates\\SH_varinfo.R`
#' `varinfo.xlsx` einlesen und für jeden Datensatz folgende Punkte ergänzen/bearbeiten:
#' Unterteilung (Kapitelunterschriften) und Gliederung (Bspw. 1.1)
#' Quellen und Instruktionen
#' Infos zur Rekodierung
#' Infos zum Hintergrundmodell (HGM)
#' ggf. Anmerkungen

#' bearbeitete Version oder Zwischenstände als `varinfo_bearbeitet.xlsx` speichern
#' und wieder neu einlesen

#eatAnalysis::write_xlsx(df_list = varinfo, row.names = FALSE, filePath = ".\\07_SH_Erstellung\\varinfo_bearbeitet.xlsx")
varinfo <- getVarInfo(".\\07_SH_Erstellung\\varinfo_bearbeitet.xlsx")

### 7. Gliederung --------------------------------------------------------------
### ----------------------------------------------------------------------------

#' gliederung erstellen mit `createStructure(varinfo)` und als `gliederung.xlsx` speichern
#' zur Bearbeitung am besten neue .R Datei erstellen, s. `.\\inst\\shb_templates\\SH_gliederung.R`
#' bearbeitete Excel wieder einlesen mit `getStructure()`

# gliederung erstellen und speichern
gliederung <- createStructure(varinfo)
#eatAnalysis::write_xlsx(df_list = gliederung, row.names = FALSE, filePath = ".\\07_SH_Erstellung\\gliederung.xlsx")

# Bearbeitung: s. SH_gliederung.R

# Änderungen einlesen
gliederung <- getStructure(".\\07_SH_Erstellung\\gliederung.xlsx")

### 8. Literaturverzeichnis ----------------------------------------------------
### ----------------------------------------------------------------------------

#' `createLitInfo(varinfo)` erstellt ein Objekt, was alle Literaturkurzangaben aus varinfo enthält
#' als `literatur.xlsx` speichern
#' zur Bearbeitung am besten neue .R Datei erstellen, s. `.\\inst\\shb_templates\\SH_literatur.R`
#' Es müssen Langangaben ergänzt werden, sowie die Info, ob die Quelle ins Skalenhandbuch aufgenommen werden soll
#' bearbeitete Excel wieder einlesen mit `getLitInfo()`
#' und LaTeX Syntax erstellen mit `makeLit()`

# Literaturverzeichnis anlegen
literatur_varinfo <- createLitInfo(varinfo)
# Excel erstellen
#eatAnalysis::write_xlsx(df_list = literatur, row.names = FALSE, filePath = ".\\07_SH_Erstellung\\literatur.xlsx")

# Bearbeitung: s. SH_literatur.R

# Excel einlesen
literatur <- getLitInfo(".\\07_SH_Erstellung\\literatur.xlsx")
# LaTeX Syntax erstellen
literatur_final <- makeLit(literatur)

### 9. Hintergrundmodell (HGM) -------------------------------------------------
### ----------------------------------------------------------------------------

hgm <- makeBGM(varinfo)

### 10. Cover ------------------------------------------------------------------
### ----------------------------------------------------------------------------

# Cover entweder direkt hier im Skript erstellen oder als pdf einlesen.

#cover <- makeCover(logoFile = ".\\07_SH_Erstellung\\Texte\\iqb-logo.jpg",
#                   maintitle = "IQB-Bildungstrend~2018",
#                   subtitle = 'Skalenhandbuch zur Dokumentation der Erhebungsinstrumente \\newline in Mathematik und den naturwissenschaftlichen F{\\"a}chern',
#                   authors = "Benjamin Becker*, Johanna Busse*, Marleen Holtmann, Sebastian Weirch, \\newline Stefan Schipolowski, Nicole Mahler & Petra Stanat",
#                   addAuthors = 'Unter Mitarbeit von ERG{\\"A}NZEN',
#                   schriftenreihe = 'Schriftenreihe des Institutes zur Qualit{\\"a}tsentwicklung im Bildungswesen - Band~11',
#                   bibinfo = 'Band~11 \\par \\medskip
#                   Becker, B.*, Busse, J.*, Holtmann, M., Weirich, S., Schipolowski, S., Mahler, N. & Stanat P. (2022). \\textit{IQB-Bildungstrend~2018. Skalenhandbuch zur Dokumentation der Erhebungsinstrumente in Mathematik und den naturwissenschaftlichen F{\\"a}chern.} Berlin: Humboldt-Universit{\\"a}t zu Berlin, Institut zur Qualit{\\"a}tsentwicklung im Bildungswesen.'
#)

#pdf_cover <- '\\includepdf[pages=-]{.\\08_Einleitungstexte/cover_page_230110.pdf}'


### 11. Meta Data erstellen ---------------------------------------------------
### ----------------------------------------------------------------------------

meta <- createMetadata()
#meta[1, "Title"] <- "IQB-Bildungstrend~2021. Skalenhandbuch zur Dokumentation der Erhebungsinstrumente in den naturwissenschaftlichen F?chern und Mathematik"
#meta[1, "Author"] <- "Stefan Schipolowski \\sep Petra Stanat"
#meta[1, "Keywords"] <- "Bildungstrend 2021\\sep IQB, Skalenhandbuch\\sep Erhebungsinstrumente Sekundarstufe I"
#meta[1, "Subject"] <- "Skalenhandbuch zum Bildungstrend 2021"

#eatAnalysis::write_xlsx(meta, ".\\07_SH_Erstellung\\meta.xlsx", row.names = FALSE)

meta_final <- makeMetadata(".\\07_SH_Erstellung\\meta.xlsx")

### 12. Kapitel erstellen -----------------------------------------------------
### ----------------------------------------------------------------------------

#' neues Objekt mit `createChapters(varinfo)` erstellen
#' als `chapters.xlsx` speichern
#' ggf. `chapterName` anpassen und Excel neu speichern
#' Excel wieder einlesen mit `getChapters()`


# chapters erstellen
chapters <- createChapters(varinfo)
#eatAnalysis::write_xlsx(chapters, ".\\07_SH_Erstellung\\chapters.xlsx", row.names = FALSE)

# Chapters bearbeiten --------------
# für jeden Datensatz/Sheet in varinfo ein Kapitel
#chapters$chapterName <- c("Lehrkräfte allgemein", "Lehrkräfte spez", "Matching Daten", "SLFB")

# Änderungen speichern und neu einlesen
#eatAnalysis::write_xlsx(chapters, ".\\07_SH_Erstellung\\chapters.xlsx", row.names = FALSE)
chapters <- getChapters(".\\07_SH_Erstellung\\chapters.xlsx")

### 13. weitere Sachen: sonstige Texte -----------------------------------------
### ----------------------------------------------------------------------------

#' Intro und andere Texte einlesen mit `readLines()`

# Introtext
# intro_pages <- readLines("Q:\\BT2021\\BT\\90_Skalenhandbuch\\08_Einleitungstexte/Latex_Intro_snippet.tex")

# Outrotext
# lastpage <- readLines("Q:\\BT2021\\BT\\90_Skalenhandbuch\\08_Einleitungstexte/Latex_lastpage_snippet.tex")


### 14. Skalenhandbuch erstellen und speichern ---------------------------------
### ----------------------------------------------------------------------------

#' alle erstellen Objekte in `codebook()` übergeben (Erstellung dauert mehrere Minuten)
#' erstelltes LaTeX Skript als `.tex` Datei mit `write.table()` speichern
#' meta Daten `meta_final` als `.xmpdata` Datei mit `write.table()` speichern

codebook <- codebook(varInfo = varinfo, missings = missings, struc = gliederung,
                     scaleInfo = skalen, register = NULL, dat = lapply(datenliste, eatGADS::extractData),
                     Kennwertedatensatz = kennwerte,
                     chapters = chapters, deckblatt = pdf_cover, intro = intro_pages,
                     literatur = literatur_final, abkuerzverz = abbr_list, hintmod = hgm , lastpage = lastpage)

# als .tex speichern
#write.table(codebook, file = ".\\07_SH_Erstellung\\Latex\\IQB_BT2021_Skalenhandbuch_Stand_14.03.tex" , fileEncoding="UTF-8",
#            col.names=FALSE, row.names=FALSE, quote = FALSE )

# als .xmpdata speichern
#write.table(meta_final, file = ".\\07_SH_Erstellung\\Latex\\IQB_BT2021_Skalenhandbuch_Stand_14.03.xmpdata", fileEncoding="UTF-8",
#            col.names=FALSE, row.names=FALSE, quote = FALSE )


