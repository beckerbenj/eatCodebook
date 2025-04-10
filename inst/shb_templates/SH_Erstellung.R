#' nachdem alle Exceldateien erstellt/bearbeitet wurden, kann man mit diesem
#' Skript das Skalenhandbuch erstellen, ohne dass man ausversehen was verändert.

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

#daten <- import_spss()
#daten2 <- import_spss()
#daten3 <- import_spss()


#' alle Datensätze in einer Liste `datenliste` speichern
datenliste <- list()

### 2. Kennwertdatensätze (InputForDescriptives) -------------------------------
### ----------------------------------------------------------------------------

# Listen für Kennwerte und input_descriptives laden
kennwerte <- readRDS(".\\07_SH_Erstellung\\kennwerte.RDS")
input_descriptives <- readRDS(".\\07_SH_Erstellung\\input_descriptives.RDS")

### 3. Missingsobjekt einlesen -------------------------------------------------
### ----------------------------------------------------------------------------

missings <- getMissings(".\\07_SH_Erstellung\\missings.xlsx")

### 4. Skalen einlesen ---------------------------------------------------------
### ----------------------------------------------------------------------------

skalen <- getScaleInfo(".\\07_SH_Erstellung\\skalen.xlsx")

### 5. Abkürzungsverzeichnis (abbr) --------------------------------------------
### ----------------------------------------------------------------------------

abbr_list <- makeAbbrList(".\\07_SH_Erstellung\\abkuerzung.xlsx")

### 6. varInfo einlesen --------------------------------------------------------
### ----------------------------------------------------------------------------

varinfo <- getVarInfo(".\\07_SH_Erstellung\\varinfo_bearbeitet.xlsx")

### 7. Gliederung --------------------------------------------------------------
### ----------------------------------------------------------------------------

gliederung <- getStructure(".\\07_SH_Erstellung\\gliederung.xlsx")

### 8. Literaturverzeichnis ----------------------------------------------------
### ----------------------------------------------------------------------------

literatur <- getLitInfo(".\\07_SH_Erstellung\\literatur.xlsx")

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

meta_final <- makeMetadata(".\\07_SH_Erstellung\\meta.xlsx")

### 12. Kapitel erstellen -----------------------------------------------------
### ----------------------------------------------------------------------------

chapters <- getChapters(".\\07_SH_Erstellung\\chapters.xlsx")

### 13. weitere Sachen: sonstige Texte -----------------------------------------
### ----------------------------------------------------------------------------

# Introtext
# intro_pages <- readLines("Q:\\BT2021\\BT\\90_Skalenhandbuch\\08_Einleitungstexte/Latex_Intro_snippet.tex")

# Outrotext
# lastpage <- readLines("Q:\\BT2021\\BT\\90_Skalenhandbuch\\08_Einleitungstexte/Latex_lastpage_snippet.tex")


### 14. Skalenhandbuch erstellen und speichern ---------------------------------
### ----------------------------------------------------------------------------

codebook <- codebook(varInfo = varinfo, missings = missings, struc = gliederung,
                     scaleInfo = skalen, register = NULL, dat = lapply(datenliste, eatGADS::extractData),
                     Kennwertedatensatz = kennwerte,
                     chapters = chapters, deckblatt = pdf_cover, intro = intro_pages,
                     literatur = literatur_final, abkuerzverz = abbr_list, hintmod = hgm, lastpage = lastpage)

# als .tex speichern
#write.table(codebook_new, file = ".\\07_SH_Erstellung\\Latex\\IQB_BT2021_Skalenhandbuch_Stand2025_04_03.tex" , fileEncoding="UTF-8" ,
#            col.names=FALSE , row.names=FALSE , quote = FALSE )

# als .xmpdata speichern
#write.table(meta_final , file = ".\\07_SH_Erstellung\\Latex\\IQB_BT2021_Skalenhandbuch_Stand2025_04_03.xmpdata", fileEncoding="UTF-8" ,
#            col.names=FALSE , row.names=FALSE , quote = FALSE )

