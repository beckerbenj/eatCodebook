# Minimal Example

This vignettes shows a minimal working example for creating a codebook
via the `eatCodebook` package. For illustrative purposes we use the `R`
built in data set `iris`. We import the data set using the `eatGADS`
package, which is automatically installed when `eatCodebook` is
installed. We also run the
[`checkFormat()`](https://beckerbenj.github.io/eatGADS/reference/checkFormat.html)
function from the `eatGADS` package which adds `SPSS` format information
to the meta data of the data set.

``` r
library(eatCodebook)
dat <- eatGADS::import_DF(iris)
#> Sepal.Length has been renamed to Sepal_Length
#> Sepal.Width has been renamed to Sepal_Width
#> Petal.Length has been renamed to Petal_Length
#> Petal.Width has been renamed to Petal_Width
dat <- eatGADS::checkFormat(dat)
#> Format of Variable Sepal_Length will be changed from character(0) to F3.1
#> Format of Variable Sepal_Width will be changed from character(0) to F3.1
#> Format of Variable Petal_Length will be changed from character(0) to F3.1
#> Format of Variable Petal_Width will be changed from character(0) to F3.1
#> Format of Variable Species will be changed from character(0) to F1
```

## Descriptive Statistics

One of the key elements of a codebook are descriptive statistics shortly
describing each variable in the data set. What kind of descriptive
statistics is reported for each variable depends on the type of the
variable. The function
[`createInputForDescriptives()`](https://beckerbenj.github.io/eatCodebook/reference/createInputForDescriptives.md)
creates a template to provide the information that is needed to
calculate the descriptive statistics for an `GADSdat` object.

``` r
inputForDescriptives <- createInputForDescriptives(GADSdat = dat)
head(inputForDescriptives)
#>              varName varLabel format   imp     type   scale        group
#> FALSE.1 Sepal_Length     <NA>   F3.1 FALSE variable numeric Sepal_Length
#> FALSE.2  Sepal_Width     <NA>   F3.1 FALSE variable numeric  Sepal_Width
#> FALSE.3 Petal_Length     <NA>   F3.1 FALSE variable numeric Petal_Length
#> FALSE.4  Petal_Width     <NA>   F3.1 FALSE variable numeric  Petal_Width
#> FALSE.5      Species     <NA>     F1 FALSE variable ordinal      Species
```

The template should be exported to `.xlsx`, modified and reimported to
`R`.

``` r
writeExcel(inputForDescriptives, "inputForDescriptives.xlsx")
inputForDescriptives_edited <- getInputForDescriptives("inputForDescriptives.xlsx")
```

This input is then used to calculate descriptive statistics via
[`calculateDescriptives()`](https://beckerbenj.github.io/eatCodebook/reference/calculateDescriptives.md).

``` r
# calculate descriptives
descStatistics <- calculateDescriptives(GADSdat = dat, 
                                        inputForDescriptives = inputForDescriptives_edited)
descStatistics[[3]]
#>         N.valid      mean.valid        sd.valid       min.valid       max.valid 
#>           "150"          "5.84"          "0.83"           "4.3"           "7.9" 
#> sysmis.totalabs 
#>             "0"
```

## Value and Missing Labels

Another important part of a codebook is the documentation of the value
labels of valid and missing values. A respective overview is created via
[`createMissings()`](https://beckerbenj.github.io/eatCodebook/reference/createMissings.md).

``` r
missings <- createMissings(dat, inputForDescriptives = inputForDescriptives_edited)
head(missings)
#>   Var.name Wert missing    LabelSH Zeilenumbruch_vor_Wert
#> 5  Species    1    nein     setosa                   nein
#> 6  Species    2    nein versicolor                   nein
#> 7  Species    3    nein  virginica                   nein
```

In this case, the resulting object `missings` has to be written to
`xlsx` and imported via
[`getMissings()`](https://beckerbenj.github.io/eatCodebook/reference/getMissings.md).
Note that all the `getXXX` functions perform important cleaning and
preparation steps, therefore the exporting to `xlsx` is obligatory.

``` r
writeExcel(missings, "example_miss.xlsx", row.names = FALSE)
miss_final <- getMissings("example_miss.xlsx")
```

    #>   Var.name Wert missing    LabelSH Zeilenumbruch_vor_Wert
    #> 1  Species    1    nein     setosa                   nein
    #> 2  Species    2    nein versicolor                   nein
    #> 3  Species    3    nein  virginica                   nein

## Variable Information

A key element of the `eatCodebook` package is that various forms of
variable information can be supplied.

``` r
varInfo <- createVarInfo(dat, inputForDescriptives = inputForDescriptives_edited)
head(varInfo)
#>       Var.Name in.DS.und.SH Unterteilung.im.Skalenhandbuch Layout LabelSH
#> 1 Sepal_Length           ja                             NA      -    <NA>
#> 2  Sepal_Width           ja                             NA      -    <NA>
#> 3 Petal_Length           ja                             NA      -    <NA>
#> 4  Petal_Width           ja                             NA      -    <NA>
#> 5      Species           ja                             NA      -    <NA>
#>   Anmerkung.Var Gliederung Reihenfolge Titel rekodiert QuelleSH Instruktionen
#> 1             -          -          NA  <NA>      nein        -             -
#> 2             -          -          NA  <NA>      nein        -             -
#> 3             -          -          NA  <NA>      nein        -             -
#> 4             -          -          NA  <NA>      nein        -             -
#> 5             -          -          NA  <NA>      nein        -             -
#>   Hintergrundmodell HGM.Reihenfolge HGM.Variable.erstellt.aus intern.extern
#> 1              nein               -                         -             -
#> 2              nein               -                         -             -
#> 3              nein               -                         -             -
#> 4              nein               -                         -             -
#> 5              nein               -                         -             -
#>   Seitenumbruch.im.Inhaltsverzeichnis
#> 1                                nein
#> 2                                nein
#> 3                                nein
#> 4                                nein
#> 5                                nein
```

``` r
writeExcel(varInfo, "example_varInfo.xlsx", row.names = FALSE)
varInfo_final <- getVarInfo("example_varInfo.xlsx")
varInfo_final2 <- inferLayout(varInfo_final, GADSdat = dat, 
                              inputForDescriptives = inputForDescriptives_edited)
```

    #>       Var.Name in.DS.und.SH Unterteilung.im.Skalenhandbuch Layout LabelSH
    #> 1 Sepal_Length           ja                             NA      -    <NA>
    #> 2  Sepal_Width           ja                             NA      -    <NA>
    #> 3 Petal_Length           ja                             NA      -    <NA>
    #> 4  Petal_Width           ja                             NA      -    <NA>
    #> 5      Species           ja                             NA      -    <NA>
    #>   Anmerkung.Var Gliederung Reihenfolge        Titel rekodiert QuelleSH
    #> 1             -        1.1           0 Sepal Length      nein        -
    #> 2             -        1.1           0  Sepal Width      nein        -
    #> 3             -        1.2           0 Pepal Length      nein        -
    #> 4             -        1.2           0  Pepal Width      nein        -
    #> 5             -        2.1           0      Species      nein        -
    #>   Instruktionen Hintergrundmodell HGM.Reihenfolge HGM.Variable.erstellt.aus
    #> 1             -              nein               -                         -
    #> 2             -              nein               -                         -
    #> 3             -              nein               -                         -
    #> 4             -              nein               -                         -
    #> 5             -              nein               -                         -
    #>   intern.extern Seitenumbruch.im.Inhaltsverzeichnis
    #> 1             -                                nein
    #> 2             -                                nein
    #> 3             -                                nein
    #> 4             -                                nein
    #> 5             -                                nein

## Structure

A key element of the `eatCodebook` package is that various forms of
variable information can be supplied.

``` r
struc <- createStructure(varInfo_final)
head(struc)
#>     Titel Ebene
#> 1.1    NA     1
#> 1.2    NA   1.1
#> 1.3    NA   1.2
#> 2.1    NA     2
#> 2.2    NA   2.1
```

``` r
writeExcel(struc, "example_struc.xlsx", row.names = FALSE)
struc_final <- getStructure("example_struc.xlsx")
```

    #>                 Titel Ebene
    #> 1             Metrics     1
    #> 2                <NA>   1.1
    #> 3                <NA>   1.2
    #> 4 Species Information     2
    #> 5                <NA>   2.1

## Scale Information

A key element of the `eatCodebook` package is that various forms of
variable information can be supplied.

``` r
scaleInfo <- createScaleInfo(inputForDescriptives_edited)
head(scaleInfo)
#> [1] varName              Anzahl_valider_Werte Items_der_Skala     
#> [4] Imputationen        
#> <0 rows> (or 0-length row.names)
```

``` r
writeExcel(scaleInfo, "example_scaleInfo.xlsx", row.names = FALSE)
scaleInfo_final <- getScaleInfo("example_scaleInfo.xlsx")
```

    #> [1] varName              Anzahl_valider_Werte Items_der_Skala     
    #> [4] Imputationen        
    #> <0 rows> (or 0-length row.names)

## Meta data

Meta data can be added to the codebook.

``` r
meta <- createMetadata()
meta[1, "Title"] <- "Codebook Test"
meta[1, "Author"] <- "Anna Muster"
meta[1, "Keywords"] <- "lsa, education"
meta[1, "Subject"] <- "test"
```

``` r
writeExcel(meta, "example_meta.xlsx", row.names = FALSE)
meta_final <- makeMetadata("example_meta.xlsx")
```

## Chapters

Create the chapter structure.

``` r
chapters <- createChapters(varInfo_final2)
chapters[1, 2] <- "Iris Datensatz"
```

## Codebok

Now we create the actual codebook script via calling the
[`codebook()`](https://beckerbenj.github.io/eatCodebook/reference/codebook.md)
function.

``` r
latex_skript <- codebook(varInfo = varInfo_final2, missings = miss_final, struc = struc_final,
                         scaleInfo = scaleInfo_final, dat = eatGADS::extractData(dat),
                         Kennwertedatensatz = descStatistics, chapters = chapters)
#> 
#>  Erstelle Layout-Skripte fuer: dat
#>   Layout der Variable: Sepal_Length
#> Warning in Latex.length(sections.var1[d], FALSE, FALSE): Fuer folgende Zeichen
#> gibt es keine Laengenangaben: NA. Die Laenge von NA in Latex wird daher
#> unterschaetzt.
#>   Layout der Variable: Sepal_Width
#>   Layout der Variable: Petal_Length
#> Warning in Latex.length(sections.var1[d], FALSE, FALSE): Fuer folgende Zeichen
#> gibt es keine Laengenangaben: NA. Die Laenge von NA in Latex wird daher
#> unterschaetzt.
#>   Layout der Variable: Petal_Width
#>   Layout der Variable: Species
#> Warning in Latex.length(sections.var1[d], FALSE, FALSE): Fuer folgende Zeichen
#> gibt es keine Laengenangaben: NA. Die Laenge von NA in Latex wird daher
#> unterschaetzt.
```

## Save the Codebok

The resulting object and the meta data are then separately save to the
hard drive. Both objects should be saved into the same folder.

``` r
write.table(latex_skript , file = "minimal_example.tex" , fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )
write.table(meta_final , file = "minimal_example_meta.xmpdata", fileEncoding="UTF-8" ,
            col.names=FALSE , row.names=FALSE , quote = FALSE )
```

## Compilation

The `LaTeX` script for the codebook has now been saved to the hard
drive. This file should now be opened via a `Tex`-Editor and simply
compiled. Sometimes multiple consecutive compilation steps are required
for a clean output. Alternatively, the document can be compiled from
within `R`, for example via
[`tools::texi2pdf()`](https://rdrr.io/r/tools/texi2dvi.html).
