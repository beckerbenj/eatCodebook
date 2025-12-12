# Import data from Excel.

Import all sheets from an Excel file.

## Usage

``` r
getExcel(filePath, funList = NULL)
```

## Arguments

- filePath:

  Path to excel file.

- funList:

  tbd.

## Value

Either a `data.frame` (if the Excel file has a single sheet) or a named
`list` (if the Excel file has multiple sheets).

## Examples

``` r
filePath <- system.file("extdata", "example_abbrList.xlsx", package = "eatCodebook")
excel_list   <- getExcel(filePath)
```
