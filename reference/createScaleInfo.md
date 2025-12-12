# Create scale information.

Create information on scale or imputation aggregation. Scales, 'fake
scales', and imputed variables are listed.

## Usage

``` r
createScaleInfo(inputForDescriptives)
```

## Arguments

- inputForDescriptives:

  `inputForDescriptives` object as created by the
  `createInputForDescriptives` function

## Value

Returns a `data.frame` or list of `data.frames` with the following
information:

- `varName` The name of the variable as it occurs in the data

- `Anzahl_valider_Werte` For a scale, how many values have to be not NA
  on items for a non NA value on the scale?

- `Items_der_Skala` Which items or imputations belong to the scale? This
  column is used to display the item names in the codebook.

- `Imputationen` Which imputations belong to the pooled variable? This
  column is used to display the number of imputations in the codebook.

## Details

Currently, displaying the items of a imputed scale is not supported by
the
[`codebook()`](https://beckerbenj.github.io/eatCodebook/reference/codebook.md)
function. Even though the items of an imputed scale can be defined in
the `scaleInfo`, this information is simply ignored during the creation
of the codebook.

## Examples

``` r
# import spss exemplary data
file <- system.file("extdata", "example1_clean.sav", package = "eatCodebook")
dat   <- eatGADS::import_spss(file)
inputForDescriptives <- createInputForDescriptives(dat, impExpr = "plausible value")

scaleInfo <- createScaleInfo(inputForDescriptives)
```
