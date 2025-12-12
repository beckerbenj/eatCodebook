# Check whether the mean of a scale variable equals the pooled mean of single items

Function need the output of the
[`createInputForDescriptives()`](https://beckerbenj.github.io/eatCodebook/reference/createInputForDescriptives.md)
function. For each non-imputed scale, the function checks whether the
(unweighted) mean of the scale variable equals the pooled mean of the
single items which form the scale.

## Usage

``` r
checkScaleConsistency(
  GADSdat,
  inputForDescriptives,
  id,
  tolerance = 0.02,
  verbose = TRUE
)
```

## Arguments

- GADSdat:

  Object of class`GADSdat`

- inputForDescriptives:

  The output of the function
  [`createInputForDescriptives()`](https://beckerbenj.github.io/eatCodebook/reference/createInputForDescriptives.md)

- id:

  Name or column number of the ID variable. Argument can be numeric or
  character

- tolerance:

  A positive numeric value, indicating the maximum allowed discrepancy
  between the mean of a scale variable and the pooled mean of single
  items

- verbose:

  Logical: Print informations on console?

## Value

Function does not return output but provide messages.
