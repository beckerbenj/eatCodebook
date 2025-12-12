# Calculate descriptive statistics.

Calculate descriptive statistics which should be included in the
codebook.

## Usage

``` r
calculateDescriptives(
  GADSdat,
  inputForDescriptives,
  verbose = FALSE,
  showCallOnly = FALSE
)
```

## Arguments

- GADSdat:

  Object of class `GADSdat`, created for example by `import_spss` from
  the `eatGADS` package.

- inputForDescriptives:

  `data.frame` with variable information. This table can either be
  created manually in Excel or generated from GADSdat object, using the
  function `createInputForDescriptives`. If you need a template of what
  this table should look like for different variables, you can use the
  `varInfo` object created in the examples below. Specifically, the
  `data.frame` must contain the following columns.

  - `varName`: This character column should contain all variable names
    that are part of the GADS database.

  - `varLabel`: The variable label of the corresponding variable in the
    `varName` column

  - `format`: This character columns yields the format of the
    corresponding variable in the `varName` column, for example `F8.0`

  - `imp`: Logical column (i.e., `TRUE/FALSE`) which indicates whether
    the variable is imputed

  - `type`: Character column with possible entries `variable`, `item`,
    `fake_item` or `scale`. The entry `variable` should be used for
    individual variables in the `GADSdat` database that belong neither
    to a scale nor to a fake scale. The entry `item` should be used for
    items which belong to a scale. No distinction needs to be made as to
    which scale these items belong to. It is only important that the
    database contains one or more scales for each of these items. Assume
    that the database contains the scales "self concept" and "interest",
    where "self concept" is measured with the items `SK_I1`, `SK_I2`,
    `SK_I3`, and "interest" is measured with the items `Int_I1`,
    `Int_I2`, `Int_I3`, `Int_I4`. Then `SK_I1`, `SK_I2`, `SK_I3`,
    `Int_I1`, `Int_I2`, `Int_I3`, and `Int_I4` should occur in the
    `varName` column, and the corresponding entry in `type` should be
    `item`. Note that the database also contains the scale variables of
    "self concept" and "interest". The entry `fake_item` should be used
    for items that theoretically constitute one or more latent scales,
    but for which no scale variable is contained in the database.

  - `scale`: Character column with the scale level of the corresponding
    variable. Possible entries are `numeric` for metric variables,
    `ordinal` for categorical (factor) variables, or `nominal`.

  - `group`: Character column which is only relevant for items, scales
    or fake scale variables. For all others, only the variable name is
    transferred here. For the other variables, an indicator is defined
    here that contains the assignment of items to their scales. Assume
    that the database contains the scale "self concept" which is
    measured with the items `SK_I1`, `SK_I2`, `SK_I3`. Let the scale
    variable of "self concept" be called `SC`. A common entry for
    `SK_I1`, `SK_I2`, `SK_I3`, and `scaleSelfConcept` should be entered
    in the `group` column, for example `"scaleSelfConcept"`

- verbose:

  Print variable and function information to console?

- showCallOnly:

  Logical: only for diagnostics. If TRUE, no calculation is proceed, and
  only the function which is called for calculation is returned.

## Value

Returns a list of descriptive statistics. This format may seem
unsuitable for direct further processing in R, but is primarily used for
the automatic creation of codebooks using Latex.

## Examples

``` r
# import spss exemplary data
file <- system.file("extdata", "example1_clean.sav", package = "eatCodebook")
dat   <- eatGADS::import_spss(file)
# create variable information by the eatCodebbok function createInputForDescriptives
# This table 'varInfo' can be exported to Excel for further inspection and used as a
# blueprint of what the necessary 'inputForDescriptives' argument for the
# calculateDescriptives() function should look like
varInfo <- createInputForDescriptives(dat, impExpr = "plausible value")
# calculate descriptives
descr <- calculateDescriptives(dat, varInfo)
#> Warning: the ‘isNested’ function has moved to the reformulas package. Please update your imports, or ask an upstream package maintainter to do so.
#> This warning is displayed once per session.
#> Warning: `report()` was deprecated in eatRep 0.15.0.
#> ℹ For the original behavior of report() please use eatRep version 0.14.7:
#>   'https://cran.r-project.org/src/contrib/Archive/eatRep/'
#> ℹ The deprecated feature was likely used in the eatCodebook package.
#>   Please report the issue to the authors.
```
