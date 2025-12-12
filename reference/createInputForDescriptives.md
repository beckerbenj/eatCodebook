# Create input data.frame for subsequent calculation of descriptives

Create a variable information data.frame from the GADSdat object. This
input can be used to calculate the descriptives of the data via the
`calculateDescriptives` function. See the details section for further
information.

## Usage

``` r
createInputForDescriptives(
  GADSdat,
  idExpr = "^ID",
  impExpr = c("IMPUTATION\\s+{0,1}[[:digit:]]{1,2}", "PV\\s+{0,1}[[:digit:]]{1,2}"),
  scaleExpr = "^Skala",
  itemExpr = "indikator",
  fakeItemExpr = "fake",
  nwExpr = "IDinClass",
  varNameSeparatorImp = "_",
  ncharSeparatorImp = 2,
  lastOccurrence = TRUE,
  groupSuffixImp = "imp",
  nCatsForOrdinal = c(2:5),
  nwVarNameSeparatorImp = "_",
  nwNcharSeparatorImp = 6,
  nwLastOccurrence = TRUE,
  verbose = FALSE
)
```

## Arguments

- GADSdat:

  Object of class `GADSdat`, created by `import_spss` from the `eatGADS`
  package, for example. Alternatively, a list of objects of class
  `GADSdat`

- idExpr:

  Regular expression to identify ID variables from variable names (Note:
  for multiple expressions, i.e. if `idExpr` is a character vector of
  length \> 1, at least one expression should match to identify the
  variable as ID variable). The use of a regular expression is due to
  the fact that the data records often contain not just one, but several
  identification variables, for example for pupils, teachers, schools,
  classes or federal states. Logically, no descriptive values are
  reported for identification variables. They must be specified here so
  that the subsequent `calculateDescriptives` function knows, so to
  speak, for which variables no descriptive values are to be calculated.

- impExpr:

  Regular expression to identify imputed variables from variable labels
  in GADSdat object (Note: for multiple expressions, i.e. if `impExpr`
  is a character vector of length \> 1, at least one expression should
  match to identify the variable as an imputed variable). Regular
  expressions are also used here, as several variables (each with
  different individual IDs) can be imputed.

- scaleExpr:

  Regular expression to identify scale or fake scale variables from
  variable labels in GADSdat object (Note: for multiple expressions,
  i.e. if `scaleExpr` is a character vector of length \> 1, at least one
  expression should match to identify the variable as a scale variable).
  Scales are defined when several items measure a common (latent)
  construct and there is also a variable that represents the scale
  value. The scale variable represents usually the averaged (or
  otherwise aggregated) value within a person across all items of this
  scale. Fake scales are defined when several items measure a common
  (latent) construct, but there is no additional variable that
  represents the scale value.

- itemExpr:

  Regular expression to identify items which constitute a true scale
  from the variable labels in GADSdat object. Note: Only the regular
  expressions that identify the items must be entered here. The
  additional scale variables do not have to be specified. If several
  scales are defined in the data set (e.g. "self concept" and interest),
  no distinction needs to be made here as to which items belong to which
  scale. (This is done elsewhere.) Assume that the "self concept" is
  measured with the items `SK_I1`, `SK_I2`, `SK_I3`, and the scale
  variable is called `SK_scale`. Let us also assume that "interest" is
  measured with the items `Int_I1`, `Int_I2`, `Int_I3`, `Int_I4`, and
  that the scale variable is called `Int_scale`. Then it could be
  specified here: `itemExpr = "I[1-4]{1}$"`

- fakeItemExpr:

  Regular expression to identify fake items which constitute a fake
  scale from the variable labels in GADSdat object. This works in the
  same way as with `itemExpr`.

- nwExpr:

  Regular expression to identify network variables from variable labels
  in GADSdat object (Note: for multiple expressions, i.e. if `nwExpr` is
  a character vector of length \> 1, at least one expression should
  match to identify the variable as a network variable)

- varNameSeparatorImp:

  character sign to separate the "pooled" suffix from group name in
  group column. For example, if multiple imputed variables occur in the
  wide-format data.frame as `pv_1`, `pv_2`, `pv_3`, use `"_"`. If no
  such sign exists in the data, i.e. if multiple imputations occur as
  `pv1`, `pv2`, `pv3`, instead of `pv_1`, `pv_2`, `pv_3`, or `pv.1`,
  `pv.2`, `pv.3`, use `NA` or `NULL` or `""`. In this case, you will
  have to specify the `ncharSeparatorImp` argument.

- ncharSeparatorImp:

  Integer: only relevant if no `varNameSeparatorImp` exists, i.e. if
  multiple imputations occur as `pv1`, `pv2`, `pv3`, instead of `pv_1`,
  `pv_2`, `pv_3`, or `pv.1`, `pv.2`, `pv.3`. `ncharSeparatorImp` than
  specifies the number of character signs which should be trimmed to
  identify the common variable stem. If `varNameSeparatorImp` is not
  `NA` or `NULL` or `""`, `ncharSeparatorImp` will be ignored. For
  example, if multiple imputations occur as `pv_1`, `pv_2`, `pv_3`, use
  `varNameSeparatorImp = "_"`. If multiple imputations occur as `pv1`,
  `pv2`, `pv3`, use `varNameSeparatorImp = NULL` and
  `ncharSeparatorImp = 2`. The first 2 signs of variables names (i.e.,
  `"pv"`) will be used to identify the imputed variables which belong to
  a common stem.

- lastOccurrence:

  Logical: If `varNameSeparatorImp` occurrs multiple times within a
  string, `lastOccurrence` defines whether the last occurrence should be
  used for splitting

- groupSuffixImp:

  tbd

- nCatsForOrdinal:

  Numeric vector with number of categories considered for ordinal
  variables. Variables with number of categories as defined here are
  considered to be ordinal instead of nominal. If NULL, this rule will
  be ignored, and nominal/ordinal assignment is done in other ways

- nwVarNameSeparatorImp:

  character sign to separate network variable names from network
  variable groups. For example, if network variables occur as
  `friend_1`, `friend_2`, ..., `friend_12`, use `"_"`. If no such sign
  exists in the data, i.e. if network variable names occur as `friend1`,
  `friend2`, ..., `friend12`, use `NA` or `NULL` or `""`. In this case,
  you will have to specify the `nwNcharSeparatorImp` argument.

- nwNcharSeparatorImp:

  Integer: only relevant if no `nwVarNameSeparatorImp` exists, i.e. if
  network variables occur as `friend1`, `friend2`, ..., `friend12`,
  instead of `friend_1`, `friend_2`, ..., `friend_12`.
  `nwVcharSeparatorImp` than specifies the number of character signs
  which should be trimmed to identify the common variable stem. If
  `nwVarNameSeparatorImp` is not `NA` or `NULL` or `""`,
  `ncharSeparatorImp` will be ignored. For example, if network variables
  occur as `friend_1`, `friend_2`, ..., `friend_12`, use
  `nwVarNameSeparatorImp = "_"`. If network variables occur as
  `friend1`, `friend2`, ..., `friend12`, use
  `nwVarNameSeparatorImp = NULL` and `nwNcharSeparatorImp = 6`. The
  first 6 signs of variables names (i.e., `"friend"`) will be used to
  identify the group.

- nwLastOccurrence:

  Logical: If `nwVarNameSeparatorImp` occurrs multiple times within a
  string, `nwLastOccurrence` defines whether the last occurrence should
  be used for splitting

- verbose:

  Should scale identification be reported?

## Value

Returns a `data.frame` with variable information with following columns

- `varName` The name of the variable as it occurs in the data

- `varLabel` The label of the variable as it occurs in the `GADSdat`
  label sheet

- `format` The variable format as displayed in the labels sheet of the
  `GADSdat` object

- `imp` Logical: Whether or not the variable is imputed

- `type` The type of the variable. Four possible entries with following
  meanings: `variable` for single variables which do not belong to a
  scale. Variables may be imputed or not. `item` for individual items
  that define or operationalize a common scale. `scale` for scale values
  formed by aggregating (averaging or summing) different items.
  `fake_item` for individual items that may define or operationalize a
  common scale, whereby the scale value is not included as an additional
  variable in the data set.

- `scale` The scale level of the variable. Possible entries:
  `categorical`, `ordinal`, `numeric`. ID variables and character
  variables have missing entries in this column. Be cautious that
  'ordinal' sometimes may be allocated erroneously. The resulting table
  should be exported to Excel for further checks. For categorical
  variables, only freqency tables are given in the codebook. For numeric
  variables, mean and standard deviation are given in the codebook. For
  ordinal variables, frequency tables and mean and standard deviation
  are given in teh codebook.

- `group` If the variable is part of a scale with several items, a
  common entry in the group column indicates that these variables belong
  together

## Details

The `eatCodebook` package aims to create a human-readable pdf codebook
from a `GADSdat` data base object. The codebook contains information
about the variables used in the study, including their descriptive
properties. Which descriptive properties are reported in the codebook
depends, among other things, on the scale level of the variables. For
example, the mean and standard deviation are reported for metric
variables and frequency distributions for nominal variables. For ordinal
variables whose categories are labeled numerically in ascending order,
even though they represent ascending categories rather than a metric
relationship, the means of the numerical categories and frequencies are
returned. All three types (metric, nominal, and ordinal) can occur as
individual variables or as scales. Scales consist of various individual
variables that represent or operationalize a common construct. For a
scale, both the individual variables that make up the scale and the
scale value are described in the scale manual. Both individual variables
and scales can be imputed or not imputed. For non-imputed variables, the
proportion of missing values is also given, and for scale variables, the
number of items that make up the scale and the internal consistency of
the scale are given. The codebook is created in several steps. In the
first step, the `createInputForDescriptives` function is used to
generate an auxiliary object from the database that contains information
on what type of descriptive information is to be reported for which
variable. The object created in this function is the basis for the
`calculateDescriptives` function.

## Examples

``` r
varInfo <- createInputForDescriptives(eatGADS::pisa, impExpr = "Plausible Value")
```
