# Import APA references from an Excel sheet.

Import references with APA standard from one sheet from an Excel file,
while converting italic formatting and URLs into proper LaTeX code. If
the references do not comply with APA standards (e.g. book or journal
titles in italic), this function might not work properly. For instance,
if there is more than one continuous italic sequence per cell. Use case
for creating a codebook: the reference list is saved in Excel, but the
pdf is created wit TeXWorks, so all special formatting needs LaTeX
syntax to be displayed correctly in the final codebook.

## Usage

``` r
getAPAInfo(filePath, sheet = 2)
```

## Arguments

- filePath:

  Path to the Excel file containing a sheet with the reference list.
  There should be exactly one reference list with two columns
  \`Kurzangabe\` (containing in-text citations) and \`Langangabe\`
  (containing the references).

- sheet:

  Number of the sheet in the Excel file of the reference list. The
  default is \`sheet = 2\`, as it is the most common case for
  Bildungstrend studies.

## Value

A `data.frame` with in-text citations and APA standard references. The
references in the column \`Langangabe\` contain LaTeX syntax for italic
sequences and URLs.

## Examples

``` r
# import reference example Excel
file <- system.file("extdata", "example_literatur.xlsx", package = "eatCodebook")
references <- getAPAInfo(filePath = file, sheet = 2)
```
