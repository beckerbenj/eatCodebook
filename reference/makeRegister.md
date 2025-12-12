# Create register.

Create register latex snippet.

## Usage

``` r
makeRegister(fbshort, fblong, fb.akt, varue.reg, double.vars)
```

## Arguments

- fbshort:

  Short name of data set

- fblong:

  Full name of data set

- fb.akt:

  Relevant entry from `fbshort`.

- varue.reg:

  Informationen zum Register: data.frame, Spalten sind Schlagwoerter,
  die im Register aufgelistet sind, Zeilen sind VarNamen. Eintraege sind
  `"x"` oder `""`, ob Variable unter dem Schlagwort im Register
  aufgelistet werden soll.

- double.vars:

  Character vector containing duplicate variable names between different
  data sets.

## Value

Returns a (character vector) latex snippet.

## Examples

``` r
#tbd
```
