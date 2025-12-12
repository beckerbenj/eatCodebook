# Create codebook script.

Create the complete codebook latex script.

## Usage

``` r
codebook(
  varInfo,
  missings,
  struc,
  scaleInfo,
  register = NULL,
  dat,
  Kennwertedatensatz,
  chapters,
  deckblatt = "",
  intro = "",
  literatur = "",
  abkuerzverz = "",
  hintmod = "",
  lastpage = ""
)
```

## Arguments

- varInfo:

  `data.frame` or list of `data.frames` containing the variable
  information, imported via
  [`getVarInfo`](https://beckerbenj.github.io/eatCodebook/reference/getVarInfo.md).

- missings:

  `data.frame` or list of `data.frames` containing the missing
  information, imported via
  [`getMissings`](https://beckerbenj.github.io/eatCodebook/reference/getMissings.md).

- struc:

  `data.frame` containing the structure of the codebook, imported via
  [`getStructure`](https://beckerbenj.github.io/eatCodebook/reference/getStructure.md).

- scaleInfo:

  `data.frame` or list of `data.frames` containing the information on
  scales, imported via
  [`getScaleInfo`](https://beckerbenj.github.io/eatCodebook/reference/getScaleInfo.md).

- register:

  `data.frame` containing the information on the register, imported via
  [`getRegister`](https://beckerbenj.github.io/eatCodebook/reference/getRegister.md).
  If `NULL`, now register is created. If there are registers for some
  data sets but not all, the missing registers are simply omitted.

- dat:

  `data.frame` or list of `data.frames` containing the data sets,
  imported via
  [`import_spss`](https://beckerbenj.github.io/eatGADS/reference/import_spss.html).

- Kennwertedatensatz:

  `data.frame` or list of `data.frame` containing the descriptive
  statistics, imported via
  [`calculateDescriptives`](https://beckerbenj.github.io/eatCodebook/reference/calculateDescriptives.md).

- chapters:

  `data.frame` or list of `data.frames` containing the chapter
  information, imported via
  [`getChapters`](https://beckerbenj.github.io/eatCodebook/reference/getChapters.md).
  Determines the order of chapters in the codebook.

- deckblatt:

  Character vector with the cover page, created via
  [`makeCover`](https://beckerbenj.github.io/eatCodebook/reference/makeCover.md).

- intro:

  Character vector, introduction.

- literatur:

  Character vector with the literature information, created via
  [`makeLit`](https://beckerbenj.github.io/eatCodebook/reference/makeLit.md).

- abkuerzverz:

  Character vector with the abbreviations, created via
  [`makeAbbrList`](https://beckerbenj.github.io/eatCodebook/reference/makeAbbrList.md).

- hintmod:

  Character vector with the information on the background model, created
  via
  [`makeBGM`](https://beckerbenj.github.io/eatCodebook/reference/makeBGM.md).

- lastpage:

  Character vektor, last page.

## Value

Codebook latex script.

## Examples

``` r
#tbd
```
