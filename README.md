
# eatCodebook

<!-- badges: start -->
[![R-CMD-check](https://github.com/beckerbenj/eatCodebook/workflows/R-CMD-check/badge.svg)](https://github.com/beckerbenj/eatCodebook/actions)
[![Codecov test coverage](https://codecov.io/gh/beckerbenj/eatCodebook/branch/main/graph/badge.svg)](https://app.codecov.io/gh/beckerbenj/eatCodebook?branch=main)
<!-- badges: end -->

eatCodebook (**e**ducational **a**ssessment **t**ools: **C**odebook) allows the automatic creation of a nicely formatted codebook via `LaTeX`.

## Installation

You can install the development version of eatCodebook from Github with

``` r
remotes::install_github("beckerbenj/eatCodebook", build_vignettes = TRUE)
```

## Documentation

Package functionality is documented within package vignettes. These can be accessed via 

``` r
# minimal working example using the 'iris' data set
vignette("minimal_example", package = "eatCodebook")

# full workflow example using a PISA example data set
vignette("full_workflow", package = "eatCodebook")
```

