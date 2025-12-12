# Create cover page.

Create a cover page.

## Usage

``` r
makeCover(
  logoFile = NULL,
  maintitle = NULL,
  subtitle = NULL,
  authors = NULL,
  addAuthors = NULL,
  schriftenreihe = NULL,
  bibinfo = NULL
)
```

## Arguments

- logoFile:

  Path to the graphic file.

- maintitle:

  Main title of the document.

- subtitle:

  Sub title of the document.

- authors:

  Authors of the document.

- addAuthors:

  Additional contributers to the document.

- schriftenreihe:

  Schriftenreihe of the document.

- bibinfo:

  Bibiolografic info of the document.

## Value

Returns a latex snippet.

## Examples

``` r
makeCover(maintitle = "Study of Achievement", subtitle = "Codebook of Study of Achievement",
         authors = "Some Person", addAuthors = "With the help of some other persons",
         schriftenreihe = "Book 9 of Studies of Achievement")
#>  [1] "\\thispagestyle{empty}"                                        
#>  [2] "\\vspace*{75mm}"                                               
#>  [3] "\\begin{Huge}"                                                 
#>  [4] "\\color{iqbrot} \\textbf{Study of Achievement} \\par \\medskip"
#>  [5] "\\end{Huge}"                                                   
#>  [6] "\\begin{Large}"                                                
#>  [7] "\\textbf{Codebook of Study of Achievement}\\par \\bigskip"     
#>  [8] "\\end{Large}"                                                  
#>  [9] "\\begin{large}"                                                
#> [10] "Some Person"                                                   
#> [11] "\\end{large}"                                                  
#> [12] "\\bigskip\n"                                                   
#> [13] "\\vfill"                                                       
#> [14] "Stand: \\today \\par"                                          
#> [15] "With the help of some other persons\\par"                      
#> [16] "Book 9 of Studies of Achievement \n"                           
#> [17] "\\pagebreak"                                                   
#> [18] "\\thispagestyle{empty}"                                        
#> [19] "\\pagenumbering{gobble} % frisst die Seitenzahlen"             
#> [20] "\\quad"                                                        
#> [21] "\\vfill\n"                                                     
```
