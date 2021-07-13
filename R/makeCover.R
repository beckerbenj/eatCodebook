
####
#############################################################################
#' Create cover page.
#'
#' Create a cover page.
#'
#'@param logoFile Path to the graphic file.
#'@param maintitle Main title of the document.
#'@param subtitle Sub title of the document.
#'@param authors Authors of the document.
#'@param addAuthors Additional contributers to the document.
#'@param schriftenreihe Schriftenreihe of the document.
#'@param bibinfo Bibiolografic info of the document.
#'
#'@return Returns a latex snippet.
#'
#'@examples
#'makeCover(maintitle = "Study of Achievement", subtitle = "Codebook of Study of Achievement",
#'          authors = "Some Person", addAuthors = "With the help of some other persons",
#'          schriftenreihe = "Book 9 of Studies of Achievement")
#'
#'@export
makeCover <- function(logoFile = NULL, maintitle = NULL, subtitle = NULL, authors = NULL, addAuthors = NULL,
                      schriftenreihe = NULL, bibinfo = NULL) {

  if(!is.null(logoFile)){
    logoFile <- c("\\begin{flushright}",
                         "\\begin{figure}",
                         paste0("\\includegraphics[scale=0.1]{",logoFile,"}"),
                         "\\end{figure}",
                         "\\end{flushright}")
  }

  if(!is.null(maintitle)){
    maintitle <- sonderzeichen.aufbereiten(maintitle)
    maintitle <- c("\\begin{Huge}",
                      paste0("\\color{iqbrot} \\textbf{",maintitle,"} \\par \\medskip"),
                      "\\end{Huge}")
    }

  if(!is.null(subtitle)){
    subtitle <- sonderzeichen.aufbereiten(subtitle )
    subtitle <- c("\\begin{Large}",
                  paste0("\\textbf{",subtitle,"}\\par \\bigskip"),
                  "\\end{Large}")
  }

  if(!is.null(authors)){
    authors <- sonderzeichen.aufbereiten(authors)
    authors <- c("\\begin{large}",
                     authors,
                     "\\end{large}")
  }

  if(!is.null(addAuthors)){
    addAuthors <-  sonderzeichen.aufbereiten (addAuthors)
    addAuthors <- paste0(addAuthors, "\\par")
  }

  if(!is.null(schriftenreihe)){
    schriftenreihe <- sonderzeichen.aufbereiten (schriftenreihe)
    schriftenreihe <- paste0(schriftenreihe," \n")
  }

  if(!is.null(bibinfo)){
    bibinfo <- sonderzeichen.aufbereiten (bibinfo)
    bibinfo <- c("\\textbf{Bibliographische Informationen} \\par",
                bibinfo ,
                "\\par \\bigskip",
                "Alle Rechte vorbehalten.")
  }

c( "\\thispagestyle{empty}",
   logoFile,
   "\\vspace*{75mm}",
   maintitle,
   subtitle,
   authors,
   "\\bigskip\n",
   "\\vfill",
   "Stand: \\today \\par",
   addAuthors,
   schriftenreihe,
   "\\pagebreak",
   "\\thispagestyle{empty}",
   "\\pagenumbering{gobble} % frisst die Seitenzahlen",
   "\\quad",
   "\\vfill\n",
   bibinfo)
}

