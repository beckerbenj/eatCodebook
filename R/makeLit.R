

####
#############################################################################
#' Create literature latex snippet.
#'
#' Create literature latex snippet.
#'
#'@param litInfo Literature information..
#'
#'@return Literature latex snippet.
#'
#'@examples
#'#tbd
#'
#'@export
makeLit <- function(litInfo){
  check_litInfo(litInfo)

  #names(litInfo)[names(litInfo) == "in.Literaturverzeichnis"] <- "in_Literaturverzeichnis"
  litInfo$in_Literaturverzeichnis[ tolower(litInfo$in_Literaturverzeichnis) %in% c("ja", "wahr", "yes", "true")] <- TRUE
  litInfo$in_Literaturverzeichnis[ tolower(litInfo$in_Literaturverzeichnis) %in% c("nein", "no", "false")] <- FALSE
  litInfo$in_Literaturverzeichnis <- as.logical(litInfo$in_Literaturverzeichnis)

  literatur <- NULL

  if(sum(litInfo$in_Literaturverzeichnis) > 0){
    literatur <- litInfo$Langangabe[ litInfo$in_Literaturverzeichnis ]
    literatur <- unique( literatur[ ! is.na( literatur ) ] )
    literatur <- litSort( literatur )
    literatur <- sonderzeichen.aufbereiten(literatur)
    literatur <- paste0( "\\lititem{" , literatur , "}")
    literatur <- c(	"\\phantomsection",
                    "\\section*{Literaturverzeichnis}",
                    "\\setcounter{lit}{\\thepage}",
                    "\\addcontentsline{toc}{section}{Literaturverzeichnis}",
                    "%\\clearscrheadings",
                    "\\ihead[\\leftmark]{\\leftmark \\newline \\textsc{Literaturverzeichnis}}",
                    "%\\cfoot[\\pagemark]{\\pagemark}",
                    "%\\fancyhead[L]{Literaturverzeichnis}",
                    "\\begin{literatur}",
                    literatur,
                    "\\end{literatur}",
                    "\\pagebreak")
  }

  literatur
}


# Sortierung des Literaturverzeichnisses
litSort <- function( lit ) {
  # TODO: Gleiche Namen + andere Jahreszahlen mit aufnehmen

  # Sortierung ueber Nachnamen, die ueber Klammern getrennt sind
  x <- gsub( "(^.*\\([[:digit:]]*\\))\\..*" , "\\1" , lit ) # Alles, was nach ein Klammer kommt (exklusiver dieser) entfernen --> Jahreszahlen behalten
  x <- gsub( "et al\\.", "", x) # "et al." entfernen
  x <- gsub( "&", "", x) # alle &-Zeichen entfernen
  x <- gsub( "\\s\\w\\.-\\w\\." , "" , x ) # Alle Vornamen-Abkuerzungen der Form " X.-X." loeschen
  x <- gsub( "\\s\\w\\." , "" , x ) # Alle Vornamen-Abkuerzungen der Form " X." loeschen
  x <- gsub( ",\\s*,", ",", x) # Doppelte Kommata loeschen
  x <- gsub( "\\.\\s*$" , "," , x ) # Restliche Punkte zu Kommata
  x <- gsub( "([-[:alpha:][:space:]]*)," , "\\(\\1\\)", x) # Alle durch Kommata getrennte Zeichenfolgen durch Klammern trennen

  starts_with_bracket <- grepl("^\\(" , x , fixed=FALSE)
  x[!starts_with_bracket] <- gsub("(^.*)(\\()" , "\\(\\1\\)\\2" , x[!starts_with_bracket])

  x <- gsub( "\\)\\s\\s*\\(", "\\)\\(", x) # Ueberfluessige Leerzeichen zwischen Klammern loeschen
  x <- gsub( "\\)\\s*" , "\\)" , x) # Ueberfluessige Leerzeichen am Ende loeschen
  x <- gsub( "(\\()\\s*(\\w+)" , "\\1\\2" , x ) # Leerzeichen am Anfang einer Klammer loeschen

  x <- lit[ order(x) ]

  x
}
