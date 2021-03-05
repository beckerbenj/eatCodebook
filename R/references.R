# Sortierung des Literaturverzeichnisses
lit.sort <- function( lit ) {
  # INPUT:
  #	lit: Character-Vektor der Langangaben im Literaturverzeichnisses
  # OUTPUT:
  #	x: Sortierte Einträge


  # TODO: Gleiche Namen + andere Jahreszahlen mit aufnehmen

  #### Sortierung ####

  # Sortierung über Nachnamen, die über Klammern getrennt sind
  x <- gsub( "(^.*\\([[:digit:]]*\\))\\..*" , "\\1" , lit ) # Alles, was nach ein Klammer kommt (exklusiver dieser) entfernen --> Jahreszahlen behalten
  x <- gsub( "et al\\.", "", x) # "et al." entfernen
  x <- gsub( "&", "", x) # alle &-Zeichen entfernen
  x <- gsub( "\\s\\w\\.-\\w\\." , "" , x ) # Alle Vornamen-Abkürzungen der Form " X.-X." löschen
  x <- gsub( "\\s\\w\\." , "" , x ) # Alle Vornamen-Abkürzungen der Form " X." löschen
  x <- gsub( ",\\s*,", ",", x) # Doppelte Kommata löschen
  x <- gsub( "\\.\\s*$" , "," , x ) # Restliche Punkte zu Kommata
  x <- gsub( "([-[:alpha:][:space:]]*)," , "\\(\\1\\)", x) # Alle durch Kommata getrennte Zeichenfolgen durch Klammern trennen
  x[! grepl("^\\(" , x , fixed=FALSE) ] <- gsub("(^.*)(\\()" , "\\(\\1\\)\\2" , x[!grepl("^\\(" , x , fixed=FALSE)] , ")")
  x <- gsub( "\\)\\s\\s*\\(", "\\)\\(", x) # Überflüssige Leerzeichen zwischen Klammern löschen
  x <- gsub( "\\)\\s*" , "\\)" , x) # Überflüssige Leerzeichen am Ende löschen
  x <- gsub( "(\\()\\s*(\\w+)" , "\\1\\2" , x ) # Leerzeichen am Anfang einer Klammer löschen

  x <- lit[ order(x) ]

  #### Output ####
  return ( x )
}

# Erstellung des Literaturverzeichnisses
make.lit <- function(varue.lit){
  # INPUT:
  #	varue.lit: data.frame, Übersicht aller Literatureinträge in zwei Spalten (Langangabe + TRUE/FALSE-Angabe, ob Eintrag ins Literaturverzeichnis kommt)
  # OUPTUT:
  #	literatur: character-vector, Latex-Skript zur Erstellung des Literaturverzeichnisses

  varue.lit$in.Literaturverzeichnis[ tolower(varue.lit$in.Literaturverzeichnis) %in% "ja"] <- TRUE
  varue.lit$in.Literaturverzeichnis[ tolower(varue.lit$in.Literaturverzeichnis) %in% "wahr"] <- TRUE
  varue.lit$in.Literaturverzeichnis <- as.logical(varue.lit$in.Literaturverzeichnis)
  literatur <- varue.lit$Langangabe[ varue.lit$in.Literaturverzeichnis ]
  if( length(literatur)>0){
    literatur <- unique( literatur[ ! is.na( literatur ) ] )
    literatur <- lit.sort( literatur )
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
  } else {
    literatur <- NULL
  }



  return(literatur)
}
