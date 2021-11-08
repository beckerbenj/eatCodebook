# Funktion für Zaehlervariablen
numtolet <- function( name , fb , double.vars) {
  # INPUT
  #	name: Character. Im Skript ist es immer der Name einer Variable, wie sie in der Varue erscheint
  #	fb: Character, Kürzel für den Fragebogen aus fbshort
  # OUTPUT:
  #	countername: Name der Zaehlervariable, die in Latex verwendet wird

  #### Vorbereitung ####

  # YRNTCSH! - Variablen, die namentlich in allen Datensaetzen vorkommen, d.h. der Name der Variable ist entscheidend, nicht der Inhalt.
  #	Variablennamen, der in mind. 2 Datensaetzen auftaucht, müssen hier gesondert behandelt werden,
  #	da sonst mind. 2 Mal die selbe Zaehlervariable erstellt wird. Die Funktion pastet das Fragebogen-
  #	Kürzel ans Ende des Names der Zaehlervariable, um eine eindeutige Zuordnung zu garantieren.
  #	Der Variablenname selbst wird nicht veraendert, nur der Name der Zaehlervariable in Latex.
  #	Hier muss als "IDSCH_FDZ" durch die entsprechende(n) Variable(n) ersetzt werden.

  # name aendern, falls Variable in mehr als einem Datensatz ist
  if( tolower( name ) %in% tolower(double.vars) ) {
    name <- paste0(name,tolower(fb))
  }

  # Codierungsvorschrift, um Zahlen und Sonderzeichen in den Variablennamen zu aendern
  letts	<- c( paste0("NUMBER" , c("ZERO", "ONE", "TWO", "THREE", "FOUR", "FIVE", "SIX", "SEVEN", "EIGHT", "NINE" )) , "DASH", "UNDER", "POINT" , "SPACE" , "ss")
  names ( letts ) <- c( 0:9 , "\\" , "_" , ".", " ", "ß")

  #### Erstellung des Namens der Zaehlervariable
  countername <- unlist(strsplit( name , split="") )
  countername[ ! is.na( letts[ countername ] ) ] <- letts[ countername[ ! is.na( letts[ countername ] ) ] ]
  countername <- paste(countername, collapse ="")


  ### Output ####
  return ( countername )
}

