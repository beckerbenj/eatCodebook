get.lit.info <- function(varue.file , sheets , lit.cols=c( "Kurzangabe" , "Langangabe" , "in.Literaturverzeichnis" )){
  varue.lit <- readWorkbook (xlsxFile = varue.file , sheet = sheets, startRow = 1 ) # Nur benötige Spalten; Ab erster Zeile beginnen --> Spalten bekommen richtige Überschrift
  varue.lit <- varue.lit[, lit.cols]

  return(varue.lit)
}
