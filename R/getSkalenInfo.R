get.skalen.info <- function(varue.file , sheets, skalen.cols=c( "Var.Name", "Quelle" , "Anzahl.valider.Werte" , "Items.der.Skala" )){
  skalen.info <- readWorkbook ( xlsxFile = varue.file , sheet = sheets , startRow = 1 )
  skalen.info <- skalen.info[ , skalen.cols ]
  skalen.info$Anzahl.valider.Werte[is.na(skalen.info$Anzahl.valider.Werte) | grepl("\\s","",skalen.info$Anzahl.valider.Werte) %in% "" ] <- "-"

  skalen.info <- skalen.info.aufbereiten(skalen.info)

  return(skalen.info)
}


# Aufbereitung der Varue und der Skaleninformation
skalen.info.aufbereiten <- function(skalen.info){
  for(i in names(skalen.info)){
    skalen.info[,i] <- as.character( skalen.info[,i] )
  }

  if(any(grepl("\\s" , skalen.info$Var.Name))){
    cat(paste0("  Die Variable/Variablen ", skalen.info$Var.Name[grepl("\\s" , skalen.info$Var.Name)] ," besitzt/besitzen Leerzeichen im Variablennamen. Diese werden entfernt.\n"))
    flush.console()
    skalen.info$Var.Name <- gsub("\\s" , "" , skalen.info$Var.Name)
  }

  cat(paste0("  Ueberfluessige Leerzeichen entfernen.\n"))
  flush.console()
  skalen.info$Anzahl.valider.Werte <- gsub("\\s" , "" , skalen.info$Anzahl.valider.Werte)
  skalen.info$Items.der.Skala <- gsub("\\s" , "" , skalen.info$Items.der.Skala)

  return(skalen.info)

}
