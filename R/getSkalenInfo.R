####
#############################################################################
#' Get scale information.
#'
#' Get information on scale aggregation.
#'
#'@param filePath Path to the excel-file.
#'
#'@return Returns the scale information.
#'
#'@examples
#'#tbd
#'
#'@export
getScaleInfo <- function(filePath){
  skalen.info <- openxlsx::readWorkbook(xlsxFile = filePath, startRow = 1 )
  skalen.info$Anzahl.valider.Werte[is.na(skalen.info$Anzahl.valider.Werte) | grepl("\\s","",skalen.info$Anzahl.valider.Werte) %in% "" ] <- "-"

  skalen.info <- skalen.info.aufbereiten(skalen.info)

  skalen.info
}


# Remove spaces from columns
skalen.info.aufbereiten <- function(skalen.info){
  for(i in names(skalen.info)){
    skalen.info[,i] <- as.character( skalen.info[,i] )
  }

  skalen.info$Anzahl.valider.Werte <- gsub("\\s" , "" , skalen.info$Anzahl.valider.Werte)
  skalen.info$Items.der.Skala <- gsub("\\s" , "" , skalen.info$Items.der.Skala)

  skalen.info
}
