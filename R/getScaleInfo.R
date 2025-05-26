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
  check_scaleInfo(skalen.info)

  skalen.info$Anzahl_valider_Werte[is.na(skalen.info$Anzahl_valider_Werte) | grepl("",skalen.info$Anzahl_valider_Werte)] <- "-"

  skalen.info <- skalen.info.aufbereiten(skalen.info)

  skalen.info
}


# Remove spaces from columns
skalen.info.aufbereiten <- function(skalen.info){
  for(i in names(skalen.info)){
    skalen.info[,i] <- as.character( skalen.info[,i] )
  }

  skalen.info$Anzahl_valider_Werte <- gsub("\\s" , "" , skalen.info$Anzahl_valider_Werte)
  skalen.info$Items_der_Skala <- gsub("\\s" , "" , skalen.info$Items_der_Skala)

  skalen.info
}

check_scaleInfo <- function(scaleInfo) {
  if(!is.data.frame(scaleInfo)) stop("'scaleInfo' needs to be a data.frame.")
  if(!identical(names(scaleInfo), c('varName', 'Quelle', 'Anzahl_valider_Werte', 'Items_der_Skala'))) {stop("The column names of 'scaleInfo' need to be: 'varName', 'Quelle', 'Anzahl_valider_Werte', 'Quelle'.")}
  if(!length(unique(scaleInfo[,"varName"])) == length(scaleInfo[,"varName"])) {stop("'varName' column in 'scaleInfo' must be unique.")}

  if(tibble::is_tibble(scaleInfo)) inputForDescriptives <- as.data.frame(scaleInfo)
  scaleInfo
}
