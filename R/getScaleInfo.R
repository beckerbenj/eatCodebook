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
  getExcel(filePath, funList = list(check_scaleInfo, prepareScaleInfo))
}


check_scaleInfo <- function(scaleInfo) {
  if(!is.data.frame(scaleInfo)) {
    stop("'scaleInfo' needs to be a data.frame.")
  }
  if(!identical(names(scaleInfo), c('varName', 'Anzahl_valider_Werte', 'Items_der_Skala', 'Imputationen'))) {
    stop("The column names of 'scaleInfo' need to be: 'varName', 'Anzahl_valider_Werte', 'Items_der_Skala', 'Imputationen'.")
  }
  if(!length(unique(scaleInfo[,"varName"])) == length(scaleInfo[,"varName"])) {
    stop("'varName' column in 'scaleInfo' must be unique.")
  }

  if(tibble::is_tibble(scaleInfo)) {
    inputForDescriptives <- as.data.frame(scaleInfo)
  }
  scaleInfo
}


# Remove spaces from columns
prepareScaleInfo <- function(scaleInfo){
  for(i in names(scaleInfo)){
    scaleInfo[,i] <- as.character(scaleInfo[,i])
  }

  scaleInfo$Anzahl_valider_Werte[is.na(scaleInfo$Anzahl_valider_Werte) | grepl("", scaleInfo$Anzahl_valider_Werte)] <- "-"
  scaleInfo$Anzahl_valider_Werte <- gsub("\\s" , "" , scaleInfo$Anzahl_valider_Werte)
  scaleInfo$Items_der_Skala <- gsub("\\s" , "" , scaleInfo$Items_der_Skala)

  scaleInfo
}
