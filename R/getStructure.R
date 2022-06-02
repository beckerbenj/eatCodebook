####
#############################################################################
#' Import structure information.
#'
#' Import structure information.
#'
#'@param filePath Path to excel file.
#'
#'@return Structure information.
#'
#'@examples
#'#tbd
#'
#'@export
getStructure <- function(filePath){
  getExcel(filePath, funList = list(fix_struc))
}


fix_struc <- function(struc) {
  struc <- struc[, 1:2]
  names(struc) <- sonderzeichen.aufbereiten(names(struc))
  struc$Titel <- gsub("[" , "{[" , struc$Titel , fixed=TRUE)
  struc$Titel <- gsub("]" , "]}" , struc$Titel , fixed=TRUE)

  ## checks
  struc
}


