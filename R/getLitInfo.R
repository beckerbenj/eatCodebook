
####
#############################################################################
#' Import literature information.
#'
#' Import literature information.
#'
#'@param filePath Path to excel file.
#'
#'@return Literature information.
#'
#'@examples
#'#tbd
#'
#'@export
getLitInfo <- function(filePath){
  openxlsx::readWorkbook(xlsxFile = filePath, startRow = 1 )
}
