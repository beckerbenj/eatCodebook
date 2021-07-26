
####
#############################################################################
#' Create literature template.
#'
#' Create literature template.
#'
#'
#'@return Literature template.
#'
#'@examples
#'#tbd
#'
#'@export
createLitInfo <- function(){
  data.frame(Kurzangabe = character(), Langangabe = character(), in_Literaturverzeichnis = character())
}

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
