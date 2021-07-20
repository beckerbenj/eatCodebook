
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
createLitinfo <- function(){
  data.frame(Kurzangabe = character(), Langangabe = character(), in_Literaturverzeichnis = character())
}

####
#############################################################################
#' Import register excel.
#'
#' Import register excel.
#'
#'@param filePath Path to excel file.
#'
#'@return Register.
#'
#'@examples
#'#tbd
#'
#'@export
getLitinfo <- function(filePath){
  readWorkbook(xlsxFile = filePath, startRow = 1 )
}
