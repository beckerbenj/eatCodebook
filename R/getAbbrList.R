
####
#############################################################################
#' Import abbreviation list.
#'
#' Import abbreviation list.
#'
#'@param filePath Path to excel file.
#'
#'@return Abbreviation list.
#'
#'@examples
#'#tbd
#'
#'@export
getAbbrList <- function(filePath){
  stop("'getAbbrList' is deprecated, use 'makeAbbrList' instead.")
  getExcel(filePath)
}
