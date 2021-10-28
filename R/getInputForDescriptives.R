####
#############################################################################
#' Import input for descriptives information.
#'
#' Import input for descriptives information.
#'
#'@param filePath Path to excel file.
#'
#'@return variable information.
#'
#'@examples
#'#tbd
#'
#'@export
getInputForDescriptives <- function(filePath){
  getExcel(filePath, funList = list(check_inputForDescriptives))
}
