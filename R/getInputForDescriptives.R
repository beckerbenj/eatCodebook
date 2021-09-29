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
  sheet_names <- openxlsx::getSheetNames(filePath)
  names(sheet_names) <- sheet_names

  all_input <- lapply(sheet_names , function(sheet_name){
    input <- openxlsx::readWorkbook(xlsxFile = filePath, sheet = sheet_name, startRow = 1)
    check_inputForDescriptives(input)
    input
  })

  all_input
}
