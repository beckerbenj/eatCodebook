
####
#############################################################################
#' Import data from Excel.
#'
#' Import all sheets from an Excel file.
#'
#'@param filePath Path to excel file.
#'
#'@return Either a \code{data.frame} (if the Excel file has a single sheet)
#' or a named \code{list} (if the Excel file has multiple sheets).
#'
#'@examples
#'#tbd
#'
#'@export
getExcel <- function(filePath){
  sheet_names <- openxlsx::getSheetNames(filePath)
  names(sheet_names) <- sheet_names

  all_dfs <- lapply(sheet_names , function(sheet_name){
    openxlsx::readWorkbook(xlsxFile = filePath, sheet = sheet_name, startRow = 1)
  })

  if(length(all_dfs) == 1) all_dfs <- all_dfs[[1]]
  all_dfs
}

