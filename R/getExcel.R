
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
#'filePath <- system.file("extdata", "example_abbrList.xlsx", package = "eatCodebook")
#'excel_list   <- getExcel(filePath)
#'
#'@export
getExcel <- function(filePath, funList = NULL){
  check_funList(funList)

  sheet_names <- openxlsx::getSheetNames(filePath)
  names(sheet_names) <- sheet_names

  all_dfs <- lapply(sheet_names, function(sheet_name){
    out <- openxlsx::readWorkbook(xlsxFile = filePath, sheet = sheet_name, startRow = 1)
    if(!is.null(funList)) {
      for(single_fun in funList) {
        #browser()
        out <- single_fun(out)
      }
    }
    out
  })

  if(length(all_dfs) == 1) all_dfs <- all_dfs[[1]]
  all_dfs
}


check_funList <- function(funList) {
  if(!is.null(funList)) {
    if(!is.list(funList)) stop("'funList' needs to be a list (of functions).")
    not_function <- unlist(lapply(funList, function(x) !is.function(x)))
    if(any(not_function)) stop("'funList' needs to be a list (of functions).")
  }
  return()
}
