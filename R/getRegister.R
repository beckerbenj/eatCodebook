
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
getRegister <- function(filePath){
  #browser()
  sheet_names <- openxlsx::getSheetNames(filePath)
  names(sheet_names) <- sheet_names

  all_reg <- lapply(sheet_names , function(sheet_name){
    single_reg <- openxlsx::readWorkbook(xlsxFile = filePath , sheet = sheet_name, startRow = 1)
    names(single_reg) <- sonderzeichen.aufbereiten(names(single_reg))

    #varue.reg.aufb <- varue.reg.aufb[ order( sapply( varue.reg.aufb$Var.Name, function(d) which( varue.info$Var.Name[ varue.info$Var.Name %in% varue.reg.aufb$Var.Name ] %in% d ) ) ), ]

    ## checks
    if(ncol(single_reg) <= 3) stop("Keywords are missing.")

    for(keyword in names(single_reg)[4:ncol(single_reg)]){
      single_col <- single_reg[!is.na(single_reg[, keyword]), keyword]
      if(any(single_col != "x")) stop("Other entry than 'x' or NA in column '", keyword, "' in sheet '", sheet_name, "'.")
      if(!any(single_col == "x")) stop("Keyword '", keyword, "' is not assigned in sheet '", sheet_name, "'.")
    }
    #if(!any(single_reg[!is.na(single_reg)] == "x")) stop("No keywords assigned.")

    sorted_keywords <- sort(names(single_reg)[-(1:3)])
    single_reg[, c(names(single_reg)[1:3], sorted_keywords)]
  })

  all_reg
}

## Remarks/ideas:
# -----------------------------------
# fit-checks vs. other variable information in separate function (maybe when it all comes together)
# sorting automatically during creation by sorting in data set

