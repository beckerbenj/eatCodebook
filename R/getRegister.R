
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
#'tbd
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
    if(!any(single_reg[!is.na(single_reg)] == "x")) stop("No keywords assigned.")

    single_reg
  })

  all_reg
}

## Remarks/ideas:
# -----------------------------------
# fit-checks vs. other variable information in separate function (maybe when it all comes together)
# sorting automatically during creation by sorting in data set

