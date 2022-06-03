
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
  litInfo <- getExcel(filePath)
  check_litInfo(litInfo)
  litInfo
}


check_litInfo <- function(litInfo) {
  #browser()
  if(!is.data.frame(litInfo)) stop("'litInfo' must be a data.frame.")
  if(!identical(names(litInfo), c("Kurzangabe", "Langangabe", "in_Literaturverzeichnis"))) stop("Column names in 'litInfo' must 'Kurzangabe', 'Langangabe' and 'in_Literaturverzeichnis'.")
  if(any(is.na(litInfo[["in_Literaturverzeichnis"]]))) stop("There are missing entries in 'in_Literaturverzeichnis' in 'litInfo'.")
  if(any(!litInfo[["in_Literaturverzeichnis"]] %in% c("ja", "nein", "yes", "no"))) stop("There are invalid entries in 'in_Literaturverzeichnis' in 'litInfo'. Valid entries are 'yes/ja' or 'nein/no'.")
  NULL
}
