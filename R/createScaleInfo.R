####
#############################################################################
#' Create scale information.
#'
#' Create information on scale aggregation.
#'
#'@param varInfo \code{varInfo} object.
#'
#'@return Returns the scale information.
#'
#'@examples
#'#tbd
#'
#'@export
createScaleInfo <- function(varInfo){
  UseMethod("createScaleInfo")
}
#'@export
createScaleInfo.data.frame <- function(varInfo){
  #if(!is.character(source) || length(source) != 1) stop("'source' needs to be a character of length 1.")

  scales <- varInfo[varInfo$type == "scale", "group"]

  scaleInfo <- data.frame(varName = character(),	Quelle = character(),
                          Anzahl_valider_Werte = character(),	Items_der_Skala = character())

  for(i in seq_along(scales)) {
    single_varInfo <- varInfo[varInfo$group == scales[i], ]
    scaleInfo[i, "varName"] <- single_varInfo[single_varInfo$type == "scale", "varName"]
    scaleInfo[i, "Items_der_Skala"] <- paste(single_varInfo[single_varInfo$type == "variable", "varName"], collapse = ",")
    scaleInfo[i, "Anzahl_valider_Werte"] <- "-"
    scaleInfo[i, "Quelle"] <- NA
  }
  scaleInfo
}
#'@export
createScaleInfo.list <- function(varInfo){
  scaleInfo_list <- lapply(varInfo, function(x) {
    createScaleInfo(x)
  })

  scaleInfo <- eatTools::do_call_rbind_withName(scaleInfo_list, colName = "Quelle2")[, c("varName", "Quelle2", "Anzahl_valider_Werte",
                                                                                         "Items_der_Skala")]
  names(scaleInfo)[2] <- "Quelle"
  scaleInfo
}
