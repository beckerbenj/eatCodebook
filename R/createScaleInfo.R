####
#############################################################################
#' Create scale information.
#'
#' Create information on scale aggregation.
#'
#'@param inputForDescriptives \code{inputForDescriptives} object as created by the \code{createInputForDescriptives} function
#         source character string of length 1: source information of the data, for example \code{"sfb"}, \code{"lfb"}, etc.
#'
#'@return Returns the scale information.
#'
#'@examples
#'#tbd
#'
#'@export
createScaleInfo <- function(inputForDescriptives){
  UseMethod("createScaleInfo")
}
#'@export
createScaleInfo.data.frame <- function(inputForDescriptives){
#  if (!missing(source)) {
#      if(!is.character(source) || length(source) != 1) {stop("'source' needs to be a character of length 1.")}
#  } else {
  source <- NA
#  }

  inputForDescriptives <- check_inputForDescriptives(inputForDescriptives)
  scales_inputForDescriptives <- inputForDescriptives[which(inputForDescriptives$type == "scale"), ]
  scales <- scales_inputForDescriptives[!duplicated(scales_inputForDescriptives$group), "group"]

  scaleInfo <- data.frame(varName = character(),	Quelle = character(),
                          Anzahl_valider_Werte = character(),	Items_der_Skala = character(), stringsAsFactors = FALSE)

  for(i in seq_along(scales)) {
    single_inputForDescriptives <- inputForDescriptives[inputForDescriptives$group == scales[i], ]
    # special treatment for imputed scales (all of these are marked as scales!)
    if(all(single_inputForDescriptives$imp)) {
      scaleInfo[i, "varName"] <- unique(single_inputForDescriptives$group)
      scaleInfo[i, "Items_der_Skala"] <- paste(single_inputForDescriptives$varName, collapse = ",")
      scaleInfo[i, "Anzahl_valider_Werte"] <- "-"
      scaleInfo[i, "Quelle"] <- source
      next
    }
    scaleInfo[i, "varName"] <- single_inputForDescriptives[single_inputForDescriptives$type == "scale", "varName"]
    scaleInfo[i, "Items_der_Skala"] <- paste(single_inputForDescriptives[single_inputForDescriptives$type == "variable", "varName"], collapse = ",")
    scaleInfo[i, "Anzahl_valider_Werte"] <- "-"
    scaleInfo[i, "Quelle"] <- source
  }
  scaleInfo
}
#'@export
createScaleInfo.list <- function(inputForDescriptives){
  scaleInfo_list <- lapply(inputForDescriptives, function(x) {
    createScaleInfo(x)
  })

  scaleInfo <- eatTools::do_call_rbind_withName(scaleInfo_list, colName = "Quelle2")[, c("varName", "Quelle2", "Anzahl_valider_Werte",
                                                                                         "Items_der_Skala")]
  names(scaleInfo)[2] <- "Quelle"
  scaleInfo
}
