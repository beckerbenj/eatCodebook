####
#############################################################################
#' Create scale information.
#'
#' Create information on scale aggregation. Scales, 'fake scales', and imputed variables are listed.
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

  scaleInfo <- data.frame(varName = character(),	Quelle = character(),
                          Anzahl_valider_Werte = character(),	Items_der_Skala = character(), stringsAsFactors = FALSE)

  ## Scales
  #browser()
  inputForDescriptives <- check_inputForDescriptives(inputForDescriptives)
  scales_inputForDescriptives <- inputForDescriptives[which(inputForDescriptives$type %in% c("scale", "fake_item")), ]
  scales <- scales_inputForDescriptives[!duplicated(scales_inputForDescriptives$group), "group"]

  ## Inputed
  imputed_inputForDescriptives <- inputForDescriptives[which(inputForDescriptives$imp), ]
  pooled_variables <- unique(imputed_inputForDescriptives$group)

  scales_and_imputed <- unique(c(scales, pooled_variables))

  for(i in seq_along(scales_and_imputed)) {
    single_inputForDescriptives <- inputForDescriptives[inputForDescriptives$group == scales_and_imputed[i], ]
    # special treatment for imputed scales
    if(all(single_inputForDescriptives$imp) && any(single_inputForDescriptives$type == "scale")) {
      #browser()
      scaleInfo[i, "varName"] <- unique(single_inputForDescriptives$group)
      scaleInfo[i, "Items_der_Skala"] <- paste(single_inputForDescriptives$varName, collapse = ",")
      scaleInfo[i, "Anzahl_valider_Werte"] <- "-"
      scaleInfo[i, "Quelle"] <- source
      next
    }
    scaleInfo[i, "varName"] <- unique(single_inputForDescriptives$group)
    items <- single_inputForDescriptives[single_inputForDescriptives$type %in% c("item", "fake_item"), "varName"]
    scaleInfo[i, "Items_der_Skala"] <- paste(items, collapse = ",")
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

  scaleInfo <- eatTools::do_call_rbind_withName(scaleInfo_list, colName = "Quelle2")[, c("varName", "Quelle2",
                                                                                         "Anzahl_valider_Werte",
                                                                                         "Items_der_Skala")]
  names(scaleInfo)[2] <- "Quelle"
  scaleInfo
}

