####
#############################################################################
#' Create scale information.
#'
#' Create information on scale or imputation aggregation.
#' Scales, 'fake scales', and imputed variables are listed.
#'
#' Currently, displaying the items of a imputed scale is not supported by the \code{codebook()} function.
#' Even though the items of an imputed scale can be defined in the \code{scaleInfo},
#' this information is simply ignored during the creation of the codebook.
#'
#'@param inputForDescriptives \code{inputForDescriptives} object as created by the \code{createInputForDescriptives} function
#'
#'@return Returns a \code{data.frame} with the following information:
#'\itemize{
#'  \item \code{varName} The name of the variable as it occurs in the data
#'  \item \code{Quelle} The name of the data set the variable belongs to.
#'  This is only meaningful, if \code{inputForDescriptives} is a list
#'  (meaning that the function runs for multiple data sets).
#'  \item \code{Anzahl_valider_Werte} For a scale, how many values have to be not NA on items for a
#'  non NA value on the scale?
#'  \item \code{Items_der_Skala} Which items or imputations belong to the scale?
#'  This column is used to display the item names in the codebook.
#'  \item \code{Imputationen} Which imputations belong to the pooled variable?
#'  This column is used to display the number of imputations in the codebook.
#'}
#'
#'@examples
#'# import spss exemplary data
#'file <- system.file("extdata", "example1_clean.sav", package = "eatCodebook")
#'dat   <- eatGADS::import_spss(file)
#'inputForDescriptives <- createInputForDescriptives(dat, impExpr = "plausible value")
#'
#'scaleInfo <- createScaleInfo(inputForDescriptives)
#'
#'@export
createScaleInfo <- function(inputForDescriptives){
  UseMethod("createScaleInfo")
}
#'@export
createScaleInfo.data.frame <- function(inputForDescriptives){
  inputForDescriptives <- check_inputForDescriptives(inputForDescriptives)

  source <- NA
  scaleInfo <- data.frame(varName = character(),	Quelle = character(),
                          Anzahl_valider_Werte = character(),
                          Items_der_Skala = character(), Imputationen = character(),
                          stringsAsFactors = FALSE)

  ## Scales
  scales_inputForDescriptives <- inputForDescriptives[which(inputForDescriptives$type %in% c("scale", "fake_item")), ]
  scales <- scales_inputForDescriptives[!duplicated(scales_inputForDescriptives$group), "group"]
  ## Inputed
  imputed_inputForDescriptives <- inputForDescriptives[which(inputForDescriptives$imp), ]
  pooled_variables <- unique(imputed_inputForDescriptives$group)

  scales_and_imputed <- unique(c(scales, pooled_variables))

  for(i in seq_along(scales_and_imputed)) {
    single_inputForDescriptives <- inputForDescriptives[inputForDescriptives$group == scales_and_imputed[i], ]
    scaleInfo[i, "varName"] <- unique(single_inputForDescriptives$group)
    scaleInfo[i, "Anzahl_valider_Werte"] <- "-"
    scaleInfo[i, "Quelle"] <- source

    # scales
    items <- single_inputForDescriptives[single_inputForDescriptives$type %in% c("item", "fake_item"), "varName"]
    scaleInfo[i, "Items_der_Skala"] <- paste(items, collapse = ",")

    #browser()
    # imputations
    imputations <- single_inputForDescriptives[single_inputForDescriptives$imp, "varName"]
    scaleInfo[i, "Imputationen"] <- paste(imputations, collapse = ",")
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

