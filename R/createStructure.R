####
#############################################################################
#' Create structure template.
#'
#' Create structure template based on variable information.
#'
#'@param varInfo Object (either list or data.frame) containing variable information.
#'
#'@return Structure template.
#'
#'@examples
#'#tbd
#'
#'@export
createStructure <- function(varInfo = NULL){
  UseMethod("createStructure")
}
#'@export
createStructure.NULL <- function(varInfo){
  data.frame(Titel = NA, Ebene = NA)
}
#'@export
createStructure.data.frame <- function(varInfo){
  all_levels <- sort(unique(varInfo$Gliederung))
  data.frame(Titel = NA, Ebene = all_levels)
}
#'@export
createStructure.list <- function(varInfo){
  all_struc <- lapply(varInfo, function(single_varInfo){
    createStructure(single_varInfo)
  })
  all_struc
}
