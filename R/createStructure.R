####
#############################################################################
#' Create structure template.
#'
#' Create structure template based on variable information.
#'
#'@param inputForDescriptives Object (either list or data.frame) containing variable information.
#'
#'@return Structure template.
#'
#'@examples
#'#tbd
#'
#'@export
createStructure <- function(inputForDescriptives){
  UseMethod("createStructure")
}
#'@export
createStructure.data.frame <- function(inputForDescriptives){
  data.frame(Titel = NA, Ebene = NA)
}
#'@export
createStructure.list <- function(inputForDescriptives){
  all_struc <- lapply(varInfo , function(single_varInfo){
    createStructure(single_varInfo)
  })
  all_struc
}
