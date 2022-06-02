
####
#############################################################################
#' Create chapter template.
#'
#' Create chapter template based on variable information. The order of the entries determines the order of the documentation in the codebook.
#'
#'@param varInfo Object (either \code{list} or \code{data.frame}) containing variable information.
#'
#'@return Chapter template.
#'
#'@examples
#'#tbd
#'
#'@export
createChapters <- function(varInfo){
  UseMethod("createChapters")
}
#'@export
createChapters.data.frame <- function(varInfo){
  data.frame(dataName = "dat",
             chapterName = NA)
}

#'@export
createChapters.list <- function(varInfo){
  data.frame(dataName = names(varInfo),
             chapterName = NA)
}

