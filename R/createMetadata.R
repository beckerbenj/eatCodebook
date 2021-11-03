####
#############################################################################
#' Create meta data template.
#'
#' Create meta data template.
#'
#'
#'@return Returns a meta data template.
#'
#'@examples
#'createMetadata
#'
#'@export
createMetadata <- function(){
  data.frame(Title = character(), Author = character(), Keywords = character(), Subject = character())
}
