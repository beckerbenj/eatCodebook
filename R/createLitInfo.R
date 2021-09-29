
####
#############################################################################
#' Create literature template.
#'
#' Create literature template.
#'
#'@param varInfo A \code{varInfo} object as imported by \code{getVarInfo}.
#'
#'@return Literature template.
#'
#'@examples
#'#tbd
#'
#'@export
createLitInfo <- function(varInfo = NULL){
  UseMethod("createLitInfo")
}
#'@export
createLitInfo.NULL <- function(varInfo){
  data.frame(Kurzangabe = character(), Langangabe = character(), in_Literaturverzeichnis = character())
}
#'@export
createLitInfo.data.frame <- function(varInfo){
  all_short_sources <- unique(varInfo$QuelleSH)
  data.frame(Kurzangabe = all_short_sources, Langangabe = NA, in_Literaturverzeichnis = NA)
}
#'@export
createLitInfo.list <- function(varInfo){
  litInfo_list <- lapply(varInfo, createLitInfo)
  litInfo <- do.call(rbind, litInfo_list)
  litInfo_lean <- litInfo[!duplicated(litInfo$Kurzangabe), ]
  litInfo_final <- litInfo[order(litInfo_lean$Kurzangabe), ]
}
