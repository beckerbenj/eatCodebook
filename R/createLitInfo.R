
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
  if(length(all_short_sources) > 1) all_short_sources <- all_short_sources[all_short_sources != "-"]
  litInfo <- data.frame(Kurzangabe = all_short_sources, Langangabe = NA, in_Literaturverzeichnis = NA)
  litInfo[order(litInfo$Kurzangabe), ]
}
#'@export
createLitInfo.list <- function(varInfo){
  litInfo_list <- lapply(varInfo, createLitInfo)
  litInfo <- do.call(rbind, litInfo_list)
  litInfo_lean <- litInfo[!duplicated(litInfo$Kurzangabe), ]
  litInfo_final <- litInfo_lean[order(litInfo_lean$Kurzangabe), ]

  if(nrow(litInfo_final) > 1) litInfo_final <- litInfo_final[litInfo_final$Kurzangabe != "-", ]
  litInfo_final
}
