####
#############################################################################
#' Create structure template.
#'
#' Create structure template based on variable information.
#'
#' To create the structure template, information from the columns \code{'Unterteilung.im.Skalenhandbuch'} and \code{'Gliederung'} are used.
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
  check_varInfo(varInfo)

  struc <- unique(varInfo[, c("Unterteilung.im.Skalenhandbuch", "Gliederung")])

  #if(length(unique(struc$Unterteilung.im.Skalenhandbuch)) != length(unique(struc$Gliederung))) browser()

  by(struc, struc$Unterteilung.im.Skalenhandbuch, function(sub_struc) {
    sub_gliederung <- unique(sub_struc$Gliederung)
    sub_unterteilung <- unique(sub_struc$Unterteilung.im.Skalenhandbuch)
    if(length(sub_gliederung) != 1) stop("For 'Unterteilung.im.Skalenhandbuch' ", sub_unterteilung, " there are different entries in 'Gliederung': ", paste(sub_gliederung, collapse = ", "))
  })
  by(struc, struc$Gliederung, function(sub_struc) {
    sub_gliederung <- unique(sub_struc$Gliederung)
    sub_unterteilung <- unique(sub_struc$Unterteilung.im.Skalenhandbuch)
    if(length(sub_unterteilung) != 1) stop("For 'Gliederung' ", sub_gliederung, " there are different entries in 'Unterteilung.im.Skalenhandbuch': ", paste(sub_unterteilung, collapse = ", "))
  })

  gliederung_splitted <- eatTools::halveString(struc$Gliederung, "\\.")
  ho_chapter <- as.numeric(gliederung_splitted[, 1])
  struc2 <- cbind(struc, chapter = ho_chapter)

  struc_list <- by(struc2, struc2$chapter, function(struc_single) {
    data.frame(Titel = c(NA, struc_single$Unterteilung.im.Skalenhandbuch),
               Ebene = c(unique(struc_single$chapter), struc_single$Gliederung))
  })

  do.call(rbind, struc_list)
}
#'@export
createStructure.list <- function(varInfo){
  all_struc <- lapply(varInfo, function(single_varInfo){
    createStructure(single_varInfo)
  })
  all_struc
}
