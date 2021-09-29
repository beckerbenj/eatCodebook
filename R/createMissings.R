

####
#############################################################################
#' Create missing value information.
#'
#' Create information on value level (value labels and missing codes).
#'
#'@param GADSdat A \code{GADSdat} object.
#'
#'@return Returns the value information.
#'
#'@examples
#'#tbd
#'
#'@export
createMissings <- function(GADSdat){
  UseMethod("createMissings")
}

#'@export
createMissings.GADSdat <- function(GADSdat){
  all_meta <- eatGADS::extractMeta(GADSdat)
  missings <- all_meta[!is.na(all_meta$value), c("varName", "value", "missings", "valLabel")]

  if(any(is.na(missings$missings))) stop("Missings in column 'missings'.")
  missings$missings <- ifelse(missings$missings == "miss", yes = "ja", no = "nein")
  names(missings) <- c("Var.name", "Wert", "missing", "LabelSH")
  missings[, "Zeilenumbruch_vor_Wert"] <- "nein"

  missings
}
#'@export
createMissings.list <- function(GADSdat){
  lapply(GADSdat, function(x) {
    createMissings(x)
  })
}
