

####
#############################################################################
#' Create missing value information.
#'
#' Create information on value level (value labels and missing codes).
#'
#'@param GADSdat A \code{GADSdat} object.
#'@param inputForDescriptives Input for descriptive statistics calculation.
#'
#'@return Returns the value information.
#'
#'@examples
#'#tbd
#'
#'@export
createMissings <- function(GADSdat, inputForDescriptives){
  UseMethod("createMissings")
}

#'@export
createMissings.GADSdat <- function(GADSdat, inputForDescriptives){
  inputForDescriptives <- check_inputForDescriptives(inputForDescriptives)

  all_meta <- eatGADS::extractMeta(GADSdat)
  missings <- all_meta[!is.na(all_meta$value), c("varName", "value", "missings", "valLabel")]

  if(any(is.na(missings$missings))) stop("Missings in column 'missings'.")
  missings$missings <- ifelse(missings$missings == "miss", yes = "ja", no = "nein")
  names(missings) <- c("Var.name", "Wert", "missing", "LabelSH")
  missings[, "Zeilenumbruch_vor_Wert"] <- "nein"

  ## imputed variables
  inputed_info <- inputForDescriptives[inputForDescriptives$imp == TRUE, ]
  pooled_variables <- unique(inputed_info[["group"]])
  for(i in pooled_variables) {
    #browser()
    single_inputed_info <- inputed_info[inputed_info$group == i, ]
    first_entry <- single_inputed_info[1, "varName"]

    newRows <- missings[missings$Var.name == first_entry, ]
    newRows[, "Var.name"] <- i

    #browser()
    missings <- insertRows(missings, newRows = newRows, index = max(which(missings$Var.name == first_entry)))
  }

  missings
}
#'@export
createMissings.list <- function(GADSdat, inputForDescriptives){
  lapply(GADSdat, function(x) {
    createMissings(x)
  })
}
