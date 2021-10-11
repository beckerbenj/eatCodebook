
####
#############################################################################
#' Create register template.
#'
#' Create register template based on variable information.
#'
#'@param inputForDescriptives Object (either \code{list} or \code{data.frame}) containing variable information.
#'@param keywordList Character vector of keyword columns to be added. If \code{NULL}, no additional columns are added.
#'If \code{inputForDescriptives} is a \code{list}, \code{keywordList} must also be a \code{list} (of character vectors).
#'
#'@return Register template.
#'
#'@examples
#'#tbd
#'
#'@export
createRegister <- function(inputForDescriptives, keywordList = NULL){
  UseMethod("createRegister")
}
#'@export
createRegister.data.frame <- function(inputForDescriptives, keywordList = NULL){
  #browser()

  inputForDescriptives[, "Nr"] <- NA
  register <- unique(inputForDescriptives[, c("Nr", "group")])
  names(register)[2] <- "varName"
  register[, "Nr"] <- seq(nrow(register))

  for(i in keywordList) {
    register[, i] <- NA
  }

  rownames(register) <- NULL
  register
}

#'@export
createRegister.list <- function(inputForDescriptives, keywordList = NULL){
  #browser()
  if(is.null(keywordList)) keywordList <- lapply(seq_along(inputForDescriptives), function(x) NULL)

  if(!is.list(keywordList)) stop("If 'inputForDescriptives' is a list, 'keywordList' must be a list, too.")
  if(length(inputForDescriptives) != length(keywordList)) stop("'inputForDescriptives' and 'keywordList' lists must be of identical length.")

  all_reg <- Map(createRegister, inputForDescriptives = inputForDescriptives, keywordList = keywordList)

  all_reg
}

## WIe Unterkapitel integrieren?
