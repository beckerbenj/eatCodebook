
####
#############################################################################
#' Create register template.
#'
#' Create register template based on variable information.
#'
#'@param inputForDescriptives Object (either list or data.frame) containing variable information.
#'@param keywordList Character vector of keyword columns to be added.
#'
#'@return Register template.
#'
#'@examples
#'#tbd
#'
#'@export
createRegister <- function(inputForDescriptives, keywordList){
  UseMethod("createRegister")
}
#'@export
createRegister.data.frame <- function(inputForDescriptives, keywordList){
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
createRegister.list <- function(inputForDescriptives, keywordList){
  #browser()

  all_reg <- lapply(inputForDescriptives, function(single_inputForDescriptives){
    createRegister(single_inputForDescriptives, keywordList)
  })

  all_reg
}

## WIe Unterkapitel integrieren?
