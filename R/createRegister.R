
####
#############################################################################
#' Create register template.
#'
#' Create register template based on variable information.
#'
#'@param varinfo Object (either list or data.frame) containing variable information.
#'
#'@return Register template.
#'
#'@examples
#'tbd
#'
#'@export
createRegister <- function(varinfo, keywordList){
  UseMethod("createRegister")
}
#'@export
createRegister.list <- function(varinfo, keywordList){
  #browser()

  all_reg <- lapply(varinfo , function(single_varinfo){
    createRegister(single_varinfo, keywordList)
  })

  all_reg
}
#'@export
createRegister.data.frame <- function(varinfo, keywordList){
  #browser()

  varinfo[, "Nr"] <- NA
  register <- unique(varinfo[, c("Nr", "group")])
  names(register)[2] <- "varName"
  register[, "Nr"] <- seq(nrow(register))

  for(i in keywordList) {
    register[, i] <- NA
  }

  rownames(register) <- NULL
  register
}

## WIe Unterkapitel integrieren?
