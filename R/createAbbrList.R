####
#############################################################################
#' Create abbreviation list template.
#'
#' Create abbreviation list template.
#'
#'@param headings Headings.
#'
#'@return Returns a latex snippet.
#'
#'@examples
#'#tbd
#'
#'@export
createAbbrList <- function(headings=list("Akronyme"=c( "Abkuerzung", "Bedeutung"),
                                         "Statistische Formelzeichen"=c( "Symbol", "Bedeutung"))){
  lapply(headings, function(x) {
    out <- data.frame()
    for(i in x){
      out[, i] <- character()
    }
    out
  })
}
