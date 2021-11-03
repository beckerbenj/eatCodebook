
####
#############################################################################
#' Create meta data snippet.
#'
#' Create meta data latex snippet.
#'
#'@param filePath Path to file.
#'
#'@return Returns a latex snippet.
#'
#'@examples
#'#tbd
#'
#'@export
makeMetadata <- function(filePath){
  varue.meta <- getExcel(filePath)

  for( i in names(varue.meta)){
    if(is.na(varue.meta[,i])) varue.meta[,i] <- ""
    if(is.null(varue.meta[,i])) varue.meta[,i] <- ""

    varue.meta[,i] <- sonderzeichen.aufbereiten(varue.meta[,i])
  }
  cols <- c("Title" , "Author" , "Keywords" , "Subject")
  if(any(! cols %in% names(varue.meta))){
    warning(paste0("Die Spalte/Spalten " , paste0(cols[! cols %in% names(varue.meta)] , collapse=", ") , " ist/sind nicht in der Uebersicht der Metadaten und wird/werden auf \"-\" gesetzt.\n"))
    for(s in cols[! cols %in% names(varue.meta)]){
      varue.meta[,s] <- "-"
    }
  }

  c(paste0("\\Title{",varue.meta$Title,"}"),
    paste0("\\Author{",varue.meta$Author,"}"),
    paste0("\\Keywords{",varue.meta$Keywords,"}"),
    paste0("\\Subject{",varue.meta$Subject,"}"))
}


