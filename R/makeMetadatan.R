##############################################
### TEXT-ELEMENTE ###
##############################################



makeMetadaten <- function(varue.file , sheet){
  cat(paste0("Skript für Metadaten erstellen.\n"))
  flush.console()
  # Skript für Metadaten
  # YRNTCSH! - Metadaten in der Varue anpassen

  cat(paste0(" Metadaten einlesen.\n"))
  flush.console()
  varue.meta <- readWorkbook (xlsxFile = varue.file , sheet = "Metadaten", startRow = 1 )

  cat(paste0(" Metadaten aufbereiten.\n"))
  flush.console()
  for( i in names(varue.meta)){
    if(is.na(varue.meta[,i])) varue.meta[,i] <- ""
    if(is.null(varue.meta[,i])) varue.meta[,i] <- ""

    varue.meta[,i] <- sonderzeichen.aufbereiten(varue.meta[,i])
  }
  cols <- c("Title" , "Author" , "Keywords" , "Subject")
  if(any(! cols %in% names(varue.meta))){
    warning(paste0("Die Spalte/Spalten " , paste0(cols[! cols %in% names(varue.meta)] , collapse=", ") , " ist/sind nicht in der Übersicht der Metadaten und wird/werden auf \"-\" gesetzt.\n"))
    for(s in cols[! cols %in% names(varue.meta)]){
      varue.meta[,s] <- "-"
    }
  }

  skript <- c( paste0("\\Title{",varue.meta$Title,"}"),
               paste0("\\Author{",varue.meta$Author,"}"),
               paste0("\\Keywords{",varue.meta$Keywords,"}"),
               paste0("\\Subject{",varue.meta$Subject,"}"))

  return(skript)
}


