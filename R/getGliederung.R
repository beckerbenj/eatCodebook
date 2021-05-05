get.gliederung <- function(varue.file , sheets){
  # Varue - Gliederung
  varue.gliederung <- lapply( sheets , function(d) readWorkbook ( xlsxFile = varue.file , sheet = d, startRow = 1 ) )
  varue.gliederung <- lapply( 1:length(fbshort) , function(d) varue.gliederung[[d]] [, c(1,2)] )
  names(varue.gliederung) <- fbshort

  varue.gliederung <- lapply(varue.gliederung , varue.gliederung.aufbereiten )
  names(varue.gliederung) <- fbshort

  return(varue.gliederung)
}

varue.gliederung.aufbereiten <- function(varue.gliederung){
  varue.gliederung$Titel <- sonderzeichen.aufbereiten(varue.gliederung$Titel)

  cat(paste0("  Sonderzeichen in den Titeln bearbeiten.\n"))
  flush.console()
  varue.gliederung$Titel <- gsub("[" , "{[" , varue.gliederung$Titel , fixed=TRUE)
  varue.gliederung$Titel <- gsub("]" , "]}" , varue.gliederung$Titel , fixed=TRUE)


  return(varue.gliederung)
}

