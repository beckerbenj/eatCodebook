get.register <- function(varue.file , sheets , fbshort, varue.info){
  cat(paste0("ERSTELLE REGISTER.\n"))
  flush.console()

  # Varue - Register
  varue.reg <- lapply( sheets , function(d) readWorkbook ( xlsxFile = varue.file , sheet = d, startRow = 1 ) )
  names(varue.reg) <- fbshort

  varue.reg <- lapply(fbshort , function(d) varue.register.aufbereiten(varue.reg[[d]] , varue.info[[d]]))
  names(varue.reg) <- fbshort

  return(varue.reg)
}

#Register - Reduzierung
varue.register.aufbereiten <- function(varue.reg , varue.info){
  varue.reg.aufb <- as.data.frame(varue.reg[ varue.reg$Var.Name %in% varue.info$Var.Name[ varue.info$in.DS.und.SH %in% c("ja", "sh") ] ,  ] , stringsAsFactors=FALSE)
  names(varue.reg.aufb) <- names(varue.reg)

  if(any(grepl("\\s" , varue.reg.aufb$Var.Name))){
    cat(paste0("  Die Variable/Variablen ", varue.reg.aufb$Var.Name[grepl("\\s" , varue.reg.aufb$Var.Name)] ," besitzt/besitzen Leerzeichen im Variablennamen. Diese werden entfernt.\n"))
    flush.console()
    varue.reg.aufb$Var.Name <- gsub("\\s" , "" , varue.reg.aufb$Var.Name)
  }

  # keine Schlagwörter
  if(dim(varue.reg.aufb)[2]==1){
    cat(paste0("  Es liegen keine Schlagwörter vor. Es wird kein Register erstellt.\n"))
    flush.console()
    varue.reg.aufb <- data.frame("Var.Name"=c("") , stringsAsFactors=FALSE)
  } else if(all(! sapply(names(varue.reg.aufb) , function(d) any(varue.reg.aufb[[d]] %in% "x"), USE.NAMES=FALSE ) ) ) {
    cat(paste0("  Zu keinem Schlagwort wurden Variablen zugeordnet. Es wird kein Register erstellt.\n"))
    flush.console()
    varue.reg.aufb <- data.frame("Var.Name"=c("") , stringsAsFactors=FALSE)
  } else {
    #Register - Sortierung der Variablennamen
    cat(paste0("  Schlagwörter-Variablen-Zuordnung liegt vor.\n"))
    flush.console()
    cat(paste0("   Variablen werden nach Gliederung und Reihenfolge aus der Variableninformation sortiert.\n"))
    flush.console()
    varue.reg.aufb <- varue.reg.aufb[ order( sapply( varue.reg.aufb$Var.Name, function(d) which( varue.info$Var.Name[ varue.info$Var.Name %in% varue.reg.aufb$Var.Name ] %in% d ) ) ), ]

    cat(paste0("   Sonderzeichen in den Schlagwörtern bearbeiten.\n"))
    flush.console()
    names(varue.reg.aufb) <- sonderzeichen.aufbereiten(names(varue.reg.aufb))

  }
  return(varue.reg.aufb)
}
