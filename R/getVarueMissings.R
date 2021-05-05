get.varue.missings <- function(varue.file , sheets, fbshort , miss.cols=c( "Var.name" , "Wert" , "missing" , "LabelSH", "Zeilenumbruch.vor.Wert") , varue.info , Gesamtdatensatz){
  cat(paste0("Einlesen der Werteinfos.\n"))
  flush.console()

  names(sheets) <- fbshort

  varue.missings <- lapply( fbshort , function(d) {
    cat(paste0(" ",d, "..."))
    flush.console()
    x <- readWorkbook ( xlsxFile = varue.file , sheet = sheets[d], startRow = 1 )
    cat(paste0("done!\n"))
    flush.console()
    return(x)
  })
  cat("\n")
  flush.console()
  varue.missings <- lapply( 1:length(fbshort) , function(d) varue.missings[[d]][, miss.cols] )
  names(varue.missings) <- fbshort

  varue.missings <- lapply(fbshort , function(d) varue.missings.aufbereiten(varue.info[[d]] , varue.missings[[d]] , Gesamtdatensatz[[d]] , d) )
  names(varue.missings) <- fbshort

  return(varue.missings)
}

varue.missings.aufbereiten <- function(varue.info , varue.missings , Gesamtdatensatz, fbshort){
  cat(paste0(" Aufbereitung der Werteinfos: ", fbshort, ".\n"))
  flush.console()
  varue.missings.aufb <- varue.missings

  if(any(grepl("\\s" , varue.missings.aufb$Var.name))){
    cat(paste0("  Die Variable/Variablen ", varue.missings.aufb$Var.name[grepl("\\s" , varue.missings.aufb$Var.name)] ," besitzt/besitzen Leerzeichen im Variablennamen. Diese werden entfernt.\n"))
    flush.console()
    varue.missings.aufb$Var.name <- gsub("\\s" , "" , varue.missings.aufb$Var.name)
  }

  # Werte im Datensatz - Grundlage für Häufigkeiten und Fallzahlen
  cat(paste0("  Check: Match zwischen Werteinfo und Datensatz.\n"))
  flush.console()
  for(name in unique(varue.missings$Var.name)){
    if( ! name %in% names(Gesamtdatensatz) & ! varue.info$in.DS.und.SH[varue.info$Var.Name %in% name] %in% c("nein", "sh") ){
      cat(paste0("   Die Variable " , name , " ist in der Werteinfo und soll ins Skalenhandbuch, aber existiert nicht im Datensatz.\n"))
      flush.console()
    } else if(varue.info$Layout[varue.info$Var.Name %in% name] %in% c(2,3) & ! varue.info$in.DS.und.SH[varue.info$Var.Name %in% name] %in% "nein"){
      varue.missings.aktuell <- varue.missings[ varue.missings$Var.name %in% name ,]
      if(! all(unique(Gesamtdatensatz[ ! is.na(Gesamtdatensatz[ , name ]),name]) %in% varue.missings.aktuell$Wert)){
        w <- unique(Gesamtdatensatz[ ! is.na(Gesamtdatensatz[ , name ]),name])[which(! unique(Gesamtdatensatz[ ! is.na(Gesamtdatensatz[ , name ]),name]) %in% varue.missings.aktuell$Wert)]
        w <- sort(w)
        cat(paste0("   Für die Variable " , name , " treten im Datensatz Werte auf, die nicht definiert sind (d.h. nicht in der Varue stehen). Die betroffenen Werte (" , paste0(w , collapse=", ") , ") werden ohne Label als nicht-Missing mit aufgenommen.\n"))
        flush.console()
        #warning(paste0("Für die Variable " , name , " treten im Datensatz Werte auf, die nicht definiert sind (d.h. nicht in der Varue stehen). Die betroffenen Werte sind: " , paste0(w , collapse=", ") , ".\n"))
        n <- length(w)
        varue.missings.aufb <- rbind(varue.missings.aufb , data.frame("Var.name"=rep(name, n) , "Wert"=w , "missing"=rep("nein",n) , "LabelSH"=rep("",n) , "Zeilenumbruch.vor.Wert"=rep("nein",n) , stringsAsFactors=FALSE))
      }
    }
  }


  cat(paste0("  Sortierung der Werte.\n"))
  flush.console()
  # Werteinfos
  varue.missings.aufb$Var.name <- as.character(varue.missings.aufb$Var.name)
  varue.missings.aufb$Wert <- as.character(varue.missings.aufb$Wert)
  varue.missings.aufb$missing <- as.character(varue.missings.aufb$missing)
  varue.missings.aufb$LabelSH <- as.character(varue.missings.aufb$LabelSH)

  var.num <- unique( varue.missings.aufb$Var.name)
  for(j in var.num){
    if(any(grepl(",",varue.missings.aufb[varue.missings.aufb$Var.name %in% j, "Wert" ]))){
      varue.missings.aufb[varue.missings.aufb$Var.name %in% j, "Wert" ] <- sub("(\\d*)(,)(\\d*)","\\1\\.\\3",varue.missings.aufb[varue.missings.aufb$Var.name %in% j, "Wert" ], fixed=FALSE)
    }
    if(suppressWarnings(any(is.na( as.numeric(varue.missings.aufb$Wert[varue.missings.aufb$Var.name %in% j ] ))))){
      varue.missings.aufb[varue.missings.aufb$Var.name %in% j, ] <- rbind(varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "nein", ][order(gsub(",",".",varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "nein", "Wert"] , fixed=TRUE)) , ] , varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "ja", ][order(gsub(",",".",varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "ja", "Wert"] , fixed=TRUE)) , ])
    } else {
      varue.missings.aufb[varue.missings.aufb$Var.name %in% j, ] <- rbind(varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "nein", ][order(as.numeric(varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "nein", "Wert"])) , ] , varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "ja", ][order(abs(as.numeric(varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "ja", "Wert"] ))) , ])
    }
  }

  cat(paste0("  Sonderzeichen bearbeiten.\n"))
  flush.console()
  varue.missings.aufb$LabelSH <- sonderzeichen.aufbereiten(varue.missings.aufb$LabelSH)

  cat("\n")
  flush.console()

  return(varue.missings.aufb)
}
