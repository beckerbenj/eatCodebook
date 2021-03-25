
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

get.varue.info <- function(varue.file , sheets, fbshort , varue.cols=c("Var.Name" , "in.DS.und.SH" , "Layout" , "LabelSH" , "Anmerkung.Var" , "Gliederung" , "Reihenfolge" , "Titel" , "rekodiert","QuelleSH" , "Instruktionen" , "Hintergrundmodell", "HGM.Variable.erstellt.aus", "HGM.Reihenfolge", "intern.extern", "Seitenumbruch.im.Inhaltsverzeichnis" )){
  cat(paste0("Einlesen der Variableninfos.\n"))
  flush.console()
  varue.info <- lapply( sheets , function(d) readWorkbook ( xlsxFile = varue.file , sheet = d, startRow = 1 ) )
  varue.info <- lapply( 1:length(fbshort) , function(d) varue.info[[d]][, varue.cols] )
  names(varue.info) <- fbshort

  varue.info <- lapply(varue.info , varue.info.aufbereiten)
  names(varue.info) <- fbshort

  return(varue.info)
}

varue.info.aufbereiten <- function(varue.info , col.sonderzeichen=c("LabelSH" , "Titel" , "QuelleSH" , "Anmerkung.Var")){
  cat(paste0(" Aufbereitung der Variableninformationen.\n"))
  flush.console()
  # Variableninfromationen - zu character-Strings
  varue.info$Var.Name <- as.character(varue.info$Var.Name)
  varue.info$in.DS.und.SH <- as.character(varue.info$in.DS.und.SH)
  varue.info$Layout <- as.character(varue.info$Layout)
  varue.info$LabelSH <- as.character(varue.info$LabelSH)

  if(any(grepl("\\s" , varue.info$Var.Name))){
    cat(paste0("  Die Variable/Variablen ", varue.info$Var.Name[grepl("\\s" , varue.info$Var.Name)] ," besitzt/besitzen Leerzeichen im Variablennamen. Diese werden entfernt.\n"))
    flush.console()
    varue.info$Var.Name <- gsub("\\s" , "" , varue.info$Var.Name)
  }

  cat(paste0("  Aufbereitung der Titel, Anmerkungen und Quellen.\n"))
  flush.console()
  # Variablentitel bearbeiten
  varue.info$Titel <- sub( "^\\s*(.*)\\s*$" , "\\1" , varue.info$Titel )
  varue.info$Titel[which(toupper(varue.info$Titel) %in% "NA")] <- "-"
  varue.info$Titel[which(toupper(varue.info$Titel) %in% "NULL" )] <- "-"
  varue.info$Titel[which(varue.info$Titel %in% "")] <- "-"
  varue.info$Titel[which(is.na(varue.info$Titel))] <- "-"
  varue.info$Titel[which(is.null(varue.info$Titel))] <- "-"

  # Variablenanmerkungen bearbeiten
  varue.info$Anmerkung.Var <- sub( "^\\s*(.*)\\s*$" , "\\1" , varue.info$Anmerkung.Var )
  varue.info$Anmerkung.Var[which(toupper(varue.info$Anmerkung.Var) %in% "NA")] <- "-"
  varue.info$Anmerkung.Var[which(toupper(varue.info$Anmerkung.Var) %in% "NULL" )] <- "-"
  varue.info$Anmerkung.Var[which(varue.info$Anmerkung.Var %in% "")] <- "-"
  varue.info$Anmerkung.Var[which(is.na(varue.info$Anmerkung.Var))] <- "-"
  varue.info$Anmerkung.Var[which(is.null(varue.info$Anmerkung.Var))] <- "-"

  # Variablentitel bearbeiten
  varue.info$QuelleSH <- sub( "^\\s*(.*)\\s*$" , "\\1" , varue.info$QuelleSH )
  varue.info$QuelleSH[which(toupper(varue.info$QuelleSH) %in% "NA")] <- "-"
  varue.info$QuelleSH[which(toupper(varue.info$QuelleSH) %in% "NULL" )] <- "-"
  varue.info$QuelleSH[which(varue.info$QuelleSH %in% "")] <- "-"
  varue.info$QuelleSH[which(is.na(varue.info$QuelleSH))] <- "-"
  varue.info$QuelleSH[which(is.null(varue.info$QuelleSH))] <- "-"

  # Reihenfolge bearbeiten
  varue.info$Reihenfolge <- sub( "^\\s*(.*)\\s*$" , "\\1" , varue.info$Reihenfolge )
  varue.info$Reihenfolge[which(toupper(varue.info$Reihenfolge) %in% "NA")] <- 0
  varue.info$Reihenfolge[which(toupper(varue.info$Reihenfolge) %in% "NULL" )] <- 0
  varue.info$Reihenfolge[which(varue.info$Reihenfolge %in% "")] <- 0
  varue.info$Reihenfolge[which(is.na(varue.info$Reihenfolge))] <- 0
  varue.info$Reihenfolge[which(is.null(varue.info$Reihenfolge))] <- 0

  # Instruktionen aufbereiten
  varue.info$Instruktionen <- sub( "^\\s*(.*)\\s*$" , "\\1" , varue.info$Instruktionen )
  varue.info$Instruktionen[which(toupper(varue.info$Instruktionen) %in% "NA")] <- "-"
  varue.info$Instruktionen[which(toupper(varue.info$Instruktionen) %in% "NULL" )] <- "-"
  varue.info$Instruktionen[which(varue.info$Instruktionen %in% "")] <- "-"
  varue.info$Instruktionen[which(is.na(varue.info$Instruktionen))] <- "-"
  varue.info$Instruktionen[which(is.null(varue.info$Instruktionen))] <- "-"
  varue.info$Instruktionen <- gsub("/" , "\\slash " , varue.info$Instruktionen , fixed=TRUE)


  # Besondere Zeichen für Latex

  cat(paste0("  Sonderzeichen bearbeiten.\n"))
  flush.console()
  for( s in col.sonderzeichen){
    if(! s %in% names(varue.info)){
      warning(paste0("\n   Die Spalte " , s , " existiert nicht in der übergebenen Varue. Für diese Spalte wird nichts aufbereitet.\n"))
    } else {
      varue.info[,s] <- sonderzeichen.aufbereiten(varue.info[,s])
    }
  }

  cat(paste0("  Sortierung der Variablen nach Gliederung und Reihenfolge.\n"))
  flush.console()

  gd <- varue.info$Gliederung
  re <- varue.info$Reihenfolge

  for( g in gd ){
    if(! is.na(g)){
      bool <- suppressWarnings( is.na(as.numeric(re[gd %in% g])))
      if(all(bool)){
        re[gd %in% g] <- 1:length(which(bool))
      } else if(any(bool)){
        re[gd %in% g][bool] <- as.numeric(max(re[gd %in% g][! bool] , na.rm=TRUE))+(1:length(which(bool)))
      }
    }

    if(suppressWarnings( is.na(as.numeric(g)))){
      if(any(! varue.info$in.DS.und.SH[ gd %in% g] %in% "nein")){
        stop(paste0("   Die Variablen " , paste0( varue.info$Var.Name[ ! varue.info$in.DS.und.SH %in% "nein" & gd %in% g] , collapse=", ") , " sollen ins Skalenhandbuch, besitzen aber keinen validen Gliederungspunkt (der Form \'Zahl.Zahl\').\n\n"))
      } else {
        if(is.na(g)){
          re[ is.na(gd)] <- 0
          gd[ is.na(gd)] <- 0
        } else {
          re[ gd %in% g] <- 0
          gd[ gd %in% g] <- 0
        }
      }
    }
  }


  # Sortieren nach Gliederung -> Die Funktion zur Generierung des Gesamt-Tex-Skripts benötigt einen Vektor mit Variablennamen, die sortiert eingegeben werden.
  varue.info <- varue.info[ order( as.numeric(gd) , as.numeric(re) ), ]

  return(varue.info)
}
