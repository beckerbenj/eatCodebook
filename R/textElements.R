##############################################
### TEXT-ELEMENTE ###
##############################################


#### Deckblatt ####
# YRNTCSH! - Sowohl Inhalte auf dem Deckblatt als auch Pfad für das IQB-Logo in der Varue anpassen
makeDeckblatt <- function(varue.file, sheet) {
  cat(paste0("ERSTELLE DECKBLATT.\n\n"))
  flush.console()
  varue.deckblatt <- readWorkbook (xlsxFile = varue.file , sheet = sheet, startRow = 1 )

  db.graphics.file <- varue.deckblatt$Graphics.file
  db.title <- varue.deckblatt$Title
  db.subtitle <- varue.deckblatt$Subtitle
  db.author <- varue.deckblatt$Authors
  db.mitarbeit <- varue.deckblatt$Mitarbeit
  db.schriftenreihe <- varue.deckblatt$Schriftenreihe
  db.biblioInfo <- varue.deckblatt$BiblioInfo

  if(is.null(db.graphics.file)){
    part.graphics <- NULL
  } else {
    if(gsub("\\s" , "" , db.graphics.file)=="-"){
      part.graphics <- NULL
    } else {
      part.graphics <- c("\\begin{flushright}",
                         "\\begin{figure}",
                         paste0("\\includegraphics[scale=0.1]{",db.graphics.file,"}"),
                         "\\end{figure}",
                         "\\end{flushright}")
    }
  }

  if(is.null(db.title)){
    part.title <- NULL
  } else {
    if(gsub("\\s" , "" , db.title)=="-"){
      part.title <- NULL
    } else {
      db.title <- sonderzeichen.aufbereiten(  db.title )
      part.title <- c("\\begin{Huge}",
                      paste0("\\color{iqbrot} \\textbf{",db.title,"} \\par \\medskip"),
                      "\\end{Huge}")
    }
  }

  if(is.null(db.subtitle)){
    part.subtitle <- NULL
  } else {
    if(gsub("\\s" , "" , db.subtitle)=="-"){
      part.subtitle <- NULL
    } else {
      db.subtitle <- sonderzeichen.aufbereiten(  db.subtitle )
      part.subtitle <- c("\\begin{Large}",
                         paste0("\\textbf{",db.subtitle,"}\\par \\bigskip"),
                         "\\end{Large}")
    }
  }

  if(is.null(db.author)){
    part.author <- NULL
  } else {
    if(gsub("\\s" , "" , db.author)=="-"){
      part.author <- NULL
    } else {
      db.author <- sonderzeichen.aufbereiten(  db.author )
      part.author <- c("\\begin{large}",
                       db.author,
                       "\\end{large}")
    }
  }

  if(is.null(db.mitarbeit)){
    part.mitarbeit <- NULL
  } else {
    if(gsub("\\s" , "" , db.mitarbeit)=="-"){
      part.mitarbeit <- NULL
    } else {
      db.mitarbeit <-  sonderzeichen.aufbereiten (db.mitarbeit)
      part.mitarbeit <- paste0(db.mitarbeit, "\\par")
    }
  }

  if(is.null(db.schriftenreihe)){
    part.schriftenreihe <- NULL
  } else {
    if(gsub("\\s" , "" , db.schriftenreihe)=="-"){
      part.schriftenreihe <- NULL
    } else {
      db.schriftenreihe <- sonderzeichen.aufbereiten (db.schriftenreihe)
      part.schriftenreihe <- paste0(db.schriftenreihe," \n")
    }
  }

  if(is.null(db.biblioInfo)){
    part.biblioInfo <- NULL
  } else {
    if(gsub("\\s" , "" , db.biblioInfo)=="-"){
      part.biblioInfo <- NULL
    } else {
      db.biblioInfo <- sonderzeichen.aufbereiten (db.biblioInfo)
      part.biblioInfo <- c("\\textbf{Bibliographische Informationen} \\par",
                           db.biblioInfo ,
                           "\\par \\bigskip",
                           "Alle Rechte vorbehalten.")
    }
  }

  deckblatt <- c( "\\thispagestyle{empty}",
                  part.graphics,
                  "\\vspace*{75mm}",
                  part.title,
                  part.subtitle,
                  part.author,
                  "\\bigskip\n",
                  "\\vfill",
                  "Stand: \\today \\par",
                  part.mitarbeit,
                  part.schriftenreihe,
                  "\\pagebreak",
                  "\\thispagestyle{empty}",
                  "\\pagenumbering{gobble} % frisst die Seitenzahlen",
                  "\\quad",
                  "\\vfill\n",
                  part.biblioInfo	)

  return(deckblatt)
}

makeDeckblatt.variation <- function(db.graphics.file, db.title,db.subtitle,db.author,db.mitarbeit,db.schriftenreihe,db.biblioInfo) {
  cat(paste0("ERSTELLE DECKBLATT.\n\n"))
  flush.console()

  if(is.null(db.graphics.file)){
    part.graphics <- NULL
  } else {
    if(gsub("\\s" , "" , db.graphics.file)=="-"){
      part.graphics <- NULL
    } else {
      part.graphics <- c("\\begin{flushright}",
                         "\\begin{figure}",
                         paste0("\\includegraphics[scale=0.1]{",db.graphics.file,"}"),
                         "\\end{figure}",
                         "\\end{flushright}")
    }
  }

  if(is.null(db.title)){
    part.title <- NULL
  } else {
    if(gsub("\\s" , "" , db.title)=="-"){
      part.title <- NULL
    } else {
      db.title <- sonderzeichen.aufbereiten(  db.title )
      part.title <- c("\\begin{Huge}",
                      paste0("\\color{iqbrot} \\textbf{",db.title,"} \\par \\medskip"),
                      "\\end{Huge}")
    }
  }

  if(is.null(db.subtitle)){
    part.subtitle <- NULL
  } else {
    if(gsub("\\s" , "" , db.subtitle)=="-"){
      part.subtitle <- NULL
    } else {
      db.subtitle <- sonderzeichen.aufbereiten(  db.subtitle )
      part.subtitle <- c("\\begin{Large}",
                         paste0("\\textbf{",db.subtitle,"}\\par \\bigskip"),
                         "\\end{Large}")
    }
  }

  if(is.null(db.author)){
    part.author <- NULL
  } else {
    if(gsub("\\s" , "" , db.author)=="-"){
      part.author <- NULL
    } else {
      db.author <- sonderzeichen.aufbereiten(  db.author )
      part.author <- c("\\begin{large}",
                       db.author,
                       "\\end{large}")
    }
  }

  if(is.null(db.mitarbeit)){
    part.mitarbeit <- NULL
  } else {
    if(gsub("\\s" , "" , db.mitarbeit)=="-"){
      part.mitarbeit <- NULL
    } else {
      db.mitarbeit <-  sonderzeichen.aufbereiten (db.mitarbeit)
      part.mitarbeit <- paste0(db.mitarbeit, "\\par")
    }
  }

  if(is.null(db.schriftenreihe)){
    part.schriftenreihe <- NULL
  } else {
    if(gsub("\\s" , "" , db.schriftenreihe)=="-"){
      part.schriftenreihe <- NULL
    } else {
      db.schriftenreihe <- sonderzeichen.aufbereiten (db.schriftenreihe)
      part.schriftenreihe <- paste0(db.schriftenreihe," \n")
    }
  }

  if(is.null(db.biblioInfo)){
    part.biblioInfo <- NULL
  } else {
    if(gsub("\\s" , "" , db.biblioInfo)=="-"){
      part.biblioInfo <- NULL
    } else {
      db.biblioInfo <- sonderzeichen.aufbereiten (db.biblioInfo)
      part.biblioInfo <- c("\\textbf{Bibliographische Informationen} \\par",
                           db.biblioInfo ,
                           "\\par \\bigskip",
                           "Alle Rechte vorbehalten.")
    }
  }

  deckblatt <- c( "\\thispagestyle{empty}",
                  part.graphics,
                  "\\vspace*{75mm}",
                  part.title,
                  part.subtitle,
                  part.author,
                  "\\bigskip\n",
                  part.version,
                  "\\vfill",
                  "Stand: \\today \\par",
                  part.mitarbeit,
                  part.schriftenreihe,
                  "\\pagebreak",
                  "\\thispagestyle{empty}",
                  "\\pagenumbering{gobble} % frisst die Seitenzahlen",
                  "\\quad",
                  "\\vfill\n",
                  part.biblioInfo	)

  return(deckblatt)
}


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

varue.register.aufbereiten <- function(varue.reg , varue.info){
  #Register - Reduzierung
  cat(paste0(" Aufbereitung des Registers.\n"))
  flush.console()
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
  cat(paste0(" Aufbereitung der Gliederung.\n"))
  flush.console()
  cat(paste0("  Sonderzeichen in den Titeln bearbeiten.\n"))
  flush.console()
  varue.gliederung$Titel <- sonderzeichen.aufbereiten(varue.gliederung$Titel)

  cat(paste0("  Sonderzeichen in den Titeln bearbeiten.\n"))
  flush.console()
  varue.gliederung$Titel <- gsub("[" , "{[" , varue.gliederung$Titel , fixed=TRUE)
  varue.gliederung$Titel <- gsub("]" , "]}" , varue.gliederung$Titel , fixed=TRUE)


  return(varue.gliederung)
}


get.skalen.info <- function(varue.file , sheets, skalen.cols=c( "Var.Name", "Quelle" , "Anzahl.valider.Werte" , "Items.der.Skala" )){
  skalen.info <- readWorkbook ( xlsxFile = varue.file , sheet = sheets , startRow = 1 )
  skalen.info <- skalen.info[ , skalen.cols ]
  skalen.info$Anzahl.valider.Werte[is.na(skalen.info$Anzahl.valider.Werte) | grepl("\\s","",skalen.info$Anzahl.valider.Werte) %in% "" ] <- "-"

  skalen.info <- skalen.info.aufbereiten(skalen.info)

  return(skalen.info)
}

skalen.info.aufbereiten <- function(skalen.info){
  cat(paste0(" Aufbereitung der Skaleninformation.\n"))
  flush.console()

  # Aufbereitung der Varue und der Skaleninformation
  for(i in names(skalen.info)){
    skalen.info[,i] <- as.character( skalen.info[,i] )
  }

  if(any(grepl("\\s" , skalen.info$Var.Name))){
    cat(paste0("  Die Variable/Variablen ", skalen.info$Var.Name[grepl("\\s" , skalen.info$Var.Name)] ," besitzt/besitzen Leerzeichen im Variablennamen. Diese werden entfernt.\n"))
    flush.console()
    skalen.info$Var.Name <- gsub("\\s" , "" , skalen.info$Var.Name)
  }

  cat(paste0("  Überflüssige Leerzeichen entfernen.\n"))
  flush.console()
  skalen.info$Anzahl.valider.Werte <- gsub("\\s" , "" , skalen.info$Anzahl.valider.Werte)
  skalen.info$Items.der.Skala <- gsub("\\s" , "" , skalen.info$Items.der.Skala)

  return(skalen.info)

}

get.lit.info <- function(varue.file , sheets , lit.cols=c( "Kurzangabe" , "Langangabe" , "in.Literaturverzeichnis" )){
  varue.lit <- readWorkbook (xlsxFile = varue.file , sheet = sheets, startRow = 1 ) # Nur benötige Spalten; Ab erster Zeile beginnen --> Spalten bekommen richtige Überschrift
  varue.lit <- varue.lit[, lit.cols]

  return(varue.lit)
}




#### ABKÜRZUNGS- UND HINTERGRUNDMODELLTABELLE ####
makeAbkVerz <- function(varue.file , sheets=c("Akronyme" , "Statistische Formelzeichen") , headings=list("Akronyme"=c( "Abkürzung", "Bedeutung") , "Statistische Formelzeichen"=c( "Symbol", "Bedeutung")) , captions=list("Akronyme"=c( "Abkürzungen") , "Statistische Formelzeichen"=c( "Statistische Formelzeichen"))  , sort.entries=c(TRUE,TRUE)){

  cat(paste0("ERSTELLE ABKÜRZUNGSVERZEICHNIS.\n"))
  flush.console()

  # Reihenfolge der Headings ist wichtig
  if(is.null(sheets)){
    return(null)
  }

  if(!file.exists(varue.file)){
    warning(paste0(" Der angebenene Pfad für die Varue (" , varue.file , ") existiert nicht. Es wird NULL zurückgegeben.\n\n"))
    return(null)
  }

  names(sort.entries) <- sheets

  alle.infos <- lapply( sheets , function(s) {
    cat(paste0(" Lese Sheet \"" , s, "\" ein.\n"))
    flush.console()
    v <- readWorkbook ( xlsxFile = varue.file , sheet = s, startRow = 1 )
    if(any(!names(v) %in% headings[[s]])){
      warning(paste0(" Die angegebenen Überschriften befinden sich nicht in der ersten Zeile im Reiter ",s,". Die übergebenen Überschriften werden als Spaltennamen der ersten beiden Spalten übernommen.\n\n"))
      names(v) <- headings[[s]]
    }
    v <- v[ , headings[[s]] ]
    v <- v[ sapply(1:dim(v)[1] , function(zeile) any(sapply(1:dim(v)[2] , function(spalte) ! is.na(v[zeile,spalte])))),]
    v <- v[ sapply(1:dim(v)[1] , function(zeile) any(sapply(1:dim(v)[2] , function(spalte) ! (gsub("\\s", "" , v[zeile,spalte]) %in% "" | gsub("\\s", "" , v[zeile,spalte]) %in% "" ) ))),]
    if(sort.entries[s]){
      v <- v[ order(v[,1]),]
    }
    return(v)
  } )

  names(alle.infos) <- sheets

  alle.code <- lapply(1:length(sheets) , function(i){
    cat(paste0(" Erstelle Latex-Code für Sheet " , i, ".\n"))
    flush.console()
    v <- alle.infos[[i]]

    if(length(v[,1])>1){
      v.code <- c(paste0("\\begin{longtabu}{l",rep("Q",dim(v)[2]-1),"}"),
                  paste0("\\caption*{\\cellcolor{white} \\textbf{",captions[i],"}}\\\\"),
                  "\\toprule",
                  "\\headrow",
                  paste0(paste0("\\textbf{" , headings[[i]] , "}" , collapse=" & ") , "\\\\"),
                  "\\midrule",
                  "\\endhead",
                  paste0("\\hline \\multicolumn{",dim(v)[2],"}{@{}c@{}}{\\cellcolor{white} \\textit{Fortsetzung auf der nächsten Seite}}\\\\\\hline"),
                  "\\endfoot",
                  "\\endlastfoot",
                  "\\taburowcolors{white .. lg}",
                  paste0(sapply(1:dim(v)[1] , function(d) paste0(v[d,] , collapse=" & " )) , "\\\\"),
                  "\\nobreakbottomrule",
                  "\\end{longtabu}\n")
      return(v.code)
    } else {
      return(NULL)
    }
  })

  # Abkürzungsverzeichnis
  if(all(unname(sapply(alle.code , is.null)))){
    abkuerzverz <- NULL
  } else {
    cat(paste0(" Zusammenführen aller Codes.\n"))
    flush.console()
    abkuerzverz <- c( "\\clearpage",
                      "\\phantomsection",
                      "\\section*{Abkürzungsverzeichnis}\n",
                      "\\addcontentsline{toc}{section}{Abkürzungsverzeichnis}",
                      "%\\clearscrheadings",
                      "%\\cfoot[\\pagemark]{\\pagemark}",
                      "\\ihead[\\leftmark]{\\leftmark \\newline \\textsc{Abkürzungsverzeichnis}}",
                      do.call("c" , alle.code))
  }

  return(abkuerzverz)
}
