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

get.ds <- function(ds.files , fbshort, varue.info, use.value.labels=FALSE){
  ds <- lapply( ds.files , ds.einlesen , use.value.labels=use.value.labels , do.print=TRUE)

  names(ds) <- fbshort

  ds <- datensatz.aufbereiten(ds , varue.info , fbshort)

  return(ds)
}

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


datensatz.aufbereiten <- function(ds , varue.info , fbshort){

  names(varue.info) <- fbshort
  names(ds) <- fbshort


  # leerzeichen löschen
  var.num <- lapply( fbshort , function(d) varue.info[[d]]$Var.Name[! varue.info[[d]]$Layout %in% c(0,1)  & varue.info[[d]]$in.DS.und.SH %in% c("ja", "ds")] )
  names(var.num) <- fbshort

  for(d in fbshort){
    cat(paste0(" Leerzeichen werden aus dem Datensatz (",d,") entfernt.\n"))
    flush.console()
    if(any(grepl("\\s" , names(ds[[d]])))){
      vars <- names(ds[[d]])[grepl("\\s" , names(ds[[d]]))]
      cat(paste0("  Die Variable(n) ", paste0(vars., collapse=", ") , " besitzen Leerzeichen im Namen. Diese werden entfernt.\n"))
      flush.console()
      names(ds[[d]]) <- gsub("\\s" , "" , names(ds[[d]]))
    }

    for(v in var.num[[d]]){
      if(! v %in% names(ds[[d]])){
        cat(paste0("  Die Variable ", v , " gibts nicht im Datensatz (",d,").\n"))
        flush.console()
      } else if(any(grepl("\\s" , ds[[d]][,v]))){
        cat(paste0("  Bei der Variable ", v , " im Datensatz (",d,") gibts in mindestens einem Eintag mindestens ein Leerzeichen. Die Leerzeichen werden entfernt.\n"))
        flush.console()
        ds[[d]][,v] <- gsub("\\s" , "" , ds[[d]][,v])
      }
    }
  }

  names(ds) <- fbshort
  return(ds)
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
