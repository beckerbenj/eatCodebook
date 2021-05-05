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
