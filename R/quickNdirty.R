#### QUICK-N-DIRTY FUNKTION ####
quickNdirty <- function(ds.files , save.objects=c("skript", "varue.info" , "varue.missings" , "kennwerte"), save.folder=getwd() ){
  cat(paste0("ERSTELLE SKALENHANDBUCH: QUICK-N-DIRTY!\n\n"))
  flush.console()
  cat(paste0(" HINWEIS: Nach diversen Generalproben muss spezifiziert werden, dass die Funktion mehr dirty als quick ist...\n\n"))
  flush.console()

  fbshort <- paste0("ds", 1:length(ds.files))
  fblong <- gsub(".*/(.*)\\.sav" , "\\1" , ds.files)
  fblong <- gsub("_" , "\\_" , fblong , fixed=TRUE)
  names(fblong) <- fbshort

  names(ds.files) <- fbshort

  cat(paste0(" Einlesen der Datensaetze.\n"))
  flush.console()

  ds <- lapply( ds.files , ds.einlesen , use.value.labels=TRUE)
  names(ds) <- fbshort

  ds.df <- lapply( ds.files , ds.einlesen , use.value.labels=FALSE, do.print=FALSE)
  names(ds.df) <- fbshort

  encoding <- data.frame("input"=c("Ã¼","Ã¶","Ã¤","ÃŸ","Ã„" , "â„¢" , "â€“" , "â€¦" , "â€ž" , "Â¬"),
                         "output"=c("ü" , "ö", "ä", "ß" , "Ä", "\\textsuperscript{TM}" , "-" , "\\dots" , "``" , ""),
                         stringsAsFactors=FALSE)

  cat(paste0(" Erstelle Variableninformation(en).\n"))
  flush.console()
  varue.info <- lapply(1:length(ds) , function(d) makeVarue(ds[[d]]  , encoding.list=encoding) )
  names(varue.info) <- fbshort
  varue.info <- lapply(fbshort , function(d) varue.info.aufbereiten(varue.info[[d]] , col.sonderzeichen=c("LabelSH" , "Titel" , "QuelleSH" , "Anmerkung.Var")) )
  names(varue.info) <- fbshort

  cat(paste0(" Erstelle Werteinformation(en).\n"))
  flush.console()
  varue.missings <- lapply( 1:length(ds) , function(d) do.call("rbind" , lapply(names(ds[[d]]) , function(name) makeValueInfos(ds[[d]] , name , encoding.list=encoding) ) ) )
  names(varue.missings) <- fbshort
  varue.missings <- lapply(fbshort , function(d) varue.missings.aufbereiten(varue.info[[d]] , varue.missings[[d]] , ds.df[[d]] , d) )
  names(varue.missings) <- fbshort


  for(i in fbshort){
    varue.info[[i]]$Layout <- sapply( varue.info[[i]]$Var.Name , function(name) {
      if( name %in% varue.missings[[i]]$Var.name) {
        if(all(varue.missings[[i]]$missing[varue.missings[[i]]$Var.name %in% name] %in% "ja")){
          return(4)
        } else {
          return(2)
        }
      } else {
        if( is.character(ds[[i]][[name]])){
          return(1)
        } else if(all(is.na(ds[[i]][[name]]))){
          return(8)
        } else {
          return(0)
        }
      }
    } )
  }


  cat(paste0(" Erstelle Skaleninformation(en) (es werden keine Skalen aufgenommen, es muss lediglich das Objekt erstellt werden.\n"))
  flush.console()
  skalen.info <- data.frame( "Var.Name"="", "Quelle"="" , "Anzahl.valider.Werte"="" , "Items.der.Skala"="" , stringsAsFactors=FALSE )

  cat(paste0(" Erstelle Gliederungsinformation(en).\n"))
  flush.console()
  varue.gliederung <- lapply( fbshort , function(d) {
    n <- length(varue.info[[d]]$Var.Name)
    data.frame( "Titel"=varue.info[[d]]$LabelSH, "Ebene"=1:n , stringsAsFactors=FALSE)
  })
  names(varue.gliederung) <- fbshort

  cat(paste0(" Erstelle Register.\n"))
  flush.console()
  varue.reg <- lapply(fbshort , function(d) data.frame("Var.Name"="" , stringsAsFactors=FALSE))
  names(varue.reg) <- fbshort

  variablen.all <- lapply(1:length(ds) , function(d) varue.info[[d]]$Var.Name)
  names(variablen.all) <- fbshort

  id <- sapply(fbshort , function(d) varue.info[[d]]$Var.Name[1])
  names(id) <- fbshort

  kds <- lapply(fbshort , function(d) kds.erstellen( fbshort=d, varue.info=varue.info[[d]], varue.missings=varue.missings[[d]] , id=varue.info[[d]]$Var.Name[1], ds=ds.df[[d]] , variablen.=variablen.all[[d]] , ds.file=NULL , folder.data=NULL, do.save=FALSE ) )
  names(kds) <- fbshort


  cat(paste0(" Erstelle Metadaten.\n"))
  flush.console()
  metadata <- c( paste0("\\Title{Skalenhandbuch}"),
                 paste0("\\Author{Quick, I.\\sep Dirty, S.}"),
                 paste0("\\Keywords{Skalenhandbuch}"))

  intro <- NULL

  abkuerzverz <- NULL

  hintmod <- NULL

  lastpage <- NULL

  literatur <- NULL

  deckblatt <- makeDeckblatt.variation(db.graphics.file=NULL , db.title="Skalenhandbuch." , db.subtitle="Dokumentation der Erhebungsinstrumente" , db.author=NULL , db.mitarbeit=NULL , db.schriftenreihe=NULL , db.biblioInfo=NULL)

  make.reg <- c(FALSE,FALSE,FALSE)
  names(make.reg) <- fbshort

  skript <- SHtotal( varue.info=varue.info ,
                     varue.missings=varue.missings,
                     varue.gliederung=varue.gliederung,
                     skalen.info=skalen.info,
                     varue.reg=varue.reg,
                     make.reg=make.reg,
                     Gesamtdatensatz=ds.df,
                     Kennwertedatensatz=kds,
                     variablen=variablen.all,
                     id=id,
                     fbshort=fbshort,
                     fblong=fblong,
                     intro=intro,
                     literatur=literatur,
                     abkuerzverz=abkuerzverz,
                     hintmod=hintmod,
                     lastpage=lastpage,
                     deckblatt=deckblatt)

  if(! any(is.null(save.objects))){
    if("skript" %in% save.objects){
      cat(paste0(" Speichern des Latex-Skripts in " , save.folder , ".\n"))
      flush.console()
      tex.file <- file.path(save.folder , "quickndirty.tex")
      metadata.file <- file.path(save.folder , "quickndirty.xmpdata")

      write.table( metadata , file = metadata.file , fileEncoding="UTF-8" , col.names=FALSE , row.names=FALSE , quote = FALSE )
      write.table( skript , file = tex.file , fileEncoding="UTF-8" , col.names=FALSE , row.names=FALSE , quote = FALSE )
    }

    if(any(c("varue.info" , "variableninfo" , "variableninformation", "variableninformationen") %in% tolower(save.objects))){
      cat(paste0(" Speichern der Variableninformation in " , save.folder , ".\n"))
      flush.console()
      for(i in fbshort){
        varue.save.file <- file.path(save.folder , paste0("variableninfo_",i,".txt"))
        write.table(varue.info[[i]] , file=varue.save.file , sep="\t" , row.names=FALSE)
      }
    }

    if(any(c("varue.missings" , "werteinfo" , "werteinformation", "werteinformationen") %in% tolower(save.objects))){
      cat(paste0(" Speichern der Werteinformation in " , save.folder , ".\n"))
      flush.console()
      for(i in fbshort){
        varue.save.file <- file.path(save.folder , paste0("werteinfo_",i,".txt"))
        write.table(varue.missings[[i]] , file=varue.save.file , sep="\t" , row.names=FALSE)
      }
    }

    if(any(c("kennwerte" , "kds", "kennwertedatensatz") %in% tolower(save.objects))){
      cat(paste0(" Speichern des Kennwertedatensatzes in " , save.folder , ".\n"))
      flush.console()
      for(i in fbshort){
        kds.save.file <- file.path(save.folder , paste0("kennwertedatensatz_",i,".rdata"))
        save( kds[[i]], file = kds.save.file )
      }
    }
  }

  return(skript)
}

