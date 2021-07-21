varue.erstellen <- function(ds , fbshort=NULL, set.gliederung=TRUE , encoding.list=data.frame("input"=c("Ã¼","Ã¶","Ã¤","ÃŸ","Ã„" , "â„¢" , "â€“" , "â€¦" , "â€ž" , "Â¬"),
                                                                                              "output"=c("ü" , "ö", "ä", "ß" , "Ä", "\\textsuperscript{TM}" , "-" , "\\dots" , "``" , ""),
                                                                                              stringsAsFactors=FALSE), return.method="save.only", save.folder=getwd()){
  cat(paste0("ERSTELLEN DER SHEETS FUER VOLLSTAENDIGE VARUE.\n"))
  flush.console()

  cat(paste0(" SHEETS FUER VARIABLENINFORMATIONEN.\n"))
  flush.console()

  if(is.null(fbshort)){
    if(is.null(names(ds))){
      fbshort <- paste0("ds",1:length(ds))
    } else if( any(is.na(names(ds)))){
      fbshort <- paste0("ds",1:length(ds))
    } else {
      fbshort <- names(ds)
    }
  } else if(any(is.na(fbshort))){
    if(is.null(names(ds))){
      fbshort <- paste0("ds",1:length(ds))
    } else if( any(is.na(names(ds)))){
      fbshort <- paste0("ds",1:length(ds))
    } else {
      fbshort <- names(ds)
    }
  } else {
    if(is.null(names(ds))){
      fbshort <- paste0("ds",1:length(ds))
    } else if( any(is.na(names(ds)))){
      fbshort <- paste0("ds",1:length(ds))
    } else {
      fbshort <- names(ds)
    }
  }

  ### Achtung, Hotfix!!!!!!
  message("achtung hotfix von Benjamin")
  make.gliederung <- TRUE

  variableninfo <- lapply( 1:length(ds) , function(d) makeVarue(ds[[d]] , make.gliederung=make.gliederung , encoding.list=encoding.list) )
  names(variableninfo) <- fbshort
  cat(paste0(" SHEETS FUER WERTEINFORMATIONEN.\n"))
  flush.console()

  werteinfos <- lapply( 1:length(ds) , function(k) {
    x <- lapply(names(ds[[k]]) , function(d) makeValueInfos( ds=ds[[k]], name=d ,encoding.list=encoding.list ))
    x <- do.call("rbind" , x)
    rownames(x) <- NULL

    return(x)
  } )
  names(werteinfos) <- fbshort

  for(i in fbshort){
    variableninfo[[i]]$Layout <- sapply( variableninfo[[i]]$Var.Name , function(name) {
      if( name %in% werteinfos[[i]]$Var.name) {
        if(all(werteinfos[[i]]$missing[werteinfos[[i]]$Var.name %in% name] %in% "ja")){
          return(4)
        } else {
          return(2)
        }
      } else {
        if( all(is.character(ds[[i]][[name]]))){
          return(1)
        } else if(all(is.na(ds[[i]][[name]]))){
          return(8)
        } else {
          return(0)
        }
      }
    } )
  }

  sheets <- lapply(1:length(ds) , function(d) return( list("Variableninfo"=variableninfo[[d]] , "Werteinfo"=werteinfos[[d]]) ) )
  names(sheets) <- fbshort

  if(tolower(return.method)=="save.only" | tolower(return.method)=="save.and.return"){
    cat(paste0("SPEICHERN ALLER SHEETS.\n"))
    flush.console()

    if(is.null(save.folder)){
      save.folder <- getwd()
    } else if (!file.exists(save.folder)) {
      save.folder <- getwd()
    }
    for(i in 1:length(ds)){
      cat(paste0(" SPEICHERN DER VARIABLENINFORMATION.\n"))
      flush.console()
      save.file <- file.path(save.folder , paste0("Variableninformation_",fbshort[i],".txt"))
      write.table(variableninfo[[i]] , file=save.file , sep="\t" , row.names=FALSE)

      cat(paste0(" SPEICHERN DER WERTEINFORMATION.\n"))
      flush.console()
      save.file <- file.path(save.folder , paste0("Werteinformation_",fbshort[i],".txt"))
      write.table(werteinfos[[i]] , file=save.file , sep="\t" , row.names=FALSE)
    }
  }

  if(tolower(return.method)=="save.and.return" | tolower(return.method)=="return.only"){
    return(sheets)
  } else {
    return(NULL)
  }
}
