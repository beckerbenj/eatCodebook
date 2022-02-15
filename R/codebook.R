
####
#############################################################################
#' Create codebook script.
#'
#' Create the complete codebook latex script.
#'
#'@param varue.info Liste mit data.frames der Uebersichten der Variableninformationen
#'@param varue.missings Liste mit data.frames der Uebersichten der Werteinformationen
#'@param varue.gliederung Liste mit data.frames der Uebersichten der Gliederungsinformationen
#'@param skalen.info data.frame, Skaleninformationen ueber alle Fragebogen
#'@param varue.reg Liste mit data.frames der Uebersichten der Registerinformationen
#'@param make.reg tbd
#'@param Gesamtdatensatz Liste mit data.frames der Datensaetze
#'@param Kennwertedatensatz Liste mit data.frames der Kennwertedatensaetze
#'@param variablen Liste mit character-Vektoren der zu berichtenden Variablen
#'@param id tbd
#'@param fbshort Character-Vektor, Fragebogenkuerzel
#'@param fblong Character-Vektor, Namen der Fragebogen, wie sie im Skalenhandbuch ausformuliert genannt werden
#'@param deckblatt tbd
#'@param intro Character-Vektor, Einleitung
#'@param literatur Character-Vektor, Literaturverzeichnis
#'@param abkuerzverz Character-Vekotr, Tabelle Abkuerzungsverzeichnis
#'@param hintmod Character-Vektor, Tabelle Hintergrundmodell
#'@param lastpage Character-Vektor, Letzte Seite
#'
#'@param varue.info tbd
#'
#'@return Codebook latex script.
#'
#'@examples
#'#tbd
#'
#'@export
codebook <- function( varue.info ,
                     varue.missings,
                     varue.gliederung,
                     skalen.info,
                     varue.reg,
                     make.reg=NULL,
                     Gesamtdatensatz,
                     Kennwertedatensatz,
                     variablen,
                     id,
                     fbshort,
                     fblong,
                     deckblatt,
                     intro,
                     literatur,
                     abkuerzverz,
                     hintmod,
                     lastpage) {

  if(any(is.null(fbshort))){
    stop()
  }

  ### names anpassen (zur Sicherheit) ###
  if(length(Gesamtdatensatz)!=length(fbshort)) stop()
  if( ! all(names(Gesamtdatensatz) %in% fbshort)){
      names(Gesamtdatensatz) <- fbshort
  }

  if( length(varue.info)!=length(fbshort)) stop()
  if( ! all(names(varue.info) %in% fbshort)){
      names(varue.info) <- fbshort
  }

  if( length(varue.missings)!=length(fbshort)) stop()
  if( ! all(names(varue.missings) %in% fbshort)){
      names(varue.missings) <- fbshort
  }

  if( length(varue.gliederung)!=length(fbshort)) stop()
  if( ! all(names(varue.gliederung) %in% fbshort)){
      names(varue.gliederung) <- fbshort
  }

  if( length(varue.reg)!=length(fbshort)) stop()
  if( ! all(names(varue.reg) %in% fbshort)){
      names(varue.reg) <- fbshort
  }

  if( length(Kennwertedatensatz)!=length(fbshort)) stop()
  if( ! all(names(Kennwertedatensatz) %in% fbshort)){
    names(Kennwertedatensatz) <- fbshort
  }

  if( length(variablen)!=length(fbshort)) stop()
  if( ! all(names(variablen) %in% fbshort)){
      names(variablen) <- fbshort
  }

  if( length(fblong)!=length(fbshort)) stop()
  if( ! all(names(fblong) %in% fbshort)){
      names(fblong) <- fbshort
  }


  #### Skript erstellen ####
  # Doppelte Variablen
  double.vars <- unname(unlist(variablen))
  double.vars <- names( table(double.vars)[table(double.vars)>1])

  # TRUE/FALSE pro Instrument, ob Register erstellt werden soll (falls nicht schon uebergeben)
  if(is.null(make.reg)){
    make.reg <- unlist(lapply( fbshort , function(d) {
      bool <- all(sapply( names(varue.reg[[d]]) , function(k) all(is.null(varue.reg[[d]][[k]])) ) )
      bool <- bool |  all(sapply( names(varue.reg[[d]]) , function(k) all( gsub("\\s" , "" , varue.reg[[d]][[k]]) %in% "" ) ) )
      return(!bool)
    } ) )

    names(make.reg) <- fbshort
  } else if( ! class(make.reg)=="boolean"){
    make.reg <- unlist(lapply( fbshort , function(d) {
      bool <- all(sapply( names(varue.reg[[d]]) , function(k) all(is.null(varue.reg[[d]][[k]])) ) )
      bool <- bool |  all(sapply( names(varue.reg[[d]]) , function(k) all( gsub("\\s" , "" , varue.reg[[d]][[k]]) %in% "" ) ) )
      return(!bool)
    } ) )

    names(make.reg) <- fbshort
  } else if( ! length(make.reg)==length(fbshort)) {
    make.reg <- unlist(lapply( fbshort , function(d) {
      bool <- all(sapply( names(varue.reg[[d]]) , function(k) all(is.null(varue.reg[[d]][[k]])) ) )
      bool <- bool |  all(sapply( names(varue.reg[[d]]) , function(k) all( gsub("\\s" , "" , varue.reg[[d]][[k]]) %in% "" ) ) )
      return(!bool)
    } ) )

    names(make.reg) <- fbshort
  }


  # Praeambel
  alleEbenen <- unique(unlist(sapply(1:length(fbshort) , function(d) {
    g <- paste0(varue.info[[d]]$Gliederung[ varue.info[[d]]$in.DS.und.SH %in% c("ja","sh") ] )
    r <- varue.info[[d]]$Reihenfolge[varue.info[[d]]$in.DS.und.SH %in% c("ja","sh")]

    r[ r %in% "-"] <- 0

    return(paste0(g, ".", r))
  } ) ) )

  alleEbenen <- list( "Chapter"=as.character(as.roman(1:(length(fbshort)+1))),
                      "Section"=unique(unlist(sapply( alleEbenen , function(d) paste0(unlist(strsplit(d , ""))[ 1:(which(unlist(strsplit(d , "")) %in% ".")[1]-1) ]  , collapse="")
                      ) ) ),
                      "SubSection"=unique(sapply( alleEbenen , function(d) {
                        if( length(which(unlist(strsplit(d , "")) %in% ".")) <2){
                          return(d)
                        } else {
                          return(paste0(unlist(strsplit(d , ""))[ 1:(which(unlist(strsplit(d , "")) %in% ".")[2]-1) ] , collapse="") )
                        } } ) ),
                      "SubSubSection"=unique(sapply( alleEbenen , function(d) {
                        if( length(which(unlist(strsplit(d , "")) %in% "."))<3){
                          return(d)
                        } else {
                          return(paste0(unlist(strsplit(d , ""))[ 1:(which(unlist(strsplit(d , "")) %in% ".")[3]-1) ] , collapse="") )
                        } } ) ) )

  max.Chap <- paste0(rep("X",max(nchar(alleEbenen[["Chapter"]]) ) ) , collapse="")
  max.Sec <- paste0(gsub("\\d" , "0" , alleEbenen[["Section"]][which.max(sapply(alleEbenen[["Section"]] , function(d) length(unlist(strsplit(d , ""))) ))] ) , "0")
  max.Subsec <-paste0(gsub("\\d" , "0" , alleEbenen[["SubSection"]][which.max(sapply(alleEbenen[["SubSection"]] , function(d) length(unlist(strsplit(d , ""))) ))] ) , "0")
  max.Subsubsec <- paste0(gsub("\\d" , "0" , alleEbenen[["SubSubSection"]][which.max(sapply(alleEbenen[["SubSubSection"]] , function(d) length(unlist(strsplit(d , ""))) ))] ) , "0")

  layout.prae.ges <- layout.prae(variablen , fbshort=fbshort , double.vars=double.vars , deckblatt=deckblatt , makeCounter=make.reg,maxLength.Chap=max.Chap, maxLength.Sec=max.Sec,maxLength.Subsec=max.Subsec, maxLength.Subsubsec=max.Subsubsec)

  all_length <- c(max.Chap , max.Sec , max.Subsec ,max.Subsubsec)

  # Skript der Variablen erstellen
  skript.fb <- lapply( fbshort , function(d) {
    #												lastcountervar <- varue.info[[d]]$Var.Name[ varue.info[[d]]$in.DS.und.SH %in% c("ja","sh") ][length( varue.info[[d]]$Var.Name[varue.info[[d]]$in.DS.und.SH %in% c("ja","sh")] )]
    message(paste0("\n Erstelle Layout-Skripte fuer: ", d, "\n"))
    ret. <- c( "\\phantomsection" ,
               paste0("\\chapter{",fblong[d],"}"),
               paste0("\\setcounter{sec",toupper(d),"}{\\thepage}"),
               unlist( sapply( variablen[[d]], function(v) {
                 #browser()
                 layout.var( name=v,
                             fb=tolower(d),
                             id.fb=id[d],
                             kennwerte.var = unlist(Kennwertedatensatz[[d]][ names(Kennwertedatensatz[[d]]) %in% v] , recursive=FALSE),
                             varue.info=varue.info[[d]],
                             varue.missings=varue.missings[[d]],
                             Gesamtdatensatz=Gesamtdatensatz[[d]],
                             skalen.info=skalen.info[ skalen.info$Quelle %in% d,],
                             varue.gliederung=varue.gliederung[[d]],
                             double.vars=double.vars,
                             makeCounter=make.reg[d],
                             all_length=all_length)
               }))
               #,paste0( "\\setcounter{", numtolet( lastcountervar , fb=tolower(d) ,double.vars=double.vars ) ,"}{\\thepage }" )
    )

    return(ret.)
  } )



  register.fb <- lapply( fbshort , function(d) {
    if(! make.reg[d]) {
      return(NULL)
    } else {
      # Erstelle Register
      return(makeRegister(fblong = fblong, fb.akt=d , varue.reg=varue.reg[[d]] , double.vars=double.vars) )
    }
  } )


  # Formatieren
  skript.fb <- unname(unlist(skript.fb))
  register.fb <- unname(unlist(register.fb))


  # Gesamter Anhang
  if(all( unname( sapply( list(literatur, register.fb, abkuerzverz, hintmod, lastpage) , is.null) ) ) ){
    anhang <- NULL
    skript.fb[length(skript.fb)] <- sub("\\clearpage" , "" , skript.fb[length(skript.fb)] , fixed=TRUE)
  } else {
    anhang <- c("\\pagebreak","\\chapter{Anhang}" , literatur, register.fb, abkuerzverz, hintmod, lastpage)
  }

  # Gesamtes Skript
  skript <- c(layout.prae.ges, intro, skript.fb, anhang, "\\end{document}")
  skript
}

