
####
#############################################################################
#' Create codebook script.
#'
#' Create the complete codebook latex script.
#'
#'@param varInfo Liste mit data.frames der Uebersichten der Variableninformationen
#'@param missings Liste mit data.frames der Uebersichten der Werteinformationen
#'@param struc Liste mit data.frames der Uebersichten der Gliederungsinformationen
#'@param scaleInfo data.frame, Skaleninformationen ueber alle Fragebogen
#'@param register Liste mit data.frames der Uebersichten der Registerinformationen
#'@param make.reg tbd
#'@param dat Liste mit data.frames der Datensaetze
#'@param Kennwertedatensatz Liste mit data.frames der Kennwertedatensaetze
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
#'
#'@return Codebook latex script.
#'
#'@examples
#'#tbd
#'
#'@export
codebook <- function(varInfo, missings, struc, scaleInfo, register,
                     make.reg = NULL, dat, Kennwertedatensatz, id,
                     fbshort, fblong,
                     deckblatt, intro, literatur, abkuerzverz, hintmod, lastpage) {

  # allow input as single data.frames
  if(is.data.frame(varInfo)) {
    varInfo <- list(varInfo)
    missings <- list(missings)
    dat = list(dat)
    Kennwertedatensatz = list(Kennwertedatensatz)
    fblong <- list(fblong)
    names(varInfo) <- names(missings) <- names(dat) <- names(Kennwertedatensatz) <- names(fblong) <- fbshort
  }

  check_codebook_input(varInfo = varInfo, missings = missings, struc = struc, register = register,
                       dat = dat, Kennwertedatensatz = Kennwertedatensatz, fbshort = fbshort, fblong = fblong)

  # SH-Variablen
  variablen <- lapply(varInfo, function(single_varInfo) {
    single_varInfo[single_varInfo$in.DS.und.SH %in% c("ja", "sh"), "Var.Name"]
  })

  #### Skript erstellen ####
  # Doppelte Variablen
  double.vars <- unname(unlist(variablen))
  double.vars <- names( table(double.vars)[table(double.vars)>1])

  # TRUE/FALSE pro Instrument, ob Register erstellt werden soll (falls nicht schon uebergeben)
  if(is.null(make.reg) || ! is.logical(make.reg) || ! length(make.reg)==length(fbshort)){
    make.reg <- unlist(lapply( fbshort , function(d) {
      bool <- all(sapply( names(register[[d]]) , function(k) all(is.null(register[[d]][[k]])) ) )
      bool <- bool |  all(sapply( names(register[[d]]) , function(k) all( gsub("\\s" , "" , register[[d]][[k]]) %in% "" ) ) )
      return(!bool)
    } ) )

    names(make.reg) <- fbshort
  }

  #browser()
  # Praeambel
  alleEbenen <- unique(unlist(sapply(1:length(fbshort) , function(d) {
    g <- paste0(varInfo[[d]]$Gliederung[ varInfo[[d]]$in.DS.und.SH %in% c("ja","sh") ] )
    r <- varInfo[[d]]$Reihenfolge[varInfo[[d]]$in.DS.und.SH %in% c("ja","sh")]

    r[ r %in% "-"] <- 0

    return(paste0(g, ".", r))
  } ) ) )

  alleEbenen <- list( "Chapter"=as.character(utils::as.roman(1:(length(fbshort)+1))),
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
    #												lastcountervar <- varInfo[[d]]$Var.Name[ varInfo[[d]]$in.DS.und.SH %in% c("ja","sh") ][length( varInfo[[d]]$Var.Name[varInfo[[d]]$in.DS.und.SH %in% c("ja","sh")] )]
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
                             varue.info = varInfo[[d]],
                             varue.missings=missings[[d]],
                             Gesamtdatensatz=dat[[d]],
                             skalen.info=scaleInfo[ scaleInfo$Quelle %in% d,],
                             varue.gliederung=struc[[d]],
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
      return(makeRegister(fblong = fblong, fb.akt=d , varue.reg=register[[d]] , double.vars=double.vars) )
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



# -----------------------------------------------------------------------------
check_codebook_input <- function(varInfo, missings, struc, register,
                                 dat, Kennwertedatensatz, fbshort, fblong) {
  if(any(is.null(fbshort))){
    stop()
  }

  ### names anpassen (zur Sicherheit) ###
  if(length(dat)!=length(fbshort)) stop()
  if( ! all(names(dat) %in% fbshort)) stop()

  if( length(varInfo)!=length(fbshort)) stop()
  if( ! all(names(varInfo) %in% fbshort)) stop()

  if( length(missings)!=length(fbshort)) stop()
  if( ! all(names(missings) %in% fbshort)) stop()

  if( length(struc)!=length(fbshort)) stop()
  if( ! all(names(struc) %in% fbshort)) stop()

  if( length(register)!=length(fbshort)) stop()
  if( ! all(names(register) %in% fbshort)) stop()

  if( length(Kennwertedatensatz)!=length(fbshort)) stop()
  if( ! all(names(Kennwertedatensatz) %in% fbshort)) stop()

  if( length(fblong)!=length(fbshort)) stop()
  if( ! all(names(fblong) %in% fbshort)) stop()

  NULL
}


