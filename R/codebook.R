
####
#############################################################################
#' Create codebook script.
#'
#' Create the complete codebook latex script.
#'
#'@param varInfo \code{data.frame} or list of \code{data.frames} containing the variable information, imported via \code{\link{getVarInfo}}.
#'@param missings \code{data.frame} or list of \code{data.frames} containing the missing information, imported via \code{\link{getMissings}}.
#'@param struc \code{data.frame} containing the structure of the codebook, imported via \code{\link{getStructure}}.
#'@param scaleInfo \code{data.frame} containing the information on scales, imported via \code{\link{getScaleInfo}}.
#'@param register \code{data.frame} containing the information on the register, imported via \code{\link{getRegister}}.
#'If \code{NULL}, now register is created. If there are registers for some data sets but not all, the missing registers are simply omitted.
#'@param dat \code{data.frame} or list of \code{data.frames} containing the data sets, imported via \code{\link{import_spss}}.
#'@param Kennwertedatensatz \code{data.frame} or list of \code{data.frame} containing the descriptive statistics, imported via \code{\link{calculateDescriptives}}.
#'@param chapters \code{data.frame} or list of \code{data.frames} containing the chapter information, imported via \code{\link{getChapters}}.
#'Determines the order of chapters in the codebook.
#'@param deckblatt Character vector with the cover page, created via \code{\link{makeCover}}.
#'@param intro Character vector, introduction.
#'@param literatur Character vector with the literature information, created via \code{\link{makeLit}}.
#'@param abkuerzverz Character vector with the abbreviations, created via \code{\link{makeAbbrList}}.
#'@param hintmod Character vector with the information on the background model, created via \code{\link{makeBGM}}.
#'@param lastpage Character vektor, last page.
#'
#'@return Codebook latex script.
#'
#'@examples
#'#tbd
#'
#'@export
codebook <- function(varInfo, missings, struc, scaleInfo, register = NULL, dat, Kennwertedatensatz,
                     chapters, deckblatt, intro, literatur, abkuerzverz, hintmod, lastpage) {

  # allow input as single data.frames
  if(is.data.frame(varInfo)) {
    varInfo <- list(varInfo)
    missings <- list(missings)
    dat <- list(dat)
    Kennwertedatensatz <- list(Kennwertedatensatz)
    struc <- list(struc)
    names(varInfo) <- names(missings) <- names(dat) <- names(Kennwertedatensatz) <- names(struc) <- chapters[["dataName"]]
  }

  # input validation
  check_codebook_input(varInfo = varInfo, missings = missings, struc = struc, register = register,
                       dat = dat, Kennwertedatensatz = Kennwertedatensatz, chapters = chapters)

  # recreate fbshort and fblong
  fbshort <- chapters[["dataName"]]
  fblong <- chapters[["chapterName"]]
  names(fblong) <- chapters[["dataName"]]

  # recreate make.reg
  make.reg <- fbshort %in% names(register)
  names(make.reg) <- fbshort

  # SH-Variablen
  variablen <- lapply(varInfo, function(single_varInfo) {
    single_varInfo[single_varInfo$in.DS.und.SH %in% c("ja", "sh"), "Var.Name"]
  })

  #### Skript erstellen ####
  # Doppelte Variablen
  double.vars <- unname(unlist(variablen))
  double.vars <- names( table(double.vars)[table(double.vars)>1])

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

  layout.prae.ges <- layout.prae(variablen , fbshort=fbshort , double.vars=double.vars , deckblatt=deckblatt , makeCounter=make.reg, maxLength.Chap=max.Chap, maxLength.Sec=max.Sec,maxLength.Subsec=max.Subsec, maxLength.Subsubsec=max.Subsubsec)

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



  register.fb <- lapply(names(register) , function(d) {
    makeRegister(fblong = fblong, fb.akt=d , varue.reg=register[[d]] , double.vars=double.vars)
  })


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
                                 dat, Kennwertedatensatz, chapters) {
  fbshort <- chapters[["dataName"]]

  ### names anpassen (zur Sicherheit) ###
  if(length(dat)!=length(fbshort)) stop("'dat' and 'chapters' have different lengths.")
  if( ! all(names(dat) %in% fbshort)) stop("'dat' is differently named than the 'dataName' entries in 'chapters'.")

  if( length(varInfo)!=length(fbshort)) stop("'varInfo' and 'chapters' have different lengths.")
  if( ! all(names(varInfo) %in% fbshort)) stop("'varInfo' is differently named than the 'dataName' entries in 'chapters'.")

  if( length(missings)!=length(fbshort)) stop("'missings' and 'chapters' have different lengths.")
  if( ! all(names(missings) %in% fbshort)) stop("'missings' is differently named than the 'dataName' entries in 'chapters'.")

  if( length(struc)!=length(fbshort)) stop("'struc' and 'chapters' have different lengths.")
  if( ! all(names(struc) %in% fbshort)) stop("'struc' is differently named than the 'dataName' entries in 'chapters'.")

  if( ! all(names(register) %in% fbshort)) stop("'register' contains entries that are not in the 'dataName' entries in 'chapters'.")

  if( length(Kennwertedatensatz)!=length(fbshort)) stop("'Kennwertedatensatz' and 'chapters' have different lengths.")
  if( ! all(names(Kennwertedatensatz) %in% fbshort)) stop("'Kennwertedatensatz' is differently named than the 'dataName' entries in 'chapters'.")

  NULL
}


