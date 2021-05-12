# englische namen, keine punkte in Variablennamen
# spss-Datensatz nehmen, daraus gads-dat objekt erzeugen,
#   importSPSS nehmen
#   kleine spss-Datei, wo aber alles drin ist, kategorial, metrisch, wenige zeilen reichen (vielleicht 10)
#   zusaetzliche tabelle mit zwei spalten, die angibt, welche imputationen zu welcher gemeinsamen variablen gehoeren
# welche infos braucht kds.erstellen, die nicht im gads-.dat objekt enthalten sind
# man braucht zusaetzliche info, welche imputationen zu einer gemeinsamen variable gehoeren (momentan: skalen.info)
#
# check:
# library(eatGADS); gd <- import_spss("c:/Diskdrv/Winword/Psycho/IQB/Repositories/eatCodebook/inst/extdata/example1.sav")
# load("c:/Diskdrv/Winword/Psycho/IQB/Repositories/eatCodebook/inst/extdata/example1_varinfo.rda")
# cds(gd, varinfo)


#### Apply Change Meta data
#############################################################################
#' Calculate descriptive statistics.
#'
#' Calculate descriptive statistics which should be included in the codebook.
#'
#'
#'@param GADSdat.obj Object of class GADSdat, created by import_spss from the eatGADS package, for example
#'@param varinfo data frame with variable information
#'
#'@return Returns a list of descriptive statistics.
#'
#'@examples
#'# tbd
#'
#'@export
cds <- function( GADSdat.obj, varinfo, verbose = TRUE) {
### checks, dass varinfo korrekt spezifiziert
  fehlend <- setdiff (c( "var",   "group", "type",  "scale", "imp"), colnames(varinfo))
  if ( length(fehlend)>0) { stop("Column(s) '",paste(fehlend, collapse="', '"), "' missed in 'varinfo'.")}
  if(!length(unique(varinfo[,"var"])) == length(varinfo[,"var"])) {stop("'var' column in 'varinfo' must be unique.")}
  not_allowed1 <- setdiff(c("variable", "scale"), varinfo[,"type"])
  if ( length(not_allowed1)>0) { stop("Invalid entries in 'type' column of 'varinfo': '",paste(not_allowed1, collapse="', '"), "'")}
  not_allowed2 <- setdiff(c("nominal", "ordinal", "numeric"), varinfo[,"scale"])
  if ( length(not_allowed2)>0) { stop("Invalid entries in 'scale' column of 'varinfo': '",paste(not_allowed2, collapse="', '"), "'")}
  not_allowed3 <- setdiff (c("FALSE", "TRUE"), varinfo[,"imp"])
  if ( length(not_allowed3)>0) { stop("'imp' column in 'varinfo' must only contain 'FALSE' or 'TRUE'")}
### welche variablen werden ignoriert?
  vars <- c("type",  "scale", "imp")
  mis  <- lapply(vars, FUN = function ( v ) { which(is.na(varinfo[,v]))})
  anz  <- unlist(lapply(mis, length))
  mis  <- unique(unlist(mis))
  if ( any(anz>0) ) {
      message("Following variables will be ignored due to missing entries in 'type', 'scale' or 'imp' column of 'varinfo': '",paste(varinfo[mis,"var"]), "'")
      varinfo <- varinfo[-mis,]
      if(nrow(varinfo)==0) {
         message("No valid entries in 'varinfo'.")
         return(NULL)
      }
  }
  ret <- by(data = varinfo, INDICES = varinfo[,"group"], FUN = function (v) {varStats(GADSdat.obj, v, verbose)})
  return(ret)
}


varStats <- function(GADSdat.obj, sub.varinfo, verbose) {
  dat <- eatGADS::extractData(GADSdat.obj, convertLabels ="numeric")
### checks
  if ( !all(sub.varinfo[,"var"] %in% colnames(dat)) ) {
       message("Following variables from the 'varinfo' missed in GADSdat.obj: '",paste(setdiff(sub.varinfo[,"var"],colnames(dat)), collapse="', '"), "'.\nSkip collecting variable statistics for '",sub.varinfo[1,"group"],"'.")
       return(NULL)
  }
### Ausgabe des Variablennames auf der Konsole
  if(verbose) { cat ( paste0 ( "Compute variable statistics for '",sub.varinfo[1,"group"],"'.\n")); flush.console()}

### Berechnung der Kennwerte
  if(nrow(sub.varinfo)>1) {
     if ( isTRUE(sub.varinfo[1,"imp"])) {
         if ( sub.varinfo[1,"scale"] == "numeric") {
             stats <- kennwerte.gepoolt.metrisch(datWide=dat, imputedVariableCols = sub.varinfo[,"var"])
         }  else  {
             stats <- kennwerte.gepoolt.kategorial(datWide=dat, imputedVariableCols = sub.varinfo[,"var"])
         }
     }  else  {
### differenzieren, ob es skala (es gibt eine separate skalenvariale) oder fake.skala (es gibt keine separate skalenvariale) ist
         if ( "scale" %in% sub.varinfo[,"type"] ) {
             stopifnot(length(which("scale" == sub.varinfo[,"type"])) == 1)
             stats <- kennwerte.skala (dat=dat,scaleCol=sub.varinfo[which(sub.varinfo[,"type"] == "scale"),"var"], variableCols=sub.varinfo[which(sub.varinfo[,"type"] != "scale"),"var"], missingValues = NULL)
         }  else  {
             stats <- kennwerte.skala.fake (dat=dat,variableCols=sub.varinfo[,"var"], missingValues = NULL)
         }
     }
  }  else  {
     if (sub.varinfo[,"scale"] == "nominal") {
         stats <- kennwerte.kategorial(x=dat[,sub.varinfo[,"var"]], value_table = GADSdat.obj[["labels"]][which(GADSdat.obj[["labels"]][,"varName"] == sub.varinfo[,"var"]),])
     }
     if (sub.varinfo[,"scale"] == "numeric") {
         stats <- kennwerte.metrisch(x=dat[,sub.varinfo[,"var"]], value_table = GADSdat.obj[["labels"]][which(GADSdat.obj[["labels"]][,"varName"] == sub.varinfo[,"var"]),])
     }
     if (sub.varinfo[,"scale"] == "ordinal") {
         stats <- kennwerte.ordinal(x=dat[,sub.varinfo[,"var"]], value_table = GADSdat.obj[["labels"]][which(GADSdat.obj[["labels"]][,"varName"] == sub.varinfo[,"var"]),])
     }
  }

#  if( all( varue.missings[varue.missings$Var.name %in% name, "missing" ] %in% "ja" ) & i==2 ) {
#    kennwerte.var <- kennwerte.kategorial.variation(name=name, varue.missings=varue.missings , Gesamtdatensatz=Gesamtdatensatz)
#  } else if (i %in% c(0,1,8)) {
#    kennwerte.var <- NULL
#  } else if (i==2) {
#    kennwerte.var <-  kennwerte.kategorial(name=name,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz)
#  } else if (i==3) {
#    kennwerte.var <-  kennwerte.ordinal(name=name,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz)
#  } else if (i==4) {
#    kennwerte.var <- kennwerte.metrisch(name=name,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz)
#  } else if (i==5) {
#    kennwerte.var <- kennwerte.skala(name=name,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info)
#  } else if (i==6) {
#    kennwerte.var <- kennwerte.gepoolt.metrisch(name=name,id.fb=id.fb,Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info)
#  } else if (i==7) {
#    kennwerte.var <- kennwerte.gepoolt.kategorial(name=name,id.fb=id.fb,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info)
#  } else if (i==9) {
#    kennwerte.var <- kennwerte.skala.fake(name=name,varue.info=varue.info,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info)
#  }

  #### Output ####
#  kennwerte.var
return(stats)}


