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


####
#############################################################################
#' Calculate descriptive statistics.
#'
#' Calculate descriptive statistics which should be included in the codebook.
#'
#'
#'@param GADSdat.obj Object of class GADSdat, created by \code{import_spss} from the \code{eatGADS} package, for example
#'@param varinfo \code{data.frame} with variable information
#'@param verbose Cat to console?
#'@param showCallOnly tbd
#'
#'@return Returns a list of descriptive statistics.
#'
#'@examples
#'# tbd
#'
#'@export

### showCallOnly: nur zum checken, welche Funktion gecalled wird
cds <- function( GADSdat.obj, varinfo, verbose = TRUE, showCallOnly = FALSE) {
### checks, dass varinfo korrekt spezifiziert
  fehlend <- setdiff (c( "varName",   "group", "type",  "scale", "imp"), colnames(varinfo))
  if ( length(fehlend)>0) { stop("Column(s) '",paste(fehlend, collapse="', '"), "' missed in 'varinfo'.")}
  if(!length(unique(varinfo[,"varName"])) == length(varinfo[,"varName"])) {stop("'varName' column in 'varinfo' must be unique.")}
  not_allowed1 <- setdiff(na.omit(varinfo[,"type"]), c("variable", "scale", ""))
  if ( length(not_allowed1)>0) { stop("Invalid entries in 'type' column of 'varinfo': '",paste(not_allowed1, collapse="', '"), "'")}
  not_allowed2 <- setdiff(na.omit(varinfo[,"scale"]), c("nominal", "ordinal", "numeric", "") )
  if ( length(not_allowed2)>0) { stop("Invalid entries in 'scale' column of 'varinfo': '",paste(not_allowed2, collapse="', '"), "'")}
  not_allowed3 <- setdiff (na.omit(varinfo[,"imp"]), c("FALSE", "TRUE", "") )
  if ( length(not_allowed3)>0) { stop("'imp' column in 'varinfo' must only contain 'FALSE' or 'TRUE'")}
### welche variablen werden ignoriert?
  vars <- c("type",  "scale", "imp")
  mis  <- lapply(vars, FUN = function ( v ) { c(which(is.na(varinfo[,v])), which(varinfo[,v] == "")) })
  anz  <- unlist(lapply(mis, length))
  mis  <- unique(unlist(mis))
  if ( any(anz>0) ) {
      message("Following variables will be ignored due to missing entries in 'type', 'scale' or 'imp' column of 'varinfo': '",paste(varinfo[mis,"varName"], collapse="', '"), "'")
      varinfo <- varinfo[-mis,]
      if(nrow(varinfo)==0) {
         message("No valid entries in 'varinfo'.")
         return(NULL)
      }
  }
  ret <- by(data = varinfo, INDICES = varinfo[,"group"], FUN = function (v) {varStats(GADSdat.obj=GADSdat.obj, sub.varinfo=v, verbose=verbose, showCallOnly=showCallOnly)})
  return(ret)
}


### showCallOnly: nur zum checken, welche Funktion gecalled wird
varStats <- function(GADSdat.obj, sub.varinfo, verbose, showCallOnly = FALSE) {
### checks
  if ( isFALSE(showCallOnly) && !all(sub.varinfo[,"varName"] %in% colnames(GADSdat.obj[["dat"]])) ) {
       message("Following variables from the 'varinfo' missed in GADSdat.obj: '",paste(setdiff(sub.varinfo[,"varName"],colnames(GADSdat.obj[["dat"]])), collapse="', '"), "'.\nSkip collecting variable statistics for '",sub.varinfo[1,"group"],"'.")
       return(NULL)
  }
### Ausgabe des Variablennames auf der Konsole
  if(isFALSE(showCallOnly) && verbose) { cat ( paste0 ( "Compute variable statistics for '",sub.varinfo[1,"group"],"': ")); flush.console()}

### Berechnung der Kennwerte
  if(nrow(sub.varinfo)>1) {
     if ( isTRUE(sub.varinfo[1,"imp"])) {
         if ( sub.varinfo[1,"scale"] == "numeric") {
             if ( isTRUE(showCallOnly) ) {return("kennwerte.gepoolt.metrisch")}
             if ( verbose) {cat("Use function 'kennwerte.gepoolt.metrisch'.\n")}
             stats <- kennwerte.gepoolt.metrisch(datWide=GADSdat.obj[["dat"]], imputedVariableCols = sub.varinfo[,"varName"])
         }  else  {
             if ( isTRUE(showCallOnly) ) {return("kennwerte.gepoolt.kategorial")}
             if ( verbose) {cat("Use function 'kennwerte.gepoolt.kategorial'.\n")}
             stats <- kennwerte.gepoolt.kategorial(datWide=GADSdat.obj[["dat"]], imputedVariableCols = sub.varinfo[,"varName"])
         }
     }  else  {
### differenzieren, ob es skala (es gibt eine separate skalenvariale) oder fake.skala (es gibt keine separate skalenvariale) ist
         if ( "scale" %in% sub.varinfo[,"type"] ) {
             if(length(which("scale" == sub.varinfo[,"type"])) != 1) {cat("Error: Activate browser.\n"); browser()}
             if ( isTRUE(showCallOnly) ) {return("kennwerte.skala")}
             if ( verbose) {cat("Use function 'kennwerte.skala'.\n")}
             stats <- kennwerte.skala (GADSdat.obj=GADSdat.obj,sub.varinfo=sub.varinfo)
         }  else  {
             if ( isTRUE(showCallOnly) ) {return("kennwerte.skala.fake")}
             if ( verbose) {cat("Use function 'kennwerte.skala.fake'.\n")}
             stats <- kennwerte.skala.fake (dat=GADSdat.obj[["dat"]],variableCols=sub.varinfo[,"varName"], missingValues = NULL)
         }
     }
  }  else  {
     if (sub.varinfo[,"scale"] == "nominal") {
         if ( isTRUE(showCallOnly) ) {return("kennwerte.kategorial")}
         if ( verbose) {cat("Use function 'kennwerte.kategorial'.\n")}
         stats <- kennwerte.kategorial(x=GADSdat.obj[["dat"]][,sub.varinfo[,"varName"]], value_table = GADSdat.obj[["labels"]][which(GADSdat.obj[["labels"]][,"varName"] == sub.varinfo[,"varName"]),])
     }
     if (sub.varinfo[,"scale"] == "numeric") {
         if ( isTRUE(showCallOnly) ) {return("kennwerte.metrisch")}
         if ( verbose) {cat("Use function 'kennwerte.metrisch'.\n")}
         stats <- kennwerte.metrisch(x=GADSdat.obj[["dat"]][,sub.varinfo[,"varName"]], value_table = GADSdat.obj[["labels"]][which(GADSdat.obj[["labels"]][,"varName"] == sub.varinfo[,"varName"]),])
     }
     if (sub.varinfo[,"scale"] == "ordinal") {
         if ( isTRUE(showCallOnly) ) {return("kennwerte.ordinal")}
         if ( verbose) {cat("Use function 'kennwerte.ordinal'.\n")}
         stats <- kennwerte.ordinal(x=GADSdat.obj[["dat"]][,sub.varinfo[,"varName"]], value_table = GADSdat.obj[["labels"]][which(GADSdat.obj[["labels"]][,"varName"] == sub.varinfo[,"varName"]),])
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


