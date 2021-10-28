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
# calculateDescriptives(gd, varinfo)


####
#############################################################################
#' Calculate descriptive statistics.
#'
#' Calculate descriptive statistics which should be included in the codebook.
#'
#'
#'@param GADSdat Object of class \code{GADSdat}, created for example by \code{import_spss} from the \code{eatGADS} package.
#'@param inputForDescriptives \code{data.frame} with variable information. This table can be created from GADSdat object, using the \code{createInputForDescriptives} function
#'@param verbose Print variable and function information to console?
#'@param showCallOnly Logical: only for diagnostics. If TRUE, no calculation is proceed, and only the function which is called for calculation is returned.
#'
#'@return Returns a list of descriptive statistics.
#'
#'@examples
#'# tbd
#'
#'@export
calculateDescriptives <- function( GADSdat, inputForDescriptives, verbose = TRUE, showCallOnly = FALSE) {
  UseMethod("calculateDescriptives")
}

#'@export
calculateDescriptives.list <- function( GADSdat, inputForDescriptives, verbose = TRUE, showCallOnly = FALSE) {
    ### Achtung! wenn mehrere GADSdat-Objekte als Liste uebergeben werden, koennen die weiteren Argumente ebenfalls als Liste uebergeben werden,
    ### oder man kann ein Argument fuer alle GADSdat-Objekte benutzen. welches von beiden hier der Fall ist, muss ermittelt werden
           #fwa    <- createFunNameWithArgs(funName = "calculateDescriptives")   ### 'fwa' = function with arguments
           fwa    <- as.list(match.call())
           argList<- list()                                                     ### list with arguments
           for ( i in names(fwa)[-1] ) {eval(parse(text = paste0("argList[[i]] <- ",i)))}
           loop   <- createAndExecuteFunctionCalls(funName = "calculateDescriptives", argList = argList)
           names(loop) <- names(GADSdat)
           return(loop)}

#'@export
calculateDescriptives.GADSdat <- function( GADSdat, inputForDescriptives, verbose = TRUE, showCallOnly = FALSE) {
### showCallOnly: nur zum checken, welche Funktion gecalled wird
### checks, dass inputForDescriptives korrekt spezifiziert ... es muss u.a. ein data.frame sein
  if ("tbl" %in% class(inputForDescriptives)) {
       if(verbose){message("'inputForDescriptives' has class '",paste(class(inputForDescriptives), collapse="', '"), "'. Transform 'inputForDescriptives' into 'data.frame'.")}
       inputForDescriptives <- as.data.frame(inputForDescriptives)
  }
  check_inputForDescriptives(inputForDescriptives)
### welche variablen werden ignoriert?
  vars <- c("type",  "scale", "imp")
  mis  <- lapply(vars, FUN = function ( v ) { c(which(is.na(inputForDescriptives[,v])), which(inputForDescriptives[,v] == "")) })
  anz  <- unlist(lapply(mis, length))
  mis  <- unique(unlist(mis))
  if ( any(anz>0) ) {
      if(verbose){message("Following variables will be ignored due to missing entries in 'type', 'scale' or 'imp' column of 'inputForDescriptives': '",paste(inputForDescriptives[mis,"varName"], collapse="', '"), "'")}
      inputForDescriptives <- inputForDescriptives[-mis,]
      if(nrow(inputForDescriptives)==0) {
         if(verbose){message("No valid entries in 'inputForDescriptives'.")}
         return(NULL)
      }
  }
  ret <- by(data = inputForDescriptives, INDICES = inputForDescriptives[,"group"], FUN = function (v) {varStats(GADSdat=GADSdat, sub.inputForDescriptives=v, verbose=verbose, showCallOnly=showCallOnly)})
  return(ret)
}


### showCallOnly: nur zum checken, welche Funktion gecalled wird
varStats <- function(GADSdat, sub.inputForDescriptives, verbose, showCallOnly = FALSE) {
### checks
  if ( isFALSE(showCallOnly) && !all(sub.inputForDescriptives[,"varName"] %in% colnames(GADSdat[["dat"]])) ) {
       if(verbose){message("Following variables from the 'inputForDescriptives' missed in GADSdat: '",paste(setdiff(sub.inputForDescriptives[,"varName"],colnames(GADSdat[["dat"]])), collapse="', '"), "'.\nSkip collecting variable statistics for '",sub.inputForDescriptives[1,"group"],"'.")}
       return(NULL)
  }
### Ausgabe des Variablennames auf der Konsole
  if(isFALSE(showCallOnly) && verbose) { cat ( paste0 ( "Compute variable statistics for '",sub.inputForDescriptives[1,"group"],"': ")); utils::flush.console()}

### Berechnung der Kennwerte
  if(nrow(sub.inputForDescriptives)>1) {
     if ( isTRUE(sub.inputForDescriptives[1,"imp"])) {
         if ( sub.inputForDescriptives[1,"scale"] == "numeric") {
             if ( isTRUE(showCallOnly) ) {return("kennwerte.gepoolt.metrisch")}
             if ( verbose) {cat("Use function 'kennwerte.gepoolt.metrisch'.\n")}
             stats <- kennwerte.gepoolt.metrisch(datWide=GADSdat[["dat"]], imputedVariableCols = sub.inputForDescriptives[,"varName"])
         }  else  {
             if ( isTRUE(showCallOnly) ) {return("kennwerte.gepoolt.kategorial")}
             if ( verbose) {cat("Use function 'kennwerte.gepoolt.kategorial'.\n")}
             stats <- kennwerte.gepoolt.kategorial(datWide=GADSdat[["dat"]], imputedVariableCols = sub.inputForDescriptives[,"varName"], verbose=verbose)
         }
     }  else  {
### differenzieren, ob es skala (es gibt eine separate skalenvariale) oder fake.skala (es gibt keine separate skalenvariale) ist
         if ( "scale" %in% sub.inputForDescriptives[,"type"] ) {
             if(length(which("scale" == sub.inputForDescriptives[,"type"])) != 1) {cat("Error: Activate browser.\n"); browser()}
             if ( isTRUE(showCallOnly) ) {return("kennwerte.skala")}
             if ( verbose) {cat("Use function 'kennwerte.skala'.\n")}
             stats <- kennwerte.skala (GADSdat=GADSdat,sub.inputForDescriptives=sub.inputForDescriptives, verbose=verbose)
         }  else  {
             if ( isTRUE(showCallOnly) ) {return("kennwerte.skala.fake")}
             if ( verbose) {cat("Use function 'kennwerte.skala.fake'.\n")}
             stats <- kennwerte.skala.fake (dat=GADSdat[["dat"]],variableCols=sub.inputForDescriptives[,"varName"], missingValues = NULL)
         }
     }
  }  else  {
     if (sub.inputForDescriptives[,"scale"] == "nominal") {
         if ( isTRUE(showCallOnly) ) {return("kennwerte.kategorial")}
         if ( verbose) {cat("Use function 'kennwerte.kategorial'.\n")}
         stats <- kennwerte.kategorial(x=GADSdat[["dat"]][,sub.inputForDescriptives[,"varName"]], value_table = GADSdat[["labels"]][which(GADSdat[["labels"]][,"varName"] == sub.inputForDescriptives[,"varName"]),])
     }
     if (sub.inputForDescriptives[,"scale"] == "numeric") {
         if ( isTRUE(showCallOnly) ) {return("kennwerte.metrisch")}
         if ( verbose) {cat("Use function 'kennwerte.metrisch'.\n")}
         stats <- kennwerte.metrisch(x=GADSdat[["dat"]][,sub.inputForDescriptives[,"varName"]], value_table = GADSdat[["labels"]][which(GADSdat[["labels"]][,"varName"] == sub.inputForDescriptives[,"varName"]),])
     }
     if (sub.inputForDescriptives[,"scale"] == "ordinal") {
         if ( isTRUE(showCallOnly) ) {return("kennwerte.ordinal")}
         if ( verbose) {cat("Use function 'kennwerte.ordinal'.\n")}
         stats <- kennwerte.ordinal(x=GADSdat[["dat"]][,sub.inputForDescriptives[,"varName"]], value_table = GADSdat[["labels"]][which(GADSdat[["labels"]][,"varName"] == sub.inputForDescriptives[,"varName"]),])
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


