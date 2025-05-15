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
#############################################################################
#' Calculate descriptive statistics.
#'
#' Calculate descriptive statistics which should be included in the codebook.
#'
#'
#'@param GADSdat Object of class \code{GADSdat}, created for example by \code{import_spss} from the \code{eatGADS} package.
#'@param inputForDescriptives \code{data.frame} with variable information. This table can either be created manually in
#'Excel or generated from GADSdat object, using the function \code{createInputForDescriptives}. If you need a template
#'of what this table should look like for different variables, you can use the \code{varInfo} object created in the examples below.
#'Specifically, the \code{data.frame} must contain the following columns.
#'\itemize{
#'    \item \code{varName}: This character column should contain all variable names that are part of the GADS database.
#'    \item \code{varLabel}: The variable label of the corresponding variable in the \code{varName} column
#'    \item \code{format}: This character columns yields the format of the corresponding variable in the \code{varName} column, for example \code{F8.0}
#'    \item \code{imp}: Logical column (i.e., \code{TRUE/FALSE}) which indicates whether the variable is imputed
#'    \item \code{type}: Character column with possible entries \code{variable}, \code{item}, \code{fake_item} or \code{scale}.
#'The entry \code{variable} should be used for individual variables in the \code{GADSdat} database that belong neither to a scale nor to a fake scale.
#'The entry \code{item} should be used for items which belong to a scale. No distinction needs to be made as to which scale
#'these items belong to.  It is only important that the database contains one or more scales for each of these items.
#'Assume that the database contains the scales "self concept" and "interest", where "self concept" is measured with the
#'items \code{SK_I1}, \code{SK_I2}, \code{SK_I3}, and "interest" is measured with the items \code{Int_I1}, \code{Int_I2},
#'\code{Int_I3}, \code{Int_I4}. Then \code{SK_I1}, \code{SK_I2}, \code{SK_I3}, \code{Int_I1}, \code{Int_I2},
#'\code{Int_I3}, and \code{Int_I4} should occur in the \code{varName} column, and the corresponding entry in \code{type}
#'should be \code{item}. Note that the database also contains the scale variables of "self concept" and "interest".
#'The entry \code{fake_item} should be used for items that theoretically constitute one or more latent scales, but for
#'which no scale variable is contained in the database.
#'    \item \code{scale}: Character column with the scale level of the corresponding variable. Possible entries
#'are \code{numeric} for metric variables, \code{ordinal} for categorical (factor) variables, or \code{nominal}.
#'    \item \code{group}: Character column which is only relevant for items, scales or fake scale variables.
#'For all others, only the variable name is transferred here. For the other variables, an indicator is defined
#'here that contains the assignment of items to their scales. Assume that the database contains the
#'scale "self concept" which is measured with the items \code{SK_I1}, \code{SK_I2}, \code{SK_I3}. Let the scale
#'variable of "self concept" be called \code{SC}. A common entry for \code{SK_I1}, \code{SK_I2},
#'\code{SK_I3}, and \code{scaleSelfConcept} should be entered in the \code{group} column, for example \code{"scaleSelfConcept"}}
#'@param verbose Print variable and function information to console?
#'@param showCallOnly Logical: only for diagnostics. If TRUE, no calculation is proceed, and only the function which is called for calculation is returned.
#'
#'@return Returns a list of descriptive statistics. This format may seem unsuitable for direct further
#'processing in R, but is primarily used for the automatic creation of codebooks using Latex.
#'
#'@examples
#'# import spss exemplary data
#'file <- system.file("tests/testthat", "helper_clean2.sav", package = "eatCodebook")
#'dat   <- eatGADS::import_spss(file)
#'# create variable information by the eatCodebbok function createInputForDescriptives
#'# This table 'varInfo' can be exported to Excel for further inspection and used as a
#'blueprint of what the necessary 'inputForDescriptives' argument for the
#'calculateDescriptives() function should look like
#'varInfo <- createInputForDescriptives(dat, impExpr = "plausible value")
#'# calculate descriptives
#'descr <- calculateDescriptives(dat, varInfo)
#'
#'@export
calculateDescriptives <- function( GADSdat, inputForDescriptives, verbose = FALSE, showCallOnly = FALSE) {
  UseMethod("calculateDescriptives")
}

#'@export
calculateDescriptives.list <- function( GADSdat, inputForDescriptives, verbose = FALSE, showCallOnly = FALSE) {
    ### Achtung! wenn mehrere GADSdat-Objekte als Liste uebergeben werden, koennen die weiteren Argumente ebenfalls als Liste uebergeben werden,
    ### oder man kann ein Argument fuer alle GADSdat-Objekte benutzen. welches von beiden hier der Fall ist, muss ermittelt werden
           #fwa    <- createFunNameWithArgs(funName = "calculateDescriptives")   ### 'fwa' = function with arguments
           fwa    <- as.list(match.call())
           argList<- list()                                                     ### list with arguments
           for ( i in names(fwa)[-1] ) {eval(parse(text = paste0("argList[[i]] <- ",i)))}
           loop   <- createAndExecuteFunctionCalls(funName = "calculateDescriptives", argList = argList)
           return(loop)}

#'@export
calculateDescriptives.GADSdat <- function( GADSdat, inputForDescriptives, verbose = FALSE, showCallOnly = FALSE) {
### showCallOnly: nur zum checken, welche Funktion gecalled wird
### checks, dass inputForDescriptives korrekt spezifiziert ... es muss u.a. ein data.frame sein
  inputForDescriptives <- eatTools::makeDataFrame(inputForDescriptives)
  check_inputForDescriptives(inputForDescriptives)
### welche variablen werden ignoriert?
  vars <- c("type",  "scale", "imp")
  mis  <- lapply(vars, FUN = function ( v ) { c(which(is.na(inputForDescriptives[,v])), which(inputForDescriptives[,v] == "")) })
  anz  <- unlist(lapply(mis, length))
  mis  <- unique(unlist(mis))
  if ( any(anz>0) ) {
      if(verbose){message("Following variables will be ignored due to missing entries in 'type', 'scale' or 'imp' column of 'inputForDescriptives': '",
                          paste(inputForDescriptives[mis,"varName"], collapse="', '"), "'")}
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
#         if ( "scale" %in% sub.inputForDescriptives[,"type"] ) {
             if(length(which("scale" == sub.inputForDescriptives[,"type"])) > 1) {cat("Error: Activate browser.\n"); browser()}
             if ( isTRUE(showCallOnly) ) {return("kennwerte.skala")}
             if ( verbose) {cat("Use function 'kennwerte.skala'.\n")}
             stats <- kennwerte.skala (GADSdat=GADSdat,sub.inputForDescriptives=sub.inputForDescriptives, verbose=verbose)
#         }  else  {
#             if ( isTRUE(showCallOnly) ) {return("kennwerte.skala.fake")}
#             if ( verbose) {cat("Use function 'kennwerte.skala.fake'.\n")}
#             stats <- kennwerte.skala.fake (dat=GADSdat[["dat"]],variableCols=sub.inputForDescriptives[,"varName"], missingValues = NULL)
#         }
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

#### Output ####
#  kennwerte.var
return(stats)}


