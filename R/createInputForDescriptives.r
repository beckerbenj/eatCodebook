### Gruppenzuordnung fuer nicht-imputierte Variablen:
###     - Name der Skala wird rausgesucht
###     - alle Variablen, die mit derselben buchstabenkombination beginnen, werden als zugehoerige Items behandelt (= gehoeren zu einer Gruppe)
### enriches labels from GADSdat-object with plain information necessary for codebook generation

####
#############################################################################
#' Create input data.frame for subsequent calculation of descriptives
#'
#' Create a variable information data.frame from the GADSdat object. This input can be used to calculate the descriptives of the data via the \code{calculateDescriptives} function
#'
#'
#'@param GADSdat.obj Object of class \code{GADSdat}, created by \code{import_spss} from the \code{eatGADS} package, for example. Alternatively, a list of objects of class \code{GADSdat}
#'@param idExpr Regular expression to identify ID variables from variable names (Note: for multiple expressions, i.e. if \code{idExpr} is a character vector of length > 1, at least one expression should match to identify the variable as ID variable)
#'@param impExpr Regular expression to identify imputed variables from variable labels in GADSdat object (Note: for multiple expressions, i.e. if \code{impExpr} is a character vector of length > 1, at least one expression should match to identify the variable as an imputed variable)
#'@param scaleExpr Regular expression to identify scale variables from variable labels in GADSdat object (Note: for multiple expressions, i.e. if \code{scaleExpr} is a character vector of length > 1, at least one expression should match to identify the variable as a scale variable)
#'@param varNameSeparatorImp character sign to separate the "pooled" suffix from group name in group column
#'@param lastOccurrence Logical: If varNameSeparatorImp occurrs multiple times within a string, lastOccurrence defines whether the last occurrence should be used for splitting
#'@param groupSuffixImp tbd
#'@param nCatsForOrdinal Numeric vector with number of categories considered for ordinal variables. Variables with number of categories as defined here are considered to be ordinal instead of nominal. If NULL, this rule will be ignored, and nominal/ordinal assignment is done in other ways
#'@param verbose tbd
#'
#'@return Returns a \code{data.frame} with variable information with following columns
#'\itemize{
#'  \item \code{varName} The name of the variable as it occurs in the data
#'  \item \code{varLabel} The label of the variable as it occurs in the \code{GADSdat} label sheet
#'  \item \code{format} The variable format as displayed in the labels sheet of the \code{GADSdat} object
#'  \item \code{imp} Logical: Whether or not the variable is imputed
#'  \item \code{type} The type of the variable. Two possible entries, \code{variable} or \code{scale}
#'  \item \code{scale} The scale level of the variable. Possible entries: \code{nominal}, \code{ordinal}, \code{numeric}. ID variables and character variables have missing entries in this column. Be cautious that 'ordinal' sometimes may be allocated erroneously. The resulting table should be exported to Excel for further checks.
#'  \item \code{group} If the variable is part of a scale with several items, a common entry in the group column indicates that these variables belong together
#'}
#'
#'@examples
#'varInfo <- createInputForDescriptives(eatGADS::pisa, impExpr = "Plausible Value")
#'
#'@export
createInputForDescriptives <- function ( GADSdat.obj, idExpr = "^ID", impExpr = c("IMPUTATION[[:digit:]]{1,2}$", "PV[[:digit:]]{1,2}"), scaleExpr = "^Skala", varNameSeparatorImp = "_", lastOccurrence =TRUE, groupSuffixImp = "imp", nCatsForOrdinal = c(2:5), verbose = TRUE) {
  UseMethod("createInputForDescriptives")
}
#'@export
createInputForDescriptives.GADSdat <- function ( GADSdat.obj, idExpr = "^ID", impExpr = c("IMPUTATION[[:digit:]]{1,2}$", "PV[[:digit:]]{1,2}"), scaleExpr = "^Skala", varNameSeparatorImp = "_", lastOccurrence =TRUE, groupSuffixImp = "imp", nCatsForOrdinal = c(2:5), verbose = TRUE) {
           vari <- GADSdat.obj[["labels"]][!duplicated(GADSdat.obj[["labels"]][,"varName"]),c("varName","varLabel", "format")]
           vari[,"imp"] <- FALSE
    ### imp-Eintrag vergeben
           for ( i in impExpr) {  vari[grep(i, vari[,"varLabel"]),"imp"] <- TRUE  }
           vari[,"type"] <- "variable"
           vari[grep(scaleExpr, vari[,"varLabel"]),"type"] <- "scale"
    ### scale-Eintrag vergeben
           vari[,"laufnummer"] <- 1:nrow(vari)                                  ### dieses, damit die Reihenfolge der varinfo so ist wie im labels sheet
           vari <- do.call("rbind", by(data = vari, INDICES = vari[,"laufnummer"], FUN = function ( z ) {
    ### wenn Variable als ID variable identifiziert wird, soll scale-Eintrag leer sein
                   if ( length(unlist(lapply(idExpr, FUN = function (ie) {grep(ie, z[["varName"]])})))>0 ) {
                        message(paste0("Variable '",z[["varName"]],"' matches ID variable definition (as defined in 'idExpr') and will be handled as ID variable."))
                        z[,"scale"] <- NA
                        return(z)
                   }
    ### wenn Variable im GADSdat-Labelsfile ein "A" in der Format-Spalte hat, bedeutet das "character". Es soll ein leerer Eintrag in der "scale"-Spalte eingetragen werden
                   if(toupper(substr(z[["format"]],1,1)) == "A") {
                        z[,"scale"] <- NA
                        return(z)
                   }
                   if ( class(GADSdat.obj[["dat"]][,z[["varName"]]]) == "character") {scale <- "nominal"}
                   if ( class(GADSdat.obj[["dat"]][,z[["varName"]]]) == "numeric") {
                        mis    <- GADSdat.obj[["labels"]][which(GADSdat.obj[["labels"]][,"varName"] == z[["varName"]]),]
                        mis    <- mis[which(mis[,"missings"] == "miss"),"value"]
                        nonmis <- sort(setdiff(unique(GADSdat.obj[["dat"]][,z[["varName"]]]), mis))
                        if ( any(is.na(as.integer(nonmis))) ) {
                             warning(paste0("Variable '",z[["varName"]],"': Missing values in sorted integer entries found. This should only occur for pseudo-numeric values, i.e. id variables."))
                             scale <- NA
                        }  else  {
                            if ( !all(nonmis == as.integer(nonmis)) ) {
                                 scale <- "numeric"
                            }  else  {
                                 vgl   <- min(nonmis) : max(nonmis)
                                 if ( length(vgl) == length(nonmis) && all(nonmis == (min(nonmis) : max(nonmis)) ) ) {
                                      scale <- "ordinal"
                                 }  else {
                                      scale <- "nominal"
                                 }
                            }
                        }
                   }
    ### check No. 1 und ggf. korrektur der 'scale'-Zuweisung
                   if (scale != "numeric") {
                        digit <- unlist(strsplit(z[["format"]], "\\."))
                        digit <- suppressWarnings(eatTools::asNumericIfPossible(digit[length(digit)], force.string=FALSE))
                        krit1 <- is.numeric(digit) && digit>0
                        if(substr(z[["format"]],1,1) == "F" && isTRUE(krit1)) {
                            message(paste0("Variable '",z[["varName"]],"' has identified scale '",scale,"' but is expected to be 'numeric' due to format definition '",z[["format"]],"' in GADSdat labels sheet. Transform '",z[["varName"]],"' to be numeric."))
                            scale <- "numeric"
                        }
    ### wenn das erste Kriterium fuer numerisch nicht erfuellt wurde, soll hier das zweite geprueft werden: wenn eine variable nur missings als definierte labels hat, dann soll sie numerisch sein
                        if (scale != "numeric") {
                            if ( all(na.omit(GADSdat.obj[["labels"]][which(GADSdat.obj[["labels"]][,"varName"] == z[["varName"]]),"missings"]) == "miss")) {
                                 message(paste0("Variable '",z[["varName"]],"' has only missings as defined labels in the labels sheet of the GADSdat object. hence, '",z[["varName"]],"' is expected to be numeric. Change 'scale' value from '",scale,"' to 'numeric'."))
                                 scale <- "numeric"
                            }
                        }
                   }
    ### check No. 2 und ggf. korrektur der 'scale'-Zuweisung
                   if (scale == "ordinal") {
                        if(!is.null(nCatsForOrdinal)) {
                             if ( !length(unique(nonmis)) %in% nCatsForOrdinal) {
                                 message(paste0("Variable '",z[["varName"]],"' has ",length(unique(nonmis))," non-missing categories. This is outside of the range defined in the 'nCatsForOrdinal' argument. Hence, '",z[["varName"]],"' will be transformed from '",scale,"' to 'nominal'."))
                                 scale <- "nominal"
                             }
                        }
                   }
                   z[,"scale"] <- scale
                   return(z)}))
    ### group-Eintrag (Gruppenzuordnung) vergeben, das geschieht fuer imputierte und nicht imputierte Variablen separat
           vari <- do.call("rbind", by(data = vari, INDICES = vari[,"imp"], FUN = function ( v ) {
                   if ( isFALSE(v[1,"imp"]) ) {                                 ### hier beginnt die Behandlung fuer nicht-imputierte Variablen
                        v[,"group"] <- NA                                       ### Gruppenzuhehoerigkeit initialisieren
                        scales<- v[which(v[,"type"] == "scale"),"varName"]      ### das sind die Namen der Skalen, zu jeder werden jetzt die Items herausgesucht
                        for ( sc in scales) {
                              items <- setdiff(v[which(substr(v[,"varName"],1,nchar(sc)) == sc),"varName"], sc)
                              if ( length(items)==0) {warning(paste0("Cannot found any non-imputed items for scale '",sc,"' (not imputed)."))}
                              if (verbose) {cat(paste0("Scale '",sc,"' (not imputed): Found following ",length(items)," items: '",paste(items, collapse="', '"),"'.\n"))}
                              v[eatTools::whereAre(c(items, sc), v[,"varName"], verbose=FALSE),"group"] <- sc
                        }
                        v[which(is.na(v[,"group"])),"group"] <- v[which(is.na(v[,"group"])),"varName"]
                   }  else  {                                                   ### hier beginnt die Behandlung fuer imputierte Variablen
                        v[,"group"] <- paste(eatTools::halveString(string = v[,"varName"], pattern = varNameSeparatorImp, first = !lastOccurrence)[,1], "pooled",sep="_")
                   }
                   return(v)}))
    ### nach Laufnummer sortieren und dann die Spalte entfernen
           vari <- data.frame(vari[sort(vari[,"laufnummer"],decreasing=FALSE,index.return=TRUE)$ix,-match("laufnummer", colnames(vari))])
           return(vari)}
           
#'@export
createInputForDescriptives.list <- function ( GADSdat.obj, idExpr = "^ID", impExpr = c("IMPUTATION[[:digit:]]{1,2}$", "PV[[:digit:]]{1,2}"), scaleExpr = "^Skala", varNameSeparatorImp = "_", lastOccurrence =TRUE, groupSuffixImp = "imp", nCatsForOrdinal = c(2:5), verbose = TRUE) {
  lapply(GADSdat.obj, function(x) {
    createInputForDescriptives(x, idExpr = idExpr, impExpr = impExpr, scaleExpr = scaleExpr, varNameSeparatorImp = varNameSeparatorImp, lastOccurrence =lastOccurrence, groupSuffixImp = groupSuffixImp, nCatsForOrdinal = nCatsForOrdinal, verbose = verbose)
  })
}

