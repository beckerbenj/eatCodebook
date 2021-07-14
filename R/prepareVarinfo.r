### Gruppenzuordnung fuer nicht-imputierte Variablen:
###     - Name der Skala wird rausgesucht
###     - alle Variablen, die mit derselben buchstabenkombination beginnen, werden als zugehoerige Items behandelt (= gehoeren zu einer Gruppe)
### enriches labels from GADSdat-object with plain information necessary for codebook generation

####
#############################################################################
#' Prepare variable information.
#'
#' Create a variable information data.frame.
#'
#'
#'@param GADSdat.obj Object of class GADSdat, created by \code{import_spss} from the \code{eatGADS} package, for example
#'@param varsToExclude tbd
#'@param impExpr Cat to console?
#'@param scaleExpr Cat to console?
#'@param varNameSeparatorImp tbd
#'@param lastOccurence tbd
#'@param groupSuffixImp tbd
#'@param verbose tbd
#'
#'@return Returns a \code{data.frame} with variable information.
#'
#'@examples
#'varInfo <- prepareVarinfo(eatGADS::pisa, impExpr = "Plausible Value")
#'
#'@export
prepareVarinfo <- function ( GADSdat.obj, varsToExclude = NULL, impExpr = c("IMPUTATION[[:digit:]]{1,2}$", "PV[[:digit:]]{1,2}"), scaleExpr = "^Skala", varNameSeparatorImp = "_", lastOccurrence =TRUE, groupSuffixImp = "imp", verbose = TRUE) {
       weg  <- setdiff(varsToExclude,GADSdat.obj[["labels"]][,"varName"])
       if ( length(weg)>0) { message("Following ",length(weg), " variable(s) which should be excluded do exists in GADSdat.obj: '",paste(weg, collapse="', '"), "'.")}
       vari <- GADSdat.obj[["labels"]][!duplicated(GADSdat.obj[["labels"]][,"varName"]),c("varName","varLabel")]
       if ( length( setdiff(varsToExclude, weg))>0) {
            vari <- vari[-match(setdiff(varsToExclude, weg), vari[,"varName"]),]
       }
       vari[,"imp"] <- FALSE
       for ( i in impExpr) {  vari[grep(i, vari[,"varLabel"]),"imp"] <- TRUE  }
       vari[,"type"] <- "variable"
       vari[grep(scaleExpr, vari[,"varLabel"]),"type"] <- "scale"
       vari <- do.call("rbind", by(data = vari, INDICES = vari[,"varName"], FUN = function ( z ) {
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
               z[,"scale"] <- scale
               return(z)}))
    ### Gruppenzuordnung vergeben, das geschieht fuer imputierte und nicht imputierte Variablen separat
       vari <- do.call("rbind", by(data = vari, INDICES = vari[,"imp"], FUN = function ( v ) {
               if ( isFALSE(v[1,"imp"]) ) {                                     ### hier begintn die Behandlung fuer nicht-imputierte Variablen
                    v[,"group"] <- NA                                           ### Gruppenzuhehoerigkeit initialisieren
                    scales<- v[which(v[,"type"] == "scale"),"varName"]          ### das sind die Namen der Skalen, zu jeder werden jetzt die Items herausgesucht
                    for ( sc in scales) {
                          items <- setdiff(v[which(substr(v[,"varName"],1,nchar(sc)) == sc),"varName"], sc)
                          if ( length(items)==0) {warning(paste0("Cannot found any non-imputed items for scale '",sc,"' (not imputed)."))}
                          if (verbose) {cat(paste0("Scale '",sc,"' (not imputed): Found following ",length(items)," items: '",paste(items, collapse="', '"),"'.\n"))}
                          v[eatTools::whereAre(c(items, sc), v[,"varName"], verbose=FALSE),"group"] <- sc
                    }
                    v[which(is.na(v[,"group"])),"group"] <- v[which(is.na(v[,"group"])),"varName"]
                    v[,"group"] <- paste(v[,"group"], "notImputed",sep="_")
               }  else  {                                                       ### hier beginnt die Behandlung fuer imputierte Variablen
                    v[,"group"] <- paste(eatTools::halveString(string = v[,"varName"], pattern = varNameSeparatorImp, first = !lastOccurrence)[,1], "imputed",sep="_")
               }
               return(v)}))
       return(vari)}

