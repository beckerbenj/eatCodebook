### Gruppenzuordnung fuer nicht-imputierte Variablen:
###     - Name der Skala wird rausgesucht
###     - alle Variablen, die mit derselben buchstabenkombination beginnen, werden als zugehoerige Items behandelt (= gehoeren zu einer Gruppe)
### enriches labels from GADSdat-object with plain information necessary for codebook generation

####
#############################################################################
#' Create input data.frame for subsequent calculation of descriptives
#'
#' Create a variable information data.frame from the GADSdat object. This input can be used
#'to calculate the descriptives of the data via the \code{calculateDescriptives} function.
#'See the details section for further information.
#'
#'
#'@param GADSdat Object of class \code{GADSdat}, created by \code{import_spss} from the \code{eatGADS}
#'package, for example. Alternatively, a list of objects of class \code{GADSdat}
#'@param idExpr Regular expression to identify ID variables from variable names (Note: for multiple
#'expressions, i.e. if \code{idExpr} is a character vector of length > 1, at least one expression
#'should match to identify the variable as ID variable). The use of a regular expression is due to
#'the fact that the data records often contain not just one, but several identification variables,
#'for example for pupils, teachers, schools, classes or federal states. Logically, no descriptive
#'values are reported for identification variables. They must be specified here so that the subsequent
#'\code{calculateDescriptives} function knows, so to speak, for which variables no descriptive values are
#'to be calculated.
#'@param impExpr Regular expression to identify imputed variables from variable labels in GADSdat
#'object (Note: for multiple expressions, i.e. if \code{impExpr} is a character vector of length > 1,
#'at least one expression should match to identify the variable as an imputed variable). Regular
#'expressions are also used here, as several variables (each with different individual IDs) can be imputed.
#'@param scaleExpr Regular expression to identify scale or fake scale variables from variable labels in GADSdat
#'object (Note: for multiple expressions, i.e. if \code{scaleExpr} is a character vector of length > 1,
#'at least one expression should match to identify the variable as a scale variable). Scales are defined when
#'several items measure a common (latent) construct and there is also a variable that represents the scale value.
#'The scale variable represents usually the averaged (or otherwise aggregated) value within a person across all
#'items of this scale. Fake scales are defined when several items measure a common (latent) construct, but there
#'is no additional variable that represents the scale value.
#'@param itemExpr Regular expression to identify items which constitute a true scale from the variable
#'labels in GADSdat object. Note: Only the regular expressions that identify the items must be entered here.
#'The additional scale variables do not have to be specified. If several scales are defined in the data set
#'(e.g. "self concept" and interest), no distinction needs to be made here as to which items belong to which
#'scale. (This is done elsewhere.) Assume that the "self concept" is measured with the items \code{SK_I1},
#'\code{SK_I2}, \code{SK_I3}, and the scale variable is called \code{SK_scale}. Let us also assume that
#'"interest" is measured with the items \code{Int_I1}, \code{Int_I2}, \code{Int_I3}, \code{Int_I4}, and
#'that the scale variable is called \code{Int_scale}. Then it could be specified
#'here: \code{itemExpr = "I[1-4]{1}$"}
#'@param fakeItemExpr Regular expression to identify fake items which constitute a fake scale from the variable
#'labels in GADSdat object. This works in the same way as with \code{itemExpr}.
#'@param nwExpr Regular expression to identify network variables from variable labels in GADSdat object
#'(Note: for multiple expressions, i.e. if \code{nwExpr} is a character vector of length > 1, at least
#'one expression should match to identify the variable as a network variable)
#'@param varNameSeparatorImp character sign to separate the "pooled" suffix from group name in group
#'column. For example, if multiple imputed variables occur in the wide-format data.frame as \code{pv_1},
#'\code{pv_2}, \code{pv_3}, use \code{"_"}. If no such sign exists in the data, i.e. if multiple imputations
#'occur as \code{pv1}, \code{pv2}, \code{pv3}, instead of \code{pv_1}, \code{pv_2}, \code{pv_3}, or \code{pv.1},
#'\code{pv.2}, \code{pv.3}, use \code{NA} or \code{NULL} or \code{""}. In this case, you will have to specify
#'the \code{ncharSeparatorImp} argument.
#'@param ncharSeparatorImp Integer: only relevant if no \code{varNameSeparatorImp} exists, i.e. if multiple
#'imputations occur as \code{pv1}, \code{pv2}, \code{pv3}, instead of \code{pv_1}, \code{pv_2}, \code{pv_3},
#'or \code{pv.1}, \code{pv.2}, \code{pv.3}. \code{ncharSeparatorImp} than specifies the number of character
#'signs which should be trimmed to identify the common variable stem. If \code{varNameSeparatorImp} is not
#'\code{NA} or \code{NULL} or \code{""}, \code{ncharSeparatorImp} will be ignored. For example, if multiple
#'imputations occur as \code{pv_1}, \code{pv_2}, \code{pv_3}, use \code{varNameSeparatorImp = "_"}. If multiple
#'imputations occur as \code{pv1}, \code{pv2}, \code{pv3}, use \code{varNameSeparatorImp = NULL} and
#'\code{ncharSeparatorImp = 2}. The first 2 signs of variables names (i.e., \code{"pv"}) will be used to
#'identify the imputed variables which belong to a common stem.
#'@param lastOccurrence Logical: If \code{varNameSeparatorImp} occurrs multiple times within a string,
#'\code{lastOccurrence} defines whether the last occurrence should be used for splitting
#'@param groupSuffixImp tbd
#'@param nCatsForOrdinal Numeric vector with number of categories considered for ordinal variables. Variables
#'with number of categories as defined here are considered to be ordinal instead of nominal. If NULL, this rule
#'will be ignored, and nominal/ordinal assignment is done in other ways
#'@param nwVarNameSeparatorImp character sign to separate network variable names from network variable groups.
#'For example, if network variables occur as \code{friend_1}, \code{friend_2}, ..., \code{friend_12}, use \code{"_"}.
#'If no such sign exists in the data, i.e. if network variable names occur as \code{friend1}, \code{friend2}, ...,
#'\code{friend12}, use \code{NA} or \code{NULL} or \code{""}. In this case, you will have to specify
#'the \code{nwNcharSeparatorImp} argument.
#'@param nwNcharSeparatorImp Integer: only relevant if no \code{nwVarNameSeparatorImp} exists, i.e. if network variables
#'occur as \code{friend1}, \code{friend2}, ..., \code{friend12}, instead of \code{friend_1}, \code{friend_2}, ...,
#'\code{friend_12}. \code{nwVcharSeparatorImp} than specifies the number of character signs which should be trimmed to
#'identify the common variable stem. If \code{nwVarNameSeparatorImp} is not \code{NA} or \code{NULL} or \code{""},
#'\code{ncharSeparatorImp} will be ignored. For example, if network variables occur as \code{friend_1}, \code{friend_2},
#'..., \code{friend_12}, use \code{nwVarNameSeparatorImp = "_"}. If network variables occur as \code{friend1}, \code{friend2},
#'..., \code{friend12}, use \code{nwVarNameSeparatorImp = NULL} and \code{nwNcharSeparatorImp = 6}. The first 6 signs of
#'variables names (i.e., \code{"friend"}) will be used to identify the group.
#'@param nwLastOccurrence Logical: If \code{nwVarNameSeparatorImp} occurrs multiple times within a string, \code{nwLastOccurrence}
#'defines whether the last occurrence should be used for splitting
#'@param verbose Should scale identification be reported?
#'
#'@details The \code{eatCodebook} package aims to create a human-readable pdf codebook from a \code{GADSdat} data base object. The codebook
#'contains information about the variables used in the study, including their descriptive properties. Which descriptive properties are reported
#'in the codebook depends, among other things, on the scale level of the variables. For example, the mean and standard deviation are reported
#'for metric variables and frequency distributions for categorical variables. For non-imputed variables, the proportion of missing values is
#'also given, and for scale variables, the number of items that make up the scale and the internal consistency of the scale are given. The
#'codebook is created in several steps. In the first step, the \code{createInputForDescriptives} function is used to generate an auxiliary
#'object from the database that contains information on what type of descriptive information is to be reported for which variable. The object
#' created in this function is the basis for the \code{calculateDescriptives} function.
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
createInputForDescriptives <- function ( GADSdat, idExpr = "^ID", impExpr = c("IMPUTATION\\s+{0,1}[[:digit:]]{1,2}", "PV\\s+{0,1}[[:digit:]]{1,2}"), scaleExpr = "^Skala", itemExpr = "plausible|indikator", fakeItemExpr = "fake", nwExpr = "IDinClass", varNameSeparatorImp = "_", ncharSeparatorImp = 2, lastOccurrence =TRUE, groupSuffixImp = "imp", nCatsForOrdinal = c(2:5), nwVarNameSeparatorImp = "_", nwNcharSeparatorImp = 6, nwLastOccurrence = TRUE, verbose = FALSE) {
  UseMethod("createInputForDescriptives")
}
#'@export
createInputForDescriptives.GADSdat <- function ( GADSdat, idExpr = "^ID", impExpr = c("IMPUTATION\\s+{0,1}[[:digit:]]{1,2}", "PV\\s+{0,1}[[:digit:]]{1,2}"), scaleExpr = "^Skala", itemExpr = "plausible|indikator", fakeItemExpr = "fake", nwExpr = "IDinClass", varNameSeparatorImp = "_", ncharSeparatorImp = 2, lastOccurrence =TRUE, groupSuffixImp = "imp", nCatsForOrdinal = c(2:5), nwVarNameSeparatorImp = "_", nwNcharSeparatorImp = 6, nwLastOccurrence = TRUE, verbose = FALSE) {
    ### wenn es missings in der Format-Spalte des GADSdat-Labels-Objekt gibt, soll zuvor eatGADS::checkFormat aufgerufen werden
           if(any(is.na( GADSdat[["labels"]][,"format"]))) {
              message("Call 'checkFormat()' from the 'eatGADS' package.")
              GADSdat <- eatGADS::checkFormat(GADSdat)
           }
           vari <- GADSdat[["labels"]][!duplicated(GADSdat[["labels"]][,"varName"]),c("varName","varLabel", "format")]
           vari[,"imp"] <- FALSE
    ### imp-Eintrag vergeben
           for ( i in impExpr) {  vari[grep(i, vari[,"varLabel"]),"imp"] <- TRUE  }
           vari[,"type"] <- "variable"
           for ( i in scaleExpr) { vari[grep(i, vari[,"varLabel"]),"type"] <- "scale"}
           for ( i in itemExpr) { vari[grep(i, vari[,"varLabel"]),"type"] <- "item"}
           for ( i in fakeItemExpr) { vari[grep(i, vari[,"varLabel"]),"type"] <- "fake_item"}
    ### scale-Eintrag vergeben
           vari[,"laufnummer"] <- 1:nrow(vari)                                  ### dieses, damit die Reihenfolge der varinfo so ist wie im labels sheet
    ### Hilfsvariable anlegen, um Netzwerkvariablen zu identifizieren (wird spaeter wieder geloescht)
           vari[,"nw"]  <- FALSE
           vari <- do.call("rbind", by(data = vari, INDICES = vari[,"laufnummer"], FUN = function ( z ) {
    ### wenn Variable als ID variable oder Netzwerk-Variable identifiziert wird, soll scale-Eintrag leer sein
                   if ( length(unlist(lapply(idExpr, FUN = function (ie) {grep(ie, z[["varName"]])})))>0 ) {
                        # message(paste0("Variable '",z[["varName"]],"' matches ID variable definition (as defined in 'idExpr') and will be handled as ID variable (set 'scale' entry to NA)."))
                        z[,"scale"] <- NA
                        return(z)
                   }
                   if ( length(unlist(lapply(nwExpr, FUN = function (ie) {grep(ie, z[["varLabel"]])})))>0 ) {
                        # message(paste0("Variable '",z[["varName"]],"' matches network variable definition (as defined in 'nwExpr') and will be handled like ID variables (set 'scale' and 'type' entries to NA)."))
                        z[,"scale"] <- z[,"type"] <- NA
                        z[,"nw"]    <- TRUE
                        return(z)
                   }
    ### wenn Variable im GADSdat-Labelsfile ein "A" in der Format-Spalte hat, bedeutet das "character". Es soll ein leerer Eintrag in der "scale"-Spalte eingetragen werden
                   if(toupper(substr(z[["format"]],1,1)) == "A") {
                        z[,"scale"] <- NA
                        return(z)
                   }
                   if ( is.character(GADSdat[["dat"]][,z[["varName"]]])) {scale <- "nominal"}
                   if ( is.numeric(GADSdat[["dat"]][,z[["varName"]]])) {
                        mis    <- GADSdat[["labels"]][which(GADSdat[["labels"]][,"varName"] == z[["varName"]]),]
                        mis    <- mis[which(mis[,"missings"] == "miss"),"value"]
                        nonmis <- sort(setdiff(unique(GADSdat[["dat"]][,z[["varName"]]]), mis))
                        if ( any(is.na(as.integer(nonmis))) ) {
                             warning(paste0("Variable '",z[["varName"]],"': Missing values in sorted integer entries found. This should only occur for pseudo-numeric values, i.e. id variables."))
                             scale <- NA
                        }  else  {
    ### wenn sie ausschliesslich fehlende Werte hat und daher kein skalenniveau identifiziert werden kann, soll NA eingetragen werden
                            if ( length(nonmis) == 0) {
                                 message(paste0("Variable '",z[["varName"]],"' only has missing values. Scale level is not identifiable (set 'scale' entry to NA)."))
                                 z[,"scale"] <- NA
                                 return(z)
                            }
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
                            # message(paste0("Variable '",z[["varName"]],"' has identified scale '",scale,"' but is expected to be 'numeric' due to format definition '",z[["format"]],"' in GADSdat labels sheet. Transform '",z[["varName"]],"' to be numeric."))
                            scale <- "numeric"
                        }
    ### wenn das erste Kriterium fuer numerisch nicht erfuellt wurde, soll hier das zweite geprueft werden: wenn eine variable nur missings als definierte labels hat, dann soll sie numerisch sein
                        if (scale != "numeric") {
                            if ( all(stats::na.omit(GADSdat[["labels"]][which(GADSdat[["labels"]][,"varName"] == z[["varName"]]),"missings"]) == "miss")) {
                                 # message(paste0("'",z[["varName"]],"': only missing labels are defined in in the labels sheet of the GADSdat object. Hence, '",z[["varName"]],"' is expected to be numeric. Change 'scale' value from '",scale,"' to 'numeric'."))
                                 scale <- "numeric"
                            }
                        }
                   }
    ### check No. 2 und ggf. korrektur der 'scale'-Zuweisung
                   if (scale == "ordinal") {
                        if(!is.null(nCatsForOrdinal)) {
                             if ( !length(unique(nonmis)) %in% nCatsForOrdinal) {
                                 # message(paste0("Variable '",z[["varName"]],"' has ",length(unique(nonmis))," non-missing categories. This is outside of the range defined in the 'nCatsForOrdinal' argument. Hence, '",z[["varName"]],"' will be transformed from '",scale,"' to 'nominal'."))
                                 scale <- "nominal"
                             }
                        }
                   }
                   z[,"scale"] <- scale
                   return(z)}))
    ### group-Eintrag (Gruppenzuordnung) vergeben, das geschieht fuer imputierte und nicht imputierte Variablen separat
           vari <- do.call("rbind", by(data = vari, INDICES = vari[,"imp"], FUN = function ( v ) {
                   if ( isFALSE(v[1,"imp"]) ) {                                 ### hier beginnt die Behandlung fuer nicht-imputierte Variablen
                        v[,"group"] <- NA                                       ### Gruppenzugehoerigkeit initialisieren
                        scales<- v[which(v[,"type"] == "scale"),"varName"]      ### das sind die Namen der Skalen, zu jeder werden jetzt die Items herausgesucht
                        for ( sc in scales) {
                              items <- setdiff(v[which(substr(v[,"varName"],1,nchar(sc)) == sc),"varName"], sc)
                              if ( length(items)==0) {warning(paste0("Cannot found any non-imputed items for scale '",sc,"' (not imputed)."))}
                              if (verbose) {cat(paste0("Scale '",sc,"' (not imputed): Found following ",length(items)," items: '",paste(items, collapse="', '"),"'.\n"))}
                              v[eatTools::whereAre(c(items, sc), v[,"varName"], verbose=FALSE),"group"] <- sc
                        }
                        if ("fake_item" %in% v[,"type"]) {
                              v[grep("fake_item",v[,"type"]) ,"group"] <- eatTools::removeNumeric(v[grep("fake_item",v[,"type"]) ,"varName"])
                              warning("Identification of fake scales cannot be done completely automatically. Please check if the assignment of which items belong to a common scale is correct.")
                        }
                        v[which(is.na(v[,"group"])),"group"] <- v[which(is.na(v[,"group"])),"varName"]
                   }  else  {                                                   ### hier beginnt die Behandlung fuer imputierte Variablen
                        if (is.null(varNameSeparatorImp) || is.na(varNameSeparatorImp) || varNameSeparatorImp == "") {
                            v[,"group"] <- substr(v[,"varName"], 1, ncharSeparatorImp)
                        }  else  {
                            v[,"group"] <- paste(eatTools::halveString(string = v[,"varName"], pattern = varNameSeparatorImp, first = !lastOccurrence)[,1], "pooled",sep="_")
                        }
                   }
                   return(v)}))
    ### ggf, nachtraeglich die group-zuordnung nur fuer die netzwerk-variablen anpassen
           if (any(vari[,"nw"] == TRUE)) {
               if (is.null(nwVarNameSeparatorImp) || is.na(nwVarNameSeparatorImp) || nwVarNameSeparatorImp == "") {
                   vari[which(vari[,"nw"] == TRUE),"group"] <- substr(vari[which(vari[,"nw"] == TRUE),"group"],1, nwNcharSeparatorImp)
               }  else  {
                   vari[which(vari[,"nw"] == TRUE),"group"] <- eatTools::halveString(string = vari[which(vari[,"nw"] == TRUE),"group"], pattern = nwVarNameSeparatorImp, first = !nwLastOccurrence)[,1]
               }
           }
    ### nach Laufnummer sortieren und dann die Laufnummer- und Netzwerk-Spalte entfernen
           vari <- data.frame(vari[sort(vari[,"laufnummer"],decreasing=FALSE,index.return=TRUE)$ix,-match(c("laufnummer","nw"), colnames(vari))])
    ### consistency checks
           variV<- vari[which(vari[,"type"] %in% c("variable", "item", "fake_item")),]
           chk2 <- by(data = variV, INDICES = variV[,"group"], FUN = function ( x ) {
                   if ( nrow(x)>1) {
                        if ( length(unique(x[,"scale"])) > 1) {
                             warning("Scale level is not unique for items '",paste(x[,"varName"], collapse="', '"), "'. Scale definition seems to be invalid. Please correct by hand.")
                        }
                   } })
           return(vari)}

#'@export
createInputForDescriptives.list <- function ( GADSdat, idExpr = "^ID", impExpr = c("IMPUTATION\\s+{0,1}[[:digit:]]{1,2}", "PV\\s+{0,1}[[:digit:]]{1,2}"), scaleExpr = "^Skala", itemExpr = "plausible|indikator", fakeItemExpr = "fake", nwExpr = "IDinClass", varNameSeparatorImp = "_", ncharSeparatorImp = 2, lastOccurrence =TRUE, groupSuffixImp = "imp", nCatsForOrdinal = c(2:5), nwVarNameSeparatorImp = "_", nwNcharSeparatorImp = 6, nwLastOccurrence = TRUE, verbose = FALSE) {
    ### Achtung! wenn mehrere GADSdat-Objekte als Liste uebergeben werden, koennen die weiteren Argumente ebenfalls als Liste uebergeben werden,
    ### oder man kann ein Argument fuer alle GADSdat-Objekte benutzen. welches von beiden hier der Fall ist, muss ermittelt werden
           #fwa    <- createFunNameWithArgs(funName = "createInputForDescriptives")# 'fwa' = function with arguments
           fwa    <- as.list(match.call())
           argList<- list()                                                     ### list with arguments
           for ( i in names(fwa)[-1] ) {eval(parse(text = paste0("argList[[i]] <- ",i)))}
           loop   <- createAndExecuteFunctionCalls(funName = "createInputForDescriptives", argList = argList)
           return(loop)}

check_inputForDescriptives <- function(inputForDescriptives){
  if(!is.data.frame(inputForDescriptives)) stop("'inputForDescriptives' needs to be a data.frame.")
  if(!identical(names(inputForDescriptives), c('varName', 'varLabel', 'format', 'imp', 'type', 'scale', 'group'))) {stop("The column names of 'inputForDescriptives' need to be: 'varName', 'varLabel', 'format', 'imp', 'type', 'scale', 'group'.")}
  if(!is.logical(inputForDescriptives$imp)) stop("The column 'imp' in 'inputForDescriptives' must be logical.")
  if(any(!inputForDescriptives$type %in% c("variable", "scale","", "item", "fake_item", NA))) stop("The column 'type' in 'inputForDescriptives' can only contain the entries 'variable', 'scale', 'item', and 'fake_item'.")
  if(any(!inputForDescriptives$scale %in% c("numeric", "ordinal", "nominal", NA))) stop("The column 'scale' in 'inputForDescriptives' can only contain the entries 'numeric', 'ordinal', 'nominal'.")
  if(!length(unique(inputForDescriptives[,"varName"])) == length(inputForDescriptives[,"varName"])) {stop("'varName' column in 'inputForDescriptives' must be unique.")}
  if(tibble::is_tibble(inputForDescriptives)) inputForDescriptives <- as.data.frame(inputForDescriptives)
  checkItemScaleConsistency(inputForDescriptives)
  inputForDescriptives
}

checkItemScaleConsistency <- function (ifd) {
    if ( "scale" %in% ifd[,"type"]) {
         groups <- unique(ifd[which(ifd[,"type"] == "scale"),"group"])
         chk    <- lapply(groups, FUN = function(g) {
                   ifdg <- ifd[which(ifd[,"group"] == g),]
                   tab  <- table(ifdg[,"scale"], useNA = "ifany")
                   if ( length(which(is.na(names(tab)))) > 0) {
                     cat("Error : Scale definition must not contain any NA values if several items belong to one scale: \n")
                     print(ifdg, row.names=FALSE)
                     stop()
                  }
                   if ( "nominal" %in% names(tab)) {
                     cat("Error: 'nominal' is not allowed for scale column for items which belong to one scale: \n")
                     print(ifdg, row.names=FALSE)
                     stop()
                  }
         })
    }
}

# teste:
# test <- function ( x=1, y = 12) { print(as.list(match.call(definition = test))) }
# test <- function ( x=1, y = 12) { print(as.list(sys.call())) }
# test2 <- function ( xx = 100, yy = 200) {a <- 1000; test(x=99, y = 999)}
# test(x=12, y=19)
# test(12, 19)
# test(12)
# test()
createAndExecuteFunctionCalls <- function(funName, argList){
           isList<- lapply(argList, is.list)
           stopifnot(isList[[1]])
           noList<- which(isList == FALSE)
           if ( length(noList)>0) {
                for ( i in noList) {
                     iEntry <- list()
                     for ( j in 1:length(argList[[1]])) {
                         iEntry[[j]] <- argList[[i]]
                     }
                     argList[[i]] <- iEntry
                }
           }
    ### function call
           ret <- list()
           for ( i in 1:length(argList[[1]])) {
                  ret[[i]] <- eval(parse(text=paste0(funName,"(",
                                                     paste(names(argList), paste0("argList[[\"",names(argList),"\"]][[",i,"]]"), sep="=", collapse=", "), ")")))
           }
           names(ret) <- names(argList[[1]])
           return(ret)}


#createFunNameWithArgs <- function ( funName) {
#          i     <- 0
#          while ( eatTools::crop(unlist(strsplit(deparse(sys.call(i))[1], split = "\\("))[1]) != funName) {i <- i-1}
#          fc    <- as.list(sys.call(i))
#          missed<- setdiff(which(names(fc) == ""), 1)                           ### fuer welche Argumente hat der user keine namen angegeben?
#          if ( length(missed)>0) {
#               names(fc)[missed] <- names(formals(funName))[missed-1]
#          }
#          return(fc)}
