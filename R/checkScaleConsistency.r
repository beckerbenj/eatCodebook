#' Check whether the mean of a scale variable equals the pooled mean of single items
#'
#' Function need the output of the \code{createInputForDescriptives()} function.
#'For each non-imputed scale, the function checks whether the (unweighted) mean
#'of the scale variable equals the pooled mean of the single items which form the scale.
#'
#'@param GADSdat Object of class\code{GADSdat}
#'@param inputForDescriptives The output of the function \code{createInputForDescriptives()}
#'@param id Name or column number of the ID variable. Argument can be numeric or character
#'@param verbose Logical: Print informations on console?
#'
#'@return Function does not return output but provide messages.
#'
#'@export
checkScaleConsistency <- function ( GADSdat, inputForDescriptives, id, verbose = TRUE) {
  UseMethod("checkScaleConsistency")
}
#'@export
checkScaleConsistency.GADSdat <- function ( GADSdat, inputForDescriptives, id, verbose = TRUE) {
           if ("tbl" %in% class(inputForDescriptives)) {
               if(verbose){message("'inputForDescriptives' has class '",paste(class(inputForDescriptives), collapse="', '"), "'. Transform 'inputForDescriptives' into 'data.frame'.")}
               inputForDescriptives <- as.data.frame(inputForDescriptives)
           }
           allNam<- lapply(list(id=id), FUN=function(ii) {eatTools::existsBackgroundVariables(dat = GADSdat[["dat"]], variable=ii)})
           inpSel<- inputForDescriptives[which(inputForDescriptives[,"imp"] == FALSE),]
           groups<- unique(inpSel[which(inpSel[,"type"] == "scale"),"group"])   ### nur fuer nicht-imputierte variablen
           vars  <- unique(inpSel[which(inpSel[,"group"] %in% groups),"varName"])## nur diese Variablen sollen aus dem GADSdat-Objekt extrahiert werden
           gdSel <- GADSdat                                                     ### GADSdat-selektiert
           gdSel[["dat"]]    <- gdSel[["dat"]][,which(colnames(gdSel[["dat"]]) %in% c(allNam[["id"]], vars))]
           gdSel[["labels"]] <- gdSel[["labels"]][which(gdSel[["labels"]][,"varName"] %in% c(allNam[["id"]], vars)),]
           dat   <- extractData ( gdSel , convertLabels = "numeric")
           scales<- do.call(plyr::rbind.fill, lapply(groups, FUN = function ( v ) {
                    inpV <- inpSel[which(inpSel[,"group"] == v),]
                    vars <- by(data = inpV, INDICES = inpV[,"type"], FUN = function ( x ) { x[,"varName"]})
                    if ( length(vars) != 2 || min(sapply(vars, length)) != 1) {
                         if (verbose) { message(paste0("Scale '",vars[["scale"]],"' without items."))}
                         ret <- data.frame ( scale = vars[["scale"]], nItems = 0, items = NA, checkOK = FALSE, reason = "no Items", stringsAsFactors = FALSE)
                         return(ret)
                    }  else  {
                         m1 <- mean( dat[,vars[["scale"]]], na.rm=TRUE)
                         dL <- reshape2::melt(dat, id.vars = allNam[["id"]], measure.vars = vars[["variable"]], na.rm = TRUE)
                         if(inherits(try(m2 <- eatRep::repMean(datL = dL, ID=allNam[["id"]], imp = "variable", dependent = "value", verbose=FALSE, progress = FALSE) ),"try-error"))  {
                             if(verbose) {message("Skip scale '",inpV[which(inpV[,"type"] == "scale"), "group"],"' with items '",paste(unique(inpV[which(inpV[,"type"] == "variable"),"varName"]), collapse = "', '"),"'.")}
                             ret <- data.frame ( scale = inpV[which(inpV[,"type"] == "scale"), "group"], nItems = length(unique(inpV[which(inpV[,"type"] == "variable"),"varName"])), items = paste0("'", paste(unique(inpV[which(inpV[,"type"] == "variable"),"varName"]), collapse = "', '"),"'"),  checkOK = FALSE, reason = "computation of pooled mean failed", stringsAsFactors = FALSE)
                             return(ret)
                         }  else  {
                         m2r<- eatRep::report(m2)
                             if ( abs(m1 - m2r[which(m2r[,"parameter"] == "mean"), "est"]) > 0.02) {
                                  message("Scale '",vars[["scale"]],"': Mean of scale variable ",round(m1, digits = 3), " does not equal the pooled mean of the items '",paste(vars[["variable"]], collapse="', '"),"' (",round(m2r[which(m2r[,"parameter"] == "mean"), "est"], digits = 3),").")
                                  ret <- data.frame ( scale = vars[["scale"]], nItems = length(vars[["variable"]]), items = paste0("'", paste(vars[["variable"]], collapse = "', '"),"'"),  checkOK = FALSE, scaleMean = m1, pooledMean = m2r[which(m2r[,"parameter"] == "mean"), "est"], reason = "scale mean and pooled mean differ", stringsAsFactors = FALSE)
                                  return(ret)
                             }
                         ret <- data.frame ( scale = vars[["scale"]], nItems = length(vars[["variable"]]), items = paste0("'", paste(vars[["variable"]], collapse = "', '"),"'"),  checkOK = TRUE, scaleMean = m1, pooledMean = m2r[which(m2r[,"parameter"] == "mean"), "est"], stringsAsFactors = FALSE)
                         }
                    }}))
           return(scales)}

#'@export
checkScaleConsistency.list <- function ( GADSdat, inputForDescriptives, id, verbose = TRUE) {
    ### Achtung! wenn mehrere GADSdat-Objekte als Liste uebergeben werden, koennen die weiteren Argumente ebenfalls als Liste uebergeben werden,
    ### oder man kann ein Argument fuer alle GADSdat-Objekte benutzen. welches von beiden hier der Fall ist, muss ermittelt werden
           fwa    <- createFunNameWithArgs(funName = "checkScaleConsistency")   ### 'fwa' = function with arguments
           argList<- list()                                                     ### list with arguments
           for ( i in names(fwa)[-1] ) {eval(parse(text = paste0("argList[[i]] <- ",i)))}
           loop   <- createAndExecuteFunctionCalls(funName = "checkScaleConsistency", argList = argList)
           return(loop)}
