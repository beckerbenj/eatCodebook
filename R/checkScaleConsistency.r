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
           scales<- lapply(groups, FUN = function ( v ) {
                    inpV <- inpSel[which(inpSel[,"group"] == v),]
                    vars <- by(data = inpV, INDICES = inpV[,"type"], FUN = function ( x ) { x[,"varName"]})
                    if ( length(vars) != 2 || min(sapply(vars, length)) != 1) {
                         if (verbose) { message(paste0("Scale '",vars[["scale"]],"' without items."))}
                    }  else  {
                         m1 <- mean( GADSdat[["dat"]][,vars[["scale"]]], na.rm=TRUE)
                         dL <- reshape2::melt(GADSdat[["dat"]], id.vars = allNam[["id"]], measure.vars = vars[["variable"]], na.rm = TRUE)
                         if(inherits(try(m2 <- eatRep::repMean(datL = dL, ID=allNam[["id"]], imp = "variable", dependent = "value", verbose=FALSE, progress = FALSE) ),"try-error"))  {
                             if(verbose) {message("Skip scale '",inpV[which(inpV[,"type"] == "scale"), "group"],"' with items '",paste(unique(inpV[which(inpV[,"type"] == "variable"),"varName"]), collapse = "', '"),"'.")}
                         }  else  {
                         m2r<- eatRep::report(m2)
                             if ( abs(m1 - m2r[which(m2r[,"parameter"] == "mean"), "est"]) > 0.02) {
                                  message("Scale '",vars[["scale"]],"': Mean of scale variable ",round(m1, digits = 3), " does not equal the pooled mean of the items '",paste(vars[["variable"]], collapse="', '"),"' (",round(m2r[which(m2r[,"parameter"] == "mean"), "est"], digits = 3),").")
                             }
                         }
                    }}) }
                    
#'@export
checkScaleConsistency.list <- function ( GADSdat, inputForDescriptives, id, verbose = TRUE) {
    ### Achtung! wenn mehrere GADSdat-Objekte als Liste uebergeben werden, koennen die weiteren Argumente ebenfalls als Liste uebergeben werden,
    ### oder man kann ein Argument fuer alle GADSdat-Objekte benutzen. welches von beiden hier der Fall ist, muss ermittelt werden
           fwa    <- createFunNameWithArgs(funName = "checkScaleConsistency")   ### 'fwa' = function with arguments
           argList<- list()                                                     ### list with arguments
           for ( i in names(fwa)[-1] ) {eval(parse(text = paste0("argList[[i]] <- ",i)))}
           loop   <- createFunctionCalls(funName = "checkScaleConsistency", argList = argList)
           length(loop)
           ret    <- list()
           for ( i in 1:length(loop)) {
                ret[[i]] <- eval(parse(text = loop[i]))
           }
           return(ret) }
