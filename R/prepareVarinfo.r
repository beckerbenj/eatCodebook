### enriches labels from GADSdat-object with plain information necessary for codebook generation
prepareVarinfo <- function ( GADSdat.obj, varsToExclude = NULL, impExpr = "IMPUTATION[[:digit:]]{1,2}$", scaleExpr = "^Skala", varNameSeparatorImp = "_", lastOccurrence =TRUE, groupSuffixImp = "imp") {
       weg  <- setdiff(varsToExclude,GADSdat.obj[["labels"]][,"varName"])
       if ( length(weg)>0) { message("Following ",length(weg), " variable(s) which should be excluded do exists in GADSdat.obj: '",paste(weg, collapse="', '"), "'.")}
       vari <- GADSdat.obj[["labels"]][!duplicated(GADSdat.obj[["labels"]][,"varName"]),c("varName","varLabel")]
       if ( length( setdiff(varsToExclude, weg))>0) {
            vari <- vari[-match(setdiff(varsToExclude, weg), vari[,"varName"]),]
       }
       vari[,"imp"] <- FALSE
       vari[grep(impExpr, vari[,"varLabel"]),"imp"] <- TRUE
       vari[,"type"] <- "variable"
       vari[grep(scaleExpr, vari[,"varLabel"]),"type"] <- "scale"
       vari <- do.call("rbind", by(data = vari, INDICES = vari[,"varName"], FUN = function ( z ) {
               if ( class(GADSdat.obj[["dat"]][,z[["varName"]]]) == "character") {scale <- "nominal"}
               if ( class(GADSdat.obj[["dat"]][,z[["varName"]]]) == "numeric") {
                    mis    <- GADSdat.obj[["labels"]][which(GADSdat.obj[["labels"]][,"varName"] == z[["varName"]]),]
                    mis    <- mis[which(mis[,"missings"] == "miss"),"value"]
                    nonmis <- sort(setdiff(unique(GADSdat.obj[["dat"]][,z[["varName"]]]), mis))
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
               z[,"scale"] <- scale
               return(z)}))
       # die group-zuordnung zu vergeben, ist komplizierter
       # fuer imputierte variablen sollte es gehen,
       # aber bei skalen, z.B. bei Sswert01a, Sswert01b, Sswert01c, Sswert01d, Sswert01e, Sswert
       # aus originaler varinfor wirds bloed
       length(unique(vari[,"varLabel"]))
       return(vari)}
               

#      match("sfb_b", vari[,"varName"])



#       vars <- setdiff(unique(GADSdat.obj[["labels"]][,"varName"]), varsToExclude)


#       View(GADSdat.obj[["labels"]])
       
       
       
#       test <- c("a", "a imp", "a IMPUTATION2", "ba IMPUTATION12", "qw IMPUTATION123")
#       grep(impExpr, test)

# ?regexpr