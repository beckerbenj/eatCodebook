
# INPUT
#	x: actual data vector
#	missings: vector of variable values which are missing codes


kennwerte.kategorial <- function(x, value_table) {
  missings <- stats::na.omit(value_table[value_table$missings == "miss", "value"])
  unique_values <- value_table[c(which(is.na(value_table[,"missings"])),which(value_table[,"missings"] == "valid")),"value"]

### wenn unique_values nicht aus dem labels-objekt ausgelesen werden koennen (etwa weil sie in der SPSS-datei nicht definiert waren)
### muessen sie aus den Daten ausgelesen werden
  uvd  <- sort(setdiff(stats::na.omit(unique(x)), missings))

### check, ob es sich um freie antwortfelder handelt
  if( !"numeric" %in% class(x) && length(uvd)>50) {
      warning("Variable '",unique(value_table[,"varName"]), "' has class '",class(x),"' with ",length(uvd)," unique values. '",unique(value_table[,"varName"]), "' seems to stem from an open answer box in the questionnaire. Calculating descriptive statistics seems questionable.")
  }
  if(!identical(unique_values, uvd)) {
      warning("Variable '",unique(value_table[["varName"]]),"': Mismatch between values declared in 'labels' sheet of the 'GADSdat' object and data. \n    'GADSdat' object: '",paste(unique_values, collapse="', '"), "'\n                data: '",paste(uvd,collapse="', '"), "'")
      unique_values <- sort(unique(c(uvd, unique_values)))
  }

### schauen, ob fuer alle empirisch vorhandenen Werte auch wertelabels vorhanden sind
  not_labeled_values <- setdiff(unique_values, value_table[,"value"])
  if ( length(not_labeled_values)>0) {
      warning("Variable '",unique(value_table[["varName"]]),"': Following values are not labeled in the 'labels' sheet of the 'GADSdat' object: '", paste(not_labeled_values, collapse="', '"), "'. Values will be used as labels.")
  }

### Berechnung der Haeufigkeiten
  werte.valid <- x[!x %in% missings  & ! is.na(x)]
  werte.total <- x

### Warnung ist hier erstmal ausgeschaltet
#  if(is.numeric(x)) {
#      warning("Original function only allows for non-numeric values.")
#  }

#  for(k in unique_values){
#      if(inherits(try(werte.valid[grepl(paste0("^\\s*",k,"\\s*$") , werte.valid)] <- k ),"try-error"))  {warning("Regular expression failed for category '",k,"' of variable '",unique(value_table[["varName"]]),"'.")}
#      if(inherits(try(werte.total[grepl(paste0("^\\s*",k,"\\s*$") , werte.total)] <- k ),"try-error"))  {warning("Regular expression failed for category '",k,"' of variable '",unique(value_table[["varName"]]),"'.")}
#    }
#  werte.valid[grepl("^\\s*$" , werte.valid)] <- NA
#  werte.valid <- werte.valid[! werte.valid %in% missings  & ! is.na( werte.valid )]
#  werte.total[grepl("^\\s*$" , werte.total)] <- NA

  # Valide und totale Fallzahlen
  N.valid <- length(werte.valid)
  N.total	<- length(werte.total)

  # absolute Haeufigkeiten
  werte.total.abs <- as.numeric(table(factor(werte.total, levels = c(unique_values, missings)) , useNA = "always"))

  # relative Haeufigkeiten (alle Faelle)
  werte.total.frq	<- 100* as.numeric(table(factor(werte.total, levels = c(unique_values, missings)) , useNA = "always")) / N.total

  # relative Haeufigkeiten (valide Faelle)
  if(N.valid==0){
    werte.valid.frq <- as.numeric(table(factor(werte.total, levels = unique_values), useNA = "always"))
  } else {
    werte.valid.frq <- 100* as.numeric(table(factor(werte.total, levels= unique_values), useNA = "always")) / N.valid
  }

  #### Aufbereitung der Kennwerte ####
  # Formatierung und Names der validen Werte
  werte.valid.frq <- formatC(werte.valid.frq, format = "f" , digits = 1 )
  names(werte.valid.frq) <- paste0(c(unique_values, "sysmis" ) , ".valid" )
  werte.valid.frq[paste0(c(missings , "sysmis"), ".valid" ) ] <- "\\multic{--}"

  # Formatierung und Names der totalen Werte
  werte.total.frq <- formatC(werte.total.frq, format = "f", digits = 1 )
  names(werte.total.frq) <- paste0(c(unique_values ,missings, "sysmis" ) , ".total" )

  # Formatierung und Names der absoluten Werte
  werte.total.abs <- formatC(werte.total.abs, format = "f", digits = 0 )
  names(werte.total.abs) <- paste0(c(unique_values, missings, "sysmis" ), ".totalabs" )

  ret <- c(N.valid, N.total, werte.valid.frq, werte.total.frq, werte.total.abs)
  names(ret)[1:2] <- c("N.valid", "N.total")
  return(ret)
}


kennwerte.ordinal <- function(x, value_table) {
  cat_values <- kennwerte.kategorial(x, value_table)

  missings <- stats::na.omit(value_table[value_table[,"missings"] == "miss", "value"])
  werte.valid <- x[!x %in% missings  & ! is.na(x)]

  #### Berechnung der metrischen Kennwerte ####
  # Arithmetisches Mittel
  mean.valid <- formatC(mean(werte.valid), format = "f", digits = 2)
  names(mean.valid) <- "mean.valid"

  # Standardabweichung
  sd.valid <- formatC(stats::sd(werte.valid), format = "f", digits = 2)
  names(sd.valid) <- "sd.valid"

  c(cat_values["N.valid"], cat_values["N.total"], mean.valid , sd.valid ,
    cat_values[!names(cat_values) %in% c("N.valid", "N.total")])
}


kennwerte.ordinal.skala <- function(x, value_table) {
  ord_values <- kennwerte.ordinal(x, value_table)
  valid_freqs <- grep("valid$", names(ord_values), value = TRUE)
  valid_freqs <- setdiff(valid_freqs, c("N.valid", "mean.valid", "sd.valid"))

  ord_values[setdiff(names(ord_values), valid_freqs)]
}


kennwerte.metrisch <- function(x, value_table) {
  missings <- stats::na.omit(value_table[value_table$missings == "miss", "value"])
  werte.valid <- x[!x %in% missings  & ! is.na(x)]

  N.valid <- length(werte.valid)
  mean.valid <- formatC(mean(werte.valid), format = "f", digits = 2 )
  sd.valid <- formatC(stats::sd(werte.valid), format = "f", digits = 2 )
  min.valid <- formatC(min(werte.valid), format = "f", digits = 1 )
  max.valid <- formatC(max(werte.valid), format = "f", digits = 1 )
  sysmis.totalabs <- length(which(is.na(x)))

  c("N.valid" = N.valid , "mean.valid" = mean.valid , "sd.valid" = sd.valid,
   "min.valid" = min.valid, "max.valid" = max.valid , "sysmis.totalabs" = sysmis.totalabs)
}


# how to integrate variable sets (items of scales?)
### kennwerte.skala(dat=dat, scaleCol = "DM_erfahrung", c("Semz19_a", "Semz19_b", "Semz19_c", "Semz19_d"), missingValues = c(-98,-99))
kennwerte.skala <- function(GADSdat.obj,sub.varinfo) {
  # erzeugt denselben output wie die originale kennwerte.skala
  # INPUT
  #	dat: Datensatz (data.frame)
  #	scaleCol: Spaltennummer oder Name der Variable, die die Skalenwerte enthaelt
  #	variableCols: Spaltennummern oder Namen der Einzelitems der Skala
  #	missingValues: optional, Vektor aus numerischen Werten, die missings bezeichnen sollen
  # OUTPUT:
  #	ret.var: Liste mit zwei Eintraegen:
  #			 Erster Listeneintrag ist ein Vektor mit den metrischen Kennwerten der Skala (M, SD, Min, Max, Cronbachs Alpha)
  #			 Zweiter Listeneintrag ist ein data.frame mit den ordinalen Kennwerten der
scaleCol<- sub.varinfo[which(sub.varinfo[,"type"] == "scale"),"varName"]
variableCols <- sub.varinfo[which(sub.varinfo[,"type"] != "scale"),"varName"]

# erstmal keine checks, die passieren auf hoeherer Ebene
  dat   <- GADSdat.obj[["dat"]]
  allVar<- list(sc = scaleCol, vc = variableCols)
  allNam<- lapply(allVar, FUN=function(ii) {eatTools::existsBackgroundVariables(dat = dat, variable=ii)})

# descriptives der einzelitems ... rekursiver Funktionsaufruf ... sub.varinfo kopieren und anpassen
  svi   <- sub.varinfo
  svi[,"group"] <- svi[,"varName"]
  check0<- svi[which(svi[,"type"] == "variable"),"scale"]
  if ( length(unique(check0)) != 1) {
       stop("All ",nrow(svi)," items belonging to the same scale '",scaleCol,"' must have equal scale definition.")
  }
  items <- calculateDescriptives(GADSdat.obj, svi[which(svi[,"type"] != "scale"),], showCallOnly = FALSE)
  check1<- table(sapply(items, length))
  if ( length(check1) != 1) {
       stop("Vector of descriptives for ",length(items)," items belonging to the same scale '",scaleCol,"' must be of equal length.")
  }
  desc  <- as.matrix(do.call("cbind", items))
  desc  <- desc[-grep("multic", desc[,1]),]

# missing values aus GADSdat-objekt auslesen und in den Daten rekodieren, falls es missings gibt
  for ( i in c(allNam[["vc"]], allNam[["sc"]]) ) {
      sublab <- GADSdat.obj[["labels"]][which(GADSdat.obj[["labels"]][,"varName"] == i),]
      if ( "miss" %in% sublab[,"missings"]) {
           mv <- sublab[which(sublab[,"missings"] == "miss"),"value"]
           rs <- paste(mv , " = " , "NA",sep="", collapse="; ")
           dat[,i] <- car::recode(dat[,i], rs)
      } }


### 2. Skalenkennwerte (erstes Objekt der zurueckgegebenen Liste)
### Rueckgabe sind alles character-Werte mit unterschiedlicher Stellenanzahl, auf die gerundet wird
### wenn rundung etwas ganzzahliges ergibt, soll trotzdem 4.0 angezeigt werden statt 4
  ret <- list(data.frame ( v1 = as.character(c(length(stats::na.omit(dat[,allNam[["sc"]]])),
                                 format(round(mean( dat[,allNam[["sc"]]], na.rm=TRUE),digits = 2), nsmall = 2),
                                 format(round(stats::sd( dat[,allNam[["sc"]]], na.rm=TRUE),digits = 2),nsmall = 2),
                                 format(round(min( dat[,allNam[["sc"]]], na.rm=TRUE),digits = 1),nsmall = 1),
                                 format(round(max( dat[,allNam[["sc"]]], na.rm=TRUE),digits = 1),nsmall = 1),
                                 length(which(is.na(dat[,allNam[["sc"]]]))),
                                 format(round(psy::cronbach(dat[,allNam[["vc"]]])[["alpha"]], digits = 2), nsmall = 2))), stringsAsFactors = FALSE))
  colnames(ret[[1]]) <- allNam[["sc"]]
  rownames(ret[[1]]) <- c("N.valid", "mean.valid", "sd.valid", "min.valid", "max.valid", "sysmis.totalabs", "alpha")
### 3. Itemkennwerte (zweites Objekt der zurueckgegebenen Liste)
#  ret2<- lapply(allNam[["vc"]], FUN = function ( vname ) {
#         data.frame ( v1 = as.character(c(length(na.omit(dat[,vname])), length(dat[,vname]),
#                                 format(round(mean( dat[,vname], na.rm=TRUE),digits = 2), nsmall = 2),
#                                 format(round(sd( dat[,vname], na.rm=TRUE),digits = 2),nsmall = 2),
#                                 format(round(length(which(is.na(dat[,vname]))) / nrow(dat),digits = 1),nsmall = 1),
#                                 length(which(is.na(dat[,vname]))),
#                                 format(round(cor(dat[,c(vname, allNam[["sc"]])], use="pair")[1,2],digits = 2), nsmall = 2))),stringsAsFactors = FALSE)})
#  ret2<- do.call("cbind", ret2)
#  colnames(ret2) <- allNam[["vc"]]
#  rownames(ret2) <- c("N.valid", "N.total", "mean.valid", "sd.valid", "sysmis.total", "sysmis.totalabs", "cor.valid")
#  ret2 <- as.matrix(ret2)                                                       ### urspruengliche Struktur von Felix replizieren
### 4. Rueckgabeobjet bauen
  ret[[2]] <- desc
  return(ret)}


### kennwerte.skala.fake(dat=dat, variableCols = c("Semz19_a", "Semz19_b", "Semz19_c", "Semz19_d"), missingValues = c(-98,-99))
kennwerte.skala.fake <- function(dat,variableCols, missingValues = NULL) {
  # INPUT
  #	dat: Datensatz (data.frame)
  #	scaleCol: Spaltennummer oder Name der Variable, die die Skalenwerte enthaelt
  #	variableCols: Spaltennummern oder Namen der Einzelitems der Skala
  #	missingValues: optional, Vektor aus numerischen Werten, die missings bezeichnen sollen
  allVar<- list(vc = variableCols)
  allNam<- lapply(allVar, FUN=function(ii) {eatTools::existsBackgroundVariables(dat = dat, variable=ii)})
### 1. Itemkennwerte umkodieren, falls noetig
  # wenn missing values gegeben sind, daten umkodieren!
  if(!is.null(missingValues)) {
     for ( j in allNam[["vc"]] ) {
         if (any(missingValues %in% dat[,j])) {
             dat[which(dat[,j] %in% missingValues),j] <- NA
         }
     }
  }
### 2. Itemkennwerte zurueckgeben
  allValues <- names(eatTools::tableUnlist(dat[,allNam[["vc"]]]))
  ret2<- lapply(allNam[["vc"]], FUN = function ( vname ) {
         tab     <- eatTools::tablePattern(dat[,vname], pattern = allValues)
         results <- data.frame ( v1 = as.character(c(length(stats::na.omit(dat[,vname])), length(dat[,vname]),
                                 format(round(mean( dat[,vname], na.rm=TRUE),digits = 2), nsmall = 2),
                                 format(round(stats::sd( dat[,vname], na.rm=TRUE),digits = 2),nsmall = 2),
                                 eatTools::crop(format(round( 100*as.vector(tab/sum(tab)) ,digits = 2),nsmall = 2)),
                                 eatTools::crop(format(round(100*length(which(is.na(dat[,vname]))) / nrow(dat),digits = 2),nsmall = 2)),
                                 as.vector(tab),
                                 length(which(is.na(dat[,vname]))))),stringsAsFactors = FALSE)
         return(results)})
  ret2<- do.call("cbind", ret2)
  colnames(ret2) <- allNam[["vc"]]
  rowNames       <- c("N.valid", "N.total", "mean.valid", "sd.valid", paste(allValues, "total", sep="."), "sysmis.total", paste(allValues, "totalabs", sep="."), "sysmis.totalabs")
  stopifnot(length(rowNames) == nrow(ret2))
  rownames(ret2) <- rowNames
  ret2 <- list(as.matrix(ret2))                                                 ### urspruengliche Struktur von Felix replizieren (Liste mit einem Objekt, einer character-Matrix)
  return(ret2)}

## check:
## load("c:/Diskdrv/Winword/Psycho/IQB/Repositories/eatCodebook/tests/testthat/dat.rda")
## kennwerte.gepoolt.metrisch(datWide = dat, imputedVariableCols = 3:6)
kennwerte.gepoolt.metrisch <- function( datWide, imputedVariableCols) {
  # INPUT
  # datWide: Datensatz im Wideformat (!!)
  #          in diesem datensatz muss zwingend jede zeile einer person entsprechen!
  #          in diesem Fall braucht man keine ID-variable, die wird spaeter kuenstlich erzeugt
  #	imputedVariableCols: Spaltennummern oder Namen der Imputationen der Variablen
### ACHTUNG! da es sich hier um imputierte Variablen handelt, findet KEIN missing handling mehr statt!
### Funktion repliziert den Output von Felix Originalfunktion
### um es gegenzuchecken, 't:/Sebastian/sh_functions.r' sourcen und folgendes aufrufen:
### load("T:/sebastian/dat.rda")
### skalen.info <- data.frame ( Var.Name = "DM_erfahrung", Quelle = "sfb", Items.der.Skala = paste("Semz19_", letters[1:4], sep="", collapse=", ") , stringsAsFactors = FALSE)
### kennwerte.gepoolt.metrisch ( name="DM_erfahrung" , id.fb="IDSTUD" , Gesamtdatensatz=dat, skalen.info=skalen.info)
  allVar<- list(vc = imputedVariableCols)
  allNam<- lapply(allVar, FUN=function(ii) {eatTools::existsBackgroundVariables(dat = datWide, variable=ii)})
### check, ob es numerisch ist
  stopifnot(length(setdiff(sapply(datWide[,allNam[["vc"]]], class), c("numeric", "integer")))==0)
### pseudo-id erzeugen ... wenn es es wide-format Datensatz ist, muss keine id vorgegeben weren
  datWide[,"id"] <- paste0("P", 1:nrow(datWide))
### long format datensatz
  z <- reshape2::melt( data=datWide , id.vars = "id", measure.vars = allNam[["vc"]], na.rm=TRUE)

### Berechnung der gepoolten Kennwerte
  means <- eatRep::repMean( datL=z, ID = "id" , dependent = "value" ,  imp = "variable",  na.rm = TRUE )
  resM  <- eatRep::report(means, exclude = "var")

# Minimum - kleinster Wert aller aufsummierten Imputationswerte einer Person
  min.valid <- formatC ( min( rowSums(datWide[, allNam[["vc"]]] , na.rm = FALSE )/length(allNam[["vc"]]) , na.rm=TRUE), format = "f" , digits = 1 )

# Maximum - groesster Wert aller aufsummierten Imputationswerte einer Person
  max.valid <- formatC ( max( rowSums(datWide[, allNam[["vc"]]] , na.rm = FALSE )/length(allNam[["vc"]]) , na.rm=TRUE), format = "f" , digits = 1 )

### rueckgabeobjekt bauen (genamter Vektor)
  ret <- c(N.valid= as.character(resM[which(resM[,"parameter"] == "NcasesValid"),"est"]),
           mean.valid = formatC(resM[which(resM[,"parameter"] == "mean"),"est"], format="f", digits =2),
           sd.valid =  formatC(resM[which(resM[,"parameter"] == "sd"),"est"], format="f", digits =2),
           max.valid =max.valid,  min.valid=min.valid,
           sysmis.totalabs = as.character(nrow(datWide) - resM[which(resM[,"parameter"] == "NcasesValid"),"est"]))
  return(ret)}


## load("c:/Diskdrv/Winword/Psycho/IQB/Repositories/eatCodebook/tests/testthat/dat.rda")
## kennwerte.gepoolt.kategorial ( datWide=dat, imputedVariableCols=3:6 )
kennwerte.gepoolt.kategorial <- function( datWide, imputedVariableCols ) {
### wie mit benjamin besprochen: Funktion erlaubt nur NAs als missings, keine -98 etc.
  # INPUT
  # datWide: Datensatz im Wideformat (!!)
  #          in diesem datensatz muss zwingend jede zeile einer person entsprechen!
  #          in diesem Fall braucht man keine ID-variable, die wird spaeter kuenstlich erzeugt
  #	imputedVariableCols: Spaltennummern oder Namen der Imputationen der Variablen
  allVar<- list(vc = imputedVariableCols)
  allNam<- lapply(allVar, FUN=function(ii) {eatTools::existsBackgroundVariables(dat = datWide, variable=ii)})
### pseudo-id erzeugen ... wenn es es wide-format Datensatz ist, muss keine id vorgegeben weren
  datWide[,"id"] <- paste0("P", 1:nrow(datWide))
### long format datensatz
  z <- reshape2::melt( data=datWide , id.vars = "id", measure.vars = allNam[["vc"]], na.rm=FALSE)
### nur valide werte
  cat("Analysis of valid values: ")
  res  <- eatRep::repTable( datL=z, ID = "id" , dependent = "value" ,  imp = "variable",  separate.missing.indicator = FALSE, na.rm=TRUE )
  ret  <- eatRep::report(res)
  retA <- formatC(100*ret[,"est"], format="f", digits=1)                        ### aufbereiten
  names(retA) <- paste(ret[,"parameter"], "valid", sep=".")
### alle Werte
  if(any(is.na(z[,"value"]))) {
      cat("Analysis of total values: ")
      res1 <- eatRep::repTable( datL=z, ID = "id" , dependent = "value" ,  imp = "variable",  separate.missing.indicator = TRUE, na.rm=FALSE, forceTable=TRUE )
      ret1 <- eatRep::report(res1)
      weg  <- match(".NA.", ret1[,"parameter"])                                 ### Ergebnisse aufbereiten, in der richtigen Reihenfolge
      ret1A<- formatC(100* c(ret1[-weg,"est"],ret1[weg,"est"]), format="f", digits=1)
      names(ret1A) <- c(paste(ret1[-weg,"parameter"], "total", sep="."),"sysmis.total")
  }  else  {
      ret1A <- NULL
  }
### Fallzahlen (etwas effizienter)
  bool<- !is.na(datWide[,allNam[["vc"]]])
  N.valid<- length(which(rowSums(bool) == length(allNam[["vc"]])))
  #### Output ####

### felix berechnet absolute Haeufigkeiten bei imputierter Variablen ... finde ich etwas komisch,
### aber der konsistenz zuliebe passiert es jetzt hier auch
  absfreqs <- rowMeans(sapply(datWide[,imputedVariableCols], FUN = table, useNA="al"))
  absf     <- as.character(round( absfreqs, digits = 0))
  names(absf) <- paste(c(names(absfreqs)[-length(absfreqs)], "sysmis"), "totalabs", sep=".")
  ret.var <- c( N.valid=as.character(N.valid) , N.total=as.character(nrow(datWide)) , retA, sysmis.valid = "\\multic{--}", ret1A, absf)
  return( ret.var )
}


