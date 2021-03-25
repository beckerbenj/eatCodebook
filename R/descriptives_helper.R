
# INPUT
#	x: actual data vector
#	missings: vector of variable values which are missing codes


kennwerte.kategorial <- function(x, value_table) {
  missings <- value_table[value_table$missings == "miss", "value"]
  unique_values <- value_table$value

  # formerly a separate function ("variation") -> if no value labels are used
  if(length(unique_values) == length(missings)) unique_values <- c(sort(unique(x[!x %in% missings  & ! is.na(x)])),
                                                                   missings)

  not_labeled_values <- unique(x[!x %in% unique_values & !is.na(x)])
  if(length(not_labeled_values) > 0) stop("The following values are not labeled: ",
                                          paste(not_labeled_values, collapse = ", "))

  #### Berechnung der Häufigkeiten ####
  werte.valid <- x[!x %in% missings  & ! is.na(x)]
  werte.total <- x
  if(!is.numeric(x)){
    for(k in unique_values){
      werte.valid[grepl(paste0("^\\s*",k,"\\s*$") , werte.valid)] <- k
      werte.total[grepl(paste0("^\\s*",k,"\\s*$") , werte.total)] <- k
    }
    werte.valid[grepl("^\\s*$" , werte.valid)] <- NA
    werte.valid <- werte.valid[! werte.valid %in% missings  & ! is.na( werte.valid )]
    werte.total[grepl("^\\s*$" , werte.total)] <- NA
  } #else {
  #  werte.valid <- as.numeric(gsub("\\s" , "" , x[!x %in% missings  & !is.na(x)]))
  #  werte.total <- as.numeric(gsub("\\s" , "" , x))
  #}

  # Valide und totale Fallzahlen
  N.valid <- c("N.valid" = length(werte.valid))
  N.total	<- c("N.total" = length(werte.total))

  # absolute Häufigkeiten
  werte.total.abs <- as.numeric(table(factor(werte.total, levels = unique_values) , useNA = "always"))

  # relative Häufigkeiten (alle Fälle)
  werte.total.frq	<- 100* as.numeric(table(factor(werte.total, levels = unique_values) , useNA = "always")) / N.total

  # relative Häufigkeiten (valide Fälle)
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
  names(werte.total.frq) <- paste0(c(unique_values , "sysmis" ) , ".total" )

  # Formatierung und Names der absoluten Werte
  werte.total.abs <- formatC(werte.total.abs, format = "f", digits = 0 )
  names(werte.total.abs) <- paste0(c(unique_values, "sysmis" ), ".totalabs" )

  c(N.valid, N.total, werte.valid.frq, werte.total.frq, werte.total.abs)
}


kennwerte.ordinal <- function(x, value_table) {
  cat_values <- kennwerte.kategorial(x, value_table)

  missings <- value_table[value_table$missings == "miss", "value"]
  werte.valid <- x[!x %in% missings  & ! is.na(x)]

  #### Berechnung der metrischen Kennwerte ####
  # Arithmetisches Mittel
  mean.valid <- formatC(mean(werte.valid), format = "f", digits = 2)
  names(mean.valid) <- "mean.valid"

  # Standardabweichung
  sd.valid <- formatC(sd(werte.valid), format = "f", digits = 2)
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
  missings <- value_table[value_table$missings == "miss", "value"]
  unique_values <- value_table$value

  werte.valid <- x[!x %in% missings  & ! is.na(x)]

  N.valid <- length(werte.valid)
  mean.valid <- formatC(mean(werte.valid), format = "f", digits = 2 )
  sd.valid <- formatC(sd(werte.valid), format = "f", digits = 2 )
  min.valid <- formatC(min(werte.valid), format = "f", digits = 1 )
  max.valid <- formatC(max(werte.valid), format = "f", digits = 1 )
  sysmis.totalabs <- length(which(is.na(x)))

  c("N.valid" = N.valid , "mean.valid" = mean.valid , "sd.valid" = sd.valid,
   "min.valid" = min.valid, "max.valid" = max.valid , "sysmis.totalabs" = sysmis.totalabs)
}


###### move on from here
# how to integrate variable sets (items of scales?)

kennwerte.skala <- function(name,varue.missings,Gesamtdatensatz,skalen.info) {
  # INPUT
  #	name: Name der Variable, wie sie in der Varue erscheint
  #	varue.missings: Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: Datensatz des Fragebogens
  #	skalen.info: Übersicht der Skaleninformationen
  # OUTPUT:
  #	ret.var: Liste mit zwei Einträgen:
  #			 Erster Listeneintrag ist ein Vektor mit den metrischen Kennwerten der Skala (M, SD, Min, Max, Cronbachs Alpha)
  #			 Zweiter Listeneintrag ist ein data.frame mit den ordinalen Kennwerten der

  #### Vorbereitung ####
  # Identifikation der Items aus Skaleninfo in Varue
  skala.items	 <- sub( "^\\s*", "", unlist( strsplit( skalen.info[ tolower( skalen.info$Var.Name ) %in% tolower( name ), "Items.der.Skala" ], ",", fixed = TRUE ) ) )

  # Reduzierte Varue für Items
  varue.missings.items <- varue.missings[varue.missings$Var.name %in% skala.items,]

  # Identifikation derjenigen Fälle, die auf allen Items valide Angaben haben
  x <- sapply ( skala.items , function (d) ! Gesamtdatensatz [ , d ] %in% varue.missings.items$Wert[varue.missings.items$missing %in% "ja" & varue.missings.items$Var.name %in% d])
  y <- sapply ( 1:length(Gesamtdatensatz[,1]) , function (d) all( x[d,] ) )


  #### Berechnung der metrischen Kennwerte ####

  # Kennwerte der Skala
  skala.kennwerte <- as.data.frame( c( kennwerte.metrisch( name=name,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz ), sub( ".*(\\..*)", "\\1", formatC( cronbach( Gesamtdatensatz[ y , skala.items] )$alpha[1] , format="f", digits=2) ) ) , optional = TRUE)
  names( skala.kennwerte ) <- name

  skala.kennwerte[,1] <- as.character(skala.kennwerte[,1])
  rownames( skala.kennwerte )[length(skala.kennwerte[,1])] <- "alpha"

  # Kennwerte der Items
  items.kennwerte <- lapply ( skala.items , kennwerte.ordinal.skala ,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz )
  names(items.kennwerte) <- skala.items
  vals <- unique(unname(unlist(lapply(skala.items , function(d) names(items.kennwerte[[d]])))))
  for(i in skala.items){
    if( any(!vals %in% names(items.kennwerte[[i]]))){
      w <- vals[which(!vals %in% names(items.kennwerte[[i]]))]
      k <- rep("\\multic{--}" , length(w))
      names(k) <- w
      items.kennwerte[[i]] <- c(items.kennwerte[[i]] , k)
    }
    items.kennwerte[[i]] <- items.kennwerte[[i]][vals]
  }
  items.kennwerte <- do.call("cbind" , items.kennwerte)

  cor.valid <- sub("^.*(\\..*)" , "\\1" , formatC( alpha( Gesamtdatensatz[ y , skala.items ] )$item.stats$r.drop , format = "f" , digits = 2 ) )
  names ( cor.valid ) <- skala.items
  items.kennwerte <- rbind ( items.kennwerte, cor.valid )


  #### Output ####

  ret.var <- list( skala.kennwerte , items.kennwerte )
  return( ret.var )

}


kennwerte.skala.fake <- function(name,varue.missings,Gesamtdatensatz,skalen.info) {
  # INPUT
  #	name: Name der Variable, wie sie in der Varue erscheint
  #	varue.missings: Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: Datensatz des Fragebogens
  #	skalen.info: Übersicht der Skaleninformationen
  # OUTPUT:
  #	ret.var: Liste mit zwei Einträgen:
  #			 Erster Listeneintrag ist ein Vektor mit den metrischen Kennwerten der Skala (M, SD, Min, Max, Cronbachs Alpha)
  #			 Zweiter Listeneintrag ist ein data.frame mit den ordinalen Kennwerten der

  #### Vorbereitung ####
  # Identifikation der Items aus Skaleninfo in Varue
  skala.items	 <- gsub( "\\s", "", unlist( strsplit( skalen.info[ tolower( skalen.info$Var.Name ) %in% tolower( name ), "Items.der.Skala" ], ",", fixed = TRUE ) ) )

  skala.items <- skala.items[skala.items %in% varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja" , "sh", "ds")]]

  #### Berechnung der metrischen Kennwerte ####

  # Kennwerte der Items
  items.kennwerte <-  lapply ( skala.items , kennwerte.ordinal.skala ,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz )
  names(items.kennwerte) <- skala.items
  vals <- unique(unname(unlist(lapply(skala.items , function(d) names(items.kennwerte[[d]])))))
  for(i in skala.items){
    if( any(!vals %in% names(items.kennwerte[[i]]))){
      w <- vals[which(!vals %in% names(items.kennwerte[[i]]))]
      k <- rep("\\multic{--}" , length(w))
      names(k) <- w
      items.kennwerte[[i]] <- c(items.kennwerte[[i]] , k)
    }
    items.kennwerte[[i]] <- items.kennwerte[[i]][vals]
  }
  items.kennwerte <- do.call("cbind" , items.kennwerte)

  #### Output ####

  return( list(items.kennwerte) )
}


kennwerte.gepoolt.metrisch <- function( name , id.fb , Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Name der Variable, wie sie in der Varue erscheint
  #	id.fb: Name der Identifikiationsvariable im Datensatz
  #	Gesamtdatensatz: Datensatz des Fragebogens
  #	skalen.info: Übersicht der Skaleninformationen
  # OUTPUT:
  #	ret.var: Vektor mit metrischen Kennwerten


  #### Vorbereitung ####

  # Identifikation der Items aus Skaleninfo in Varue
  skala.items	<- gsub( "\\s*", "", unlist( strsplit( skalen.info[ tolower( skalen.info$Var.Name ) %in% tolower( name ), "Items.der.Skala" ], ",", fixed = TRUE ) ) )

  # Reduzierter Gesamtdatensatz x
  x <- Gesamtdatensatz[,c(id.fb, skala.items)]
  x <- as.data.frame( sapply ( names(x) , function(d) as.numeric ( x[,d] ) ) )

  # Erstellung eines Datensatzes im Longformat - Grundlage fürs Poolen
  z <- melt( data=x , id.vars = id.fb, measure.vars = skala.items)

  # Zusätzliche Variable, die die Imputation angibt --> siehe Erläuterung zu Beginn der Funktion
  df_imp <- data.frame("Variable"=skala.items , "Imp"=1:length(skala.items))
  z[,"imp"] <- as.numeric(sapply(as.character(z[,"variable"]) , function(d) df_imp$Imp[df_imp$Variable %in% d] , USE.NAMES=FALSE))
  #z[,"imp"] <- as.numeric(unlist(lapply(strsplit(as.character(z[,"variable"]),"\\."), FUN = function ( l ) { l[2]})))

  # "Value"-Spalte numerisch
  z[,"value"] <- as.numeric(z[,"value"])


  #### Berechnung der gepoolten Kennwerte ####

  # Berechnung der metrischen Kennwerte über jk2.mean
  means <- jk2.mean( datL=z, ID = id.fb , dependent = "value" ,  imp = "imp",  na.rm = TRUE )

  #### Achtung, Fix Benjamin & Johanna (13.03.2019, da sich eatRep Ergbnisstruktur geändert hat)
  means <- means$resT$noTrend

  # Valide Fallzahl
  N.valid <- means$value[means$parameter %in% "NcasesValid" & means$coefficient %in% "est"]

  # Arithmetisches Mittel
  mean.valid <- formatC ( means$value[means$parameter %in% "mean" & means$coefficient %in% "est"]  , format = "f" , digits = 2 )

  # Standardabweichung
  sd.valid <- formatC ( means$value[means$parameter %in% "sd" & means$coefficient %in% "est"] , format = "f" , digits = 2 )

  # Minimum - kleinster Wert aller aufsummierten Imputationswerte einer Person
  min.valid <- formatC ( min( rowSums(Gesamtdatensatz[, skala.items] , na.rm = FALSE )/length(skala.items) , na.rm=TRUE), format = "f" , digits = 1 )

  # Maximum - größter Wert aller aufsummierten Imputationswerte einer Person
  max.valid <- formatC ( max( rowSums(Gesamtdatensatz[, skala.items] , na.rm = FALSE )/length(skala.items) , na.rm=TRUE), format = "f" , digits = 1 )

  # Sysmis Zahl
  sysmis.totalabs <- length(Gesamtdatensatz[,1]) - as.numeric(N.valid)

  # Names
  names(N.valid) <- "N.valid"
  names(mean.valid) <- "mean.valid"
  names(sd.valid) <- "sd.valid"
  names(max.valid) <- "max.valid"
  names(min.valid)<- "min.valid"
  names(sysmis.totalabs) <- "sysmis.totalabs"

  #### Output ####

  ret.var <- c( N.valid , mean.valid , sd.valid , max.valid , min.valid ,sysmis.totalabs)
  return( ret.var )
}


kennwerte.gepoolt.kategorial <- function( name , id.fb, varue.missings, Gesamtdatensatz, skalen.info ) {
  # YRNTCSH! - Anpassung der Identifikation der Imputation: Der Datenatz z wird im longformat erstellt und benötigt für die weiteren Analysen die Spalte "imp". Diese Spalte wird über den Befehl strplit erzeugt, indem die Variablennamen der Imputationen übergeben werden und nach einer passenden Zeichenfolge aufgespalten werden. Die Spalte "imp" wird also auf Grundlage der Variablennamen der Imputationen erkannt. Im LV12 endeten die Imputationsnamen einheitlich auf ".NR", im LV15 auf "_NR". Für den LV12 wurde daher "imp" über strsplit(as.character(z[,"variable"]),"\\.") gebildet, im LV15 über strsplit(as.character(z[,"variable"]),"\\_").


  # INPUT
  #	name: Name der Variable, wie sie in der Varue erscheint
  #	id.fb: Name der Identifikiationsvariable im Datensatz
  #	name: Name der Variable, wie sie in der Varue erscheint
  #	varue.missings: Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: Datensatz des Fragebogens
  #	skalen.info: Übersicht der Skaleninformationen
  # OUTPUT:
  #	ret.var: Vektor mit metrischen Kennwerten

  #### Vorbereitung ####

  # Identifikation der Items aus Skaleninfo in Varue
  skala.items	<- gsub( "\\s*", "", unlist( strsplit( skalen.info[ tolower( skalen.info$Var.Name ) %in% tolower( name ), "Items.der.Skala" ], ",", fixed = TRUE ) ) )

  # Kategorien/ Werte
  kat <- varue.missings$Wert[ varue.missings$Var.name %in% name ]

  cat(paste0("  Aufbereitung des Datensatzes: Reduktion+Longformat+Identifikation der Imputationen.\n"))
  flush.console()
  # Reduzierter Gesamtdatensatz
  x <- Gesamtdatensatz[,c(id.fb, skala.items)]

  # Erstellung eines Datensatzes im Longformat - Grundlage fürs Poolen
  z <- melt( data=x , id.vars = id.fb, measure.vars = skala.items)

  # Zusätzliche Variable, die die Imputation angibt --> siehe Erläuterung zu Beginn der Funktion
  df_imp <- data.frame("Variable"=skala.items , "Imp"=1:length(skala.items))
  z[,"imp"] <- as.numeric(sapply(as.character(z[,"variable"]) , function(d) df_imp$Imp[df_imp$Variable %in% d] , USE.NAMES=FALSE))
  #z[,"imp"] <- as.numeric(unlist(lapply(strsplit(as.character(z[,"variable"]),"\\_"), FUN = function ( l ) { l[2]})))

  # "Value"-Spalte numerisch
  #z[,"value"] <- as.numeric(z[,"value"])

  # Long-Datensatz für valide relative Häufigkeiten
  zR <- z[!is.na(z[,"value"]),]


  #### Berechnung der gepoolten Häufigkeiten ####
  #if(name == "buecher_gepoolt") browser()
  ### Relative, totale Häufigkeiten (mit Missing Indikator (workaround, funktion macht seltsame dinge))
  cat(paste0("  Poolen über alle Kategorien (inkl. Missing).\n"))
  flush.console()
  total <- jk2.table( datL=z, ID = id.fb , dependent = "value" ,  imp = "imp", doCheck = FALSE, separate.missing.indicator = TRUE )

  #### Achtung, Fix Benjamin & Johanna (28.03.2019, da sich eatRep Ergbnisstruktur geändert hat)
  total <- total$resT$noTrend

  # Falsch gelabelte NAs richtig benennen
  if( ".NA." %in% total$parameter ) {
    if( "NA" %in% total$parameter ) {
      total <- total[ !total$parameter %in% "NA",]
      total$parameter[total$parameter %in% ".NA."] <- "NA"
      total <- rbind(total[ ! total$parameter %in% "NA" ,] , total[ total$parameter %in% "NA" ,])
    } else {
      total$parameter[total$parameter %in% ".NA."] <- "NA"
      total <- rbind(total[ ! total$parameter %in% "NA" ,] , total[ total$parameter %in% "NA" ,])
    }
  }

  if( any( grepl("^\\.*$" , total$parameter) ) ) {
    if( "NA" %in% total$parameter ) {
      total <- total[ !total$parameter %in% "NA",]
      total$parameter[ which(grepl("^\\.*$" , total$parameter)) ] <- "NA"
      total <- rbind(total[ ! total$parameter %in% "NA" ,] , total[ total$parameter %in% "NA" ,])
    } else {
      total$parameter[ which(grepl("^\\.*$" , total$parameter)) ] <- "NA"
      total <- rbind(total[ ! total$parameter %in% "NA" ,] , total[ total$parameter %in% "NA" ,])
    }
  }

  # Fallzahlen
  cat(paste0("  Berechne valide Fallzahl.\n"))
  flush.console()
  bool <- sapply( 1:length(x[,1]) , function(d) all( ! is.na(x[d, 2:(length(skala.items)+1)] ) ) )
  N.total <- length( Gesamtdatensatz[, id.fb] )
  N.valid <- length( which(bool) )
  names(N.total) <- "N.total"
  names(N.valid) <- "N.valid"


  # Parameter aufbereiten --> bei nicht-numerischen Variablen werden Punkte angefügt
  for( k in kat){
    total$parameter[grepl(paste0("^\\.*",k,"\\.*$") , total$parameter)] <- k
  }


  # Relative, totale Häufigkeiten formatieren
  werte.total.frq <- formatC ( 100*total$value[ total$parameter %in% c( kat, "NA" ) & total$coefficient %in% "est"] , format = "f" , digits = 1 )

  ### 02.04.19: Missings aussortieren (neu: auch in gepoolten Variablen jetzt Missings!)
  if(length(name) > 1) stop("Benjamin hat hier nen Fehler gemacht, an ihn wenden.")
  missings <- varue.missings[varue.missings$Var.name == name & varue.missings$missing == "ja", "Wert"]
  zR_noMiss <- zR
  zR_noMiss[zR_noMiss$value %in% missings, "value"] <- NA
  ### Relative, valide Häufigkeiten (ohne Missing Indikator (workaround, funktion macht seltsame dinge));
  # forceTable um Probleme mit Werteset c(0, 1) zu vermeiden, da sonst jk2.mean gecalled wird (Benjamin & Sebastian, 28.03.2019)
  cat(paste0("  Poolen über valide Kategorien (exkl. Missing).\n"))
  flush.console()
  valid <- jk2.table( datL=zR_noMiss, ID = id.fb , dependent = "value" ,  imp = "imp", doCheck = FALSE, separate.missing.indicator = FALSE,
                      forceTable = TRUE)

  #### Achtung, Fix Benjamin & Johanna (13.03.2019, da sich eatRep Ergbnisstruktur geändert hat)
  valid <- valid$resT$noTrend

  if( any(grepl("^\\.*$" , valid$parameter))) valid$parameter[ grepl("^\\.*$" , valid$parameter) ] <- "NA"
  valid <- rbind(valid[ ! valid$parameter %in% "NA" ,] , valid[ valid$parameter %in% "NA" ,])

  # Parameter aufbereiten --> bei nicht-numerischen Variablen werden Punkte angefügt
  for( k in kat){
    valid$parameter[grepl(paste0("^\\.*",k,"\\.*$") , valid$parameter)] <- k
  }

  # Valide Häufigkeiten formatieren
  werte.valid.frq <- formatC ( 100*valid$value[ valid$parameter %in% kat & valid$coefficient %in% "est"] , format = "f" , digits = 1 )


  ### Absolute Häufigkeiten - berechnet aus rel. Hfg., da jk2.table abs. Hfg. nicht berechnet
  werte.total.abs <- formatC ( total$value[ total$parameter %in% c( kat, "NA" ) & total$coefficient %in% "est" ]*N.total  , format = "f" , digits = 0 )

  # Names der totalen Werte
  names( werte.total.frq ) <- paste0( c( kat , "sysmis" ) , ".total" )

  # if(identical(name, "TR_NOTE_DEU_gepoolt")) browser()
  ## Names der validen Werte (02.04., Benjamin: Kategorienamen angepasst, da missings ja wegfallen und leere Zellen bekommen sollen)
  kat_noMiss <- kat[!kat %in% missings]
  kat_Miss <- kat[kat %in% missings]
  miss_cols <- rep("\\multic{--}" , 1+length(kat_Miss))
  werte.valid.frq <- c( werte.valid.frq , miss_cols )
  names( werte.valid.frq ) <- paste0( c( kat_noMiss , kat_Miss, "sysmis" ) , ".valid" )

  # Names der absoluten Werte
  names( werte.total.abs ) <- paste0( c(kat , "sysmis" ) , ".totalabs" )

  #### Output ####

  ret.var <- c( N.valid , N.total , werte.valid.frq , werte.total.frq, werte.total.abs )
  return( ret.var )
}


