kennwerte.kategorial <- function(name , varue.missings , Gesamtdatensatz) {
  # INPUT
  #	name: Name der Variable, wie sie in der Varue erscheint
  #	varue.missings: Variablenübersicht für Werteinformationen
  #	Gesamtdatensatz des Fragebogens
  # OUTPUT:
  #	ret.var: Vektor mit Werten: Valide und totale Fallzahl, relative Häufigkeiten
  #			 bzgl. der validen und totalen Fallzahl und absolute Häufigkeiten

  #### Vorbereitung ####
  # Reduzierte Varue für einfacheres Arbeiten
  varue.missings.aktuell 	<- varue.missings[varue.missings$Var.name %in% name,]

  # Missingkategorien
  missings <- varue.missings.aktuell$Wert[varue.missings.aktuell$missing %in% "ja"]

  #### Berechnung der Häufigkeiten ####


  if(suppressWarnings(any(is.na(as.numeric(Gesamtdatensatz[ ! is.na(Gesamtdatensatz[ , name ]) , name ]))))){
    werte.valid <- Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ]
    werte.total <- Gesamtdatensatz[ , name ]
    for( k in varue.missings.aktuell$Wert ){
      werte.valid[grepl(paste0("^\\s*",k,"\\s*$") , werte.valid)] <- k
      werte.total[grepl(paste0("^\\s*",k,"\\s*$") , werte.total)] <- k
    }
    werte.valid[grepl("^\\s*$" , werte.valid)] <- NA
    werte.valid <- werte.valid[! werte.valid %in% missings  & ! is.na( werte.valid )]
    werte.total[grepl("^\\s*$" , werte.total)] <- NA
  } else {
    werte.valid <- as.numeric(gsub("\\s" , "" , Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ]))
    werte.total <- as.numeric(gsub("\\s" , "" , Gesamtdatensatz[ , name ]))
  }

  # Valide und totale Fallzahlen
  N.valid <- length( werte.valid )
  N.total	<- length( werte.total )
  names(N.valid)	<- "N.valid"
  names(N.total) 	<- "N.total"

  # absolute Häufigkeiten
  werte.total.abs <- as.numeric( table ( factor(werte.total, lev= varue.missings.aktuell$Wert) , useNA = "always") )

  # relative Häufigkeiten (alle Fälle)
  werte.total.frq	<- 100* as.numeric( table ( factor(werte.total, lev= varue.missings.aktuell$Wert) , useNA = "always") ) / N.total

  # relative Häufigkeiten (valide Fälle)
  if(N.valid==0){
    werte.valid.frq <- as.numeric( table ( factor(werte.total, lev= varue.missings.aktuell$Wert ) , useNA = "always") )
  } else {
    werte.valid.frq <- 100* as.numeric( table ( factor(werte.total, lev= varue.missings.aktuell$Wert ) , useNA = "always") ) / N.valid
  }

  #### Aufbereitung der Kennwerte ####

  # Formatierung und Names der validen Werte
  werte.valid.frq <- formatC ( werte.valid.frq , format = "f" , digits = 1 )
  names( werte.valid.frq ) <- paste0(c(varue.missings.aktuell$Wert , "sysmis" ) , ".valid" )
  werte.valid.frq[ paste0( c(missings , "sysmis") , ".valid" ) ] <- "\\multic{--}"

  # Formatierung und Names der totalen Werte
  werte.total.frq <- formatC ( werte.total.frq , format = "f" , digits = 1 )
  names( werte.total.frq ) <- paste0( c(varue.missings.aktuell$Wert , "sysmis" ) , ".total" )

  # Formatierung und Names der absoluten Werte
  werte.total.abs <- formatC ( werte.total.abs , format = "f" , digits = 0 )
  names( werte.total.abs ) <- paste0( c(varue.missings.aktuell$Wert , "sysmis" ) , ".totalabs" )

  #### Output ####
  ret.var <- c( N.valid , N.total , werte.valid.frq , werte.total.frq , werte.total.abs)
  return ( ret.var )
}


kennwerte.kategorial.variation <- function(name, varue.missings , Gesamtdatensatz) {

  # wenn keine Labels vorhanden sind wie bspw. bei psinusteiltransvon (aus dem SLFB)


  # INPUT
  #	name: Name der Variable, wie sie in der Varue erscheint
  #	varue.missings: Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: Datensatz des Fragebogens
  # OUTPUT:
  #	ret.var: Vektor mit Werten: Valide und totale Fallzahl, relative Häufigkeiten
  #			 bzgl. der validen und totalen Fallzahl und absolute Häufigkeiten

  #### Vorbereitung ####

  # Reduzierte Varue für einfacheres Arbeiten
  varue.missings.aktuell 	<- varue.missings[varue.missings$Var.name == name,]

  # Missingkategorien
  missings <- varue.missings.aktuell$Wert[varue.missings.aktuell$missing=="ja"]


  #### Berechnung der Häufigkeiten ####

  # Werte im Datensatz - Grundlage für Häufigkeiten und Fallzahlen
  werte.valid <- Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ]
  werte.total	<- Gesamtdatensatz[ , name ]

  # Wertelabels = Einträge in Gesamtdatensatz
  level.valid	<- sort ( unique(Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ] ) )
  level.total	<- c ( level.valid , missings )

  # Valide und totale Fallzahlen
  N.valid	<- length( werte.valid )
  N.total	<- length( werte.total )
  names( N.valid ) <- "N.valid"
  names( N.total ) <- "N.total"

  # absolute Häufigkeiten
  werte.total.abs <- as.numeric( table ( factor(werte.total, lev= level.total) , useNA = "always") )

  # relative Häufigkeiten
  werte.total.frq <- 100* as.numeric( table ( factor(werte.total, lev= level.total ) , useNA = "always") ) / N.total
  if(N.valid==0){
    werte.valid.frq <- as.numeric( table ( factor(werte.total, lev= level.total ) , useNA = "always") )
  } else {
    werte.valid.frq <- 100* as.numeric( table ( factor(werte.total, lev= level.total ) , useNA = "always") ) / N.valid
  }


  #### Aufbereitung der Kennwerte ####

  # Formatierung und Names der validen Werte
  werte.valid.frq <- formatC ( werte.valid.frq , format = "f" , digits = 1 )
  names( werte.valid.frq ) <- paste0(c( level.total , "sysmis" ), ".valid")
  werte.valid.frq[ paste0(c(missings , "sysmis"), ".valid") ] <- "\\multic{--}"

  # Formatierung und Names der totalen Werte
  werte.total.frq <- formatC ( werte.total.frq , format = "f" , digits = 1 )
  names( werte.total.frq ) <- paste0( c( level.total , "sysmis" ), ".total" )

  # Formatierung und Names der absoluten Werte
  werte.total.abs <- formatC ( werte.total.abs , format = "f" , digits = 0 )
  names( werte.total.abs ) <- paste0( c(level.total , "sysmis" ) , ".totalabs" )

  #### Output ####
  ret.var <- c( N.valid , N.total , werte.valid.frq , werte.total.frq , werte.total.abs )
  return ( ret.var )
}

kennwerte.ordinal <- function(name,varue.missings,Gesamtdatensatz) {
  # INPUT
  #	name: Name der Variable, wie sie in der Varue erscheint
  #	varue.missings: Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: Datensatz des Fragebogens
  # OUTPUT:
  #	ret.var: Vektor mit Werten: Valide und totale Fallzahl, relative Häufigkeiten
  #			 bzgl. der validen und totalen Fallzahl und absolute Häufigkeiten
  #			 und metrische Kennwerte (M, SD)


  #### Vorbereitung ####

  # Reduzierte Varue für einfacheres Arbeiten
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name == name,]

  # Missingkategorien
  missings <- varue.missings.aktuell$Wert[varue.missings.aktuell$missing=="ja"]

  #### Berechnung der Häufigkeiten ####

  # Werte im Datensatz - Grundlage für Häufigkeiten, Fallzahlen und metrische Kennwerte
  if(suppressWarnings(any(is.na(as.numeric(Gesamtdatensatz[! is.na(Gesamtdatensatz[ , name ]) , name ]))))){
    werte.valid <- Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ]
    werte.total <- Gesamtdatensatz[ , name ]
    for( k in varue.missings.aktuell$Wert ){
      werte.valid[grepl(paste0("^\\s*",k,"\\s*$") , werte.valid)] <- k
      werte.total[grepl(paste0("^\\s*",k,"\\s*$") , werte.total)] <- k
    }
    werte.valid[grepl("^\\s*$" , werte.valid)] <- NA
    werte.valid <- werte.valid[! werte.valid %in% missings  & ! is.na( werte.valid )]
    werte.total[grepl("^\\s*$" , werte.total)] <- NA
  } else {
    werte.valid <- as.numeric(gsub("\\s" , "" , Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ]))
    werte.total <- as.numeric(gsub("\\s" , "" , Gesamtdatensatz[ , name ]))
  }

  # Valide und totale Fallzahlen
  N.valid <- length( werte.valid )
  N.total <- length( werte.total )
  names( N.valid ) <- "N.valid"
  names( N.total ) <- "N.total"


  # absolute Häufigkeiten
  werte.total.abs <- as.numeric( table ( factor(werte.total, lev= varue.missings.aktuell$Wert) , useNA = "always") )

  # relative Häufigkeiten
  werte.total.frq <- 100 * as.numeric( table ( factor( werte.total, lev= varue.missings.aktuell$Wert) , useNA = "always") ) / N.total
  if(N.valid==0){
    werte.valid.frq <- as.numeric( table ( factor( werte.total, lev= varue.missings.aktuell$Wert) , useNA = "always") )
  } else {
    werte.valid.frq <- 100 * as.numeric( table ( factor( werte.total, lev= varue.missings.aktuell$Wert) , useNA = "always") ) / N.valid
  }


  # Formatierung und Names der validen Werte
  werte.valid.frq <- formatC ( werte.valid.frq , format = "f" , digits = 1 )
  names( werte.valid.frq ) <- paste0( c(varue.missings.aktuell$Wert , "sysmis" ) , ".valid" )
  werte.valid.frq[ paste0(c(missings , "sysmis"), ".valid") ] <- "\\multic{--}"

  # Formatierung und names der total Werte
  werte.total.frq <- formatC ( werte.total.frq , format = "f" , digits = 1 )
  names( werte.total.frq ) <- paste0( c(varue.missings.aktuell$Wert , "sysmis" ) , ".total" )


  # Formatierung und names der absoluten Werte
  werte.total.abs <- formatC ( werte.total.abs , format = "f" , digits = 0 )
  names( werte.total.abs ) <- paste0( c(varue.missings.aktuell$Wert , "sysmis" ) , ".totalabs" )


  #### Berechnung der metrischen Kennwerte ####

  # Arithmetisches Mittel
  mean.valid <- formatC ( mean( werte.valid ) , format = "f" , digits = 2 )
  names( mean.valid ) <- "mean.valid"


  # Standardabweichung
  sd.valid <- formatC ( sd( werte.valid ) , format = "f" , digits = 2 )
  names( sd.valid ) <- "sd.valid"


  #### Output ####
  ret.var <- c ( N.valid , N.total , mean.valid , sd.valid , werte.valid.frq , werte.total.frq , werte.total.abs )
  return	( ret.var )
}


kennwerte.ordinal.skala <- function(name,varue.missings,Gesamtdatensatz) {
  # INPUT
  #	name: Name der Variable, wie sie in der Varue erscheint
  #	varue.missings: Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: Datensatz des Fragebogens
  # OUTPUT:
  #	ret.var: Vektor mit Werten: Valide und totale Fallzahl, relative Häufigkeiten
  #			 bzgl. totalen Fallzahl und absolute Häufigkeiten und metrische Kennwerte (M, SD)

  #### Vorbereitung ####

  # Reduzierte Varue für einfacheres Arbeiten
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name == name,]

  # Missingkategorien
  missings <- varue.missings.aktuell$Wert[varue.missings.aktuell$missing %in% "ja"]

  #### Berechnung der Häufigkeiten ####

  # Werte im Datensatz - Grundlage für Häufigkeiten, Fallzahlen und metrische Kennwerte
  if(suppressWarnings(any(is.na(as.numeric(Gesamtdatensatz[ ! is.na(Gesamtdatensatz[ , name ]) , name ]))))){
    werte.valid <- Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ]
    werte.total <- Gesamtdatensatz[ , name ]
    for( k in varue.missings.aktuell$Wert ){
      werte.valid[grepl(paste0("^\\s*",k,"\\s*$") , werte.valid)] <- k
      werte.total[grepl(paste0("^\\s*",k,"\\s*$") , werte.total)] <- k
    }
    werte.valid[grepl("^\\s*$" , werte.valid)] <- NA
    werte.valid <- werte.valid[! werte.valid %in% missings  & ! is.na( werte.valid )]
    werte.total[grepl("^\\s*$" , werte.total)] <- NA
  } else {
    werte.valid <- as.numeric(gsub("\\s" , "" , Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ]))
    werte.total <- as.numeric(gsub("\\s" , "" , Gesamtdatensatz[ , name ]))
  }

  # valide und totale Fallzahlen
  N.valid <- length( werte.valid )
  N.total <- length( werte.total )
  names( N.valid ) <- "N.valid"
  names( N.total ) <- "N.total"

  # absolute Häufigkeiten
  werte.total.abs <- as.numeric( table ( factor(werte.total, lev= varue.missings.aktuell$Wert) , useNA = "always") )

  # relative Häufigkeiten
  werte.total.frq <- 100* as.numeric( table ( factor(werte.total, lev= varue.missings.aktuell$Wert) , useNA = "always") ) / N.total


  # Formatierung und Names der total Werte
  werte.total.frq <- formatC ( werte.total.frq , format = "f" , digits = 1 )
  names( werte.total.frq ) <- paste0(c(varue.missings.aktuell$Wert, "sysmis" ) , ".total" )

  # Formatierung und Names der absoluten Werte
  werte.total.abs <- formatC ( werte.total.abs , format = "f" , digits = 0 )
  names( werte.total.abs ) <- paste0( c(varue.missings.aktuell$Wert , "sysmis" ) , ".totalabs" )


  #### Berechnung der metrischen Kennwerte ####

  if(length(werte.valid)==0 | all(is.na(werte.valid))){
    mean.valid <- "\\multic{--}"
    names( mean.valid ) <- "mean.valid"

    # Standardabweichung
    sd.valid <- "\\multic{--}"
    names( sd.valid ) <- "sd.valid"
  } else {
    # Arithmetisches Mittel
    mean.valid <- formatC ( mean( werte.valid ) , format = "f" , digits = 2 )
    names( mean.valid ) <- "mean.valid"

    # Standardabweichung
    sd.valid <- formatC ( sd( werte.valid )  , format = "f" , digits = 2 )
    names( sd.valid ) <- "sd.valid"
  }




  #### Output ####

  ret.var <- c( N.valid , N.total , mean.valid , sd.valid, werte.total.frq , werte.total.abs )
  return( ret.var )
}


kennwerte.metrisch <- function(name,varue.missings,Gesamtdatensatz) {
  # INPUT
  #	name: Name der Variable, wie sie in der Varue erscheint
  #	varue.missings: Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: Datensatz des Fragebogens
  # OUTPUT:
  #	ret.var: Vektor mit Werten: Valide Fallzahl, M, SD, Min und Max


  #### Vorbereitung ####

  # Reduzierte Varue für einfacheres Arbeiten
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name == name,]

  # Missingkategorien
  missings <- varue.missings.aktuell$Wert[varue.missings.aktuell$missing=="ja"]

  #### Berechnung der metrischen Kennwerte ####

  # Werte im Datensatz - Grundlage für Fallzahl und metrische Kennwerte
  if(suppressWarnings(any(is.na(as.numeric(Gesamtdatensatz[ ! is.na(Gesamtdatensatz[ , name ]) , name ]))))){
    werte.valid <- Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ]
    for( k in varue.missings.aktuell$Wert ){
      werte.valid[grepl(paste0("^\\s*",k,"\\s*$") , werte.valid)] <- k
    }
    werte.valid[grepl("^\\s*$" , werte.valid)] <- NA
    werte.valid <- gsub("\\s" , "" , werte.valid)
    werte.valid <- as.numeric(werte.valid)
    werte.valid <- werte.valid[! werte.valid %in% missings  & ! is.na( werte.valid )]
  } else {
    werte.valid <- as.numeric(gsub("\\s" , "" , Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ]))
  }
  # Fallzahl
  N.valid <- length( werte.valid )
  names( N.valid ) <- "N.valid"

  # Arithmetisches Mittel
  mean.valid <- formatC ( mean( werte.valid ) , format = "f" , digits = 2 )
  names( mean.valid ) <- "mean.valid"

  # Standardabweichung
  sd.valid <- formatC ( sd( werte.valid ) , format = "f" , digits = 2 )
  names( sd.valid ) <- "sd.valid"

  # Minimum
  min.valid <- formatC ( min( werte.valid ) , format = "f" , digits = 1 )
  names( min.valid ) <- "min.valid"

  # Maximum
  max.valid <- formatC ( max( werte.valid ) , format = "f" , digits = 1 )
  names( max.valid ) <- "max.valid"

  # Sysmis Zahl
  sysmis.totalabs <- length(which(is.na(Gesamtdatensatz[,name])))
  names(sysmis.totalabs) <- "sysmis.totalabs"

  #### Output ####
  ret.var <- c( N.valid , mean.valid , sd.valid , min.valid, max.valid , sysmis.totalabs)
  return( ret.var )
}


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


#### ZUSAMMENFÜHRUNG ALLER KENNWERTE-FUNKTIONEN ####

kennwerte <- function(name, id.fb, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Name der Variable, wie sie in der Varue erscheint
  #	id.fb: Name der Identifikationsvariable im Datensatz
  #	varue.info: Variablenübersicht der Variableninformationen
  #	varue.missings: Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: Datensatz des Fragebogens
  #	skalen.info: Übersicht der Skaleninformationen
  # OUTPUT:
  #	Aufruf der zu name zugehörigen Kennwerte-Funktion

  #### Vorbereitung ####
  # Identifikation des Layout-Typs

  i <- varue.info$Layout[varue.info$Var.Name %in% name]

  if(is.null(i)){
    stop(" Layout-Typ fehlt (NULL)\n")
  }
  if(is.na(i)){
    stop(" Layout-Typ fehlt (NA)\n")
  }

  if(length(which(varue.info$Var.Name %in% name))>1 ){
    stop(paste0(" Die Variable " , name , " ist mehr als einmal in der Varue.\n"))
  }

  # Ausgabe des Variablennames auf der Konsole
  cat ( paste0 ( " Berechne Kennwerte der Variable: ", name , "\n" ) )
  flush.console()

  #### Berechnung der Kennwerte ####
  if( all( varue.missings[varue.missings$Var.name %in% name, "missing" ] %in% "ja" ) & i==2 ) {
    kennwerte.var <- kennwerte.kategorial.variation(name=name, varue.missings=varue.missings , Gesamtdatensatz=Gesamtdatensatz)
  } else if (i %in% c(0,1,8)) {
    kennwerte.var <- NULL
  } else if (i==2) {
    kennwerte.var <-  kennwerte.kategorial(name=name,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz)
  } else if (i==3) {
    kennwerte.var <-  kennwerte.ordinal(name=name,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz)
  } else if (i==4) {
    kennwerte.var <- kennwerte.metrisch(name=name,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz)
  } else if (i==5) {
    kennwerte.var <- kennwerte.skala(name=name,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info)
  } else if (i==6) {
    kennwerte.var <- kennwerte.gepoolt.metrisch(name=name,id.fb=id.fb,Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info)
  } else if (i==7) {
    kennwerte.var <- kennwerte.gepoolt.kategorial(name=name,id.fb=id.fb,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info)
  } else if (i==9) {
    kennwerte.var <- kennwerte.skala.fake(name=name,varue.info=varue.info,varue.missings=varue.missings,Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info)
  }

  #### Output ####
  return (kennwerte.var)
}
