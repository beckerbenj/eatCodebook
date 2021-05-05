# Funktion zum erstellen des Kennwertedatensatzes
kds.erstellen <- function( fbshort, varue.info, varue.missings , skalen.info, ds , variablen., id) {
  # OUTPUT
  #	Kennwertedatensatz: data.frame

  Kennwertedatensatz <- sapply ( variablen. , function(v) {
    kennwerte( v , id.fb=id, varue.info=varue.info , varue.missings=varue.missings, Gesamtdatensatz=ds,
               skalen.info=skalen.info[ skalen.info$Quelle %in% fbshort ,] )
    })

  Kennwertedatensatz
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
  kennwerte.var
}


