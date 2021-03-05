# Funktion zum Einlesen der Datensätze
ds.einlesen <- function ( d , use.value.labels=FALSE , do.print=TRUE) {
  # INPUT
  #	d: Character, Pfad des einzulesenden Datensatzes

  # OUTPUT
  #	e: data.frame, eingelesener Datensatz

  # ANMERKUNG:
  #	Diese Funktion wird auf alle Pfade angewandt, aus denen
  #	Datensätze eingelesen werden soll und berücksichtigt
  #	dabei nur R-Datensätze ("rdata", "rda") und SPSS-Datensätze
  #	("sav").

  if(! file.exists(d) ){
    stop(" Fehler! Pfad existiert nicht")
  }

  dattype <- toupper ( sub ( ".*\\.(.*)$" , "\\1" , d ) )
  if(dattype %in% "SAV") {
    dattype.com <- " .sav (SPSS, das kann ne Weile dauern, je nach Größe des Datensatzes)"
  } else if ( dattype %in% c("RDATA","RDA") ){
    dattype.com <- paste0(" .",tolower(dattype)," (R-Datensatz)")
  } else if ( dattype %in% "CSV" ){
    dattype.com <- paste0(" .",tolower(dattype)," (Textdatei , Semikolon-getrennt)")
  } else if ( dattype %in% "DAT" ){
    dattype.com <- paste0(". ",tolower(dattype)," (Textdatei, Tabstopp-getrennt)")
  } else {
    dattype.com <- tolower(dattype)
    stop(" Fehler! Es können nur Datensätze aus SPSS (.sav), R (.rda oder .rdata) oder Textdateien (.csv, .dat) eingelesen werden.")
  }

  if ( dattype %in% "SAV" ) {
    if(do.print) {
      cat ( paste0 (" Lese Datensatz: ", d , "\n" , "  Datentyp: " , dattype.com ,"\n" ) )
      flush.console()
      cat ( paste0 ("  Größe: ", round(file.info(d)$size/(1024*1024),2) ," MB \n" ) )
      flush.console()
    }

    ### Benjamin 29.03.: Bug bei foreign, hat gelöschte Variablen "wiederbelebt"; langfristig: Umstellung auf haven, dann muss alles andere angepasst werden
    e <- suppressWarnings ( foreign::read.spss ( d ,
                                        use.value.labels = use.value.labels ,
                                        to.data.frame = !use.value.labels ,
                                        use.missings = use.value.labels
    ) )
    # Hotfix: Variablen entfernen, die nicht von haven importiert werden
    have <- haven::read_sav(d, user_na = TRUE)
    emptyVars <- which(!names(e) %in% names(have))
    if(length(emptyVars) > 0) e <- e[,-emptyVars]
    ### atomic loswerden

    if(!use.value.labels ){
      cat ( paste0 ("  Datensatz-Attribute loswerden.\n" ) )
      flush.console()

      var. <- sapply( colnames(e) , function(d) ! is.null(attributes(e[,d])))
      do <- paste0("e$", colnames ( e[var.]),  " <- as.vector ( e$", colnames ( e[var.] ) ," ) " )

      eval ( parse ( text = do ) )

      # Datensatz-Attribute loswerden
      attr ( e , "codepage" ) <- NULL
      attr ( e , "variable.labels" ) <- NULL
    }

    ## Hotfix

  } else if ( dattype %in% c("RDATA","RDA") ) {
    if(do.print) {
      cat ( paste0 (" Lese Datensatz: ", d , "\n" , "Datentyp: " , dattype.com ,"\n" ) )
      flush.console()
      cat ( paste0 (" Größe: ", round(file.info(d)$size/(1024*1024),2) ," MB \n" ) )
      flush.console()
    }

    e.char <- load ( d )
    e <- get ( e.char )
    e <- data.frame ( e )
    do <- paste0("e$", colnames ( e ),  " <- as.vector ( e$", colnames ( e ) ," ) " )
    eval ( parse ( text = do ) )
  } else if( dattype %in% c("TXT","DAT") ) {
    if(do.print) {
      cat ( paste0 (" Lese Datensatz: ", d , "\n" , "Datentyp: " , dattype.com ,"\n" ) )
      flush.console()
    }
    e <- read.table(d , sep="\t", header=TRUE)
  } else if( dattype %in% c("TXT","CSV") ) {
    if(do.print) {
      cat ( paste0 (" Lese Datensatz: ", d , "\n" , "Datentyp: " , dattype.com ,"\n" ) )
      flush.console()
    }
    e <- read.csv2(d , sep=";", header=TRUE)
  } else {
    if(do.print) {
      cat(paste0(" Datentyp \'" , dattype , "\' nicht einlesbar. Es wird NULL zurückgegeben.\n"))
      flush.console()
    }
    e <- NULL
  }

  cat ( "\n" )
  flush.console()

  return ( e )
}
