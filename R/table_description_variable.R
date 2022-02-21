
table_description_variable <- function(name, varue.info, varue.missings=NULL, var.typ, skala.items=NULL, Gesamtdatensatz=NULL, werte=NULL,
                                       show.kategorien=TRUE, gepoolt=FALSE){
  stopifnot(length(name) == 1)

  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name %in% name,]
  nameSH <- gsub( "_" , "\\_" , name , fixed = TRUE)

  # Quellenangabe - falls vorhanden
  quelle <- NULL
  if(!gsub( "\\s" , "" , varue.info.aktuell$QuelleSH ) %in% c("","-") & ! is.na(varue.info.aktuell$QuelleSH) ) {
    quelle <- paste0("Quelle:&", varue.info.aktuell$QuelleSH, "\\\\")
  }

  # Anmerkung, falls vorhanden
  anmerkung <- NULL
  if(!gsub( "\\s" , "" , varue.info.aktuell$Anmerkung.Var ) %in% "-" & ! is.na(varue.info.aktuell$Anmerkung.Var) ) {
    anmerkung <- paste0("Anmerkungen:&", varue.info.aktuell$Anmerkung.Var, "\\\\")
  }

  # Instruktion
  instruktion <- NULL
  if(!gsub( "\\s" , "" , varue.info.aktuell$Instruktionen ) %in% c("", "-") & ! is.na(varue.info.aktuell$Instruktionen) ) {
    instruktion <- paste0("Instruktion:&", varue.info.aktuell$Instruktionen, "\\\\")
  }

  # Bericht der Kategorien
  kategorien <- NULL
  missings <- NULL
  if(!is.null(varue.missings.aktuell) && nrow(varue.missings.aktuell) > 0){
    # Fallunterscheidung: Falls Variable als kategoriale definiert wurde, aber außer Missings keine Kategorien definiert hat
    if( all( varue.missings.aktuell$missing %in% "ja" ) ) {
      # Missingkategorien
      missings <- varue.missings.aktuell$Wert

      # min/max bestimmen
      varDat <- Gesamtdatensatz[, name]
      varDat <- varDat[!varDat %in% missings & !is.na(varDat)]
      if(is.numeric(varDat)){
        min.val <- min(varDat , na.rm =TRUE)
        max.val <- max(varDat , na.rm =TRUE)
      } else {
        show.kategorien <- FALSE
      }

      # Bericht der validen Kategorien im Skript
      kategorien <- NULL
      if(show.kategorien){
        kategorien <- paste0("Kategorien:& " , min.val , "--" ,  max.val  , "\\\\")
      }
    } else {
      # fehlen Werte-Label?
      value_noMissings <- tolower(varue.missings.aktuell$Wert[varue.missings.aktuell$missing %in% "nein"])
      valueLabel_noMissings <- tolower(varue.missings.aktuell$LabelSH[varue.missings.aktuell$missing %in% "nein"])
      bool.kat <- is.na(valueLabel_noMissings) |
        gsub("\\s" , "" , valueLabel_noMissings) %in% "" |
        grepl("kein label vergeben", valueLabel_noMissings, fixed=TRUE)

      names(bool.kat) <- value_noMissings

      kategorien <- NULL
      if(show.kategorien){
        # Vorbereitung für Bercht der validen Kategorien im Skript - Form: "ZAHL~$=$~\\textit{LabelSH};"
        # --> wenn keine Label vergeben, dann in der Form "Kategorien: 1-6"; wenn nur der hoechste und der kleinste Wert Label haben, dann "1=X bis 6=Y"
        if(suppressWarnings(any(is.na(as.numeric(value_noMissings))))){
          bool.kat.bis <- all(! bool.kat[ as.character(sort(value_noMissings)[c(1, length(value_noMissings))])]) &
            all(bool.kat[ ! names(bool.kat) %in% as.character(sort(value_noMissings)[c(1, length(value_noMissings))])]) &
            length(bool.kat)>2
        } else {
          bool.kat.bis <- all(! bool.kat[ as.character(sort(as.numeric(value_noMissings))[c(1, length(value_noMissings))])]) &
            all(bool.kat[ ! names(bool.kat) %in% as.character(sort(as.numeric(value_noMissings))[c(1, length(value_noMissings))])]) &
            length(bool.kat)>2
        }

        if(all( bool.kat ) ){
          if(all(sapply(suppressWarnings(as.numeric(value_noMissings)) , is.numeric))){
            minVal <- min(as.numeric(value_noMissings , na.rm=TRUE))
            maxVal <- max(as.numeric(value_noMissings , na.rm=TRUE))
            kategorien <- paste0( "Kategorien:& " , minVal , "--" , maxVal , "\\\\" )
          } else {
            kategorien <- NULL
          }
        } else if ( bool.kat.bis){
          minVal <- paste0(value_noMissings[which.min(value_noMissings)] , "~$=$~\\textit{", valueLabel_noMissings[which.min(value_noMissings)], "}")
          maxVal <- paste0(value_noMissings[which.max(value_noMissings)] , "~$=$~\\textit{" , valueLabel_noMissings[which.max(value_noMissings)], "}")

          kategorien <- paste0( "Kategorien:& " , minVal , " bis " , maxVal , "\\\\" )
        } else { # Wenn mindestens ein Label vorliegt
          # alle Werte ohne Labels bekommen "kein Label vergeben" (in kursiv)

          varue.missings.aktuell$LabelSH[ is.na(varue.missings.aktuell$LabelSH) & varue.missings.aktuell$missing %in% "nein"] <- "(kein Label vergeben)"
          varue.missings.aktuell$LabelSH[ gsub("\\s" , "" , varue.missings.aktuell$LabelSH) %in% "" & varue.missings.aktuell$missing %in% "nein" ] <- "(kein Label vergeben)"
          # Aufbereitung der Labels
          label.nonmiss <- paste0("~$=$~\\textit{", varue.missings.aktuell$LabelSH[varue.missings.aktuell$missing %in% "nein"], "}" )
          label.nonmiss[1:(length(label.nonmiss)-1)] <- paste0( label.nonmiss[1:(length(label.nonmiss)-1)] , "; " )
          # Aufbereitung der Werte --> Wenn Zeilenumbruch vor Wert, dann "\\\\ \n & " vor den Wert
          kat_werte <- varue.missings.aktuell$Wert
          no_miss_yes_zeilenumbruch <- varue.missings.aktuell$missing=="nein" & varue.missings.aktuell$Zeilenumbruch_vor_Wert=="ja"
          kat_werte[no_miss_yes_zeilenumbruch] <- paste0("\\\\ \n & ", kat_werte[no_miss_yes_zeilenumbruch])
          kat_werte <- kat_werte[varue.missings.aktuell$missing=="nein"]
          # Zusammenfügen von Werten und Labels
          label.nonmiss <- paste0( kat_werte , label.nonmiss )
          # Bericht der Kategorien im Skript
          kategorien <- paste0("Kategorien:& " , paste0(label.nonmiss, collapse="") , "\\\\" )
        }
      }
    }

    # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet, das wird hier korrigiert
    names(werte) <- sub( paste0(name,"\\.(.*)$") , "\\1" , names(werte) )


    #### 16.04.: Hotfix Benjamin: Bug bei Skalen-Variablen: length(name) ist hier nicht groesser 1, obwohl das erwartet wurde -> else if statement eingebaut!!

    # Skript für Bericht der Missingkategorien vorbereiten - Form: "$ZAHL=$~\\textit{LabelSH};" bzw. "Fehlende Werte: -96 -- -99", wenn keine Labels vergeben wurden
    if (class(werte) == "list") { ### 16.04.: Hotfix
      werte <- c("sysmis.totalabs" =as.character(max(as.numeric(werte[[length(werte)]]["sysmis.totalabs",]), na.rm=TRUE) ))
    } else {
      if(class(werte) %in% c("matrix" , "data.frame")){
        werte <- c("sysmis.totalabs" =as.character(max(as.numeric(werte["sysmis.totalabs",]), na.rm=TRUE) ))
      }
    }

    value_missings <- varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "ja" ]
    valueLabel_missings <- varue.missings.aktuell$LabelSH[ tolower(varue.missings.aktuell$missing) %in% "ja" ]

    if(werte[ "sysmis.totalabs" ] %in% "0" &
       length(value_missings) >1 &
       all( is.na(valueLabel_missings) |  gsub("\\s" , "" , valueLabel_missings) %in% "")){
      minVal <- min(abs(as.numeric(value_missings)) , na.rm=TRUE)
      maxVal <- max(abs(as.numeric(value_missings)) , na.rm=TRUE)
      missings <- paste0( "Fehlende Werte:& " , minVal, "--", maxVal, "\\\\" )
    } else {
      varue.missings.aktuell$LabelSH[ is.na(varue.missings.aktuell$LabelSH) & varue.missings.aktuell$missing %in% "ja"] <- "(kein Label vergeben)"
      varue.missings.aktuell$LabelSH[ gsub("\\s" , "" , varue.missings.aktuell$LabelSH) %in% "" & varue.missings.aktuell$missing %in% "ja" ] <- "(kein Label vergeben)"
      # wobei hier zusaetzlich Sysmis (falls vorhanden) eingefügt werden und nach der Anzahl der Kategorien (für das Setzen des ";") unterschieden wird
      if( ! werte[ "sysmis.totalabs" ] %in% "0" & length( which( tolower ( varue.missings.aktuell$missing ) %in% "ja" ) )>0 ) { # Fall: Es gibt Sysmis und mindestens eine sonsitge Missingkategorie
        label.miss <- paste0("~$=$~\\textit{", varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "ja"] , "}; ")
        label.miss <- c( label.miss , "~$=$~\\textit{kein Dateneintrag}" )
        label.miss <- cbind( c( value_missings, "." ) , label.miss )
      } else if ( werte[ "sysmis.totalabs" ] %in% "0" & length( which( tolower ( varue.missings.aktuell$missing ) %in% "ja" ) )>1 ) { # Fall: Es gibt keine Sysmis und mehr als eine sonsige Missingkategorie
        label.miss <- paste0("~$=$~\\textit{", varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "ja"] , "}")
        label.miss[1:(length(label.miss)-1)] <- paste0( label.miss[1:(length(label.miss)-1)] , "; " )
        label.miss <- cbind( value_missings , label.miss )
      } else if (  werte[ "sysmis.totalabs" ] %in% "0" & length( which( tolower ( varue.missings.aktuell$missing ) %in% "ja" ) )==1 ) { # Fall: Es gibt keine Sysmis und genau eine sonsitge Missingkategorie
        label.miss <- paste0("~$=$~\\textit{", varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "ja"] , "}" )
        label.miss <- cbind( value_missings , label.miss )
      } else if ( length( value_missings ) == 0 ) { # Fall: Es sind keine Missingkategorie in der Varue definiert --> es wird dann Standardmaessig Sysmis in Beschreibungstabelle aufgefuehrt.

        label.miss <-  cbind(".","~$=$~\\textit{kein Dateneintrag}")
        #### 29.03. Benjamin: Korrektur der Missinglabel-Anzeige: nur wenn Missings vorkommen!
        # Achtung: passiert das für anderen Faelle (gibt auch andere Missingkategorien) auch? -> überprüfen!!
        # ohje, würde das die Latex-Tabelle kaputt machen? Johanna fragen, ob das notwendig ist?
        if(werte["sysmis.totalabs"] == 0) label.miss <- cbind("", "")
      }
      missings <- paste0( "Fehlende Werte:& " , paste(paste0(label.miss[,1] , label.miss[,2] ), collapse=""), "\\\\" )
      ### 16.04. riskante Aenderung Benjamin: Zeilen fehlende Werte komplett raus, wenn gar keine Missings vorhanden
      if(length( value_missings ) == 0 && werte["sysmis.totalabs"] == 0) {
        missings <- ""
      }

    }
  }

  anzahl.items <- NULL
  if(!is.null(skala.items)){
    if(gepoolt){
      anzahl.items <- paste0("Anzahl der Imputationen: & ", length(skala.items) , "\\\\")
    } else {
      anzahl.items <- paste0("Anzahl der Items: & ", length(skala.items) , "\\\\")
    }
  }

  if(tolower(var.typ)=="zeichenfolge"){
    var.typ.entry <- paste0("Variablentyp:& " , var.typ , " \\\\")
  } else {
    var.typ.entry <- NULL
  }
  skript <- c( "\\begin{tabnormallong}{Beschreibung der Variable}",
                 paste0("Variablenname:&",nameSH,"\\\\"),
                 paste0("Label:&",varue.info.aktuell$LabelSH,"\\\\"),
                 var.typ.entry,
                 anzahl.items,
                 quelle,
                 instruktion,
                 kategorien,
                 missings,
                 anmerkung,
                 "\\end{tabnormallong}" )


  return(skript)
}
