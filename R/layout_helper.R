#### LAYOUT-FUNKTIONEN ####

# Die Layout-Funktionen unterscheiden sich in zwei Arten: Die erste Art sind layoutspezifische Funktionen.
# Jede Variable bekommt in der Varue einen Layouttyp, durch den in diesem Skript entschieden wird, welche Layout-Funktion aufgerufen wird (layout.id, layout.kategorial, ...).
# Innerhalb dieser Layout-Funktionen werden Funktion der zweiten Art aufgerufen (table.descriptive, table.means, ...). Jede dieser Funktion erstellt das Skript für eine Tabelle (Beschreibungstabelle, Haeufigkeitstabelle etc.). Das sind layoutuebergreifende Funk
# Diese Aufteilung soll die Bearbeitung der Funktionen vereinfachen: Sollte sich etwas für nur einen Layouttyp verändern, so kann das in den Funktion erster Art vorgenommen werden. Sollte sich etwas an einer Tabellenart aendern, die für alle Layouttypen verwendet wird, so kann das in den Funktionen zweiter Art getan werden.


# Funktionen erster Art: Funktionen pro Layouttyp (Layoutspezifisch)
# layout.id:				: Skript für Identifikationsvariablen (Tabelle mit Variablenbeschreibung)
# layout.string				: Skript für String-Variablen (Tabelle mit Variablenbeschreibung)
# layout.kategorial			: Skript für kategoriale Variablen (Tabelle mit Variablenbeschreibung und Haeufigkeitstabelle)
# layout.ordinal			: Skript für ordinale Variablen (Tabelle mit Variablenbeschreibung, Haeufigkeitstabelle und Tabelle mit metrischen Kennwerten)
# layout.metrisch			: Skript für metrische Variablen (Tabelle mit Variablenbeschreibung und Tabelle mit metrischen Kennwerten)
# layout.skala				: Skript für Skalen (Tabelle mit Variablenbeschreibungen (Skala und Items), Haeufigkeitstabelle (Items) und Tabelle mit metrischen Kennwerten (Skala) )
# layout.gepoolt.metrisch	: Skript für gepoolte metrische Variablen (Tabelle mit Variablenbeschreibung und Tabelle mit metrischen Kennwerten)
# layout.gepoolt.kategorial	: Skript für gepoolte kategoriale Variablen (Tabelle mit Variablenbeschreibung und Haeufigkeitstabelle)
# layout.numerisch.geleert	: Skript für vom FDZ geleerte numerische Variablen (Tabelle mit Variablenbeschreibung)

# Funktionen zweiter Art: Funktionen pro Tabellentyp (Layoutübergreifend)
# table.descriptive: Tabelle "Beschreibung der Variable"
# table.means: Tabelle mit metrischen Kennwerten, auch für die Tabelle "Itemanalyse" von Items in enier Skala
# table.frequencies: Haeufigkeitstabelle für einzelne Variablen
# table.frequencies.items: Haeufigkeitstabelle für Items, die zu einer Skala gehören

table.descriptive <- function(name , varue.info , varue.missings=NULL , var.typ  , skala.items=NULL , Gesamtdatensatz=NULL , werte=NULL, show.kategorien=TRUE, gepoolt=FALSE){
  if(length(name)>1){
    varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name[1],]
    varue.missings.aktuell <- varue.missings[varue.missings$Var.name %in% name,]
    varue.missings.aktuell <- varue.missings.aktuell[! duplicated(varue.missings.aktuell$Wert),]
    varue.missings.aktuell <- varue.missings.aktuell[order(as.numeric(varue.missings.aktuell$Wert)),]
  } else {
    varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
    varue.missings.aktuell <- varue.missings[varue.missings$Var.name %in% name,]
  }

  nameSH <- gsub( "_" , "\\_" , name , fixed = TRUE)

  # Quellenangabe - falls vorhanden
  if( ! gsub( "\\s" , "" , varue.info.aktuell$QuelleSH ) %in% c("","-") & ! is.na(varue.info.aktuell$QuelleSH) ) {
    quelle <- paste0("Quelle:&", varue.info.aktuell$QuelleSH, "\\\\")
  } else {
    quelle <- NULL
  }

  # Invertierte Items
  if(all(varue.info$rekodiert[varue.info$Var.Name %in% name] %in% "nein")){
    invert.item <- NULL
  } else {
    if( length(which(varue.info$rekodiert[varue.info$Var.Name %in% name] %in% "ja" ) )==1 ) {
      if(length(name)>1){
        invert.item <- paste0("Invertiertes Item: & ", gsub( "_" , "\\_" , paste( varue.info$Var.Name[ varue.info$Var.Name %in% name & varue.info$rekodiert %in% "ja"] , collapse =", "), fixed=TRUE) , "\\\\" )
      } else {
        invert.item <- NULL
      }
    } else if( length(which(varue.info$rekodiert[varue.info$Var.Name %in% name] %in% "ja" ) ) > 1 ) {
      invert.item <- paste0("Invertierte Items: & ",  gsub( "_" , "\\_" , paste( varue.info$Var.Name[ varue.info$Var.Name %in% name & varue.info$rekodiert %in% "ja"] , collapse =", "), fixed=TRUE) , "\\\\" )
    } else {
      invert.item <- NULL
    }
  }


  # Anmerkung, falls vorhanden
  if( ! gsub( "\\s" , "" , varue.info.aktuell$Anmerkung.Var ) %in% "-" & ! is.na(varue.info.aktuell$Anmerkung.Var) ) {
    anmerkung <- paste0("Anmerkungen:&", varue.info.aktuell$Anmerkung.Var, "\\\\")
  } else {
    anmerkung <- NULL
  }

  # Instruktion
  if( ! gsub( "\\s" , "" , varue.info.aktuell$Instruktionen ) %in% c("", "-") & ! is.na(varue.info.aktuell$Instruktionen) ) {
    instruktion <- paste0("Instruktion:&", varue.info.aktuell$Instruktionen, "\\\\")
  } else {
    instruktion <- NULL
  }

  # Bericht der Kategorien
  if(is.null(varue.missings.aktuell)){
    # falls keine Kategoien/Missings vorhanden
    kategorien <- NULL
    missings <- NULL
  } else if( length(varue.missings.aktuell[,1])==0) {
    # falls keine Kategoien/Missings vorhanden
    kategorien <- NULL
    missings <- NULL
  } else {
    # Fallunterscheidung: Falls Variable als kategoriale definiert wurde, aber außer Missings keine Kategorien definiert hat
    if( all( varue.missings.aktuell$missing %in% "ja" ) ) {
      # Missingkategorien
      missings <- varue.missings.aktuell$Wert

      # min/max bestimmen

      if(all(suppressWarnings(sapply(unique(Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ]) , is.numeric)))){
        min.val <- min(Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ] , na.rm =TRUE)
        max.val <- max(Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ] , na.rm =TRUE)
      } else {
        show.kategorien <- FALSE
      }

      # Bericht der validen Kategorien im Skript
      if(show.kategorien){
        kategorien <- paste0("Kategorien:& " , min.val , "--" ,  max.val  , "\\\\")
      } else {
        kategorien <- NULL
      }
    } else {
      bool.kat <- is.na(varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "nein"]) |  gsub("\\s" , "" , varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "nein"]) %in% "" |  grepl("kein label vergeben", tolower(varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "nein"]), fixed=TRUE)
      names(bool.kat) <- varue.missings.aktuell$Wert[ tolower ( varue.missings.aktuell$missing ) %in% "nein"]
      if(show.kategorien){
        # Vorbereitung für Bercht der validen Kategorien im Skript - Form: "ZAHL~$=$~\\textit{LabelSH};" --> wenn keine Label vergeben, dann in der Form "Kategorien: 1-6"; wenn nur der höchste und der kleinste Wert Label haben, dann "1=X bis 6=Y"

        if(suppressWarnings(any(is.na(as.numeric(varue.missings.aktuell$Wert[ tolower ( varue.missings.aktuell$missing ) %in% "nein"]))))){
          bool.kat.bis <- all(! bool.kat[ as.character(sort(varue.missings.aktuell$Wert[ tolower ( varue.missings.aktuell$missing ) %in% "nein"])[c(1,length(varue.missings.aktuell$Wert[ tolower ( varue.missings.aktuell$missing ) %in% "nein"]))])]) &  all(bool.kat[ ! names(bool.kat) %in% as.character(sort(varue.missings.aktuell$Wert[ tolower ( varue.missings.aktuell$missing ) %in% "nein"])[c(1,length(varue.missings.aktuell$Wert[ tolower ( varue.missings.aktuell$missing ) %in% "nein"]))])]) & length(bool.kat)>2
        } else {
          bool.kat.bis <- all(! bool.kat[ as.character(sort(as.numeric(varue.missings.aktuell$Wert[ tolower ( varue.missings.aktuell$missing ) %in% "nein"]))[c(1,length(varue.missings.aktuell$Wert[ tolower ( varue.missings.aktuell$missing ) %in% "nein"]))])]) &  all(bool.kat[ ! names(bool.kat) %in% as.character(sort(as.numeric(varue.missings.aktuell$Wert[ tolower ( varue.missings.aktuell$missing ) %in% "nein"]))[c(1,length(varue.missings.aktuell$Wert[ tolower ( varue.missings.aktuell$missing ) %in% "nein"]))])]) & length(bool.kat)>2
        }

        if(all( bool.kat ) ){
          if(all(sapply(suppressWarnings(as.numeric(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "nein" ])) , is.numeric))){
            minVal <- min(as.numeric(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "nein" ] , na.rm=TRUE))
            maxVal <- max(as.numeric(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "nein" ] , na.rm=TRUE))
            kategorien <- paste0( "Kategorien:& " , minVal , "--" , maxVal , "\\\\" )
          } else {
            kategorien <- NULL
          }
        } else if ( bool.kat.bis){
          minVal <- paste0(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "nein" ][which.min(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "nein" ])] , "~$=$~\\textit{" ,  varue.missings.aktuell$LabelSH[ tolower(varue.missings.aktuell$missing) %in% "nein" ][which.min(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "nein" ])], "}")
          maxVal <- paste0(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "nein" ][which.max(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "nein" ])] , "~$=$~\\textit{" ,  varue.missings.aktuell$LabelSH[ tolower(varue.missings.aktuell$missing) %in% "nein" ][which.max(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "nein" ])], "}")

          kategorien <- paste0( "Kategorien:& " , minVal , " bis " , maxVal , "\\\\" )
        } else { # Wenn mindestens ein Label vorliegt
          # alle Werte ohne Labels bekommen "kein Label vergeben" (in kursiv)
          varue.missings.aktuell$LabelSH[ is.na(varue.missings.aktuell$LabelSH) & varue.missings.aktuell$missing %in% "nein"] <- "(kein Label vergeben)"
          varue.missings.aktuell$LabelSH[ gsub("\\s" , "" , varue.missings.aktuell$LabelSH) %in% "" & varue.missings.aktuell$missing %in% "nein" ] <- "(kein Label vergeben)"
          # Aufbereitung der Labels
          label.nonmiss <- paste0("~$=$~\\textit{", varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "nein"], "}" )
          label.nonmiss[1:(length(label.nonmiss)-1)] <- paste0( label.nonmiss[1:(length(label.nonmiss)-1)] , "; " )
          # Aufbereitung der Werte --> Wenn Zeilenumbruch vor Wert, dann "\\\\ \n & " vor den Wert
          kat_werte <- varue.missings.aktuell$Wert
          kat_werte[varue.missings.aktuell$missing=="nein" & varue.missings.aktuell$Zeilenumbruch_vor_Wert=="ja"] <- paste0("\\\\ \n & ",kat_werte[varue.missings.aktuell$missing=="nein" & varue.missings.aktuell$Zeilenumbruch_vor_Wert=="ja"])
          kat_werte <- kat_werte[varue.missings.aktuell$missing=="nein"]
          # Zusammenfügen von Werten und Labels
          label.nonmiss <- paste0( kat_werte , label.nonmiss )
          # Bericht der Kategorien im Skript
          kategorien <- paste0("Kategorien:& " , paste0(label.nonmiss, collapse="") , "\\\\" )
        }
      } else {
        kategorien <- NULL
      }
    }

    # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet, das wird hier korrigiert
    if(length(nameSH)==1){
      names(werte) <- sub( paste0(name,"\\.(.*)$") , "\\1" , names(werte) )
    }


    #### 16.04.: Hotfix Benjamin: Bug bei Skalen-Variablen: length(name) ist hier nicht groesser 1, obwohl das erwartet wurde -> else if statement eingebaut!!

    # Skript für Bericht der Missingkategorien vorbereiten - Form: "$ZAHL=$~\\textit{LabelSH};" bzw. "Fehlende Werte: -96 -- -99", wenn keine Labels vergeben wurden
    if(length(name)>1){
      if(class(werte)=="list"){
        werte <- c("sysmis.totalabs" =as.character(max(as.numeric(werte[[length(werte)]]["sysmis.totalabs",]), na.rm=TRUE) ))
      } else {
        werte <- c("sysmis.totalabs" =as.character(max(as.numeric(werte["sysmis.totalabs",]), na.rm=TRUE) ))
      }
    } else if (length(name) == 1 && class(werte) == "list") { ### 16.04.: Hotfix
      werte <- c("sysmis.totalabs" =as.character(max(as.numeric(werte[[length(werte)]]["sysmis.totalabs",]), na.rm=TRUE) ))
    } else {
      if(class(werte) %in% c("matrix" , "data.frame")){
        werte <- c("sysmis.totalabs" =as.character(max(as.numeric(werte["sysmis.totalabs",]), na.rm=TRUE) ))
      }
    }

    if( werte[ "sysmis.totalabs" ] %in% "0" & length(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "ja" ]) >1 &  all( is.na(varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "ja"]) |  gsub("\\s" , "" , varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "ja"]) %in% "") ){
      minVal <- min(abs(as.numeric(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "ja" ])) , na.rm=TRUE)
      maxVal <- max(abs(as.numeric(varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "ja" ])) , na.rm=TRUE)
      missings <- paste0( "Fehlende Werte:& " , minVal, "--", maxVal, "\\\\" )
    } else {
      varue.missings.aktuell$LabelSH[ is.na(varue.missings.aktuell$LabelSH) & varue.missings.aktuell$missing %in% "ja"] <- "(kein Label vergeben)"
      varue.missings.aktuell$LabelSH[ gsub("\\s" , "" , varue.missings.aktuell$LabelSH) %in% "" & varue.missings.aktuell$missing %in% "ja" ] <- "(kein Label vergeben)"
      # wobei hier zusaetzlich Sysmis (falls vorhanden) eingefügt werden und nach der Anzahl der Kategorien (für das Setzen des ";") unterschieden wird
      if( ! werte[ "sysmis.totalabs" ] %in% "0" & length( which( tolower ( varue.missings.aktuell$missing ) %in% "ja" ) )>0 ) { # Fall: Es gibt Sysmis und mindestens eine sonsitge Missingkategorie
        label.miss <- paste0("~$=$~\\textit{", varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "ja"] , "}; ")
        label.miss <- c( label.miss , "~$=$~\\textit{kein Dateneintrag}" )
        label.miss <- cbind( c( varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "ja" ], "." ) , label.miss )
      } else if ( werte[ "sysmis.totalabs" ] %in% "0" & length( which( tolower ( varue.missings.aktuell$missing ) %in% "ja" ) )>1 ) { # Fall: Es gibt keine Sysmis und mehr als eine sonsige Missingkategorie
        label.miss <- paste0("~$=$~\\textit{", varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "ja"] , "}")
        label.miss[1:(length(label.miss)-1)] <- paste0( label.miss[1:(length(label.miss)-1)] , "; " )
        label.miss <- cbind( varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "ja" ] , label.miss )
      } else if (  werte[ "sysmis.totalabs" ] %in% "0" & length( which( tolower ( varue.missings.aktuell$missing ) %in% "ja" ) )==1 ) { # Fall: Es gibt keine Sysmis und genau eine sonsitge Missingkategorie
        label.miss <- paste0("~$=$~\\textit{", varue.missings.aktuell$LabelSH[ tolower ( varue.missings.aktuell$missing ) %in% "ja"] , "}" )
        label.miss <- cbind( varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "ja" ] , label.miss )
      } else if ( length( varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "ja" ] ) == 0 ) { # Fall: Es sind keine Missingkategorie in der Varue definiert --> es wird dann Standardmaessig Sysmis in Beschreibungstabelle aufgefuehrt.

        label.miss <-  cbind(".","~$=$~\\textit{kein Dateneintrag}")
        #### 29.03. Benjamin: Korrektur der Missinglabel-Anzeige: nur wenn Missings vorkommen!
        # Achtung: passiert das für anderen Faelle (gibt auch andere Missingkategorien) auch? -> überprüfen!!
        # ohje, würde das die Latex-Tabelle kaputt machen? Johanna fragen, ob das notwendig ist?
        if(werte["sysmis.totalabs"] == 0) label.miss <- cbind("", "")
      }
      missings <- paste0( "Fehlende Werte:& " , paste(paste0(label.miss[,1] , label.miss[,2] ), collapse=""), "\\\\" )
      ### 16.04. riskante Aenderung Benjamin: Zeilen fehlende Werte komplett raus, wenn gar keine Missings vorhanden
      if(length( varue.missings.aktuell$Wert[ tolower(varue.missings.aktuell$missing) %in% "ja" ] ) == 0 && werte["sysmis.totalabs"] == 0) {
        missings <- ""
      }

    }
  }

  if(is.null(skala.items)){
    anzahl.items <- NULL
  } else {
    if(gepoolt){
      anzahl.items <- paste0("Anzahl der Imputationen: & ", length(skala.items) , "\\\\")
    } else {
      anzahl.items <- paste0("Anzahl der Items: & ", length(skala.items) , "\\\\")
    }
  }

  if(length(nameSH)==1){
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
                 invert.item,
                 anmerkung,
                 "\\end{tabnormallong}" )
  } else {
    varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
    name <- varue.info.aktuell$Var.Name
    nameSH <- gsub( "_" , "\\_" , name , fixed = TRUE)
    skript <- c(# Beschreibungstabelle der Items
      "\\begin{tabnormallong}{Beschreibung der Items}",
      kategorien,
      missings,
      invert.item,
      anmerkung,
      #instruktion, # Instruktionen machen wenig Sinn, wenn die variablenspezifisch sind, aber in dieser Tabelle variablenübergreifende Informationen gegeben werden
      "\\end{tabnormallong}",

      # Labeltabelle der Items
      "\\begin{tabcoloredNoCaption}{lX}",
      "\\textbf{Variablen} & \\textbf{Labels} \\\\" ,
      "\\midrule",
      paste0(nameSH , " & ", varue.info.aktuell$LabelSH , sep = "\\\\"),
      "\\bottomrule" ,
      "\\end{tabcoloredNoCaption}")
  }

  return(skript)
}


table.frequencies <- function(name , varue.missings.aktuell , werte){
  # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet, das wird hier korrigiert
  names(werte) <- sub( paste0(name,"\\.(.*)$") , "\\1" , names(werte) )

  # Sysmis als Missing-Kategorie in der varue hinzufügen - wird spaeter wieder gelöscht, falls nicht besetzt
  varue.missings.aktuell <- data.frame( "Var.name"=c( varue.missings.aktuell$Var.name  ,name ) ,
                                        "Wert"= c( varue.missings.aktuell$Wert  , "." ) ,
                                        "LabelSH"=c( varue.missings.aktuell$LabelSH  , "kein Dateneintrag" ),
                                        "missing"=c( varue.missings.aktuell$missing  , "ja" ) , stringsAsFactors=FALSE)

  # Entferung derjenigen Missingkategorien aus der reduzierten Varue, die nicht besetzt sind
  varue.missings.aktuell <- varue.missings.aktuell[ 	varue.missings.aktuell$missing %in% "nein" |
                                                       ! sapply(sub("^\\.$" , "sysmis" , varue.missings.aktuell$Wert ), function(d) werte[ paste0(d , ".totalabs" )] %in% "0") ,]


  # Wertelabel anpassen - wenn kein Label vergeben, dass "(kein Label vergeben)" in kursiv in die Tabelle
  if(any(is.na(varue.missings.aktuell$LabelSH) | gsub("\\s*", "", varue.missings.aktuell$LabelSH) %in% "")){
    varue.missings.aktuell$LabelSH[ is.na(varue.missings.aktuell$LabelSH) | gsub("\\s*", "", varue.missings.aktuell$LabelSH) %in% ""] <- "(\\textit{kein Label vergeben})"
  }

  # Tabellenanmerkung: Wenn gerundete Prozentzahl "0.0" ist, aber absoluter Wert größer 0
  ## versuchter Bugfix Benjamin 02.04.2019: nur valide Werte prüfen (aber falscher Ansatz)
  # valid_werte <- varue.missings.aktuell[varue.missings.aktuell$missing == "nein", "Wert"]
  # if( any( ( werte[ paste0(sub("^\\.$","sysmis", valid_werte), ".valid") ] == "0.0" |
  #browser()
  if( any( ( werte[ paste0(sub("^\\.$","sysmis",varue.missings.aktuell$Wert), ".valid") ] == "0.0" |  ### alte Syntax von Felix
             werte[ paste0(sub("^\\.$","sysmis",varue.missings.aktuell$Wert), ".total") ] == "0.0" ) &
           ! werte[ paste0(sub("^\\.$","sysmis",varue.missings.aktuell$Wert), ".totalabs") ] == "0" ) ) {
    anmerkungtab <- " und die Prozentzahl besetzter Kategorien 0.0~Prozent betragen"
  } else anmerkungtab <- ""

  # Tabellenanmerkung: Wenn keine Kategorien fehlender Werte vorhanden (also nur Sysmis)
  if( length( which(varue.missings.aktuell$missing %in% "ja") ) == 0 & "." %in% varue.missings.aktuell$Wert ){
    anmerkung.tab.miss <- "Die Kategorie \\textit{kein Dateneintrag} wird berichtet, wenn bei dieser mindestens eine Angabe vorliegt."
  } else {
    anmerkung.tab.miss <- "Kategorien fehlender Werte werden berichtet, wenn bei diesen mindestens eine Angabe vorliegt."
  }

  skript <- c("\\begin{tabcoloredlong}",
              paste0( varue.missings.aktuell$Wert , " & " , varue.missings.aktuell$LabelSH , " & " , werte[ paste0( sub("^\\.$","sysmis",varue.missings.aktuell$Wert) , ".valid") ] , " & " , werte[ paste0( sub("^\\.$","sysmis",varue.missings.aktuell$Wert) , ".total") ] , "\\\\", "" ),
              "\\nobreakbottomrule",
              paste0("\\anmerkungen{4}{Es werden gerundete relative Haeufigkeiten in Prozent in Bezug auf die Fallzahl der gültigen Werte ($N_{valid}~=~", werte["N.valid"] ,"$) und in Bezug auf die Fallzahl aller Werte ($N_{total}~=~", werte["N.total"] , "$) berichtet. Dadurch kann die Summe der Prozente minimal von 100 abweichen" , anmerkungtab ,". ",anmerkung.tab.miss, "}"),
              "\\end{tabcoloredlong}")

  return(skript)
}


table.means <- function(name , werte, setsizefst , cols, anm.tab=NULL , items=FALSE){
  nameSH <- gsub( "_" , "\\_" , name , fixed = TRUE)
  headlines <- cols[1,]
  bool <- sapply( "_" , grepl , x=headlines) | sapply("\\\\" , grepl , x=headlines)
  headlines[ which(bool) ] <- paste0("$\\mathbold{" , headlines[which(bool)] , "}$" )
  headlines[which(!bool)] <- paste0("\\fk{" , headlines[which(!bool) ] , "}" )

  if(!items){
    # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet, das wird hier korrigiert
    names(werte) <- sub( paste0(name,"\\.(.*)$") , "\\1" , names(werte) )
    tab.titel <- ""
    tabellenumgebung <- "tabcoloredNoCaption"
    vals <- paste0(nameSH , " & " , paste0(werte[unlist(unname(cols[2,]))] , collapse="& ") , sep="\\\\")
  } else {
    tab.titel <- "Itemanalyse"
    tabellenumgebung <- "tabcolored"
    vals <- sapply(name , function(d) paste0(gsub( "_" , "\\_" , d , fixed = TRUE) , " & " , paste0(werte[unlist(unname(cols[2,])),d] , collapse="& ") , sep="\\\\")  , USE.NAMES=FALSE)
  }

  skript <-	c(setsizefst,
              paste0("\\begin{",tabellenumgebung,"}{q{\\sizefst}*{",length(cols),"}{Z}}{",tab.titel,"}"),
              paste0("\\textbf{Variablenname} & " , paste0(headlines , collapse="& ") , "\\\\"),
              "\\midrule",
              vals,
              "\\bottomrule",
              anm.tab,
              paste0("\\end{",tabellenumgebung,"}"))

  return(skript)
}

table.frequencies.items <- function(name , varue.info, varue.missings.item , werte, endung){
  nameSH <- gsub( "_" , "\\_" , name , fixed = TRUE)

  # Tabellenanmerkung: Wenn gerundete Prozentzahl "0.0" ist, aber absoluter Wert größer 0
  if( any( sapply( name , function(d) any( ! werte[[length(werte)]][ paste0(sub("^\\.$","sysmis",varue.missings.item$Wert), ".totalabs") , d  ] %in% "0" &
                                           werte[[length(werte)]][ paste0(sub("^\\.$","sysmis",varue.missings.item$Wert), ".total" ) , d ] %in% "0.0" )
  ) ) ) {
    anmerkung.tab <- " und die Prozentzahl besetzter Kategorien 0.0~Prozent betragen"
  } else anmerkung.tab <- ""

  # Tabellenanmerkung: Wenn mind. eine Variable rekodiert wurde
  if( any( varue.info$rekodiert[varue.info$Var.Name %in% nameSH] %in% "ja" ) ){
    anmerkung.tab.rek <-  paste0(" Mit \\glqq ",endung,"\\grqq{} gekennzeichnete Variablen wurden rekodiert.")
  } else {
    anmerkung.tab.rek <- ""
  }

  # Sortierung der Varue
  varue.miss <- varue.missings.item[varue.missings.item$missing %in% "ja",]
  varue.miss <- varue.miss[order(abs(as.numeric(varue.miss$Wert))) ,]
  varue.valid <- varue.missings.item[varue.missings.item$missing %in% "nein",]
  varue.valid <- varue.valid[order(as.numeric(varue.valid$Wert)) ,]
  varue.missings.item <- rbind(varue.valid , varue.miss)

  # Reduktion der Item-Varue auf zu berichtende Faelle
  varue.missings.item <- varue.missings.item[ tolower(varue.missings.item$missing) %in% "nein" |
                                                ! varue.missings.item$Wert %in% varue.missings.item$Wert[ sapply( paste0(varue.missings.item$Wert, ".totalabs") , function(d) all(  werte[[length(werte)]][d, ] %in% "0" ) ) ]	,
  ]

  # Ergaenzung der Varue, falls Sysmis berichtet werden
  if( ! all( werte[[length(werte)]][ "sysmis.totalabs", ] %in% "0" ) ) {
    varue.missings.item <- data.frame( "Var.name"=c( varue.missings.item$Var.name  ,varue.missings.item$Var.name[1] ) ,
                                       "Wert"= c( varue.missings.item$Wert  , "." ) ,
                                       "LabelSH"=c( varue.missings.item$LabelSH  , "kein Dateneintrag" ),
                                       "missing"=c( varue.missings.item$missing  , "ja" ))
  }


  # Tabellenanmerkung: Wenn keine Kategorien fehlender Werte vorhanden (also nur Sysmis)
  if( length( which(varue.missings.item$missing %in% "ja") ) == 0 ){
    anmerkung.tab.miss <- NULL
    varue.missings.item <- data.frame( "Var.name"=c( varue.missings.item$Var.name  ,varue.missings.item$Var.name[1] ) ,
                                       "Wert"= c( varue.missings.item$Wert  , "." ) ,
                                       "LabelSH"=c( varue.missings.item$LabelSH  , "kein Dateneintrag" ),
                                       "missing"=c( varue.missings.item$missing  , "ja" ))
  } else {
    anmerkung.tab.miss <- "Kategorien fehlender Werte werden berichtet, wenn bei diesen auf mindestens einem Item mindestens eine Angabe vorliegt."
  }

  textwidth <- 455.24417
  tabcolsep <- 6
  sizefst <- max(c( Latex.length("Variablenname", TRUE) , sapply( name , Latex.length , bold=FALSE) ) )


  ## debug Lkf!!!
  #if(identical(name, c("Lkf01a", "Lkf01b", "Lkf01c", "Lkf01d", "Lkf01e"))) browser()
  ## Hotfix Benjamin (29.03.): Error in Latex, bei else Klammer (und ich verstehe nicht, was da passiert) -> Auskommentiert und das andere dann verwenden, so scheints zu passen

  #if((((textwidth - 2*tabcolsep - sizefst)/length(varue.missings.item$Wert) - 2*tabcolsep))*length(varue.missings.item$Wert[varue.missings.item$missing %in% "ja"])>75){
  column_spec <- paste0("q{\\sizefst}*{", length( varue.missings.item$Wert ) ,"}{y}")
  #} else {
  #	column_width <- 75/length( varue.missings.item$Wert[ varue.missings.item$missing %in% "ja"] )
  #	column_spec <- paste0("q{\\sizefst}*{", length( varue.missings.item$Wert[ varue.missings.item$missing %in% "nein"]) ,"}{y}*{",length( varue.missings.item$Wert[ varue.missings.item$missing %in% "ja"]),"}{v{",column_width,"}}")
  #}


  skript <- c(# Haeufigkeitstabelle der Items
    paste0("\\begin{tabcoloreditem}{",column_spec,"}{", length( varue.missings.item$Wert[ varue.missings.item$missing %in% "nein"] ) ,"}{",length( varue.missings.item$Wert[ varue.missings.item$missing %in% "ja"] ) ,"}"),
    paste0("\\rulefiller \\cmidrule[\\lightrulewidth](lr){2-", length( varue.missings.item$Wert[ varue.missings.item$missing %in% "nein"] )+1 ,"} \\cmidrule[\\lightrulewidth](lr){", length( varue.missings.item$Wert[ varue.missings.item$missing %in% "nein"] )+2,"-", length( varue.missings.item$Wert )+1 ,"}"),
    "\\headrow",
    paste0(" & ", paste( paste0("\\multic{\\textbf{", varue.missings.item$Wert, "}}"), collapse =" & "), " \\\\"),
    "\\midrule",
    "\\endhead",
    paste0("\\hline \\multicolumn{",1+length( varue.missings.item$Wert ),"}{@{}c@{}}{\\cellcolor{white} \\textit{Fortsetzung auf der nächsten Seite}}\\\\"),
    "\\hline",
    "\\endfoot",
    "\\endlastfoot",
    paste0(nameSH, " & ", sapply(name, function(d) paste( werte[[length(werte)]][ paste0( sub("^\\.$","sysmis",varue.missings.item$Wert) , ".total" ) , d ], collapse = " & " ) ) , "\\\\"),
    "\\nobreakbottomrule",
    paste0("\\anmerkungen{",1+length( varue.missings.item$Wert ),"}{Es werden gerundete relative Häufigkeiten in Prozent in Bezug auf die Fallzahl aller Werte ($N_{total}~=~" , werte[[length(werte)]]["N.total",1] , "$) berichtet. Dadurch kann die Summe der Prozente minimal von 100 abweichen" , anmerkung.tab , ". ",anmerkung.tab.miss, anmerkung.tab.rek , "}"),
    "\\end{tabcoloreditem}")

  return(skript)
}

# layout.id:				: Skript für Identifikationsvariablen (Tabelle mit Variablenbeschreibung)
# layout.string				: Skript für String-Variablen (Tabelle mit Variablenbeschreibung)
# layout.kategorial			: Skript für kategoriale Variablen (Tabelle mit Variablenbeschreibung und Haeufigkeitstabelle)
# layout.ordinal			: Skript für ordinale Variablen (Tabelle mit Variablenbeschreibung, Haeufigkeitstabelle und Tabelle mit metrischen Kennwerten)
# layout.metrisch			: Skript für metrische Variablen (Tabelle mit Variablenbeschreibung und Tabelle mit metrischen Kennwerten)
# layout.skala				: Skript für Skalen (Tabelle mit Variablenbeschreibungen (Skala und Items), Haeufigkeitstabelle (Items) und Tabelle mit metrischen Kennwerten (Skala) )
# layout.gepoolt.metrisch	: Skript für gepoolte metrische Variablen (Tabelle mit Variablenbeschreibung und Tabelle mit metrischen Kennwerten)
# layout.gepoolt.kategorial	: Skript für gepoolte kategoriale Variablen (Tabelle mit Variablenbeschreibung und Haeufigkeitstabelle)
# layout.numerisch.geleert	: Skript für vom FDZ geleerte numerische Variablen (Tabelle mit Variablenbeschreibung)

layout.id <- function( name , varue.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	varue.info: data.frame, Übersicht der Variableninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die für die Variable den Tabellenblock erstellt
  #			Hier nur Beschreibungstabelle


  #### Skript schreiben ####
  skript <- c(table.descriptive(name=name, varue.info=varue.info , var.typ="ID-Variable") , "\\clearpage" )


  #### Output ####

  return( skript )
}

layout.numerisch.geleert <- function( name , varue.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	varue.info: data.frame, Übersicht der Variableninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die für die Variable den Tabellenblock erstellt
  #			Hier nur Beschreibungstabelle



  #### Skript schreiben ####
  skript <- c(table.descriptive(name=name, varue.info=varue.info , var.typ="Numerisch") , "\\clearpage" )


  #### Output ####

  return( skript )
}

layout.string <- function( name , varue.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	varue.info: data.frame, Übersicht der Variableninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die für die Variable den Tabellenblock erstellt
  #			Hier nur Beschreibungstabelle

  #### Skript schreiben ####

  skript <- c(table.descriptive(name=name, varue.info=varue.info , var.typ="Zeichenfolge") , "\\clearpage" )

  #### Output ####

  return( skript )
}

layout.kategorial <- function(name , kennwerte.var = NULL, id.fb, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	id.fb: Character, Name der Identifikiationsvariable im Datensatz
  #	varue.info: data.frame, Übersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Übersicht der Skaleninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die für die Variable den Tabellenblock erstellt
  #			Hier Beschreibungstabelle und Haeufigkeitstabelle

  # ANMERKUNG: Zur Erstellung der Haeufigkeitstabellen sind verschiedene Aenderungen an den Kennwerten und der Werteinformationen aus der Varue nötig.
  #			 Zuerst wird eine neue Variableninformation erstellt, sofern keine validen Kategorien definiert sind, also nur Missing in der Varue stehen. Valide Werte müssen berichtet werden.
  #			 In der Tabelle sollen alle validen Kategorien aufgelistet werden, aber nur besetzte Missing-Kategorien.
  #			 Zusaetzlich werden Sysmis berichtet, sofern diese besetzt sind.
  #			 Außerdem wird eine Tabellenanmerkung eingefügt, sofern Kategorien besetzt sind, aber die gerundete Prozentzahl "0.0" ist.

  #### Vorbereitung ####

  # Reduzierte Varue der Variableninformationen
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name %in% name,]

  # Kennwerte berechnen
  if( all( varue.missings.aktuell$missing %in% "ja" ) ) {
    ## Reduzierte Varue der Werteinformationen neu erstellen

    # Missingkategorien
    missings <- varue.missings$Wert[varue.missings$Var.name == name & varue.missings$missing=="ja"]

    # alle auftretenden validen Werte finden
    level.valid	<- sort ( unique( Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ] ) )

    # alle Werte
    level <- c(level.valid , missings )

    # Variablenname
    varname <- rep(name, length( level ))

    # Label für das Skalenhandbuch
    labelsh <- c( rep("", length(level.valid) ) , varue.missings$LabelSH[varue.missings$Var.name %in% name ] )

    # Wertekategorien
    varval = c( rep("nein", length(level.valid) ) , rep("ja" , length(missings) ) )

    # Reduzierte Varue der Werteinformationen erstellen
    varue.missings.aktuell 	<-	data.frame( "Var.Name"=varname,
                                           "Wert"=level,
                                           "missing"=varval,
                                           "LabelSH"=labelsh,
                                           stringsAsFactors = FALSE )
    if( is.null(kennwerte.var) ){
      werte <- kennwerte.kategorial.variation( name=name, varue.missings=varue.missings , Gesamtdatensatz=Gesamtdatensatz)
    } else {
      werte <- kennwerte.var
    }
  } else {
    # Kennwerte einlesen bzw. berechnen
    if( is.null(kennwerte.var) ){
      werte <- kennwerte( name=name, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
    } else {
      werte <- kennwerte.var
    }
  }

  # Sonderzeichen für Latex
  nameSH <- gsub( "_" , "\\_" , name , fixed = TRUE)

  #### Skript schreiben ####
  skript.descriptive <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz , werte=werte , var.typ="Numerisch" , skala.items=NULL)

  skript.frequencies <- table.frequencies(name=name , varue.missings.aktuell=varue.missings.aktuell , werte=werte)

  skript <- c(skript.descriptive,
              skript.frequencies,
              "\\clearpage" )

  #### Output ####
  return( skript )

}

layout.ordinal <- function(name , kennwerte.var = NULL, id.fb, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	id.fb: Character, Name der Identifikiationsvariable im Datensatz
  #	varue.info: data.frame, Übersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Übersicht der Skaleninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die für die Variable den Tabellenblock erstellt
  #			Hier Beschreibungstabelle, Tabelle mit metrischen Kennwerten und Haeufigkeitstabelle

  # ANMERKUNG: Zur Erstellung der Haeufigkeitstabellen sind verschiedene Aenderungen an den Kennwerten und der Werteinformationen aus der Varue nötig.
  #			 Zuerst wird eine neue Variableninformation erstellt, sofern keine validen Kategorien definiert sind, also nur Missing in der Varue stehen. Valide Werte müssen berichtet werden.
  #			 In der Tabelle sollen alle validen Kategorien aufgelistet werden, aber nur besetzte Missing-Kategorien.
  #			 Zusaetzlich werden Sysmis berichtet, sofern diese besetzt sind.
  #			 Außerdem wird eine Tabellenanmerkung eingefügt, sofern Kategorien besetzt sind, aber die gerundete Prozentzahl "0.0" ist.

  #### Vorbereitung ####

  # Reduzierte Varue der Variableninformationen
  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name %in% name, ]

  # Kennwerte einlesen bzw. berechnen
  if( is.null(kennwerte.var) ){
    werte <- kennwerte( name=name, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
  } else {
    werte <- kennwerte.var
  }

  # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet
  names(werte) <- sub( paste0(name,"\\.(.*)$") , "\\1" , names(werte) )

  # Sonderzeichen für Latex
  nameSH <- gsub( "_" , "\\_" , name , fixed = TRUE)


  # Falls Variablenname zu lang für Tabelle
  if( Latex.length( nameSH , FALSE) > Latex.length("Variablenname" , TRUE) ){
    setsizefst <- paste0("\\settowidth{\\sizefst}{",nameSH,"}")
  } else {
    setsizefst <- "\\settowidth{\\sizefst}{\\textbf{Variablenname}}"
  }

  anm.tab.ordinal <- paste0("\\anmerkungen{4}{$N =$ Fallzahl; $M =$ Mittelwert; $SD =$ Standardabweichung.}")


  #### Skript schreiben ####

  skript.descriptive <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings , var.typ="Numerisch" , werte=werte , skala.items=NULL)
  skript.frequencies <- table.frequencies(name=name, varue.missings.aktuell=varue.missings.aktuell , werte=werte)
  skript.means <- table.means(name=name , werte=werte, setsizefst=setsizefst , cols=data.frame(c("N_{valid}","N.valid") , c("M","mean.valid") , c("SD","sd.valid") , stringsAsFactors=FALSE ) , anm.tab=anm.tab.ordinal)
  skript <- c(skript.descriptive,
              skript.means,
              skript.frequencies,
              "\\clearpage" )

  #### Output ####

  return( skript )
}

layout.metrisch <- function(name , kennwerte.var = NULL, id.fb, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	id.fb: Character, Name der Identifikiationsvariable im Datensatz
  #	varue.info: data.frame, Übersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Übersicht der Skaleninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die für die Variable den Tabellenblock erstellt
  #			Hier Beschreibungstabelle und Tabelle mit metrischen Kennwerten


  #### Vorbereitung ####

  # Reduzierte Varue
  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name %in% name,]
  # Kennwerte einlesen bzw. berechnen
  if( is.null(kennwerte.var) ){
    werte <- kennwerte( name=name, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
  } else {
    werte <- kennwerte.var
  }

  # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet
  names(werte) <- sub( paste0(name,"\\.(.*)$") , "\\1" , names(werte) )

  # Sonderzeichen für Latex
  nameSH <- gsub( "_" , "\\_" , name , fixed = TRUE)

  if( Latex.length( nameSH , FALSE) > Latex.length("Variablenname" , TRUE) ){
    setsizefst <- paste0("\\settowidth{\\sizefst}{",nameSH,"}")
  } else {
    setsizefst <- "\\settowidth{\\sizefst}{\\textbf{Variablenname}}"
  }

  anm.tab.metrisch <- paste0("\\anmerkungen{6}{$N =$ Fallzahl; $M =$ Mittelwert; $SD =$ Standardabweichung; $Min. =$ Minimum; $Max. =$ Maximum. $N_{total}$~=~",length(Gesamtdatensatz[,name]),".}")

  #### Skript schreiben ####

  skript.descriptive <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings , var.typ="Numerisch" , Gesamtdatensatz=Gesamtdatensatz ,  werte=werte , skala.items=NULL, show.kategorien=FALSE)

  skript.means <- table.means(name=name , werte=werte, setsizefst=setsizefst , cols=data.frame(c("N_{valid}","N.valid") , c("M","mean.valid") , c("SD","sd.valid") , c("Min.","min.valid") , c("Max.","max.valid"), stringsAsFactors=FALSE), anm.tab = anm.tab.metrisch)

  skript <- c(skript.descriptive,
              skript.means,
              "\\clearpage" )

  #### Output ####
  return( skript )

}

layout.skala <- function(name , kennwerte.var = NULL, id.fb, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	id.fb: Character, Name der Identifikiationsvariable im Datensatz
  #	varue.info: data.frame, Übersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Übersicht der Skaleninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die für die Variable den Tabellenblock erstellt
  #			Skala: Beschreibungstabelle und Tabelle mit metrischen Kennwerten
  #			Items: Beschreibungstabelle, Labeltabelle, Tabelle mit metrischen Kennwerten und Haeufigkeitstabelle


  # YRNTCSH! - Der Funktion "table.frequencies.items" (ganz am Ende) muss die richtige Endung übergeben werden, mit der rekodierte Variablen identifizert werden können. Im LV12 war dies einheitlich ".r", im LV15 "_r". In der genannten Funktion muss dann die Variablenspezifikation 'endung="_r"' durch die neue Endung in der Form'endung="NEUE_ENDUNG"' ersetzt werden.

  #### Vorbereitung ####

  # Reduzierte Skala-Varue der Variableninformationen
  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name %in% name,]

  # Identifikation der zur Skala zugehörigen Items
  skala.items <- gsub( "\\s", "", unlist( strsplit( skalen.info[ tolower( skalen.info$varName ) %in% tolower( name ), "Items_der_Skala" ], ",", fixed = TRUE ) ) )

  # Reduktion der Items, falls diese in der Varue auf "nein" gesetzt sind
  skala.items <- skala.items[skala.items %in% varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja" , "sh", "ds")]]

  # Sortierung der Items --> Reihenfolge im Reiter "Skaleninformationen" wird an die in der Varue angepasst
  skala.items <- varue.info$Var.Name[varue.info$Var.Name %in% skala.items]

  # Reduzierte Item-Varue der Werteinformation (über ein nicht-rekodierten Items) - um richtige Ordnung der Werte zu bekommen
  #if( all( sub( ".*\\.(.{1})$", "\\1" ,skala.items ) %in% "r" ) ) {
  #	varue.missings.item <- varue.missings[varue.missings$Var.name %in% skala.items[1],]
  #} else {
  #	varue.missings.item <- varue.missings[varue.missings$Var.name %in% skala.items[ which( ! sub( ".*\\.(.{1})$", "\\1" ,skala.items ) %in% "r" )[1]],]
  #}

  varue.missings.item <- varue.missings[ varue.missings$Var.name %in% skala.items , ]
  varue.missings.item <- varue.missings.item[ ! duplicated(varue.missings.item$Wert) , ]
  varue.missings.item <- lapply(skala.items , function(d) data.frame("Var.name"=rep(d , length(varue.missings.item$Wert)) , "Wert"=varue.missings.item$Wert , "missing"=varue.missings.item$missing , "LabelSH" = varue.missings.item$LabelSH , "Zeilenumbruch_vor_Wert"=varue.missings.item$Zeilenumbruch_vor_Wert , stringsAsFactors=FALSE) )
  varue.missings.item <- do.call("rbind" , varue.missings.item)

  # Reduzierte Item-Varue der Variableninformationen
  varue.info.item <- varue.info[varue.info$varName %in% skala.items[1],]

  # Kennwerte berechnen bzw. nutzen
  if( is.null(kennwerte.var) ){
    werte <- kennwerte( name=name, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
  } else {
    werte <- kennwerte.var
  }

  # Sonderzeichen für Latex
  nameSH <- gsub( "_" , "\\_" , name , fixed = TRUE)
  skala.items.name <- gsub( "_" , "\\_" , skala.items , fixed = TRUE)

  # Anzahl gültiger Werte zur Berechnung der Skala
  if( skalen.info$Anzahl_valider_Werte[ tolower(skalen.info$varName) %in% tolower(name) ] %in% c("einem","ein","eins") ) {
    anmerkung.anzahl_valider_werte <- " Für die Berechnung der Skalenkennwerte wurden alle Teilnehmenden einbezogen, die auf mindestens einem Item einen gültigen Wert aufweisen ($N_{valid}$)."
  } else if( skalen.info$Anzahl_valider_Werte[ tolower(skalen.info$varName) %in% tolower(name) ] %in% c("alle","allen","all") ) {
    anmerkung.anzahl_valider_werte <- " Für die Berechnung der Skalenkennwerte wurden alle Teilnehmenden einbezogen, die auf allen Items einen gültigen Wert aufweisen ($N_{valid}$)."
  } else if( skalen.info$Anzahl_valider_Werte[ tolower(skalen.info$varName) %in% tolower(name) ] %in% "-" ) {
    anmerkung.anzahl_valider_werte <- NULL
  } else {
    anmerkung.anzahl_valider_werte <- paste0(" Für die Berechnung der Skalenkennwerte wurden alle Teilnehmenden einbezogen, die auf mindestens ", skalen.info$Anzahl_valider_Werte[ tolower(skalen.info$varName) %in% tolower(name) ]," Items einen gültigen Wert aufweisen ($N_{valid}$).")
  }

  if( Latex.length( nameSH , FALSE) > Latex.length("Variablenname" , TRUE) ){
    setsizefst <- paste0("\\settowidth{\\sizefst}{",nameSH,"}")
  } else {
    setsizefst <- "\\settowidth{\\sizefst}{\\textbf{Variablenname}}"
  }

  if( any( unname(sapply(skala.items.name , Latex.length , FALSE)) > Latex.length("Variablenname" , TRUE) ) ){
    setsizefst.items <- paste0("\\settowidth{\\sizefst}{",skala.items.name[which.max(nchar(skala.items.name))],"}")
  } else {
    setsizefst.items <- "\\settowidth{\\sizefst}{\\textbf{Variablenname}}"
  }

  ### reinbrowsen um Bug in Lfk zu finden (bis hier hin ok!) -> Problem bei skript.frequencies.items!!

  #### Skript schreiben ####
  anm.tab.skala <- paste0("\\anmerkungen{7}{$N =$ Fallzahl; $Min. =$ Minimum; $Max. =$ Maximum; $\\alpha =$~Cronbachs Alpha (Cronbach, 1951).", anmerkung.anzahl_valider_werte," Für die Reliabilitätsanalyse wurden nur Teilnehmende einbezogen, die auf allen Items gültige Werte besitzen.}")
  anm.tab.item.means <- "\\anmerkungen{5}{$N_{valid}$ gibt pro Item die Anzahl aller Fälle mit gültigen Werten an. Bei der Trennschärfe~$r_{pw}$ handelt es sich um die part-whole-korrigierte Korrelation des jeweiligen Items mit der Skala.}"

  skript.descriptive.skala <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings , var.typ="Numerisch" , werte=werte , Gesamtdatensatz=Gesamtdatensatz , skala.items=skala.items, show.kategorien=FALSE)
  skript.means.skala <- table.means(name=name , werte=t(werte[[1]])[1,], setsizefst=setsizefst , anm.tab=anm.tab.skala , cols=data.frame(c("N_{valid}","N.valid") , c("M","mean.valid") , c("SD","sd.valid") , c("Min.","min.valid") , c("Max.","max.valid") , c("\\alpha" , "alpha"), stringsAsFactors=FALSE))

  skript.descriptive.items <- table.descriptive(name=skala.items, varue.info=varue.info , varue.missings=varue.missings.item , var.typ="Numerisch"  , werte=werte[[2]] , Gesamtdatensatz=Gesamtdatensatz , skala.items=NULL)
  skript.means.items <- table.means(name=skala.items , werte=werte[[2]], setsizefst=setsizefst.items , anm.tab=anm.tab.item.means , cols=data.frame(c("N_{valid}","N.valid") , c("M","mean.valid") , c("SD","sd.valid") , c("r_{pw}","cor.valid") , stringsAsFactors=FALSE) , items=TRUE)
  skript.frequencies.items <- table.frequencies.items(name=skala.items , varue.info=varue.info , varue.missings.item=varue.missings.item[varue.missings.item$Var.name %in% skala.items[1],] , werte=werte , endung="_r")

  skript <- c( skript.descriptive.skala , skript.means.skala , "\\clearpage", skript.descriptive.items ,  "\\clearpage", skript.means.items , skript.frequencies.items  , "\\clearpage")

  #### Output ####

  return( skript )
}

layout.skala.fake <- function(name , kennwerte.var = NULL, id.fb, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	id.fb: Character, Name der Identifikiationsvariable im Datensatz
  #	varue.info: data.frame, Übersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Übersicht der Skaleninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die für die Variable den Tabellenblock erstellt
  #			Skala: Beschreibungstabelle und Tabelle mit metrischen Kennwerten
  #			Items: Beschreibungstabelle, Labeltabelle, Tabelle mit metrischen Kennwerten und Haeufigkeitstabelle


  # YRNTCSH! - Der Funktion "table.frequencies.items" (ganz am Ende) muss die richtige Endung übergeben werden, mit der rekodierte Variablen identifizert werden können. Im LV12 war dies einheitlich ".r", im LV15 "_r". In der genannten Funktion muss dann die Variablenspezifikation 'endung="_r"' durch die neue Endung in der Form'endung="NEUE_ENDUNG"' ersetzt werden.

  #### Vorbereitung ####

  # Identifikation der zur Skala zugehörigen Items
  skala.items <- gsub( "\\s", "", unlist( strsplit( skalen.info[ tolower( skalen.info$varName ) %in% tolower( name ), "Items_der_Skala" ], ",", fixed = TRUE ) ) )

  # Reduktion der Items, falls diese in der Varue ein "nein" haben
  skala.items <- skala.items[skala.items %in% varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja" , "sh", "ds")]]


  # Sortierung der Items --> Reihenfolge im Reiter "Skaleninformationen" wird an die in der Varue angepasst
  skala.items <- varue.info$Var.Name[varue.info$Var.Name %in% skala.items]

  # Reduzierte Item-Varue der Werteinformation (über ein nicht-rekodierten Items) - um richtige Ordnung der Werte zu bekommen
  varue.missings.item <- varue.missings[ varue.missings$Var.name %in% skala.items , ]
  varue.missings.item <- varue.missings.item[ ! duplicated(varue.missings.item$Wert) , ]
  varue.missings.item <- lapply(skala.items , function(d) data.frame("Var.name"=rep(d , length(varue.missings.item$Wert)) , "Wert"=varue.missings.item$Wert , "missing"=varue.missings.item$missing , "LabelSH" = varue.missings.item$LabelSH , "Zeilenumbruch_vor_Wert"=varue.missings.item$Zeilenumbruch_vor_Wert , stringsAsFactors=FALSE) )
  varue.missings.item <- do.call("rbind" , varue.missings.item)


  # Reduzierte Item-Varue der Variableninformationen
  varue.info.item <- varue.info[varue.info$Var.Name %in% skala.items[1],]

  # Kennwerte berechnen bzw. nutzen
  if( is.null(kennwerte.var) ){
    werte <- kennwerte( name=name, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
  } else {
    werte <- kennwerte.var
  }

  # Sonderzeichen für Latex
  nameSH <- gsub( "_" , "\\_" , name , fixed = TRUE)
  skala.items.name <- gsub( "_" , "\\_" , skala.items , fixed = TRUE)

  if( any( unname(sapply(skala.items.name , Latex.length , FALSE)) > Latex.length("Variablenname" , TRUE) ) ){
    setsizefst.items <- paste0("\\settowidth{\\sizefst}{",skala.items.name[which.max(nchar(skala.items.name))],"}")
  } else {
    setsizefst.items <- "\\settowidth{\\sizefst}{\\textbf{Variablenname}}"
  }

  #### Skript schreiben ####
  anm.tab.item.means <- "\\anmerkungen{4}{$N_{valid}$ gibt pro Item die Anzahl aller Fälle mit gültigen Werten an. \\textit{M}=Mittelwert; \\textit{SD}=Standardabweichung.}"


  skript.descriptive.items <- table.descriptive(name=skala.items, varue.info=varue.info , varue.missings=varue.missings.item , var.typ="Numerisch"  , werte=werte[[1]] , Gesamtdatensatz=Gesamtdatensatz , skala.items=NULL)
  skript.means.items <- table.means(name=skala.items , werte=werte[[1]], setsizefst=setsizefst.items , anm.tab=anm.tab.item.means , cols=data.frame(c("N_{valid}","N.valid") , c("M","mean.valid") , c("SD","sd.valid") , stringsAsFactors=FALSE), items=TRUE)
  skript.frequencies.items <- table.frequencies.items(name=skala.items , varue.info=varue.info , varue.missings.item=varue.missings.item[varue.missings.item$Var.name %in% skala.items[1],] , werte=werte , endung="_r")

  skript <- c(skript.descriptive.items ,  "\\clearpage", skript.means.items , skript.frequencies.items  , "\\clearpage")

  #### Output ####

  return( skript )
}

layout.gepoolt.metrisch <- function(name , kennwerte.var = NULL, id.fb, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	id.fb: Character, Name der Identifikiationsvariable im Datensatz
  #	varue.info: data.frame, Übersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Übersicht der Skaleninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die für die Variable den Tabellenblock erstellt
  #			Hier: Beschreibungstabelle und Tabelle mit metrischen gepoolten Kennwerten


  # Hinweis:	Die Variablen, auf Grundlage derer die Kennwerte gepoolt werden, werden im Skalenhandbuch nicht extra berichtet (wie bei Skalen).
  #			Daher werden Anmerkungen, Instruktionen etc. zu diesen Variablen bei der gepoolten Variablen eingefügt.


  #### Vorbereitung ####

  # Reduzierte Skala-Varue der Variableninformationen
  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]

  # Kennwerte berechnen bzw. übernehmen
  if( is.null(kennwerte.var) ){
    werte <- kennwerte( name=name, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
  } else {
    werte <- kennwerte.var
  }

  # Items der gepoolten Variable
  skala.items <- gsub( "\\s", "", unlist( strsplit( skalen.info[ tolower( skalen.info$varName ) %in% tolower( name ), "Items_der_Skala" ], ",", fixed = TRUE ) ) )

  # Reduktion der Items, falls diese in der Varue ein "nein" haben
  skala.items <- skala.items[skala.items %in% varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja" , "sh", "ds")]]

  # Sortierung der Items --> Reihenfolge im Reiter "Skaleninformationen" wird an die in der Varue angepasst
  skala.items <- varue.info$Var.Name[varue.info$Var.Name %in% skala.items]

  # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet
  names(werte) <- sub( paste0(name,"\\.(.*)$") , "\\1" , names(werte) )

  # Sonderzeichen für Latex
  nameSH <- gsub( "_" , "\\_" , name , fixed = TRUE)
  skala.items.name <- gsub( "_" , "\\_" , skala.items , fixed = TRUE)

  if( Latex.length( nameSH , FALSE) > Latex.length("Variablenname" , TRUE) ){
    setsizefst <- paste0("\\settowidth{\\sizefst}{",nameSH,"}")
  } else {
    setsizefst <- "\\settowidth{\\sizefst}{\\textbf{Variablenname}}"
  }

  # Tabellenanmerkung
  anm.tab <- "\\anmerkungen{6}{$N =$ Fallzahl; $Min. =$ Minimum; $Max. =$ Maximum. \\textit{Min.} bzw. \\textit{Max.} gibt das Minimum bzw. Maximum über die gepoolten Werte aller Imputationen an.}"


  #### Skript schreiben ####

  skript.descriptive <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings , var.typ="Numerisch" , Gesamtdatensatz=Gesamtdatensatz ,  werte=werte , skala.items=skala.items , show.kategorien=FALSE , gepoolt=TRUE)
  skript.means <- table.means(name=name , werte=werte, setsizefst=setsizefst , anm.tab=anm.tab, cols=data.frame(c("N_{valid}","N.valid") , c("M","mean.valid") , c("SD","sd.valid") , c("Min.","min.valid") , c("Max.","max.valid"), stringsAsFactors=FALSE))

  skript <- c(skript.descriptive,
              skript.means,
              "\\clearpage" )

  #### Output ####

  return( skript )

}

layout.gepoolt.kategorial <- function(name , kennwerte.var = NULL, id.fb, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	id.fb: Character, Name der Identifikiationsvariable im Datensatz
  #	varue.info: data.frame, Übersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Übersicht der Skaleninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die für die Variable den Tabellenblock erstellt
  #			Hier: Beschreibungstabelle, Haeufigkeitstabelle mit gepoolten Kennwerten und Zuordnungstabelle (s.o.)


  # Hinweis:	Die Variablen, auf Grundlage derer die Kennwerte gepoolt werden, werden im Skalenhandbuch nicht extra berichtet (wie bei Skalen).
  #			Daher werden Anmerkungen, Instruktionen etc. zu diesen Variablen bei der gepoolten Variablen eingefügt.


  #### Vorbereitung ####

  # Reduzierte Varue der Variableninformationen
  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
  varue.missings.aktuell <-  varue.missings[varue.missings$Var.name %in% name, ]

  # Kennwerte übernehemen bzw. berechnen
  if( is.null(kennwerte.var) ){
    werte <- kennwerte( name=name, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
  } else {
    werte <- kennwerte.var
  }

  # Items der gepoolten Variable
  skala.items <- gsub( "\\s", "", unlist( strsplit( skalen.info[ tolower( skalen.info$varName ) %in% tolower( name ), "Items_der_Skala" ], ",", fixed = TRUE ) ) )

  # Reduktion der Items, falls diese in der Varue ein "nein" haben
  skala.items <- skala.items[skala.items %in% varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja" , "sh", "ds")]]

  # Sortierung der Items --> Reihenfolge im Reiter "Skaleninformationen" wird an die in der Varue angepasst
  skala.items <- varue.info$Var.Name[varue.info$Var.Name %in% skala.items]

  # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet
  names(werte) <- sub( paste0(name,"\\.(.*)$") , "\\1" , names(werte) )

  #### Skript schreiben ####
  skript.descriptive <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz , werte=werte , var.typ="Numerisch" , skala.items=skala.items , gepoolt=TRUE)
  skript.frequencies <- table.frequencies(name=name , varue.missings.aktuell=varue.missings.aktuell , werte=werte)

  skript <- c(skript.descriptive,
              skript.frequencies,
              "\\clearpage" )

  #### Output ####
  return( skript )
}



