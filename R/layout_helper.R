#### LAYOUT-FUNKTIONEN ####

# Die Layout-Funktionen unterscheiden sich in zwei Arten: Die erste Art sind layoutspezifische Funktionen.
# Jede Variable bekommt in der Varue einen Layouttyp, durch den in diesem Skript entschieden wird, welche Layout-Funktion aufgerufen wird (layout.id, layout.kategorial, ...).
# Innerhalb dieser Layout-Funktionen werden Funktion der zweiten Art aufgerufen (table.descriptive, table.means, ...). Jede dieser Funktion erstellt das Skript fuer eine Tabelle (Beschreibungstabelle, Haeufigkeitstabelle etc.). Das sind layoutuebergreifende Funk
# Diese Aufteilung soll die Bearbeitung der Funktionen vereinfachen: Sollte sich etwas fuer nur einen Layouttyp veraendern, so kann das in den Funktion erster Art vorgenommen werden. Sollte sich etwas an einer Tabellenart aendern, die fuer alle Layouttypen verwendet wird, so kann das in den Funktionen zweiter Art getan werden.


# Funktionen erster Art: Funktionen pro Layouttyp (Layoutspezifisch)
# layout.id:				: Skript fuer Identifikationsvariablen (Tabelle mit Variablenbeschreibung)
# layout.string				: Skript fuer String-Variablen (Tabelle mit Variablenbeschreibung)
# layout.kategorial			: Skript fuer kategoriale Variablen (Tabelle mit Variablenbeschreibung und Haeufigkeitstabelle)
# layout.ordinal			: Skript fuer ordinale Variablen (Tabelle mit Variablenbeschreibung, Haeufigkeitstabelle und Tabelle mit metrischen Kennwerten)
# layout.metrisch			: Skript fuer metrische Variablen (Tabelle mit Variablenbeschreibung und Tabelle mit metrischen Kennwerten)
# layout.skala				: Skript fuer Skalen (Tabelle mit Variablenbeschreibungen (Skala und Items), Haeufigkeitstabelle (Items) und Tabelle mit metrischen Kennwerten (Skala) )
# layout.gepoolt.metrisch	: Skript fuer gepoolte metrische Variablen (Tabelle mit Variablenbeschreibung und Tabelle mit metrischen Kennwerten)
# layout.gepoolt.kategorial	: Skript fuer gepoolte kategoriale Variablen (Tabelle mit Variablenbeschreibung und Haeufigkeitstabelle)
# layout.numerisch.geleert	: Skript fuer vom FDZ geleerte numerische Variablen (Tabelle mit Variablenbeschreibung)

# Funktionen zweiter Art: Funktionen pro Tabellentyp (Layoutuebergreifend)
# table.descriptive: Tabelle 'Beschreibung der Variable'
# table.means: Tabelle mit metrischen Kennwerten, auch fuer die Tabelle 'Itemanalyse' von Items in enier Skala
# table.frequencies: Haeufigkeitstabelle fuer einzelne Variablen
# table.frequencies.items: Haeufigkeitstabelle fuer Items, die zu einer Skala gehoeren



# layout.id:				: Skript fuer Identifikationsvariablen (Tabelle mit Variablenbeschreibung)
# layout.string				: Skript fuer String-Variablen (Tabelle mit Variablenbeschreibung)
# layout.kategorial			: Skript fuer kategoriale Variablen (Tabelle mit Variablenbeschreibung und Haeufigkeitstabelle)
# layout.ordinal			: Skript fuer ordinale Variablen (Tabelle mit Variablenbeschreibung, Haeufigkeitstabelle und Tabelle mit metrischen Kennwerten)
# layout.metrisch			: Skript fuer metrische Variablen (Tabelle mit Variablenbeschreibung und Tabelle mit metrischen Kennwerten)
# layout.skala				: Skript fuer Skalen (Tabelle mit Variablenbeschreibungen (Skala und Items), Haeufigkeitstabelle (Items) und Tabelle mit metrischen Kennwerten (Skala) )
# layout.gepoolt.metrisch	: Skript fuer gepoolte metrische Variablen (Tabelle mit Variablenbeschreibung und Tabelle mit metrischen Kennwerten)
# layout.gepoolt.kategorial	: Skript fuer gepoolte kategoriale Variablen (Tabelle mit Variablenbeschreibung und Haeufigkeitstabelle)
# layout.numerisch.geleert	: Skript fuer vom FDZ geleerte numerische Variablen (Tabelle mit Variablenbeschreibung)

layout.id <- function( name , varue.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	varue.info: data.frame, Uebersicht der Variableninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die fuer die Variable den Tabellenblock erstellt
  #			Hier nur Beschreibungstabelle


  #### Skript schreiben ####
  skript <- c(table.descriptive(name=name, varue.info=varue.info , var.typ='ID-Variable') , '\\clearpage' )


  #### Output ####

  return( skript )
}

layout.numerisch.geleert <- function( name , varue.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	varue.info: data.frame, Uebersicht der Variableninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die fuer die Variable den Tabellenblock erstellt
  #			Hier nur Beschreibungstabelle



  #### Skript schreiben ####
  skript <- c(table.descriptive(name=name, varue.info=varue.info , var.typ='Numerisch') , '\\clearpage' )


  #### Output ####

  return( skript )
}

layout.string <- function( name , varue.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	varue.info: data.frame, Uebersicht der Variableninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die fuer die Variable den Tabellenblock erstellt
  #			Hier nur Beschreibungstabelle

  #### Skript schreiben ####

  skript <- c(table.descriptive(name=name, varue.info=varue.info , var.typ='Zeichenfolge') , '\\clearpage' )

  #### Output ####

  return( skript )
}

layout.kategorial <- function(name , kennwerte.var = NULL, varue.info, varue.missings, Gesamtdatensatz) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	varue.info: data.frame, Uebersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenuebersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die fuer die Variable den Tabellenblock erstellt
  #			Hier Beschreibungstabelle und Haeufigkeitstabelle

  # ANMERKUNG: Zur Erstellung der Haeufigkeitstabellen sind verschiedene Aenderungen an den Kennwerten und der Werteinformationen aus der Varue noetig.
  #			 Zuerst wird eine neue Variableninformation erstellt, sofern keine validen Kategorien definiert sind, also nur Missing in der Varue stehen. Valide Werte muessen berichtet werden.
  #			 In der Tabelle sollen alle validen Kategorien aufgelistet werden, aber nur besetzte Missing-Kategorien.
  #			 Zusaetzlich werden Sysmis berichtet, sofern diese besetzt sind.
  #			 Außerdem wird eine Tabellenanmerkung eingefuegt, sofern Kategorien besetzt sind, aber die gerundete Prozentzahl '0.0' ist.

  #### Vorbereitung ####

  # Reduzierte Varue der Variableninformationen
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name %in% name,]

  if( all( varue.missings.aktuell$missing %in% 'ja' ) ) {
    ## Reduzierte Varue der Werteinformationen neu erstellen

    # Missingkategorien
    missings <- varue.missings$Wert[varue.missings$Var.name == name & varue.missings$missing=='ja']

    # alle auftretenden validen Werte finden
    level.valid	<- sort ( unique( Gesamtdatensatz[ ! Gesamtdatensatz[ , name] %in% missings  & ! is.na( Gesamtdatensatz[ , name] ) , name ] ) )

    # alle Werte
    level <- c(level.valid , missings )

    # Variablenname
    varname <- rep(name, length( level ))

    # Label fuer das Skalenhandbuch
    labelsh <- c( rep('', length(level.valid) ) , varue.missings$LabelSH[varue.missings$Var.name %in% name ] )

    # Wertekategorien
    varval = c( rep('nein', length(level.valid) ) , rep('ja' , length(missings) ) )

    # Reduzierte Varue der Werteinformationen erstellen
    varue.missings.aktuell 	<-	data.frame( 'Var.Name'=varname,
                                           'Wert'=level,
                                           'missing'=varval,
                                           'LabelSH'=labelsh,
                                           stringsAsFactors = FALSE )

    }
  werte <- kennwerte.var

  # Sonderzeichen fuer Latex
  nameSH <- gsub( '_' , '\\_' , name , fixed = TRUE)

  #### Skript schreiben ####
  skript.descriptive <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz , werte=werte , var.typ='Numerisch' , skala.items=NULL)

  skript.frequencies <- table.frequencies(name=name , varue.missings.aktuell=varue.missings.aktuell , werte=werte)

  skript <- c(skript.descriptive,
              skript.frequencies,
              '\\clearpage' )

  #### Output ####
  return( skript )

}

layout.ordinal <- function(name , kennwerte.var = NULL, varue.info, varue.missings, Gesamtdatensatz) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	varue.info: data.frame, Uebersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenuebersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die fuer die Variable den Tabellenblock erstellt
  #			Hier Beschreibungstabelle, Tabelle mit metrischen Kennwerten und Haeufigkeitstabelle

  # ANMERKUNG: Zur Erstellung der Haeufigkeitstabellen sind verschiedene Aenderungen an den Kennwerten und der Werteinformationen aus der Varue noetig.
  #			 Zuerst wird eine neue Variableninformation erstellt, sofern keine validen Kategorien definiert sind, also nur Missing in der Varue stehen. Valide Werte muessen berichtet werden.
  #			 In der Tabelle sollen alle validen Kategorien aufgelistet werden, aber nur besetzte Missing-Kategorien.
  #			 Zusaetzlich werden Sysmis berichtet, sofern diese besetzt sind.
  #			 Außerdem wird eine Tabellenanmerkung eingefuegt, sofern Kategorien besetzt sind, aber die gerundete Prozentzahl '0.0' ist.

  #### Vorbereitung ####

  # Reduzierte Varue der Variableninformationen
  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name %in% name, ]

  # Kennwerte
  werte <- kennwerte.var

  # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet
  names(werte) <- sub( paste0(name,'\\.(.*)$') , '\\1' , names(werte) )

  # Sonderzeichen fuer Latex
  nameSH <- gsub( '_' , '\\_' , name , fixed = TRUE)


  # Falls Variablenname zu lang fuer Tabelle
  if( Latex.length( nameSH , FALSE) > Latex.length('Variablenname' , TRUE) ){
    setsizefst <- paste0('\\settowidth{\\sizefst}{',nameSH,'}')
  } else {
    setsizefst <- '\\settowidth{\\sizefst}{\\textbf{Variablenname}}'
  }

  anm.tab.ordinal <- paste0('\\anmerkungen{4}{$N =$ Fallzahl; $M =$ Mittelwert; $SD =$ Standardabweichung.}')


  #### Skript schreiben ####

  skript.descriptive <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings , var.typ='Numerisch' , werte=werte , skala.items=NULL)
  skript.frequencies <- table.frequencies(name=name, varue.missings.aktuell=varue.missings.aktuell , werte=werte)
  skript.means <- table.means(name=name , werte=werte, setsizefst=setsizefst , cols=data.frame(c('N_{valid}','N.valid') , c('M','mean.valid') , c('SD','sd.valid') , stringsAsFactors=FALSE ) , anm.tab=anm.tab.ordinal)
  skript <- c(skript.descriptive,
              skript.means,
              skript.frequencies,
              '\\clearpage' )

  #### Output ####

  return( skript )
}

layout.metrisch <- function(name , kennwerte.var = NULL, varue.info, varue.missings, Gesamtdatensatz) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	varue.info: data.frame, Uebersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenuebersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die fuer die Variable den Tabellenblock erstellt
  #			Hier Beschreibungstabelle und Tabelle mit metrischen Kennwerten


  #### Vorbereitung ####

  # Reduzierte Varue
  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name %in% name,]
  # Kennwerte
  werte <- kennwerte.var

  # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet
  names(werte) <- sub( paste0(name,'\\.(.*)$') , '\\1' , names(werte) )

  # Sonderzeichen fuer Latex
  nameSH <- gsub( '_' , '\\_' , name , fixed = TRUE)

  if( Latex.length( nameSH , FALSE) > Latex.length('Variablenname' , TRUE) ){
    setsizefst <- paste0('\\settowidth{\\sizefst}{',nameSH,'}')
  } else {
    setsizefst <- '\\settowidth{\\sizefst}{\\textbf{Variablenname}}'
  }

  anm.tab.metrisch <- paste0('\\anmerkungen{6}{$N =$ Fallzahl; $M =$ Mittelwert; $SD =$ Standardabweichung; $Min. =$ Minimum; $Max. =$ Maximum. $N_{total}$~=~',length(Gesamtdatensatz[,name]),'.}')

  #### Skript schreiben ####

  skript.descriptive <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings , var.typ='Numerisch' , Gesamtdatensatz=Gesamtdatensatz ,  werte=werte , skala.items=NULL, show.kategorien=FALSE)

  skript.means <- table.means(name=name , werte=werte, setsizefst=setsizefst , cols=data.frame(c('N_{valid}','N.valid') , c('M','mean.valid') , c('SD','sd.valid') , c('Min.','min.valid') , c('Max.','max.valid'), stringsAsFactors=FALSE), anm.tab = anm.tab.metrisch)

  skript <- c(skript.descriptive,
              skript.means,
              '\\clearpage' )

  #### Output ####
  return( skript )

}

layout.skala <- function(name , kennwerte.var = NULL, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	varue.info: data.frame, Uebersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenuebersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Uebersicht der Skaleninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die fuer die Variable den Tabellenblock erstellt
  #			Skala: Beschreibungstabelle und Tabelle mit metrischen Kennwerten
  #			Items: Beschreibungstabelle, Labeltabelle, Tabelle mit metrischen Kennwerten und Haeufigkeitstabelle


  # YRNTCSH! - Der Funktion 'table.frequencies.items' (ganz am Ende) muss die richtige Endung uebergeben werden, mit der rekodierte Variablen identifizert werden koennen. Im LV12 war dies einheitlich '.r', im LV15 '_r'. In der genannten Funktion muss dann die Variablenspezifikation 'endung='_r'' durch die neue Endung in der Form'endung='NEUE_ENDUNG'' ersetzt werden.

  #### Vorbereitung ####

  # Reduzierte Skala-Varue der Variableninformationen
  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
  varue.missings.aktuell <- varue.missings[varue.missings$Var.name %in% name,]

  # Identifikation der zur Skala zugehoerigen Items
  skala.items <- gsub( '\\s', '', unlist( strsplit( skalen.info[ tolower( skalen.info$varName ) %in% tolower( name ), 'Items_der_Skala' ], ',', fixed = TRUE ) ) )

  # Reduktion der Items, falls diese in der Varue auf 'nein' gesetzt sind
  skala.items <- skala.items[skala.items %in% varue.info$Var.Name[varue.info$in.DS.und.SH %in% c('ja' , 'sh', 'ds')]]

  # Sortierung der Items --> Reihenfolge im Reiter 'Skaleninformationen' wird an die in der Varue angepasst
  skala.items <- varue.info$Var.Name[varue.info$Var.Name %in% skala.items]

  # Reduzierte Item-Varue der Werteinformation (ueber ein nicht-rekodierten Items) - um richtige Ordnung der Werte zu bekommen
  #if( all( sub( '.*\\.(.{1})$', '\\1' ,skala.items ) %in% 'r' ) ) {
  #	varue.missings.item <- varue.missings[varue.missings$Var.name %in% skala.items[1],]
  #} else {
  #	varue.missings.item <- varue.missings[varue.missings$Var.name %in% skala.items[ which( ! sub( '.*\\.(.{1})$', '\\1' ,skala.items ) %in% 'r' )[1]],]
  #}

  varue.missings.item <- varue.missings[ varue.missings$Var.name %in% skala.items , ]
  varue.missings.item <- varue.missings.item[ ! duplicated(varue.missings.item$Wert) , ]
  varue.missings.item <- lapply(skala.items , function(d) data.frame('Var.name'=rep(d , length(varue.missings.item$Wert)) , 'Wert'=varue.missings.item$Wert , 'missing'=varue.missings.item$missing , 'LabelSH' = varue.missings.item$LabelSH , 'Zeilenumbruch_vor_Wert'=varue.missings.item$Zeilenumbruch_vor_Wert , stringsAsFactors=FALSE) )
  varue.missings.item <- do.call('rbind' , varue.missings.item)

  # Reduzierte Item-Varue der Variableninformationen
  varue.info.item <- varue.info[varue.info$varName %in% skala.items[1],]

  # Kennwerte
  werte <- kennwerte.var

  # Sonderzeichen fuer Latex
  nameSH <- gsub( '_' , '\\_' , name , fixed = TRUE)
  skala.items.name <- gsub( '_' , '\\_' , skala.items , fixed = TRUE)

  # Anzahl gueltiger Werte zur Berechnung der Skala
  if( skalen.info$Anzahl_valider_Werte[ tolower(skalen.info$varName) %in% tolower(name) ] %in% c('einem','ein','eins') ) {
    anmerkung.anzahl_valider_werte <- ' F{\\"u}r die Berechnung der Skalenkennwerte wurden alle Teilnehmenden einbezogen, die auf mindestens einem Item einen g{\\"u}ltigen Wert aufweisen ($N_{valid}$).'
  } else if( skalen.info$Anzahl_valider_Werte[ tolower(skalen.info$varName) %in% tolower(name) ] %in% c('alle','allen','all') ) {
    anmerkung.anzahl_valider_werte <- ' F{\\"u}r die Berechnung der Skalenkennwerte wurden alle Teilnehmenden einbezogen, die auf allen Items einen g{\\"u}ltigen Wert aufweisen ($N_{valid}$).'
  } else if( skalen.info$Anzahl_valider_Werte[ tolower(skalen.info$varName) %in% tolower(name) ] %in% '-' ) {
    anmerkung.anzahl_valider_werte <- NULL
  } else {
    anmerkung.anzahl_valider_werte <- paste0(' F{\\"u}r die Berechnung der Skalenkennwerte wurden alle Teilnehmenden einbezogen, die auf mindestens ', skalen.info$Anzahl_valider_Werte[ tolower(skalen.info$varName) %in% tolower(name) ],' Items einen g{\\"u}ltigen Wert aufweisen ($N_{valid}$).')
  }

  if( Latex.length( nameSH , FALSE) > Latex.length('Variablenname' , TRUE) ){
    setsizefst <- paste0('\\settowidth{\\sizefst}{',nameSH,'}')
  } else {
    setsizefst <- '\\settowidth{\\sizefst}{\\textbf{Variablenname}}'
  }

  if( any( unname(sapply(skala.items.name , Latex.length , FALSE)) > Latex.length('Variablenname' , TRUE) ) ){
    setsizefst.items <- paste0('\\settowidth{\\sizefst}{',skala.items.name[which.max(nchar(skala.items.name))],'}')
  } else {
    setsizefst.items <- '\\settowidth{\\sizefst}{\\textbf{Variablenname}}'
  }

  ### reinbrowsen um Bug in Lfk zu finden (bis hier hin ok!) -> Problem bei skript.frequencies.items!!

  #### Skript schreiben ####
  anm.tab.skala <- paste0('\\anmerkungen{7}{$N =$ Fallzahl; $Min. =$ Minimum; $Max. =$ Maximum; $\\alpha =$~Cronbachs Alpha (Cronbach, 1951).', anmerkung.anzahl_valider_werte,' F{\\"u}r die Reliabilit{\\"a}tsanalyse wurden nur Teilnehmende einbezogen, die auf allen Items g{\\"u}ltige Werte besitzen.}')
  anm.tab.item.means <- '\\anmerkungen{5}{$N_{valid}$ gibt pro Item die Anzahl aller F{\\"a}lle mit g{\\"u}ltigen Werten an. Bei der Trennsch{\\"a}rfe~$r_{pw}$ handelt es sich um die part-whole-korrigierte Korrelation des jeweiligen Items mit der Skala.}'

  skript.descriptive.skala <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings , var.typ='Numerisch' , werte=werte , Gesamtdatensatz=Gesamtdatensatz , skala.items=skala.items, show.kategorien=FALSE)
  skript.means.skala <- table.means(name=name , werte=t(werte[[1]])[1,], setsizefst=setsizefst , anm.tab=anm.tab.skala , cols=data.frame(c('N_{valid}','N.valid') , c('M','mean.valid') , c('SD','sd.valid') , c('Min.','min.valid') , c('Max.','max.valid') , c('\\alpha' , 'alpha'), stringsAsFactors=FALSE))

  skript.descriptive.items <- table.descriptive(name=skala.items, varue.info=varue.info , varue.missings=varue.missings.item , var.typ='Numerisch'  , werte=werte[[2]] , Gesamtdatensatz=Gesamtdatensatz , skala.items=NULL)
  skript.means.items <- table.means(name=skala.items , werte=werte[[2]], setsizefst=setsizefst.items , anm.tab=anm.tab.item.means , cols=data.frame(c('N_{valid}','N.valid') , c('M','mean.valid') , c('SD','sd.valid') , c('r_{pw}','cor.valid') , stringsAsFactors=FALSE) , items=TRUE)
  skript.frequencies.items <- table.frequencies.items(name=skala.items , varue.info=varue.info , varue.missings.item=varue.missings.item[varue.missings.item$Var.name %in% skala.items[1],] , werte=werte , endung='_r')

  skript <- c( skript.descriptive.skala , skript.means.skala , '\\clearpage', skript.descriptive.items ,  '\\clearpage', skript.means.items , skript.frequencies.items  , '\\clearpage')

  #### Output ####

  return( skript )
}

layout.skala.fake <- function(name , kennwerte.var = NULL, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	varue.info: data.frame, Uebersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenuebersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Uebersicht der Skaleninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die fuer die Variable den Tabellenblock erstellt
  #			Skala: Beschreibungstabelle und Tabelle mit metrischen Kennwerten
  #			Items: Beschreibungstabelle, Labeltabelle, Tabelle mit metrischen Kennwerten und Haeufigkeitstabelle


  # YRNTCSH! - Der Funktion 'table.frequencies.items' (ganz am Ende) muss die richtige Endung uebergeben werden, mit der rekodierte Variablen identifizert werden koennen. Im LV12 war dies einheitlich '.r', im LV15 '_r'. In der genannten Funktion muss dann die Variablenspezifikation 'endung='_r'' durch die neue Endung in der Form'endung='NEUE_ENDUNG'' ersetzt werden.

  #### Vorbereitung ####

  # Identifikation der zur Skala zugehoerigen Items
  skala.items <- gsub( '\\s', '', unlist( strsplit( skalen.info[ tolower( skalen.info$varName ) %in% tolower( name ), 'Items_der_Skala' ], ',', fixed = TRUE ) ) )

  # Reduktion der Items, falls diese in der Varue ein 'nein' haben
  skala.items <- skala.items[skala.items %in% varue.info$Var.Name[varue.info$in.DS.und.SH %in% c('ja' , 'sh', 'ds')]]


  # Sortierung der Items --> Reihenfolge im Reiter 'Skaleninformationen' wird an die in der Varue angepasst
  skala.items <- varue.info$Var.Name[varue.info$Var.Name %in% skala.items]

  # Reduzierte Item-Varue der Werteinformation (ueber ein nicht-rekodierten Items) - um richtige Ordnung der Werte zu bekommen
  varue.missings.item <- varue.missings[ varue.missings$Var.name %in% skala.items , ]
  varue.missings.item <- varue.missings.item[ ! duplicated(varue.missings.item$Wert) , ]
  varue.missings.item <- lapply(skala.items , function(d) data.frame('Var.name'=rep(d , length(varue.missings.item$Wert)) , 'Wert'=varue.missings.item$Wert , 'missing'=varue.missings.item$missing , 'LabelSH' = varue.missings.item$LabelSH , 'Zeilenumbruch_vor_Wert'=varue.missings.item$Zeilenumbruch_vor_Wert , stringsAsFactors=FALSE) )
  varue.missings.item <- do.call('rbind' , varue.missings.item)


  # Reduzierte Item-Varue der Variableninformationen
  varue.info.item <- varue.info[varue.info$Var.Name %in% skala.items[1],]

  # Kennwerte
  werte <- kennwerte.var

  # Sonderzeichen fuer Latex
  nameSH <- gsub( '_' , '\\_' , name , fixed = TRUE)
  skala.items.name <- gsub( '_' , '\\_' , skala.items , fixed = TRUE)

  if( any( unname(sapply(skala.items.name , Latex.length , FALSE)) > Latex.length('Variablenname' , TRUE) ) ){
    setsizefst.items <- paste0('\\settowidth{\\sizefst}{',skala.items.name[which.max(nchar(skala.items.name))],'}')
  } else {
    setsizefst.items <- '\\settowidth{\\sizefst}{\\textbf{Variablenname}}'
  }

  #### Skript schreiben ####
  anm.tab.item.means <- '\\anmerkungen{4}{$N_{valid}$ gibt pro Item die Anzahl aller F{\\"a}lle mit g{\\"u}ltigen Werten an. \\textit{M}=Mittelwert; \\textit{SD}=Standardabweichung.}'


  skript.descriptive.items <- table.descriptive(name=skala.items, varue.info=varue.info , varue.missings=varue.missings.item , var.typ='Numerisch'  , werte=werte[[1]] , Gesamtdatensatz=Gesamtdatensatz , skala.items=NULL)
  skript.means.items <- table.means(name=skala.items , werte=werte[[1]], setsizefst=setsizefst.items , anm.tab=anm.tab.item.means , cols=data.frame(c('N_{valid}','N.valid') , c('M','mean.valid') , c('SD','sd.valid') , stringsAsFactors=FALSE), items=TRUE)
  skript.frequencies.items <- table.frequencies.items(name=skala.items , varue.info=varue.info , varue.missings.item=varue.missings.item[varue.missings.item$Var.name %in% skala.items[1],] , werte=werte , endung='_r')

  skript <- c(skript.descriptive.items ,  '\\clearpage', skript.means.items , skript.frequencies.items  , '\\clearpage')

  #### Output ####

  return( skript )
}

layout.gepoolt.metrisch <- function(name , kennwerte.var = NULL, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	varue.info: data.frame, Uebersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenuebersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Uebersicht der Skaleninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die fuer die Variable den Tabellenblock erstellt
  #			Hier: Beschreibungstabelle und Tabelle mit metrischen gepoolten Kennwerten


  # Hinweis:	Die Variablen, auf Grundlage derer die Kennwerte gepoolt werden, werden im Skalenhandbuch nicht extra berichtet (wie bei Skalen).
  #			Daher werden Anmerkungen, Instruktionen etc. zu diesen Variablen bei der gepoolten Variablen eingefuegt.


  #### Vorbereitung ####

  # Reduzierte Skala-Varue der Variableninformationen
  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]

  # Kennwerte
  werte <- kennwerte.var

  # Imputationen der gepoolten Variable
  imputations <- gsub( '\\s', '', unlist( strsplit( skalen.info[ tolower( skalen.info$varName ) %in% tolower( name ), 'Imputationen' ], ',', fixed = TRUE ) ) )

  # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet
  names(werte) <- sub( paste0(name,'\\.(.*)$') , '\\1' , names(werte) )

  # Sonderzeichen fuer Latex
  nameSH <- gsub( '_' , '\\_' , name , fixed = TRUE)

  if( Latex.length( nameSH , FALSE) > Latex.length('Variablenname' , TRUE) ){
    setsizefst <- paste0('\\settowidth{\\sizefst}{',nameSH,'}')
  } else {
    setsizefst <- '\\settowidth{\\sizefst}{\\textbf{Variablenname}}'
  }

  # Tabellenanmerkung
  anm.tab <- '\\anmerkungen{6}{$N =$ Fallzahl; $Min. =$ Minimum; $Max. =$ Maximum. \\textit{Min.} bzw. \\textit{Max.} gibt das Minimum bzw. Maximum {\\"u}ber die gepoolten Werte aller Imputationen an.}'


  #### Skript schreiben ####

  skript.descriptive <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings , var.typ='Numerisch' , Gesamtdatensatz=Gesamtdatensatz ,  werte=werte , skala.items=NULL , imputations=imputations, show.kategorien=FALSE , gepoolt=TRUE)
  skript.means <- table.means(name=name , werte=werte, setsizefst=setsizefst , anm.tab=anm.tab, cols=data.frame(c('N_{valid}','N.valid') , c('M','mean.valid') , c('SD','sd.valid') , c('Min.','min.valid') , c('Max.','max.valid'), stringsAsFactors=FALSE))

  skript <- c(skript.descriptive,
              skript.means,
              '\\clearpage' )

  #### Output ####

  return( skript )

}

layout.gepoolt.ordinal <- function(name , kennwerte.var = NULL, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	varue.info: data.frame, Uebersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenuebersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Uebersicht der Skaleninformationen
  
  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die fuer die Variable den Tabellenblock erstellt
  #			Hier: Beschreibungstabelle, Haeufigkeitstabelle mit gepoolten Kennwerten und Zuordnungstabelle (s.o.)
  
  
  # Hinweis:	Die Variablen, auf Grundlage derer die Kennwerte gepoolt werden, werden im Skalenhandbuch nicht extra berichtet (wie bei Skalen).
  #			Daher werden Anmerkungen, Instruktionen etc. zu diesen Variablen bei der gepoolten Variablen eingefuegt.
  
  
  #### Vorbereitung ####
  
  # Reduzierte Varue der Variableninformationen
  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
  varue.missings.aktuell <-  varue.missings[varue.missings$Var.name %in% name, ]
  
  # Kennwerte
  werte <- kennwerte.var
  
  # Imputationen der gepoolten Variable
  imputations <- gsub( '\\s', '', unlist( strsplit( skalen.info[ tolower( skalen.info$varName ) %in% tolower( name ), 'Imputationen' ], ',', fixed = TRUE ) ) )
  
  # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet
  names(werte) <- sub( paste0(name,'\\.(.*)$') , '\\1' , names(werte) )
  
  # Sonderzeichen fuer Latex
  nameSH <- gsub( '_' , '\\_' , name , fixed = TRUE)
  
  if( Latex.length( nameSH , FALSE) > Latex.length('Variablenname' , TRUE) ){
    setsizefst <- paste0('\\settowidth{\\sizefst}{',nameSH,'}')
  } else {
    setsizefst <- '\\settowidth{\\sizefst}{\\textbf{Variablenname}}'
  }
  
  anm.tab.ordinal <- paste0('\\anmerkungen{4}{$N =$ Fallzahl; $M =$ Mittelwert; $SD =$ Standardabweichung.}')
  
  #### Skript schreiben ####
  skript.descriptive <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz , werte=werte , var.typ='Numerisch' , skala.items=NULL , imputations=imputations , gepoolt=TRUE)
  skript.means <- table.means(name=name , werte=werte, setsizefst=setsizefst , cols=data.frame(c('N_{valid}','N.valid') , c('M','mean.valid') , c('SD','sd.valid') , stringsAsFactors=FALSE ) , anm.tab=anm.tab.ordinal)
  skript.frequencies <- table.frequencies(name=name , varue.missings.aktuell=varue.missings.aktuell , werte=werte)
  
  skript <- c(skript.descriptive,
              skript.means,
              skript.frequencies,
              '\\clearpage' )
  
  #### Output ####
  return( skript )
}

layout.gepoolt.kategorial <- function(name , kennwerte.var = NULL, varue.info, varue.missings, Gesamtdatensatz, skalen.info) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: Character-Vektor, gelabelter Vektor mit Kennwerten im Character-Format
  #	varue.info: data.frame, Uebersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenuebersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Uebersicht der Skaleninformationen

  # OUTPUT:
  #	skript: Character-Vektor mit Skript, die fuer die Variable den Tabellenblock erstellt
  #			Hier: Beschreibungstabelle, Haeufigkeitstabelle mit gepoolten Kennwerten und Zuordnungstabelle (s.o.)


  # Hinweis:	Die Variablen, auf Grundlage derer die Kennwerte gepoolt werden, werden im Skalenhandbuch nicht extra berichtet (wie bei Skalen).
  #			Daher werden Anmerkungen, Instruktionen etc. zu diesen Variablen bei der gepoolten Variablen eingefuegt.


  #### Vorbereitung ####

  # Reduzierte Varue der Variableninformationen
  varue.info.aktuell <- varue.info[varue.info$Var.Name %in% name,]
  varue.missings.aktuell <-  varue.missings[varue.missings$Var.name %in% name, ]

  # Kennwerte
  werte <- kennwerte.var

  # Imputationen der gepoolten Variable
  imputations <- gsub( '\\s', '', unlist( strsplit( skalen.info[ tolower( skalen.info$varName ) %in% tolower( name ), 'Imputationen' ], ',', fixed = TRUE ) ) )

  # Kennwerte-names: bekommen automatisch den namen der Variable rangepastet
  names(werte) <- sub( paste0(name,'\\.(.*)$') , '\\1' , names(werte) )

  #### Skript schreiben ####
  skript.descriptive <- table.descriptive(name=name, varue.info=varue.info , varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz , werte=werte , var.typ='Numerisch' , skala.items=NULL , imputations=imputations , gepoolt=TRUE)
  skript.frequencies <- table.frequencies(name=name , varue.missings.aktuell=varue.missings.aktuell , werte=werte)

  skript <- c(skript.descriptive,
              skript.frequencies,
              '\\clearpage' )

  #### Output ####
  return( skript )
}



