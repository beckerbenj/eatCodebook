# Layout-Funktion für beliebige Variablen
layout.var <- function(name , fb, id.fb , double.vars , kennwerte.var = NULL, varue.info, varue.missings, Gesamtdatensatz, skalen.info, varue.gliederung, makeCounter , all_length ) {
  # INPUT
  #	name: Character, Name der Variable, wie sie in der Varue erscheint
  #	kennwerte.var: gelabelter Vektor mit Kennwerten
  #	fb: Character, Fragebogenküzel aus fbshort
  #	varue.info: data.frame, Übersicht der Variableninformationen
  #	varue.missings: data.frame, Variablenübersicht der Werteinformationen
  #	Gesamtdatensatz: data.frame, Datensatz des Fragebogens
  #	skalen.info: data.frame, Übersicht der Skaleninformationen
  #	varue.gliederung: data.frame, Gliederungsreiter aus der Varue

  # OUTPUT:
  #	skript: Character-Vektor, der für eine Variable alle Informationen ausgibt, die für den Bericht der Variable noetig sind.

  # ANMERKUNG:
  #	Im Skalenhandbuch werden drei Ebene unterschiedene: Kapitel, Abschnitt, Unterabschnitt.
  #	Jede Variable erhaelt in der Variableninformation neben dem Label und dem Variablennamen auch einen eigenen Titel,
  #	der im Inhaltsverzeichnis auftauchen soll. Dieser Titel befindet sich auf der dritten Ebene der Gliederung und wird
  #	als Unterabschnitt gesetzt.
  #	Jeder Variable wird eine Gliederungsebene zugeordnet, die das Kapitel (erste Ebene) und den Abschnitt (zweite Ebene)
  #	angibt, unter der die Variable im Skalenhandbuch berichtet wird. Zu einer Gliederungsbene können mehrere Variablen
  #	gehören.
  #	Falls bei zwei aufeinanderfolgenden Variablen ein neues Kapitel anfaengt oder sich der Abschnitt innerhalb des selben
  #	Kapitels aendert sich, so kann dies eindeutig über die Gliederungsebene festgestellt werden.

  #### Vorbereitung ####

  # Ausgabe des Variablennames - Erleichtert Fehlersuche
  message(paste0 ( "  Layout der Variable: " , name , "\n" ) )


  # Reduzierung der Varue: Nur diejenigen Variablen, die echt berichtet werden (also "ds" raus)
  varue.info.sh <- varue.info[ varue.info$in.DS.und.SH %in% c("ja" , "sh"), ]

  # Zaehlvariable wird auf Seitenzahl gesetzt
  if(makeCounter){
    counter <- paste0("\\setcounter{", numtolet(name, fb, double.vars), "}{\\thepage}")
  } else {
    counter <- NULL
  }


  # Gliederungsebene der Variable, die zuvor berichtet wurde
  if( which( varue.info.sh$Var.Name %in% name ) == 1 ) { # falls name die allererste Variable ist
    ebene.zuvor <-0
  } else {
    ebene.zuvor <- as.numeric( varue.info.sh$Gliederung[ which( varue.info.sh$Var.Name %in% name ) -1 ] )
  }

  ebenen.var0 <- paste0(varue.info.sh$Gliederung[ which( varue.info.sh$Var.Name %in% name ) -1]  ,"." ,
                        varue.info.sh$Reihenfolge[ which( varue.info.sh$Var.Name %in% name ) -1])
  ebenen.var1 <- paste0(varue.info.sh$Gliederung[ varue.info.sh$Var.Name %in% name ]  ,"." ,
                        varue.info.sh$Reihenfolge[ varue.info.sh$Var.Name %in% name ])

  while(grepl("\\." , ebenen.var0[1] )) {
    ebenen.var0 <- c(sub("(^.*)(\\..*$)" , "\\1" , ebenen.var0[1]),ebenen.var0)
  }


  while("." %in% unlist(strsplit(as.character(ebenen.var1[1]) , ""))) {
    ebenen.var1 <- c(sub("(^.*)(\\..*$)" , "\\1" , ebenen.var1[1]),ebenen.var1)
  }

  #browser()

  all_indents <- c(0 , 0 , 30 , 70) # Einzug für Chapter, Section, Subsection und Subsubsection in Latex (in pt)

  #browser() ## to do: wieso section bei Felix und uns so unterschiedlich? -> vergleichen!
  if( gsub("\\s","", as.character(varue.info$Reihenfolge[ varue.info$Var.Name %in% name])) == "-") {
    sections.var1 <- varue.info.sh$Titel[ varue.info.sh$Var.Name %in% name ]
    sections.var1 <- lapply(1:1 , function(d){
      right_margins <- 32.54317 # Laenge von  tocrmarg in Latex (zur Verfügung stehender Platz für den rechten Rand im Inhaltsverzeichnis, in pt)
      bold <- d %in% c(1,2)
      left_margins <- Latex.length( all_length[d+1] , bold=bold , FALSE)
      textwidth <- 455.24417 # gesamte Breite für den Text (ca. 16cm) in pt
      space_title <- floor(textwidth - left_margins - right_margins - all_indents[d])-10 # geschaetzter zur Verfügung stehender Platz für Titel im Inhaltsverzeichnis (zur Sicherheit abgerundet)
      titel.kurz <- NULL
      sections.var1[d] <- gsub("[" , "{["  , sections.var1[d] , fixed=TRUE)
      sections.var1[d] <- gsub("]" , "]}"  , sections.var1[d] , fixed=TRUE)
      if(  grepl("\\\\footnote\\{.*\\}" , sections.var1[d] ) ) {
        titel.kurz <- paste0("[" ,  sub("\\\\footnote\\{.*\\}", "" , sections.var1[d] ) , "]")
      }
      if(Latex.length(sections.var1[d] , bold=bold, FALSE)>space_title ){
        titel.kurz <- paste0("[",toc_linebreak( sections.var1[d] , bold ,  space_title),"]")
      }
      if(varue.info$Seitenumbruch.im.Inhaltsverzeichnis[ varue.info$Var.Name %in% name] %in% "ja"){
        s <- c(paste0("\\section", titel.kurz , "{", gsub("/" , "\\slash " , sections.var1[d] , fixed=TRUE) , "}"),
               "\\addtocontents{toc}{\\protect\\newpage}")
      } else {
        s <- paste0("\\section", titel.kurz , "{", gsub("/" , "\\slash " , sections.var1[d] , fixed=TRUE) , "}")
      }
      return(s)
    } )
  } else {
    sections.var1 <- c(varue.gliederung$Titel[varue.gliederung$Ebene %in% ebenen.var1 ] , varue.info.sh$Titel[ varue.info.sh$Var.Name %in% name ] )
    sections.var1 <- lapply(1:length(ebenen.var1) , function(d){
      if(!ebenen.var1[d]==ebenen.var0[d] | length(ebenen.var1)==d ){
        right_margins <- 32.54317 # Laenge von  tocrmarg in Latex (zur Verfuegung stehender Platz für den rechten Rand im Inhaltsverzeichnis, in pt)
        bold <- d %in% c(1,2)
        left_margins <- Latex.length( all_length[d+1] , bold=bold , FALSE)
        textwidth <- 455.24417 # gesamte Breite für den Text (ca. 16cm) in pt
        space_title <- floor(textwidth - left_margins - right_margins - all_indents[d+1])-10 # geschaetzter zur Verfuegung stehender Platz für Titel im Inhaltsverzeichnis (zur Sicherheit abgerundet)
        titel.kurz <- NULL
        sections.var1[d] <- gsub("[" , "{["  , sections.var1[d] , fixed=TRUE)
        sections.var1[d] <- gsub("]" , "]}"  , sections.var1[d] , fixed=TRUE)
        if(  grepl("\\\\footnote\\{.*\\}" , sections.var1[d] ) ) {
          titel.kurz <- paste0("[" ,  sub("\\\\footnote\\{.*\\}", "" , sections.var1[d] ) , "]")
        }
        if(Latex.length(sections.var1[d] , FALSE, FALSE)>space_title ){
          titel.kurz <- paste0("[",toc_linebreak( sections.var1[d] , bold , space_title),"]")
        }
        if(varue.info$Seitenumbruch.im.Inhaltsverzeichnis[ varue.info$Var.Name %in% name] %in% "ja" & d==length(ebenen.var1)){
          s <- c(paste0("\\",paste0(rep("sub",d-1) , collapse=""),"section", titel.kurz , "{", gsub("/" , "\\slash " , sections.var1[d] , fixed=TRUE) , "}"),
                 "\\addtocontents{toc}{\\protect\\newpage}")
        } else {
          s <- paste0("\\",paste0(rep("sub",d-1) , collapse=""),"section", titel.kurz , "{", gsub("/" , "\\slash " , sections.var1[d] , fixed=TRUE) , "}")
        }
        return(s)
      } else {
        return(NULL)
      }
    } )
  }

  sections.var1 <- unlist(sections.var1)

  # Identifikation des Layouttyps
  i <- varue.info$Layout[varue.info$Var.Name %in% name]

  # Beginn des Variablenblocks visuell hervorheben im Latex-Skript (durch Prozentzeichen wird Zeile kommentiert in Latex)
  var.start <- c ( "","% ========================================================================== %" ,
                   paste0 ( "%                                  ",name," %" ) ,
                   "% ========================================================================== %" , "" )


  # Anmerkung: 	Im Folgenden werden die Ueberschriften für ein evtl. neu zu beginnendes Kapitel, ein evtl. neu zu beginnender
  #				Abschnitt und den auf jeden Fall neu zu beginnenden Unterabschnitt definiert.
  #				Beim Einfuegen von Fussnoten in die Ueberschrift kann es zu Problemen im Inhaltsverzeichnis und den Bookmarks kommen.
  #				Die Probleme sind dergestalt, dass der Text für den Titel den Latex-Befehl für das Setzen der Fußnote beinhaltet, also
  #				von der Form "Kapiteltitelanfang\footnote{fußnotentext}Kapiteltitelende" ist. Um das zu korrigieren wird abgefragt, ob
  #				im Titel eine Fußnote gesetzt wird und ein Kurztitel mit "\section[Kurztitel]{Langtitel} gesetzt, der dem Langtitel ohne
  #				den Fussnotenbefehl entspricht.

  #### Skript schreiben ####

  if(!i %in% 0:9) stop("Invalid value of 'i'.")
  # Aufruf der Layout-Funktion abhaengig von Layout-typ
  if (i==0) layout.typ.var <- layout.id( name=name , varue.info=varue.info )
  if (i==1) layout.typ.var <- layout.string( name=name , varue.info=varue.info)
  if (i==2) layout.typ.var <- layout.kategorial(name=name , kennwerte.var=kennwerte.var, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
  if (i==3) layout.typ.var <- layout.ordinal(name=name , kennwerte.var=kennwerte.var, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
  if (i==4) layout.typ.var <- layout.metrisch(name=name , kennwerte.var=kennwerte.var, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
  if (i==5) layout.typ.var <- layout.skala(name=name , kennwerte.var=kennwerte.var, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
  if (i==6) layout.typ.var <- layout.gepoolt.metrisch(name=name , kennwerte.var=kennwerte.var, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info )
  if (i==7) layout.typ.var <- layout.gepoolt.kategorial(name=name , kennwerte.var=kennwerte.var, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info)
  if (i==8) layout.typ.var <- layout.numerisch.geleert( name=name , varue.info=varue.info)
  if (i==9) layout.typ.var <- layout.skala.fake(name=name , kennwerte.var = kennwerte.var, id.fb=id.fb, varue.info=varue.info, varue.missings=varue.missings, Gesamtdatensatz=Gesamtdatensatz, skalen.info=skalen.info)



  skript <- c(var.start , counter , sections.var1 , layout.typ.var)
  #### Output ####

  return ( skript )
}





