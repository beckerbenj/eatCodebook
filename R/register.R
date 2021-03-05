##### REGISTER #####
cat(paste0(" LADE REGISTER-FUNKTIONEN.\n"))
flush.console()
# Funktion zur Aufbereitung der Seitenzahlen im Register
pages <- function( numbers , fb , varue.reg, double.vars) {
  # INPUT:
  #	numbers: numerischer Vektor, numerische Position der Variablen im Skalenhandbuch
  #	fb: Character, Fragebogenküzel aus fbshort
  #	varue.reg: data.frame, Informationen zum Register: data.frame, Spalten sind Schlagwörter, die im Register aufgelistet sind,
  #			   Zeilen sind VarNamen. Einträge sind "x" oder "", ob Variable unter dem Schlagwort im Register aufgelistet werden soll.
  # OUTPUT:
  #	counter: Character-Vektor mit (unsortierten, aber identifizierbaren) Latex-Befehlen, die
  #		     für das Register entweder die Seitenzahl setzen oder die Zählervariable manipulieren.

  # ANMERKUNG:
  #	Bei der Funktion pages handelt es sich um eine rekursive Funktion, d.h. innerhalb
  #	der Funktion wird die Funktion erneut aufgerufen bis verschiedene Fallunterscheidugen
  #	duchlaufen wurden.
  #	Der Output wird in der Funktion register.sw aufbereitet und nutzbar gemacht.
  #	Die Seitenzahlen, die für ein Schlagwort gesetzt werden, werden entweder nacheinander
  #	aufgelistet oder, falls Variablen aufeinanderfolgen (also numbers eine aufsteigende,
  #	nicht unterbreochene Folge von Zahlen enthält, bspw. 212,213,214), durch als Intervall
  #	berichtet. Im letzten Fall wird die höchste und niedrigste Zahl als Grenzen gesetzt.


  ##### Vorbereitung ####
  # Laufvariablen definieren, um Intervall zu identifizieren
  j <- 2

  #### Skript schreiben ####

  # Fallunterscheidung: Es gibt mehr als eine Zahl und die ersten drei Zahlen sind aufeinanderfolgend
  if ( length( numbers ) > 2 & ( numbers[1] + 1 == numbers[j] ) & ( numbers[1] + 2 == numbers[j+1] ) ) {

    # Identifikation des Eintrags der größten Zahl im Intervall
    while( numbers[j] + 1 == numbers[j+1] & !is.na(numbers[j+1]) ) j<-j+1

    # Identifikation der nachfolgenden Variable
    # Deren Seite minus 1 ergibt letzte Zahl im Intervall
    if( length( varue.reg$Var.Name ) > numbers[j] ) { # Falls nachfolgende Variable Teil des FB ist
      refpage <- paste0( numtolet( name=varue.reg$Var.Name[ numbers[j] +1 ], fb=fb ,double.vars=double.vars) )
    } else { # falls nachfolgende Variable im neuen Teil ist
      if(fb==fbshort[length(fbshort)]){
        refpage <- paste0("lit")
      } else {
        refpage <- paste0("sec",toupper(fbshort[which(fb %in% fbshort)+1]))
      }
    }

    # Schreiben des Counters
    counter <- c( paste0("\\setcounter{temp", numtolet( name=varue.reg$Var.Name[ numbers[j] ] , fb=fb ,double.vars=double.vars),"}{", "\\the", numtolet( name=varue.reg$Var.Name[ numbers[j] ] , fb=fb ,double.vars=double.vars) ,"}" ),
                  paste0("\\setcounter{", numtolet( name=varue.reg$Var.Name[ numbers[j] ] , fb=fb ,double.vars=double.vars) , "}{\\the\\numexpr\\value{", refpage, "}-1\\relax}" ),
                  paste0("\\the", sapply( varue.reg$Var.Name[ numbers[c(1,j) ] ] , numtolet , fb=fb ,double.vars=double.vars ) , collapse = "--"),
                  paste0("\\setcounter{", numtolet( name=varue.reg$Var.Name[ numbers[j] ] , fb=fb,double.vars=double.vars ) , "}{\\thetemp", numtolet( name=varue.reg$Var.Name[ numbers[j] ] , fb=fb ,double.vars=double.vars ),"}" )
    )

    #### Output und rekursiver Zugriff ####
    if ( j == length(numbers) ) { # falls alle Zahlen aufeinanderfolgend sind
      return( counter )
    } else {
      return( c( counter , pages( numbers[-c(1:j)] , fb=fb , varue.reg=varue.reg , double.vars=double.vars ) ) ) # Es wird an Funktion die um das Intervall reduzierten Zahl erneut übergeben
    }

    # Fallunterscheidung: Es gibt mehr als zwei Zahlen
  } else if ( length( numbers ) > 2 ) {

    # Schreiben des Counters
    counter <- paste0("\\the", numtolet( name=varue.reg$Var.Name[ numbers[ 1 ] ] , fb=fb ,double.vars=double.vars ) )

    #### Output und rekursiver Zugriff ####
    return ( c( counter , pages( numbers[ -c(1) ] , fb=fb , varue.reg=varue.reg , double.vars=double.vars) ) )

    # Fallunterscheidung: Es gibt eine oder zwei Zahlen
  } else if ( length( numbers ) %in% c(1,2) ) {

    # Schreiben des Counters
    counter <- paste( sapply( numbers , function(n)  paste0("\\the", numtolet( name=varue.reg$Var.Name[ n ] , fb=fb ,double.vars=double.vars ) ) ) , sep=",")

    #### Output ####
    return ( counter )
  }
}


# Funktion, um für ein Schlagwort den Registereintrag zu erstellen
register.sw <- function ( schlagwort, fb.akt , varue.reg,double.vars) {
  # INPUT:
  #	schlagwort: Schlagwort, wie es im Register in der Varue vorkommt
  #	fb.akt: Fragebogen-Kürzel aus fbshort
  #	varue.reg: Informationen zum Register: data.frame, Spalten sind Schlagwörter, die im Register aufgelistet sind,
  #			   Zeilen sind VarNamen. Einträge sind "x" oder "", ob Variable unter dem Schlagwort im Register aufgelistet werden soll.
  # OUTPUT:
  #	skript: Character-Vektor mit Latex-Befehlen, um für ein Schlagwort den Eintrag zu setzen.

  # Ausgabe des Schlagwortes- Erleichtert Fehlersuche
  cat ( paste0 ( " Register (", fb.akt , ") - Schlagwort: ",schlagwort ,"\n" ) )
  flush.console()

  #### Vorbereitung ####

  # Identifikation der numerischen Postition, die an pages übergeben wird
  numbers <- 	which( tolower( varue.reg[ ,schlagwort] ) %in% "x" )

  # unsortierte Befehle bestimmen
  counter <- pages ( numbers=numbers , fb=fb.akt , varue.reg=varue.reg , double.vars=double.vars)

  # Identifikation der Befehle, die Counter setzen
  sets <- counter[ sub( "^(\\\\setcounter).*" , "\\1" , counter ) %in% "\\setcounter"  ]

  # Identifikation der Befehle, die Counter vor Eintag setzen
  sets.before <- 	sets[ ! sub( "^\\\\setcounter\\{.*\\}\\{(\\\\thetemp).*" , "\\1" , sets ) %in% "\\thetemp" ]

  # Identifikation der Befehle, die Counter nach Eintag setzen
  sets.after <- 	sets[ sub( "^\\\\setcounter\\{.*\\}\\{(\\\\thetemp).*" , "\\1" , sets ) %in% "\\thetemp" ]

  # Eintrag
  counter <-  paste0( counter[ !sub( "^(\\\\setcounter).*" , "\\1" , counter ) %in% "\\setcounter" ], collapse = ", " )

  #### Skript schreiben ####
  skript <- c( sets.before, paste0(  "\\regitem{" , gsub( "\\." , " " , schlagwort ) , "}{", counter , "}"  ), sets.after )

  #### Output ####
  return ( skript )
}

# Funktion für gesamtes Register eines Instruments
register.ges <- function ( fb.akt , varue.reg ,double.vars) {
  # INPUT:
  #	fb.akt: Fragebogenkürzel aus fbshort
  #	varue.reg: Informationen zum Register: data.frame, Spalten sind Schlagwörter, die im Register aufgelistet sind,
  #			   Zeilen sind VarNamen. Einträge sind "x" oder "", ob Variable unter dem Schlagwort im Register aufgelistet werden soll.
  # OUTPUT:
  #	skript: Character-Vektor mit Latex-Befehlen, um das gesamte Register zu erstellen


  #### Vorbereitung ####

  # Identifikation der Schlagwörter - Vektor mit alphabetisch sortierten Schlagwörtern
  schlagwoerter <- sort( names(varue.reg)[-which(names(varue.reg) %in% "Var.Name")  ] )

  # Reduktion der Schlagworte auf diejenigen, unter denen mindestens eine Variable verschlagwortet sind
  schlagwoerter <- schlagwoerter[ sapply( schlagwoerter , function(d) any(varue.reg[,d] %in% "x") ) ]


  #### Skript schreiben ####
  skript  <- c(   "\\phantomsection" ,
                  paste0("\\section*{Register: ",fblong[fb.akt],"}"),
                  paste0("\\addcontentsline{toc}{section}{Register: ",fblong[fb.akt],"}"),
                  "%\\clearscrheadings",
                  "%\\cfoot[\\pagemark]{\\pagemark}",
                  paste0("\\ihead[\\leftmark]{\\leftmark \\newline \\textsc{Register ",toupper(fb.akt),"}}"),
                  "\\renewcommand*{\\thefootnote}{\\fnsymbol{footnote}}",
                  "\\renewcommand*{\\thefootnotemark}{\\fnsymbol{footnote}}",
                  "\\begin{register}",
                  unname( unlist( sapply( schlagwoerter , register.sw, fb.akt=fb.akt , varue.reg=varue.reg ,double.vars=double.vars) ) ),
                  "\\end{register}"
                  # Hotfix Versuch Benjamin 06.05.19 Formatierung Register
                  , "\\pagebreak"
  )

  #### Output ####
  return ( skript )
}
