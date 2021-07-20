# Hintergrundmodell
makehintmod <- function(varue.info,fbshort) {

  cat(paste0("Erstellen des Hintergrundmodells.\n" ))
  flush.console()

  cols <- c("Var.Name" , "Hintergrundmodell" , "HGM.Variable.erstellt.aus" , "HGM.Reihenfolge", "LabelSH")
  # Hintergrundmodell erstellen
  hint.info <- lapply( fbshort , function(d) {
    if(any(! cols %in% names(varue.info[[d]]))){
      cat(paste0(" In der Variableninformation von " , d , " fehlt/fehlen die Spalten " , paste0(cols[! cols %in% names(varue.info[[d]])] , collapse=", ") ,". Die Variablen aus diesem Instrument erscheinen nicht im Hintergrundmodell.\n" ))
      flush.console()
      return(NULL)
    } else {
      return(varue.info[[d]][,cols])
    }
  })
  hint.info <- do.call("rbind" , hint.info)

  if(all(hint.info$Hintergrundmodell %in% c("","nein") | is.na(hint.info$Hintergrundmodell))){
    return(NULL)
  } else {
    hint.info <- hint.info[ hint.info$Hintergrundmodell %in% "ja",]
    cat(paste0("Aufbereitung des Hintergrundmodells.\n" ))
    flush.console()

    cat(paste0(" Entfernen aller Eintraege, bei denen keine Variablen angegeben ist.\n" ))
    flush.console()
    hint.info <- hint.info[ ! is.na(hint.info$Var.Name), ]
    hint.info <- hint.info[ ! gsub("\\s" , "" , hint.info$Var.Name) %in% "", ]

    if(any(is.na(hint.info$HGM.Variable.erstellt.aus))) {
      hint.info$HGM.Variable.erstellt.aus[ is.na(hint.info$HGM.Variable.erstellt.aus) ] <- "-"
    }

    # Sonderzeichen fuer Latex
    cat(paste0(" Sonderzeichen bearbeiten.\n" ))
    flush.console()

    hint.info$Var.Name <- sonderzeichen.aufbereiten(hint.info$Var.Name, TRUE)
    hint.info$HGM.Variable.erstellt.aus <- sonderzeichen.aufbereiten(hint.info$HGM.Variable.erstellt.aus, TRUE)
    hint.info$LabelSH <- sonderzeichen.aufbereiten(hint.info$LabelSH)

    # Aufbereitung: Wenn Eintrag "-" ist, dann diesen links ausrichten (multil vs multic)
    # frueher: zentrieren (über Latex-Befehl, der in der Praeambel definiert wird
    hint.info$HGM.Variable.erstellt.aus[ grepl("-",hint.info$HGM.Variable.erstellt.aus) ] <- "\\multil{-}"

    # Aufbereitung: Leerzeichen entfernen
    hint.info$HGM.Reihenfolge <-  gsub(" " , "" , hint.info$HGM.Reihenfolge)

    # Reihenfolge: als erstes diejenigen Variablen, die in Spalte "HGM Reihenfolge" ein, danach die mit "-" in der Reihenfolge, wie sie im SH auftreten
    cat(paste0(" Variablen sortieren.\n"))
    flush.console()
    if(any(grepl( "\\d" , hint.info$HGM.Reihenfolge))){
      if(all(!is.na(suppressWarnings(as.numeric(hint.info$HGM.Reihenfolge[grepl( "\\d" , hint.info$HGM.Reihenfolge)]))))){
        hint.info <- rbind(hint.info[grepl( "\\d" , hint.info$HGM.Reihenfolge),][ order(as.numeric(hint.info$HGM.Reihenfolge[grepl( "\\d" ,hint.info$HGM.Reihenfolge)])) ,]  , hint.info[! grepl( "\\d" , gsub(" " , "" , hint.info$HGM.Reihenfolge)),] )
      } else {
        cat(paste0("  Mindestens ein Eintrag der numerischen Angaben in der Spalte \"HGM Reihenfolge\" wird nicht erkannt. Die entsprechenden Eintraege (" , paste0(hint.info$HGM.Reihenfolge[grepl( "\\d" , hint.info$HGM.Reihenfolge)][ is.na(suppressWarnings(as.numeric(hint.info$HGM.Reihenfolge[grepl( "\\d" , hint.info$HGM.Reihenfolge)])))] , collapse="," ) , ") werden bei der Sortierung der Variablen ignoriert.\n"))
        flush.console()
        hint.info$Reihenfolge[is.na(suppressWarnings(as.numeric(hint.info$HGM.Reihenfolge[grepl( "\\d" , hint.info$HGM.Reihenfolge)])))] <- "-"
        hint.info <- rbind(hint.info[grepl( "\\d" , hint.info$HGM.Reihenfolge),][ order(as.numeric(hint.info$HGM.Reihenfolge[grepl( "\\d" ,hint.info$HGM.Reihenfolge)])) ,]  , hint.info[! grepl( "\\d" , gsub(" " , "" , hint.info$HGM.Reihenfolge)),] )
      }
    } else {
      cat(paste0("  Da keine numerische Angaben vorliegen, wird nichts an der Reihenfolge geaendert.\n"))
      flush.console()
    }

    cat(paste0(" Erstellen des Skripts.\n" ))
    flush.console()
    skript <- c( "\\clearpage",
                 "\\phantomsection",
                 "\\label{Tab:hintmod}",
                 "\\section*{Hintergrundmodell}\n",
                 "\\addcontentsline{toc}{section}{Hintergrundmodell}",
                 "\\ihead[\\leftmark]{\\leftmark \\newline \\textsc{Hintergrundmodell}}",
                 "\\begin{longtabu}{llQ} % die ersten beiden Spalten sind so breit wie sie mindestens sein müssen + linksbuendig (Spaltentyp l). Die letzte Spalte ist linksbuendig+kein Blocksatz + Breite ist gleich dem Rest, der nach Rechts noch frei ist (Spaltentyp Q)",
                 "\\caption*{\\cellcolor{white} \\textbf{Variablen im Hintergrundmodell}}\\\\",
                 "\\toprule",
                 "\\headrow",
                 "\\textbf{Hintergrundvariable} & \\textbf{Erstellt aus } & \\textbf{Inhalt der Hintergrundvariable}  \\\\",
                 "\\midrule",
                 "\\endhead",
                 "\\hline \\multicolumn{3}{c}{\\cellcolor{white} \\textit{Fortsetzung auf der nächsten Seite}}\\\\\\hline",
                 "\\endfoot",
                 "\\endlastfoot",
                 "\\taburowcolors{white .. lg}",
                 paste0(hint.info$Var.Name, " & ", hint.info$HGM.Variable.erstellt.aus, " & ", hint.info$LabelSH, " \\\\"),
                 "\\bottomrule",
                 "\\end{longtabu}")
    return(skript)
  }
}
