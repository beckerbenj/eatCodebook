####
#############################################################################
#' Create background model information.
#'
#' Create background model information.
#'
#'@param varueInfo Variable information
#'
#'@return Returns a latex snippet.
#'
#'@examples
#'#tbd
#'
#'@export
makeBGM <- function(varueInfo) {
  UseMethod('makeBGM')
}
#'@export
makeBGM.list <- function(varueInfo) {
  hint.info <- do.call('rbind' , varueInfo)
  makeBGM(hint.info)
}
#'@export
makeBGM.data.frame <- function(varueInfo) {
  hint.info <- varueInfo

  if(all(hint.info$Hintergrundmodell %in% c('','nein') | is.na(hint.info$Hintergrundmodell))){
    return(character())
  } else {
    hint.info <- hint.info[ hint.info$Hintergrundmodell %in% 'ja',]

    #### Aufbereitung des Hintergrundmodells
    # Entfernen aller Eintraege, bei denen keine Variablen angegeben ist
    hint.info <- hint.info[ ! is.na(hint.info$Var.Name), ]
    hint.info <- hint.info[ ! gsub('\\s' , '' , hint.info$Var.Name) %in% '', ]

    if(any(is.na(hint.info$HGM.Variable.erstellt.aus))) {
      hint.info$HGM.Variable.erstellt.aus[ is.na(hint.info$HGM.Variable.erstellt.aus) ] <- '-'
    }

    # Sonderzeichen fuer Latex
    hint.info$Var.Name <- sonderzeichen.aufbereiten(hint.info$Var.Name, TRUE)
    hint.info$HGM.Variable.erstellt.aus <- sonderzeichen.aufbereiten(hint.info$HGM.Variable.erstellt.aus, TRUE)
    hint.info$LabelSH <- sonderzeichen.aufbereiten(hint.info$LabelSH)

    # Aufbereitung: Wenn Eintrag '-' ist, dann diesen links ausrichten (multil vs multic)
    # frueher: zentrieren (ueber Latex-Befehl, der in der Praeambel definiert wird
    hint.info$HGM.Variable.erstellt.aus[ grepl('-',hint.info$HGM.Variable.erstellt.aus) ] <- '\\multil{-}'

    # Aufbereitung: Leerzeichen entfernen
    hint.info$HGM.Reihenfolge <-  gsub(' ' , '' , hint.info$HGM.Reihenfolge)

    # Reihenfolge: als erstes diejenigen Variablen, die in Spalte 'HGM Reihenfolge' ein, danach die mit '-' in der Reihenfolge, wie sie im SH auftreten
    if(any(grepl( '\\d' , hint.info$HGM.Reihenfolge))){
      if(all(!is.na(suppressWarnings(as.numeric(hint.info$HGM.Reihenfolge[grepl( '\\d' , hint.info$HGM.Reihenfolge)]))))){
        hint.info <- rbind(hint.info[grepl( '\\d' , hint.info$HGM.Reihenfolge),][ order(as.numeric(hint.info$HGM.Reihenfolge[grepl( '\\d' ,hint.info$HGM.Reihenfolge)])) ,]  , hint.info[! grepl( '\\d' , gsub(' ' , '' , hint.info$HGM.Reihenfolge)),] )
      } else {
        message(paste0('  Mindestens ein Eintrag der numerischen Angaben in der Spalte \'HGM Reihenfolge\' wird nicht erkannt. Die entsprechenden Eintraege (' , paste0(hint.info$HGM.Reihenfolge[grepl( '\\d' , hint.info$HGM.Reihenfolge)][ is.na(suppressWarnings(as.numeric(hint.info$HGM.Reihenfolge[grepl( '\\d' , hint.info$HGM.Reihenfolge)])))] , collapse=',' ) , ') werden bei der Sortierung der Variablen ignoriert.\n'))
        hint.info$Reihenfolge[is.na(suppressWarnings(as.numeric(hint.info$HGM.Reihenfolge[grepl( '\\d' , hint.info$HGM.Reihenfolge)])))] <- '-'
        hint.info <- rbind(hint.info[grepl( '\\d' , hint.info$HGM.Reihenfolge),][ order(as.numeric(hint.info$HGM.Reihenfolge[grepl( '\\d' ,hint.info$HGM.Reihenfolge)])) ,]  , hint.info[! grepl( '\\d' , gsub(' ' , '' , hint.info$HGM.Reihenfolge)),] )
      }
    } else {
      message('Da keine numerische Angaben vorliegen, wird nichts an der Reihenfolge geaendert.')
    }

    skript <- c( '\\clearpage',
                 '\\phantomsection',
                 '\\label{Tab:hintmod}',
                 '\\section*{Hintergrundmodell}\n',
                 '\\addcontentsline{toc}{section}{Hintergrundmodell}',
                 '\\ihead[\\leftmark]{\\leftmark \\newline \\textsc{Hintergrundmodell}}',
                 '\\captionof*{table}{\\textbf{Variablen im Hintergrundmodell}}',
                 '\\begin{xltabular}{\\textwidth}{lq{5cm}Q} % die ersten beiden Spalten sind so breit wie sie mindestens sein muessen + linksbuendig (Spaltentyp l). Die letzte Spalte ist linksbuendig+kein Blocksatz + Breite ist gleich dem Rest, der nach Rechts noch frei ist (Spaltentyp Q)',
                 '\\toprule',
                 '\\headrow',
                 '\\textbf{Hintergrundvariable} & \\textbf{Erstellt aus } & \\textbf{Inhalt der Hintergrundvariable}  \\\\',
                 '\\midrule',
                 '\\endhead',
                 '\\hline \\multicolumn{3}{c}{\\cellcolor{white} \\textit{Fortsetzung auf der n{\\"a}chsten Seite}}\\\\\\hline',
                 '\\endfoot',
                 '\\endlastfoot',
                 # '\\taburowcolors{white .. lg}',
                 paste0(hint.info$Var.Name, ' & ', hint.info$HGM.Variable.erstellt.aus, ' & ', hint.info$LabelSH, ' \\\\'),
                 '\\bottomrule',
                 '\\end{xltabular}')
    skript
  }
}
