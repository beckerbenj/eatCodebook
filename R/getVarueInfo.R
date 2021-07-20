
get.varue.info <- function(varue.file , sheets, fbshort , varue.cols=c("Var.Name" , "in.DS.und.SH" , "Layout" , "LabelSH" , "Anmerkung.Var" , "Gliederung" , "Reihenfolge" , "Titel" , "rekodiert","QuelleSH" , "Instruktionen" , "Hintergrundmodell", "HGM.Variable.erstellt.aus", "HGM.Reihenfolge", "intern.extern", "Seitenumbruch.im.Inhaltsverzeichnis" )){
  cat(paste0("Einlesen der Variableninfos.\n"))
  flush.console()
  varue.info <- lapply( sheets , function(d) openxlsx::readWorkbook ( xlsxFile = varue.file , sheet = d, startRow = 1 ) )
  varue.info <- lapply( 1:length(fbshort) , function(d) varue.info[[d]][, varue.cols] )
  names(varue.info) <- fbshort

  varue.info <- lapply(varue.info , varue.info.aufbereiten)
  names(varue.info) <- fbshort

  return(varue.info)
}

varue.info.aufbereiten <- function(varue.info , col.sonderzeichen=c("LabelSH" , "Titel" , "QuelleSH" , "Anmerkung.Var")){
  cat(paste0(" Aufbereitung der Variableninformationen.\n"))
  flush.console()
  # Variableninfromationen - zu character-Strings
  varue.info$Var.Name <- as.character(varue.info$Var.Name)
  varue.info$in.DS.und.SH <- as.character(varue.info$in.DS.und.SH)
  varue.info$Layout <- as.character(varue.info$Layout)
  varue.info$LabelSH <- as.character(varue.info$LabelSH)

  if(any(grepl("\\s" , varue.info$Var.Name))){
    cat(paste0("  Die Variable/Variablen ", varue.info$Var.Name[grepl("\\s" , varue.info$Var.Name)] ," besitzt/besitzen Leerzeichen im Variablennamen. Diese werden entfernt.\n"))
    flush.console()
    varue.info$Var.Name <- gsub("\\s" , "" , varue.info$Var.Name)
  }

  cat(paste0("  Aufbereitung der Titel, Anmerkungen und Quellen.\n"))
  flush.console()
  # Variablentitel bearbeiten
  varue.info$Titel <- sub( "^\\s*(.*)\\s*$" , "\\1" , varue.info$Titel )
  varue.info$Titel[which(toupper(varue.info$Titel) %in% "NA")] <- "-"
  varue.info$Titel[which(toupper(varue.info$Titel) %in% "NULL" )] <- "-"
  varue.info$Titel[which(varue.info$Titel %in% "")] <- "-"
  varue.info$Titel[which(is.na(varue.info$Titel))] <- "-"
  varue.info$Titel[which(is.null(varue.info$Titel))] <- "-"

  # Variablenanmerkungen bearbeiten
  varue.info$Anmerkung.Var <- sub( "^\\s*(.*)\\s*$" , "\\1" , varue.info$Anmerkung.Var )
  varue.info$Anmerkung.Var[which(toupper(varue.info$Anmerkung.Var) %in% "NA")] <- "-"
  varue.info$Anmerkung.Var[which(toupper(varue.info$Anmerkung.Var) %in% "NULL" )] <- "-"
  varue.info$Anmerkung.Var[which(varue.info$Anmerkung.Var %in% "")] <- "-"
  varue.info$Anmerkung.Var[which(is.na(varue.info$Anmerkung.Var))] <- "-"
  varue.info$Anmerkung.Var[which(is.null(varue.info$Anmerkung.Var))] <- "-"

  # Variablentitel bearbeiten
  varue.info$QuelleSH <- sub( "^\\s*(.*)\\s*$" , "\\1" , varue.info$QuelleSH )
  varue.info$QuelleSH[which(toupper(varue.info$QuelleSH) %in% "NA")] <- "-"
  varue.info$QuelleSH[which(toupper(varue.info$QuelleSH) %in% "NULL" )] <- "-"
  varue.info$QuelleSH[which(varue.info$QuelleSH %in% "")] <- "-"
  varue.info$QuelleSH[which(is.na(varue.info$QuelleSH))] <- "-"
  varue.info$QuelleSH[which(is.null(varue.info$QuelleSH))] <- "-"

  # Reihenfolge bearbeiten
  varue.info$Reihenfolge <- sub( "^\\s*(.*)\\s*$" , "\\1" , varue.info$Reihenfolge )
  varue.info$Reihenfolge[which(toupper(varue.info$Reihenfolge) %in% "NA")] <- 0
  varue.info$Reihenfolge[which(toupper(varue.info$Reihenfolge) %in% "NULL" )] <- 0
  varue.info$Reihenfolge[which(varue.info$Reihenfolge %in% "")] <- 0
  varue.info$Reihenfolge[which(is.na(varue.info$Reihenfolge))] <- 0
  varue.info$Reihenfolge[which(is.null(varue.info$Reihenfolge))] <- 0

  # Instruktionen aufbereiten
  varue.info$Instruktionen <- sub( "^\\s*(.*)\\s*$" , "\\1" , varue.info$Instruktionen )
  varue.info$Instruktionen[which(toupper(varue.info$Instruktionen) %in% "NA")] <- "-"
  varue.info$Instruktionen[which(toupper(varue.info$Instruktionen) %in% "NULL" )] <- "-"
  varue.info$Instruktionen[which(varue.info$Instruktionen %in% "")] <- "-"
  varue.info$Instruktionen[which(is.na(varue.info$Instruktionen))] <- "-"
  varue.info$Instruktionen[which(is.null(varue.info$Instruktionen))] <- "-"
  varue.info$Instruktionen <- gsub("/" , "\\slash " , varue.info$Instruktionen , fixed=TRUE)


  # Besondere Zeichen fuer Latex

  cat(paste0("  Sonderzeichen bearbeiten.\n"))
  flush.console()
  for( s in col.sonderzeichen){
    if(! s %in% names(varue.info)){
      warning(paste0("\n   Die Spalte " , s , " existiert nicht in der uebergebenen Varue. Fuer diese Spalte wird nichts aufbereitet.\n"))
    } else {
      varue.info[,s] <- sonderzeichen.aufbereiten(varue.info[,s])
    }
  }

  cat(paste0("  Sortierung der Variablen nach Gliederung und Reihenfolge.\n"))
  flush.console()

  gd <- varue.info$Gliederung
  re <- varue.info$Reihenfolge

  for( g in gd ){
    if(! is.na(g)){
      bool <- suppressWarnings( is.na(as.numeric(re[gd %in% g])))
      if(all(bool)){
        re[gd %in% g] <- 1:length(which(bool))
      } else if(any(bool)){
        re[gd %in% g][bool] <- as.numeric(max(re[gd %in% g][! bool] , na.rm=TRUE))+(1:length(which(bool)))
      }
    }

    if(suppressWarnings( is.na(as.numeric(g)))){
      if(any(! varue.info$in.DS.und.SH[ gd %in% g] %in% "nein")){
        stop(paste0("   Die Variablen " , paste0( varue.info$Var.Name[ ! varue.info$in.DS.und.SH %in% "nein" & gd %in% g] , collapse=", ") , " sollen ins Skalenhandbuch, besitzen aber keinen validen Gliederungspunkt (der Form \'Zahl.Zahl\').\n\n"))
      } else {
        if(is.na(g)){
          re[ is.na(gd)] <- 0
          gd[ is.na(gd)] <- 0
        } else {
          re[ gd %in% g] <- 0
          gd[ gd %in% g] <- 0
        }
      }
    }
  }


  # Sortieren nach Gliederung -> Die Funktion zur Generierung des Gesamt-Tex-Skripts benoetigt einen Vektor mit Variablennamen, die sortiert eingegeben werden.
  varue.info <- varue.info[ order( as.numeric(gd) , as.numeric(re) ), ]

  return(varue.info)
}
