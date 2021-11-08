
####
#############################################################################
#' Import variable information.
#'
#' Import variable information.
#'
#'@param filePath Path to excel file.
#'
#'@return variable information.
#'
#'@examples
#'#tbd
#'
#'@export
getVarInfo <- function(filePath){
  getExcel(filePath, funList = list(check_varInfo, prepareVarInfo))
}

prepareVarInfo <- function(varue.info , col.sonderzeichen=c("LabelSH" , "Titel" , "QuelleSH" , "Anmerkung.Var")){
  # Variableninfromationen - zu character-Strings
  varue.info$Var.Name <- as.character(varue.info$Var.Name)
  varue.info$in.DS.und.SH <- as.character(varue.info$in.DS.und.SH)
  varue.info$Layout <- as.character(varue.info$Layout)
  varue.info$LabelSH <- as.character(varue.info$LabelSH)

  # Titel, Anmerkungen, Quellen bearbeiten
  varue.info$Titel <- replaceNASignes(varue.info$Titel)
  varue.info$Anmerkung.Var <- replaceNASignes(varue.info$Anmerkung.Var)
  varue.info$QuelleSH <- replaceNASignes(varue.info$QuelleSH)
  # Instruktionen aufbereiten
  varue.info$Instruktionen <- replaceNASignes(varue.info$Instruktionen)
  varue.info$Instruktionen <- gsub("/" , "\\slash " , varue.info$Instruktionen , fixed=TRUE)
  # Reihenfolge bearbeiten
  varue.info$Reihenfolge <- sub( "^\\s*(.*)\\s*$" , "\\1" , varue.info$Reihenfolge )
  varue.info$Reihenfolge[which(toupper(varue.info$Reihenfolge) %in% "NA")] <- 0
  varue.info$Reihenfolge[which(toupper(varue.info$Reihenfolge) %in% "NULL" )] <- 0
  varue.info$Reihenfolge[which(varue.info$Reihenfolge %in% "")] <- 0
  varue.info$Reihenfolge[which(is.na(varue.info$Reihenfolge))] <- 0
  varue.info$Reihenfolge[which(is.null(varue.info$Reihenfolge))] <- 0

  # Besondere Zeichen fuer Latex
  for( s in col.sonderzeichen){
    if(! s %in% names(varue.info)){
      warning(paste0("\n   Die Spalte " , s , " existiert nicht in der uebergebenen Varue. Fuer diese Spalte wird nichts aufbereitet.\n"))
    } else {
      varue.info[,s] <- sonderzeichen.aufbereiten(varue.info[,s])
    }
  }

 # Sortierung der Variablen nach Gliederung und Reihenfolge
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
  varue.info
}


replaceNASignes <- function(char_vec) {
  char_vec <- sub( "^\\s*(.*)\\s*$" , "\\1" , char_vec )
  char_vec[which(toupper(char_vec) %in% c("NA", "NULL", ""))] <- "-"
  char_vec[which(is.na(char_vec))] <- "-"
  char_vec
}

# tbd: check structure, check that Titel column does not contain any missings (!)
check_varInfo <- function(varInfo) {
  invisible(varInfo)
}
