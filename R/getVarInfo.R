
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
  varue.info$Reihenfolge[which(toupper(varue.info$Reihenfolge) %in% c("NA", "NULL", "-"))] <- 0
  varue.info$Reihenfolge[which(varue.info$Reihenfolge %in% "")] <- 0
  varue.info$Reihenfolge[which(is.na(varue.info$Reihenfolge))] <- 0
  varue.info$Reihenfolge[which(is.null(varue.info$Reihenfolge))] <- 0

  # Besondere Zeichen fuer Latex
  for( s in col.sonderzeichen){
    if(! s %in% names(varue.info)){
      warning(paste0("Die Spalte " , s , " existiert nicht in der uebergebenen Varue. Fuer diese Spalte wird nichts aufbereitet."))
    } else {
      varue.info[,s] <- sonderzeichen.aufbereiten(varue.info[,s])
    }
  }

 # Sortierung der Variablen nach Gliederung und Reihenfolge
  varue.info$Gliederung <- prepareStructure(varue.info$Gliederung, varNames = varue.info$Var.Name, in.DS.und.SH = varue.info$in.DS.und.SH)
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

    if(is.na(g)){
      re[ is.na(gd)] <- 0
      gd[ is.na(gd)] <- 0
    } else {
      re[ gd %in% g] <- 0
      gd[ gd %in% g] <- 0
    }
  }

  # fix special signs
  for(i in c("LabelSH", "Anmerkung.Var", "Titel", "Instruktionen")) {
    varue.info[[i]] <- sonderzeichen.aufbereiten(varue.info[[i]])
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

prepareStructure <- function(struc, varNames, in.DS.und.SH) {
  # strings to na
  no_struc <- is.na(suppressWarnings(as.numeric(struc)))
  struc[no_struc] <- NA

  if(all(is.na(struc[!in.DS.und.SH %in% "nein"]))) {
    struc[!in.DS.und.SH %in% "nein"] <- "1.1"
    return(struc)
  }
  if(!any(is.na(struc[!in.DS.und.SH %in% "nein"]))) return(struc)

  # if some variables in the codebook have Gliederungspunkte, others don't
  missing_struc <- varNames[!in.DS.und.SH %in% "nein" & is.na(struc)]
  stop("The following variable(s) should be in the codebook but contain no valid 'Gliederungspunkt' ('number.number'): ", paste(missing_struc, collapse = ", "))
}

# tbd: check structure, check that Titel column does not contain any missings (!)
check_varInfo <- function(varInfo) {
  if(!identical(names(varInfo), c("Var.Name", "in.DS.und.SH", "Unterteilung.im.Skalenhandbuch", "Layout", "LabelSH",
                                  "Anmerkung.Var", "Gliederung", "Reihenfolge", "Titel", "rekodiert", "QuelleSH",
                                  "Instruktionen", "Hintergrundmodell", "HGM.Reihenfolge", "HGM.Variable.erstellt.aus",
                                  "intern.extern","Seitenumbruch.im.Inhaltsverzeichnis"))) stop("Malformed column names in 'varInfo'.")
  if(any(is.na(varInfo$Var.Name))) stop("Missing values in 'Var.Name' column in 'varInfo'.")
  if(any(is.na(varInfo$Titel) & varInfo$in.DS.und.SH != "nein")) stop("Missing values in 'Titel' column in 'varInfo'.")
  if(any(is.na(varInfo$in.DS.und.SH))) stop("Missing values in 'in.DS.und.SH' column in 'varInfo'.")
  if(any(!varInfo$in.DS.und.SH %in% c("sh", "ds", "ja", "nein"))) stop("Invalid values in 'in.DS.und.SH' column in 'varInfo'.")
  invisible(varInfo)
}
