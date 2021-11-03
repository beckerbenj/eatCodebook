
####
#############################################################################
#' Get value information.
#'
#' Get information on value level (value labels and missing codes).
#'
#'@param filePath Path to the excel-file.
#'
#'@return Returns the value information.
#'
#'@examples
#'#tbd
#'
#'@export
getMissings <- function(filePath){
  getExcel(filePath, funList = list(check_missings, prepareMissings))
}

check_missings <- function(missings) {
  if(!is.data.frame(missings)) stop("'missings' must be a data.frame.")
  if(!identical(names(missings), c("Var.name", "Wert", "missing", "LabelSH", "Zeilenumbruch_vor_Wert"))) {
    stop("Column names for 'missings' must be: Var.name, Wert, missing, LabelSH, Zeilenumbruch_vor_Wert'")
  }
  invisible(missings)
}

prepareMissings <- function(varue.missings){
  varue.missings.aufb <- varue.missings

  #Sortierung der Werte

  # Werteinfos
  varue.missings.aufb$Var.name <- as.character(varue.missings.aufb$Var.name)
  varue.missings.aufb$Wert <- as.character(varue.missings.aufb$Wert)
  varue.missings.aufb$missing <- as.character(varue.missings.aufb$missing)
  varue.missings.aufb$LabelSH <- as.character(varue.missings.aufb$LabelSH)

  var.num <- unique( varue.missings.aufb$Var.name)
  for(j in var.num){
    if(any(grepl(",",varue.missings.aufb[varue.missings.aufb$Var.name %in% j, "Wert" ]))){
      varue.missings.aufb[varue.missings.aufb$Var.name %in% j, "Wert" ] <- sub("(\\d*)(,)(\\d*)","\\1\\.\\3",varue.missings.aufb[varue.missings.aufb$Var.name %in% j, "Wert" ], fixed=FALSE)
    }
    if(suppressWarnings(any(is.na( as.numeric(varue.missings.aufb$Wert[varue.missings.aufb$Var.name %in% j ] ))))){
      varue.missings.aufb[varue.missings.aufb$Var.name %in% j, ] <- rbind(varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "nein", ][order(gsub(",",".",varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "nein", "Wert"] , fixed=TRUE)) , ] , varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "ja", ][order(gsub(",",".",varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "ja", "Wert"] , fixed=TRUE)) , ])
    } else {
      varue.missings.aufb[varue.missings.aufb$Var.name %in% j, ] <- rbind(varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "nein", ][order(as.numeric(varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "nein", "Wert"])) , ] , varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "ja", ][order(abs(as.numeric(varue.missings.aufb[varue.missings.aufb$Var.name %in% j & varue.missings.aufb$missing %in% "ja", "Wert"] ))) , ])
    }
  }

  varue.missings.aufb$LabelSH <- sonderzeichen.aufbereiten(varue.missings.aufb$LabelSH)

  varue.missings.aufb
}


