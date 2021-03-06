
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
  sheet_names <- openxlsx::getSheetNames(filePath)
  names(sheet_names) <- sheet_names

  varue.missings <- lapply(sheet_names, function(sheet_name) {
    x <- openxlsx::readWorkbook(filePath, sheet = sheet_name, startRow = 1)
    prepareMissings(x)
  })

  varue.missings
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


