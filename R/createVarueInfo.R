####
#############################################################################
#' Create variable information template.
#'
#' Create variable information (background model, which variables in the data set, ...) template.
#'
#'@param GADSdat \code{GADSdat} object.
#'@param encodingList tbd.
#'@param makeStructure Should an automatic structuring of variables be created_
#'
#'@return Returns the variable information template.
#'
#'@examples
#'#tbd
#'
#'@export
createVarueInfo <- function(GADSdat, encodingList = NULL, makeStructure = TRUE){
  UseMethod("createVarueInfo")
}
#'@export
createVarueInfo.GADSdat <- function(GADSdat, encodingList = NULL, makeStructure = TRUE){
  var_labs <- unique(eatGADS::extractMeta(GADSdat)[, c("varName", "varLabel")])
  n <- ncol(GADSdat$dat)

  g <- rep("-" , n)
  if(makeStructure) g <- 1:n

  variableninfo <- data.frame(
    "Var.Name" = var_labs$varName,
    "in.DS.und.SH" = rep("ja" , n),
    "Layout" = rep("-" , n),
    "LabelSH" = var_labs$varLabel,
    "Anmerkung.Var" = rep("-" , n),
    "Gliederung" = g,
    "Reihenfolge" = rep("-" , n),
    "Titel" = var_labs$varLabel,
    "rekodiert" = rep("nein" , n),
    "QuelleSH" = rep("-" , n),
    "Instruktionen" = rep("-" , n),
    "Hintergrundmodell" = rep("nein" , n),
    "HGM.Reihenfolge" = rep("-" , n),
    "HGM.Variable.erstellt.aus" = rep("-" , n),
    "intern.extern" = rep("-" , n) ,
    "Seitenumbruch.im.Inhaltsverzeichnis" = rep("nein" , n) ,
    stringsAsFactors=FALSE)

  if(!is.null(encodingList)) {
    for( i in 1:length(encodingList$input)){
      variableninfo$LabelSH <- gsub(encodingList$input[i] , encodingList$output[i] , variableninfo$LabelSH , fixed=TRUE)
      variableninfo$Titel <- gsub(encodingList$input[i] , encodingList$output[i] , variableninfo$Titel , fixed=TRUE)
      variableninfo$Var.Name <- gsub(encodingList$input[i] , encodingList$output[i] , variableninfo$Var.Name , fixed=TRUE)
    }
  }
  variableninfo
}
#'@export
createVarueInfo.list <- function(GADSdat, encodingList = NULL, makeStructure = TRUE){
  lapply(GADSdat, function(x) {
    createVarueInfo(x, encodingList = encodingList, makeStructure = makeStructure)
  })
}

