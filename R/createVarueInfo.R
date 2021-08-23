####
#############################################################################
#' Create variable information template.
#'
#' Create variable information (background model, which variables in the data set, ...) template.
#'
#'@param GADSdat \code{GADSdat} object.
#'@param inputForDescriptives Input for descriptives calculation.
#'@param encodingList tbd.
#'@param makeStructure Should an automatic structuring of variables be created_
#'
#'@return Returns the variable information template.
#'
#'@examples
#'#tbd
#'
#'@export
createVarueInfo <- function(GADSdat, inputForDescriptives, encodingList = NULL, makeStructure = TRUE){
  UseMethod("createVarueInfo")
}
#'@export
createVarueInfo.GADSdat <- function(GADSdat, inputForDescriptives, encodingList = NULL, makeStructure = TRUE){
  var_labs <- unique(eatGADS::extractMeta(GADSdat)[, c("varName", "varLabel")])

  var_labs2 <- var_labs
  inputed_info <- inputForDescriptives[inputForDescriptives$imp == TRUE, ]
  pooled_variables <- unique(inputed_info[, "group"])
  for(i in pooled_variables) {
    #browser()
    single_inputed_info <- inputed_info[inputed_info$group == i, ]
    first_entry <- single_inputed_info[1, "varName"]
    newRow <- var_labs2[var_labs2$varName == first_entry, ]
    newRow[, "varName"] <- i

    var_labs2 <- insertRow(var_labs2, newRow = newRow, index = which(var_labs2$varName == first_entry))
  }

  # insert pooled variables here? with modified variable label
  # defaults in.DS.und.SH (ja/sh/ds)

  n <- nrow(var_labs2)
  g <- rep("-" , n)
  if(makeStructure) g <- 1:n

  variableninfo <- data.frame(
    "Var.Name" = var_labs2$varName,
    "in.DS.und.SH" = rep("ja" , n),
    "Layout" = rep("-" , n),
    "LabelSH" = var_labs2$varLabel,
    "Anmerkung.Var" = rep("-" , n),
    "Gliederung" = g,
    "Reihenfolge" = rep("-" , n),
    "Titel" = var_labs2$varLabel,
    "rekodiert" = rep("nein" , n),
    "QuelleSH" = rep("-" , n),
    "Instruktionen" = rep("-" , n),
    "Hintergrundmodell" = rep("nein" , n),
    "HGM.Reihenfolge" = rep("-" , n),
    "HGM.Variable.erstellt.aus" = rep("-" , n),
    "intern.extern" = rep("-" , n) ,
    "Seitenumbruch.im.Inhaltsverzeichnis" = rep("nein" , n) ,
    stringsAsFactors=FALSE)

  variableninfo[, "in.DS.und.SH"] <- ifelse(variableninfo[, "Var.Name"] %in% pooled_variables, yes = "sh", no = variableninfo[, "in.DS.und.SH"])
  variableninfo[, "in.DS.und.SH"] <- ifelse(variableninfo[, "Var.Name"] %in% inputed_info$varName, yes = "ds", no = variableninfo[, "in.DS.und.SH"])

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
createVarueInfo.list <- function(GADSdat, inputForDescriptives, encodingList = NULL, makeStructure = TRUE){
  Map(function(single_GADSdat, single_input) {
    createVarueInfo(single_GADSdat, single_input, encodingList = encodingList, makeStructure = makeStructure)
  }, single_GADSdat = GADSdat, single_input = inputForDescriptives)
}


insertRow <- function(df, newRow, index) {
  newDF <- rbind(df, newRow)
  newDF <- newDF[order(c(1:(nrow(newDF) - 1), index - 0.5)), ]
  newDF
}

