####
#############################################################################
#' Create variable information template.
#'
#' Create variable information (background model, which variables in the data set, ...) template.
#'
#'@param GADSdat \code{GADSdat} object.
#'@param inputForDescriptives Input for descriptive statistics calculation.
#'@param encodingList tbd.
#'@param makeStructure Should an automatic structuring of variables be created?
#'
#'@return Returns the variable information template.
#'
#'@examples
#'#tbd
#'
#'@export
createVarInfo <- function(GADSdat, inputForDescriptives, encodingList = NULL, makeStructure = FALSE){
  UseMethod("createVarInfo")
}
#'@export
createVarInfo.GADSdat <- function(GADSdat, inputForDescriptives, encodingList = NULL, makeStructure = FALSE){
  inputForDescriptives <- check_inputForDescriptives(inputForDescriptives)

  var_labs <- unique(eatGADS::extractMeta(GADSdat)[, c("varName", "varLabel")])

  var_labs2 <- var_labs

  ## inputed variables
  inputed_info <- inputForDescriptives[inputForDescriptives$imp == TRUE, ]
  pooled_variables <- unique(inputed_info[["group"]])
  #browser()
  for(i in pooled_variables) {
    #browser()
    single_inputed_info <- inputed_info[inputed_info$group == i, ]
    first_entry <- single_inputed_info[1, "varName"]
    newRow <- var_labs2[var_labs2$varName == first_entry, ]
    newRow[, "varName"] <- i

    var_labs2 <- insertRow(var_labs2, newRow = newRow, index = which(var_labs2$varName == first_entry))
  }

  ## network variables
  netw_info <- inputForDescriptives[inputForDescriptives$imp == FALSE & !inputForDescriptives$group %in% inputForDescriptives$varName, ]
  netw_abstracts <- unique(netw_info[["group"]])
  netw_variables <- unique(netw_info[["varName"]])
  for(i in netw_abstracts) {
    single_netw_info <- netw_info[netw_info$group == i, ]
    first_entry <- single_netw_info[1, "varName"]
    newRow <- var_labs2[var_labs2$varName == first_entry, ]
    newRow[, "varName"] <- i

    var_labs2 <- insertRow(var_labs2, newRow = newRow, index = which(var_labs2$varName == first_entry))
  }

  ## scales
  scale_variables <- inputForDescriptives[which(inputForDescriptives$type == "scale"), "varName"]
  item_variables <- inputForDescriptives[which(inputForDescriptives$group %in% scale_variables & !inputForDescriptives$varName %in% scale_variables),
                                         "varName"]

  n <- nrow(var_labs2)
  g <- rep("-" , n)
  if(makeStructure) g <- 1:n

  variableninfo <- data.frame(
    "Var.Name" = var_labs2$varName,
    "in.DS.und.SH" = rep("ja" , n),
    "Unterteilung.im.Skalenhandbuch" = rep(NA , n),
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

  ## Defaults in.DS.und.SH
  variableninfo[, "in.DS.und.SH"] <- ifelse(variableninfo[, "Var.Name"] %in% pooled_variables, yes = "sh", no = variableninfo[, "in.DS.und.SH"])
  variableninfo[, "in.DS.und.SH"] <- ifelse(variableninfo[, "Var.Name"] %in% inputed_info$varName, yes = "ds", no = variableninfo[, "in.DS.und.SH"])

  variableninfo[, "in.DS.und.SH"] <- ifelse(variableninfo[, "Var.Name"] %in% netw_abstracts, yes = "sh", no = variableninfo[, "in.DS.und.SH"])
  variableninfo[, "in.DS.und.SH"] <- ifelse(variableninfo[, "Var.Name"] %in% netw_variables, yes = "ds", no = variableninfo[, "in.DS.und.SH"])

  variableninfo[, "in.DS.und.SH"] <- ifelse(variableninfo[, "Var.Name"] %in% item_variables, yes = "ds", no = variableninfo[, "in.DS.und.SH"])

  ## Defaults Titel & LabelSH
  variableninfo[, "Titel"] <- ifelse(variableninfo[, "in.DS.und.SH"] == "ds", yes = "-", no = variableninfo[, "Titel"])
  # pooled variables: has to be inserted by hand
  variableninfo[, "Titel"] <- ifelse(variableninfo[, "in.DS.und.SH"] == "sh", yes = NA, no = variableninfo[, "Titel"])
  # scales
  variableninfo[, "Titel"] <- ifelse(variableninfo[, "Var.Name"] %in% item_variables, yes = "-", no = variableninfo[, "Titel"])
  variableninfo[, "LabelSH"] <- variableninfo[, "Titel"]

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
createVarInfo.list <- function(GADSdat, inputForDescriptives, encodingList = NULL, makeStructure = FALSE){
  if(length(GADSdat) != length(inputForDescriptives)) stop("'GADSdat' and 'inputForDescriptives' lists have different lengths.")

  Map(function(single_GADSdat, single_input) {
    createVarInfo(single_GADSdat, single_input, encodingList = encodingList, makeStructure = makeStructure)
  }, single_GADSdat = GADSdat, single_input = inputForDescriptives)
}


insertRow <- function(df, newRow, index) {
  newDF <- rbind(df, newRow)
  newDF <- newDF[order(c(1:(nrow(newDF) - 1), index - 0.5)), ]
  newDF
}

