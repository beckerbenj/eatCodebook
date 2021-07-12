createStructure <- function(namesVec){
  names(namesVec) <- namesVec
  all_reg <- lapply(namesVec , function(nam){
    data.frame(Titel = NA, Ebene = NA)
  })
  all_reg
}

getStructure <- function(filePath){
  sheet_names <- openxlsx::getSheetNames(filePath)
  names(sheet_names) <- sheet_names

  all_struc <- lapply(sheet_names , function(sheet_name){
    single_struc <- openxlsx::readWorkbook(xlsxFile = filePath , sheet = sheet_name, startRow = 1)[, 1:2]

    names(single_struc) <- sonderzeichen.aufbereiten(names(single_struc))
    single_struc$Titel <- gsub("[" , "{[" , single_struc$Titel , fixed=TRUE)
    single_struc$Titel <- gsub("]" , "]}" , single_struc$Titel , fixed=TRUE)

    ## checks

    single_struc
  })

  all_struc
}

