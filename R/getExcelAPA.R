
####
#############################################################################
#' Import APA references from an Excel sheet.
#'
#' Import APA references from one sheet from an Excel file, while translating italic formatting and URLs into proper LaTeX code.
#'
#'
#'@param filePath Path to Excel file. The reference sheet should have two columns:the first containing short APA references, the second long APA references. Be mindful of formatting your references correctly in the Excel sheet, if there's more than one continuous italic sequence the function might not work.
#'@param sheet number of the sheet where your APA references are.
#'
#'@return A \code{data.frame} with short and long APA references. The long references contain LaTeX code, so italic input and URLs are displayed correctly.
#'
#'@examples
#'
#'@export

getExcelAPA <- function(filePath, sheet = 2){
  checkmate::assert_character(filePath)
  checkmate::assert_numeric(sheet, len = 1)
  # reading in the proper Excel sheet
  if(sheet == 1){
    excelTable <- getExcel(filePath)
  } else{
    excelTable <- getExcel(filePath)[[sheet]]
  }
  # adding latex syntax for italic and URL input
  long_new <- addItalic(filePath, sheet)
  long_new <- addURL(long_new)
  # updating the data frame from the excel
  excelTable$Langangabe <- long_new
  return(excelTable)
}

addItalic <- function(filePath, sheet){
  # reading in Excel with format
  format <- xlsx_cells(filePath, sheet)
  # find Langangaben/long reference
  format <- format[format$col == 2 & !format$character == "Langangabe",]
  # saving format-info in a list
  longEntry <- format$character_formatted

  # checking right format
  checkmate::assert_list(longEntry)
  for(i in length(longEntry)){
    checkmate::assert_tibble(longEntry[[i]], max.rows = 3)
  }

  # adding italic syntax
  lang_neu <- character()
  for(i in 1:length(longEntry)){
    if(nrow(longEntry[[i]]) == 3){
      kursiv <- paste0("\\textit{", longEntry[[i]]$character[2], "}")
      # neue chr in neuer liste Speichern
      lang_neu <- c(lang_neu, paste0(longEntry[[i]]$character[1], kursiv, longEntry[[i]]$character[3]))
    } else {
      lang_neu <- c(lang_neu, longEntry[[i]]$character)
    }
  }
  return(lang_neu)
}

addURL <- function(lang_neu){
  # finding references with links/doi
  with_url <- grep("http", lang_neu, value=TRUE)
  url_pos <- grep("http", lang_neu)
  # adding url syntax
  url_neu <- c()
  for(i in with_url){
    url <- halveString(i, "http")
    url_neu <- c(url_neu, paste0(url[1], "\\urlstyle{same}\\url{http", url[2], "}"))
  }
  # creating a new object with the added latex syntax
  lang_neu[url_pos] <- url_neu
  return(lang_neu)
}

