
####
#############################################################################
#' Import APA references from an Excel sheet.
#'
#' Import APA references from one sheet from an Excel file, while translating italic formatting and URLs into proper LaTeX code.
#'
#'
#'@param filePath Path to Excel file. The reference sheet should have two columns:the first containing short APA references, the second long APA references. Be mindful of formatting your references correctly in the Excel sheet, if there's more than one continuous italic sequence the function might not work.
#'
#'@return A \code{data.frame} with short and long APA references. The long references contain LaTeX code, so italic input and URLs are displayed correctly.
#'
#'@examples
#'
#'@export

getAPAInfo <- function(filePath){
  # checks
  checkmate::assert_character(filePath, len = 1)

  # getting proper Excel sheet -------------------------------------------------
  ref_table <- getExcel(filePath)

  if(is_APAInfo(ref_table)){
    ref_table_format <- tidyxl::xlsx_cells(filePath, include_blank_cells = FALSE)
  } else {
    for(i in 1:length(ref_table)){
      if(is_APAInfo(ref_table[[i]])){
        ref_table_format <- tidyxl::xlsx_cells(filePath, include_blank_cells = FALSE,
                                                sheets = i)
        ref_table <- ref_table[[i]]
      }
    }
  }
  check_APAInfo(ref_table)

  # selecting objects with proper format: from col 2 and without "Langangabe"
  format <- ref_table_format[ref_table_format$col == 2 & !ref_table_format$character == "Langangabe",]
  #View(format)

  # adding latex syntax for italic input ---------------------------------------

  # saving format-info in a list
  ref_formatInfo <- format$character_formatted
  # checking right format
  checkmate::assert_list(ref_formatInfo)
  for(i in length(ref_formatInfo)){
    checkmate::assert_tibble(ref_formatInfo[[i]], max.rows = 3)
  }

  # adding italic syntax
  ref_italic <- character()
  for(i in 1:length(ref_formatInfo)){
    if(nrow(ref_formatInfo[[i]]) == 3){
      italic <- addLatex_Italic(ref_formatInfo[[i]]$character[2])
      # save new chr in list `ref_italic`
      ref_italic <- c(ref_italic, paste0(ref_formatInfo[[i]]$character[1], italic, ref_formatInfo[[i]]$character[3]))
    } else{
      ref_italic <- c(ref_italic, ref_formatInfo[[i]]$character)
    }
  }

  # adding latex syntax for URL input ------------------------------------------

  # finding references with links/doi
  with_url <- grep("http", ref_italic, value=TRUE)
  url_pos <- grep("http", ref_italic)

  # adding url syntax
  url_latex <- c()
  for(i in with_url){
    split <- strsplit(i, "http")
    link <- paste("http", split[2])

    link_latex <- addLatex_URL(link)
    url_latex <- c(url_latex, paste0(split[1], link_latex))
    #link <- "https:\\thisisalink.de"
  }
  # creating a new object with the added latex syntax
  ref_new[url_pos] <- url_latex


  # updating the data frame from the excel -------------------------------------
  ref_table$Langangabe <- ref_new
  return(ref_table)
}

# adds latex syntax around any (singular) string
addLatex_Italic <- function(string){
  # checks
  checkmate::assert_character(string, len = 1)

  string_italic <- paste0("\\textit{", string, "}")
  return(string_italic)
}

# adds latex syntax around a link/string
addLatex_URL <- function(link){
  # finding references with links/doi
  #with_url <- grep("http", lang_neu, value=TRUE)
  #url_pos <- grep("http", lang_neu)
  # adding url syntax
  #url_neu <- c()
  #for(i in with_url){
  #  url <- halveString(i, "http")
  #  url_neu <- c(url_neu, paste0(url[1], "\\urlstyle{same}\\url{http", url[2], "}"))
  #}
  # creating a new object with the added latex syntax
  #lang_neu[url_pos] <- url_neu
  return(link)
}


check_APAInfo <- function(ref_table) {
  #browser()
  if(!is.data.frame(ref_table)) stop("File path must be an Excel file with at least one sheet or a data.frame.")
  if(!identical(names(ref_table), c("Kurzangabe", "Langangabe"))) stop("Column names in at least one sheet must be 'Kurzangabe' and 'Langangabe'.")
 NULL
}

is_APAInfo <- function(ref_table) {
  #browser()
  if(!is.data.frame(ref_table)) return(FALSE)
  if(!identical(names(ref_table), c("Kurzangabe", "Langangabe"))) return(FALSE)
  return(TRUE)
}

