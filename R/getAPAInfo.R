
#################################################################################
#' Import APA references from an Excel sheet.
#'
#' Import references with APA standard from one sheet from an Excel file, while converting italic formatting and URLs into proper LaTeX code. If the references don't comply with APA standards (e.g. book or journal titles in italic), this function might not work properly. For instance, if there's more than one continuous italic sequence per cell.
#' Use case for creating a codebook: the reference list is saved in Excel, but the pdf is created wit TeXWorks, so all special formatting needs LaTeX syntax to be displayed correctly in the final codebook.
#'
#'
#'@param filePath Path to the Excel file containing a sheet with the reference list. There should be exactly one reference list with two columns `Kurzangabe` (containing in-text citations) and `Langangabe` (containing the references).
#'
#'@return A \code{data.frame} with in-text citations and APA standard references. The references in the column `Langangabe` contain LaTeX syntax for italic sequences and URLs.
#'
#'@examples
#'# import spss exemplary data
#'file <- system.file("extdata", "example_literatur.xlsx", package = "eatCodebook")
#'references <- getAPAInfo(file)
#'
#'@export

getAPAInfo <- function(filePath){
  # checks
  checkmate::assert_character(filePath, len = 1)

  # identify proper Excel sheet -------------------------------------------------
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

  # select objects with proper format: from col 2 and without "Langangabe"
  format <- ref_table_format[ref_table_format$col == 2 & !ref_table_format$character == "Langangabe",]

  # adding latex syntax for italic input ---------------------------------------
  # saving format-info in a list
  ref_formatInfo <- format$character_formatted
  # check format
  checkmate::assert_list(ref_formatInfo)
  for(i in length(ref_formatInfo)){
    checkmate::assert_tibble(ref_formatInfo[[i]], max.rows = 3)
  }
  # add italic syntax
  ref_latex <- character()
  for(i in 1:length(ref_formatInfo)){
    if(nrow(ref_formatInfo[[i]]) == 3){
      italic <- addLatex_italic(ref_formatInfo[[i]]$character[2])
      # save new chr in list `ref_latex`
      ref_latex <- c(ref_latex, paste0(ref_formatInfo[[i]]$character[1], italic, ref_formatInfo[[i]]$character[3]))
    } else{
      ref_latex <- c(ref_latex, ref_formatInfo[[i]]$character)
    }
  }

  # adding latex syntax for URL input ------------------------------------------
  # find references with links/doi
  with_url <- grep("http", ref_latex, value=TRUE)
  url_pos <- grep("http", ref_latex)
  # add url syntax
  url_latex <- c()
  url <- eatTools::halveString(with_url, "http", colnames = c("ref", "link"))
  url[,2] <- paste0("http", url[,2])
  url_latex <- c(url_latex, paste0(url[,1], addLatex_URL(url[,2])))

  # add URL latex syntax to `ref_latex`
  ref_latex[url_pos] <- url_latex

  # updating the data frame from the Excel -------------------------------------
  ref_table$Langangabe <- ref_latex
  return(ref_table)
}

addLatex_italic <- function(string){
  # checks
  checkmate::assert_character(string)
  # add latex syntax to (singular) string
  string_italic <- paste0("\\textit{", string, "}")
  return(string_italic)
}

# adds latex syntax around a link/string
addLatex_URL <- function(link){
  # checks
  checkmate::assert_character(link, pattern = "http")
  # add URL syntax to a link
  link_latex <- paste0("\\urstyle{same}\\url{", link, "}")
  return(link_latex)
}

check_APAInfo <- function(ref_table) {
  #browser()
  if(checkmate::test_list(ref_table)) stop("Excel file must contain a sheet with the two columns 'Kurzangabe' and 'Langangabe'.")
  if(!is.data.frame(ref_table)) stop("'ref_table' must be a data frame.")
  if(!identical(names(ref_table), c("Kurzangabe", "Langangabe"))) stop("Column names in 'ref_table' must be 'Kurzangabe' and 'Langangabe'.")
 NULL
}

is_APAInfo <- function(ref_table) {
  #browser()
  if(!is.data.frame(ref_table)) return(FALSE)
  if(!identical(names(ref_table), c("Kurzangabe", "Langangabe"))) return(FALSE)
  return(TRUE)
}

