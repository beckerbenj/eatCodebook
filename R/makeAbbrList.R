


####
#############################################################################
#' Create abbreviation list.
#'
#' Create abbreviation lists latex snippet.
#'
#'@param filePath Path to the file.
#'@param captions tbd
#'@param sort.entries Should the abbreviation lists be sorted alphabetically?
#'
#'@return Returns a latex snippet.
#'
#'@examples
#'#tbd
#'
#'@export
makeAbbrList <- function(filePath, captions=list('Akronyme'= 'Abk{\\"u}rzungen', 'Statistische Formelzeichen' = 'Statistische Formelzeichen'),
                         sort.entries = TRUE){

  sheet_names <- openxlsx::getSheetNames(filePath)
  names(sheet_names) <- sheet_names
  sheet_names <- sort(sheet_names)

  alle.infos <- lapply(sheet_names, function(s) {
    v <- openxlsx::readWorkbook ( xlsxFile = filePath, sheet = s, startRow = 1 )
    if(sort.entries) v <- v[ order(v[,1]),]
    v
  })

  names(alle.infos) <- sheet_names

  alle.code <- lapply(1:length(sheet_names) , function(i){
    v <- alle.infos[[i]]
    v.code <- NULL

    if(nrow(v) >= 1){
      v.code <- c(paste0('\\captionof*{table}{\\textbf{',captions[i],'}}'),
                  paste0('\\begin{xltabular}{\\textwidth}{l',rep('X',dim(v)[2]-1),'}'),
                  '\\toprule',
                  '\\headrow',
                  paste0(paste0('\\textbf{' , names(v) , '}' , collapse=' & ') , '\\\\'),
                  '\\midrule',
                  '\\endhead',
                  paste0('\\hline \\multicolumn{',dim(v)[2],'}{@{}c@{}}{\\cellcolor{white} \\textit{Fortsetzung auf der n{\\"a}chsten Seite}}\\\\\\hline'),
                  '\\endfoot',
                  '\\endlastfoot',
                  #'\\taburowcolors{white .. lg}',
                  paste0(sapply(1:dim(v)[1] , function(d) paste0(v[d,] , collapse=' & ' )) , '\\\\'),
                  '\\nobreakbottomrule',
                  '\\end{xltabular}\n')
    }
    v.code
  })

  # Abkuerzungsverzeichnis
  if(all(unname(sapply(alle.code , is.null)))){
    abkuerzverz <- NULL
  } else {
    abkuerzverz <- c( '\\clearpage',
                      '\\phantomsection',
                      '\\section*{Abk{\\"u}rzungsverzeichnis}\n',
                      '\\addcontentsline{toc}{section}{Abk{\\"u}rzungsverzeichnis}',
                      '%\\clearscrheadings',
                      '%\\cfoot[\\pagemark]{\\pagemark}',
                      '\\ihead[\\leftmark]{\\leftmark \\newline \\textsc{Abk{\\"u}rzungsverzeichnis}}',
                      do.call('c' , alle.code))
  }

  return(abkuerzverz)
}
