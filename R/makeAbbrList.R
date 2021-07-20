

####
#############################################################################
#' Create abbreviation list.
#'
#' Create abbreviation lists.
#'
#'@param filePath Path to the file.
#'@param sheets Main title of the document.
#'@param headings Sub title of the document.
#'@param captions Authors of the document.
#'@param sort.entries Additional contributers to the document.
#'
#'@return Returns a latex snippet.
#'
#'@examples
#'#tbd
#'
#'@export
makeAbbrList <- function(filePath, sheets=c("Akronyme" , "Statistische Formelzeichen") , headings=list("Akronyme"=c( "Abkürzung", "Bedeutung") , "Statistische Formelzeichen"=c( "Symbol", "Bedeutung")) , captions=list("Akronyme"=c( "Abkürzungen") , "Statistische Formelzeichen"=c( "Statistische Formelzeichen"))  , sort.entries=c(TRUE,TRUE)){

  names(sort.entries) <- sheets

  alle.infos <- lapply( sheets , function(s) {
    v <- readWorkbook ( xlsxFile = varue.file , sheet = s, startRow = 1 )
    if(any(!names(v) %in% headings[[s]])){
      warning(paste0(" Die angegebenen Ueberschriften befinden sich nicht in der ersten Zeile im Reiter ",s,". Die uebergebenen Ueberschriften werden als Spaltennamen der ersten beiden Spalten uebernommen.\n\n"))
      names(v) <- headings[[s]]
    }
    v <- v[ , headings[[s]] ]
    v <- v[ sapply(1:dim(v)[1] , function(zeile) any(sapply(1:dim(v)[2] , function(spalte) ! is.na(v[zeile,spalte])))),]
    v <- v[ sapply(1:dim(v)[1] , function(zeile) any(sapply(1:dim(v)[2] , function(spalte) ! (gsub("\\s", "" , v[zeile,spalte]) %in% "" | gsub("\\s", "" , v[zeile,spalte]) %in% "" ) ))),]
    if(sort.entries[s]){
      v <- v[ order(v[,1]),]
    }
    return(v)
  } )

  names(alle.infos) <- sheets

  alle.code <- lapply(1:length(sheets) , function(i){
    cat(paste0(" Erstelle Latex-Code für Sheet " , i, ".\n"))
    flush.console()
    v <- alle.infos[[i]]

    if(length(v[,1])>1){
      v.code <- c(paste0("\\begin{longtabu}{l",rep("Q",dim(v)[2]-1),"}"),
                  paste0("\\caption*{\\cellcolor{white} \\textbf{",captions[i],"}}\\\\"),
                  "\\toprule",
                  "\\headrow",
                  paste0(paste0("\\textbf{" , headings[[i]] , "}" , collapse=" & ") , "\\\\"),
                  "\\midrule",
                  "\\endhead",
                  paste0("\\hline \\multicolumn{",dim(v)[2],"}{@{}c@{}}{\\cellcolor{white} \\textit{Fortsetzung auf der nächsten Seite}}\\\\\\hline"),
                  "\\endfoot",
                  "\\endlastfoot",
                  "\\taburowcolors{white .. lg}",
                  paste0(sapply(1:dim(v)[1] , function(d) paste0(v[d,] , collapse=" & " )) , "\\\\"),
                  "\\nobreakbottomrule",
                  "\\end{longtabu}\n")
      return(v.code)
    } else {
      return(NULL)
    }
  })

  # Abkürzungsverzeichnis
  if(all(unname(sapply(alle.code , is.null)))){
    abkuerzverz <- NULL
  } else {
    cat(paste0(" Zusammenführen aller Codes.\n"))
    flush.console()
    abkuerzverz <- c( "\\clearpage",
                      "\\phantomsection",
                      "\\section*{Abkürzungsverzeichnis}\n",
                      "\\addcontentsline{toc}{section}{Abkürzungsverzeichnis}",
                      "%\\clearscrheadings",
                      "%\\cfoot[\\pagemark]{\\pagemark}",
                      "\\ihead[\\leftmark]{\\leftmark \\newline \\textsc{Abkürzungsverzeichnis}}",
                      do.call("c" , alle.code))
  }

  return(abkuerzverz)
}
