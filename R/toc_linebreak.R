# Funktion, um im Inhaltsverzeichnis einen Zeilenumbruch zu setzen
toc_linebreak <- function( var_title, bold , space_title, do.print=TRUE ){
  if(do.print){
    message("Check, ob Zeilenumbrueche ins Inhaltsverzeichnis muessen (rekursive Funktion).")
  }

  #if(grepl("^Haben", var_title)) browser()

  var_title_clean <- gsub("[\\p{Zs}]+", " ", var_title, perl = TRUE)
  # clean non-breaking whitespaces (https://blog.tonytsai.name/blog/2017-12-04-detecting-non-breaking-space-in-r/)
  # otherwise leads potentially to not infinite recursion as function can not find any whitespace in a title that is too long

  splitted_strings_with_ws <-  unname(unlist(strsplit(var_title_clean , "(?<= )", perl = TRUE)))
  splitted_strings <-  unname(unlist(strsplit(var_title_clean , " ", perl = TRUE)))
  length_of_splitted_strings <- sapply(splitted_strings_with_ws, Latex.length, bold = bold, in.cm=FALSE)

  k <- 1
  while(sum(length_of_splitted_strings[1:k]) < space_title){
    k <- k+1
  }

  l <- nchar(paste0(splitted_strings[1:(k-1)] , collapse=" "))
  left_title <- paste0(splitted_strings[k:length(splitted_strings)] , collapse=" ")
  if( Latex.length(left_title, bold=bold, FALSE) > space_title ){
    pre_linebreak <- paste0(splitted_strings[1:(k-1)] , collapse=" ")
    post_linebreak <- toc_linebreak( left_title , bold , space_title , do.print=FALSE)
    return(paste0(pre_linebreak, "\\texorpdfstring{\\newline}{}" , post_linebreak ))
  } else {
    pre_linebreak <- paste0(splitted_strings[1:(k-1)] , collapse=" ")
    return(paste0(pre_linebreak, "\\texorpdfstring{\\newline}{}" , left_title ) )
  }
}
