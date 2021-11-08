# Funktion, um im Inhaltsverzeichnis einen Zeilenumbruch zu setzen
toc_linebreak <- function( var_title, bold , space_title, do.print=TRUE ){
  if(do.print){
    message("Check, ob Zeilenumbrüche ins Inhaltsverzeichnis muessen (rekursive Funktion).")
  }
  k <- 1
  while(sum(sapply(1:k , function(v) Latex.length( unname(unlist(strsplit(var_title , " ")))[v] , bold=bold , FALSE) +  Latex.length( " " , bold=bold, FALSE)) ) < space_title){
    k <- k+1
  }
  l <- nchar(paste0(unname(unlist(strsplit(var_title , " ")))[1:(k-1)] , collapse=" "))
  left_title <- paste0(unname(unlist(strsplit(var_title , " ")))[k:length(unname(unlist(strsplit(var_title , " "))))] , collapse=" ")
  if( Latex.length(left_title, bold=bold, FALSE) > space_title ){
    return( paste0(paste0(unname(unlist(strsplit(var_title , " ")))[1:(k-1)] , collapse=" ") , "\\texorpdfstring{\\newline}{}" , toc_linebreak( left_title , bold , space_title , do.print=FALSE) ) )
  } else {
    return(paste0(paste0(unname(unlist(strsplit(var_title , " ")))[1:(k-1)] , collapse=" ") , "\\texorpdfstring{\\newline}{}" , left_title ) )
  }
}
