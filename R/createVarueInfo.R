createVarueInfo <- function(ds , encoding.list , set.gliederung=TRUE, make.gliederung){
  n <- length(ds)

  if(make.gliederung){
    g <- 1:n
  } else {
    g <- rep("-" , n)
  }

  variableninfo <- data.frame(
    "Var.Name"=names(ds),
    "in.DS.und.SH"=rep("ja" , n),
    "Layout"=rep("-" , n),
    "LabelSH"=unname(attr(ds , "variable.labels")),
    "Anmerkung.Var"=rep("-" , n),
    "Gliederung"=g,
    "Reihenfolge"=rep("-" , n),
    "Titel"=unname(attr(ds , "variable.labels")),
    "rekodiert"=rep("nein" , n),
    "QuelleSH"=rep("-" , n),
    "Instruktionen"=rep("-" , n),
    "Hintergrundmodell"=rep("nein" , n),
    "HGM.Reihenfolge"=rep("-" , n),
    "HGM.Variable.erstellt.aus"=rep("-" , n),
    "intern.extern"=rep("-" , n) ,
    "Seitenumbruch.im.Inhaltsverzeichnis"=rep("nein" , n) ,
    stringsAsFactors=FALSE)

  for( i in 1:length(encoding.list$input)){
    variableninfo$LabelSH <- gsub(encoding.list$input[i] , encoding.list$output[i] , variableninfo$LabelSH , fixed=TRUE)
    variableninfo$Titel <- gsub(encoding.list$input[i] , encoding.list$output[i] , variableninfo$Titel , fixed=TRUE)
    variableninfo$Var.Name <- gsub(encoding.list$input[i] , encoding.list$output[i] , variableninfo$Var.Name , fixed=TRUE)
  }


  return(variableninfo)
}
