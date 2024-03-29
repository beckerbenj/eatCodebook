
### LATEX-FUNKTIONEN
# Funktion, um Breite von Woertern in Latex zu erhalten

Latex.length <- function(word , bold , in.cm=FALSE){
  stopifnot(is.character(word) && length(word) == 1)

  Latex.Letter.Length <- data.frame(
    c("a", 4.86172),c("b", 5.47499),c("c", 4.86172),c("d", 5.47499),c("e", 4.86172),c("f", 3.6463),c("g", 5.47499),c("h", 5.47499),c("i", 3.04399),c("j", 3.04399),c("k", 5.47499),c("l", 3.04399),c("m", 8.51898),c("n", 5.47499),c("o", 5.47499),c("p", 5.47499),c("q", 5.47499),c("r", 3.6463),c("s", 4.25943),c("t", 3.04399),c("u", 5.47499),c("v", 5.47499),c("w", 7.90585),c("x", 5.47499),c("y", 5.47499),c("z", 4.86172),
    c("a_fett", 5.47499),c("b_fett", 6.08812),c("c_fett", 4.86172),c("d_fett", 6.08812),c("e_fett", 4.86172),c("f_fett", 3.6463),c("g_fett", 5.47499),c("h_fett", 6.08812),c("i_fett", 3.04399),c("j_fett", 3.6463),c("k_fett", 6.08812),c("l_fett", 3.04399),c("m_fett", 9.1213),c("n_fett", 6.08812),c("o_fett", 5.47499),c("p_fett", 6.08812),c("q_fett", 6.08812),c("r_fett", 4.86172),c("s_fett", 4.25943),c("t_fett", 3.6463),c("u_fett", 6.08812),c("v_fett", 5.47499),c("w_fett", 7.90585),c("x_fett", 5.47499),c("y_fett", 5.47499),c("z_fett", 4.86172),
    c("A", 7.90585),c("B", 7.30356),c("C", 7.30356),c("D", 7.90585),c("E", 6.69043),c("F", 6.08812),c("G", 7.90585),c("H", 7.90585),c("I", 3.6463),c("J", 4.25943),c("K", 7.90585),c("L", 6.69043),c("M", 9.73444),c("N", 7.90585),c("O", 7.90585),c("P", 6.08812),c("Q", 7.90585),c("R", 7.30356),c("S", 6.08812),c("T", 6.69043),c("U", 7.90585),c("V", 7.90585),c("W", 10.33673),c("X", 7.90585),c("Y", 7.90585),c("Z", 6.69043),
    c("A_fett", 7.90585),c("B_fett", 7.30356),c("C_fett", 7.90585),c("D_fett", 7.90585),c("E_fett", 7.30356),c("F_fett", 6.69043),c("G_fett", 8.51898),c("H_fett", 8.51898),c("I_fett", 4.25943),c("J_fett", 5.47499),c("K_fett", 8.51898),c("L_fett", 7.30356),c("M_fett", 10.33673),c("N_fett", 7.90585),c("O_fett", 8.51898),c("P_fett", 6.69043),c("Q_fett", 8.51898),c("R_fett", 7.90585),c("S_fett", 6.08812),c("T_fett", 7.30356),c("U_fett", 7.90585),c("V_fett", 7.90585),c("W_fett", 10.95),c("X_fett", 7.90585),c("Y_fett", 7.90585),c("Z_fett", 7.30356),
    c("\U00FC",5.47499), c("\U00E4",4.86172), c("\U00F6",5.47499), c("\U00DF",5.47499),
    c("\U00FC_fett",6.08812), c("\U00E4_fett",5.47499), c("\U00F6_fett",5.47499), c("\U00DF_fett",6.08812),
    c("\U00DC",7.90585), c("\U00C4",7.90585), c("\U00D6",7.90585),
    c("\U00DC_fett",7.90585), c("\U00C4_fett",7.90585), c("\U00D6_fett",8.51898),
    c(" " , 2.73749),c(" _fett" , 2.73749), c("-",3.6463), c("-_fett" , 3.6463),c("_" , 5.47499), c("__fett" , 5.47499),
    c("0",5.47499),c("1",5.47499),c("2",5.47499),c("3",5.47499),c("4",5.47499),c("5",5.47499),c("6",5.47499),c("7",5.47499),c("8",5.47499),c("9",5.47499),
    c("0_fett",5.47499),c("1_fett",5.47499),c("2_fett",5.47499),c("3_fett",5.47499),c("4_fett",5.47499),c("5_fett",5.47499),c("6_fett",5.47499),c("7_fett",5.47499),c("8_fett",5.47499),c("9_fett",5.47499),
    c("." ,2.73749) , c("._fett",2.73749),
    c("," ,2.73749) , c(",_fett",2.73749),
    c(":" ,3.04399) , c(":_fett",3.6463),
    c(";" ,3.04399) , c(";_fett",3.6463),
    c("(" ,3.6463) , c("(_fett",3.6463),
    c(")" ,3.6463) , c(")_fett",3.6463),
    c("/" ,3.04399) , c("/_fett",3.04399),
    c("$" ,5.47499) , c("$_fett",5.47499),
    c("#" ,5.47499) , c("#_fett",5.47499),
    c("@" ,10.0849) , c("@_fett",10.18341),
    c("?" ,4.861729) , c("?_fett",5.47499),
    c("%" ,9.1213) , c("%_fett",10.95),
    c("`" ,3.6463) , c("`_fett",3.6463),
    c("\\glqq" ,4.86172) , c("\\glqq_fett",5.47499),
    c("\\grqq" ,4.86172) , c("\\grqq_fett",5.47499),
    c("\\grqq{}" ,4.86172) , c("\\grqq{}_fett",5.47499),
    stringsAsFactors=FALSE)

  Latex.Letter.Length <- as.data.frame(unname(t(Latex.Letter.Length)) , stringsAsFactors=FALSE)
  names(Latex.Letter.Length) <- c("Letter" , "Length")

  cutout <- NULL

  if(grepl("\\" , word , fixed=TRUE)){
    cutout <- unlist(unname(strsplit(word , " " , fixed=TRUE)))
    cutout <- cutout[grepl("\\" , cutout , fixed=TRUE)]


    word <- gsub("\\" , "" , word, fixed=TRUE)
  }
  lett <- unname(unlist(strsplit(word , "")))
  if(bold) lett <- paste0(lett , "_fett")

  if(any(!lett %in% Latex.Letter.Length$Letter) ){
    warning( paste0( "Fuer folgende Zeichen gibt es keine Laengenangaben: ", paste0(unique(lett[!lett %in% Latex.Letter.Length$Letter]), collapse=", ") , ". Die Laenge von " , word , " in Latex wird daher unterschaetzt.") ,immediate.=TRUE)
  }
  lett <- lett[ lett %in% Latex.Letter.Length$Letter ]

  if(length(lett)==0){
    return(0)
  } else {
    len <- sum(unname(sapply( lett , function(l) as.numeric(Latex.Letter.Length$Length[Latex.Letter.Length$Letter %in% l]))))

    if( in.cm){
      len <- len/28.45274
    }
    if(bold){
      return(len*1.1) # Die obigen Angaben passen nicht ganz genau zu den tatsaechlichen Wort- und Text-Breiten im finalen Skript. Der Faktor 1.1 kompensiert die Abweichung.
    } else {
      return(len)
    }
  }
}


# Funktion, um Sonderzeichen fuer Latex-Skript zu bearbeiten
sonderzeichen.aufbereiten <- function(skript, check.tilde=FALSE){
  stopifnot(is.character(skript))

  skript <- subQuotationMarks(skript)

  sonder <- c("&","%","$","#","_")
  change.to <- c("\\&","\\%","\\$","\\#","\\_")
  if(check.tilde){
    sonder <- c(sonder, "~")
    change.to <- c(change.to, "\\textasciitilde")
  }
  names(change.to) <- sonder

  for(s in sonder){
    skript <- gsub(paste0("\\",s) , s , skript , fixed=TRUE)
    skript <- gsub(s , unname(change.to[s]) , skript, fixed=TRUE)
  }

  return(skript)
}

subQuotationMarks <- function(vec) {
  unlist(lapply(vec, function(x) {
    x_old <- x
    while(grepl('"', x)) {
    x <- sub('"', '\\\\glqq ', x = x)
    if(!grepl('"', x)) stop("Detected an uneven number of quotation marks in: ", x_old)
    x <- sub('"', '\\\\grqq{}', x = x)
  }
  x
  }))
}


