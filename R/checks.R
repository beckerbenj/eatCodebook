#### CHECK-FUNKTIONEN ####
valid.entry <- function(cols){
  bool <- any(is.na(cols))
  bool <- bool | any(is.null(cols))
  bool <- bool | any(gsub("\\s" , "" , cols) %in% "")
  bool <- bool | any(gsub("\\s" , "" , cols) %in% "-")

  return(! bool)
}


valid.entry.match <- function(cols , entry){
  bool <- all(cols %in% entry[!is.na(entry)])

  if(any(is.na(entry))){
    bool <- bool & any(is.na(bool))
  }

  return(bool)
}

check.varue <- function(fbshort,varue.info,varue.missings,skalen.info,varue.gliederung,varue.register,varue.lit,ds){
  cat(paste0("CHECK DER VARIABLENINFO, WERTEINFO, SKALENINFO UND DER LITERATURINFO FUER " , fbshort , ".\n"))
  flush.console()
  # relevante Spalten der Werteinformation
  miss.cols <- c( "Var.name" , "Wert" , "missing" , "LabelSH", "Zeilenumbruch.vor.Wert")
  # relevante Spalten der Variableninformationen
  varue.cols <- c("Var.Name" , "in.DS.und.SH" , "Layout" , "LabelSH" , "Anmerkung.Var" , "Gliederung" , "Reihenfolge" , "Titel" , "rekodiert","QuelleSH" , "Instruktionen" , "Hintergrundmodell", "HGM.Variable.erstellt.aus", "HGM.Reihenfolge", "intern.extern", "Seitenumbruch.im.Inhaltsverzeichnis" ) # Spalten der Variableninformationen
  # relevante Spalten im Literatur-Reiter
  lit.cols <- c( "Kurzangabe" , "Langangabe" , "in.Literaturverzeichnis" )
  # relevante Spalten in der Skalenuebersicht
  skalen.cols <- c( "Var.Name", "Quelle" , "Anzahl.valider.Werte" , "Items.der.Skala" )
  # relevante Spalten in Gliederungsreiter
  gliederung.cols <- c("Titel", "Ebene")

  bool.rel.cols.info <- all( varue.cols %in% names(varue.info))
  if(!bool.rel.cols.info){
    cat(paste0(" Die Spaltennamen " , paste0(varue.cols[! varue.cols %in% names(varue.info)], collapse=", ") , " existieren nicht in der Variableninfo. DIESE MUESSEN EXISTIEREN!\n"))
    flush.console()
  }

  bool.rel.cols.miss <- all( miss.cols %in% names(varue.missings))
  if(!bool.rel.cols.miss){
    cat(paste0(" Die Spaltennamen " , paste0(miss.cols[! miss.cols %in% names(varue.missings)], collapse=", ") , " existieren nicht in der Werteinfo. DIESE MUESSEN EXISTIEREN!\n"))
    flush.console()
  }

  bool.rel.cols.skalen <- all( skalen.cols %in% names(skalen.info))
  if(!bool.rel.cols.skalen){
    cat(paste0(" Die Spaltennamen " , paste0(skalen.cols[! skalen.cols %in% names(skalen.info)], collapse=", ") , " existieren nicht in der Skaleninfo. DIESE MUESSEN EXISTIEREN!\n"))
    flush.console()
  }

  bool.rel.cols.lit <- all( lit.cols %in% names(varue.lit))
  if(!bool.rel.cols.lit){
    cat(paste0(" Die Spaltennamen " , paste0(lit.cols[! lit.cols %in% names(varue.lit)], collapse=", ") , " existieren nicht in der Literaturinfo. DIESE MUESSEN EXISTIEREN!\n"))
    flush.console()
  }

  bool.rel.cols.gliederung <- all( gliederung.cols %in% names(varue.gliederung))
  if(!bool.rel.cols.gliederung){
    cat(paste0(" Die Spaltennamen " , paste0(gliederung.cols[! gliederung.cols %in% names(varue.gliederung)], collapse=", ") , " existieren nicht im Gliederungsreiter. DIESE MUESSEN EXISTIEREN!\n"))
    flush.console()
  }


  # relevante Variablen aus Variableninfo im DS?
  if(all(c("Var.Name","in.DS.und.SH") %in% names(varue.info))){
    bool.rel.var.ds <- varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja","ds")] %in% names(ds)
    if(any(!bool.rel.var.ds)){
      cat(paste0(" Die Variable/Variablen " , paste0(varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja","ds")][! bool.rel.var.ds] , collapse=", "), " aus der Variableninfo ist/sind nicht im Datensatz.\n"))
      flush.console()
    }
    bool.rel.var.ds <- all(bool.rel.var.ds)
  } else {
    bool.rel.var.ds <- FALSE
  }


  #relevante Variablen auch in Werteinfo?
  if(all(c("Var.Name","in.DS.und.SH") %in% names(varue.info)) & ("Var.name" %in% names(varue.missings))){
    bool.rel.var.missings <- sapply(varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja","ds") & varue.info$Layout %in% c(2,3,5,7)] , function(name) {
      if( name %in% gsub("\\s" , "", unname(unlist(strsplit(skalen.info$Items.der.Skala , ",")))) ){
        if(varue.info$Layout[ varue.info$Var.Name %in% skalen.info$Var.Name[ sapply(skalen.info$Var.Name , function(d) name %in% gsub("\\s" , "",unname(unlist(strsplit(skalen.info$Items.der.Skala[skalen.info$Var.Name %in% d] , ",")))))] ] %in% c(6,7) ) {
          return(TRUE)
        } else {
          return(name %in% unique(varue.missings$Var.name))
        }
      } else {
        return(name %in% unique(varue.missings$Var.name))
      }
    } )
    if(any(!bool.rel.var.missings)){
      cat(paste0(" Die Variablen " , paste0( varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja","ds") & varue.info$Layout %in% c(2,3,5,7)][! bool.rel.var.missings], collapse=", ") , " aus der Variableninfo sind nicht in der Werteinfo.\n"))
      flush.console()
    }
    bool.rel.var.missings <- all(bool.rel.var.missings)
  } else {
    bool.rel.var.missings <- FALSE
  }

  # gibt es fbshort in der Spalte "Quelle" in der Skaleninformation?
  if("Quelle" %in% names(skalen.info)){
    bool.skala.quelle <- fbshort %in% skalen.info$Quelle
    if(!bool.skala.quelle){
      cat(paste0(" Es gibt in der Spalte \"Quelle\" in der Skaleninformation keinen Eintrag mit dem Kuerzel ", fbshort , ". Fuer dieses Instrument sind daher keine Skalen definiert.\n"))
      flush.console()
    }
  } else {
    bool.skala.quelle <- FALSE
  }

  #relevante Skalen aus Skaleninfo in Werte- und Variableninfo?
  if(all(c("Var.Name","in.DS.und.SH","Layout") %in% names(varue.info)) & ("Var.name" %in% names(varue.missings)) & all(c("Var.Name", "Quelle") %in% names(skalen.info)) ){
    if(! bool.skala.quelle ){
      bool.skala.in.info <- TRUE
    } else {
      bool.skala.in.info <- all(unlist(unname(lapply(skalen.info$Var.name[skalen.info$Quelle %in% fbshort] , function(name) {
        bool1 <- name %in% varue.info$Var.Name
        if(!bool1){
          cat(paste0(" Die Skala " , name , " steht in der Skaleninformation, taucht aber nicht in der Variableninformation auf.\n"))
          flush.console()
        }

        bool2 <- name %in% varue.missings$Var.name
        if(!bool2){
          cat(paste0(" Die Skala " , name , " steht in der Skaleninformation, taucht aber nicht in der Werteinformation auf.\n"))
          flush.console()
        }
        return(bool1 & bool2)
      }))))
    }
  } else {
    bool.skala.in.info <- FALSE
  }


  # besitzen alle Variablen in Werteinfo jede Kategorie nur einmal?
  if(all(c("Var.Name","in.DS.und.SH") %in% names(varue.info)) & ("Var.name" %in% names(varue.missings)) ){
    bool.werte.unique <- all(unlist(unname(lapply(varue.missings$Var.name , function(name) {
      kat <- varue.missings$Wert[varue.missings$Var.name %in% name]
      if(length(kat)!=length(unique(kat))){
        cat(paste0(" Fuer die Variable ", name , " treten in der Werteinfo Werte mehrfach auf: ", paste0(kat[table(kat)>1] , collapse=", "), "\n"))
        flush.console()
      }

      return(length(kat)==length(unique(kat)))
    }))))
  } else {
    bool.werte.unique <- FALSE
  }



  # alle Items aus Skaleninfo in Werte- und Variableninfo?
  if( "Var.Name" %in% names(varue.info) & ("Var.name" %in% names(varue.missings)) & all(c("Var.Name" , "Items.der.Skala", "Quelle") %in% names(skalen.info))){
    if(! bool.skala.quelle){
      bool.items.in.varue <- TRUE
    } else {
      bool.items.in.varue <- sapply(skalen.info$Var.Name[skalen.info$Quelle %in% fbshort] ,  function(name){
        if( ! name %in% varue.info$Var.Name){
          cat(paste0(" Die Variable " , name , " steht in der Skaleninfo, aber nicht in der Variableninfo.\n"))
          flush.console()
          return(FALSE)
        } else if (varue.info$Layout[ varue.info$Var.Name %in% name ] %in% c(6,7) ){
          return(TRUE)
        } else {
          items <- gsub( "\\s", "", unlist( strsplit( skalen.info[ skalen.info$Quelle %in% fbshort & tolower( skalen.info$Var.Name ) %in% tolower( name ), "Items.der.Skala" ], ",", fixed = TRUE ) ) )
          if(!all( items %in% varue.info$Var.Name & items %in% varue.missings$Var.name)){
            cat(paste0(" Das Item/Die Items " , paste0(items[!(items %in% varue.info$Var.Name & items %in% varue.missings$Var.name)] , collapse=", "), " der Skala ", name , " aus der Skaleninfo sind nicht in der Variableninfo bzw. Werteinfo.\n"))
            flush.console()
          }
          return(all( items %in% varue.info$Var.Name & items %in% varue.missings$Var.name))
        }
      })
      bool.items.in.varue <- unname(all(bool.items.in.varue))
    }
  } else {
    bool.items.in.varue <- FALSE
  }

  # mindestens ein Item pro Skala auch auf ds oder ja?
  if( all(c("Var.Name","in.DS.und.SH") %in% names(varue.info)) & all(c("Var.Name" , "Items.der.Skala", "Quelle") %in% names(skalen.info))){
    if(! bool.skala.quelle){
      bool.items.in.DSSH <- TRUE
    } else {
      bool.items.in.DSSH <- sapply(skalen.info$Var.Name[skalen.info$Quelle %in% fbshort] ,  function(name){
        items <- gsub( "\\s", "", unlist( strsplit( skalen.info[ skalen.info$Quelle %in% fbshort & tolower( skalen.info$Var.Name ) %in% tolower( name ), "Items.der.Skala" ], ",", fixed = TRUE ) ) )
        if(! any(items %in% varue.info$Var.Name)){
          cat(paste0(" Kein Item (" , paste0(items , collapse=", ") , ") der Skala " , name , " aus der Skaleninfo steht in der Variableninfo.\n"))
          flush.console()
          return(FALSE)
        } else {
          if(! any(varue.info$in.DS.und.SH[varue.info$Var.Name %in% items] %in% c("ja","ds"))){
            cat(paste0(" Kein Item (" , paste0(items,collapse=", ") , ") der Skala " , name , " aus der Skaleninfo wird im Skalenhandbuch berichtet (Spalte in.DS.und.SH nicht auf ja oder ds).\n"))
            flush.console()
          }

          if(any(varue.info$in.DS.und.SH[varue.info$Var.Name %in% items] %in% "ja" )){
            cat(paste0(" Das Item/Die Items " , paste0(items[varue.info$in.DS.und.SH[varue.info$Var.Name %in% items] %in% "ja"],collapse=", ") , " der Skala " , name , " aus der Skaleninfo besitzen in der Spalten \'in DS und SH\' ein \'ja\'. Diese Items werden daher im Skalenhandbuch doppelt berichtet (einmal innerhalb der Skala, einmal separat). Falls das Item/die Items nur innerhalb der Skala vorkommen soll, dann den Eintrag/die Eintraege auf \'ds\' setzen.\n"))
            flush.console()
          }

          return(any(varue.info$in.DS.und.SH[varue.info$Var.Name %in% items] %in% "ds") & ! any(varue.info$in.DS.und.SH[varue.info$Var.Name %in% items] %in% "ja"))
        }
      })
      bool.items.in.DSSH <- unname(all(bool.items.in.DSSH))
    }
  } else {
    bool.items.in.DSSH <- FALSE
  }



  # wenn skala, haben items dann gleiche Kategorien?
  if( all(c("Var.Name","in.DS.und.SH") %in% names(varue.info)) & all(c("Var.Name" , "Items.der.Skala") %in% names(skalen.info)) & all(c("Var.name" , "Wert" , "LabelSH") %in% names(varue.missings))){
    if(!bool.skala.quelle){
      bool.items.kat <- TRUE
    } else {
      bool.items.kat <- unname(lapply(skalen.info$Var.Name[skalen.info$Quelle %in% fbshort] ,  function(name){
        items <- gsub( "\\s", "", unlist( strsplit( skalen.info[ skalen.info$Quelle %in% fbshort & tolower( skalen.info$Var.Name ) %in% tolower( name ), "Items.der.Skala" ], ",", fixed = TRUE ) ) )
        if(all(!items %in% varue.missings$Var.name)){
          return(FALSE)
        } else {
          items.kat <- lapply( items , function(d) varue.missings[ varue.missings$Var.name %in% d , c("Wert" , "LabelSH")])
          items.kat.length  <- unlist(lapply(items.kat , function(d) dim(d)[1]))
          items.kat.unique <- do.call("rbind" , items.kat)
          bool <- TRUE
          if( ! all( table(items.kat.unique$Wert) %in% length(items))){
            cat(paste0(" Die Items (" , paste0(items , collapse=", "), ") der Skala ", name , " besitzen eine unterschiedliche Anzahl an Kategorien. Der Wert/Die Werte ", paste0(names(table(items.kat.unique$Wert))[! table(items.kat.unique$Wert) %in% length(items)] , collapse=", ") ," kommt/kommen nicht bei allen Items vor.\n"))
            flush.console()
            bool <- FALSE
          }
          if( ! all( table(paste0(items.kat.unique$Wert,"::",items.kat.unique$LabelSH)) %in% length(items)) ){
            cat(paste0(" Die Items (" , paste0(items , collapse=", "), ") der Skala ", name , " besitzen verschiedene Labels fuer die Werte. Das Label/Die Labels ", paste0("\'",sub(".*::","",names(table(paste0(items.kat.unique$Wert,"::",items.kat.unique$LabelSH))))[! table(paste0(items.kat.unique$Wert,"::",items.kat.unique$LabelSH)) %in% length(items)] ,"\'", collapse=", ") ," kommt/kommen nicht bei allen Items vor oder gehoeren zu verschiedenen Werten.\n"))
            flush.console()
            bool <- FALSE
          }

          return(bool)
        }
      }))
      bool.items.kat <- unname(all(unlist(bool.items.kat)))
    }
  } else {
    bool.items.kat <- FALSE
  }



  # sind alle relevanten Spalten ausgefuellt?
  if(bool.rel.cols.info){
    bool.rel.cols.info.entry <- unname(sapply( c("Var.Name" , "in.DS.und.SH" , "Layout" , "LabelSH" , "Gliederung" , "Titel"  , "Hintergrundmodell", "intern.extern", "Seitenumbruch.im.Inhaltsverzeichnis" )  , function(d) valid.entry(varue.info[! varue.info$in.DS.und.SH %in% "nein",d])))
    if(! all(bool.rel.cols.info.entry)){
      cat(paste0(" Die Spalte(n) " , paste0(c("Var.Name" , "in.DS.und.SH" , "Layout" , "LabelSH" , "Gliederung" , "Titel"  , "Hintergrundmodell", "intern.extern", "Seitenumbruch.im.Inhaltsverzeichnis" )[!bool.rel.cols.info.entry] , collapse=", ")," aus der Variableninfo besitzen keine validen Angaben, d.h. die Zellen sind leer, \'NA\' oder enthalten ein Minus (-).\n Hinweis: Es wurden nur diejenigen Variablen ueberprueft, die in der Spalte \'in DS und SH\' ein \'ja\', \'sh\' oder \'ds\' bekommen haben.\n\n"))
      flush.console()
    }
    bool.rel.cols.info.entry <- all(bool.rel.cols.info.entry)
  } else {
    bool.rel.cols.info.entry <- FALSE
  }

  if(bool.rel.cols.miss){
    bool.rel.cols.miss.entry <- unname(sapply( c( "Var.name" , "Wert" , "missing" , "Zeilenumbruch.vor.Wert")   , function(d) valid.entry(varue.missings[varue.missings$Var.name %in% varue.info$Var.Name[! varue.info$in.DS.und.SH %in% "nein"],d])))
    if(! all(bool.rel.cols.miss.entry)){
      cat(paste0(" Die Spalte(n) " , paste0(c( "Var.name" , "Wert" , "missing" , "Zeilenumbruch.vor.Wert")[!bool.rel.cols.miss.entry] , collapse=", ")," aus der Werteinfo besitzen keine validen Angaben, d.h. die Zellen sind leer, \'NA\' oder enthalten ein Minus (-).\n Hinweis: Es wurden nur diejenigen Variablen ueberprueft, die in der Spalte \'in DS und SH\' ein \'nein\' bekommen haben.\n\n"))
      flush.console()
    }
    bool.rel.cols.miss.entry <- all(bool.rel.cols.miss.entry)
  } else {
    bool.rel.cols.miss.entry <- FALSE
  }

  if(bool.rel.cols.skalen){
    bool.rel.cols.skalen.entry <- unname(sapply( c( "Var.Name", "Items.der.Skala" )    , function(d) valid.entry(skalen.info[ skalen.info$Quelle %in% fbshort & skalen.info$Var.Name %in% varue.info$Var.Name[! varue.info$in.DS.und.SH %in% "nein"],d])))
    if(! all(bool.rel.cols.skalen.entry)){
      cat(paste0(" Die Spalte(n) " , paste0(c( "Var.Name", "Items.der.Skala" )[!bool.rel.cols.skalen.entry] , collapse=", ")," aus der Skaleninfo besitzen keine validen Angaben, d.h. die Zellen sind leer, \'NA\' oder enthalten ein Minus (-).\n Hinweis: Es wurden nur diejenigen Variablen ueberprueft, die in der Spalte \'in DS und SH\' ein \'nein\' bekommen haben.\n\n"))
      flush.console()
    }
    bool.rel.cols.skalen.entry <- all(bool.rel.cols.skalen.entry)
  } else {
    bool.rel.cols.skalen.entry <- FALSE
  }

  if(bool.rel.cols.lit){
    if(any(as.logical(toupper(varue.lit$in.Literaturverzeichnis)))){
      bool.rel.cols.lit.entry <- unname(sapply(  c( "Kurzangabe" , "Langangabe" ) , function(d) valid.entry(varue.lit[as.logical(toupper(varue.lit$in.Literaturverzeichnis)),d])))
      if(! all(bool.rel.cols.lit.entry)){
        cat(paste0(" Die Spalte(n) " , paste0(c( "Kurzangabe" , "Langangabe" )[!bool.rel.cols.lit.entry] , collapse=", ")," aus der Literaturinfo besitzt7besitzen nicht-valide Angaben, d.h. die Zellen sind leer, \'NA\' oder enthalten ein Minus (-).\n"))
        flush.console()
      }

      bool.rel.cols.lit.entry <- all(bool.rel.cols.lit.entry)
    } else {
      cat(paste0(" In der Spalte \"in.Literaturverzeichnis\" ist kein Eintrag auf \"TRUE\" gesetzt.\n"))
      flush.console()

      bool.rel.cols.lit.entry <- TRUE
    }

  } else {
    bool.rel.cols.lit.entry <- FALSE
  }

  if(bool.rel.cols.gliederung){
    bool.rel.cols.gliederung.entry <- unname(sapply(  gliederung.cols , function(d) valid.entry(varue.gliederung[,d])))
    if(! all(bool.rel.cols.gliederung.entry)){
      cat(paste0(" Die Spalte(n) " , paste0(gliederung.cols[!bool.rel.cols.gliederung.entry] , collapse=", ")," aus dem Gliederungsreiter besitzet/besitzen nicht-valide Angaben, d.h. die Zellen sind leer, \'NA\' oder enthalten ein Minus (-).\n"))
      flush.console()
    }
    bool.rel.cols.gliederung.entry <- all(bool.rel.cols.gliederung.entry)
  } else {
    cat(" Die Spalten in der Gliederung konnten nicht auf valide Eintraege ueberprueft werden, da nicht alle relevanten Spaltennamen im Gliederungsreiter existieren.\n")
    flush.console()
    bool.rel.cols.gliederung.entry <- FALSE
  }

  # Check, ob gleichklingende Variablen ("VAR_a", "VAR_b" , ...) alphabetisch/numerisch sortiert sind
  if(bool.rel.cols.info){
    g <- unique(varue.info$Gliederung[! varue.info$in.DS.und.SH %in% "nein"])
    var.prae <- list()
    for( i in g){
      var.prae[[i]] <- unique(unlist(sapply( varue.info$Var.Name[varue.info$Gliederung %in% i], function(d) {
        prae <- unlist(unname(strsplit(d , "")))
        if(any(c("." , "_") %in% prae)){
          prae <- paste0(prae[1:(max(which( prae %in% c("." , "_")))-1)] , collapse="")
        } else {
          prae <- NULL
        }
        return(prae)
      }  , USE.NAMES=FALSE) ) )
    }

    bool.order.var <- list()
    for( i in g[g %in% names(var.prae)] ){
      bool.order.var[[i]] <- sapply(var.prae[[i]], function(d) {
        if( any(grepl(paste0(d , "\\..*$"), varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"& varue.info$Gliederung %in% i])) & ! any(grepl(paste0(d , "\\..*\\_.*$"), varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"]))){
          vars <- varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein" & varue.info$Gliederung %in% i][grepl(paste0(d , "\\..*$") , varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"& varue.info$Gliederung %in% i])]
          suff <- sub(paste0(d , "\\.(.*)$"), "\\1" , vars)
        } else if( any(grepl(paste0(d , "\\_.*$"), varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"& varue.info$Gliederung %in% i]))){
          vars <- varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"& varue.info$Gliederung %in% i][grepl(paste0(d , "\\_.*$") , varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"& varue.info$Gliederung %in% i])]
          suff <- sub(paste0(d , "\\_(.*)$"), "\\1" , vars)
        } else {
          return(TRUE)
        }
        if(all(suff %in% c("r","ri"))){
          prae <- unlist(unname(strsplit(d , "")))
          if(any(c("." , "_") %in% prae)){
            d <- paste0(prae[1:(max(which( prae %in% c("." , "_")))-1)] , collapse="")
            if( any(grepl(paste0(d , "\\..*$"), varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"& varue.info$Gliederung %in% i]))){
              vars <- varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"& varue.info$Gliederung %in% i][grepl(paste0(d , "\\..*$") , varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"& varue.info$Gliederung %in% i])]
              suff <- sub(paste0(d , "\\.(.*)$"), "\\1" , vars)
            } else if( any(grepl(paste0(d , "\\_.*$"), varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"& varue.info$Gliederung %in% i]))){
              vars <- varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"& varue.info$Gliederung %in% i][grepl(paste0(d , "\\_.*$") , varue.info$Var.Name[!varue.info$in.DS.und.SH %in% "nein"& varue.info$Gliederung %in% i])]
              suff <- sub(paste0(d , "\\_(.*)$"), "\\1" , vars)
            }
            suff <- sub("\\.r$", "" , suff)
            suff <- sub("\\.ri$", "" , suff)
          } else {
            return(TRUE)
          }
        }
        if( all(suff %in% "gepoolt")) {
          return(TRUE)
        } else {
          suff <- suff[! suff %in% "gepoolt"]
          if( all(grepl("^\\d*$" , suff))){
            suff <- as.numeric(suff)
            if( ! all( suff == sort(suff)) ){
              cat(paste0(" Die Variablen " , paste0(vars , collapse=", "), " besitzen evtl. eine falsche Sortierung: Es wurde geprueft, ob das Muster \'VAR_a\', \'VAR_b\' usw. bzw. \'VAR_1\', \'VAR_2\' usw. eingehalten wurde. Dies ist hier nicht der Fall.\n"))
              flush.console()
            }
            return( all( suff == sort(suff)))
          } else if(any(grepl("^\\d*$" , suff))) {
            suff1 <- suff[grepl("\\d" , suff)]
            suff1 <- as.numeric(suff1)
            suff2 <- suff[! grepl("\\d" , suff)]
            if( ! all( suff1 == sort(suff1) & all( suff2 == sort(suff2))) ){
              cat(paste0(" Die Variablen " , paste0(vars , collapse=", "), " besitzen evtl. eine falsche Sortierung: Es wurde geprueft, ob das Muster \'VAR_a\', \'VAR_b\' usw. bzw. \'VAR_1\', \'VAR_2\' usw. eingehalten wurde. Dies ist hier nicht der Fall.\n"))
              flush.console()
            }
            return( all( suff1 == sort(suff1) & all( suff2 == sort(suff2))) )
          } else if(all(nchar(suff)>2) ) {
            return(TRUE)
          } else {
            if( ! all( suff == sort(suff)) ){
              cat(paste0(" Die Variablen " , paste0(vars , collapse=", "), " besitzen evtl. eine falsche Sortierung: Es wurde geprueft, ob das Muster \'VAR_a\', \'VAR_b\' usw. bzw. \'VAR_1\', \'VAR_2\' usw. eingehalten wurde. Dies ist hier nicht der Fall.\n"))
              flush.console()
            }
            return( all( suff == sort(suff)))
          }
        }
      } )
    }
    bool.order.var <- all( sapply( bool.order.var , function(d) all(d)))
  } else {
    bool.order.var <- TRUE
  }

  if(all(c(bool.rel.cols.info,
           bool.rel.cols.miss,
           bool.rel.cols.skalen,
           bool.rel.cols.lit,
           bool.rel.cols.gliederung,
           bool.rel.var.ds,
           bool.rel.var.missings,
           bool.skala.in.info,
           bool.werte.unique,
           bool.items.in.varue,
           bool.items.in.DSSH,
           bool.items.kat,
           bool.rel.cols.info.entry,
           bool.rel.cols.miss.entry,
           bool.rel.cols.skalen.entry,
           bool.rel.cols.lit.entry,
           bool.rel.cols.gliederung.entry))){
    cat(paste0("\n Variableninformation, Werteinformation, Skaleninformation und Literaturinformation sind ok!\n\n"))
    flush.console()
    return(TRUE)
  } else {
    cat(paste0("\n Es liegen Warnungen vor (siehe Anmerkungen)!\n\n"))
    flush.console()
    return(FALSE)
  }
}


check.data <- function(varue.info , varue.missings , ds){
  cat(paste0("CHECK DES DATENSATZES.\n"))
  flush.console()

  # zusaetzliche Variablen im Datensatz? (check mit Variableninfo)
  bool.ds.var <- names(ds) %in% varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja","ds")]
  if(! all(bool.ds.var)){
    cat(paste0(" Die Variablen ",paste0(names(ds)[!bool.ds.var] , collapse=", ")," befinden sich im Datensatz, stehen aber nicht in der Spalte \"in.DS.und.SH\" der Variableninformation auf \"ja\" oder \"ds\".\n"))
    flush.console()
  }
  bool.ds.var <- all(bool.ds.var)


  # sind numerische Variablen aus varue.info auch numerisch im DS?
  bool.ds.type.num <- sapply( names(ds[,varue.info$Var.Name[! varue.info$Layout %in% 1 & varue.info$Var.Name %in% names(ds)]]) , function(d) class(ds[,d])) %in% "numeric"
  if(! all(bool.ds.type.num)){
    cat(paste0(" Die Variablen ",paste0(names(ds)[! bool.ds.type.num] , collapse=", ")," liegen nicht als numerische Variablen im Datensatz, obwohl sie laut Variableninfo numerisch sein sollen.\n"))
    flush.console()
  }
  bool.ds.type.num <- all(bool.ds.type.num)

  # sind zeichenfolgen-Variablen aus varue.info auch Zeichenfolgen im DS?
  bool.ds.type.char <- sapply( names(ds) , function(d) {
    if(! d %in% varue.info$Var.Name){
      return(TRUE)
    } else if(varue.info$Layout[ varue.info$Var.Name %in% d]  %in% 1){
      return(class(ds[,d]) == "character")
    } else {
      return(TRUE)
    }
  } )
  if(! all(bool.ds.type.char)){
    cat(paste0(" Die Variable/Variablen ",paste0(names(ds)[! bool.ds.type.char ] , collapse=", ")," liegt/liegen nicht als Zeichenfolgen im Datensatz, obwohl sie laut Variableninfo Zeichenfolgen sein sollen.\n"))
    flush.console()
  }
  bool.ds.type.char <- all(bool.ds.type.char)


  # liegen Werte aus DS in Werteinfo vor? --> fehlen in der Werteinfo Werte?
  bool.ds.values <- lapply( unique(varue.missings$Var.name[ varue.missings$Var.name %in% varue.info$Var.Name[varue.info$Layout %in% c(2,3,7) & varue.info$in.DS.und.SH %in% c("ja","ds")] ] ) , function(name){
    if(name %in% names(ds)){
      bool <- unique(ds[!is.na(ds[,name]),name]) %in% varue.missings$Wert[varue.missings$Var.name %in% name]
      if(!all(bool)){
        cat(paste0(" Fuer die Variable ",name," liegen nicht im Datensatz Werte vor, die nicht in der Werteinfo definiert sind: ",paste0(unique(ds[,name])[!bool] , collapse=", "),"\n"))
        flush.console()
      }
      return(all(bool))
    } else {
      cat(paste0(" Die Variable " , name , " steht in der Werteinfo, taucht aber nicht im Datensatz auf.\n"))
      flush.console()
      return(TRUE)
    }
  })
  bool.ds.values <- unname(unlist(bool.ds.values))


  if(all(c(bool.ds.var,bool.ds.type.num,bool.ds.type.char ,bool.ds.values ))){
    cat(paste0("\n Datensatz ok!\n\n"))
    flush.console()
    return(TRUE)
  } else {
    cat(paste0("\n Datensatz nicht ok (siehe Anmerkungen)!\n\n"))
    flush.console()
    return(FALSE)
  }
}


check.layout <- function(fbshort , varue.info , varue.missings , skalen.info , ds , layout.cols=0:9){
  cat(paste0("CHECK DER LAYOUT-TYPEN\n\n"))
  flush.console()

  # richtige Layouttypen vergeben?
  bool.layout <- valid.entry.match(varue.info$Layout[varue.info$in.DS.und.SH %in% c("ja" ,"sh","ds")] , layout.cols)
  if(!bool.layout){
    cat(" Die Variable/n " , paste0(varue.info$Var.Name[ varue.info$in.DS.und.SH %in% c("ja" ,"sh","ds") & !varue.info$Layout %in% layout.cols] , collapse=", ") , " besiten Layouttypen (", paste0(order(unique(varue.info$Layout[ !varue.info$Layout %in% layout.cols])) , collapse=", ") , "), die nicht vorgegeben sind (",paste0(layout.cols , collapse=", "),").\n")
  }

  # wenn Zeichenfolge im DS, dann auch Zeichenfolge im Layout?
  bool.char.ds <- all(unname(sapply(names(ds) , function(name){
    if( ! name %in% varue.info$Var.Name ) {
      cat(paste0(" Die Variable " , name , " existiert im Datensatz, aber nicht in der Variableninformation.\n"))
      flush.console()
      return(FALSE)
    } else {
      if( is.character(ds[,name]) ){
        if( varue.info$Layout[ varue.info$Var.Name %in% name] %in% 1){
          return(TRUE)
        } else {
          cat(paste0(" Die Variable " , name , " liegt im Datensatz als Zeichenfolge vor, wird aber nicht als Zeichenfolge (Layouttyp=1) berichtet.\n"))
          flush.console()
          return(FALSE)
        }
      } else {
        return(TRUE)
      }
    }

  })))

  bool.char.info <- all(unname(sapply(varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja", "ds")] , function(name){
    if( ! name %in% names(ds) ) {
      cat(paste0(" Die Variable " , name , " ist laut Variableninformation im Datensatz, aber dort nicht zu finden.\n"))
      flush.console()
      return(FALSE)
    } else {
      if( varue.info$Layout[varue.info$Var.Name %in% name] ==1 ){
        if( is.character(ds[,name])){
          return(TRUE)
        } else {
          cat(paste0(" Die Variable " , name , " soll als Zeichenfolge (Layouttyp=1) berichtet werden, ist aber keine Zeichenfolge im Datensatz.\n"))
          flush.console()
          return(FALSE)
        }
      } else {
        return(TRUE)
      }
    }
  })))


  # wenn kategorial bzw. ordinal, dann auch kategorien?
  bool.kat <- all(unname(unlist(lapply( varue.info$Var.Name[varue.info$Layout %in% c(2,3) & varue.info$in.DS.und.SH %in% c("ja","ds","sh")] , function(name){
    if( any(skalen.info$Quelle %in% fbshort) & name %in% gsub("\\s" , "", unname(unlist(strsplit(skalen.info$Items.der.Skala , ","))))){
      if(varue.info$Layout[ varue.info$Var.Name %in% skalen.info$Var.Name[ sapply(skalen.info$Var.Name , function(d) name %in% gsub("\\s" , "",unname(unlist(strsplit(skalen.info$Items.der.Skala[skalen.info$Var.Name %in% d] , ",")))))] ] %in% c(6,7) ) {
        return(TRUE)
      }
    }
    if(! name %in% varue.missings$Var.name){
      cat(paste0(" Die Variable " , name , " ist als kategorial/ordinal definiert, besitzt aber keinen Eintrag in der Werteinfo.\n"))
      flush.console()
      return(FALSE)
    } else {
      bool <- any( varue.missings$missing[varue.missings$Var.name %in% name] %in% "nein")
      if(!bool){
        cat(paste0(" Die Variable " , name , " ist als kategorial/ordinal definiert, besitzt keine nicht-Missings in der Werteinfo.\n"))
        flush.console()
      }
      return( bool )
    }

  }))))

  # wenn metrisch oder Skala, gibt es trotzdem kategorien? --> bei ordinal ok
  bool.met <- all(unname(unlist(lapply( varue.info$Var.Name[varue.info$Layout %in% c(4,5) & varue.info$in.DS.und.SH %in% c("ja","ds","sh")] , function(name){
    if( any(skalen.info$Quelle %in% fbshort) & name %in% gsub("\\s" , "", unname(unlist(strsplit(skalen.info$Items.der.Skala , ","))))){
      if(varue.info$Layout[ varue.info$Var.Name %in% skalen.info$Var.Name[ sapply(skalen.info$Var.Name , function(d) name %in% gsub("\\s" , "",unname(unlist(strsplit(skalen.info$Items.der.Skala[skalen.info$Var.Name %in% d] , ",")))))] ] %in% c(6,7) ) {
        return(TRUE)
      }
    }
    if(! name %in% varue.missings$Var.name){
      cat(paste0(" Die Variable " , name , " ist als metrisch/skala definiert, besitzt aber keinen Eintrag in der Werteinfo.\n"))
      flush.console()
      return(FALSE)
    } else {
      bool <- all( varue.missings$missing[varue.missings$Var.name %in% name] %in% "ja")
      if(! bool){
        cat(paste0(" Die Variable " , name , " ist als metrisch/skala definiert, besitzt aber nicht-Missings in der Werteinfo.\n"))
        flush.console()
      }
      return( bool )
    }

  }))))

  # wenn Skala, dann auch in Skaleninfo?
  bool.skala <- all(unname(unlist(lapply( varue.info$Var.Name[varue.info$Layout %in% 5 & varue.info$in.DS.und.SH %in% c("ja","ds","sh")] , function(name){
    if(all(c("Quelle" , "Var.Name") %in% names(skalen.info))){
      if(any(skalen.info$Quelle %in% fbshort)){
        if(! name %in% skalen.info$Var.Name){
          cat(paste0(" Die Variable " , name , " ist als skala definiert, besitzt aber keinen Eintrag in der Skaleninformation.\n"))
          flush.console()
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else {
        return(TRUE)
      }
    } else {
      return(TRUE)
    }

  }))))


  # wenn geleert, dann auch nur sysmis im DS?
  if(! any(varue.info$Layout %in% 8)){
    bool.geleert <- TRUE
  } else {
    bool.geleert <- all(unname(sapply( varue.info$Var.Name[varue.info$Layout %in% 8 & varue.info$in.DS.und.SH %in% c("ja" , "ds", "sh")] , function(name){
      if( name %in% names(ds)){
        if(all( is.na(ds[,name]))){
          return(TRUE)
        } else {
          cat(paste0(" Die Variable " , name , " soll als geleerte numerische Variable berichtet werden, im Datensatz liegen allerdings nicht nur sysmis (\'NA\') vor.\n"))
          flush.console()
          return(FALSE)
        }
      } else {
        cat(paste0(" Die Variable " , name , " soll als geleerte Variable berichtet werden, existiert aber nicht im Datensatz.\n"))
        flush.console()
        return(FALSE)
      }

    })))
  }

  # wenn im DS nur sysmis, dann auch Layout %in% c(1,8)?
  bool.sysmis <- all(unname(sapply( names(ds) , function(name) {
    if( name %in% varue.info$Var.Name){
      if( all(is.na(ds[,name]))){
        if(varue.info$Layout[varue.info$Var.Name %in% name] %in% c(1,8)){
          return(TRUE)
        } else {
          cat(paste0(" Die Variable " , name , " besitzt im Datensatz nur Sysmis (\'NA\'), wird aber nicht mit Layouttyp 1 (Zeichenfolge) oder 8 (geleert numerisch) berichtet, sondern hat Layouttyp " , varue.info$Layout[varue.info$Var.Name %in% name], ".\n"))
          flush.console()
          return(FALSE)
        }
      } else {
        return(TRUE)
      }
    } else {
      cat(paste0(" Die Variable " , name , " liegt im Datensatz, aber nicht in der Varue vor.\n"))
      flush.console()
      return(FALSE)
    }
  })))



  if(all(bool.layout,
         bool.char.ds,
         bool.char.info,
         bool.kat,
         bool.met,
         bool.skala,
         bool.geleert,
         bool.sysmis)){
    cat(paste0("\n Layout-Typen ok!\n\n"))
    flush.console()
    return(TRUE)
  } else {
    cat(paste0("\n Layout-Typen nicht ok! (siehe Anmerkungen)\n\n"))
    flush.console()
    return(FALSE)
  }
}


check.gliederung <- function(varue.info , varue.gliederung){
  cat("CHECK DER GLIEDERUNG.\n\n")
  flush.console()


  # besitzen alle Variablen, die ins SH kommen einen Gliederungspunkt?
  bool.rel.var.gliederung <- grepl("\\d*\\.\\d*",varue.info$Gliederung[varue.info$in.DS.und.SH %in% c("ja","sh")]) | grepl("^\\d*$",varue.info$Gliederung[varue.info$in.DS.und.SH %in% c("ja","sh")])
  if(any(!bool.rel.var.gliederung)){
    cat(paste0(" Die Variable(n) ", paste0( varue.info$Var.Name[varue.info$in.DS.und.SH %in% c("ja","sh")][! bool.rel.var.gliederung] , collapse=", ") , " sollen ins Skalenhandbuch, besitzen aber keinen validen Gliederungspunkt (\'X.Y\' oder lediglich \'X\').\n\n"))
    flush.console()
  }

  bool.rel.var.gliederung <- all(bool.rel.var.gliederung)


  # existieren alle Gliederungspunkte aus der Variableninfo auch im Gliederungsreiter?
  bool.info.in.gliederung <- varue.info$Gliederung[varue.info$in.DS.und.SH %in% c("ja","sh")] %in% varue.gliederung$Ebene
  if(any(!bool.info.in.gliederung)){
    cat(paste0(" Die Gliederungspunkte ", paste0( sort(unique(varue.info$Gliederung[varue.info$in.DS.und.SH %in% c("ja","sh")][! bool.info.in.gliederung])) , collapse=", ") , " treten in der Variableninformation, aber nicht in der Gliederungsuebersicht auf.\n\n"))
    flush.console()
  }

  bool.info.in.gliederung <- all(bool.info.in.gliederung)

  # kommen im Gliederungsreiter doppelte Ebenen vor?
  bool.gliederung.unique <- unname(table(varue.gliederung$Ebene)) %in% 1
  if( any(!bool.gliederung.unique )){
    cat(paste0(" Der Gliederungspunkt/ Die Gliederungspunkte ", paste0(names(table(varue.gliederung$Ebene))[! bool.gliederung.unique] , collapse=", ") , " treten im Gliederungsreiter mehrfach auf.\n\n"))
    flush.console()
  }


  # sind nachgestellte Zahlen in Spalte "Ebene" mit vorangestellter Null, wenn mehr als 10 Unterkapitel?
  bool.gliederung.nachgestellt <- lapply(varue.gliederung$Ebene[grepl("^\\d$" , varue.gliederung$Ebene)] , function(e){
    uk <- varue.gliederung$Ebene[ grepl(paste0(e , "\\.\\d*$") , varue.gliederung$Ebene)]
    if(is.null(uk)) {
      cat(paste0(" Das Kapitel ", e, " besitzt keine (identifizierbaren) Unterkapitel. Falls Unterkapitel vorhanden sein sollen, muessen diese im Gliederungsreiter nach dem Muster \'X.Y\' angegeben werden.\n\n"))
      flush.console()
      return(TRUE)
    } else if (length(uk)==0){
      cat(paste0(" Das Kapitel ", e, " besitzt keine (identifizierbaren) Unterkapitel. Falls Unterkapitel vorhanden sein sollen, muessen diese im Gliederungsreiter nach dem Muster \'X.Y\' angegeben werden.\n\n"))
      flush.console()
      return(TRUE)
    } else {
      uk <- gsub(paste0(e , "\\.") , "" , uk)
      if( length(unique(unname(sapply( uk , nchar))))==1){
        return(TRUE)
      } else {
        cat(paste0(" Die Nummerierung Unterkapitel der Ebene " , e , " im Gliederungsreiter besitzen nicht gleich viele Stellen hinter dem Punkt. Das kann zu einer unerwuenschten Sortierung fuehren. Bei mehr als 10 Unterkapitel sind Angaben der Form \'X.01\', \'X.02\',... ,\'X.10\', ... empfehlenswert. Ohne die vorangestellte Null wird \'X.11\' nach \'X.1\', aber vor \'X.2\' einsortiert.\n\n"))
        flush.console()
      }
    }
  })

  bool.gliederung.nachgestellt <- all(do.call("rbind" , bool.gliederung.nachgestellt))

  if(all(c(bool.rel.var.gliederung,
           bool.info.in.gliederung,
           bool.gliederung.unique,
           bool.gliederung.nachgestellt))){
    cat(paste0(" Gliederung ok!\n\n"))
    flush.console()
  } else {
    cat(paste0(" Gliederung nicht ok! (siehe Anmerkungen)\n\n"))
    flush.console()
  }
}
