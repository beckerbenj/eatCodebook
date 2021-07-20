
# no function? or just wrapper around import_spss? how to deal with multiple data files?

get.ds <- function(ds.files , fbshort){
  ds <- lapply(ds.files, function(ds_single) {
    out <- eatGADS::import_spss(ds_single)
    out$dat
  })
  names(ds) <- fbshort
  ds
}

