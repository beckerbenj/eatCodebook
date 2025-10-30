####
#############################################################################
#' Infer layout column in a variable information.
#'
#' Temporary function: Infers the layout numeric code from the \code{SPSS} format of a variable in the data and the information
#' in the \code{inputForDescriptives}.
#'
#'@param varInfo \code{varInfo} object.
#'@param GADSdat \code{GADSdat} object.
#'@param inputForDescriptives Input for descriptive statistics calculation.
#'
#'@return Returns the modified variable information.
#'
#'@examples
#'#tbd
#'
#'@export
inferLayout <- function(varInfo, GADSdat, inputForDescriptives){
  UseMethod("inferLayout")
}

#'@export
inferLayout.data.frame <- function(varInfo, GADSdat, inputForDescriptives) {
  all_names <- varInfo$Var.Name
  ds_names <- eatGADS::namesGADS(GADSdat)
  only_sh_names <- setdiff(all_names, ds_names)

  for(i in seq_along(all_names)) {
    nam <- all_names[i]
    spss_format <- NULL
    if(nam %in% ds_names) spss_format <- unique(eatGADS::extractMeta(GADSdat, nam)$format)

    input <- inputForDescriptives[inputForDescriptives$varName == nam, ]
    # workaround for pseudo/fake scales: pick first row for information, set scale metric to numeric
    # (instead of ordinal as is appropriate for items)
    if(nrow(input) == 0) {
      input <- inputForDescriptives[inputForDescriptives$group == nam, ]
      input <- input[1, ]
      input[1, "scale"] <- "numeric"
    }
    input_imp <- input[["imp"]]
    input_type <- input[["type"]]
    input_scale <- input[["scale"]]

    #if(nam == "skala_fake_item") browser()
    # pooled variables early and separately
    if(nam %in% only_sh_names && !"fake_item" %in% input_type) {
      input_scale <- unique(inputForDescriptives[inputForDescriptives$group == nam, "scale"])
      stopifnot(length(input_scale) == 1)

      if(is.na(input_scale)) {
        varInfo[i, "Layout"] <- 10 ## network 'pooled' variable (new)
        next
      }
      if(input_scale == "numeric") varInfo[i, "Layout"] <- 6 ## pooled metric
      if(input_scale == "nominal") varInfo[i, "Layout"] <- 7 ## pooled categorical
      if(input_scale == "ordinal") varInfo[i, "Layout"] <- 11 ## pooled ordinal
      next
    }
    if(nam %in% only_sh_names && input_imp && input_scale == "nominal") {

      next
    }

    if(!is.null(spss_format) && is.na(spss_format)) stop("'format' information is missing in 'GADSdat' for variable ", nam, ".")
    if(!is.null(spss_format) && grepl("^F", spss_format) && is.na(input$scale)) {
      varInfo[i, "Layout"] <- 0 ## id
      next
    }
    if(!is.null(spss_format) && grepl("^A", spss_format) && is.na(input$scale)) {
      varInfo[i, "Layout"] <- 1 ## string
      next
    }

    if(!input_imp && input_type == "variable" && input_scale == "nominal") varInfo[i, "Layout"] <- 2 ## categorical
    if(!input_imp && input_type == "variable" && input_scale == "ordinal") varInfo[i, "Layout"] <- 3 ## ordinal
    if(!input_imp && input_type == "variable" && input_scale == "numeric") varInfo[i, "Layout"] <- 4 ## metric
    if(!input_imp && input_type == "scale" && input_scale == "numeric") varInfo[i, "Layout"] <- 5 ## scale
    if(!input_imp && input_type == "fake_item" && input_scale == "numeric") varInfo[i, "Layout"] <- 5 ## fake/pseudo scale

    # this is slightly experimental, but in theory this should work (this variables do not appear in the codebook)
    # prior layout-column was also used for scale items (and maybe imputed variables?)
    if(varInfo[i, "in.DS.und.SH"] == "ds") varInfo[i, "Layout"] <- NA
  }

  varInfo$Layout <- eatTools::asNumericIfPossible(varInfo$Layout)
  varInfo
}

#'@export
inferLayout.list <- function(varInfo, GADSdat, inputForDescriptives) {
  if(!is.list(GADSdat)) stop("'GADSdat' is not a list but 'varInfo' is.")
  if(!is.list(inputForDescriptives)) stop("'inputForDescriptives' is not a list but 'varInfo' is.")
  if(length(varInfo) != length(GADSdat)) stop("'GADSdat' and 'inputForDescriptives' lists have different lengths.")
  if(length(varInfo) != length(inputForDescriptives)) stop("'GADSdat' and 'inputForDescriptives' lists have different lengths.")

  Map(function(single_varInfo, single_GADSdat, single_input) {
    inferLayout(single_varInfo, GADSdat = single_GADSdat, inputForDescriptives = single_input)
  }, single_varInfo = varInfo, single_GADSdat = GADSdat, single_input = inputForDescriptives)
}
