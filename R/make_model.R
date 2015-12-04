#' Build a distance sampling analysis
#'
#' Reproduce the corresponding call to \code{ddf} to reproduce an analysis from Distance for Windows.
#'
#' @param this_analysis an analysis from Distance
#' @param model_definitions a list of model definitions
#' @param data_filters a list of data filters
#' @param transect the transect type
#' @param data the data
#' @return a character string specifying a call to \code{ddf}
#'
#' @author David L Miller
#' @importFrom stringr str_c
make_model <- function(this_analysis, model_definitions, data_filters,
                       transect, data){

  # select the model definition and data filter for this analysis
  md <- model_definitions[[as.character(this_analysis$ModelDefinition)]]
  df <- data_filters[[as.character(this_analysis$DataFilter)]]


  # what is the method= argument?
  if(md[["Engine"]] == "MRDS"){
    possible_methods <- c("ds",
                          "io", "io.fi",
                          "trial", "trial.fi",
                          "rem", "rem.fi")
    method <- md[names(md)=="Method"]
    method <- paste0("method=\"", method[method %in% possible_methods], "\"")
  }else{
    method <- "method=\"ds\""
  }

  # build the meta.data
  meta <- make_meta.data(df, transect, data)
  # if width was NA, then use the maximum reported distance
  meta <- sub("width=NA", paste0("width=", max(data$distance)), meta)

  # make the model call
  this_call <- paste0("mrds::ddf(",
                      str_c(make_dsmodel(md),
                            make_mrmodel(md),
                            meta,
                            make_control(md),
                            method,
                            "data=data", sep=", "), ")")

  # if AIC selection, save max number of terms
  if(("Pick" %in% names(md)) && md[["Pick"]] == "AIC"){
    # if maxterms is specified set the adjustment order to NULL
    # and do AIC selection
    if("Maxterms" %in% names(md)){
      attr(this_call, "aic_select_max") <- as.numeric(md[["Maxterms"]])
    }else{
    # else no adjustments
      attr(this_call, "aic_select_max") <- NULL
    }
  }

  attr(this_call,"factors") <- md[names(md)=="Factors"]

  return(this_call)
}
