#' Build a distance sampling analysis
#'
#' Reproduce the corresponding call to \code{ddf} to reproduce an analysis from Distance for Windows.
#'
#' @param this_analysis an analysis from Distance
#' @param model_definitions a list of model definitions
#' @param data_filters a list of data filters
#' @return a character string specifying a call to \code{ddf}
#'
#' @author David L Miller
#' @importFrom stringr str_c
make_model <- function(this_analysis, model_definitions, data_filters){

  # select the model definition and data filter for this analysis
  md <- model_definitions[[this_analysis$ModelDefinition]]
  df <- data_filters[[this_analysis$DataFilter]]


  # what is the method= argument?
  possible_methods <- c("ds",
                        "io","io.fi",
                        "trial","trial.fi",
                        "rem","rem.fi")
  method <- md[names(md)=="Method"]
  method <- paste0("method=\"",method[method %in% possible_methods],"\"")

  # make the model call
  this_call <- paste0("mrds::ddf(",
                      str_c(make_dsmodel(md),
                            make_mrmodel(md),
                            make_meta.data(df),
                            method,
                            "data=obs_table",sep=","), ")")

  return(this_call)
}
