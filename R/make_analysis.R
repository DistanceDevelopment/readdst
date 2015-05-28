#' Make an analysis
#'
#' This function calls \code{\link{make_model}} to create the call to \code{\link{ddf}} it also creates an environment with the data necessary to perform the call.
#' @param this_analysis an analysis from Distance
#' @param model_definitions a list of model definitions
#' @param data_filters a list of data filters
#' @param data the data to use with the model
#' @param transect the transect type
#' @return a list with the following elements: a character string specifying a call to \code{ddf}, an environment to run it in, the name of the analysis and it's ID.
#'
#'
#' @author David L Miller
make_analysis <- function(this_analysis, model_definitions,
                          data_filters, data, transect){

  this_call <- make_model(this_analysis, model_definitions, data_filters,
                          transect)

  if(grepl("binned=TRUE", this_call)){
    cuts <- gsub(".*breaks=(c\\(.*?\\)),.*", "\\1", this_call)

    # remove the cutpoints outside the truncation
    width <- as.numeric(gsub(".*width=(.*?),.*", "\\1", this_call))
    left <- as.numeric(gsub(".*left=(.*?),.*", "\\1", this_call))
    cuts <- eval(parse(text=cuts))
    cuts <- cuts[cuts >= left & cuts <= width]

    data <- create.bins(data, cuts)
  }

  e <- new.env()
  e$obs_table <- data


  return(list(call = this_call,
              env  = e,
              name = as.character(this_analysis[["Name"]]),
              ID   = this_analysis[["ID"]]))
}
