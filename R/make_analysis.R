#' Make an analysis
#'
#' This function calls \code{\link{make_model}} to create the call to \code{\link{ddf}} it also creates an environment with the data necessary to perform the call.
#'
#'
#' @author David L Miller
make_analysis <- function(this_analysis, model_definitions,
                          data_filters, data, transect){

  this_call <- make_model(this_analysis, model_definitions, data_filters,
                          transect)

  if(grepl("binned=TRUE", this_call)){
    cuts <- gsub(".*breaks=(c\\(.*?\\)),.*", "\\1", this_call)
    data <- Distance::create.bins(data, eval(paste(text=cuts)))
  }

  e <- new.env()
  e$obs_table <- data


  return(list(call = this_call,
              env  = e,
              name = as.character(this_analysis[["Name"]]),
              ID   = this_analysis[["ID"]]))
}
