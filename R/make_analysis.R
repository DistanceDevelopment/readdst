#' Make an analysis
#'
#' This function calls \code{\link{make_model}} to create the call to \code{\link{ddf}} it also creates an environment with the data necessary to perform the call.
#' @param this_analysis an analysis from Distance
#' @param model_definitions a list of model definitions
#' @param data_filters a list of data filters
#' @param data the data to use with the model (see \code{\link{get_data}} and \code{\link{unflatfile}})
#' @param transect the transect type
#' @return a list with the following elements: a character string specifying a call to \code{ddf}, an environment to run it in, the name of the analysis and it's ID.
#'
#'
#' @author David L Miller
make_analysis <- function(this_analysis, model_definitions,
                          data_filters, data, transect){

  # get conversion table
  unit_conversion <- attr(data, "unit_conversion")

  # make the model call
  this_call <- make_model(this_analysis, model_definitions, data_filters,
                          transect, data=data)

  # deal with binning
  if(grepl("binned=TRUE", this_call)){
    cuts <- gsub(".*breaks=(c\\(.*?\\)),.*", "\\1", this_call)

    # remove the cutpoints outside the truncation
    width <- as.numeric(gsub(".*width=(.*?),.*", "\\1", this_call))
    left <- as.numeric(gsub(".*left=(.*?),.*", "\\1", this_call))
    cuts <- eval(parse(text=cuts))
    cuts <- cuts[cuts >= left & cuts <= width]

    data <- create_bins(data, cuts)
  }

  # filter the data
  filtered <- filter_data(data,
                         data_filters[[as.character(this_analysis$DataFilter)]])

  # set the variable names
  # is this necessary?
  #filtered$data <- set_covar_names(filtered$data, attr(this_call,"factors"))

  # convert the flatfile to the necessary tables and store the unit conversion
  e <- list2env(unflatfile(filtered$data))
  e$units <- unit_conversion

  # extract AIC term selection status
  aic.select <- attr(this_call, "aic_select_max")
  attr(this_call, "aic_select_max") <- NULL

  ## deal with size bias regression or using group size
  #size_est <- group_size_est(filtered$data,
  #             model_definitions[[as.character(this_analysis$ModelDefinition)]])

  # build the return object
  ret <- list(call       = this_call,
              aic.select = aic.select,
              status     = this_analysis$Status,
              env        = e,
              filter     = filtered$filter,
              group_size = model_definitions[[as.character(this_analysis$ModelDefinition)]]$Estimate$Cluster,
              name       = as.character(this_analysis[["Name"]]),
              ID         = this_analysis[["ID"]])

  class(ret) <- "converted_distance_analysis"

  return(ret)
}
