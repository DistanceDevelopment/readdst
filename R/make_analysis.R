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

  # convert the flatfile to the necessary tables and store the unit conversion
  e <- list2env(unflatfile(filtered$data))
  e$units <- unit_conversion

  # extract AIC term selection status
  aic.select <- attr(this_call, "aic_select_max")
  attr(this_call, "aic_select_max") <- NULL

  this_md <- as.character(this_analysis$ModelDefinition)
  this_df <- as.character(this_analysis$DataFilter)
  # extract whether we have to fit multiple detection functions
  detection_by <- model_definitions[[this_md]]$Estimate$Detection$by
  # extract how the density/abundance estimation is to work
  estimation <- model_definitions[[this_md]]$Estimate$Density
  # get the group size model info
  group_size <- model_definitions[[this_md]]$Estimate$Cluster
  # and the group size stratification
  if(!is.null(model_definitions[[this_md]]$Estimate$Size)){
    group_size$by <- model_definitions[[this_md]]$Estimate$Size$by
  }
  # save additional truncation
  if(!is.null(data_filters[[this_df]]$Cluster$Width)){
    group_size$Width <- as.numeric(data_filters[[this_df]]$Cluster$Width)
  }

  # build the return object
  ret <- list(call         = this_call,
              aic.select   = aic.select,
              status       = this_analysis$Status,
              env          = e,
              filter       = filtered$filter,
              group_size   = group_size,
              detection_by = detection_by,
              estimation   = estimation,
              name         = as.character(this_analysis[["Name"]]),
              ID           = this_analysis[["ID"]])

  class(ret) <- "converted_distance_analysis"

  return(ret)
}
