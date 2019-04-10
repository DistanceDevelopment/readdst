#' Convert a Distance for Windows project to be run in R
#'
#' Take each analysis in a Distance for Windows project and convert the model definition to an \code{mrds} model, data and data filters are also extracted and associated with the relevant models.
#'
#' @section Details:
#' Only CDS/MCDS/MRDS analyses are supported.
#'
#' Model names are as they are in Distance for Windows (so if you have nonsensical names in Distance for Windows they will be the same in R).
#'
#' @param project a path to a project (path to the \code{dst} file with "\code{.dst}" removed from the end of the path)
#' @return an object of class \code{\link{converted_distance_analyses}} (if there are analyses defined), an object of class \code{\link{converted_distance_data}} (if no analyses are present in the project)
#'
#' @importFrom plyr dlply "." llply
#' @export
#' @author David L Miller
#' @seealso converted_distance_analyses readdst-package
convert_project <- function(project){

  # deal with spaces on non-Windows platforms
  # somehow the RODBC driver deals with this fine on Windows
  if((.Platform$OS.type == "unix") & grepl("\\w +", project)){
    project <- gsub(" " , "\\ ", project, fixed=TRUE)
  }

  ## get file names to use
  # project file
  project_file <- paste0(project, ".dst")
  # data file
  data_file <- paste0(project, ".dat/DistData.mdb")


  # get the project settings and check that this is a Distance 6+ project
  project_settings <- db_get(project_file, "ProjectSettings")
  Distance_version <- as.numeric(as.character(
                                   subset(project_settings,
                                          Key=="DistanceVersion")$Setting))
  if(Distance_version < 6){
    stop("readdst only works with Distance 6.0 and newer projects!")
  }


  # extract the analyses table
  analyses <- db_get(project_file, "Analyses")

  # extract the model definitions
  model_definitions <- get_definitions(project_file, "ModelDefinitions")

  # extract data filters
  data_filters <- get_definitions(project_file, "DataFilters")

  # parse the model definitions and data filters
  model_definitions <- lapply(model_definitions, parse_definition.model)
  data_filters <- lapply(data_filters, parse_definition.data_filter)

  # what kind of survey do we have?
  data_names <- db_get(data_file, TRUE)
  if(any(grepl("Point.transect", data_names))){
    transect <- "point"
  }else if(any(grepl("Cue.transect", data_names))){
    transect <- "cue"
  }else{
    transect <- "line"
  }

  # get the data
  # this returns a flatfile-compatible data.frame
  data <- get_data(data_file)

  # if there are analyses to convert...
  if(nrow(analyses)>0){
    # batch convert analyses and return a list, one element per analysis
    R_analyses <- dlply(analyses, .(ID), make_analysis, model_definitions,
                        data_filters, data=data, transect=transect)
    # give each analysis a name
    names(R_analyses) <- as.character(analyses$Name)

    # save the file names of the project for later
    R_analyses <- llply(R_analyses, function(x, project, project_file){
                                      x$project <- project
                                      x$project_file <- project_file
                                      return(x)},
                        project=project, project_file=project_file)

    # return object is just a list of class "converted_distance_analysis"
    # make that list of class "converted_distance_analyses" so we can
    # dispatch it later.
    class(R_analyses) <- "converted_distance_analyses"

    return(R_analyses)
  }else{
  # if we just have data, return that

    # get conversion table
    unit_conversion <- attr(data, "unit_conversion")

    # apply the data filters
    filtered <- llply(data_filters, filter_data, data=data)
    ds_data <- llply(filtered, function(x) unflatfile(x$data))

    # store the units
    ds_data <- lapply(ds_data, function(x){
      x$units <- unit_conversion
      x
    })

    # keep it classy
    class(ds_data) <- "converted_distance_data"

    return(ds_data)
  }
}
