#' Convert a Distance for Windows project
#'
#' Summary here
#'
#' @section Details:
#' At the moment only line or point transects are supported.
#' @param project a path to a project
#'
#' @importFrom plyr dlply "." llply
#' @export
convert_project <- function(project){

  # project file
  project_file <- paste0(project, ".dst")
  # data file
  data_file <- paste0(project, ".dat/DistData.mdb")


  # get the project settings and check that this is a Distance 6+ project
  project_settings <- Hmisc::mdb.get(project_file,"ProjectSettings")
  Distance_version <- as.numeric(as.character(
                                   subset(project_settings,
                                          Key=="DistanceVersion")$Setting))
  if(Distance_version < 6){
    stop("readdst only works with Distance 6.0 and newer projects!")
  }


  # extract the analyses table
  analyses <- Hmisc::mdb.get(project_file, "Analyses")

  # extract the model definitions
  model_definitions <- get_definitions(project_file, "ModelDefinitions")

  # extract data filters
  data_filters <- get_definitions(project_file, "DataFilters")

  # parse the model definitions and data filters
  model_definitions <- lapply(model_definitions, parse_definition.model)
  data_filters <- lapply(data_filters, parse_definition.data_filter)

  # get the data
  obs_table <- get_data(data_file)

  # what kind of distances to we have?
  data_names <- mdb.get(data_file, TRUE)
  if("Point transect" %in% data_names){
    transect <- "point"
  }else{
    transect <- "line"
  }

  # batch convert analyses
  R_analyses <- dlply(analyses, .(ID), make_analysis, model_definitions,
                      data_filters, data=obs_table, transect=transect)

  names(R_analyses) <- as.character(analyses$Name)

  # save the file names of the project for later
  R_analyses <- llply(R_analyses, function(x, project, project_file){
                                    x$project <- project
                                    x$project_file <- project_file
                                    return(x)},
                      project=project, project_file=project_file)

  class(R_analyses) <- "converted_distance_analyses"

  return(R_analyses)
}
