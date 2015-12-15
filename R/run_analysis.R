#' Run a converted distance sampling analysis
#'
#' Take a single converted analysis and run the model contained therein.
#'
#' A previous call to \code{\link{convert_project}} will return a list of projects. Only one analysis at a time can be run with \code{run_analysis}. If you wish to run all the analyses in the project, see the code below using \code{\link{lapply}}.
#'
#' If an analysis needs to select the number of adjustment terms (for key plus adjustment detection functions) by AIC, then that selection is done at this stage.
#'
#' @param analysis a converted analysis
#' @param debug display the call and name of the model before it is run, print AIC selection details
#' @return fitted \code{\link{ddf}} object
#'
#' @author David L Miller
#' @export
#' @examples
#' \dontrun{
#' library(readdst)
#'
#' # load and convert the golftees project
#' project <- system.file("Golftees-example", package="readdst")
#' project <- paste0(project,"/Golftees")
#' converted <- convert_project(project)
#'
#' # run the first analysis
#' analysis_1 <- run_analysis(converted[[1]], debug=TRUE)
#'
#' # look at the resulting model output
#' summary(analysis_1)
#'
#' # run all the analyses in a project
#' all_analyses_run <- lapply(converted, run_analysis)
#' }
run_analysis <- function(analysis, debug=FALSE){

  if("converted_distance_analyses" %in% class(analysis)){
    stop("You can only run one analysis at a time with run_analysis, try again selecting only one analysis")
  }

  # if we need to fit a separate detection function per stratum
  if(!is.null(analysis$detection_by) && analysis$detection_by == "Stratum"){
    save_data <- analysis$env$data
    result <- list()
    for(lab in unique(save_data$Region.Label)){

      # select only this stratum
      analysis$env$data <- save_data[save_data$Region.Label==lab, ]

      result[[lab]] <- model_selection(analysis, debug=debug)
    }

    # reset region.table
    analysis$env$data <- save_data
    class(result) <- "ddf_analyses"
  }else{
    # otherwise just run the analysis as-is
    result <- model_selection(analysis, debug=debug)
  }

  return(result)
}
