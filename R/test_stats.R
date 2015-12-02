#' Test to see if Distance for Windows and R get the same results
#'
#' Tests the results stored in the Distance for Windows project file against those generated from running the same analysis in R.
#'
#' A previous call to \code{\link{convert_project}} will return a list of projects. Only one analysis at a time can be run with \code{test_stats}. If you wish to run all the analyses in the project, you can use \code{\link{lapply}}.
#'
#' @section Status:
#' The \code{status} code is taken from Distance for Windows to indicate whether the analysis has been run yet and what the outcome was. Status codes are as follows:
#'
#' \itemize{
#'   \item{\code{0}}{ analyses has not been run in Distance for Windows yet}
#'   \item{\code{1}}{ analysis ran without errors or warnings}
#'   \item{\code{2}}{ analysis ran with warnings}
#'   \item{\code{3}}{ analysis ran with errors}
#' }
#'
#' If an analysis has a status of 0 or 3 there will usually not be any statistics attached to the analysis, so no tests will be run.
#'
#' Note that an analysis that runs with error in Distance for Windows may run fine in R and an analysis that runs fine in Distance for Windows may not work in R. In the latter case, please consider submitting this a a bug to \url{github.com/distancedevelopment/distance-bugs}.
#'
#' @note Currently only tests AIC and log-likelihood values.
#'
#' @param analysis a converted (but not run) analysis
#' @param statuses for which statuses should tests be run? See "Status", below (Defaults to \code{1}: analysis that ran without error or warning in Distance for Windows).
#'
#' @export
#' @importFrom testthat test_that context expect_equal
#' @importFrom plyr l_ply
#' @examples
#' \dontrun{
#' library(readdst)
#' # load the golftees sample project and convert it
#' project <- system.file("Golftees-example", package="readdst")
#' project <- paste0(project,"/Golftees")
#' converted <- convert_project(project)
#'
#' # run tests for analysis 1
#' test_stats(converted[[1]])
#' }
test_stats <- function(analysis, statuses=1){

  # stop if we got passed more than one analysis
  if("converted_distance_analyses" %in% class(analysis)){
    stop("You can only run one analysis at a time with test_stats, try again selecting only one analysis")
  }

  # if we have the wrong status
  if(!(analysis$status %in% statuses)){
    message(paste0("Analysis ",analysis$ID, " has status ", analysis$status,
                   " (looking for ", paste(statuses, collapse=", "), ")"))
  }else{

    # these should be args
    AIC.tol <- 1e-4
    lnl.tol <- 1e-4

    # at the moment only testing AIC and log likelihood

    # get these stats
    stats <- get_stats(analysis$project_file)
    stats <- stats[stats$ID==analysis$ID, ]

    # if there were no results return early
    if(nrow(stats)==0){
      message(paste0("No results for analysis ",analysis$ID))
      return()
    }

    # run the analysis
    run_analysis <- run_analysis(analysis)

    # do some tests!
    test_that(paste0("Analysis ",analysis$ID, " results are correct"),{
      # test AIC result
      expect_equal(run_analysis$criterion,
       stats[stats$Parameter=="AIC",]$Value,
       label = "AIC",
       tol=AIC.tol)

      # test log likelihood
      expect_equal(run_analysis$lnl,
       stats[stats$Parameter=="log-likelihood",]$Value,
       label="log-likelihood",
       tol=lnl.tol)
    })
    message("All tests were fine!")
  }

  invisible()
}
