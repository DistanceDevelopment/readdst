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
#' @note Tests all available statistics.
#'
#' @param analysis a converted (but not run) analysis
#' @param statuses for which statuses should tests be run? See "Status", below (Defaults to \code{1}: analysis that ran without error or warning in Distance for Windows).
#' @return a \code{data.frame} with two columns: \code{Statistic}, a description of the tested statistic; and \code{Result} a series of ticks, indicating that values were the same (or similar enough in the case of numeric values; see \code{\link{stats_table}}).
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

#    # these should be args
#    AIC.tol <- 1e-4
#    lnl.tol <- 1e-4

    # what are the possible stats?
    stats <- stats_table()

    # get these stats
    stats <- get_stats(analysis$project_file, stats)
    stats <- stats[stats$ID==analysis$ID, ]

    # if there were no results return early
    if(nrow(stats)==0){
      message(paste0("No results for analysis ",analysis$ID))
      return()
    }

    # run the analysis
    model <- run_analysis(analysis)

#    # do some tests!
#    test_that(paste0("Analysis ",analysis$ID),{
#      # test AIC result
#      expect_equal(model$criterion,
#       stats[stats$Parameter=="AIC",]$Value,
#       label = "AIC",
#       tol=AIC.tol)
#
#      # test log likelihood
#      expect_equal(model$lnl,
#       stats[stats$Parameter=="log-likelihood",]$Value,
#       label="log-likelihood",
#       tol=lnl.tol)
#    })

    # set tolerance here
    # this should be set in stats_table() per statistic
    tol <- 1e-4

    # test function
    test_it <- function(x, tol){
      # get the test statistic for this model
      model_val <- eval(parse(text=x[4]))

      # get the Distance value and convert to the same mode
      # as that from the model
      test_val <- x[3]
      mode(test_val) <- mode(model_val)

      # test!
      all.equal(test_val, model_val, check.attributes = FALSE,
                tolerance = tol)
    }
    # apply over the possible tests in the table
    res <- apply(stats, 1, test_it, tol=tol)
    res_text <- res
    res <- res=="TRUE"

    ticks <- rep("\U2713", nrow(stats))
    ticks[!res] <- ""

    result_table <- data.frame(Statistic = stats$Description,
                               Result    = ticks)

#    message("All tests were fine!")
    return(result_table)
  }

  invisible()
}
