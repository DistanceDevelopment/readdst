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
#' @importFrom mrds ddf.gof
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

    # test function
    test_it <- function(x, tol){
      # get the test statistic for this model
      model_val <- eval(parse(text=x[6]))

      # get the Distance value and convert to the same mode
      # as that from the model
      test_val <- x[3]
      mode(test_val) <- mode(model_val)

      # test!
      test <- all.equal(test_val, model_val, check.attributes = FALSE,
                        tolerance = as.numeric(x[5]))
      # form result
      return(c(test=test, mrds_val=model_val))
    }
    # apply over the possible tests in the table
    res <- t(apply(stats, 1, test_it, tol=tol))
    res_text <- res[,1]

    # make some ticks
    ticks <- rep("\U2713", nrow(stats))
    ticks[res_text!="1"] <- ""

    # format the mean relative difference column
    res_text[res_text=="1"] <- 0
    res_text <- sub("^Mean relative difference: ", "", res_text)

    # build the data.frame
    res_table <- data.frame(Statistic      = stats$Name,
                            Distance_value = as.numeric(stats$Value),
                            mrds_value     = as.numeric(res[,2]),
                            Rel_diff       = as.numeric(res_text),
                            Pass           = ticks)

    # give the result a class so it can be pretty-printed
    class(res_table) <- "distance_stats_table"
    attr(res_table, "print.digits") <- sub("^\\d+\\.*\\d*e[\\+-]", "",
                                       min(stats$Tolerance[stats$Tolerance>0]))
    return(res_table)
  }

  invisible()
}

#' Print tested statistics
#'
#' This is simply a \code{print} method to nicely ouput the results of \code{\link{test_stats}}.
#' @author David L Miller
#' @param x the result of a call to \code{\link{test_stats}}
#' @return just prints the results
#' @export
print.distance_stats_table <- function(x){
  class(x) <- NULL
  print(as.data.frame(x, stringsAsFactors=FALSE), digits=attr(x, "print.digits"))
}
