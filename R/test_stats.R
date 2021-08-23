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
#' @param tolerance the tolerance of the test (default 0.01)
#' @return a \code{data.frame} with five columns: \code{Statistic}, a description of the tested statistic; \code{Distance_value} the value of the statistic stored by Distance for Windows; \code{mrds_value} the value of the statistic calculated by \code{mrds}; \code{Difference} the proportional difference between the previous two columns (computed using \code{\link{all.equal}}); \code{Pass} a series of ticks, indicating that the value in the \code{Difference} column is less than \code{tolerance}.
#'
#' @export
#' @importFrom testthat test_that context expect_equal
#' @importFrom plyr l_ply
#' @importFrom mrds ddf.gof dht
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
test_stats <- function(analysis, statuses=1, tolerance=0.01){

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
    stats <- stats_table(engine=analysis$engine)

    # get these stats
    stats <- get_stats(analysis$project_file, stats)
    stats <- stats[stats$ID==analysis$ID, ]

    # if there were no results return early
    if(nrow(stats)==0){
      message(paste0("No results for analysis ", analysis$ID))
      return()
    }

    # run the analysis
    model <- run_analysis(analysis)

    # if we didn't have statified detection function fitting
    if(!any("ddf_analyses" %in% class(model))){
      # first get the conversion between units
      convert_units <- analysis$env$units
      if(model$meta.data$point){
        # no effort for points
        convert_units <- 1/(convert_units$Conversion[convert_units=="Area"]/
                          (convert_units$Conversion[convert_units=="distance"]))
      }else{
        convert_units <- 1/(convert_units$Conversion[convert_units=="Area"]/
                          (convert_units$Conversion[convert_units=="distance"] *
                           convert_units$Conversion[convert_units=="Effort"]))
      }

      ## make an environment with the objects in that we want
      e <- list()
      # model
      e$model <- model
      # its summary
      e$model_sum <- summary(model)
      # chi^2 breaks
      e$gof <- list()
      e$gof$intervals <- analysis$gof_intervals
      # dht output
      # modify the size part of the model to take into account the
      # estimated group size from any regression or mean size estimate
      if(!is.null(analysis$group_size)){
        size_est <- group_size_est(model$data, analysis$group_size, model)
        model$data$size <- size_est
      }

      # don't estimate abundance if any of the regions don't have areas
      if(all(analysis$env$region.table$Area!=0)){
        # then do the call to dht
        e$dht <- dht(model, obs.table=analysis$env$obs.table,
                     sample.table=analysis$env$sample.table,
                     region.table=analysis$env$region.table,
                     options=list(convert.units=convert_units))
      }
    }else{
      # if we had stratification for the detection function
      # then we fitted multiple detection functions, need to aggregate
      # these over the data and combine results
      e <- merge_results(model, analysis)
    }

    # if we have zero areas remove the tests where we call dht
    if(any(analysis$env$region.table$Area==0)){
      stats <- stats[!grepl("dht", as.character(stats$MRDS)), ]
    }

    # test function
    test_it <- function(x, tol, env){
      # get the test statistic for this model
      model_val <- eval(parse(text=x[6]), envir=list2env(env))

      # get the Distance value and convert to the same mode
      # as that from the model
      test_val <- x[3]
      mode(test_val) <- mode(model_val)

      # test!
      test <- all.equal(test_val, model_val, check.attributes = FALSE,
                        scale=abs(test_val))
                        #tolerance = as.numeric(x[5]), scale=test_val)
      # form result
      return(c(test=test, mrds_val=model_val))
    }
    # apply over the possible tests in the table
    res <- t(apply(stats, 1, test_it, tol=tol, env=e))
    res_text <- res[,1]

    # format the difference column
    res_text[res_text=="1"] <- 0
    res_text <- sub("^Mean relative difference: ", "", res_text)
    res_text <- sub("^Mean absolute difference: ", "", res_text)
    res_text <- sub("^Mean scaled difference: ", "", res_text)

    # make some ticks
    ticks <- rep("\U2713", nrow(stats))
    #ticks[res_text!="1"] <- ""
    ticks[as.numeric(res_text) >= tolerance] <- ""

    # build the data.frame
    res_table <- data.frame(Statistic      = stats$Name,
                            Distance_value = as.numeric(stats$Value),
                            mrds_value     = as.numeric(res[,2]),
                            Difference     = as.numeric(res_text),
                            Pass           = ticks,
                            stringsAsFactors = FALSE)

    # give the result a class so it can be pretty-printed
    class(res_table) <- c("distance_stats_table", "data.frame")
#    attr(res_table, "print.digits") <- sub("^\\d+\\.*\\d*e[\\+-]", "",
#                                       min(stats$Tolerance[stats$Tolerance>0]))
    return(res_table)
  }

  invisible()
}
