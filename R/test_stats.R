#' Test to see if Distance and R get the same result
#'
#'
#' @param analysis a converted (but not run) analysis
#' @param statuses for which statuses should tests be run? Defaults to \code{1} but can be given values like \code{1:3} ("Run", "Run with warnings", "Run with errors", respectively).
#'
#' @export
#' @importFrom testthat test_that context expect_equal
#' @importFrom plyr l_ply
test_stats <- function(analysis, statuses=1){


  if(!(analysis$status %in% statuses)){
    message(paste0("Analysis ",analysis$ID, " has not been run before"))
  }else{

    # these should be args
    AIC.tol <- 1e-4
    lnl.tol <- 1e-4

    # at the moment only testing AIC and log likelihood

    # get these stats
    stats <- get_stats(analysis$project_file)
    stats <- subset(stats, ID==analysis$ID)

    # if there were no results return early
    if(nrow(stats)==0){
      message(paste0("No results for analysis ",analysis$ID))
      return()
    }

    # run the analysis
    run_analysis <- run_analysis(analysis)

    context(paste0("Analysis ",analysis$ID))

    dd<-try(test_that(paste0("Analysis ",analysis$ID, " results are correct"),{
      expect_equal(run_analysis$criterion,
       stats[stats$Parameter=="AIC",]$Value,
       label = "AIC",
       tol=AIC.tol)

      expect_equal(run_analysis$lnl,
       stats[stats$Parameter=="log-likelihood",]$Value,
       label="log-likelihood",
       tol=lnl.tol)
    }))
  }

  invisible()
}
