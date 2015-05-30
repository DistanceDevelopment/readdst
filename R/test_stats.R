#' Test to see if Distance and R get the same result
#'
#'
#' @param analysis a converted (but not run) analysis
#'
#' @export
#' @importFrom testthat test_that context expect_equal
#' @importFrom plyr l_ply
test_stats <- function(analysis){

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
    return(NULL)
  }

  # run the analysis
  run_analysis <- run_analysis(analysis)

  test_results <- list()
  test_results[[1]] <- try(expect_equal(run_analysis$criterion,
                            stats[stats$Parameter=="AIC",]$Value,
                            label = "AIC",
                            tol=AIC.tol), silent=TRUE)
  test_results[[2]] <- try(expect_equal(run_analysis$lnl,
                            stats[stats$Parameter=="log-likelihood",]$Value,
                            label="log-likelihood",
                            tol=lnl.tol),silent=TRUE)

  pretty_printer <- function(result){
    if(class(result)=="try-error"){
      message(result)
      return(result)
    }else{
      return(NULL)
    }
  }

  test_results <- l_ply(test_results, pretty_printer)


  if(is.null(test_results)){
    message(paste0("All tests okay for analysis ", analysis$ID))
  }else{
    test_results <- test_results[!unlist(lapply(test_results, is.null))]
  }

  invisible(test_results)
}
