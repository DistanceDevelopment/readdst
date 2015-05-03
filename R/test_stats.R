#' Test to see if Distance and R get the same result
#'
#'
#' @param analysis a converted (but not run) analysis
#' @param project a project path
#'
#' @export
#' @importFrom testthat test_that context expect_equal
#' @importFrom plyr l_ply
test_stats <- function(analysis, project){

  # these should be args
  AIC.tol <- 1e-4
  lnl.tol <- 1e-4

  run_analysis <- run_analysis(analysis)

  # at the moment only testing AIC and log likelihood

  # get these stats
  stats <- get_stats(paste0(project,".dst"))

  stats <- subset(stats, ID==analysis$ID)

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
    message("All tests okay!")
  }else{
    test_results <- test_results[!unlist(lapply(test_results, is.null))]
  }

  invisible(test_results)
}
