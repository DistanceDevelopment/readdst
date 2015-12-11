#' Print a converted distance analysis
#'
#' Prints details of an analyses that has been converted.
#'
#' @param x converted distance analyses
#' @param ... unused additional args for S3 compatibility
#'
#' @export
print.converted_distance_analysis <- function(x, ...){

  cat("Model name  :", x$name, "\n")
  cat("ID          :", x$ID, "\n")
  cat("Data filter :", x$filter, "\n")

  cat("mrds call   :", x$call, "\n")

}
