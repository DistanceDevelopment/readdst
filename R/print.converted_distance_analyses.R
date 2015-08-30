#' Converted distance analyses table
#'
#' Prints a table of the analyses that have been converted.
#'
#' @param x converted distance analyses
#' @param ... unused additional args for S3 compatibility
#'
#' @export
print.converted_distance_analyses <- function(x, ...){

  tab <- data.frame(ID = unlist(lapply(x, function(x) x$ID)),
                    Name = names(x))
  rownames(tab) <- NULL

  print(tab)
}
