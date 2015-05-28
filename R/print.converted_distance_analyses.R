#' Print a converted distance analysis
#'
#' Prints a table of the analyses that have been converted.
#'
#' @export
print.converted_distance_analyses <- function(x, ...){

  tab <- data.frame(ID = unlist(lapply(x, function(x) x$ID)),
                    Name = names(x))
  rownames(tab) <- NULL

  print(tab)
}
