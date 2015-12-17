#' Print tested statistics
#'
#' This is simply a \code{print} method to nicely ouput the results of \code{\link{test_stats}}.
#' @author David L Miller
#' @param x the result of a call to \code{\link{test_stats}}
#' @return just prints the results
#' @export
print.distance_stats_table <- function(x, ..., digits=NULL){

  x <- as.data.frame(x)

  # do table formatting so that rows use the same notation
  # making them easier to compare
  m <- as.matrix(format(as.data.frame(t(x[,2:4])), drop0trailing = TRUE,
                        digits=digits, scientific=8))
  # transpose back and insert into the matrix to print
  x[,2:4] <- t(m)

  print(x, ...)
}
