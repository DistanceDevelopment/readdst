#' Converted distance analyses table
#'
#' Prints a table of the analyses that have been converted and their status from Distance for Windows.
#'
#' @param x converted distance analyses
#' @param ... unused additional args for S3 compatibility
#'
#' @export
#' @importFrom readr read_delim
print.converted_distance_analyses <- function(x, ...){

  tab <- data.frame(ID          = unlist(lapply(x, function(x) x$ID)),
                    Name        = names(x),
                    Status_code = unlist(lapply(x, function(x) x$status)))


  rownames(tab) <- NULL

  # look up status codes
  # https://github.com/distancedevelopment/readdst/wiki/distance-status-codes
  status_tab <- read_delim(
    ' Code | Status
#    ----- | -----------------
       0   | Not Run
       1   | Ran OK
       2   | Ran with warnings
       3   | Ran with errors
       4   | Survey completed
       5   | Queued to run
       6   | Run initializing
       7   | Running
       8   | Reading results
       9   | Resetting',
  delim='|', comment="#")
  tab <- merge(tab, status_tab, by.x="Status_code", by.y="Code")
  tab$Status_code <-NULL

  print(tab)
}
