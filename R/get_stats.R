#' Extract saved statistics for analyses
#'
#' At the moment only extracts the AIC and likelihood
#'
#' @section Details:
#' Codes used to determine the meanings of statistics are given at \url{https://github.com/DistanceDevelopment/readdst/wiki/distance-results-codes}.
#'
#' @param project_file path to project file
#' @param stats_table a \code{data.frame} containing possible statistics
#'
#' @author David L Miller
get_stats <- function(project_file, stats_table){

  # get the stats
  stats <- db_get(project_file, "AnalysisResultsStats")

  # get the analyses that were run
  analyses <- db_get(project_file, "Analyses")

  # only look for stats for analyses with codes:
  # 1 "Ran OK"
  # 2 "Ran with warnings"
  # 3 "Ran with errors"
  # more documentation on numbers:
  #  https://github.com/DistanceDevelopment/readdst/wiki/distance-status-codes
  analyses <- analyses[analyses$Status %in% 1:3, ]

  # some documentation on magic numbers
  # https://github.com/DistanceDevelopment/distance-for-windows/blob/0a20284c906c26cebe582cd9ac5fba0fbc89fc2a/Distance60/Analysis%20Engines/Shared%20Stuff/NEngineInterfaceUtilities/mrds.support.r#L25

  ##abundance of individuals
  #cat('4030',dht$individuals$N$Estimate[n.ests],'\n')
  #cat('4031',dht$individuals$N$cv[n.ests],'\n')
  #cat('4032',dht$individuals$N$lcl[n.ests],'\n')
  #cat('4033',dht$individuals$N$ucl[n.ests],'\n')
  #cat('4034',dht$individuals$N$df[n.ests],'\n')

  # get the magic numbers we want to extract from the database
  magic_numbers <- stats_table$Code

  # grab the results
  results <- stats[stats$ID %in% analyses$ID &
                   stats$Parameter %in% magic_numbers, ]

  results <- merge(results, stats_table, by.x="Parameter", by.y="Code")

  #results$Parameter[results$Parameter==2020] <- "AIC"
  #results$Parameter[results$Parameter==2090] <- "log-likelihood"

  return(results)
}
