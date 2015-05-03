#' Extract saved statistics for analyses
#'
#' At the moment only extracts the AIC and likelihood
#'
#' @param project_file path to project file
#'
#' @author David L Miller
#' @importFrom Hmisc mdb.get
get_stats <- function(project_file){

  # get the stats
  stats <- mdb.get(project_file, "AnalysisResultsStats")

  # get the analyses that were run
  analyses <- Hmisc::mdb.get(project_file, "Analyses")


  # from: Distance60/Utilities/Classes/Enumerations.cls
  # 0: "Not Run"
  # 1: "Ran OK"
  # 2: "Ran with warnings"
  # 3: "Ran with errors"
  # 4: "Survey completed"
  # 5: "Queued to run"
  # 6: "Run initializing"
  # 7: "Running"
  # 8: "Reading results"
  # 9: "Resetting"

  # let's just do 1 here, maybe allow others later

  analyses <- subset(analyses, Status==1)

  # some documentation on magic numbers
  #https://github.com/DistanceDevelopment/distance-for-windows/blob/0a20284c906c26cebe582cd9ac5fba0fbc89fc2a/Distance60/Analysis%20Engines/Shared%20Stuff/NEngineInterfaceUtilities/mrds.support.r#L25
  # AIC 2020
  # LNL 2090

  ##abundance of individuals
  #cat('4030',dht$individuals$N$Estimate[n.ests],'\n')
  #cat('4031',dht$individuals$N$cv[n.ests],'\n')
  #cat('4032',dht$individuals$N$lcl[n.ests],'\n')
  #cat('4033',dht$individuals$N$ucl[n.ests],'\n')
  #cat('4034',dht$individuals$N$df[n.ests],'\n')

  # grab the results
  results <- subset(stats, ID %in% analyses$ID & Parameter %in% c(2020, 2090))

  results$Parameter[results$Parameter==2020] <- "AIC"
  results$Parameter[results$Parameter==2090] <- "log-likelihood"

  return(results)
}
