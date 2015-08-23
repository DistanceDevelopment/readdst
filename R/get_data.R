#' Extract data from a Distance database
#'
#' Extracts the relevant (existing) tables from the Distance database.
#'
#' Currently, this only returns the observation table, eventually it will return all tables.
#'
#' @param data_file the path to a \code{DistData.mdb} file.
#'
#' @author David L Miller
get_data <- function(data_file){

  obs_table <- Hmisc::mdb.get(data_file, "Observation")
  effort_table <- Hmisc::mdb.get(data_file, "Effort")

  # convert the distance column
  dist_names <- c("Perp.distance", "Perp.Distance", "PerpendicularDistance",
                  "Radial.distance", "Radial.Distance")
  if(any(dist_names %in% names(obs_table))){
    dist_name <- dist_names[dist_names %in% names(obs_table)]
    obs_table$distance <- obs_table[[dist_name]]
  }else{
    stop("Only perpendicular distances supported at the moment!")
  }

  # if group size is collected rename to size
  if(any(names(obs_table)=="Cluster.size")){
    obs_table$size <- obs_table$Cluster.size
  }else if(any(names(obs_table)=="ClusterSize")){
    obs_table$size <- obs_table$ClusterSize
  }
  # if observer ID is collected rename column to "Observer"
  if(any(names(obs_table)=="Observer") & all(names(obs_table)!="observer")){
    obs_table$observer <- obs_table$Observer
  }

  # if there aren't object IDs?
  if(all(names(obs_table)!="object")){
    obs_table$object <- 1:nrow(obs_table)
  }
#  obs_table <- obs_table[sort(obs_table$object),]

  # some covariates are collected at the effort level so
  # join the effort table to the observations
  #obs_table <- plyr::join(obs_table, effort_table, by="ID")
  effort_table$ParentID <- NULL
  obs_table <- merge(obs_table, effort_table, by.x="ParentID", by.y="ID")

  return(obs_table)
}
