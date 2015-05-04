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

  # convert the distance column
  if(any(names(obs_table)=="Perp.distance")){
    obs_table$distance <- obs_table$Perp.distance
  }else if(any(names(obs_table)=="Radial.distance")){
    obs_table$distance <- obs_table$Radial.distance
  }else{
    stop("Only perpendicular distances supported at the moment!")
  }


  # if group size is collected rename to size
  if(any(names(obs_table)=="Cluster.size")){
    obs_table$size <- obs_table$Cluster.size
  }

  # if there aren't object IDs?
  if(all(names(obs_table)!="object")){
    obs_table$object <- 1:nrow(obs_table)
  }

  return(obs_table)
}
