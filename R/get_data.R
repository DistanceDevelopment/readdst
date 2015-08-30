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
  dist_names <- c("Perp.distance", "Perp.Distance", "PerpendicularDistance",
                  "Radial.distance", "Radial.Distance")
  if(any(dist_names %in% names(obs_table))){
    dist_name <- dist_names[dist_names %in% names(obs_table)]
    obs_table$distance <- obs_table[[dist_name]]
  }else{
    stop("Only perpendicular and radial distances supported at the moment!")
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

  # in what order should we be joining layers?
  hier <- build_layer_hierarchy(data_file)
  # pop the last entry, which was "Observation"
  obs_id <- names(hier)[length(hier)]
  hier <- hier[-length(hier)]

  # get the top layer (usually "Survey area")
  dat <- Hmisc::mdb.get(data_file, hier[1])
  dat$ParentID <- NULL
  hier <- hier[-1]

  # set ID.last=ID so we can join on that
  dat$ID.last <- dat$ID

  # keep joining the tables until we have none left
  while(length(hier)>0){
    this_table <- Hmisc::mdb.get(data_file, hier[length(hier)])
    dat <- merge(dat, this_table, by.y="ParentID", by.x="ID.last",
                 all.y=TRUE, suffixes=c(paste0(".",
                                        names(hier)[1]),""))
    hier <- hier[-1]
    # new ID to do the joining on
    dat$ID.last <- dat$ID
  }

  # join  the observation table on at the end
  obs_table <- merge(dat, obs_table, by.y="ParentID", by.x="ID.last",
               all.y=TRUE, suffixes=c(paste0(".", obs_id),""))

  return(obs_table)
}
