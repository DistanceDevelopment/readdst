#' Extract data from a Distance database
#'
#' Extracts the relevant tables from the Distance for Windows database to build data that can be used with \code{mrds} or \code{Distance}.
#'
#' @param data_file the path to a \code{DistData.mdb} file.
#' @return a "flatfile" compatible \code{data.frame} containing all of the information necessary to make a stratified abundance/density estimate.
#'
#' @author David L Miller
#' @export
get_data <- function(data_file){

  # function to rename columns
  relabel <- function(old_name, new_name, data){
    if(old_name %in% names(data)){
      data[[new_name]] <- data[[old_name]]
      data[[old_name]] <- NULL
    }
    return(data)
  }


  # grab the observation table
  obs_table <- db_get(data_file, "Observation")

  # convert the distance column
  dist_names <- c("Perp.distance", "Perp.Distance", "PerpendicularDistance",
                  "Radial.distance", "Radial.Distance")
  if(any(dist_names %in% names(obs_table))){
    dist_name <- dist_names[dist_names %in% names(obs_table)]

    # if more than one of the above are in the data then throw an error
    if(length(dist_name)>1){
      stop(paste0("Non-unique distance column names: ", dist_name, collapse=""))
    }
    obs_table <- relabel(dist_name, "distance", obs_table)
  }else{
    stop("Only perpendicular and radial distances supported at the moment!")
  }

  # if group size is collected rename to size
  obs_table <- relabel("Cluster.size", "size", obs_table)
  obs_table <- relabel("ClusterSize", "size", obs_table)

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
  dat <- db_get(data_file, hier[1])
  last_name <- names(hier)[1]

  # get the ID field
  ID_field <- "ParentID"
  if("ContainerID" %in% names(dat)){
    ID_field <- "ContainerID"
  }
  dat[[ID_field]] <- NULL
  hier <- hier[-1]

  # set ID.last=ID so we can join on that
  dat$ID.last <- dat$ID

  # keep joining the tables until we have none left
  while(length(hier)>0){

    # get this table
    this_table <- db_get(data_file, hier[1])

    # merge it onto the previous data
    dat <- merge(dat, this_table,
                 by.x="ID.last", by.y=ID_field,
                 all.y=TRUE,
                 suffixes=c(paste0(".", last_name),
                            paste0(".", names(hier)[1])))

    # new ID to do the joining on
    if(is.null(dat[[paste0("ID.", names(hier)[1])]])){
      dat[[paste0("ID.", names(hier)[1])]] <- dat[["ID"]]
      dat[["ID"]] <- NULL
    }
    dat$ID.last <- dat[[paste0("ID.", names(hier)[1])]]
    # save and remove that layer
    last_name <- names(hier)[1]
    hier <- hier[-1]
  }

  # join  the observation table on at the end
  obs_table <- merge(dat, obs_table, by.y=ID_field, by.x="ID.last",
                     all=TRUE, suffixes=c(paste0(".", obs_id), ""))


  # deal with multiple visits
  if("visits" %in% names(obs_table)){
    visits <- unique(obs_table$visits)
    if(length(visits)>1) stop("Number of transects visits varies - don't know what to do!")
    if(!any("visit" %in% names(obs_table))) stop("Multiple visits but no visit column!")

    warning("Data contains transects with repeated visits, 'Sample.Label's will not match Distance for Windows")


    # what if the below will remove some repeats?
    u_lab_visit <- unique(obs_table[,c("Label","visit")])
    u_lab <- unique(obs_table$Label)
    if(nrow(u_lab_visit) != (visits*length(u_lab))){
      # for now just break!
      stop("Including repeat visits will break Sample.Labels, stopping")
    }

    # relabel the transects to include visit info
    obs_table$Label <- paste0(obs_table$Label, "-", obs_table$visit)

  }

  # clean up this data
  obs_table$ID.last <- NULL
  obs_table$ID <- NULL
  obs_table$ID.1 <- NULL
  obs_table$ID.10 <- NULL
  obs_table$ID.20 <- NULL

  # naming for dht()
  obs_table <- relabel("Label.1", "Study.Area", obs_table)
  obs_table <- relabel("Label.10", "Region.Label", obs_table)
  obs_table <- relabel("Label", "Sample.Label", obs_table)
  obs_table <- relabel("Line.length", "Effort", obs_table)
  obs_table <- relabel("Survey.effort", "Effort", obs_table)

  # rename unit conversion table
  # ....

  ## we now have a flatfile-compatible table with our data in it

  ## add an attribute for unit conversion
  unit_conv <- get_unit_conversion(data_file)
  # rename the Variables as appropriate
  unit_conv$Variable[unit_conv$Variable == "Area"] <- "Area"
  unit_conv$Variable[unit_conv$Variable == "Survey.effort"] <- "Effort"
  unit_conv$Variable[unit_conv$Variable == "Line.length"] <- "Effort"
  unit_conv$Variable[unit_conv$Variable == dist_name] <- "distance"
  # save our work
  attr(obs_table, "unit_conversion") <- unit_conv

  return(obs_table)
}
