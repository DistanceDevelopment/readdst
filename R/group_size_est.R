#' Estimate group size
#'
#' Distance for Windows includes a few different methods for accounting for group (or cluster) size (at the adundance/density estimation stage). These include using the mean group size for all observations or using a regression of size against distance or log size against distance.
#'
#' @param data the data for this analysis
#' @param group_size the \code{group_size} element of an analysis object
#' @param model a fitted model
#' @return estimated cluster sizes (\code{numeric} vector of length \code{nrow(data)}), or \code{NULL} if there were no instructions on how to estimate group/cluster size
#' @author David L Miller
#' @importFrom plyr ddply
group_size_est <- function(data, group_size, model){

### Need to also take into account SIZE p. 328

  # grab the cluster term if it exists
  if(is.null(group_size) || is.null(data$size)){
    return(NULL)
  }

  original_data_length <- nrow(data)

  # get the width and truncate
  if(!is.null(group_size$Width)){
    data <- data[data$distance <= group_size$Width, ]
  }else{
    data <- data[data$distance <= model$meta.data$width, ]
  }
  # get the test alpha

  # function to be applied over the stratification we specify
  estimate_group_size <- function(data, group_size, model){
    # if MEAN exists
    if(!is.null(group_size$Mean)){
      cluster <- mean(data$size)
    }else if(!is.null(group_size$Bias)){
      # do the size bias regression
      # from the distance manual
      # X       Regress cluster size against distance
      # GX      Regress cluster size against g(x)
      # XLOG    Regress log_e(group size) against distance
      # GXLOG   Regress log_e(s) against g(x)

      # make a data.frame
      reg_data <- data[, c("size", "distance")]
      response <- "size"
      explanatory <- "distance"
      pred_data <- data.frame(distance=0)

      # if we need to transform size...
      if(group_size$Bias %in% c("GXLOG", "XLOG")){
        reg_data$log_size <- log(reg_data$size)
        response <- "log_size"
      }
      # if we need g(x) rather than distance
      if(group_size$Bias %in% c("GX", "GXLOG")){
        reg_data$gx <- predict(model, data)[[1]]
        explanatory <- "gx"
        pred_data <- data.frame(gx=1)
      }

      # do a regression!
      size_lm <- lm(paste0(response, "~", explanatory), data=reg_data)
      # predict the cluster size at zero distance
      cluster <- predict(size_lm, pred_data)
      if(group_size$Bias %in% c("GXLOG", "XLOG")){
        cluster <- exp(cluster)
      }
    }
    return(cluster)
  }

  # what is the stratification we should use?
  if(is.null(group_size$by) || (group_size$by=="All")){
    # if there was no stratification
      cluster <- estimate_group_size(data, group_size, model)
      cluster <- rep(cluster, original_data_length)
  }else{
    # if the clustering is by sample or stratum set the grouping
    # variable for ddply appropriately
    if(group_size$by=="Sample"){
      grouping <- "Sample.Label"
    }else if(group_size$by=="Stratum"){
      grouping <- "Region.Label"
    }

    # apply the estimator to each subgroup
    cluster_ply <- ddply(data, grouping, estimate_group_size,
                         group_size=group_size, model=model)
    # join the estimates to the data and then extract that column
    # to give us the estimates per observation
    cluster_df <- merge(data, cluster_ply, by=grouping)
    cluster <- cluster_df$size
  }

  return(cluster)

}
