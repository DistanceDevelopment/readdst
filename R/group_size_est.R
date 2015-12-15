#' Estimate group size
#'
#' Distance for Windows includes a few different methods for accounting for group (or cluster) size (at the adundance/density estimation stage). These include using the mean group size for all observations or using a regression of size against distance or log size against distance.
#'
#' @param data the data for this analysis
#' @param group_size the \code{group_size} element of an analysis object
#' @param model a fitted model
#' @return a single the estimated cluster size, or \code{NULL} if there were no instructions on how to estimate group/cluster size
#' @author David L Miller
group_size_est <- function(data, group_size, model){

### Need to also take into account SIZE p. 328

  # grab the cluster term if it exists
  if(is.null(group_size) || is.null(data$size)){
    return(NULL)
  }

  # get the width and truncate
  if(!is.null(group_size$Width)){
    data <- data[data$distance <= group_size$Width, ]
  }else{
    data <- data[data$distance <= model$meta.data$width, ]
  }
  # get the test alpha
###

  # if MEAN exists
  if(!is.null(group_size$Mean)){
    cluster <- mean(data$size)
  }else if(!is.null(group_size$Bias)){
    # do the size bias regression
    # from the distance manual
    # X       Regress cluster size against distance
    # GX      Regress cluster size against g(x)
    # XLOG    Regress loge(group size) against distance
    # GXLOG   Regress loge(s) against g(x)

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
      reg_data$gx <- predict(model)[[1]]
      explanatory <- "gx"
      pred_data <- data.frame(gx=1)
    }

    # do a regression!
    size_lm <- lm(paste0(response, "~", explanatory), data=reg_data)
    # predict the cluster size at zero distance
    cluster <- predict(size_lm, pred_data)
  }

  return(cluster)

}
