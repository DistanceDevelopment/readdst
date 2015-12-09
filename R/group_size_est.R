#' Estimate group size
#'
#' Distance for Windows includes a few different methods for accounting for group (or cluster) size (at the adundance/density estimation stage). These include using the mean group size for all observations or using a regression of size against distance or log size against distance.
#'
#' @param data the data for this analysis
#' @param model_definition a model definition
#' @param model a fitted model
#' @return a single the estimated cluster size, or \code{NULL} if there were no instructions on how to estimate group/cluster size
#' @author David L Miller
group_size_est <- function(data, model_definition, model){

### Need to also take into account SIZE p. 328

  # grab the cluster term if it exists
  if(is.null(model_definition$Estimate$Cluster) || is.null(data$size)){
    return(NULL)
  }
  cb <- model_definition$Estimate$Cluster

  # get the width and truncate
  if(!is.null(cb$Width)){
    data <- data[data$distance <= cb$Width, ]
  }else{
    data <- data[data$distance <= model$meta.data$width, ]
  }
  # get the test alpha
###

  # if MEAN exists
  if(!is.null(cb$Mean)){
    cluster <- mean(data$size)
  }else if(!is.null(cb$Bias)){
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

    # if we need to transform size...
    if(cb$Bias == "XLOG"){
      reg_data$log_size <- log(reg_data$size)
      response <- "log_size"
    }
    # if we need g(x) rather than distance
    if(cb$Bias == "GXLOG"){
      reg_data$gx <- predict(model)
      explanatory <- "gx"
    }

    # do a regression!
    size_lm <- lm(as.formula(paste0(response, "~", explanatory)), data=reg_data)
    # predict the cluster size at zero distance
    cluster <- predict(size_lm, data.frame(distance=0))
  }

  return(cluster)

}
