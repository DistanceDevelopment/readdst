#' Merge results from stratified analyses
#'
#' In Distance for Windows, one can choose to estimate the detection function by stratum. In this case more than one detection function is returned when \code{\link{run_analysis}} is used to run the analysis. In order to test the statistics stored in the Distance for Windows project, one must first combine the resulting models (and their corresponding abundance and density estimates). This function performs these operations.
#'
#' @param models a \code{list} of model (\code{ddf}/\code{ds} objects)
#' @param analysis an analysis specification (to inform us on the stratification to be used.
#' @return a list including the "combined" model, summary and density/abundance estimates (\code{dht} output). Note that these are almost definitely not valid objects for their respective classes, they are only to be used to test statistics.
#' @author David L Miller
merge_results <- function(models, analysis){

  # todo list
  #ddf.gof(model, qq=FALSE)$chisquare$chi1$p
  #model_sum$average.p
  #as.numeric(model_sum$average.p.se)/model_sum$average.p
  #ddf.gof(model, qq=FALSE)$dsgof$ks$p
  #ddf.gof(model, qq=FALSE)$dsgof$CvM$p
  #as.numeric(dht$individuals$D$Estimate)[nrow(dht$individuals$D)]
  #as.numeric(dht$individuals$D$lcl)[nrow(dht$individuals$D)]
  #as.numeric(dht$individuals$D$ucl)[nrow(dht$individuals$D)]
  #as.numeric(dht$individuals$D$df)[nrow(dht$individuals$D)]
  #as.numeric(dht$individuals$N$Estimate)[nrow(dht$individuals$N)]
  #as.numeric(dht$individuals$N$lcl)[nrow(dht$individuals$N)]
  #as.numeric(dht$individuals$N$ucl)[nrow(dht$individuals$N)]
  #as.numeric(dht$individuals$N$df)[nrow(dht$individuals$N)]

  #as.numeric(dht$individuals$D$cv)[nrow(dht$individuals$D)]
  #as.numeric(dht$individuals$N$cv)[nrow(dht$individuals$N)]

  # make the first model a dummy model
  # we can slot things into this
  model <- models[[1]]

  # get the conversion between units
  convert_units <- analysis$env$units
  if(model$meta.data$point){
    # no effort for points
    convert_units <- 1/(convert_units$Conversion[convert_units=="Area"]/
                      (convert_units$Conversion[convert_units=="distance"]))
  }else{
    convert_units <- 1/(convert_units$Conversion[convert_units=="Area"]/
                      (convert_units$Conversion[convert_units=="distance"] *
                       convert_units$Conversion[convert_units=="Effort"]))
  }

  # return list
  res <- list()
  # get a summary
  res$model_sum <- summary(model)

  # make a dummy dht we can slot things into
  base_dht <- dht(model, obs.table=analysis$env$obs.table,
                  sample.table=analysis$env$sample.table,
                  region.table=analysis$env$region.table,
                  options=list(convert.units=convert_units))

  # save the region table before we start messing around...
  save_region <- analysis$env$region.table

  # initialize the values we'll be concatenating (or adding)
  base_dht$individuals$N$Estimate[nrow(base_dht$individuals$N)] <- 0
  model$par <- c()
  model$criterion <- 0
  model$lnl <- 0
  model$data <- c()

  # get the weighting for the abundance and density
  if(analysis$estimation$Weight == "None"){
    DN_weight <- rep(1, length(base_dht$individuals$summary$Area))
  }else if(analysis$estimation$Weight == "Effort"){
    DN_weight <- base_dht$individuals$summary$Effort
    # last entry is the "Total"
    DN_weight <- DN_weight/DN_weight[length(DN_weight)]
  }else if(analysis$estimation$Weight == "Area"){
    DN_weight <- base_dht$individuals$summary$Area
    # last entry is the "Total"
    DN_weight <- DN_weight/DN_weight[length(DN_weight)]
  }else{
    stop(paste("Don't know how to handle weighting:",
               analysis$estimation$density$weights,"\n"))
  }
  # give the weights names
  DN_weight <- as.list(DN_weight)
  names(DN_weight) <- base_dht$individuals$summary$Region

  # save where the total row is
  which_total <- base_dht$individuals$summary$Region=="Total"

  # iterate over the labels, building the statistics we want
  for(lab in analysis$env$region.table$Region.Label){

    # get the model and the region
    this_model <- models[[lab]]
    this_region <- save_region

    # modify the size part of the model to take into account the
    # estimated group size from any regression or mean size estimate
    if(!is.null(analysis$group_size)){
      size_est <- group_size_est(this_model$data, analysis$group_size,
                                 this_model)
      this_model$data$size <- size_est
    }

    # select only this stratum
    this_region <- this_region[this_region$Region.Label==lab, ]

    # get the estimates for this model/stratum
    this_dht <- dht(this_model, obs.table=analysis$env$obs.table,
                    sample.table=analysis$env$sample.table,
                    region.table=this_region,
                    options=list(convert.units=convert_units))

    # do the addy ups
    base_dht$individuals$N$Estimate[which_total] <-
      base_dht$individuals$N$Estimate[which_total] +
      this_dht$individuals$N$Estimate

    # for now estimate density via abundance
    #base_dht$individuals$D$Estimate[nrow(base_dht$individuals$D)] <-
    #  base_dht$individuals$D$Estimate[nrow(base_dht$individuals$D)] +
    #  DN_weight[[lab]] *
    #  this_dht$individuals$D$Estimate[nrow(this_dht$individuals$D)]

    model$criterion <- model$criterion + this_model$criterion
    model$lnl <- model$lnl + this_model$lnl

    # do the concatenations
    model$par <- c(model$par, this_model$par)
    model$data <- rbind.data.frame(model$data, this_model$data)

    # reset region.table
    analysis$env$region.table <- save_region

  }

  # calculate N from D
  #base_dht$individuals$N$Estimate[nrow(base_dht$individuals$N)] <-
  #  base_dht$individuals$D$Estimate[nrow(base_dht$individuals$D)]*
  #  base_dht$individuals$summary$Area[base_dht$individuals$summary$Region=="Total"]

  # calculate D from N
  base_dht$individuals$D$Estimate[which_total] <-
    base_dht$individuals$N$Estimate[which_total] /
    base_dht$individuals$summary$Area[which_total]

  # put the dht and model objects into the return list
  res$dht <- base_dht
  res$model <- model
  res$gof_intervals <- analysis$gof_intervals

  return(res)
}
