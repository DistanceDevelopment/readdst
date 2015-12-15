#' Model selection for key plus adjustment models
#'
#' Run model selection for a given analysis. The returned object is exactly as if the model has been run using \code{\link{ddf}}, so anything that can normally be done with a \code{ddf} object can be done with the return.
#'
#' Model selection is performed via AIC.
#'
#' @param analysis a converted analysis
#' @param debug display the call and name of the model before it is run, print AIC selection details
#' @return fitted \code{\link{ddf}} object
#'
#' @author David L Miller
model_selection <- function(analysis, debug=FALSE){

  if(debug){
    cat("Model name:", analysis$name,"\n")
    cat("Call:\n", analysis$call, "\n\n")
  }

  # handle AIC adjustment selection
  # code from Distance
  if(!is.null(analysis$aic.select)){

    if(debug){
      message("Starting AIC adjustment term selection")
    }

    max.order <- analysis$aic.select

    adjustment <- sub(".*adj\\.series=\"(\\w+)\".*", "\\1", analysis$call)
    key <- sub(".*key=\"(\\w+)\".*", "\\1", analysis$call)

    # this is according to p. 47 of IDS.
    if(adjustment=="poly"){
      orders <- seq(1, max.order)
    }else{
      orders <- seq(2, max.order)
    }

    # for Fourier...
    if(key=="unif" & adjustment=="cos"){
      orders <- c(1, orders)
    }

    if(adjustment=="herm" | adjustment=="poly"){
      orders <- 2*orders
      orders <- orders[orders<=2*max.order]
    }

    model_call <- analysis$call

    # for fourier model, don't run a no adjustments model
    if(key=="unif" & adjustment=="cos"){
      last.model <- list(criterion=Inf)
    }else{
      # run a model without adjustments first
      first_call <- sub(", adj\\.order=NULL", "", model_call)
      first_call <- sub(", adj\\.series=\"[a-z]+\"", "", first_call)
      last.model <- eval(parse(text=first_call), envir=analysis$env)
      if(debug){
        message(model_description(last.model))
      }
    }

    # now select adjustments
    for(i in seq_along(orders)){
      order <- paste0("c(", paste(orders[1:i], collapse=","),")")
      this_call <- sub("adj\\.order=NULL",
                       paste0("adj.order=", order), model_call)
      model <- eval(parse(text=this_call), envir=analysis$env)

      if(debug){
        message(model_description(model))
      }
      # if this models AIC is worse (bigger) than the last
      # return the last model and stop looking.
      # OR if the model failed to converge
      if((model$criterion >= last.model$criterion) | model$ds$converge!=0){
        model <- last.model
        break
      }else{
        # otherwise keep this, best model
        last.model <- model
      }
    }

    result <- model

    # print the selected model description
    if(debug){
      message("\nSelected model:\n  ", model_description(result))
    }

  }else{

    result <- eval(parse(text=analysis$call), envir=analysis$env)
  }

  return(result)
}
