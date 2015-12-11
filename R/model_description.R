#' Make a user-readable model description string
#'
#' This takes a fitted \code{mrds} model object and returns a string that describes the detection function fitted and the fitted model's AIC.
#'
#' @author David L Miller
#' @param model a fitted model
#' @return a string describing the model
model_description <- function(model){

  ddfobj <- model$ds$aux$ddfobj
  key <- ddfobj$type
  adjustment <- ddfobj$adjustment


  key <- switch(key,
                hn="Half-normal",
                hr="Hazard-rate",
                th1="Threshold 1",
                th2="Threshold 2")

  if(is.null(key)){
    key <- "Uniform"
  }

  mod.str <- paste(key,"key")
  if(!is.null(adjustment)){
    adj.series <- switch(adjustment$series,
                         cos="cosine",
                         herm="Hermite polynomial",
                         poly="simple polynomial")
    mod.str <- paste(mod.str,"with")

    adj.order <- adjustment$order
    mod.str <- paste(mod.str, "order", paste(adj.order,collapse=","))
    mod.str <- paste(mod.str,adj.series,"adjustments")
  }

  mod.str <- paste(mod.str, "\n  AIC =", round(model$criterion,3))

  return(mod.str)
}
