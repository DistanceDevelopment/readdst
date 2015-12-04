#' Run a converted distance sampling analysis
#'
#' Take a single converted analysis and run the model contained therein. The returned object is exactly as if the model has been run using \code{\link{ddf}}, so anything that can normally be done with a \code{ddf} object can be done with the return.
#'
#' A previous call to \code{\link{convert_project}} will return a list of projects. Only one analysis at a time can be run with \code{run_analysis}. If you wish to run all the analyses in the project, see the code below using \code{\link{lapply}}.
#'
#' If an analysis needs to select the number of adjustment terms (for key plus adjustment detection functions) by AIC, then that selection is done at this stage.
#'
#' @param analysis a converted analysis
#' @param debug display the call and name of the model before it is run
#' @return fitted \code{\link{ddf}} object
#'
#' @author David L Miller
#' @export
#' @examples
#' \dontrun{
#' library(readdst)
#'
#' # load and convert the golftees project
#' project <- system.file("Golftees-example", package="readdst")
#' project <- paste0(project,"/Golftees")
#' converted <- convert_project(project)
#'
#' # run the first analysis
#' analysis_1 <- run_analysis(converted[[1]], debug=TRUE)
#'
#' # look at the resulting model output
#' summary(analysis_1)
#'
#' # run all the analyses in a project
#' all_analyses_run <- lapply(converted, run_analysis)
#' }
run_analysis <- function(analysis, debug=FALSE){

  if("converted_distance_analyses" %in% class(analysis)){
    stop("You can only run one analysis at a time with run_analysis, try again selecting only one analysis")
  }

  if(debug){
    cat("Model name:", analysis$name,"\n")
    cat("Call:\n", analysis$call, "\n\n")
  }

  # handle AIC adjustment selection
  # code from Distance
  if(!is.null(analysis$aic.select)){

    max.order <- analysis$aic.select

    last.model<-list(criterion=Inf)

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

    this_call <- analysis$call

    for(i in seq_along(orders)){
      order <- paste("c(", orders[1:i],")", collapse=",")
      this_call <- sub("adj\\.order=NULL",
                       paste0("adj.order=", order), this_call)
      model <- eval(parse(text=this_call), envir=analysis$env)

      # if this models AIC is worse (bigger) than the last
      # return the last model and stop looking.
      if(model$criterion >= last.model$criterion){
        model <- last.model
#        message(paste0("\n\n",model$name.message," selected!"))
        break
      }else{
        # otherwise keep this, best model
        last.model <- model
      }
    }

    result <- model

  }else{

    result <- eval(parse(text=analysis$call), envir=analysis$env)
  }

  return(result)
}
