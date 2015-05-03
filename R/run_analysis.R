#' Run a converted Distance analysis
#'
#' @param analysis a converted analysis
#' @param debug display the call and name of the model before it is run
#' @return fitted \code{\link{ddf}} object
#'
#' @author David L Miller
#' @export
run_analysis <- function(analysis, debug=FALSE){

  if(debug){
    cat("Model name:", analysis$name,"\n")
    cat("Call:\n", analysis$call, "\n\n")
  }
  result <- eval(parse(text=analysis$call), env=analysis$env)

  return(result)
}
