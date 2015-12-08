#' Make the control element of a call to \code{ddf}
#'
#' Build the \code{control} options for a \code{ddf} call.
#'
#' @param md model definition data to parse
#' @return character string describing the \code{control} \code{list}
#'
make_control <- function(md){

  # just need the estimate element
  md <- md[["Estimate"]]
  control <- "control=list("
  if(any(names(md) == "Monotone")){
    if(md[["Monotone"]] == "Strict"){
      control <- paste0(control,"mono=TRUE, mono.strict=TRUE")
    }else if(md[["Monotone"]] == "Weak"){
      control <- paste0(control,"mono=TRUE, mono.strict=FALSE")
    }
  }


  control <- paste0(control,")")

  return(control)
}
