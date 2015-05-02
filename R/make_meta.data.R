#' Build the model meta.data
#'
#' From a model definition build the \code{dsmodel} part of the model.
#'
#' @param df a data filter object
#' @return a character string starting with \code{meta.data=}
#'
#' @author David L Miller
make_meta.data <- function(md){

  meta <- paste0("meta.data=list(width=",df[["Width"]],")")

  return(meta)
}
