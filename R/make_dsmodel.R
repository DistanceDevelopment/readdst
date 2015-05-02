#' Build a dsmodel call
#'
#' From a model definition build the \code{dsmodel} part of the model.
#'
#' @param md a model definition
#' @return a character string starting with \code{dsmodel=}
#'
#' @author David L Miller
make_dsmodel <- function(md){

  ds_methods <- md[names(md)=="Method"]

  if(any(ds_methods=="cds")){
    formula <- "~1"
  }else{
    stop("Argh!")
  }

  dsmethod <- ds_methods[ds_methods %in% c("cds","mcds")]

  paste0("dsmodel=~", dsmethod, "(key=\"", md[["Key"]],"\")")
}
