#' Build a dsmodel call
#'
#' From a model definition build the \code{dsmodel} part of the model.
#'
#' @param md a model definition
#' @return a character string starting with "\code{dsmodel=}" or \code{NULL} if no \code{dsmodel} component in this model
#'
#' @author David L Miller
make_dsmodel <- function(md){

  ds_methods <- md[names(md)=="Method"]
  # if there was no ds part to the model just return that part is NULL
  if(!any(ds_methods %in% c("cds", "mcds"))){
    return(NULL)
  }


  if(any(ds_methods=="cds")){
    formula <- "~1"
  }else{
    stop("Argh!")
  }

  dsmethod <- ds_methods[ds_methods %in% c("cds","mcds")]

  paste0("dsmodel=~", dsmethod, "(key=\"", md[["Key"]],"\")")
}
