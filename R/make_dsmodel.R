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
  if(!any(c("cds", "mcds") %in% ds_methods)){
    return(NULL)
  }


  if(any(ds_methods=="cds")){
    ds_formula <- "formula=~1"
  }else if("Factors" %in% names(md)){
    ds_formula <- make_formula(md[["Formula"]], md[["Factors"]])
  }else{
    ds_formula <- paste0("formula=~", md[["Formula"]])
  }

  dsmethod <- ds_methods[ds_methods %in% c("cds","mcds")]

  paste0("dsmodel=~", dsmethod, "(key=\"", md[["Key"]],"\",", ds_formula,")")
}
