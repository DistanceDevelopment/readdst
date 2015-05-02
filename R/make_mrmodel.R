#' Build a mrmodel call
#'
#' From a model definition build the \code{mrmodel} part of the model.
#'
#' @param md a model definition
#' @return a character string starting with "\code{mrmodel=}" or \code{NULL} if there is no \code{mrmodel} component in this model.
#'
#' @author David L Miller
make_mrmodel <- function(md){

  possible_mr_methods <- c("io","io.fi",
                           "trial","trial.fi",
                           "rem","rem.fi")


  mr_methods <- md[names(md)=="Method"]

  # if there was no mr part to the model just return that part is NULL
  if(!any(mr_methods %in% possible_mr_methods)){
    return(NULL)
  }

  # this actually only really works in mrds with glm
  mrmethod <- "glm"#mr_methods[mr_methods %in% possible_mr_methods]

  mr_formula <- paste0("~",md[["Formula"]])
  mr_link <- paste0("\"", md[["Link"]], "\"")

  paste0("mrmodel=~", mrmethod, "(formula=",mr_formula,", link=", mr_link,")")
}
