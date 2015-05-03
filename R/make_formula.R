#' Build a dsmodel or mrmodel formula
#'
#' Build a formula, ensuring that the correct terms are factors
#'
#' @param md_formula "Formula" data from a model definition
#' @param md_factors "Factors" data from a model definition
#' @return a character string specifying a formula, starting with "\code{formula=~}"
#'
#' @importFrom stringr str_trim
#' @author David L Miller
make_formula <- function(md_formula, md_factors){

  if(md_formula=="1"){
    return("~1")
  }

  formula_vars <- str_trim(strsplit(md_formula,"\\+")[[1]])

  factors <- strsplit(md_factors,", ")[[1]]

  which_f <- formula_vars %in% factors
  formula_vars[which_f] <- paste0("as.factor(", formula_vars[which_f],")")

  return(paste0("formula=~",paste(formula_vars, collapse="+")))
}
