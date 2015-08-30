#' Set column names in data to be as in formulae
#'
#'
#'
set_covar_names <- function(data, covnames){

  dn <- names(data)
  dn_norm <- tolower(dn)

  # normalise the covariate names
  covnames <- unlist(strsplit(covnames, ","))
  cov_norm <- tolower(covnames)
  cov_norm <- sub("factor", "", cov_norm)

  # replace the names
  names(data)[match(cov_norm, dn_norm)] <- covnames

  for(cv in covnames){
    data[[cv]] <- as.factor(data[[cv]])
  }

  return(data)
}
