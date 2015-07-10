#' Build a dsmodel call
#'
#' From a model definition build the \code{dsmodel} part of the model.
#'
#' @param md a model definition
#' @return a character string starting with "\code{dsmodel=}" or \code{NULL} if no \code{dsmodel} component in this model
#'
#' @author David L Miller
make_dsmodel <- function(md){

  if(md[["Engine"]] == "MRDS"){
    # if there was no ds part to the model just return that part is NULL
    if(!any("DSModel_Method" %in% names(md))){
      return(NULL)
    }else{
      ds_methods <- md[["DSModel_Method"]]
    }

    if(any(ds_methods=="cds")){
      ds_formula <- "formula=~1"
    }else if("Factors" %in% names(md)){
      ds_formula <- make_formula(md[["DSModel_Formula"]], md[["Factors"]])
    }else{
      ds_formula <- paste0("formula=~", md[["DSModel_Formula"]])
    }
    # set the function ~cds or ~mcds
    dsmethod <- ds_methods

    key <- paste0("key=\"", md[["DSModel_Key"]], "\"")
    adj.series <- NULL
    adj.order <- NULL

  }else if(md[["Engine"]] == "CDS" | md[["Engine"]] == "MCDS"){

    key <- switch(md[["Estimator_Key"]],
                  HN = "hn",
                  HA = "hr",
                  UN = "unif")
    key <- paste0("key=\"", key, "\"")

    adj.series <- switch(md[["Estimator_Adjust"]],
                         CO = "cos",
                         HE = "herm",
                         PO = "poly",
                         NULL)
    adj.series <- paste0("adj.series=\"", adj.series,"\"")

    # if we use AIC for selection....
    if(md[["Pick"]]=="AIC"){
      # if maxterms is specified set the adjustment order to NULL
      # and do AIC selection
      if("Maxterms" %in% names(md)){
        adj.order <- "adj.order=NULL"
      }else{
      # else no adjustments
        adj.series <- NULL
        adj.order <- NULL
      }
    }else{
      adj.order <- paste0("adj.order=c(","2",")")
    }

  }else{
    stop("Unsupported engine!")
  }

  if(md[["Engine"]] == "CDS"){
    ds_formula <- "formula=~1"
    dsmethod <- "cds"
  }else if(md[["Engine"]] == "MCDS"){
    dsmethod <- "mcds"
    covariates <- md[names(md)=="CovariateData_Field"]
    ds_formula <- paste0("formula=~",
                         paste0(covariates, collapse="+"))
  }

  paste0("dsmodel=~", dsmethod, "(",
         str_c(key, ds_formula, adj.series, adj.order, sep=","),")")
}
