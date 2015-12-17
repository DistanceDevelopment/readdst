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
    if(is.null(md$DSModel$Method)){
      return(NULL)
    }else{
      ds_methods <- md$DSModel$Method
    }

    if(any(ds_methods=="cds")){
      ds_formula <- "formula=~1"
    }else if("Factors" %in% names(md)){
      ds_formula <- make_formula(md$DSModel$Formula, md$Factors)
    }else{
      ds_formula <- paste0("formula=~", md$DSModel$Formula)
    }
    # set the function ~cds or ~mcds
    dsmethod <- ds_methods

    key <- paste0("key=\"", md$DSModel$Key, "\"")
    adj.series <- NULL
    adj.order <- NULL

  }else if(md[["Engine"]] == "CDS" | md[["Engine"]] == "MCDS"){

    key <- switch(md$Estimate$Estimator$Key,
                  HN = "hn",
                  HA = "hr",
                  UN = "unif")
    key <- paste0("key=\"", key, "\"")

    # NAP is number of adjustment parameters if zero then no
    # adjustments are to be fitted
    if(!is.null(md$Estimate$Estimator$NAP) && md$Estimate$Estimator$NAP != 0){
      adj.series <- switch(md$Estimate$Estimator$Adjust,
                           CO = "cos",
                           HE = "herm",
                           PO = "poly",
                           NULL)
      adj.series <- paste0("adj.series=\"", adj.series,"\"")
    }

    # if we use AIC for selection....
    if(md$Estimate$Pick=="AIC" &
       (!is.null(md$Estimate$Estimator$NAP) && md$Estimate$Estimator$NAP != 0)){
      # if maxterms is specified set the adjustment order to NULL
      # and do AIC selection
      if(!is.null(md$Options$Maxterms)){
        adj.order <- "adj.order=NULL"
      }else{
      # else no adjustments
        adj.series <- NULL
        adj.order <- NULL
      }
    }else if(!is.null(md$Estimate$Estimator$Order)){
      adj.order <- paste0("c(",md$Estimate$Estimator$Order,")")
    }else{
    # else no adjustments
      adj.series <- NULL
      adj.order <- NULL
    }

  }else{
    stop("Unsupported engine!")
  }

  if(md[["Engine"]] == "CDS"){
    ds_formula <- "formula=~1"
    dsmethod <- "cds"
  }else if(md[["Engine"]] == "MCDS"){
    dsmethod <- "mcds"
    factors <- md$Data$CovariateData$Field[md$Data$CovariateData$CType=="F"]
    ds_formula <- paste0(md$Data$CovariateData$Field, collapse="+")

    ds_formula <- make_formula(ds_formula, factors)
  }

  paste0("dsmodel=~", dsmethod, "(",
         str_c(key, ds_formula, adj.series, adj.order, sep=", "),")")
}
