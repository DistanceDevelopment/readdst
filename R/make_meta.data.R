#' Build the model meta.data
#'
#' From a model definition build the \code{dsmodel} part of the model.
#'
#' @param df a data filter object
#' @param transect type of transect
#' @return a character string starting with \code{meta.data=}
#'
#' @author David L Miller
#' @importFrom stringr str_c
make_meta.data <- function(df, transect){

  if(any(names(df) == "Distance_Left")){
    left_truncation <- paste0("left=",df[["Distance_Left"]])
  }else{
    left_truncation <- "left=0"
  }

  if(any(names(df) == "Distance_Intervals")){
    breaks <- paste0("breaks=c(", df[["Distance_Intervals"]], ")")
    binned <- "binned=TRUE"
  }else{
    breaks <- NULL
    binned <- NULL
  }

  if(transect == "point"){
    transect <- "point=TRUE"
  }else{
    transect <- NULL
  }

  meta <- paste0("meta.data=list(width=", df[["Distance_Width"]],",",
                 str_c(left_truncation,
                       breaks,
                       binned,
                       transect, sep=","),")")

  return(meta)
}
