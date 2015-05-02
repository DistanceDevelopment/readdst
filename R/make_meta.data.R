#' Build the model meta.data
#'
#' From a model definition build the \code{dsmodel} part of the model.
#'
#' @param df a data filter object
#' @return a character string starting with \code{meta.data=}
#'
#' @author David L Miller
#' @importFrom stringr str_c
make_meta.data <- function(df){

  if(any(names(df) == "Left")){
    left_truncation <- paste0("left=",df[["Left"]])
  }else{
    left_truncation <- "left=0"
  }

  if(any(names(df) == "Intervals")){
    breaks <- paste0("breaks=c(", df[["Intervals"]], ")")
    binned <- "binned=TRUE"
  }else{
    breaks <- NULL
    binned <- NULL
  }

  meta <- paste0("meta.data=list(width=", df[["Width"]],",",
                 str_c(left_truncation,
                       breaks,
                       binned, sep=","),")")

  return(meta)
}
