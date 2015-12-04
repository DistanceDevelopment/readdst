#' Build the model meta.data
#'
#' From a model definition build the \code{dsmodel} part of the model.
#'
#' @param df a data filter object
#' @param transect type of transect
#' @param data the data used in the model
#' @return a character string starting with \code{meta.data=}
#'
#' @author David L Miller
#' @importFrom stringr str_c
make_meta.data <- function(df, transect, data){

  # get left truncation
  if(!is.null(df$Distance$Left)){
    left <- as.numeric(df$Distance$Left)
  }else{
    left <- 0
  }

  # get right truncation (distance)
  if(!is.null(df$Distance$Width)){
    width <- as.numeric(df$Distance$Width)
  }else{
    width <- NA
  }

  # truncation may also be specified as a percentage
  if(!is.null(df$Distance$Rtruncate)){
    width <- quantile(data$distance,
                      probs=1-as.numeric(df$Distance$Rtruncate),
                      na.rm=TRUE)
  }


  if(!is.null(df$Distance$Intervals)){
    # extract the bin cutpoints -- make a vector
    cuts <- eval(parse(text=paste0("c(",
                               paste(df$Distance$Intervals, collapse=","),
                                   ")")))
    # remove those outside the truncation
    cuts <- cuts[cuts >= left & cuts <= width]

    # make the breaks and binned arguments
    breaks <- paste0("breaks=c(", paste(cuts, collapse=","), ")")
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

  meta <- paste0("meta.data=list(width=",width,",",
                 str_c(paste0("left=",left),
                       breaks,
                       binned,
                       transect, sep=","),")")

  return(meta)
}
