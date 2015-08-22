filter_data <- function(data, data_filter){

  # get the data selections
  d_sel <- unlist(data_filter[names(data_filter)=="DataSelection"],
                  recursive=FALSE)

  if(!is.null(d_sel)){
    d_sel <- paste(unlist(d_sel[grepl("Criterion", names(d_sel))]),
                   collapse=" & ")

    # make the selection
    data <- data[eval(parse(text=d_sel)),]
  }

  return(data)
}
