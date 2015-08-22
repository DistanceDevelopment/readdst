filter_data <- function(data, data_filter){

  # get the data selections
  d_sel <- unlist(data_filter[names(data_filter)=="DataSelection"],
                  recursive=FALSE)

  if(!is.null(d_sel)){

    # get the criteria
    d_sel <- unlist(d_sel[grepl("Criterion", names(d_sel))])
    # DISTANCE uses = to mean ==, fix that
    d_sel <- gsub("=", "==", d_sel[grepl("[^<>]=",d_sel)])
    # DISTANCE also uses "AND"
    d_sel <- gsub(" AND ", " & ", d_sel)

    # package that up
    d_sel <- paste(paste0("data$", d_sel), collapse=" & ")

    # make the selection
    data <- data[eval(parse(text=d_sel)),]
  }

  return(data)
}
