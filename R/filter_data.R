#' Filter the data
#'
#' Take the "Filters" applied by DISTANCE to the data and use them to subset the data.
#'
#' @author David L Miller
#' @return a list with two elements, the data and the filter string
#' @param data the data to be filtered
#' @param data_filter a data filter to be parsed (output from \code{\link{parse_definition.data_filter}})
#' @importFrom stringr str_extract str_replace
filter_data <- function(data, data_filter){

  # get the data selections
  d_sel <- unlist(data_filter[names(data_filter)=="DataSelection"],
                  recursive=FALSE)

  filter <- ""

  if(!is.null(d_sel)){

    # get the layertype numbers
    l_type <- unlist(d_sel[grepl("LayerType", names(d_sel))])
    # get the criteria
    d_sel <- unlist(d_sel[grepl("Criterion", names(d_sel))])
    # DISTANCE uses = to mean ==, fix that
    d_sel[grepl("[^<>]=", d_sel)] <- gsub("=", "==",
                                         d_sel[grepl("[^<>]=", d_sel)])
    # DISTANCE also uses "AND" -- replace with &
    d_sel <- gsub(" AND ", " & ", d_sel)
    # DISTANCE also uses "IN" -- replace with %in%
    d_sel <- gsub(" IN \\(", " %in% c\\(", d_sel)

    # since we inserted new "&"s, resplit that
    d_sel <- strsplit(d_sel," & ")
    # replicate the layer types as needed
    l_type <- rep(l_type, unlist(lapply(d_sel,length)))
    # unlist the criteria
    d_sel <- unlist(d_sel)

    # get all the variable names
    select_vars <- stringr::str_extract(d_sel,"^[:alpha:]+")

    # if there is ambiguity over which covariate we should be
    # selecting on, use the layer data to disambiguate
    for(sv in select_vars){
      if(sum(grepl(paste0(sv, "\\.\\d+"), names(data)))>1){
        data[[sv]] <- data[[paste0(sv,".",l_type[grepl(sv, d_sel)])]]
      }
    }
    # apparrently the variable names are case insensitive ¯\_(ツ)_/¯
    # so match them up and fix the filter call
    # get all the variables in the selection
    data_names <- names(data)[match(tolower(select_vars),
                                    tolower(names(data)))]
    d_sel <- stringr::str_replace(d_sel, select_vars, data_names)


    # package that up
    d_sel <- paste(d_sel, collapse=" & ")

    # make the selection
    # yes, I know this is not ideal and I should use something other
    # than subset()
    data <- subset(data, eval(parse(text=d_sel)))

    filter <- d_sel
  }

  return(list(data=data, filter=filter))
}
