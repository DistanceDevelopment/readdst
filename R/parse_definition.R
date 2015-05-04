#' Parse a Definition
#'
#' Given data from a "Definition", pre-processed by \code{\link{get_definitions}}, extract the useful information from it.
#'
#' @param df a definition
#' @return named vector of defintions
#'
#' @section Details:
#' A definition consists either of a key=value pair or a name then key=value pairs separated by \code{\\} and terminated with \code{;}.
#'
#' Note that this function should be called for a single definition, usually using \code{\link{lapply}}.
#'
#' @author David L Miller
parse_definition <- function(df){

  # remove the trailing semicolons
  df <- gsub(";", "", df)


  make_prefixes <- function(x){
    if(grepl("\\w+ /((.+?=.+?))+",x)){
      prefix <- sub("(\\w+) /((.+?=.+?))+$", "\\1",x)
      rest <- sub(prefix, "",x)
      x <- unlist(lapply(rest, function(x) strsplit(x," /")[[1]]))
      x <- x[x!=""]
      x <- paste0(prefix,"_",x)
    }
    return(x)
  }

  df <- unlist(lapply(df, make_prefixes))

#  # split the lines based on the " /" separator
#  df <- unlist(lapply(df, function(x) strsplit(x," /")[[1]]))
#  df <- df[grepl("=",df)]

  vals <- unlist(lapply(strsplit(df,"="), function(x) x[2]))
  names(vals) <- unlist(lapply(strsplit(df,"="), function(x) x[1]))

  return(vals)
}
