#' Parse a Definition of a data filter
#'
#' Given a data filter "Definition", pre-processed by \code{\link{get_definitions}}, extract the useful information from it.
#'
#' @param df a definition
#' @return named list of defintions
#'
#' @section Details:
#' A definition consists either of a key=value pair or a name then key=value pairs separated by \code{\\} and terminated with \code{;}.
#'
#' Note that this function should be called for a single definition, usually using \code{\link{lapply}}.
#'
#' @author David L Miller
#' @importFrom stringr str_replace
parse_definition.data_filter <- function(df){

  # remove the trailing semicolons
  df <- gsub(";", "", df)

  ## make the text into a list that makes
  ## sense and can be parsed further
  # at each line, the first word (chars before a space) is
  # the name of the element
  element_names <- sub("^([[:alnum:]]+) /.*","\\1", df, perl=TRUE)
  # remove the element names that we extracted
  df <- stringr::str_replace(df, element_names, "")

  # split on the " \blah", returns a list
  df <- stringr::str_split(df," /")

  # function to split everything up
  make_list_e <- function(x){
    # remove empty strings
    x <- x[x!=""]
    splits <- stringr::str_split(x, "=", n=2)

    vals <- lapply(splits, function(x) x[2])
    vnames <- lapply(splits, function(x) x[1])

    vals <- as.list(vals)
    names(vals) <- vnames

    return(vals)
  }

  # apply that and give names
  df <- lapply(df, make_list_e)
  names(df) <- element_names

  return(df)
}
