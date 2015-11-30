#' Extract definition information from tables
#'
#' Takes the "Definition" column from a table, converts it to \code{character} and puts each row in a list with names as the corresponding ID of that row.
#'
#' @param file the name of the \code{.mdb} file
#' @param table which table to extract the "Description" column from
#' @return a \code{list} of definitions, each element of which is a character vector.
#'
#' @importFrom magrittr "%>%" "%$%"
#' @author David L Miller
get_definitions <- function(file, table){
  definitions <- file %>%
                  db_get(table=table) %$%
                  as.character(Definition) %>%
                  strsplit("\n")
  names(definitions) <- file %>%
                         db_get(table=table) %$%
                         as.character(ID) %>%
                         strsplit("\n")
  return(definitions)
}
