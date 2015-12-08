#' Get the unit conversions for the data
#'
#' Obtain a list of conversion to SI units from the units that the measurements in a Distance for Windows project are in.
#'
#' @param data_file Distance for Windows project data file
#' @return a \code{data.frame} with columns \code{Variable}, \code{Units} and \code{Conversion}, giving the variable name, the units it is measured in and the conversation factor to SI units.
#' @author David L Miller
get_unit_conversion <- function(data_file){

  # get the DataFields table
  fields <- db_get(data_file, "DataFields")
  # just get the entries with units attached
  fields <- fields[as.character(fields$Units)!="",]
  # rename FieldName
  fields$Variable <- fields$FieldName
  fields$FieldName <- NULL
  # extract only the columns we want
  fields <- fields[,c("Variable", "Units")]

  # get the unit table
  units_t <- units_table()

  # make everything lowercase for the join
  units_t$Unit <- tolower(units_t$Unit)
  fields$Units <- tolower(fields$Units)

  # match data fields to conversion
  fields <- merge(fields, units_t, by.x="Units", by.y="Unit")

  # replace spaces with dots
  fields$Variable <- gsub(" ", ".", fields$Variable)

  # re-order
  fields <- fields[,c("Variable", "Units", "Conversion")]

  return(fields)
}
