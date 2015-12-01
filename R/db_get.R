#' Get data from the Distance project database
#'
#' This function is a wrapper around either calls to \code{\link{RODBC}} (on Windows) or \code{\link{mdb.get}} (on Unix-a-like systems). Given a database file name it will return either the contents of the table (as \code{data.frame}), if \code{table=NULL} then it will return all tables and if \code{table=TRUE} then it will return a character vector of table names.
#'
#' @note Currently not implemented on Windows systems.
#' @param file the path to the database file to access
#' @param table the table to extract (if \code{NULL} all tables are extracted, if \code{TRUE} a list of tables names are extracted)
#' @return a \code{data.frame} with the contents of a database table
#' @author David L Miller
#' @importFrom Hmisc mdb.get
#' @importFrom RODBC odbcDriverConnect sqlTables sqlQuery odbcClose
db_get <- function(file, table=NULL){

  # on unix systems use mdb.get
  if(.Platform$OS.type == "unix"){
    dat <- mdb.get(file, table)

  # on Windows
  }else{
    # this will probably consist of the following steps:
    require(RODBC)
    dsn <- paste0("Driver={Microsoft Access Driver (*.mdb)};Dbq=",
                  file, ";Uid=;Pwd=;")
    db <- odbcDriverConnect(dsn)

    if(is.null(table)){
      # get a character vector of table names
      table_names <- sqlTables(db)$TABLE_NAME
      table_names <- table_names[!grepl("MSys", table_names)]
      # make a list, one table per element
      dat <- lapply(table_names, function(x){
                      xx <- sqlQuery(db, paste0("SELECT * FROM \"", table, "\""))
                      names(xx) <- gsub(" ", ".", names(xx))
                    })
    }else if(table==TRUE){
      # get a character vector of table names
      dat <- sqlTables(db)$TABLE_NAME
      dat <- gsub(" ", ".", dat)
    }else{
      # do the query and get the table
      # go back to spaces not dots
      table <- gsub("\\.", " ", table)
      dat <- sqlQuery(db, paste0("SELECT * FROM \"", table, "\""))
      names(dat) <- gsub(" ", ".", names(dat))
    }
    odbcClose(db)

    # remove \r line endings
    dat <- apply(dat, 2, gsub, pattern="\\r", replace="")

    ## WHEN IMPLEMENTING THIS:
    ## for compatability with mdb.get:
    #  * mdb.get just pulls _whole tables_ and puts them in data.frames
    #  * setting table=NULL will import _all tables_
    #  * setting table=TRUE will give a list of tables
  }

  return(dat)
}
