#' Parse a Definition
#'
#' Given data from a "Definition", pre-processed by \code{\link{get_definitions}}, extract the useful information from it.
#'
#' @param df a definition (vector of character strings)
#' @return a \code{list} of \code{list}s
#'
#' @section Details:
#' See the "MCDS Command Language" section of the Distance manual for more information.
#'
#' Note that this function should be called for a single definition, usually using \code{\link{lapply}}.
#'
#' @author David L Miller
parse_definition.model <- function(df){

  # remove the trailing semicolons
  df <- gsub(";", "", df)

  parse_command <- function(x){
    this_section <- list()
    # other commands are more complicated and have "/command"s and
    # "\command=value"s in them
    # split up the line
    split_it <- strsplit(x, " /")[[1]]
    # get the command for this line
    this_key <- split_it[1]
    split_it <- split_it[-1]
    # get the subcommands and make them into key=value pairs
    # then put that in a sublist
    split_it <- strsplit(split_it, "=")
    cmd <- lapply(split_it, function(x) x[2])
    names(cmd) <- lapply(split_it, function(x) x[1])
    return(list(cmd=cmd, key=this_key))
  }

  # this can be handy for debugging
  #cat(paste(df, collapse="\n"),"\n")

  # storage
  vals <- list()

  # iterate over the entries of df, parsing each line
  for(i in 1:length(df)){
    # get the text for this row
    tx <- df[i]

    # get the Engine
    if(grepl("^Engine=", tx)){
      vals[["Engine"]] <- sub("^Engine=", "", tx)
      if(vals[["Engine"]] == "MRDS"){
        # for MRDS command language
        # doesn't seem to be references for this
        # AFAIK there are no "sections"
        this_section <- vals
      }
      next
    }

    if(vals[["Engine"]] != "MRDS"){
      # once we've done that, we go section-by-section
      if(tx %in% c("Options", "Estimate")){
        section <- tx
        this_section <- list()
        next
      }
      # data section is a bit special
      if(grepl("^Data", tx)){
        section <- "Data"
        this_section <- list()
        tx <- sub("Data /", "", tx)
        next
      }

      if(tx=="End"){
        vals[[section]] <- this_section
        next
      }

      # get the cluster size variable name
      if(grepl("Sizec.*", tx)){
        vals$cluster_size_var <- tx
        next
      }
    }

    # some entries are just of the form "Label=Value"
    if(grepl("^[[:alpha:]]+=[[:alnum:]]+\\.*[[:alnum:]]*$", tx)){
      tx <- strsplit(tx,"=")[[1]]
      this_section[[tx[1]]] <- tx[[2]]
      next
    }

    # deal with Factors in MRDS models
    if(grepl("^Factors=", tx)){
      tx <- sub("Factors=", "", tx)
      tx <- strsplit(tx,", ")[[1]]
      this_section[["Factors"]] <- tx
      next
    }

    # if it's density do something special
    if(grepl("^[[:alpha:]]+ by .*", tx)){
      tx <- sub(" by ([[:alpha:]]+)", " /by=\\1", tx)
    }

    # for covariate data in MCDS models the row starts are not unique!
    # This doesn't appear to be documented
    if(grepl("^CovariateData", tx)){
      tx <- parse_command(tx)$cmd
      if(is.null(this_section$CovariateData)){
        # make this element if we need to
        this_section$CovariateData <- tx
      }else{
        # add that to what we have
        covdat <- rbind.data.frame(as.data.frame(this_section$CovariateData,
                                                 stringsAsFactors=FALSE), tx)
        this_section$CovariateData <- as.list(covdat)
      }
      next
    }

    # other commands are more complicated and have "/command"s and
    # "\command=value"s in them use parse_command to deal with them
    tx <- parse_command(tx)
    this_section[[tx$key]] <- tx$cmd

  }

  # we didn't have sections if we used MRDS, so just copy this_section
  # onto vals
  if(vals[["Engine"]] == "MRDS"){
    vals <- this_section
  }

  return(vals)
}
