#' Take a flatfile data.frame and make dht-compatible data.frames
#'
#' Given distance sampling survey data in flatfile format, convert it to the four tables required by \code{\link{dht}}.
#'
#' @section Details:
#'
#' \itemize{
#' \item{region.table } \code{data.frame} with two columns: \code{Region.Label}, label for the region; \code{Area}, area of the region. \code{region.table} has one row for each stratum. If there is no stratification then \code{region.table} has one entry with \code{Area} corresponding to the total survey area.
#' \item{sample.table } \code{data.frame} mapping the regions to the samples (i.e. transects). There are three columns: \code{Sample.Label}, label for the sample; \code{Region.Label}, label for the region that the sample belongs to.; \code{Effort}, the effort expended in that sample (e.g. transect length).
#' \item{obs.table } \code{data.frame} mapping the individual observations (objects) to regions and samples. There should be three columns: \code{object}, the observation ID; \code{Region.Label}, label for the region that the sample belongs to; \code{Sample.Label}, label for the sample.
#' \item{data } a \code{data.frame} containing at least a column called \code{distance}. NOTE! If there is a column called \code{size} in the data then it will be interpreted as group/cluster size.
#' }
#'
#' @return list of the four \code{data.frame}s described in "Details".
#' @note Based on \code{checkdata} from package \code{Distance}.
#' @author David L. Miller
#' @export
unflatfile <- function(data){

  # TODO: make this more elegant
  if("Search.time" %in% names(data)){
    data$Effort <- data$Search.time
  }

  if(all(c("Region.Label", "Area", "Sample.Label", "Effort", "object") %in%
         colnames(data))){
    ## construct region table
    region.table <- unique(data[,c("Region.Label", "Area")])
    # make sure that the region areas are consistent -- the above can
    # lead to duplicate labels if the areas are not the same for a given
    # region label
    if(nrow(region.table) != length(unique(data$Region.Label))){
      stop("Region areas are not consistent.")
    }
    rownames(region.table) <- 1:nrow(region.table)
    # drop Area column
    data <- data[,!c(colnames(data) %in% "Area")]

    ## construct sample table
    sample.table <- unique(data[,c("Sample.Label", "Region.Label", "Effort")])

    # possible that Effort is not the same for a given
    # Sample.Label+Region.Label -- this is BAD.
    if(nrow(sample.table)!=nrow(unique(sample.table[,c("Sample.Label",
                                                       "Region.Label")]))){
      stop("A sample has a non-unique \"Effort\", check data!")
    }

    rownames(sample.table) <- 1:nrow(sample.table)
    # drop Effort column
    data <- data[,!c(colnames(data) %in% "Effort")]


    ## construct obs
    obs.table <- unique(data[,c("object", "Region.Label", "Sample.Label")])
    rownames(obs.table) <- 1:nrow(obs.table)

    # drop Region and Sample label columns
    # actually don't do this because then we can't use subset= in dht
    #data <- data[,!c(colnames(data) %in% c("Region.Label","Sample.Label"))]
    rownames(data) <- 1:nrow(data)

    # remove the NA rows
    data <- data[!is.na(data$distance),]
  }else{
    stop("Data not interpretable by readdst!")
  }

  # ensure that the Region.Label fields are character
  region.table$Region.Label <- as.character(region.table$Region.Label)
  sample.table$Region.Label <- as.character(sample.table$Region.Label)
  obs.table$Region.Label <- as.character(obs.table$Region.Label)

  # nothing bad happened, yay!
  return(list(data         = data,
              region.table = region.table,
              sample.table = sample.table,
              obs.table    = obs.table))
}
