#' Generate table of possible statistics to test
#'
#' To use \code{\link{get_stats}} we need a set of statistics to test. We also require their codes (to look up in the Distance for Windows database) and their equivalent values in \code{mrds} (or how to calculate those values). This function provides such a table.
#'
#' @section Details:
#' Data for this table (numeric code and descriptions) is from the \code{DistIni.mdb} which is shipped with Distance for Windows. See also \url{https://github.com/distancedevelopment/readdst/wiki/distance-results-codes}.
#'
#' @section Additional notes:
#' Note that the Cramer-von Mises p-value as recorded in Distance for Windows is only recorded to the nearest 0.1.
#'
#' @return a \code{data.frame} with statistics Distance for Windows collects that have equivalents in \code{mrds}. The \code{data.frame} has three columns: \code{Code}, the numeric code for the statistic (as used in the Distance for Windows database); \code{Name}, the short name for this statistic; \code{MRDS}, the operation required to obtain the equivalent statistic in \code{mrds}; \code{Description}, a short description of the statistic.
#' @author David L Miller
#' @importFrom readr read_delim
stats_table <- function(){

  # here is a lookup table
  # this trick thanks to Noam Ross
  stat_tab <- read_delim(
    ' Code | Name            | Tolerance | MRDS                                                 | Description
#    ----- | --------------- | --------- | ---------------------------------------------------- | ------------
        1  | NULL            |           | NULL                                                 | Number of sampler points
      100  | NULL            |           | NULL                                                 | Mean coverage probability
     1000  | NULL            |           | NULL                                                 | minimum AIC, within survey and data filter
     1001  | NULL            |           | NULL                                                 | minimum AICc, within survey and data filter
     1002  | NULL            |           | NULL                                                 | minimum BIC, within survey and data filter
     1003  | NULL            |           | NULL                                                 | minimum LogL, within survey and data filter
      101  | NULL            |           | NULL                                                 | Minimum coverage probability
     1010  | n               | 0         | nrow(model$data)                                     | Number of observations
      102  | NULL            |           | NULL                                                 | Maximum coverage probability
     1020  | NULL            |           | NULL                                                 | Number of samples
      103  | NULL            |           | NULL                                                 | Standard deviation of coverage probability
     1030  | NULL            |           | NULL                                                 | Total survey effort
     1040  | NULL            |           | NULL                                                 | Encounter rate
     1041  | NULL            |           | NULL                                                 | Encounter rate coeff. of var.
     1042  | NULL            |           | NULL                                                 | Encounter rate lower conf. limit
     1043  | NULL            |           | NULL                                                 | Encounter rate upper conf. limit
     1044  | NULL            |           | NULL                                                 | Encounter rate degrees of freedom
        2  | NULL            |           | NULL                                                 | Number of sampler points
     2010  | parameters      | 0         | length(model$par)                                    | Total number of parameters
     2020  | AIC             | 1e-3      | model$criterion                                      | Akaike Information Criterion
     2030  | Chi^2 p         | 1e-3      | ddf.gof(model, qq=FALSE)$chisquare$chi1$p            | Goodness-of-fit chi-square test probability (test 1, after pooling)
     2040  | NULL            |           | NULL                                                 | PDF (lines) or slope of PDF (points) at x=0
     2050  | P_a             | 1e-3      | model_sum$average.p                                  | Probability of detection
     2051  | CV(P_a)         | 1e-3      | as.numeric(model_sum$average.p.se)/model_sum$average.p | Probability of detection coeff. of var.
     2052  | NULL            |           | NULL                                                 | Probability of detection lower conf. limit
     2053  | NULL            |           | NULL                                                 | Probability of detection upper conf. limit
     2054  | NULL            |           | NULL                                                 | Probability of detection degrees of freedom
     2060  | NULL            |           | NULL                                                 | Effective strip width/detection radius
     2061  | NULL            |           | NULL                                                 | Effective strip width/detection radius coeff. of var.
     2062  | NULL            |           | NULL                                                 | Effective strip width/detection radius lower conf. limit
     2063  | NULL            |           | NULL                                                 | Effective strip width/detection radius upper conf. limit
     2064  | NULL            |           | NULL                                                 | Effective strip width/detection radius degrees of freedom
     2070  | NULL            |           | NULL                                                 | Small-sample adjusted Akaike Information Criterion value
     2080  | NULL            |           | NULL                                                 | Bayes\' Information Criterion
     2090  | log-likelihood  |  1e-4     | model$lnl                                            | Log likelihood
     2100  | K-S p           |  1e-5     | ddf.gof(model, qq=FALSE)$dsgof$ks$p                  | Goodness-of-fit Kolmogorov-Smirnov test probability
     2110  | C-vM p          |  1e-1     | round(ddf.gof(model, qq=FALSE)$dsgof$CvM$p, 1)       | Goodness-of-fit Cramer-von Mises (uniform weighting) test probability
     2120  | NULL            |           | NULL                                                 | Goodness-of-fit Cramer-von Mises (cosine weighting) test probability
     2150  | NULL            |           | NULL                                                 | Number of key function parameters
     2160  | NULL            |           | NULL                                                 | Number of adjustment term parameters
     2170  | NULL            |           | NULL                                                 | Number of covariate parameters
        3  | NULL            |           | NULL                                                 | Number of sampler lines
     3010  | NULL            |           | NULL                                                 | Mean cluster size
     3011  | NULL            |           | NULL                                                 | Mean cluster size coeff. of var.
     3012  | NULL            |           | NULL                                                 | Mean cluster size lower conf. interval
     3013  | NULL            |           | NULL                                                 | Mean cluster size upper conf. interval
     3014  | NULL            |           | NULL                                                 | Mean cluster size degrees of freedom
     3020  | NULL            |           | NULL                                                 | Size bias regression correlation coefficient
     3030  | NULL            |           | NULL                                                 | Size bias regression correlation test probability
     3040  | NULL            |           | NULL                                                 | Expected cluster size, correcting for size bias
     3041  | NULL            |           | NULL                                                 | Expected cluster size coeff. of var.
     3042  | NULL            |           | NULL                                                 | Expected cluster size lower conf. limit
     3043  | NULL            |           | NULL                                                 | Expected cluster size upper conf. limit
     3044  | NULL            |           | NULL                                                 | Expected cluster size degrees of freedom
        4  | NULL            |           | NULL                                                 | Maximum possible area coverage
     4010  | NULL            |           | NULL                                                 | Density of clusters
     4011  | NULL            |           | NULL                                                 | Density of clusters analytic coeff. of var.
     4012  | NULL            |           | NULL                                                 | Density of clusters analytic lower conf. limit
     4013  | NULL            |           | NULL                                                 | Density of clusters analytic upper conf. limit
     4014  | NULL            |           | NULL                                                 | Density of clusters degrees of freedom
     4020  | density         | 1e-4      | as.numeric(dht$individuals$D$Estimate)[nrow(dht$individuals$D)] | Density of individuals
     4021  | CV(density)     | 1e-4      | as.numeric(dht$individuals$D$cv)[nrow(dht$individuals$D)]       | Density of individuals analytic coeff. of var.
     4022  | density lcl     | 1e-4      | NULL                                                 | Density of individuals analytic lower conf. limit
     4023  | density ucl     | 1e-4      | NULL                                                 | Density of individuals analytic upper conf. limit
     4024  | density df      | 1e-1      | NULL                                                 | Density of individuals degrees of freedom
     4030  | individuals     | 1e-1      | as.numeric(dht$individuals$N$Estimate)[nrow(dht$individuals$N)] | Number of individuals
     4031  | CV(individuals) | 1e-4      | as.numeric(dht$individuals$N$cv)[nrow(dht$individuals$N)]       | Number of individuals analytic coeff. of var.
     4032  | individuals lcl | 1e-1      | NULL                                                 | Number of individuals analytic lower conf. limit
     4033  | individuals ucl | 1e-1      | NULL                                                 | Number of individuals analytic upper conf. limit
     4034  | individuals df  | 1e-1      | NULL                                                 | Number of individuals analytic degrees of freedom
     4040  | NULL            |           | NULL                                                 | Density of clusters bootstrap
     4041  | NULL            |           | NULL                                                 | Density of clusters bootstrap coeff. of var.
     4042  | NULL            |           | NULL                                                 | Density of clusters bootstrap lower conf. limit
     4043  | NULL            |           | NULL                                                 | Density of clusters bootstrap upper conf. limit
     4050  | NULL            |           | NULL                                                 | Density of individuals bootstrap
     4051  | NULL            |           | NULL                                                 | Density of individuals bootstrap coeff. of var.
     4052  | NULL            |           | NULL                                                 | Density of individuals bootstrap lower conf. limit
     4053  | NULL            |           | NULL                                                 | Density of individuals bootstrap upper conf. limit
     4060  | NULL            |           | NULL                                                 | Number of individuals bootstrap
     4061  | NULL            |           | NULL                                                 | Number of individuals bootstrap coeff. of var.
     4062  | NULL            |           | NULL                                                 | Number of individuals bootstrap lower conf. limit
     4063  | NULL            |           | NULL                                                 | Number of individuals upper confidence limit
     4064  | NULL            |           | NULL                                                 | Number of bootstrap resamples
        5  | NULL            |           | NULL                                                 | Realized area coverage
     5010  | NULL            |           | NULL                                                 | Generalized cross-validation metric for DSM model
     5020  | NULL            |           | NULL                                                 | Sample size (number of segments) for DSM model
     5030  | NULL            |           | NULL                                                 | Proportion of deviance explained by DSM model
     5040  | NULL            |           | NULL                                                 | The estimated scale parameter/error variance
        6  | NULL            |           | NULL                                                 | Proportion of survey area covered
        7  | NULL            |           | NULL                                                 | Mean realized sampler line length (mean over strata)',

  delim='|', comment="#")

  stat_tab <- stat_tab[stat_tab$MRDS!="NULL", ]

  stat_tab$Code <- as.numeric(stat_tab$Code)

  return(stat_tab)
}
