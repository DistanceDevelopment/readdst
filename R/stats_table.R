#' Generate table of possible statistics to test
#'
#' To use \code{\link{get_stats}} we need a set of statistics to test. We also require their codes (to look up in the Distance for Windows database) and their equivalent values in \code{mrds} (or how to calculate those values). This function provides such a table.
#'
#' @section Details:
#' Data for this table (numeric code and descriptions) is from the \code{DistIni.mdb} which is shipped with Distance for Windows. See also \url{https://github.com/distancedevelopment/readdst/wiki/distance-results-codes}.
#'
#' @return a \code{data.frame} with statistics Distance for Windows collects that have equivalents in \code{mrds}. The \code{data.frame} has three columns: \code{Code}, the numeric code for the statistic (as used in the Distance for Windows database); \code{MRDS} the operation required to obtain the equivalent statistic in \code{mrds}; \code{Description}, a short description of the statistic.
#' @author David L Miller
#' @importFrom readr read_delim
stats_table <- function(){

  # here is a lookup table
  # this trick thanks to Noam Ross
  stat_tab <- read_delim(
    ' Code | MRDS                                      | Description
    #----- | ----------------------------------------- | ------
        1  | NULL                                      | Number of sampler points
      100  | NULL                                      | Mean coverage probability
     1000  | NULL                                      | AIC - (minimum AIC), calculated within survey and data filter
     1001  | NULL                                      | AICc - (minimum AICc), calculated within survey and data filter
     1002  | NULL                                      | BIC - (minimum BIC), calculated within survey and data filter
     1003  | NULL                                      | LogL - (minimum LogL), calculated within survey and data filter
      101  | NULL                                      | Minimum coverage probability
     1010  | nrow(model$data)                          | Number of observations
      102  | NULL                                      | Maximum coverage probability
     1020  | NULL | Number of samples
      103  | NULL                                      | Standard deviation of coverage probability
     1030  | NULL | Total survey effort
     1040  | NULL | Encounter rate
     1041  | NULL | Encounter rate coeff. of var.
     1042  | NULL | Encounter rate lower conf. limit
     1043  | NULL | Encounter rate upper conf. limit
     1044  | NULL | Encounter rate degrees of freedom
        2  | NULL | Number of sampler points
     2010  | length(model$par)                         | Total number of parameters
     2020  | model$criterion                           | Akaike Information Criterion (AIC) value
     2030  | ddf.gof(model, qq=FALSE)$chisquare$chi1$p | Goodness-of-fit chi-square test probability (test 1, after pooling)
     2040  | NULL | Probability density function (line transects) or slope of the pdf (point transects) at x=0
     2050  | NULL | Probability of detection
     2051  | NULL | Probability of detection coeff. of var.
     2052  | NULL | Probability of detection lower conf. limit
     2053  | NULL | Probability of detection upper conf. limit
     2054  | NULL | Probability of detection degrees of freedom
     2060  | NULL | Effective strip width/detection radius
     2061  | NULL | Effective strip width/detection radius coeff. of var.
     2062  | NULL | Effective strip width/detection radius lower conf. limit
     2063  | NULL | Effective strip width/detection radius upper conf. limit
     2064  | NULL | Effective strip width/detection radius degrees of freedom
     2070  | NULL                                      | Small-sample adjusted Akaike Information Criterion value
     2080  | NULL                                      | Bayes\' Information Criterion
     2090  | model$lnl                                 | Log likelihood
     2100  | ddf.gof(model, qq=FALSE)$dsgof$ks$p       | Goodness-of-fit Kolmogorov Smirnov test probability
     2110  | ddf.gof(model, qq=FALSE)$dsgof$CvM$p      | Goodness-of-fit Cramer-von Mises (uniform weighting) test probability
     2120  | NULL                                      | Goodness-of-fit Cramer-von Mises (cosine weighting) test probability
     2150  | NULL                                      | Number of key function parameters
     2160  | NULL                                      | Number of adjustment term parameters
     2170  | NULL                                      | Number of covariate parameters
        3  | NULL                                      | Number of sampler lines
     3010  | NULL | Mean cluster size
     3011  | NULL | Mean cluster size coeff. of var.
     3012  | NULL | Mean cluster size lower conf. interval
     3013  | NULL | Mean cluster size upper conf. interval
     3014  | NULL | Mean cluster size degrees of freedom
     3020  | NULL | Size bias regression correlation coefficient
     3030  | NULL | Size bias regression correlation test probability
     3040  | NULL | Expected cluster size, correcting for size bias
     3041  | NULL | Expected cluster size coeff. of var.
     3042  | NULL | Expected cluster size lower conf. limit
     3043  | NULL | Expected cluster size upper conf. limit
     3044  | NULL | Expected cluster size degrees of freedom
        4  | NULL | Maximum possible area coverage
     4010  | NULL | Density of clusters
     4011  | NULL | Density of clusters analytic coeff. of var.
     4012  | NULL | Density of clusters analytic lower conf. limit
     4013  | NULL | Density of clusters analytic upper conf. limit
     4014  | NULL | Density of clusters degrees of freedom
     4020  | NULL | Density of individuals
     4021  | NULL | Density of individuals analytic coeff. of var.
     4022  | NULL | Density of individuals analytic lower conf. limit
     4023  | NULL | Density of individuals analytic upper conf. limit
     4024  | NULL | Density of individuals degrees of freedom
     4030  | NULL | Number of individuals
     4031  | NULL | Number of individuals analytic coeff. of var.
     4032  | NULL | Number of individuals analytic lower conf. limit
     4033  | NULL | Number of individuals analytic upper conf. limit
     4034  | NULL | Number of individuals analytic degrees of freedom
     4040  | NULL | Density of clusters bootstrap
     4041  | NULL | Density of clusters bootstrap coeff. of var.
     4042  | NULL | Density of clusters bootstrap lower conf. limit
     4043  | NULL | Density of clusters bootstrap upper conf. limit
     4050  | NULL | Density of individuals bootstrap
     4051  | NULL | Density of individuals bootstrap coeff. of var.
     4052  | NULL | Density of individuals bootstrap lower conf. limit
     4053  | NULL | Density of individuals bootstrap upper conf. limit
     4060  | NULL | Number of individuals bootstrap
     4061  | NULL | Number of individuals bootstrap coeff. of var.
     4062  | NULL | Number of individuals bootstrap lower conf. limit
     4063  | NULL | Number of individuals upper confidence limit
     4064  | NULL | Number of bootstrap resamples
        5  | NULL | Realized area coverage
     5010  | NULL | Generalized cross-validation metric for DSM model
     5020  | NULL | Sample size (number of segments) for DSM model
     5030  | NULL | Proportion of deviance explained by DSM model
     5040  | NULL | The estimated scale parameter/error variance
        6  | NULL | Proportion of survey area covered
        7  | NULL | Mean realized sampler line length (mean over strata)',

  delim='|', comment="#")

  stat_tab <- stat_tab[stat_tab$MRDS!="NULL", ]

  return(stat_tab)
}
