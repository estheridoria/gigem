#' Compute Summary Statistics (Internal)
#'
#' This function calculates summary statistics (N, mean, standard deviation, standard error, and confidence interval)
#' for a specified measure variable, grouped by one or more grouping variables.
#'
#' @param data A data.frame or data.table containing the data to be analyzed.
#' @param measurevar A character string specifying the variable for which the summary statistics are calculated.
#' @param groupvars A character vector of grouping variables by which to calculate the statistics.
#' @param na.rm A logical value indicating whether NA values should be removed before calculations (default: FALSE).
#' @param conf.interval A numeric value specifying the confidence interval to compute (default: 0.95).
#' @param .drop A logical value to determine whether to drop unused factor levels in the result (default: TRUE).
#'
#' @return A data.frame or data.table containing the summary statistics (N, mean, SD, SE, CI) for each group.
#' @keywords internal
summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, conf.interval = .95, .drop = TRUE) {

  # Function to calculate length while handling NAs
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) sum(!is.na(x)) else length(x)
  }

  # Calculate summary statistics
  datac <- plyr::ddply(data, groupvars, .drop=.drop, .fun = function(xx, col) {
    c(N = length2(xx[[col]], na.rm=na.rm),
      mean = mean(xx[[col]], na.rm=na.rm),
      sd = sd(xx[[col]], na.rm=na.rm))
  }, measurevar)
  
  # Add standard error and confidence interval
  datac$se <- datac$sd / sqrt(datac$N)  # Standard error
  ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)  # Confidence interval multiplier
  datac$ci <- datac$se * ciMult
  
  datac <- magrittr::`%>%`(datac, 
                           dplyr::rename(., 
                                         !!paste(measurevar, "mean", sep = "_") := mean,
                                         !!paste(measurevar, "sd", sep = "_") := sd,
                                         !!paste(measurevar, "se", sep = "_") := se,
                                         !!paste(measurevar, "ci", sep = "_") := ci))
  

  return(datac)
}
