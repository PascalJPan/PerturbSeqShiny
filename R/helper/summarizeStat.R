#' Summarize values of a statistic
#'
#' @description Calculates the estimate (mean or median), the confidence interval of the estimate with the specified confidence level and the variance of the data provided.
#' @param values Numeric, integer or logical vector.
#' @param summary_method Character, the measure of central tendency ("mean" or "median") to be used.
#' @param conf_level Numeric, confidence level of the interval (default: 0.95).
#'
#' @return A data frame with 1 row and 4 columns:
#' \describe{
#' \item{estimate}{Numeric, the central tendency (mean or median) of 'values'.}
#' \item{var}{Numeric, the variance of 'values'.}
#' \item{lwr}{Numeric, the lower bound of the confidence interval.}
#' \item{upr}{Numeric, the upper bound of the confidence interval.}
#' }
#' @export
summarizeStat <- function(values, summary_method, conf_level = 0.95) {
  
  if (is.null(summary_method) || !summary_method %in% c("mean", "median"))
    stop("The argument \"summary_method\" should be one of \"mean\", \"median\".")
  
  if (!inherits(conf_level, "numeric") || length(conf_level) != 1 || conf_level < 0 || conf_level >= 1)
    stop("The argument \"conf_level\" should be a numeric value between 0 and 1.")
  
  if (summary_method == "mean") {
    
    n <- length(values)
    moe_values <- -stats::qt(p = (1 - conf_level)/2, df = n - 1) * stats::sd(values) / sqrt(n)
    mean_values <- mean(values)
    
    data.frame(estimate = mean_values,
               var = stats::var(values),
               lwr = mean_values - moe_values,
               upr = mean_values + moe_values)
    
  } else if (summary_method == "median") {
    
    median_values <- stats::median(values)
    
    if (is.na(median_values)) {
      
      ci <- c(NA, NA)
      
    } else {
      
      n <- length(values)
      k <- stats::qbinom(p = (1 - conf_level) / 2, size = n, prob = 0.5, lower.tail = TRUE)
      ci <- sort(values)[c(k, n - k + 1)]
      if (identical(ci, NA_real_)) ci <- c(-Inf, Inf)
      
    }
    
    data.frame(estimate = median_values,
               var = stats::var(values),
               lwr = ci[1],
               upr = ci[2])
    
  }
  
}
