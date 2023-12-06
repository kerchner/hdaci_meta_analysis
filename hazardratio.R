# We can use this method in the event that a given study does not 
# compute hazard ratio w/confidence interval.
# Source was ChatGPT
# ChatGPT prompt was:
#  If I have two groups and their medial survival times
#  with confidence intervals, and the number of patients
#  in each group, but i do not have individual patient data, 
#  how can i estimate the hazard ratio and its confidence interval?

# Note that this method is based on an assumption that 
# survival times follow an exponential distribution within each group

# Sample data in example are from
# http://dx.doi.org/10.1016/S2352-3026(16)30147-8


#' Estimate hazard ratio from median overall survival, confidence intervals, and group sizes
#'
#' @param median_os_group1 
#' @param median_os_group2 
#' @param n_group1 
#' @param n_group2 
#'
#' @returns A list containing HR estimate with 95% confidence interval 
#'
#' @examples
#' estimate_hr(median_os_group1 = 40.3, median_os_group2 = 35.8,
#'             n_group1 = 387, n_group2 = 381)
estimate_hr <- function(median_os_group1, median_os_group2,
                        n_group1, n_group2) {
  # Calculating hazard ratio using exponential distribution approximation
  # Assuming exponential distribution: median = log(2) / lambda
  lambda_group1 <- log(2) / median_os_group1
  lambda_group2 <- log(2) / median_os_group2
  
  # Estimating hazard ratio using the ratio of hazard rates (lambda)
  hazard_ratio <- lambda_group1 / lambda_group2
  
  # Calculating confidence interval for the hazard ratio
  lower_CI <- exp(log(hazard_ratio) - 1.96 * sqrt((1 / n_group1) + (1 / n_group2)))
  upper_CI <- exp(log(hazard_ratio) + 1.96 * sqrt((1 / n_group1) + (1 / n_group2)))  
  
  result <- list()
  result$hr <- hazard_ratio
  result$ci <- c(lower_CI, upper_CI)
  
  return(result)
}
