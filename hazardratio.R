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

# Sample data here are from
# http://dx.doi.org/10.1016/S2352-3026(16)30147-8

# Median survival times and confidence intervals for each group
median_group1 <- 40.3  # Replace with actual median survival time for group 1
ci_group1 <- c(35, 44.8)  # Replace with actual CI for median survival time for group 1

median_group2 <- 35.8  # Replace with actual median survival time for group 2
ci_group2 <- c(29, 40.6)  # Replace with actual CI for median survival time for group 2

# Number of patients in each group
n_group1 <- 387  # Replace with actual number of patients in group 1
n_group2 <- 381  # Replace with actual number of patients in group 2

# Calculating hazard ratio using exponential distribution approximation
# Assuming exponential distribution: median = log(2) / lambda
lambda_group1 <- log(2) / median_group1
lambda_group2 <- log(2) / median_group2

# Estimating hazard ratio using the ratio of hazard rates (lambda)
hazard_ratio <- lambda_group1 / lambda_group2

# Calculating confidence interval for the hazard ratio
lower_CI <- exp(log(hazard_ratio) - 1.96 * sqrt((1 / n_group1) + (1 / n_group2)))
upper_CI <- exp(log(hazard_ratio) + 1.96 * sqrt((1 / n_group1) + (1 / n_group2)))

# Displaying results
cat("Estimated Hazard Ratio:", round(hazard_ratio, 2), "\n")
cat("95% Confidence Interval for Hazard Ratio:", round(lower_CI, 2), "-", round(upper_CI, 2), "\n")
