library(dplyr)
library(meta)
library(metafor)

df <- read.csv('data/study_data.csv')

# Adapted from:
# https://stats.stackexchange.com/questions/343316/hazard-ratio-meta-analysis

### log-transform hazard ratios and compute standard error 
### based on the confidence interval bounds 
df <- df %>%
  mutate(yi = log(HR),
         sei = (log(HR_CI_upper) - log(HR_CI_lower))/(2*1.96),
         trial = row_number(),
         slab = Study)

res <- rma(yi, sei=sei, data=df, slab=df$slab, method="REML")

summary(res)

forest(res)

funnel(res)
