## ChickWeight case study (bootstrap)
## Stats 506, Fall 2018
##
## Use the bootstrap to compute 95% confidence intervals for
## the median final weight within each diet. Do the same for the 
## median final weight relative to the initial birth weight.
## All analyses are conditional on survival to week 21.
##
## Data: datasets::Chickweight
##
## Updated: Nov 5, 2018
## Author: James Henderson (jbhender@umich.edu)

# libraries: ------------------------------------------------------------------
library(tidyverse); library(data.table)

# ChickWeight data: -----------------------------------------------------------
cw_dt = as.data.table(ChickWeight)
#str(cw_dt, give.attr = FALSE)

# Visualize the data: ---------------------------------------------------------

# Count cases missing times: --------------------------------------------------

# Compare starting and ending weights: ----------------------------------------
#! visually

# Compute relative weight: ----------------------------------------------------

# Interactive computation of median the data.table way: -----------------------

# Progress towards functional version: ----------------------------------------

# Funciton to compute median by group for specific columns
dt_boot_median = function(){
  
}

# Test new function

# Function to compute a subsample: --------------------------------------------
#cw_final_dt = cw_dt[Time==21]

# Note that this does not work because "i" does not operate by group
# cw_final_dt[sample(.N, replace=TRUE),  .(weight), Diet][ , .N, Diet]

dt_group_boot = function(dt, cols, group = ''){

}

# Test these two functions together: ------------------------------------------ 

# Here's a version that does both sampling and median computation in one step.

dt_boot_median = function(dt, cols, group=''){

}

# Estimate the median
s = dt_boot_median(cw_final_dt, c('weight', 'rel_weight'), 'Diet')

# Melt to long

# Sample in a loop: -----------------------------------------------------------
boot_samples = list()
for(i in 1:1e3){
  boot_samples[[i]] = 
    dt_boot_median(cw_final_dt, c('weight', 'rel_weight'), 'Diet')
}
boot_samples = rbindlist(boot_samples)

# Compute quantiles for CI: ---------------------------------------------------

# Reshape to long and then to wide with bounds as columns
boot_ci_long = 

# Plot medians and associated 95% confidence intervals for each group/stat: ...

# Compare this to a simple comparison of means: -------------------------------

## Actual weights
fit_w = aov(weight ~ Diet, data = cw_final_dt)           
summary(fit_w)
TukeyHSD(fit_w)

## Relative weights
fit_rw = aov(rel_weight ~ Diet, data = cw_final_dt)           
summary(fit_rw)
TukeyHSD(fit_rw)

# Write a better "vectorized" function to avoid the loop: ---------------------
dt_boot_medians = function(dt, cols, group='', nboot = 1e3){
  # Create nboot data sets in long format
  tmp = dt[, .SD[ sample(.N, .N*nboot, replace = TRUE)], keyby = group,
           .SDcols = cols ]
  
  # Add a column to index the bootstrap samples

  # Test
  #tmp[,.N, c(group, 'b')][,.N,c(group, 'N')]

  # Compute medians for each sample/group, clean, and return  

}

# Test and compare: -----------------------------------------------------------
boot_samples2 = dt_boot_medians(cw_final_dt, 
                                c('weight', 'rel_weight'), 
                                group = 'Diet',
                                nboot = 1e4)

boot_ci2 = boot_samples2[ , c( .(bound = c('lwr', 'upr')), 
                             lapply(.SD, quantile, probs = c(.025, .975) )
), Diet ]

# Reshape to long and then to wide with bounds as columns


