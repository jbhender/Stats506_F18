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
cw_dt[Time==max(Time)] %>%
  ggplot( aes( x = Diet, y = weight, fill = Diet) ) + 
  geom_boxplot( alpha = .5) + 
  theme_bw() + 
  geom_point( aes(color = Diet), position = position_jitter(width = .1) ) +
  ylab('Chick weight at 21 weeks (grams)')

# Count cases missing times: --------------------------------------------------
cw_dt[ , .N, keyby = .(Diet, Time)][ , .( nmiss = max(N) - min(N) ), Diet]

# Compare starting and ending weights: ----------------------------------------
merge( cw_dt[Time == min(Time), .(Diet, Chick, start = weight)],
       cw_dt[Time == max(Time), .(Chick, end = weight) ],
       by = 'Chick' ) %>%
  ggplot( aes( x = start, y = end, color = Diet) ) +
  geom_point( alpha = .5) + 
  theme_bw()

# Compute relative weight: ----------------------------------------------------
cw_dt[ , rel_weight := weight / weight[Time==0], Chick]

# Interactive computation the data.table way: ---------------------------------
cw_dt[Time==21, .(median = median(weight), n = .N ), Diet]

# Progress towards functional version: ----------------------------------------
cw_dt[Time==21, c( lapply(.SD, median), n = .N), Diet, .SDcols = 'weight' ]
cw_dt[Time==21, c( lapply(.SD, median), n = .N), 'Diet', .SDcols = 'weight' ]

# Funciton to compute median by group for specific columns
dt_group_median = function(dt, cols, group ='') {
  dt[ , c( lapply(.SD, median), n = .N), group, .SDcols = cols ]
}

# Test new function
dt_group_median(cw_dt[Time==21], 'Diet', 'weight')
dt_group_median(cw_dt[Time==21], 'Diet', c('weight','rel_weight') )

# Function to compute a subsample: --------------------------------------------
cw_final_dt = cw_dt[Time==21]

cw_final_dt[ ,  .( weight[ sample(.N, replace=TRUE) ] ), Diet][,.N,Diet]

# Note that this does not work because "i" does not operate by group
# cw_final_dt[sample(.N, replace=TRUE),  .(weight), Diet][ , .N, Diet]

cw_final_dt[ , .SD[ sample(.N, replace = TRUE) ], Diet, .SDcols = 'weight']

dt_group_boot = function(dt, cols, group = ''){
  dt[ , .SD[ sample(.N, replace = TRUE) ], group, .SDcols = cols]
}

# Test these two functions together: ------------------------------------------ 
cw_final_dt %>% 
  dt_group_boot(., 'weight', group = 'Diet') %>%
  dt_boot_median(., 'weight', group = 'Diet')

# Here's a version that does both sampling and median computation in one step.
#cw_final_dt[ , lapply(.SD[ sample(.N, replace = TRUE) ], median), Diet, .SDcols = 'weight']

dt_boot_median = function(dt, cols, group=''){
  dt[, lapply( .SD[ sample(.N, replace = TRUE) ], median), group, 
     .SDcols = cols]  
}

median_est = dt_boot_median(cw_final_dt, c('weight', 'rel_weight'), 'Diet')

# Melt to long
median_est = melt( median_est, id.vars = c('Diet'), 
                   variable.name = 'stat', value.name = 'est')

boot_samples = list()
for(i in 1:1e3){
  boot_samples[[i]] = 
    dt_boot_median(cw_final_dt, c('weight', 'rel_weight'), 'Diet')
}

boot_samples = rbindlist(boot_samples)

boot_ci = boot_samples[ , c( .(bound = c('lwr', 'upr')), 
                             lapply(.SD, quantile, probs = c(.025, .975) )
), Diet ]

# Reshape to long and then to wide with bounds as columns
boot_ci_long = melt( boot_ci, id.vars = c('Diet', 'bound'), variable.name = 'stat' ) %>%
  dcast(., Diet + stat ~ bound, value.var = 'value')

# Plot medians and associated 95% confidence intervals for each group/stat: ...
median_est[boot_ci_long, .(Diet, stat, est, lwr, upr),
           on = c('Diet', 'stat')] %>%
  ggplot( aes(x = Diet, y = est) ) +
  geom_point( pch = 15 ) +
  geom_errorbar( aes(ymin = lwr, ymax = upr) ) +
  facet_grid(stat~., scales = 'free_y' ) + 
  theme_bw() + 
  ylab('Week 21 chick weight [lower: relative to birth weight; upper: actual weight (gm)]')

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
  dtn = dt[ , .(n = .N), keyby = group]
  tmp = tmp[dtn, on = group]
  tmp[dtn , b := 0:{.N-1} %/% n, group]
  
  # Test
  #tmp[,.N, c(group, 'b')][,.N,c(group, 'N')]
  
  # Compute medians for each sample/group, clean, and return  
  tmp[, lapply(.SD, median), c('b', group)][, `:=`(b = NULL, n = NULL)][]
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
boot_ci_long = melt( boot_ci, id.vars = c('Diet', 'bound'), variable.name = 'stat' ) %>%
  dcast(., Diet + stat ~ bound, valuevar = 'value')

# Plot medians and associated 95% confidence intervals for each group/stat: ...
median_est[boot_ci_long, .(Diet, stat, est, lwr, upr),
           on = c('Diet', 'stat')] %>%
  .[stat=='rel_weight', stat:="relative weight"] %>%
  ggplot( aes(x = Diet, y = est) ) +
  geom_point( pch = 15 ) +
  geom_errorbar( aes(ymin = lwr, ymax = upr) ) +
  facet_grid(stat~., scales = 'free_y' ) + 
  theme_bw() + 
  ylab('Week 21 chick weight [lower: relative to birth weight; upper: actual weight (gm)]')


