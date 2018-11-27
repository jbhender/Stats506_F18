## Problem Set 3, Question 1
## Stats 506, Fall 2018
##
## RECS consumption data is available at:
## https://www.eia.gov/consumption/residential/data/2015/
##
## Author: James Henderson
## Updated: November 21, 2018

# libraries: ------------------------------------------------------------------
library(tidyverse); library(data.table)

# Multiplier for confidence level: --------------------------------------------
m = qnorm(.975)

# data: -----------------------------------------------------------------------
file = '../PS1/recs2015_public_v3.csv'
if ( !file.exists(file) ){
 recs =  fread('https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv')
  fwrite(recs, file = file)
} else {
  recs = fread(file)
}

# Replicate weights: ----------------------------------------------------------
brrwt_cols =  paste0('BRRWT',1:96)
weights =  recs[, c('DOEID', brrwt_cols), with = FALSE]
weights_long = melt(weights, id.vars = 'DOEID', measure.vars = brrwt_cols, 
                    variable.name = 'repl', value.name = 'w')

# Division map: ---------------------------------------------------------------
divisions = c(
  'New England',
  'Middle Atlantic',
  'East North Central',
  'West North Central',
  'South Atlantic',
  'East South Central',
  'West South Central',
  'Mountain North',
  'Mountain South',
  'Pacific'
)
recs[, division := factor(DIVISION, 1:10, divisions)]

############
## Part a ##
############
# Notes: Stucco construction as major outside wall material by Division.
## Stucco: WALLTYPE == 4

# Point estimate: ------------------------------------------------------------
p_stucco = recs[ , .(p_stucco = sum( NWEIGHT*{WALLTYPE == 4} ) / sum(NWEIGHT) ),
                 division]

# Estimates from replicate weights: -------------------------------------------
p_stucco_r = 
  weights_long[recs, .(w, repl, WALLTYPE, division), on = 'DOEID'] %>%
  .[ , .( r_stucco =  sum( w*{WALLTYPE == 4} ) / sum(w) ), .(division, repl)]

# Compute standard errors: ----------------------------------------------------
p_stucco_r = merge(p_stucco_r, p_stucco, by = 'division')

p_stucco = p_stucco_r[ , .(p_stucco = p_stucco[1],
                           se_stucco = 2*sqrt( mean( {r_stucco - p_stucco}^2 ) )
                           ) , .(division)]

# Compute confidence bounds: --------------------------------------------------
p_stucco[ , `:=`( lwr = pmax(p_stucco - m*se_stucco, 0), 
                  upr = p_stucco + m*se_stucco
                 ) ] 

# Repeat using a function: ---------------------------------------------------
est_recs = function(df, group, weights, fay = .5, m = qnorm(.975), 
                    ci_format = '%4.1f (%4.1f, %4.1f)'){
  # function to compute the weighted mean sum(w*x)/sum(w) in df
  # and it associated standard error using replicate weights in weights
  #
  # Args:
  #  weights: a data.table with columns: id, repl, w
  #  df: a data.table with columns: id, w, x, and any grouping variables
  #  group: a characer vector of grouping variables
  #  m: a multipler for computing a confidence interval: xbar +/- m*se
  #
  # Details: The weighted sum of `x`` in `df`` is first computed and then its
  # standard error is found by joining against the replicate weights `weights`
  # and recomputing for each unique value of `repl`
  #
  # Returns: 
  #  A data.table with columns:
  #   - est: weighted mean of column "x"
  #   - se:  standard error determined by replicate weights and Fay coefficient
  #   - ci:  est +/- m*se
  
  
  # Point estimate
  pe = df[, .(est = sum( w*x) / sum(w) ), group]
  
  # Replicate estimates
  pe_r = merge(weights, df[,-"w", with=FALSE], by = 'id')[ ,
                .(r = sum(w*x)/sum(w)), c('repl', group) ]

  # Std error and confidence interval
  pe = merge(pe, pe_r, by = group)
  pe = pe[, .(est = est[1], 
              se = 1/fay * sqrt( mean( {r - est}^2 ) )
              ), group]
  pe[ , `:=`(lwr = est - m*se,
             upr = est + m*se
             )]
  
  pe[ , ci := sprintf(ci_format, est, lwr, upr)][]
  
  pe

}

# Test the function above gives the previous results: -------------------------
weights_long = weights_long %>% rename( id = DOEID )
setnames(weights_long, c('id', names(weights_long)[2:3]) )

if( FALSE ){
  df = recs[ , .(id = DOEID, division, w = NWEIGHT, x = 100*{WALLTYPE == 4})]
  est_recs(df, group = 'division', weights_long )
}

############
## Part b ##
############
# Notes: What is the average total kwh of electricity usage by division?
# By each division Urban and rural subgroups?

kwh = recs[ , .(id = DOEID, w = NWEIGHT, x = KWH, division)]
kwh = est_recs(kwh, 'division', weights_long, ci_format = '%4.0f (%4.0f, %4.0f)')

kwh_div_urban = recs[ , .( id = DOEID, w = NWEIGHT, x = KWH, division, 
                           urban = UATYP10 %in% c('U', 'C') ) ]
kwh_div_urban = 
  est_recs( kwh_div_urban, c('division', 'urban'), 
            weights_long, ci_format = '%4.0f (%4.0f, %4.0f)' )

############
## Part C ##
############

# Internet access data: -------------------------------------------------------
internet =  recs[ , .( id = DOEID, w = NWEIGHT, x = 100*INTERNET, division, 
                       urban = UATYP10 %in% c('U', 'C') )  ]

# Urban/rural estimates for each division: -----------------------------------
internet_ru = est_recs(internet, c('division', 'urban'), weights_long) 

# Point estimate for difference: ---------------------------------------------
# keyby ensure we have consistent ordering
pe = internet[ , .(est = sum(w*x) / sum(w)), keyby = .(division, urban) ] %>% 
  .[ , .(est = est[2] - est[1]), division]

# Replicate estimates for difference: ----------------------------------------
pe_r = merge(weights_long, internet[,-"w", with=FALSE], by = 'id') %>% 
  .[ , .(est = sum(w*x) / sum(w)), keyby = .(repl, division, urban) ] %>% 
  .[ , .(r = est[2] - est[1]), .(repl, division)]

# Std error and confidence interval for differnce: ---------------------------
internet_disp = merge(pe_r, pe, by = c('division') ) %>%
  .[ , .( est = est[1], se = 2 * sqrt( mean( {r - est}^2 ) ) ), division]

internet_disp[ , ci := sprintf('%4.1f%% (%4.1f, %4.1f)',
                               est, est - m*se, est + m*se) ][] 

# Join urban & rural estimates to differences: -------------------------------
iru_wide = dcast(internet_ru, division ~ urban, value.var = 'ci')
internet_disp = merge(iru_wide, internet_disp[,.(division, ci, est)],
                      by = 'division')
internet_disp = internet_disp[order(-est)]
internet_disp[, est := NULL]
setnames(internet_disp, c('Division', 'Rural', 'Urban', 'Diff') )
