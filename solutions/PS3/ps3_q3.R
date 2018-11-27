## Problem Set 3, Question 3
## Stats 506, Fall 2018
##
## James Henderson
## Updated: Nov 21, 2018

# libraries: ------------------------------------------------------------------
library(tidyverse); library(data.table)

# write mtcars data for reading in translation: -------------------------------
fwrite(mtcars, file = 'mtcars.csv')

# part a, translate ps3_q3.do: ------------------------------------------------

# variables to regress
vars = c('disp', 'hp', 'wt')

# variables to keep
keep = c('mpg', 'cyl', vars)

# read, selecting only needed columns
cars = fread('mtcars.csv', select = keep)

# Here's a one-line version for a single variable: ----------------------------
cars[ , .( sum( mpg*{disp-mean(disp)})/sum({disp-mean(disp)}^2)) , keyby = cyl]

# Here's a version for all three
out = cars[ , lapply(vars, function(x){
                        xc = .SD[[x]] - mean(.SD[[x]])
                        sum(.SD[['mpg']]*xc) / sum(xc^2)
                     }), keyby = cyl]
setnames(out, c('cyl', paste0('beta_cyl_', vars)) )
fwrite(out, file = 'mpg_betass_by_cyl.csv')

## Some code to check our work above
betas = c()
for( k in c(4, 6, 8)){
  betas = c(betas, 
  c(with(cars[cyl==k], coef( lm( mpg ~ disp ) )[2] ) ),
    with(cars[cyl==k], coef( lm( mpg ~ hp ) )[2] ), 
    with(cars[cyl==k], coef( lm( mpg ~ wt ) )[2]  )
  )
}
dim(betas) = c(3, 3)
t(betas)

## part b, data.table function based on the above:  ---------------------------
calc_beta_1dv = function(dt, dv, iv, by = NULL){
  # Inputs:
  #    dt - a data.table
  #    dv - a quoted column in dt to be used as the dependent variable
  #    iv - one or more columns (quoted) in dt to be used 1-by-1 as
  #         independent variables
  #    by - one or more quoted columns in dt to group on. 
  #         The default, NULL, results in no grouping.
  #
  # Returns: a data.table of univariate regression coefficients for each level
  #          of `by` in a row. 
  
  if ( length(dv) > 1) stop('Use calc_beta for more than one iv.')
  
  out = dt[ , lapply(iv, function(x){
    xc = .SD[[x]] - mean(.SD[[x]])
    sum(.SD[[dv]]*xc) / sum(xc^2)
  }), keyby = by]
  by_string = NULL
  if ( !is.null(by) ){
    by_string = paste0( '_by_', paste(by, collapse=':'))
  }
  setnames(out, c(by, paste0('beta_', iv, by_string) ) )  
  out
}

## Tests
calc_beta_1dv(cars, dv = 'mpg', iv = vars, by = 'cyl')

# Test for multiple groups
cars = as.data.table(cars)
cars[ , g2 := {1:.N} %% 2 , cyl]
calc_beta_1iv(cars, dv = 'mpg', iv = vars, by = c('cyl', 'g2') )

# For multiple independent variables, we can call the function above multiple
# times. You did not need to do this.
calc_beta = function(dt, iv, dv, by = NULL){
  
  # Compute coefficients for each DV
  out = lapply(dv, calc_beta_1dv, dt = dt, iv = iv, by = by)
  
  # Add column identifying DV
  for(i in 1:length(out)) out[[i]][,dv:=dv[i]]
  
  # Combine reorder columns
  out = rbindlist( out )
  
  setcolorder(out, c(length(out), 1:{length(out)-1}))

  out  
}

## Test for multiple DV
cars[, mpg2 := 2*mpg]
calc_beta(cars, c('mpg', 'mpg2'), iv = vars, by = 'cyl')

# part c, use summarize_at from dplyr: ----------------------------------------
cars = as_tibble(cars)

# Template for a single variable
cars %>% 
  group_by(cyl) %>%
  mutate( disp = disp - mean(disp) ) %>%
  summarize( sum( mpg*disp ) / sum( disp^2 ) ) 

# One possible solution using only summarize_at
cars %>% group_by(cyl) %>%
  summarize_at( .vars = vars, 
                funs( sum( mpg*{ . - mean(.) } ) / sum( { . - mean(.) }^2 ) ) )

# A second, better, solution which avoids centering twice
cars %>% group_by(cyl) %>%
  mutate_at( .vars = vars, funs( . - mean(.) ) ) %>%
  summarize_at( .vars = vars, funs( sum( mpg*. ) / sum( .^2 ) ) )

# A third solution, similar to the data.table version.
cars %>% group_by(cyl) %>%
  summarize_at( .vars = vars, 
                funs( {xc = . - mean(.); sum( mpg*xc ) / sum( xc^2 )} )
  )


# part d, a dplyr function based on the above
calc_beta_1iv_tbl = function(dt, dv, iv, by = NULL){
  # Inputs:
  #    dt - a data.table
  #    dv - a quoted column in dt to be used as the dependent variable
  #    iv - one or more columns (quoted) in dt to be used 1-by-1 as
  #         independent variables
  #    by - one or more quoted columns in dt to group on. 
  #         The default, NULL, results in no grouping.
  #
  # Returns: a data.table of univariate regression coefficients for each level
  #          of `by` in a row. 
  
  if ( length(dv) > 1) stop('Use calc_beta for more than one iv.')
  
  #dt %>% group_by(.data[[ !! by ]]) %>%
  dt = ungroup(dt)
  if ( !is.null(by) ) for( i in 1:length(by) ) dt = group_by( dt, .data[[ !! by[i] ]], add = TRUE )
  
  dt %>%
    summarize_at( .vars = iv, 
                  funs( {xc = . - mean(.); sum( .data[[ !! dv ]]*xc ) / sum( xc^2 )} )
    )
  
}

## Tests
calc_beta_1iv_tbl(cars, 'mpg', c('disp', 'hp', 'wt'), by = 'cyl')

## Tests for multiple groups
calc_beta_1iv_tbl(cars %>% group_by(cyl) %>% mutate(g2 = {1:n()} %% 2),
                  'mpg', c('disp', 'hp', 'wt'), by = c('cyl', 'g2') )
