## Stats 506, Fall 2018
## Problem Set 2, Question 3
##
## Model selection and margins for age a tooth is missing using NHANES 2005
## data.
##
## Data: 2005 NHANES oral health data (OHX_D.XPT) and demograhics (DEMO_D.XPT)
##  https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination&CycleBeginYear=2005
##  https://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics&CycleBeginYear=2005
##
## Author: James Henderson, jbhender@umich.edu
## Updated: October 17, 2018 - Last modified date

# 80: --------------------------------------------------------------------------

# libraries: -------------------------------------------------------------------
library(tidyverse); library(margins)

# data: ------------------------------------------------------------------------
ohx = haven::read_xpt('./OHX_D.XPT')
demo = haven::read_xpt('./DEMO_D.XPT')

# data cleaning: ---------------------------------------------------------------
ohx = left_join(ohx, demo, by = 'SEQN')

ohx = ohx %>%
  transmute( SEQN, tooth4 = OHX04HTC, gender = RIAGENDR, race = RIDRETH1,
             age_months = RIDAGEMN, pir = INDFMPIR,
             psu = SDMVPSU, strata = SDMVSTRA, weight = WTMEC2YR
  ) %>%
  filter( !is.na(tooth4) & !is.na(age_months)) %>%
  mutate( gender = factor(gender, 1:2, c('male', 'female')))


## outcome variable
ohx = ohx %>%
  filter( tooth4 != 9 ) %>%
  mutate( y = tooth4 != 1)

#with(ohx, table(y))

## indicators for race/ethnic groups
#with(ohx, model.matrix(~ 0 + factor(race) )) ## 

ohx = ohx %>% 
  mutate( mexamer = 1*{race == 1},
          black = 1*{race == 4},
          other = 1*{race %in% c(2, 5)},
          white = 1*{race == 3}
  )

## (b) representative ages: ---------------------------------------------------

fit0 = glm( y ~ age_months , family = binomial(link = 'logit'), data = ohx)

with(ohx, table( is.na(y), is.na(age_months))) 
length(fit0$fitted)

b = coef(fit0)
log_odds = log( c(.25/.75, 1, .75/.25) )
key_ages = {log_odds - b[1]} / b[2]
rep_ages = seq( 12*floor( key_ages[1] / 12 ), 12*ceiling( key_ages[3] / 12), 12)

## (c) model selection: -------------------------------------------------------
fita = fit0

# Function and a list to record aspects of the fit
record_fit = function(fit){
  list( vars = attr( terms(fit0), "term.labels"), 
        bic = BIC(fit0),
        n = length(fit$fitted.values)
  )
}
fits = list()
fits[[1]] = record_fit(fit0)

## Gender Better? 
fitb = update(fita, ~ . + gender) 
BIC(fita, fitb)

fits[[2]] = record_fit(fitb)
if( BIC(fitb) < BIC(fita) ) fita = fitb

## Test race levels one by one
fitb = update(fita, ~ . + black)
BIC(fita, fitb)
fits[[3]] = record_fit(fitb)
if( BIC(fitb) < BIC(fita) ) fita = fitb

fitb = update(fita, ~ . + mexamer)
BIC(fita, fitb)
fits[[4]] = record_fit(fitb)
if( BIC(fitb) < BIC(fita) ) fita = fitb

fitb = update(fita, ~ . + other)
BIC(fita, fitb)
fits[[5]] = record_fit(fitb)
if( BIC(fitb) < BIC(fita) ) fita = fitb

## PIR better? Need to drop cases with missing PIR values to compare
if( !all( attr( terms(fita), 'term.labels' ) %in% c('age_months', 'black') ) ){
  cat('Update script to check additional interactions.\n')
}

fit1 = update(fit0, ~ . + black, data = ohx %>% filter(!is.na(pir)))
fitb = update(fit1, ~ . + pir)
BIC(fit1, fitb)

fits[[6]] = record_fit(fit1)
fits[[7]] = record_fit(fitb)
if( BIC(fitb) < BIC(fit1) ) fita = fitb

## Interactions? 
if( !all( attr( terms(fita), 'term.labels' ) %in% c('age_months', 'black') ) ){
  cat('Update script to check additional interactions.\n')
}
fitb = update(fita, ~ . + age_months:black)
BIC(fita, fitb)
fits[[8]] = record_fit(fitb)
if( BIC(fitb) < BIC(fita) ) fita = fitb

# (d) margins: ----------------------------------------------------------------

## Refit with black as factor to get desired behavior from margins in parts 2-3
fita_factor = update(fita, ~ . - black + factor(black))

# 1. Adjusted predictions at means for representative ages
adj_pred_at_means =  tibble( age_months = rep_ages, black = mean(ohx$black) ) 

adj_pred_at_means = cbind(
  adj_pred_at_means,
  as_tibble( predict(fita, adj_pred_at_means, type = 'response', se.fit=TRUE) )
)

#2. Marginal effects for black at representative ages: ------------------------

# Adjusted predictions for each level of black at representative ages
adj_pred_black = 
  tibble(
    age_months = rep( rep_ages, each = 2),
    black = factor(rep(0:1, length(rep_ages)) )
  )

adj_pred_black = bind_cols(adj_pred_black, 
  as_tibble( predict(fita_factor, 
                     adj_pred_black, type = 'response', se.fit = TRUE) )
)

# Marginal effects. Note: these are the differences in black==1 vs black==0 for
# each age. 

#adj_pred_black %>% 
#  group_by(age_months) %>% 
#  summarize(
#    fit = diff(fit)
#)

# Using Margins requires a factor class to get difference by default
me_black = margins(fita_factor, at = list(age_months = rep_ages))
#summary(me_black)


## Direct computation of marginal effects and delta method standard errors: ---

# Linear algebra for the point estimates
b = matrix( coef(fita), ncol = 1)
X = cbind(1, rep(rep_ages, each = 2), c(0, 1))
diff_mat = matrix(0, nrow(X)/2, nrow(X))
for(row in 1:{nrow(X)/2}) diff_mat[ row, {row-1}*2+1:2 ] = c(-1, 1)

est = diff_mat %*% plogis(X %*% b)

# Gradient of g(beta) = logistic(X1 * beta) - logistic( X2 * beta )
grad_logistic = function(X, b, diff_mat){
  l = X %*% b
  diff_mat %*% {X  * as.numeric( exp(l) / { 1 + exp(l)}^2 )}
}

# Stanard error for the differences at each age
se =  
  diag( sqrt( grad_logistic(X, b) %*% vcov(fita) %*% t(grad_logistic(X, b)) ) )

# 3. Average marginal effect for black at each age. In this instance this is
#    the same as the above because our model has only age and black in it. 
#    If it had pir in it we would need to compute a marginal effect for each
#    individual in the data set and then average the differnces. 

predict(fita, ohx %>% mutate(black = 1, age_months = 96), type = 'response') - 
  predict(fita, ohx %>% mutate(black = 0, age_months = 96), type = 'response')

# Here is an example of syntax for those with pir in the model: ---------------
ohx_pir = ohx %>% filter(!is.na(pir))

fit_test = glm( y ~ age_months + factor(black) + pir,
                family = binomial(link='logit'), data = ohx_pir)

# Margins command
me_test = margins(fit_test, at = list(age_months = rep_ages))
summary(me_test)

est3 = 
 predict(fit_test, ohx_pir %>% mutate(black = 1, age_months = 96), type = 'response') - 
 predict(fit_test, ohx_pir %>% mutate(black = 0, age_months = 96), type = 'response')

# Values we need predictions at
X = cbind(1, 
          age_months = rep(rep_ages, each = 2*nrow(ohx_pir)), 
          black = rep( rep( 0:1, length(rep_ages) ), nrow(ohx_pir) ),
          pir = rep( ohx_pir$pir, each = 2)
)
#head(X)

library(Matrix)
diff_mat = sparseMatrix( rep(1:{nrow(X)/2}, each = 2), 
                         rep({1:{nrow(X)/2}-1}*2, each = 2) +
                           rep(1:2, nrow(X)/2 ),
                         x = rep(c(-1, 1), nrow(X)/2),
                         dims = c(nrow(X)/2, nrow(X))
)
#pryr::object_size(diff_mat)
#dim(diff_mat)
diff_mat[1:2, 1:5]
b = coef(fit_test)
est3_hand = diff_mat %*% plogis(X %*% b)
#mean(est3_hand[1:7355]) == mean(est3[1:7355])


## We want to average the entries within
## each age group, not to compute all pairwise covariances. 

grad = grad_logistic(X, b, diff_mat)
r = nrow(grad) / length(rep_ages)

grad_sum = as.matrix(
  aggregate( as.matrix(grad), list( age = {{1:nrow(grad)}-1} %/% r ), mean)
  )[,-1]

se3 = sqrt( diag( grad_sum %*% vcov(fit_test) %*% t(grad_sum) ) )


# 80: --------------------------------------------------------------------------
