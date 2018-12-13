## Problem Set 4, Question 2c
## Stats 506, Fall 2018
##
## Repeat the Monte Carlo simulation from problem set 3 for different
## correlation structures of the covariate matrix X.
## 
## This script reads the following arguments from the command line:
##   sigma, mc_rep, n_cores
## Call Rscript ps4_q2c.R sigma mc_rep n_cores
##
## James Henderson, Dec 13, 2018

# libraries: ------------------------------------------------------------------
library(data.table); library(doParallel); library(future)

# sources: --------------------------------------------------------------------
source('./ps4_q2_funcs.R')

# parameters: ----------------------------------------------------------------
n = 1e3
p = 1e2
r = .1
beta = c( rep(.1, floor(r*p)), rep(0, p - floor(r*p)) ) 
dim(beta) = c(p, 1)
alpha = .05

rho_seq = seq(-.75, .75, .25)

## from command line
args = commandArgs(trailingOnly=TRUE)
if ( length(args) != 3 ){
 stop( "Please provie three command line arguments: sigma mc_rep n_cores")
}

sigma = as.numeric( args[1] )
mc_rep = as.numeric( args[2] )
n_cores = as.numeric( args[3] )

# new functions: -------------------------------------------------------------
sim_X = function(n, beta, rho){
   p = length(beta)
   
   # Covariance of X: rho*beta*beta'
   S = rho*beta %*% t(beta)
   diag(S) = 1
   R = chol(S)
   
   X = rnorm(n*p)
   dim(X) = c(n,p)
   
  X %*% R
}

do_sim = function(n, beta, rho, sigma, mc_rep, 
                  alpha = .05, methods = c('holm', 'bonferroni', 'BH', 'BY') ){
  # Arguments
  #  n
  #  beta
  #  rho
  #  sigma
  #  mc_rep
  #  alpha 
  #  methods
  
  # Simulate X
  X = sim_X(n, beta, rho)
  
  # Simulate Y, beta_hat, and pvalues
  P = sim_P(X, beta, sigma, mc_rep)
  
  # Evaluate for each method
  tp_ind = which( beta != 0)
  all0 =
    lapply( methods, function(x){
      evaluate( apply(P, 2, p.adjust, method = x), tp_ind = tp_ind)
    })
  all = rbindlist(all0)
  all[ , method := methods]
  
  # Reshape to long
  metrics = c('fwer', 'fdr', 'sens', 'spec')
  
  # Estimates
  est = 
    melt(all, id.vars = 'method', measure.vars = metrics, 
       variable.name = 'metric', value.name = 'est')
  
  # Standard errors
  se =
    melt(all, id.vars = 'method', measure.vars = paste(metrics, 'se', sep='_' ),
       variable.name = 'metric', value.name = 'se')
  
  # Replace metric_se with just metric using regex
  se[ , metric := stringr::str_extract(metric, '^[^_]+')]

  out = merge(est, se, by = c('method', 'metric'))
    
  # Add parameters to results matrix and return
  out[, `:=`(sigma = sigma, rho = rho)][]
  
  out
}

## Test
#out = do_sim(n, beta, rho=.75, sigma=1, mc_rep = 1e3, alpha = .05)

# Simulations: ----------------------------------------------------------------
RNGkind(kind = "L'Ecuyer-CMRG")

## Use futures to run asynchronously
plan(cluster, workers = n_cores)

result_list = list()
for ( rho in rho_seq ){
 result_list[[paste(rho)]] = future({  do_sim(n, beta, rho, sigma, mc_rep, alpha ) })
}

results = rbindlist( lapply(result_list, value) )

# Save results: ---------------------------------------------------------------

file = sprintf('./ps4_q2c_sigma%4.2f.RData', sigma)
save(results, file = file)

proc.time()