## Gamma MLE script for testing in perpreation for Monte Carlo Study
##
## Author: James Henderson (jbhender@umich.edu)
## Date: Oct 26, 2017

# libraries
library(parallel)

## parameters
cores = 2
mcrep = 1e2
n = 100
shape = 2
rate = 3

# Random draws
x = rgamma(100, shape, rate)

# Log likelihood for Gamma distribution
neg_loglik_gamma = function(par,x){
  # par should be a named vector with shape and rate parameters
  -sum(dgamma(x,par["shape"], par["rate"], log=TRUE))
}

# Test example
par = c(shape=shape, rate=rate)
neg_loglik_gamma(par,x)

mle_gamma= function(x, rate=1){

  # Initial values
  shape = rate / mean(x)
  par = c(shape=shape, rate=rate)

  # Optimization
  opt = optim(par, function(theta) neg_loglik_gamma(theta, x))
  
  if(opt$convergence == 0){
    return(opt$par)
  } else{
    return(c(shape=NA, rate=NA))
  }
}

# Test MLE
mle_gamma(x)

# simulation function
sim_mle_gamma = function(n, shape, rate){
   mle_gamma(rgamma(n, shape, rate))
}

# test simulation function
sim_mle_gamma(n, shape, rate)

# Test MC loop
results =
  mclapply(1:mcrep, function(i) sim_mle_gamma(n, shape, rate),
         mc.cores=cores)

# Test comparisons computations
est = do.call(dplyr::bind_rows, results)

bias = colMeans(est) - c(shape, rate)
varn = apply(est, 2, var)
mse  = bias^2 + varn
