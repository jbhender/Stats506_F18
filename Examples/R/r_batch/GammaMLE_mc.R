## Monte Carlo study of MLE for Gamma distribution
## 
## Designed to be called from the command line with command line args:
##   cores (for computation), mcrep (Monte Carlo runs), n (sample size), shape and rate (parameters)
##
## To run with default parameters: Rscript GammaMLE_mc.R 
##
## To pass parameters from the command line, use --args:
##  
##    Rscript GammaMLE_mc.R --args cores=2 mcrep=1e3 n=10 shape=1 rate=2
##
## Author: James Henderson (jbhender@umich.edu)
## Date: Oct 26, 2017

# if needed
#.libPaths('~/Rlib')

# libraries
library(parallel)

# default arguments
args_list = list(
  cores=1,
  mcrep=1e3,
  n=1e2,
  shape=1,
  rate=1
)

## get parameters from command line
args = commandArgs(trailingOnly = TRUE)
print(args)

# functions for finding named arguments
args_to_list = function(args){
  ind = grep('=', args)  
  args_list = strsplit(args[ind], '=')
  names(args_list) = sapply(args_list, function(x) x[1])

  args_list = lapply(args_list, function(x) as.numeric(x[2]))
  args_list
}

# get named arguments
args_list_in = args_to_list(args)

# update non default arguments
ignored = c()
for ( arg in names(args_list_in) ) {
 # Check for unknown argument
 if ( is.null(args_list[[arg]]) ) {
    ignored = c(ignored, arg)
 } else{
   # update if known
   args_list[[arg]] = args_list_in[[arg]]
 }
}

# Print warning message about unknown arguments
if ( length(ignored) > 0 ) {
  cat('Ignoring unkown arguments:',paste(ignored,collapse=', '), '\n')
}

# -----------------------------------------------------------------------------
# Define functions for simulation

# Log likelihood for Gamma distribution

# -log likilhood
neg_loglik_gamma = function(par, x){
  # par should be a named vector with shape and rate parameters
  -sum( dgamma(x, par["shape"], par["rate"], log=TRUE) )
}

# mle optimization
mle_gamma= function(x, rate=1){

  # Initial values
  shape = rate / mean(x)
  par = c(shape=shape, rate=rate)

  # Optimization
  opt = optim( par, function(theta) neg_loglik_gamma(theta, x) )
  
  if ( opt$convergence == 0) {
    return(opt$par)
  } else {
    return( c(shape=NA, rate=NA) )
  }
}

# simulation function
sim_mle_gamma = function(n, shape, rate){
   mle_gamma( rgamma(n, shape, rate) )
}
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Do the computations
results = 
  with(args_list,
    mclapply(1:mcrep, function(i) sim_mle_gamma(n, shape, rate),
         mc.cores=cores)
  )


est = do.call(rbind, results)
bias = with(args_list, colMeans(est) - c(shape, rate) )
varn = apply(est, 2, var)
mse  = bias^2 + varn
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Print messages about the results
shape_msg = sprintf('shape: bias = %4.3f, std dev = %4.3f, RMSE = %4.3f\n', 
              bias['shape'], varn['shape'], mse['shape']
)
rate_msg = sprintf(' rate: bias = %4.3f, std dev = %4.3f, RMSE = %4.3f\n', 
               bias['rate'], varn['rate'], mse['rate']
)

# Report results to stdout
cat('arguments:', paste(names(args_list), unlist(args_list), sep='=', collapse=', '), '\n')
cat(shape_msg, rate_msg, sep='')

