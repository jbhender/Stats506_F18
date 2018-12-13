## Problem Set 4, Question 2b
## Stats 506, Fall 2018
##
## Repeat the Monte Carlo simulation from problem set 3 for different
## correlation structures of the covariate matrix X.
##
## James Henderson, Dec 13, 2018

# libraries: ------------------------------------------------------------------
library(data.table); library(doParallel); library(doRNG)

# sources: --------------------------------------------------------------------
source('./ps4_q2_funcs.R')

# parameters: ----------------------------------------------------------------
n = 1e3
p = 1e2
r = .1
beta = c( rep(.1, floor(r*p)), rep(0, p - floor(r*p)) ) 
dim(beta) = c(p, 1)
sigma = 1
mc_rep = 1e4
alpha = .05
ncores = 4

rho_seq = seq(-.75, .75, .25)
sigma_seq = c(.25, .5, 1)

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


if ( FALSE ){
  # Test sim_X 
  X = sim_X(1e3, beta, .75*10)
  s_hat = cor(X)
  x11 = s_hat[1:10, 1:10][lower.tri(s_hat[1:10,1:10])]
  x00 = s_hat[11:100, 11:100][lower.tri(s_hat[11:100,11:100])]
  x01 = s_hat[11:100,][,1:10]
  boxplot(x00, x01, x11)
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

## Setup the cluster
cl = makeCluster(ncores)
registerDoParallel(cl)

set.seed(1984)
# Here is a nested for loop using %dopar%, as requested 
results = 
 foreach(rho=rho_seq, .packages = 'data.table', .combine = 'rbind') %:%
   foreach(sigma=sigma_seq, .combine = 'rbind') %dopar% 
  {
      do_sim(n, beta, rho, sigma, mc_rep, alpha )
  }

# Here is a reproducible version using %dorng% 
if ( FALSE ) { # set to TRUE to run
 pars = data.table( rho = rho_seq, sigma = sigma_seq)

 set.seed(1984)
 results_dorng = 
  foreach(ell=1:nrow(pars), .packages = 'data.table', .combine = 'rbind') %dorng% 
  { 
    rho = pars$rho[ell]
    sigma = pars$sigma[ell]
    do_sim(n, beta, rho, sigma, mc_rep, alpha )
  }
} # ends if (FALSE)

# Save results: ---------------------------------------------------------------
file = './ps4_q2b.RData'
save(results, file = file)

proc.time()