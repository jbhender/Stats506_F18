# Problem Set 3, Question 2 Solution
# Stats 506, Fall 2018
#
# James Henderson
# Updated: Nov 21, 2018

# libraries: ------------------------------------------------------------------
library(data.table)

# Parameters: -----------------------------------------------------------------
n = 1e3; p = 1e2; r = .1; rho = -.1
beta = c( rep(.1, floor(r*p)), rep(0, p - floor(r*p)) ) 
dim(beta) = c(p, 1)

# X ~ N(0, Sigma): -----------------------------------------------------------
Sigma = p*diag(p)/2

# Ensure it is symmetric, then rescale to give variances all equal to one
Sigma[lower.tri(Sigma)] = rho
Sigma = {Sigma + t(Sigma)} / p
R = chol(Sigma)

# Here is an X for testing: ---------------------------------------------------
X = matrix( rnorm(n*p), n, p) %*%  R

# Part a, Monte Carlo simulation function for fixed X: ------------------------
sim_beta = function(X, beta, sigma = 1, mc_rep = 1e3){
 # Simulate Y from Y|X ~ N(XB, sigma^2 I) and compute p-values corresponding to
 # Wald tests for B != 0. Repeat mc_rep times.
 #
 # Arguments:
 #   X : an n by p numeric matrix
 #   beta: a p by 1 numeric matrix
 #   sigma: std deviation for Y|X,  Y|X ~ N(XB, sigma^2 I)
 #   mc_rep: The number of Monte Carlo replications to use
 #
 # Output: A p by mc_rep matrix of p-values

  # This part doesn't need to change for each replication
  QR = qr( crossprod(X) )
  QX = X %*% qr.Q(QR) 
  XtXinv = solve( qr.R(QR), t( qr.Q(QR) ))
  
  n = nrow(X)
  p = ncol(X)
    
  # Generate mc_rep copies of Y at once, each in a column.
  Y = as.numeric(X %*% beta) + rnorm(n*mc_rep)
  dim(Y) = c(n, mc_rep)

  # estimate betas and residual standard errors
  b = solve(qr.R(QR), crossprod( QX, Y ) )
  
  # It's okay if you divide by {n - p} outside the sum, but this
  # is more comparable to what is done by .lm.fit()
  s_sq = colSums( {Y - as.numeric(X %*% b)}^2 / {n - p})
  
  # standard error of b
  v = sqrt( diag(XtXinv) * rep(s_sq, each = p) )

  # return a matirx of p-values
  # Use pt to replicate lm, but the normal approximation is fine here. 
  matrix( 2*pt( abs( b / v ), df = {n-p}, lower.tail = FALSE ), p, mc_rep )  
}

## Here is a test comparing to "lm"
set.seed(42)
P1 = sim_beta(X, beta, sigma = 1, mc_rep = 1)

set.seed(42)
Y = as.numeric(X %*% beta) + rnorm(n*1)
dim(Y) = c(n, 1)
fit = lm(Y ~ 0 + X)
dplyr::near( summary(fit)$coefficients[,4], P1[,1] )
#plot(summary(fit)$coefficients[,4], P1[,1])
#abline(0, 1)

# Part b, test for a specific sigma and Sigma: --------------------------------
P = sim_beta(X, beta, sigma = 1, mc_rep = 1e4)

# Part c, evaluate: -----------------------------------------------------------
evaluate = function(P, tp_ind, alpha = .05){
  P = P < alpha

  p = nrow(P)
  n = ncol(P)
  
  # Compute TP, FP, TN, FN for each replcation
  TP = colSums(P[tp_ind, ])
  FP = colSums(P[-tp_ind,])
  TN = colSums(!P[-tp_ind,])
  FN = colSums(!P[tp_ind,])
  
  # Call FDR 0 when no discoveries. 
  P = FP + TP
  fdr = ifelse(P > 0, FP  / {FP + TP}, 0)
  fwer = mean( FP > 0 )
  sens = TP / {TP + FN}
  spec = TN / {FP + TN}
  
  list( fwer = fwer, fwer_se = sqrt(fwer*{1-fwer} / n), 
        fdr =  mean(fdr), fdr_se = sd(fdr) / sqrt(n),
        sens = mean(sens), sens_se = sd(sens) / sqrt(n),
        spec = mean(spec), spec_se = sd(spec) / sqrt(n)
        )
}

# part d, compare hypothesis tests with adjusted p-values: --------------------
evaluate(P, 1:10)
all0 =
lapply( c('holm', 'bonferroni', 'BH', 'BY'), function(x){
  evaluate( apply(P, 2, p.adjust, method = x), tp_ind = 1:10)
})
all = rbindlist(all0)
all[ , method := c('holm', 'bonferroni', 'BH', 'BY') ]

m = qnorm(.975)
ci = list()
for(x in c('fwer', 'fdr', 'sens', 'spec')){
  ci[[x]] = all[ , .(est = .SD[[x]], se = .SD[[paste0(x,'_se')]])]
  ci[[x]][, ci := sprintf('%5.3f (%5.3f, %5.3f)', est, est-m*se, est+m*se) ]
}

ps3_q2result = as.data.table(do.call('cbind', lapply(ci, function(x) x[,ci]) ))
ps3_q2result[ , method := c('Holm', 'Bonferroni', 'BH', 'BY') ]
