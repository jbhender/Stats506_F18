# Define an S4 class for principal components analysis
# and associated methods.

# Define the class representation: ---------------------------------------------
pca = setClass("pca", 
               slots = c(scores="matrix", 
                         loadings="matrix", 
                         singular_values="numeric")
)

# Define a constructor function: ----------------------------------------------
pca = function(x){
  stopifnot(is.numeric(x) & is.matrix(x))
  x_svd = svd(x)
  new("pca", scores = x_svd$u, loadings = x_svd$v, singular_values = x_svd$d)
}

# Define a show method: -------------------------------------------------------
setMethod("show", "pca", function(object){
  cat( sprintf(
    "Principal component decomposition from SVD of %i by %i matrix. \n", 
    nrow(object@scores), ncol(object@loadings) 
  ) )
})

# Define a summary method: ----------------------------------------------------
setMethod("summary", "pca", 
 function(object, max_vars = 2){
   ve = 100*object@singular_values^2 / sum(object@singular_values^2)
   show(object)
   cat('Component | Variance Explained | Cumulative\n')
   cat('_________ | __________________ | __________\n')
   for( i in 1:length(ve) ){
     if(i > max_vars) break
       cat( sprintf('%9i |', i) )
       cat( sprintf('%18.1f%% |', ve[i] ) )
       cat( sprintf('%10.1f%%\n', sum(ve[1:i]) ) )
   }
 }
)

# Create a new generic function "scree": --------------------------------------
setGeneric("scree", function(object, ...){
  standardGeneric("scree")
})

# Define a scree method for the class "pca": ----------------------------------
setMethod("scree", "pca", 
 function(object, type = 'o', pch = 15, las = 1,
          xlab = 'component', ylab = 'eigenvalue', ...){
  eig = object@singular_values^2
  plot(1:length(eig), eig, type = type, pch = pch, las = las, 
       xlab = xlab, ylab = ylab, xaxt='n', ...)
  axis(1, at=1:length(eig))
 }
)

# Tests: ----------------------------------------------------------------------
if ( FALSE ){
  
  # Example input data
  n = 10; p = 4
  x = rnorm(n*p)
  dim(x) = c(n, p)
  
  rho = .8
  sigma = rep(rho, p*p)
  dim(sigma) = c(p, p)
  diag(sigma) = 1
  x = x %*% chol(sigma)
  
  # Test constructor and methods
  xpca = pca(x)
  show(xpca)
  summary(xpca, max_vars = 3)
  scree(xpca)
  
}
