# Define an S3 class for principal components analysis
# and associated methods using the pcaS4 class as a reference.

# Define a constructor function: ----------------------------------------------
pca = function(x){
  stopifnot(is.numeric(x) & is.matrix(x))
  x_svd = svd(x)
  
  #! <a> construct a list with the same names 
  #new("pca", scores = x_svd$u, loadings = x_svd$v, singular_values = x_svd$d)
  # Note: scores are usually defined as "x_svd$u %*% diag(x_svd$d)" which 
  #       scales each column by the corresponding entry in d. 
  out = list(
    scores = x_svd$u, loadings = x_svd$v, singular_values = x_svd$d
  )
  #! <b> change the class attribute of out to "pca"
  class(out) = 'pca'
  
  # Return
  out
}

# Define a print method equivalent to the S4 "show" method: -------------------
#setMethod("show", "pca")
#! <c> Give the correct name for the function
#A: The pattern is "generic"."class". The comment specifies a "print" method.
print.pca <- function(object){
  cat( sprintf(
    "Principal component decomposition from SVD of %i by %i matrix. \n", 
    #nrow(object@scores), ncol(object@loadings) 
    #! <d> Provide the the equivalent values
    #A: You just need to replace @ with $
    nrow(object$scores), ncol(object$scores)
  ) )
}

# Define a summary method: ----------------------------------------------------
#setMethod("summary", "pca", 
#! <e> Give the correct name for the function
#A: Same pattern, new method.

summary.pca <- function(object, max_vars = 2){
   #ve = 100*object@singular_values^2 / sum(object@singular_values^2)
   #! <f> 
   #A: Again, just replace @ with $. You can use x_svd here as it has no
   # meaning in this scope.

   ve = 100*object$singular_values^2 / sum(object$singular_values^2)
   #show(object)
   #! <g> 
   #A: Also acceptable, print.pca(object).
   print(object)
   
   cat('Component | Variance Explained | Cumulative\n')
   cat('_________ | __________________ | __________\n')
   for( i in 1:length(ve) ){
     if(i > max_vars) break
       cat( sprintf('%9i |', i) )
       cat( sprintf('%18.1f%% |', ve[i] ) )
       cat( sprintf('%10.1f%%\n', sum(ve[1:i]) ) )
   }
 }


# Create a new generic function "scree": --------------------------------------
#setGeneric("scree", function(object, ...){
#  standardGeneric("scree")
#})
#! <h> Do as the comment says
#A: All S3 generics are defined in the same way.  You should include the ...
# but I did not mark you off if not.

scree = function(object, ...){
  UseMethod("scree")
}


# Define a scree method for the class "pca": ----------------------------------
#setMethod("scree", "pca", 
#! <i> Give the correct name for the function

scree.pca <- function(object, type = 'o', pch = 15, las = 1,
          xlab = 'component', ylab = 'eigenvalue', ...){
  # eig = object@singular_values^2
  #! <j> Create an equivalent vector "eig"
  eig = object$singular_values^2
  
  plot(1:length(eig), eig, type = type, pch = pch, las = las, 
       xlab = xlab, ylab = ylab, xaxt='n', ...)
  axis(1, at=1:length(eig))
 }

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
  print(xpca)
  summary(xpca, max_vars = 3)
  scree(xpca)
  
}
