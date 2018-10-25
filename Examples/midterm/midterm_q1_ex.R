## Midterm Question 1 follow up examples
##
## Lapply, lists, and with using mtcars from the base datasets package
## 
## October 25, 2018

# Iterating over lists: ------------------------------------------------------

## Recall that the data.frame class is a list with additional features.

# We can apply a function to all columns of a data.frame using lapply:
lapply(mtcars, function(x) { ifelse(is.numeric(x), mean(x), class(x) ) } )

# sapply will attempt to simplify the list if all all elements have the same
# length
sapply(mtcars, function(x) { ifelse(is.numeric(x), mean(x), class(x) ) } )

# vapply is a safer but less flexible version of sapply that requires you to
# state the return type of your function  using a prototype
vapply(mtcars, mean, FUN.VALUE = double(1))

# We can also use these functions with atomic vectors as inputs
# Here we us an anonymous rather than a name function. 
lapply(seq(1, length(mtcars)), function(i) mean(mtcars[,i]) )
sapply(seq(1, length(mtcars)), function(i) mean(mtcars[,i]) )
vapply(seq(1, length(mtcars)), function(i) mean(mtcars[,i]),  
       FUN.VALUE = double(1))

# lapply, sapply, and vapply are "functionals" or functions that take another
# function as input.

# Notice that all of these functions involve `...` in there arguments. 
# Use this to pass additional named agruments to named functions you wish to use. 
#?quantile
sapply(mtcars, quantile, probs = c(.25, .5, .75) )
vapply(mtcars, quantile, probs = c(.25, .5, .75), FUN.VALUE = double(3) )

# Remember "double" is a type and not a class. 
class(double(3))

# Using "with" to construct an environment: -----------------------------------

# Example object
mtcars_quartiles = 
  vapply(mtcars, quantile, probs = c(.25, .75), FUN.VALUE = double(2) )

# The assignment above ("=") binds the name "mtcars_quartiles" to the data 
# returned by the vapply function.  This name only has meaning in the global
# environment (and therefore also in any environment nested within it.) 
# This is why we can refer to it using an unquoted name.
mtcars_quartiles

# We can't however, refer to columns in mtcars without specifying where to look.
if(FALSE){
  mpg
}
mtcars$mpg

# We can, however use the function `with()` to construct an environment containing
# the columns (i.e. list entries) of mtcars.
with(mtcars, cyl)
with(mtcars, quantile( mpg, probs = c(.25, .75)) )
# This also works with an arbitrary list: 
with(lapply(mtcars, quantile, probs = c(.25, .75)), mpg )

## Midterm Question 1: --------------------------------------------------------
start = list(x = TRUE, y = 'two', z = floor(pi) )
lapply(start, class)
start$xyz = with(start, c(x, y, z) )

