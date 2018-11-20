## Examples comparing the process ids when executing parallel code
## using mclapply or foreach.
##
## Updated: Nov 20, 2018

# load packages: --------------------------------------------------------------
library(parallel); library(doParallel); library(future)

# This function just returns the process id for the R sessions it is called in.
f = function(i) {  Sys.getpid() } 

# Serial computation
result = c()
for( i in 1:10 ){
  result[i] = f(i)
}
print(result)

unlist( lapply(1:10, f)  )

# Parallel computation using mclapply

## prescheduled on two cores
getOption("mc.cores", default = 2L)
unlist( mclapply(1:10, f)  ) 

# if repeated, we spin up new sessions
unlist( mclapply(1:10, f)  ) 

## presceduled on four-cores
unlist( mclapply(1:10, f, mc.cores = 4)  )

## not presceduled 
f = function(i) {  Sys.getpid() } 
unlist( mclapply(1:10, f, mc.preschedule = FALSE)  )

## Parallel computation using foreach with two processes
cl = makeCluster(2)
registerDoParallel(cl)
result = foreach(i = 1:10) %dopar% {
  f(i)
}
unlist(result)

## if repeated we resuse the same two processes
result2 = foreach(i = 1:10) %dopar% {
  f(i)
}
unlist(result2)

stopCluster(cl)

# Parallel computation using foreach with four processes
cl = makeCluster(4)
registerDoParallel(cl)

result3 = foreach(i = 1:10) %dopar% {
  f(i)
}
# Note that results may not be in the same order
unlist(result3)

## Understanding prescheduling and blocking
f_wait = function(i){ 
 Sys.sleep(i)  
 Sys.getpid()
}
f_wait(2)

tm4 = system.time({
  result4 = foreach(i = 1:12) %dopar% {
    f_wait(1/i)
  }
})
unlist(result4)
tm4

ind = as.numeric(as.factor( unlist(result4) ) )
for (k in 1:4){
  print( {1 / {1:12}}[1:12 == k] )
}  
tm4

## Prescheduled
system.time( {result5 = mclapply(1:12, function(x) f_wait(1/x), mc.cores = 4)} )
unlist(result5)

## Not prescheduled
system.time( {result6 = mclapply(1:12, function(x) f_wait(1/x), mc.cores = 4, 
                                 mc.preschedule = FALSE)} )
unlist(result6)

## Some code to help reason about the programs above. 
sum( {1 / { 1:12 }}[seq(1, 12, 2)] )
sum( {1 / { 1:12 }}[seq(2, 12, 2)] )
for ( k in 1:4) {
 print( sum( {1 / { 1:12 }}[seq(k, 12, 4)] ) )
}

