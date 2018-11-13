## An example R script for using command line arguments
##
## Author: James Henderson (jbhender@umich.edu)
## Date: October 25, 2017


## All R args
args = commandArgs()

typeof(args)
length(args)
print(args)

## Trailing args
args2 = commandArgs(trailingOnly=TRUE)
typeof(args2)
length(args2)
print(args2)