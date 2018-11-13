## An example R script for running in batch mode
##
## 1. Run this script using: R CMD BATCH my_script1.R
##    Notice the creation of ".RData" using: ls -a 
##
## 2. Run: R CMD BATCH my_script0.R again and notice what changes
##    
## 3. Repeat 2 with: R CMD BATCH --no-restore my_script0.R
##
## 4. Remove .RData: rm .RData
##
## 5. Repeat 1 with: R CMD BATCH --no-save my_script1.R 
##
## 6. Repeat 1 with: R CMD BATCH --vanilla my_sript1.R
##
## Author: James Henderson (jbhender@umich.edu)
## Date: October 25, 2017

data(Loblolly)

