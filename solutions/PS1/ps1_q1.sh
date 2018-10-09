#!/bin/bash

# A short script with solutions to
# question one from problem set one
# for Stats 506, Fall 2018.
#
# The script assumes the dataset:
#  recs2015_public_v3.csv 
# is in the same directory as this
# script is called from.
#
# Updated: Sep 29, 2018
# Author: James Henderson 


# Get the data, if needed:
## This was not required in your solution.
if [ ! -f recs2015_public_v3.csv ]; then
 wget https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
fi

###########
# Part A, i
###########

echo -n Observations for region 3:
< recs2015_public_v3.csv cut -d, -f2 | grep -e "3" | wc -l

############
# Part A, ii
############

# From the code book, we see the desired variables are in 
# columns or "fields" 1, 475-571

# Check that this is right ...
# head -1 recs2015_public_v3.csv | cut -d, -f 1,474-570

# Extract these and then compress if successful.
if [ ! -f recs2015_weights.csv.gz ]; then
  < recs2015_public_v3.csv cut -d, -f 1,475-571 > recs2015_weights.csv \
   && gzip recs2015_weights.csv
fi

###########
# Part B, i  
###########

echo "Observations for each region"
for r in `seq 4`
do 
 echo -n "$r: "
 < recs2015_public_v3.csv cut -d, -f2 | grep -e $r | wc -l
done

############
# Part B, ii
############

# First get the headers
< recs2015_public_v3.csv head -1 | cut -d, -f2-3 > region_division.txt 

# Now get the unique sorted values
< recs2015_public_v3.csv tail -n +2 | cut -d, -f2-3 | sort -t'"' -k4 -n | uniq >> region_division.txt
