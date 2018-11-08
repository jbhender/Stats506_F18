#!/bin/bash

## This script converts the data stored in iris.csv.gz
## to a bucketized, columnar format with:
##  buckets determined by the value of the "species" variable
##  and columns represented
## Data is stored in the following data hierarchy:
##   iris/species/col.csv.gz
## Where `iris/species/col.csv.gz` contains all data for
## the column "col" from the subset of rows matching
## species "species" in a gzip compressed format.

wd0=$(pwd)
file=iris.csv.gz

## Get the unique values of species ignoring the header
species=$(gunzip -c $file | tail -n+2 | cut -f5 -d, | sort | uniq)

## Get the values of the column headers
cols=$(gunzip -c $file | head -n1 | cut -f'1-4' -d,)

## Loop over species
for spec in $species
do
    echo $spec
    if [ ! -d $wd0/$spec ]
    then
	mkdir -f $wd0/$spec
    fi

    ## Loop over column names "col" and keep a counter "k"
    k=0
    for col in $(echo $cols | sed "s/,/ /g")
    do
	k=$(($k + 1))
	# Find rows for species "spec", extract column "col" (using k), 
        # redirect, and then compress.
        colfile=$wd0/$spec/$col.csv
	gunzip -c $file | grep -e$spec | cut -f$k -d, > $colfile && gzip -f $colfile
    done
done

