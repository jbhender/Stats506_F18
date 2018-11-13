#!/bin/sh

for shape in `seq 3 5`; do
   Rscript /home/jbhender/r_batch/GammaMLE_mc.R \
     cores=2 mcrep=1e3 n=50 shape=${shape} rate=2 \
     >> GammaMLE_mc_rate.out
done
