/*****************************************************
An example SAS program for Stats 506.

This file reads the RECS data from:
 ./data/recs2009_public.sas7bdat
 http://www.eia.gov/consumption/residential/data/2009/index.cfm?view=microdata

Then demonstrates sevaral procs for descriptive statistics:
 proc means, proc summary, proc freq
 Not shown here but similar: proc univariate

Author: James Henderson (jbhender@umich.edu)
Date: Nov 28, 2018
 *****************************************************
*/

/* data library for reading/writing data */
libname mylib '~/Stats506_F18/Examples/SAS/data/';

/* create a short name for recs data */
data recs;
 set mylib.recs2009_public_v4;


/* proc means */
proc means data=recs;
 var cdd65; 
 class regionc;

/* proc sort for use with 'by' in proc means */
proc sort data=recs;
 by regionc;

proc means data=recs;
 var cdd65;
 by regionc;

/* twice stratified using "by" */
proc sort data=recs;
 by regionc ur;

proc means;
 var cdd65;
 by regionc ur;

proc means;
 var cdd65 hdd65;
 by regionc ur; 

proc means;
 var cdd65 hdd65;
 class ur;
 by regionc; 

/***********
 PROC FREQ
 ***********
*/ 

proc freq data=recs;
  tables occupyyrange / out=mylib.occupyrange_freq;  

proc print data=occupyrange_freq; 

/************
 PROC SUMMARY
 ************
*/

proc summary data=recs;;
 class regionc;
 output out=meanstats
   mean(kwh) = mean_kwh
   std(kwh) = std_kwh;

proc print data=meanstats;

run;