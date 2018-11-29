/*****************************************************
An example SAS program for Stats 506.

This file reads the RECS data from:
 ./data/recs2009_public_v4.sas7bdat
 http://www.eia.gov/consumption/residential/data/2009/index.cfm?view=microdata

Then runs the proc contents procedure.

Author: James Henderson (jbhender@umich.edu)
Date: Nov 28, 2018
 *****************************************************
*/

/* data library for reading/writing data */
libname mylib '~/Stats506_F18/Examples/SAS/data/';

/* Create a data set recs referring to existing file */
data recs;
 set mylib.recs2009_public_v4;  

proc contents data=recs;

proc print data=mylib.recs2009(obs=5);
 var DOEID;

proc print data=recs(obs=5);
 var DOEID;

run;

