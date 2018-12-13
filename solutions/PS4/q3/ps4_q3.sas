/* Find MRI procedures with the highest volume, total payments, and 
 * average payments using 2016 Medicare public use files.
 * 
 * PS4, Q3
 * Stats 506, F18
 *
 * Original Data
 * from: https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Physician-and-Other-Supplier.html
 * download at: https://www.cms.gov/apps/ama/license.asp?file=https://downloads.cms.gov/files/Medicare-Physician-and-Other-Supplier-PUF.zip
 *
 * James Henderson (jbhender@umich.edu)
 * Nov 30, 2018
 */

/* 80: ---------------------------------------------------------------------- */

/* Library: ----------------------------------------------------------------- */
libname saslib '/home/jbhender/ps4q3/data';

/* a: See ps4_q3_import.sas.

/* b: reduce to MRI procedures using hcpcs_description and hcpcs_code starting
 *    with 7: --------------------------------------------------------------- */

data saslib.mri;
 set saslib.Medicare_PS_PUF;
 where prxmatch("/^7/",  hcpcs_code) and
       prxmatch("/MRI/", hcpcs_description); 
run; 


/* c: Find highest volume, highest total payment, and highest avg payment: -- */

/* create total provider level payment */
data work.mri;
 set saslib.mri;
 totpay = line_srvc_cnt*average_medicare_payment_amt;

/* create summarized payment data */
proc summary data=work.mri;
 class hcpcs_code;
 id hcpcs_description;
 output out=mri_pay
  sum(line_srvc_cnt)=volume
  sum(totpay)=totpay;

/* drop aggreggate row and compute average payment */
data work.mri_pay;
 set work.mri_pay; 
 avgpay = totpay / volume;
 where  _type_ = 1; 


/* Create copies sorted in ascending order for each variable */
proc sort data=work.mri_pay out = work.avgpay; 
  by descending avgpay; 

proc sort data=work.mri_pay out=work.totpay;
 by descending totpay; 

proc sort data=work.mri_pay out=work.volume;
 by descending volume; 

/* Combine the first observation of each table */
data work.mri_max;
 set work.totpay(obs=1)
     work.volume(obs=1)
     work.avgpay(obs=1);

/* sort by hcpcs_code and remove duplicates */
proc sort data=work.mri_max nodup;
 by hcpcs_code;

/* Print results to listing file */ 
title "Most used and highest cost MRI procedures"; 
proc print data=work.mri_max;
 var hcpcs_code hcpcs_description volume totpay avgpay;
run; 

/* Export to csv */
proc export data=work.mri_max(drop=_TYPE_ _FREQ_)
 outfile = '/home/jbhender/ps4q3/data/ps4q3_c.csv' 
 dbms=dlm replace;
 delimiter = ',';
run; 

/* d, repeat b-c using proc sql: -------------------------------------------- */

proc sql;

 create table work.mri_pay_sql as
  select hcpcs_code, hcpcs_description, sum(line_srvc_cnt) as volume, 
         sum(line_srvc_cnt*average_medicare_payment_amt) as totpay
  from saslib.Medicare_PS_PUF
  where 
    hcpcs_code like '7%' and
    hcpcs_description like '%MRI%'
  group by hcpcs_code, hcpcs_description
  ; 

  create table work.mri_max_sql as 
   select hcpcs_code, hcpcs_description, 
          volume, totpay, avgpay
   from (select *, totpay/volume as avgpay
         from work.mri_pay_sql )
   having volume = max(volume) or 
          totpay = max(totpay) or
          avgpay = max(avgpay)
  ; 

quit;
run;

/* Print results to listing file */
title "Most used and highest cost MRI procedures (SQL)";
proc print data=work.mri_max_sql; 

/* Export to csv */
proc export data=work.mri_max_sql
 outfile = '/home/jbhender/ps4q3/data/ps4q3_d.csv'
 dbms=dlm replace;
 delimiter = ',';
run;

/* 80: ---------------------------------------------------------------------- */  