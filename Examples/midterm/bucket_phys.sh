#!/bin/bash

# Split Medicare Provider Utilization Files from 2014-2015 
# by row into buckets based on the provider type column. 
#
# Also eliminate columns which contain 
# provider information other than the unique identifier
# for the NPI number.  
#
# Notes: 
#   1. These are tab delimted so use cut -d$'\t' to extract columns.
#   2. We have files named $stemYYYY.txt in the current directory and
#      a sub directory "./buckets/" where all new files are created.
#   3. Some provider types will still need manual pairing as names
#      change over the years included. 

# These are the fields we want to keep for the new files
fields=1,14-26      

# Files to be split are of the form: $stem$year.txt
stem=Medicare_Provider_Util_Payment_PUF_CY 

# Loop over years
for year in `seq -w 2014 2016`
do

 # This is the file to be split
 file=$stem$year.txt
 
 # Get the unique provider types (column 14)
 # but ignore the first two rows (header & copyright)
 < $file cut -f14 -d$'\t' | tail -n+3 | sort | uniq > provider_types_$year.txt
 
 # Clean up provider types to make better file names
 sed -e 's/[^A-Za-z0-9._-]/_/g' provider_types_$year.txt > provider_types_clean_$year.txt
 # Replace multiple _ with a single _
 sed -i -r -e 's/[_{2,}]/_/g' prov_types_$year.txt    

 # Create a two-column file with original type and clean type
 # and then remove the originals. 
 paste provider_types_$year.txt provider_types_clean_$year.txt > prov_types_$year.txt
 rm provider_types*_$year.txt 

 # Loop over unique provider types listed as rows in the file just created.
 # The iterator variables below refer to the values in its two columns: 
 #  type contains PROVIDER_TYPE as it appears in the original file
 #  type_clean is the clean version for file naming. 
 while IFS=$'\t' read -r type type_clean
 do

   # Create new provider type bucket if needed
   if [ ! -d $type_clean ]; then
       mkdir ./buckets/$type_clean
   fi

   # Extract rows mathching provider type $type
   # and store in correct folder/bucket

   new_file=./buckets/$type_clean/phys_puf_$year-min.txt
   echo $new_file

   < $file cut -f$fields -d$'\t' | grep -e"$type" > $new_file  

   # Compress this new file
   gzip $new_file

   echo Finished $col1 for year $year.

 done < prov_types_$year.txt

echo Finished $year.
done

