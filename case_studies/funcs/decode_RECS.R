## Functions for decoding variables in the 2009 "RECS" data.
##
## Author: James Henderson (jbhender@umich.edu)
## Updated: Sep 22, 2018

# Functions to decode states or reportable_domains from RECS.

# decode_state returns a single value
decode_state = function(x){
  # Throw an error if x isn't numeric
  if(!is.numeric(x)) stop('decode_states expects numeric input indexed from 1!')
  
  switch(x,
         "CT, ME, NH, RI, VT", "MA", "NY", "NJ", "PA", "IL", "IN, OH", "MI", "WI",
         "IA, MN, ND, SD", "KS, NE", "MO", "VA", "DE, DC, MD, WV", "GA",
         "NC, SC" , "FL", "AL, KY, MS", "TN", "AR, LA, OK",
         "TX", "CO", "ID, MT, UT, WY", "AZ", "NV, NM",
         "CA", "AK, HI, OR, WA"
  )
}

# this is a wrapper to an apply call
decode_all_states = function(x){
  sapply(x, decode_state)
}

# Functions to decode housing type
decode_house_type = function(x){
  if(!is.numeric(x)) stop('decode_house_type expects numeric input indexed from 1!')
  
  switch(x,
         'MobileHome',
         'SingleFamilyDetached',
         'SingleFamilyAttached',
         'ApartmentFew',
         'ApartmentMany'
  )
}

decode_all_house_types = function(x){
  sapply(x, decode_house_type)
}
