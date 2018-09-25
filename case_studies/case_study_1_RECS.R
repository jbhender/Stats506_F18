## Case Study: RECS 2009 Home types
##
## The RECS 2009 Data used in this script can be found at the link below: 
##  https://www.eia.gov/consumption/residential/data/2009/index.php?view=microdata
## In particular, see the csv data file, the code book, and the note on 
## computing standard errors using replicate weights.
##
## Author: James Henderson (jbhender@umich.edu)
## Updated: Sep 24, 2018

# 80: -------------------------------------------------------------------------

# Libraries: ------------------------------------------------------------------
library(tidyverse)

# Utility functions: ----------------------------------------------------------
source('./funcs/decode_RECS.R')

# Obtain or restore data: -----------------------------------------------------
file = './data/recs2009_public.RData'
if (!file.exists(file)) {
   recs_tib = readr::read_delim(
"https://www.eia.gov/consumption/residential/data/2009/csv/recs2009_public.csv",
              delim = ',' )
  save(recs_tib, file = file)
} else {
  load(file) #recs_tib
}

# replicate weights: ----------------------------------------------------------
file = './data/recs2009_weights.RData'
if ( !file.exists(file) ) {
  weights = readr::read_delim( 
  'https://www.eia.gov/consumption/residential/data/2009/csv/recs2009_public_repweights.csv',
  delim = ',')
  save(weights, file = file)
} else {
  load(file)  # weigths
}
  
# Home type by state: ---------------------------------------------------------
home_type_prop = recs_tib %>% 
  transmute(State=REPORTABLE_DOMAIN, Type=TYPEHUQ, Weight = NWEIGHT) %>%
  complete(State, Type) %>%
  replace_na( list(Weight = 0) ) %>%
  mutate(State=decode_all_states(State), Type=decode_all_house_types(Type)) %>%
  group_by(State, Type) %>%
  summarize(Homes=sum(Weight)) %>%
  group_by(State) %>%
  mutate( pct = 100*Homes / sum(Homes) ) 
#home_type_prop

###########################################################
## Compute replicate weighted proportions using group_by ##
###########################################################

# Key values for each observation: --------------------------------------------
home_type = recs_tib %>% 
  transmute(DOEID, State=REPORTABLE_DOMAIN, Type=TYPEHUQ, Weight = NWEIGHT) %>%
  replace_na( list(Weight=0) ) %>%
  group_by(State, Type)

# Convert weights to long: ----------------------------------------------------
weights_long = weights %>% 
  gather(key = 'repl', value = 'w', brr_weight_1:brr_weight_244 )

# Join home type to weights: --------------------------------------------------
home_type_rep = 
  weights_long %>% 
  left_join(home_type %>% mutate( DOEID=as.integer(DOEID) ) , by='DOEID' )

# Check nothing is lost
if( nrow(weights_long) != nrow(home_type_rep) ) {
  stop("DOEID mismatch!")
}

# Replicate weighted proportions: --------------------------------------------
home_type_prop_repl = 
  home_type_rep %>%
  group_by(State, Type, repl) %>%
  summarize(Homes_r=sum(w)) %>%
  group_by(State, repl) %>%
  mutate( pct_r = 100*Homes_r / sum(Homes_r) ) 

## Missing value for IL
# with(home_type, length(unique(State)))
# with(home_type, length(unique(Type)))
# nrow(home_type_prop_repl)  / 244

# Add labels and join with point estimates: -----------------------------------
home_type_prop_repl = 
  home_type_prop_repl %>% 
  ungroup() %>%
  mutate(State=decode_all_states(State), Type=decode_all_house_types(Type)) %>%
  left_join(home_type_prop, by = c('State', 'Type') )

# Comptue standard errors: ----------------------------------------------------
home_type_prop =
  home_type_prop_repl %>%
  group_by(State, Type) %>%
  summarize( pct = pct[1],
             std_err = 2 * sqrt( mean( {pct_r - pct}^2 ) )
  ) %>%
  mutate( lwr = pct - qnorm(.975)*std_err,
          upr = pct + qnorm(.975)*std_err
  )

# Reassign states to regions: -------------------------------------------------
region_map = 
  recs_tib %>% 
  group_by(Region = REGIONC, State = REPORTABLE_DOMAIN) %>%
  summarize( n = n() ) %>%
  ungroup() %>%
  mutate(Region = factor(Region, levels = 1:4, 
                         labels  = c('Northeast', 'Midwest', 'South', 'West') ),
         State = decode_all_states(State)
         ) %>%
  select( -n )

home_type_prop = home_type_prop %>%
  left_join(region_map, by = 'State')

# Plot states by region in terms of housing types: ----------------------------
p_sfd =
  home_type_prop %>% 
  ungroup() %>%
  # Order states by % single family detached
  filter( Type %in% c('SingleFamilyDetached') ) %>%
  arrange( desc(pct) ) %>%
  mutate(State = factor(State, levels = State) ) %>%
  ggplot( aes( x = State, y = pct) ) +
  #geom_col( fill='gold' ) +
  geom_point( col='red', pch = 15, cex=2) + 
  geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy' ) +
  facet_wrap(~Region, scales='free_x', nrow=4) +
  theme_bw() + 
  theme( axis.text.x = 
           element_text( 
             angle = 0,
             size = 10
           ) ) +
  ylim( c(0, 100) ) +
  xlab('State(s)') +
  ylab('% Single family detached homes (2009)')
p_sfd

# Function to get similar plot for any Type: ----------------------------------
plot_home_type = function(type, ylab=''){
  home_type_prop %>% 
    ungroup() %>%
    # Order states by % single family detached
    filter( Type %in% type ) %>%
    arrange( desc(pct) ) %>%
    mutate(State = factor(State, levels = State) ) %>%
    ggplot( aes( x = State, y = pct) ) +
    geom_point( col='red', pch = 15, cex=2) + 
    geom_errorbar( aes(ymin=lwr, ymax=upr), col='navy' ) +
    facet_wrap(~Region, scales='free_x', nrow=4) +
    theme_bw() + 
    theme( axis.text.x = 
             element_text( 
               angle = 0,
               size = 10
             ) ) +
    ylim( c(0, 100) ) +
    xlab('State(s)') +
    ylab( ylab )
}

#! To Do: Error bars are missing for some State:Type combinations: ------------
plot_home_type('SingleFamilyAttached', '% Single family attached homes (2009)')  
plot_home_type('MobileHome', '% Mobile homes (2009)')
plot_home_type('ApartmentFew', '% Domiciles in Apartemtns with 2-4 units (2009)')
plot_home_type('ApartmentMany', '% Domiciles in Apartemtns with 5+ units (2009)')

home_type_prop %>% 
  filter( Type == 'SingleFamilyAttached' & State == 'MI')

# Exercise: Modify the function above to truncate the lower bound of the 95% CI
# to zero. 
# 80: -------------------------------------------------------------------------
