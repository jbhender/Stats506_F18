## Problem Set 4, Question 1
## Stats 506, Fall 2018
##
## Use the Lahman baseball data to find the 
## all time hits leaders by birth country
## among those with at least 200 hits. 
##
## James Henderson, Dec 7, 2018

# Packages: -------------------------------------------------------------------
library(tidyverse)
library(dbplyr)
library(Lahman)

# Create a local SQLlite database of the Lahman data: -------------------------
lahman = lahman_sqlite()

#SQL
query = 
  '
SELECT m.nameFirst First, m.nameLast Last, m.debut Debut, birthCountry, 
       max(b.Hits) Hits
FROM ( SELECT playerID, sum(H) as Hits
       FROM BATTING
       GROUP BY playerID
       HAVING Hits > 199
     ) b
LEFT JOIN MASTER m
 ON b.playerID = m.playerID
GROUP BY birthCountry
ORDER BY -b.Hits
'

country_hits_leaders = lahman %>% tbl(sql(query)) %>% collect()

country_hits_leaders %>%
  transmute(
    Player = paste(First, Last),
    Debut = format.Date(lubridate::ymd(Debut), '%b %d, %Y'),
    "Country of Birth" = birthCountry,
    Hits = format(Hits, big.mark = ',')
  )
