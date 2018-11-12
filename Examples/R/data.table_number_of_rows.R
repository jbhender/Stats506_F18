# Example to address the following question submitted by a student.
#
# Student: I would like to know when the number of rows will decrease 
# when we use "by" argument. Is it like when we use = to define columns, 
# all the rows will be kept and if we use := instead, 
# only one row for each group will be kept?

# Answer: The number of rows  returned for each group is dependent on what 
# you have in the "j" clause. When we use "=" within a "j" clause as in
# DT[ , .(a = RHS), group]  we get a new data.table. 
# The number of rows depends on what the RHS expression returns.  If it returns
# m rows the result will have m rows per group. In comparison, for dplyr
# we often have a RHS expression that returns the same number of rows as its 
# input (i.e. mutate/transmute) or a single value (i.e. summarize). 
# Moreover, in dplyr we  have to be explicit about this by specifying
# sumarize(), mutate(), etc. Sometimes, however we have a RHS that returns a
# single-value within a mutate() call ... in this case the single value is
# recycled to the appropriate length since mutate() never changes the number of
# rows.  We get similar behavior in data.table when we use `:=` as this function
# never changes the number of rows (and conceptually can't).
#
# Why do we preserve the number of rows when using := ? 
# Remember that := updates values by reference -- it can not change the
# number of rows; only change the values held in each "row" of a given column. 
# Remember that a data.table (and tibble and data.frame) is really just a list
# of vectors that all have the same length.  A "row" is just a 
# particular position in each of these vectors. We can't modify the 
# length of some and not others and still have a data.frame. 
# This isn't an issue in the examples below that don't use ":=",
# as we are creating a new data.frame/data.table in each case. 

##############
## Examples ##
##############

# libraries: ------------------------------------------------------------------
library(tidyverse); library(data.table)

# a data.table to work with: --------------------------------------------------
DT = as.data.table(mtcars)

# Summarize returns one row per group.: ----------------------------------------
DT %>% group_by(cyl) %>% summarize(max(mpg))
DT[ , .(max_mpg =  max(mpg) ) , cyl]

# Mutate preserves the number of rows.: ---------------------------------------
DT %>% group_by(cyl) %>% mutate(max_mpg = max(mpg))

# This data.table syntax "mutates" by making a copy
DT[ , c( .SD, .( max_mpg = max(mpg) ) ), cyl]

# In the above, the shorter max_mpg list is recycled within each group.
# Here is a more explicit version.
DT[ , c( .SD, .( max_mpg = rep(max(mpg), .N) ) ), cyl]

# Mutate in place by reference
DT[ , max_mpg := max(mpg), cyl][]

# However, in data.table we are not limited to maps that take nrows --> nrows
# or nrows --> 1row.

DT[ , .( type = c('min', 'max'), value=c(min(mpg), max(mpg))), cyl]
