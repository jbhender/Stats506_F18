* Display
display 2
display 5 - 2
display sqrt(4)

* basic command syntax
regress y x1 x2
regress y x1 x2, noconstant
reg y x1 x2, nocons
regress y x1 x2 if female == 1
regress y x1 x2 in 1/100

* Getting help
help display
help regress

* Importing data
* https://www.eia.gov/consumption/residential/data/2015/
import delimited https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
save res2015
save res2015, replace

** Preserve/restore/collapse
collapse (mean) dollarel (percent) cellar, by(region)
use res2015, clear
preserve
collapse (mean) dollarel (percent) cellar, by(region)
list
restore

summarize dollarel
histogram dollarel
browse doeid dollarel if dollarel > 8000
describe region cellar dollarel

codebook dollarel
tab cellar
mean dollarel

* Using a do-file
doedit
* Move a few commands into Do-file via history, demostrate running.
do myfile
*** stata myfile.do

* Discuss terminology
* variable, macro, command, expression, function

* Estimation commands
regress dollarel totcsqft tothsqft
estat ic
help regress postestimation
predict preddollarel
list dollarel preddollarel in 1/10

* Returned objects
display e(r2)
matrix list e(V)
help regress
ereturn list
tab cellar
return list

* Macros
* Predictors are heated/unheated square footage
regress dollarel totcsqft tothsqft
local predictors totcsqft tothsqft
regress dollarel `predictors'
* Run twice to show local vanishing

local x 3
display `x'
local y = `x' + 2
display `y'

local x 3
local y = `x' + 2
display `y'
display "`y'"
local y2 `x' + 2
display `y2'
display "`y2'"

local r2 e(r2)
display `r2'
display "The model R^2 is " round(`r2', .001) "."

matrix v = e(V)
matrix list v
* Note doesn't require ticks

* Loops
* electricity, natural gas, propane, oil
foreach var of varlist dollarel dollarng dollarlp dollarfo {
	regress `var' totcsqft tothsqft
	local r2_`var' e(r2)
}
display "Electricity: " round(`r2_dollarel', .01) _newline ///
				"Natural gas: " round(`r2_dollarng', .01) _newline ///
				"Propane:     " round(`r2_dollarlp', .01) _newline ///
				"Fuel:        " round(`r2_dollarfo', .01)

foreach i of numlist 1/100 {
	* Do something each `i'
}

* Manipulating data
preserve
drop cellar
keep if division == 1
restore

generate lgdollarel = log(dollarel)
replace dollarng = log(dollarng)
replace cellar = . if cellar == -2

* Mata:
mata
5 - 4
end

mata
x = 4
x + 2
end

mata
x
sqrt(x)
end

mata
M = (1,2\3,4)
M
I(2)
M*I(2)
N = (2,4\1,3)
N
M+N
N'
M:*N
M\N
M,N
end

regress
matrix v = e(V)
matrix list v

mata
V = st_matrix("v")
V
V = diagonal(sqrt(V))
V
st_matrix("se", V)
end
matrix list se

* Debugging
set trace on
mean dollarel
set trace off

* This is not a smart thing to do, nor a smart way to do it, but...
local var 0
foreach v of varlist dollarel dollarng dollarlp dollarfo {
	qui regress `v'
	local vce e(vce)
	* display `vce'
	local var = `var' + `vce'
}
display `var'/4

pause on
preserve
local vars dollarel dollarng dollarlp dollarfo
egen maxexpend = rowmax(`vars')
collapse (max) maxexpend, by(region)
pause
restore

* Prefixes
* svy: regress y x
* help svyset

regress dollarel totcsqft tothsqft
bayes: regress dollarel totcsqft tothsqft
bayes, normalprior(10): regress dollarel totcsqft tothsqft


bysort division: summarize dollarel
mean dollarel, over(division)

bysort division (dollarel): gen lowestdollar = dollarel[1]
bysort division (dollarel): gen highestdollar = dollarel[_N]
gen rangeexpend = highestdollar - lowestdollar
collapse (first) rangeexpend, by(division)
browse



* Regression example
regress dollarel totcsqft tothsqft i.region
margins region
margins region, pwcompare
margins region, pwcompare(pv)


regress dollarel totcsqft c.tothsqft##i.region
summarize tothsqft
margins region, at(tothsqft = (0 2000 4000 6000 8000))
marginsplot

margins region, dydx(tothsqft)
margins region, dydx(tothsqft) pwcompare(pv)
