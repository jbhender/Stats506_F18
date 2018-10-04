*cd "/Users/josh/Desktop"
use clean-merged-withoutcomes-visitlog, clear

* Add some nice labels
label variable numvisits "Number of visits"
label variable uniquedrugs "Number of unique drugs"
label variable maxdailydose "Maximum daily dose"
label variable dosesperday "Doses per day"
label variable totaldoses "Total number of doses"
label variable female "Female"
label variable tractmedianhouseholdincome "Tract Median HH Income"
label variable age "Age"

graph matrix numvisits-totaldoses, half
graph export ~/Desktop/matrix.pdf

pca numvisits-totaldoses
loadingplot, xline(0) yline(0)
predict fact1 fact2


foreach var of varlist numvisits-totaldoses fact1 fact2 {
	quietly regress `var' age tractmedianhouseholdincome i.female
	estimates store `var'
}

* User written command: Type `ssc install outreg2` to install
outreg2 [numvisits  uniquedrugs maxdailydose dosesperday totaldoses fact1 fact2] using tmp, ///
			excel replace label alpha(0.01, 0.05, 0.10)
