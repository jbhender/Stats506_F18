*cd "/Users/josh/Desktop"


**** Prescription data
import excel "visitlog", sheet("Patient Data") firstrow case(lower) clear
drop if studyid == .


replace medicationname = "none" if medicationname == "None"

foreach var of varlist medication medicationname freq {
 			encode `var', gen(`var'2)
 			order `var'2, after(`var')
 			drop `var'
 			rename `var'2 `var'
}

* This was previously "18 for 1 day, then 10", but over 2 days (10/27-10/28)
list dosesperday if dosesperday == "18 for one day, then 10"
replace dosesperday = "14" if dosesperday == "18 for one day, then 10"
destring dosesperday, replace

compress

save clean-visitlog, replace

**** Individual data
import excel "visitlog", sheet("Demographics") firstrow case(lower) clear

encode race, gen(race2)
order race2, after(race)
drop race
rename race2 race

* We'll recalculate age to get it at time of appointment
drop age

rename patientid studyid

compress

save clean-demographics, replace

**** Merge  files

use clean-demographics, clear
merge 1:m studyid using clean-visitlog
drop if _merge == 1
drop _merge

replace tractmedianhouseholdincome = tractmedianhouseholdincome/1000
rename female1 female

compress

sort studyid date medication
order studyid date

save clean-merged-visitlog, replace
