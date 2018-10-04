*cd "/Users/josh/Desktop"
use clean-merged-visitlog, clear

* Generate days between visits, and total number of visits
preserve
keep studyid date
duplicates drop studyid date, force
bysort studyid (date): gen daystillnext = date[_n+1] - date[_n]
bysort studyid (date): gen numvisits = _N
save tmp, replace
restore

merge m:1 studyid date using tmp
assert _merge == 3
drop _merge
order studyid date 

* Total doses per med/visit
*  Number of doses per day * days between visits
generate totaldosespervisit = dosesperday*daystillnext
replace totaldosespervisit = dosesperday*duration_days if duration_days < .
* If it was a patients last visit, they got a "duration_days" which indicates how long they
*  have to keep taking the drops.

* Max daily doses
*  First count doses per visit across all medications, then find their max.
egen dailydosesbyvisit = sum(dosesperday), by(studyid date)
egen maxdailydose = max(dailydosesbyvisit), by(studyid)
drop dailydosesbyvisit

* Number of unique drugs
*  Find all unique drugs then count them (total is non-missing).
egen tag = tag(studyid medicationname)
egen uniquedrugs = total(tag), by(studyid)
drop tag

* Calculate age. 
*  First indicate first visit date.
bysort studyid (date): gen firstvisit = date[1]
format firstvisit %td
generate age = firstvisit - dob
replace age = age/365

* Collapse by person
collapse (first) age female tractmedianhouseholdincome numvisits uniquedrugs maxdailydose ///
				 (mean) dosesperday /// Average doses per day
				 (sum) totaldoses = totaldosespervisit /// Total doses taken
						, by(studyid)

compress

save clean-merged-withoutcomes-visitlog, replace


