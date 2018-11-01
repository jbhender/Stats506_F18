*----------------------------------------------------------*
* Compute univariate regression coefficients for mpg vs 
*  other continuous variables by cylinder (cyl) groups.
*
*
* Updated: Nov 1, 2018
* Author: James Henderson
*----------------------------------------------------------*

// Read data and reduce to needed variables

import delimited mtcars.csv

local vars = "disp hp wt" // mpg will be regressed against these by cyl
keep mpg cyl `vars'

// Sort by cyl and then compute centered variables, 
// cross product with mpg, and squares. 
gsort+ cyl

foreach var in `vars' {
 by cyl: egen `var'_grp_mean = mean(`var')
 generate `var'_gc = `var' - `var'_grp_mean
 generate `var'Xmpg = mpg * `var'_gc
 generate `var'_sq = `var'_gc * `var'_gc
}

// Compute the cross products, sum of squares, and regression coefficients
collapse (sum) *Xmpg (sum) *_sq, by(cyl)

foreach var in `vars' {
 generate beta_cyl_`var' = `var'Xmpg / `var'_sq 
}

drop *Xmpg *_sq

// Export values
export delimited mpg_betas_by_cyl.csv, replace
