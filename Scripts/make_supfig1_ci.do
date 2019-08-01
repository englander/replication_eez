*Calculate confidence intervals for Supplementary Figs 1a-c
cd "C:\Users\englander\Box Sync\VMS\Nature Sustainability\replication_files"

*Load data created in make_supfig1.R
use "Data\supfig1dat.dta", clear

*Declare time series
tsset dist

*Recenter dist on middle of integer bin
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"

*Have to do this to keep as tsset
reg depth inner##c.dist##c.dist##c.dist
estat bgodfrey, lags(1)

*Optimal lag is 18
newey depth inner##c.dist##c.dist##c.dist, lag(18) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig1a_ci.csv", comma replace //writing out results for R

*Calculate confidence interval for Sup Fig 1b
*Have to reload data
use "Data\supfig1dat.dta", clear
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg depth inner##c.dist##c.dist##c.dist
estat bgodfrey, lags(1)

*Optimal lag is 32
newey npp inner##c.dist##c.dist##c.dist, lag(32) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig1b_ci.csv", comma replace //writing out results for R

*Calculate confidence interval for Sup Fig 1c
*Have to reload data
use "Data\supfig1dat.dta", clear
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg depth inner##c.dist##c.dist##c.dist
estat bgodfrey, lags(1)

*Optimal lag is 4
newey sst inner##c.dist##c.dist##c.dist, lag(4) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig1c_ci.csv", comma replace //writing out results for R
