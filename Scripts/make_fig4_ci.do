*Calculate confidence intervals for Figs 4b-d
cd "C:\Users\englander\Box Sync\VMS\Nature Sustainability\replication_files"

*Load data created in make_Fig4.R
use "Data\ais_fig4dat.dta", clear

*Do abovenarrow first
keep if above==1 & drifting==0

*Declare time series
tsset dist

*Recenter dist on middle of integer bin
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"

*Have to do this to keep as tsset
reg hours_msqkm inner##c.dist##c.dist##c.dist
estat bgodfrey, lags(1)

*Optimal bandwidth is 5
newey hours_msqkm inner##c.dist##c.dist##c.dist, lag(5) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\fig4d_above_ci.csv", comma replace  //writing out results for R

*Have to reload data
use "Data\ais_fig4dat.dta", clear
keep if above==0 & drifting==0
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist##c.dist##c.dist
estat bgodfrey, lags(1)

*Optimal bandwidth is 6
newey hours_msqkm inner##c.dist##c.dist##c.dist, lag(6) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\fig4d_below_ci.csv", comma replace  //writing out results for R

*Have to reload data
use "Data\ais_fig4dat.dta", clear
keep if above==1 & drifting==1
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist##c.dist##c.dist
estat bgodfrey, lags(1)

*Optimal bandwidth is 6
newey hours_msqkm inner##c.dist##c.dist##c.dist, lag(6) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\fig4c_above_ci.csv", comma replace  //writing out results for R


*Have to reload data
use "Data\ais_fig4dat.dta", clear
keep if above==0 & drifting==1
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist##c.dist##c.dist
estat bgodfrey, lags(1)

*Optimal bandwidth is 1
newey hours_msqkm inner##c.dist##c.dist##c.dist, lag(1) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\fig4c_below_ci.csv", comma replace  //writing out results for R


*Have to reload data
use "Data\ais_fig4dat.dta", clear
keep if above==1 & drifting==.
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist##c.dist##c.dist
estat bgodfrey, lags(1)

*Optimal bandwidth is 5
newey hours_msqkm inner##c.dist##c.dist##c.dist, lag(5) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\fig4b_above_ci.csv", comma replace  //writing out results for R


*Have to reload data
use "Data\ais_fig4dat.dta", clear
keep if above==0 & drifting==.
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist##c.dist##c.dist
estat bgodfrey, lags(1)

*Optimal bandwidth is 4
newey hours_msqkm inner##c.dist##c.dist##c.dist, lag(4) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\fig4b_below_ci.csv", comma replace  //writing out results for R

