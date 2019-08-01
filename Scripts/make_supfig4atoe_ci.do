*Calculate confidence intervals for Supplementary Figures 4a to 2
cd "C:\Users\englander\Box Sync\VMS\Nature Sustainability\replication_files"

*Load data
use "Data\ais_vestype_100km.dta", clear

*Only want unauthorized foreign
drop if group!="bad_for"

*First plot is just for comparison; within 50 km
drop if abs(dist) > 50

*Declare time series
tsset dist

replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"

*Have to do this to keep as tsset
reg hours_msqkm inner##c.dist
estat bgodfrey, lags(1)

*6 for optimal lag
newey hours_msqkm inner##c.dist, lag(6) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig4a_ci.csv", comma replace  //writing out results for R

*Second plot is 10 out of 50
use "Data\ais_vestype_100km.dta", clear
drop if group!="bad_for"
drop if abs(dist) > 50 | abs(dist) < 10
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist
estat bgodfrey, lags(1)

*6 for optimal lag
newey hours_msqkm inner##c.dist, lag(6) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig4b_ci.csv", comma replace  //writing out results for R

*Third plot is 10 out of 100
use "Data\ais_vestype_100km.dta", clear
drop if group!="bad_for"
drop if abs(dist) < 10
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist
estat bgodfrey, lags(1)

*8 for optimal lag
newey hours_msqkm inner##c.dist, lag(8) force
set matsize 9000
margins inner, at(dist=(-99.5(1)99.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig4c_ci.csv", comma replace  //writing out results for R


*Fourth plot is 25 out of 50
use "Data\ais_vestype_100km.dta", clear
drop if group!="bad_for"
drop if abs(dist) > 50 | abs(dist) < 25
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist
estat bgodfrey, lags(1)

*5 for optimal lag
newey hours_msqkm inner##c.dist, lag(5) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig4d_ci.csv", comma replace  //writing out results for R


*Fifth plot is 25 out of 100
use "Data\ais_vestype_100km.dta", clear
drop if group!="bad_for"
drop if abs(dist) < 25
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist
estat bgodfrey, lags(1)

*7 for optimal lag
newey hours_msqkm inner##c.dist, lag(7) force
set matsize 9000
margins inner, at(dist=(-99.5(1)99.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig4e_ci.csv", comma replace  //writing out results for R
