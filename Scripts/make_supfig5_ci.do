*Calculate confidence intervals for Supplementary Figure 5
cd "C:\Users\englander\Box Sync\VMS\Nature Sustainability\replication_files"

*Load data created in make_supfig5.R
use "Data\subfig5dat.dta", clear

*Declare panel
tsset public dist

replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"

*Have to do this to keep as tsset
reg hours_msqkm inner##c.dist if public==1
estat bgodfrey, lags(1)

*Optimal lag is 6
newey hours_msqkm inner##c.dist##c.dist##c.dist if public==1, lag(6) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig5a_ci.csv", comma replace  //writing out results for R

*Have to reload data
use "Data\subfig5dat.dta", clear
tsset public dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist if public==1
estat bgodfrey, lags(1)

*Optimal lag is 5
newey hours_msqkm inner##c.dist##c.dist##c.dist if public==0, lag(5) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig5b_ci.csv", comma replace  //writing out results for R


