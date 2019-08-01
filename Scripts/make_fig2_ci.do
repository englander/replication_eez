*Calculate confidence intervals for Figure 2
cd "C:\Users\englander\Box Sync\VMS\Nature Sustainability\replication_files"

*Load AIS data for figures 2c and 2d
use "Data\ais_vestype_100km.dta", clear

*Filter data to within 50 km
drop if abs(dist) > 50

*Have to convert group to be numeric 
encode group, gen(group_num)
*Note that group_num==1 means bad_for (unauthorized foreign), 2 means dom (domestic), 3 means ok_for (authorized foreign)

*Declare panel data; panelvar timevar (I'm taking distance to be same as time here)
tsset group_num dist

*Center data on middle of integer bin
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"

*Have to run estat to keep data as tsset
reg hours_msqkm inner##c.dist##c.dist##c.dist if group == "bad_for"
estat bgodfrey, lags(1)

*Optimal lag in make_fig2.R says use 4 for bad_for
newey hours_msqkm inner##c.dist##c.dist##c.dist if group == "bad_for", lag(4) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\fig2c_ci.csv", comma replace //writing out results for R

*Calculate confidence interval for Fig 2D
*Have to reload data
use "Data\ais_vestype_100km.dta", clear
drop if abs(dist) > 50
encode group, gen(group_num)
tsset group_num dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist##c.dist##c.dist if group == "bad_for"
estat bgodfrey, lags(1)

*Optimal lag is 7 for dom
newey hours_msqkm inner##c.dist##c.dist##c.dist if group == "dom", lag(7) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\fig2d_dom_ci.csv", comma replace  //writing out results for R

*Have to reload data
use "Data\ais_vestype_100km.dta", clear
drop if abs(dist) > 50
encode group, gen(group_num)
tsset group_num dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist##c.dist##c.dist if group == "bad_for"
estat bgodfrey, lags(1)

*Optimal lag is 4 for ok_for
newey hours_msqkm inner##c.dist##c.dist##c.dist if group == "ok_for", lag(4) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\fig2d_okfor_ci.csv", comma replace  //writing out results for R


*Now calculate confidence interval for Fig 2A
use "Data\ais_tot.dta", clear
drop if abs(dist) > 50
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg hours_msqkm inner##c.dist##c.dist##c.dist
estat bgodfrey, lags(1)

*Optimal lag is 2
newey hours_msqkm inner##c.dist##c.dist##c.dist, lag(2) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\fig2a_ci.csv", comma replace  //writing out results for R


*Now calculate confidence interval for Fig 2b
use "Data\vbd_100km.dta", clear
drop if abs(dist) > 50
tsset dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg count_msqkm inner##c.dist##c.dist##c.dist
estat bgodfrey, lags(1)

*Optimal lag is 6
newey count_msqkm inner##c.dist##c.dist##c.dist, lag(6) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\fig2b_ci.csv", comma replace  //writing out results for R
