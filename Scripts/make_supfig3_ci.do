*Calculate confidence intervals for Supplementary Figure 3
cd "C:\Users\englander\Box Sync\VMS\Nature Sustainability\replication_files"

*Load data
use "Data\detdf.dta", clear

*Have to convert group to be numeric 
encode group, gen(group_num)
*Note that group_num==1 means bad_for (unauthorized foreign), 2 means dom (domestic), 3 means ok_for (authorized foreign)

*Declare panel data; panelvar timevar (I'm taking distance to be same as time here)
tsset group_num dist

*Center data on middle of integer bin
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"

*Have to run estat to keep data as tsset
reg count_tenthveshours inner##c.dist##c.dist##c.dist if group == "bad_for"
estat bgodfrey, lags(1)

*Optimal lag is 8
newey count_tenthveshours inner##c.dist##c.dist##c.dist if group == "bad_for", lag(8) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig3a_ci.csv", comma replace //writing out results for R

*Calculate confidence interval for Sup Fig 3b
*Have to reload data
use "Data\detdf.dta", clear
encode group, gen(group_num)
tsset group_num dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg count_tenthveshours inner##c.dist##c.dist##c.dist if group == "bad_for"
estat bgodfrey, lags(1)

*Optimal lag is 3
newey count_tenthveshours inner##c.dist##c.dist##c.dist if group == "ok_for", lag(3) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig3b_ci.csv", comma replace  //writing out results for R

*Have to reload data
use "Data\detdf.dta", clear
encode group, gen(group_num)
tsset group_num dist
replace dist = dist - .5 if type=="inner"
replace dist = dist + .5 if type=="outer"
reg count_tenthveshours inner##c.dist##c.dist##c.dist if group == "bad_for"
estat bgodfrey, lags(1)

*Optimal lag is 22
newey count_tenthveshours inner##c.dist##c.dist##c.dist if group == "dom", lag(22) force
set matsize 9000
margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
parmest, norestore level(95)
outsheet using "Data\Confidence_Intervals\supfig3c_ci.csv", comma replace  //writing out results for R

