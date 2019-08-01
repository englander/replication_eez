*Calculate confidence intervals for Supplementary Figure 6
cd "C:\Users\englander\Box Sync\VMS\Nature Sustainability\replication_files"

*Load data created in make_supfig6.R
use "Data\supfig6dat.dta", clear

*Calculate confidence intervals for vessels above and below median tonnage
drop if var!="tonnage"

levelsof crossunit

foreach i in `r(levels)'{
cd "C:\Users\englander\Box Sync\VMS\Nature Sustainability\replication_files"
use "Data\supfig6dat.dta", clear
	
	di _n _n as txt "crossunit = " as result "`i'" 
	
	keep if crossunit == "`i'"
	
	qui levelsof lag
	local mylag `r(levels)'
	
	tsset dist

	replace dist = dist - .5 if type=="inner"
	replace dist = dist + .5 if type=="outer"
	reg hours_msqkm inner##c.dist##c.dist##c.dist
	estat bgodfrey, lags(1)
	
	cd "Data\Confidence_Intervals\"
	local end ".csv"
		
	newey hours_msqkm inner##c.dist##c.dist##c.dist, lag(`mylag') force
	set matsize 9000
	margins inner, at(dist=(-49.5(1)49.5)) post noestimcheck level(95)
	parmest, norestore level(95)
	outsheet using `i'`end', comma replace  //writing out results for R

}
