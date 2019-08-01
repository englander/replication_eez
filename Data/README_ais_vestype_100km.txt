ais_vestype_100km.Rdata and .dta contains AIS unauthorized foreign fishing at the integer bin-vessel type level. 
For example, one observation could be hours of fishing by all unauthorized foreign vessels between 38 and 39 km outside an EEZ between 2012 and 2016. 

Variables:
-dist: Integer bin distance from an EEZ-sea region's high seas boundary
-type: Indicates whether fishing observation took place inside an EEZ or outside an EEZ on the high seas
-group: Vessel type. "bad_for" indicates unauthorized foreign fishing, "ok_for" indicates authorized foreign fishing, and "dom" indicates domestic fishing.
-hours: hours of fishing
-Area_km2: Area in square km of the integer bin
-hours_msqkm: Hours of fishing per million square km. Equals (hours/Area_km2)*1000000
-absdist: Absolute value of dist variable
-dist2: absdist squared
-dist3: absdist cubed
-inner: Equals 1 if observation is inside an EEZ and equals 0 otherwise
