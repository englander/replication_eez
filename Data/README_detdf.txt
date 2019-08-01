detdf.Rdata and .dta contains the number of instances in each integer bin between 2012 and 2016 in which the transponders of vessels of a given vessel type turned off for more than 24 hours (the number of "off instances"). 
For example, one observation could be the number of times an unauthorized foreign vessel's transponder turned off for more than 24 hours between 38 and 39 km outside an EEZ between 2012 and 2016. 

Variables:
-dist: Integer bin distance from an EEZ-sea region's high seas boundary
-type: Indicates whether fishing observation took place inside an EEZ or outside an EEZ on the high seas
-group: Vessel type. "bad_for" indicates unauthorized foreign fishing, "ok_for" indicates authorized foreign fishing, and "dom" indicates domestic fishing.
-count: Number of off instances
-veshours: Hours of vessel presence 
-absdist: Absolute value of dist variable
-dist2: absdist squared
-dist3: absdist cubed
-count_tenthveshours: Equals (count/veshours)*10000
-inner: Equals 1 if observation is inside an EEZ and equals 0 otherwise

