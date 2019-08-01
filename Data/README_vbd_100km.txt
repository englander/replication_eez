vbd_100km.Rdata and .dta contains nighttime lit fishing vessel count at the integer bin level. 
For example, one observation is the count of individual lit fishing vessels in 2017 that are 38 and 39 km outside an EEZ on the high seas.

Variables:
-dist: Integer bin distance from an EEZ-sea region's high seas boundary
-type: Indicates whether fishing observation took place inside an EEZ or outside an EEZ on the high seas
-count: Number of individual lit fishing vessels
-Area_km2: Area in square km of the integer bin
-count_msqkm: Equals (count/Area_km2)*1000000
-absdist: Absolute value of dist variable
-dist2: absdist squared
-dist3: absdist cubed
-inner: Equals 1 if observation is inside an EEZ and equals 0 otherwise
