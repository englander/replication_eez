ais_cross_100km.Rdata contains AIS fishing at the EEZ-sea region-integer bin-vessel type level. 
For example, one observation could be hours of fishing by all unauthorized foreign fishing vessels 38 and 39 km outside of Argentina's EEZ between 2012 and 2016. 

Variables:
-MarRegion: EEZ-sea region
-dist: Integer bin distance from MarRegion's high seas boundary
-type: Indicates whether fishing observation took place inside an EEZ or outside an EEZ on the high seas
-group: Vessel type. "bad_for" indicates unauthorized foreign fishing, "ok_for" indicates authorized foreign fishing, and "dom" indicates domestic fishing.
-hours: hours of fishing
-Area_km2: Area in square km of the EEZ-sea region-integer bin
-hours_thsqkm: Hours of fishing per thousand square km. Equals (hours/Area_km2)*1000

