ais_badfor.Rdata contains AIS unauthorized foreign fishing at the EEZ-sea region-day-integer bin-flag state-gear type level. 
For example, one observation could be hours of fishing by all Taiwanese-flagged drifting longline vessels between 38 and 39 km outside of Argentina's EEZ on January 1, 2016.

Variables:
-MarRegion: EEZ-sea region
-date: Date
-dist: Integer bin distance from MarRegion's high seas boundary
-type: Indicates whether fishing observation took place inside an EEZ or outside an EEZ on the high seas
-flag: country the fishing vessel is flagged to
-geartype: type of fishing activity
-hours: hours of fishing
-Area_km2: Area in square km of the EEZ-sea region-integer bin
-hours_thsqkm: Hours of fishing per thousand square km. Equals (hours/Area_km2)*1000

