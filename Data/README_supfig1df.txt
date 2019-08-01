supfig1df.Rdata and .dta contains the average ocean depth, Net Primary Productivity (NPP), and sea surface temperature (SST) by distance to an EEZ-high seas boundary. 

Variables:
-dist: Integer bin distance to an EEZ-sea region's high seas boundary
-type: Indicates whether observation is inside an EEZ or outside an EEZ on the high seas
-depth: Average ocean depth in km
-npp: Average npp in mg Carbon per square km per day
-sst: Average sst in degrees Celsius
-absdist: Absolute value of dist variable
-dist2: absdist squared
-dist3: absdist cubed
-inner: Equals 1 if observation is inside an EEZ and equals 0 otherwise