#Make Supplementary Figure 2
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")

#No scientific notation
options(scipen=999)

library(dplyr)
library(ggplot2)
library(sf)
library(rworldmap)

myThemeStuff <- theme(panel.background = element_rect(fill = NA),
                      panel.border = element_rect(fill = NA, color = NA),
                      panel.grid.major = element_line(color="gray90"), #turn off graticules
                      panel.grid.minor = element_blank(),
                      axis.ticks = element_line(color = "gray5",size=.35),
                      axis.text = element_text(color = "black", size = 6, family="sans"),
                      axis.title = element_text(color = "black", size = 7, family = "sans"),
                      axis.title.y.right = element_text(angle = 90,hjust=0),
                      legend.key = element_blank(),
                      plot.tag = element_text(family = "sans", size = 9),
                      plot.margin = unit(c(.01,.01,.01,.01),"in")
)

#Load eez-sea shapefile. Original source. http://www.marineregions.org/sources.php#iho. 
#Created in 1. filter eez to those that intersect*
eez <- st_read("Data/Intersect_IHO_EEZ_v2_2012/eez.shp")
eez$MarRegion <- as.character(eez$MarRegion)

#Want to fill EEZ-sea regions that border the high seas orange
#Load 50 km buffers around these EEZ-sea regions, so I can add an indicator to 
#the above eez object for EEZ-sea regions that border the high seas
bufs <- st_read("Data/eez_bufs_together_50km.shp")

#EEZ-sea regions that border the high seas
border <- bufs$MarRegion %>% as.character() %>% unique()

#Add indicator for bordering HS to eez object
eez <- mutate(eez, border = if_else(MarRegion %in% border, 1, 0) %>% as.factor())

#Clean up
rm(border, bufs)

#Project
eez <- st_transform(eez,"+proj=moll +lon_0=0 +lat_0=0 +ellps=WGS84 +no_defs")

#Get country boundaries
countries <- getMap(resolution = "low")
countries <- st_as_sf(countries)
countries <- st_transform(countries,"+proj=moll +lon_0=0 +lat_0=0 +ellps=WGS84 +no_defs")

eez$border <- relevel(eez$border, ref = "0")

#Make plot
supfig2 <- ggplot() + 
  geom_sf(data = countries, fill = "gray90", col = NA) + 
  geom_sf(data=eez, aes(fill = border)) + 
  scale_fill_manual("In analysis?",
                     values = c("white", "darkorange1"),
                     labels = c("No", "Yes")) +
  guides(fill=FALSE) + 
  myThemeStuff

ggsave(plot=supfig2, filename = "Figures/supfig2.png",
       units = "mm", width = 180, height = 120, dpi = 900)
