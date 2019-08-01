#Make Figure 1
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")

options(scipen=999)

library(dplyr)
library(ggplot2)
library(sf)
library(geosphere)
library(viridis)
library(latex2exp)
library(rworldmap)
library(rworldxtra)
library(gridExtra)
library(raster)
library(grid)
library(fasterize)


myThemeStuff <- theme(panel.background = element_rect(fill = NA),
                      panel.border = element_rect(fill = NA, color = "black"),
                      panel.grid.major = element_line(color=NA), #turn off graticules
                      panel.grid.minor = element_blank(),
                      axis.ticks = element_line(color = "gray5",size=.35),
                      axis.text = element_text(color = "black", size = 6, family="sans"),
                      axis.title = element_text(color = "black", size = 6, family = "sans"),
                      axis.title.y.right = element_text(angle = 90,hjust=0),
                      legend.key = element_blank(),
                      plot.margin = unit(c(.01,.01,.01,.01),"in"),
                      legend.text=element_text(size=6, family = "sans"),
                      legend.title = element_text(size=6, family = "sans"),
                      plot.tag = element_text(family = "sans", size = 9, face = 'bold')
)



##Make location map first

#Plot Argentina landmass and nearby countries
countries <- getMap(resolution = "high")
countries <- st_as_sf(countries)

#This is the bounding box for everything in the plot
mybox <- c(-77,-39,-59.5,-21)
names(mybox) <- c("xmin","xmax","ymin","ymax")

#Crop
cropped_countries <- filter(countries, GEO3 == "South America")
cropped_countries <- st_crop(cropped_countries, xmin=mybox["xmin"]%>%as.numeric(),
                             xmax=-39,ymin=-55.9,ymax=mybox["ymax"]%>%as.numeric())

#Also crop eezs
eez <- st_read("Data/Intersect_IHO_EEZ_v2_2012/eez.shp")
eez <- st_crop(eez, mybox)
eez <- st_union(eez) %>% st_sf()

#Red line to demarcate Argentina EEZ's outer boundary
boundary <- st_read("Data/Intersect_IHO_EEZ_v2_2012/eez.shp") %>% 
  filter(MarRegion=="Argentinean part of the South Atlantic Ocean")

#Finally want 50 km buffer
un_buf <- st_read("Data/eez_bufs_together_50km.shp") %>% 
  filter(MarRegion=="Argentinean part of the South Atlantic Ocean") %>%
  st_union() %>% st_sf() %>% st_buffer(dist=0.0001) #Buffer for display purposes only to erase interior boundary

#What should dimensions of output be?
midpt_y <- gcIntermediate(
  c(mybox["xmin"],mybox["ymin"]), c(mybox["xmin"],mybox["ymax"]), n = 1
)

distGeo(c(mybox["xmin"], midpt_y[1,2]), c(mybox["xmax"], midpt_y[1,2]))/10^6 #width
distGeo(c(mybox["xmin"], mybox["ymin"]), c(mybox["xmin"], mybox["ymax"]))/10^6 #height
ratio_inset <- distGeo(c(mybox["xmin"], mybox["ymin"]), c(mybox["xmin"], mybox["ymax"])) /
  distGeo(c(mybox["xmin"], midpt_y[1,2]), c(mybox["xmax"], midpt_y[1,2]))

inset <- ggplot() +  
  geom_sf(data=eez,col=NA,fill="skyblue2",alpha=.4)+
  geom_sf(data=boundary,col="red",fill=NA,size=.6) + 
  geom_sf(data=filter(cropped_countries, ADMIN == "Argentina") %>% 
            st_buffer(dist=.5), col = "grey60", fill = "grey60")+
  geom_sf(data=boundary, col=NA,fill="skyblue2") + 
  geom_sf(data=cropped_countries, col = NA, fill = "grey80") + 
  geom_sf(data=filter(cropped_countries, ADMIN == "Argentina"), fill="grey60",col=NA) + 
  myThemeStuff + 
  geom_sf(data=un_buf, fill=NA, col = "black", size = .2, linetype = "dashed") + 
  scale_y_continuous(limits = c(mybox["ymin"], mybox["ymax"]), expand = c(0, 0)) + 
  scale_x_continuous(limits = c(mybox["xmin"], mybox["xmax"]), expand = c(0, 0)) + 
  theme(plot.margin = unit(c(0,0.06,0,.0),"in"), 
        panel.border = element_blank(),
        axis.line = element_line(color = 'black')) + 
  labs(tag = "a")  

ggsave(inset, filename = "Figures/fig1a.png",
       units = "in", width = 2.35, height = 2.35*ratio_inset, dpi = 1200)

rm(countries, cropped_countries, midpt_y, un_buf, mybox, eez)



##Now make Argentina AIS within 50 km (for northern portion)
arg <- st_read("Data/eez_bufs_together_50km.shp")  %>% 
  filter(MarRegion == "Argentinean part of the South Atlantic Ocean")

#Choose upper portion of EEZ
arg <- st_crop(arg, xmin=-62,xmax=-51,ymin=-50,ymax=-35)

#Make boundary for this portion
boundary <- st_intersection(boundary,
                            #Have to give arg a little buffer
                            filter(arg, type=="outer") %>%
                              st_buffer(dist = .005)) 

un_buf <- st_union(arg) %>% st_sf()

#Load AIS data within 50 km of Argentina's high seas boundary
load("Data/aiswithin50arg.Rdata")

#Calculate fishing hours per square km
#Function to calculate area of a grid cell given latitude of grid cell
areaFun <- function(cellsize = 0.01, lat){
  
  #Calculate latitude in radians for upper and lower point
  latup <- (lat+cellsize/2)*(pi/180)
  latdown <- (lat-cellsize/2)*(pi/180)
  
  #Source: https://gis.stackexchange.com/questions/29734/how-to-calculate-area-of-1-x-1-degree-cells-in-a-raster
  area <- (sin(latup) - sin(latdown)) * (cellsize*(pi/180)) * (6371^2)
  
  return(area)
}

areas <- sapply(unique(aiswithin50arg$lat), function(x){
  areaFun(lat = x)
})

areas <- cbind(unique(aiswithin50arg$lat), areas) %>% as.data.frame() %>% tbl_df()
names(areas) <- c("lat","area")

#Join with aiswithin50arg
aiswithin50arg <- left_join(aiswithin50arg, areas, by = "lat")

#Calculate hours per sq km
aiswithin50arg <- mutate(aiswithin50arg, hours_persqkm = hours/area)

#Make plot
argvis_ais <- ggplot() +  
  geom_raster(data=aiswithin50arg, aes(x=lon,y=lat,fill=hours_persqkm)) + 
  geom_sf(data=boundary, col="red", fill = NA, size=.4) + 
  geom_sf(data=un_buf, fill=NA, col = "black", size = .25, linetype = "dashed") + 
  scale_fill_viridis("AIS fishing \nhours km", trans='log10',
                     breaks = c(.1,1,10,100),
                     labels = c(".1","1","10","100")) + 
  myThemeStuff + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  theme(legend.position = c(.82,.23),
        plot.margin = unit(c(0,0.05,0,0),"in"),
        legend.title.align=0, 
        axis.title=element_blank(),
        axis.text=element_blank(), axis.ticks=element_blank(),
        panel.border = element_blank()) + 
  labs(tag = "b")


#What should dimensions of output be?

mybox <- st_bbox(arg)

midpt_y <- gcIntermediate(
  c(mybox["xmin"],mybox["ymin"]), c(mybox["xmin"],mybox["ymax"]), n = 1
)

distGeo(c(mybox["xmin"], midpt_y[1,2]), c(mybox["xmax"], midpt_y[1,2]))/10^6 #width
distGeo(c(mybox["xmin"], mybox["ymin"]), c(mybox["xmin"], mybox["ymax"]))/10^6 #height
ratio_ais <- distGeo(c(mybox["xmin"], mybox["ymin"]), c(mybox["xmin"], mybox["ymax"])) /
  distGeo(c(mybox["xmin"], midpt_y[1,2]), c(mybox["xmax"], midpt_y[1,2]))

ggsave(argvis_ais, filename = "Figures/fig1b.png",
       units = "in", width = 2.02, height = 2.02*ratio_ais, dpi = 1200)


##Night lights plot

#Load night lights
load("Data/vbdwithin50arg.Rdata")

#Now want to make this plot for VBD counts
argvis_vbd <- ggplot() +  
  geom_sf(data=un_buf, fill = "black", col = "white", size = .25) + 
  geom_raster(data = vbdwithin50arg, aes(x=lon,y=lat,fill=count)) + 
  geom_sf(data=boundary, col="red", fill = NA, size=.4) + 
  geom_sf(data=un_buf, fill = NA, col = "black", size = .25) + 
  myThemeStuff + 
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_fill_gradientn("Nighttime lit \nvessel count",
                       colors=c("grey5","grey80","white")) + 
  theme(legend.position = c(.82,.23),
        plot.margin = unit(c(0,0.02,0,0),"in"),
        legend.title.align=0, 
        axis.title=element_blank(),
        axis.text=element_blank(), axis.ticks=element_blank(),
        panel.border = element_blank()) + 
  labs(tag = "c")

ggsave(argvis_vbd, filename = "Figures/fig1c.png",
       units = "in", width = 2.02, height = 2.02*ratio_ais, dpi = 1200)


##Make Figure 1 with all three pieces

#Horizontal to vertical dimension ratio
inset_horiz_prop <- .4

png(file="Figures/fig1.png",w=180,h=180*inset_horiz_prop*ratio_inset, units = "mm", res=1200)
grid.newpage()
v1<-viewport(width = inset_horiz_prop, height = 1, x = 0.2, y = 0.5) #plot area for the inset map
v2<-viewport(width = (1-inset_horiz_prop)/2, height = 1, 
             x = inset_horiz_prop + (1-inset_horiz_prop)/4, y = 0.5) #plot area for the ais map
v3<-viewport(width = (1-inset_horiz_prop)/2, height = 1, 
             x = 1 - (1-inset_horiz_prop)/4, y = 0.5) #plot area for the vbd map
print(inset,vp=v1)
print(argvis_ais,vp=v2)
print(argvis_vbd,vp=v3)
dev.off()
