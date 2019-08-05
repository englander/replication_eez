#Create Figure 3 and Supplementary Figure 9
rm(list=ls())
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")

#Tell R not to use scientific notation
options(scipen=999)

#Load packages. If you do not have a package installed, you can install it by running: install.packages('packagename')
library(dplyr)
library(ggplot2)
library(latex2exp)
library(purrr)
library(sf)
library(lfe)
library(rworldmap)
library(viridis)
library(RColorBrewer)
library(grid)
library(rgeos)

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

load("Data/ais_cross_100km.Rdata")

#Filter data to within 50 km of an EEZ-high seas boundary and unauthorized fishing only
aisdf <- filter(aisdf, dist <= 50 & group == "bad_for")

#Center distances to middle of integer bin
aisdf$dist <- aisdf$dist - .5

#Create indicator for being inside an EEZ and create distance squared and distance cubed variables 
aisdf <- mutate(aisdf, inner = if_else(type=="inner",1,0), 
                   dist2 = dist^2, dist3=dist^3)
  
#Regression: Estimate a separate deterrence effect for each EEZ-sea region
coefs <- felm(hours_thsqkm ~ inner:MarRegion + dist:MarRegion + dist2:MarRegion + dist3:MarRegion + 
                dist:inner:MarRegion + dist2:inner:MarRegion + dist3:inner:MarRegion
              | MarRegion, data = aisdf)
#Note that the warning message refers to the calculation of the standard errors, 
#which are not used in fig3

#Grab deterrence effect for each EEZ-sea region
plotcoefs <- coefficients(coefs)[grep("inner:MarRegion",coefficients(coefs) %>% names())]

#Drop distance coefs
plotcoefs <- plotcoefs[-grep(":dist",names(plotcoefs))]

#Drop "inner:MarRegion" part of names(plotcoefs) so can join onto sf object by MarRegion
names(plotcoefs) <- gsub("inner:MarRegion","",names(plotcoefs))

#Convert plotdf to df so can join onto eez
plotcoefs <- cbind(names(plotcoefs), plotcoefs) %>% as.data.frame() %>% tbl_df() %>% 
  mutate_all(as.character)

names(plotcoefs) <- c("MarRegion", "coef")

plotcoefs$coef <- as.numeric(plotcoefs$coef)

#Create an indicator for coef = 0 (no unauthorized foreign effort anywhere)
plotcoefs <- mutate(plotcoefs, zero = if_else(coef==0,1,0)) %>% 
  #Indicator for positive treatment effect
  mutate(pos = if_else(coef>0,1,0))

#Load 50 km buffers around each EEZ-sea region
eez <- st_read("Data/eez_bufs_together_50km.shp")

eez$MarRegion <- eez$MarRegion %>% as.character()

#Union inner and outer buffers for each MarRegion
eez <- lapply(unique(eez$MarRegion), function(x){
  filt <- filter(eez, MarRegion == x)
  
  st_union(filt[1,],filt[2,])
})

eez <- do.call("rbind",eez)

#Project
eez <- st_transform(eez,"+proj=moll +lon_0=0 +lat_0=0 +ellps=WGS84 +no_defs")

#Join
eez <- left_join(eez, plotcoefs, by = "MarRegion")

#Plot with countries
countries <- getMap(resolution = "low")
countries <- st_as_sf(countries)
countries <- st_transform(countries,"+proj=moll +lon_0=0 +lat_0=0 +ellps=WGS84 +no_defs")

#Also fill white EEZ-sea regions that have less than 10 hours of unauthorized foreign fishing effort
#within 50 km of the high seas boundary between 2012 and 2016
fillwhite <- group_by(aisdf, MarRegion) %>% 
  summarise(hours = sum(hours)) %>% 
  arrange(hours) %>% 
  filter(hours > 0 & hours < 10) #Already filling hours = 0 with white because coef for those is na

fillwhite <- mutate(fillwhite, MarRegion = as.character(MarRegion), 
                    nodata = 1) %>% dplyr::select(-hours)

#Join onto eez
eez <- left_join(eez, fillwhite, by = "MarRegion")

#19 EEZ-sea regions have no AIS fishing within 50 km of their high seas boundary (in any vessel type),
#so are not in aisdf. These 19 EEZ-sea regions should also be coded as having nodata==1
eez$nodata[eez$MarRegion %not in% unique(aisdf$MarRegion)] <- 1

#Remaining EEZ-sea regions should have nodata==0
eez$nodata[is.na(eez$nodata)] <- 0

#Create one variable to capture the three possible ways to not fill a buffer
#(because discontinuity is positive, because discontinuity is zero, or because less than 10 hours per thousand sq. km of unauthorized foreign fishing)
eez <- mutate(eez, noeffect = if_else(zero==1 | pos==1 | nodata==1, 1, 0))

#Fill buffers according to coefficient. 
#Only fill buffers for EEZ-sea regions that have a deterrence effect.
eez <- mutate(eez, coeffill = if_else(noeffect==1,as.numeric(NA),coef))

#Now make coeffill positive so can log transform in plot
eez$coeffill <- -eez$coeffill

#Theme for ggplot
myThemeStuff <- theme(panel.background = element_rect(fill = NA),
                      panel.border = element_rect(fill = NA, color = NA),
                      panel.grid.major = element_line(color="gray90"), #turn off graticules
                      panel.grid.minor = element_blank(),
                      axis.ticks = element_line(color = "gray5",size=.35),
                      axis.text = element_text(color = "black", size = 6, family="sans"),
                      axis.title = element_text(color = "black", size = 7, family = "sans"),
                      axis.title.y.right = element_text(angle = 90,hjust=0),
                      legend.key = element_blank(),
                      plot.margin = unit(c(0,0,.6,0),"cm"),
                      legend.text=element_text(size=9, family = "sans"),
                      legend.title = element_text(size=9, family = "sans"),
                      plot.tag = element_text(family = "sans", size = 12, face = 'bold')
)

#Plot
fig3a <- ggplot() + 
  geom_sf(data = countries, fill = "gray90", col = NA) + 
  geom_sf(data=eez, aes(fill=coeffill), col = NA) + 
  scale_fill_viridis(TeX("Deterrence effect (hours per thousand $\ km^2$)"),trans='log', na.value='white',
                     breaks = c(1,7,55,400,3000,20000),
                     labels = c("1","7","55","400","3,000","20,000"),
                       guide = guide_colorbar(direction = "horizontal",
                                              position="bottom",
                                              title.position="top")) +
  myThemeStuff + theme(legend.key.width = unit(2.1, "cm"),
                       legend.title.align=.5,
                       legend.position=c(.5,-.08)) + 
  labs(tag = "a")

ggsave(plot=fig3a, filename = "Figures/fig3a.pdf",
       units = "mm", width = 180, height = 120, dpi = 1200)

#19 have no AIS data of any kind
filter(eez, is.na(coef)) %>% nrow()

#17 have no unauthorized foreign fishing
filter(eez, zero==1) %>% nrow()

#12 have more than 0 but less than 10 hours per th. sq. km
filter(eez, MarRegion %in% fillwhite$MarRegion) %>% nrow()

#83 have positive discontinuity
filter(eez, pos==1 & MarRegion %not in% fillwhite$MarRegion) %>% nrow()

#66 have negative discontinuity
filter(eez, coef<0 & MarRegion %not in% fillwhite$MarRegion) %>% nrow()

#What percent of deterrence effect does each EEZ-sea make up?
prop <- filter(plotcoefs, MarRegion %not in% fillwhite$MarRegion & zero != 1) %>% 
  mutate(prop = coef/sum(coef[coef<0])) %>% 
  arrange(desc(prop))

sum(prop$prop[1:10]) #Top 10 make up 97% of deterrence effect

##Also make density plot
#Calculate total hours of unauthorized foreign fishing effort within 50 km (per thousand km^2)
tothours <- group_by(aisdf, MarRegion) %>% 
  summarise(tothours = sum(hours), totarea = sum(Area_km2)) %>% ungroup() %>%
  mutate(tothours_thsqkm = (tothours/totarea)*1000)

#Join onto eez
eez <- left_join(eez, tothours, by = "MarRegion")

#Plot
fig3b <- ggplot() + 
  geom_sf(data = countries, fill = "gray90", col = NA) + 
  geom_sf(data=eez, aes(fill=tothours_thsqkm), col = NA) + 
  scale_fill_viridis(TeX("Unauthorized foreign fishing hours per thousand $\ km^2$ within 50 km of boundary"),
                     trans='log',na.value='white',
                     breaks = c(.1, 1,15,180,2500),
                     labels = c(".1","1","15","180","2,500"),
                       guide = guide_colorbar(direction = "horizontal",
                                              position="bottom",
                                              title.position="top")) +
  myThemeStuff + theme(legend.key.width = unit(2.1, "cm"),
                       legend.title.align=.5,
                       legend.position=c(.5,-.08)) + 
  labs(tag = "b")

ggsave(plot=fig3b, filename = "Figures/fig3b.pdf",
       units = "mm", width = 180, height = 120, dpi = 1200)


#Now make Supplementary Figure 9. 
#Plot total hours of unauthorized foreign fishing per thousand km^2 only for EEZ-sea regions that have 
#more than 10 hours of unauthorized foreign fishing but a positive discontinuity
#(the 83 EEZ-sea regions not deterring unauthorized foreign fishing)
supfig9 <- ggplot() + 
  geom_sf(data = countries, fill = "gray90", col = NA) + 
  geom_sf(data=filter(eez, pos==1 & MarRegion %not in% fillwhite$MarRegion), aes(fill=tothours_thsqkm), col = NA) + 
  scale_fill_viridis(TeX("Unauthorized foreign fishing hours per thousand $\ km^2$ within 50 km of boundary"),
                     trans='log',na.value='white',
                     breaks = c(.1, 1,15,180,500),
                     labels = c(".1","1","15","180","500"),
                     guide = guide_colorbar(direction = "horizontal",
                                            position="bottom",
                                            title.position="top")) +
  myThemeStuff + theme(legend.key.width = unit(2.1, "cm"),
                       legend.title.align=.5,
                       legend.position=c(.5,-.08))

ggsave(plot=supfig9, filename = "Figures/supfig9.png",
       units = "mm", width = 180, height = 120, dpi = 1200)


#Output one figure 3 with both parts
pdf(file="Figures/fig3.pdf",width=180/25.4,height=(110*2)/25.4)
grid.newpage()
v1 <-viewport(width = unit(180/25.4, "inches"), height = unit(110/25.4, "inches"),
              x = .5, y = .77)
v2 <-viewport(width = unit(180/25.4, "inches"), height = unit(110/25.4, "inches"),
              x = .5, y = .27)
print(fig3a, vp=v1)
print(fig3b, vp=v2)
dev.off()
