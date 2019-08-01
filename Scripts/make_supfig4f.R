#Make Supplementary Figure 4f
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")

library(dplyr)
library(ggplot2)
library(purrr)
library(readr)
library(stringr)
library(sf)
library(lwgeom)
library(lfe)

#Load 50 km buffer
buf <- st_read("Data/eez_bufs_together_50km.shp")

#Filter to control areas
control <- filter(buf, type=="outer")

#Area intersecting
inter <- st_intersection(control)

#Drop if n.overlaps==1. This means the row contains the portion of the outerbuffer that doesn't overlap any other outer buffers
inter <- filter(inter, n.overlaps != 1)

#Do a projection of a given polygon
projGen <- function(poly){
  
  center <- st_centroid(poly) %>%
    st_coordinates()
  
  x <- center[1,1]
  y <- center[1,2]
  
  #Project so that can do buffer
  poly <- st_transform(poly,
                       paste0("+proj=aeqd +lat_0=",y,
                              " +lon_0=",x,
                              " +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
                         st_crs()
  )
  
  #If invalid, make it valid
  if(st_is_valid(poly)=="FALSE"){
    poly <- st_make_valid(poly)
  }
  
  return(poly)
}

#Calculate area of overlap and multiply by number of overlaps (if applicable)
calcArea <- function(poly){
  
  poly <- projGen(poly)
  
  #Area
  area <- st_area(poly)
  
  #Multiply by number of overlaps
  area <- area*(poly$n.overlaps-1)
  
  return(area)
}

#Apply over EEZs
areas <- lapply(1:nrow(inter), function(x){
  area <- calcArea(inter[x,])
})

#Add onto inter
inter <- mutate(inter, inter_area = unlist(areas))

#For given MarRegion, calculate area that is overlapping
#If MarRegion 1 overlaps MarRegion 2, there is only one row with this area and it has the MarRegion 1 in the MarRegion column
#I want to assign this overlap area to MarRegion 2 as well.
overArea <- function(index){
  
  overarea_index <- 0
  for(x in 1:nrow(inter)){
    
    origin <- unlist(inter$origins[x])
    
    #If this MarRegion is present in this overlap
    if(index %in% origin){
      #Add to its area
      overarea_index <- overarea_index + inter$inter_area[x]
    }
    
  }
  
  return(overarea_index)
  
}


#Apply over MarRegions
inter_areas <- sapply(1:nrow(control), function(x){
  overArea(x)
})

#Make into a df
inter_areas <- cbind(as.character(control$MarRegion), inter_areas) %>% as.data.frame() %>% tbl_df()
names(inter_areas) <- c("MarRegion","overlap_area")
inter_areas <- mutate(inter_areas, MarRegion = as.character(MarRegion), 
                      overlap_area = as.character(overlap_area) %>% as.numeric())

#Need to calculate area of each control area
#Add a column called n.overlaps that always equals 2 so that I can use calcArea function
control <- mutate(control, n.overlaps=2)

controlareas <- lapply(1:nrow(control), function(x){
  area <- calcArea(control[x,])
})

#Create new df to contain relevant information
overlapdf <- as.data.frame(control) %>% dplyr::select(MarRegion) %>% 
  mutate(area_m2 = unlist(controlareas), MarRegion = as.character(MarRegion)) %>% 
  left_join(inter_areas, by = "MarRegion") %>% 
  mutate(overfrac = overlap_area / area_m2) %>% tbl_df()


#There are 14 islands whose control regions don't overlap anything else (out of 197 EEZ-sea regions). 
ggplot() + geom_histogram(data=overlapdf, aes(x=overfrac))


#Clean up
rm(areas, buf, control, controlareas, inter, inter_areas)

#Now estimate heterogeneous treatment effects by quantile bin.
#Load fishing data
load("Data/ais_cross_100km.Rdata")

aisdf <- filter(aisdf, dist <= 50 & group == "bad_for")

#Join
aisdf <- left_join(aisdf, overlapdf, by = "MarRegion")

#Create an inner indicator variable so type:MarRegion not colinear with FE
aisdf <- mutate(aisdf, inner = if_else(type=="inner",1,0))

#Shift distance in by .5
aisdf$dist <- aisdf$dist - .5

aisdf$MarRegion <- as.factor(aisdf$MarRegion)

aisdf <- mutate(aisdf, dist2 = dist^2, dist3 = dist^3)

##Binned regressions
#Filter overlapdf to EEZs with unauthorized foreign fishing effort
myoverlap <- filter(overlapdf, MarRegion %in% aisdf$MarRegion)
  
#What are 5 quantiles? (to report in appendix) 
quantile(myoverlap$overfrac, probs = seq(0,1,.25))
#0, .072, .16, .40, 2.17

#Define 19 quantiles for MarRegions with bad_for effort and positive overlap
qvec <- quantile(myoverlap$overfrac[myoverlap$overfrac>0], probs = seq(0,1,1/19))
  
#Add column onto myoverlap
overlapdf$overlap_quant <- cut(overlapdf$overfrac, breaks = qvec, include.lowest = TRUE)

overlapdf$overlap_quant <- as.character(overlapdf$overlap_quant)
  
#The 14 island EEZs with no overlap are in the first quantile
overlapdf$overlap_quant[overlapdf$overfrac==0] <- "[0,0]"

#All bins have 8-14 MarRegions

#Join onto aisdf
aisdf <- left_join(aisdf, overlapdf)

#Regression
reg <- felm(hours_thsqkm ~ inner:overlap_quant + dist:overlap_quant
            + dist2:overlap_quant + dist3:overlap_quant
            + dist:inner:overlap_quant + dist2:inner:overlap_quant
            + dist3:inner:overlap_quant |overlap_quant, data = aisdf)


myThemeStuff <- theme(panel.background = element_rect(fill = NA),
                      panel.border = element_rect(fill = NA, color = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.ticks = element_line(color = "gray5",size=.35),
                      axis.text = element_text(color = "black", size = 5.5, family="sans"),
                      axis.title = element_text(color = "black", size = 6.5, family = "sans"),
                      #axis.title.y.right = element_text(angle = 90,hjust=0),
                      axis.title.y = element_text(hjust = .5),
                      legend.key = element_blank(),
                      plot.title = element_text(hjust = 0.5), 
                      legend.text=element_text(size=6.5, family = "sans"),
                      legend.title = element_text(size=6.5, family = "sans"),
                      plot.margin = unit(c(0.01,0.01,0.01,.05),"in"),
                      plot.tag = element_text(family = "sans", size = 9, face='bold')
)

#First grab relevant coefficients
badcoefs <- coefficients(reg)
badcoefs <- badcoefs[grep("inner:overlap_quant",names(badcoefs))]
#Drop those interacted with distance
badcoefs <- badcoefs[-grep("dist",names(badcoefs))]

#Reorder coefficients from smallest to largest
badcoefs <- c(badcoefs[19:20],badcoefs[1:18])

#Add an xvar for plotting
badcoefs <- as.data.frame(badcoefs) %>% mutate(xvar = 1:20)

#Flip coefficients so that plot is proportion of deterrence effect
badcoefs <- mutate(badcoefs, flip = -badcoefs)

#Calculate proportion
badcoefs <- mutate(badcoefs, prop = flip/(sum(badcoefs)*-1))

#Want quantiles making a positive contribution filled green and negative filled red
badcoefs <- mutate(badcoefs, contr = if_else(prop>0,"pos","neg") %>% as.factor())
badcoefs$contr <- relevel(badcoefs$contr, ref = "pos")

supfig4f <- ggplot(data = badcoefs, aes(x=xvar, y = prop)) + 
  geom_bar(aes(fill = contr),stat="identity", color="black", alpha = .3, size = .1,
           position=position_dodge()) + 
  geom_smooth(formula = y~poly(x,1), method = "lm", se = FALSE,col="black",size=.2) + 
  scale_fill_manual(values = c("springgreen4","firebrick")) + 
  guides(fill=FALSE) + 
  myThemeStuff + 
  scale_x_continuous("Overlap fraction group", breaks = 1:(1/.05)) + 
  scale_y_continuous("Contribution to total deterrence effect (%)", 
                     breaks = c(0,.2,.4,.6), 
                     labels = c("0%","20%","40%","60%")) + 
  labs(tag= "f")

ggsave("Figures/supfig4f.png",supfig4f,
       width=88,height=54.83077,units="mm",dpi=1200)

#What is linear trend and is it significant?
lm(prop ~ xvar, data = badcoefs) %>% summary()
#slope = -.0056, p-val = .34
