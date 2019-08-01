#Make Supplementary Figure 5
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")
options(scipen=999)

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

library(dplyr)
library(ggplot2)
library(latex2exp)
library(purrr)
library(readr)
library(sandwich)

myThemeStuff <- theme(panel.background = element_rect(fill = NA),
                      panel.border = element_rect(fill = NA, color = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.ticks = element_line(color = "gray5",size=.35),
                      axis.text = element_text(color = "black", size = 5.5, family="sans"),
                      axis.title = element_text(color = "black", size = 6.5, family = "sans"),
                      #axis.title.y.right = element_text(angle = 90,hjust=0),
                      axis.title.y = element_text(hjust = 0.5),
                      legend.key = element_blank(),
                      plot.title = element_text(hjust = 0.5), 
                      legend.text=element_text(size=6.5, family = "sans"),
                      legend.title = element_text(size=6.5, family = "sans"),
                      plot.margin = unit(c(.01,.01,.01,.01),"in")
)

#Want to use total buffer area for each integer bin in calculating hours/unit area in a few lines
load("Data/ais_vestype_100km.Rdata")

binarea <- filter(aisdf, abs(dist)<=50) %>% 
  dplyr::select(dist, type, Area_km2) %>% distinct() 

#Load ais data
load("Data/ais_badfor.Rdata")

#Load country codes for flags (fishing countries) that have unauthorized foreign fishing and have no 
#publicly recorded access agreements by the Sea Around Us or by the European Union 
load("Data/noaccess.Rdata")

#Create aggregated df of unauthorized foreign fishing separating fishing vessels from countries
#with public access agreements from those with no public acccess agreements
aisdf <- mutate(aisdf, public = if_else(flag %in% noaccess, 0, 1)) %>% 
  group_by(dist, type, public) %>% 
  summarise(hours = sum(hours)) %>% ungroup() %>%
  #Make dist in outer negative
  mutate(dist = if_else(type=='outer',-dist,dist)) %>% 
  #Create inner indicator
  mutate(inner = if_else(type=="inner",1,0))

#Join integer bin areas onto aisdf
aisdf <- left_join(aisdf, 
                   #Make dist match the way it is currently parameterized in aisdf
                   mutate(binarea, dist = if_else(type=="outer",dist-.5,dist+.5)),
                   by = c("dist","type"))

#Calculate hours per million km^2
aisdf <- mutate(aisdf, hours_msqkm = (hours/Area_km2)*10^6)

#Output for Stata
haven::write_dta(aisdf, path = "Data/subfig5dat.dta")

#Move dist to center of integer bin
aisdf <- mutate(aisdf, dist = if_else(type=='inner',dist - .5,dist+.5)) %>% 
  mutate(absdist = abs(dist)) %>% 
  mutate(dist2 = absdist^2, dist3 = absdist^3)

#Choose optimal bandwidth
#Public first
pubresids <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                  inner:dist2 + inner:dist3, data = filter(aisdf, public==1))

#Bandwidth
bwNeweyWest(pubresids, kernel = "Quadratic Spectral") #4.5
bwNeweyWest(pubresids, kernel = "Bartlett") #5.3

#Private
priresids <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                  inner:dist2 + inner:dist3, data = filter(aisdf, public==0))

#Bandwidth
bwNeweyWest(priresids, kernel = "Quadratic Spectral") #4.0
bwNeweyWest(priresids, kernel = "Bartlett") #4.99

rm(pubresids, priresids)

#Load confidence intervals generated in Stata
cidat <- bind_rows(
  read_csv("Data/Confidence_Intervals/supfig5a_ci.csv") %>% mutate(public=1),
  read_csv("Data/Confidence_Intervals/supfig5b_ci.csv") %>% mutate(public=0)
)

#Create dist variable
cidat$dist <- gsub( "\\..*$", "", cidat$parm)
cidat$dist <- as.numeric(cidat$dist) - 50.5

#Create type variable
cidat$type <- "outer"
cidat$type[grep("1.inner",cidat$parm)] <- "inner"

#Drop non-relevant parameter values (e.g. inner with dist = -30 doesn't actually exist)
cidat <- filter(cidat, (type=="outer" & dist <= -1.5) | 
                  (type == "inner" & dist >= 1.5))

#Only need estimate (which should be very close to hours_msqkm), min95, max95, dist, type, public
aisdf <- left_join(aisdf,
                       dplyr::select(cidat, estimate, min95, max95, dist, type, public), 
               by = c("dist", "type", "public"))

#Check that point estimates are close to actual values (i.e. Stata regression was correct)
sum(aisdf$hours_msqkm - aisdf$estimate)

#Make public plot first
aisdf$type <- as.factor(aisdf$type)
#Want high seas to come first on plot
aisdf$type <- relevel(aisdf$type, ref = "outer")

#Write High seas and EEZs as text above axis instead of legend
textdf <- rbind(
  cbind(-25,0,"High seas"),
  cbind(25,0,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))

mybreaks <- seq(from=0,to=50000,by=10000)

#Plot
supfig5a <- ggplot(data = filter(aisdf, public == 1)) + 
  geom_point(aes(x=dist, y = hours_msqkm, shape = type),size = .5,color="firebrick") + 
  geom_smooth(aes(x=dist, y = hours_msqkm, shape = type),
              formula = y~poly(x,3), method = "lm",se = FALSE, size = .3,color="firebrick") + 
  scale_y_continuous(TeX("Hours of AIS fishing per million $\ \ km^2$"),
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(0, max(aisdf$hours_msqkm[aisdf$public==1]))) +
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) +   
  myThemeStuff + 
  geom_ribbon(aes(x=dist,ymin=min95,ymax=max95,shape=type),alpha=.3,fill="firebrick",col=NA) + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans", col = "black")+
  theme(plot.margin = unit(c(0.01,0.01,0.01,.01),"in"),
        plot.tag = element_text(family = "sans", size = 9,face='bold')) + 
  labs(tag = "a")

ggsave("Figures/supfig5a.png",supfig5a,
       width=88,height=54.83077,units="mm",dpi=1200) 


#Private plot

mybreaks <- seq(from=0,to=200000,by=50000)

supfig5b <- ggplot(data = filter(aisdf, public == 0)) + 
  geom_point(aes(x=dist, y = hours_msqkm, shape = type),size = .5,color="firebrick") + 
  geom_smooth(aes(x=dist, y = hours_msqkm, shape = type),
              formula = y~poly(x,3), method = "lm",se = FALSE, size = .3,color="firebrick") + 
  scale_y_continuous(TeX("Hours of AIS fishing per million $\ \ km^2$"),
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(0, max(aisdf$max95[aisdf$public==0]))) +
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) +   
  myThemeStuff + 
  geom_ribbon(aes(x=dist,ymin=min95,ymax=max95,shape=type),alpha=.3,fill="firebrick",col=NA) + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans", col = "black")+
  theme(plot.margin = unit(c(0.01,0.01,0.01,.01),"in"),
        plot.tag = element_text(family = "sans", size = 9,face='bold')) + 
  labs(tag = "b")

ggsave("Figures/supfig5b.png",supfig5b,
       width=88,height=54.83077,units="mm",dpi=1200) 
