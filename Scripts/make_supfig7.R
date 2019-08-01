#Make Supplementary Figure 7
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")

options(scipen=999)

library(dplyr)
library(ggplot2)
library(latex2exp)
library(lfe)
library(readr)
library(purrr)
library(sandwich)
library(haven)

#Load ais data
load("Data/ais_badfor.Rdata")

#Center on middle of integer
aisdf <- mutate(aisdf, dist = dist-.5) %>%
  #Make being on high seas distance negative
  mutate(dist = if_else(type=="outer",-dist,dist))

#Sum to integer distance bin and six categories
geardf <-  group_by(aisdf, dist, type, geartype) %>% 
  summarise(hours = sum(hours)) %>% ungroup()

#Get area of total buffer
load("Data/ais_vestype_100km.Rdata")

geardf <- left_join(geardf, 
                    distinct(aisdf, dist, type, Area_km2), 
               by = c("dist","type"))

geardf <- mutate(geardf, hours_msqkm = (hours/Area_km2)*10^6)

rm(aisdf)

#Don't make a plot for other_fishing
geardf <- filter(geardf, geartype!="other_fishing")

geardf <- mutate(geardf, absdist = abs(dist)) %>% 
  mutate(dist2 = absdist^2, dist3 = absdist^3, inner = if_else(type=="inner",1,0))

#Given geartype, calculate optimal lag
optLag <- function(mygear){
  
  #drift
  reg <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
              inner:dist2 + inner:dist3, data = filter(geardf, geartype==mygear))
  
  #Bandwidth
  quad <- bwNeweyWest(reg, kernel = "Quadratic Spectral")
  bart <- bwNeweyWest(reg, kernel = "Bartlett")
  
  #Take max and round up to nearest integer
  lag <- max(quad, bart) %>% ceiling()
  
  #Add lag as column onto df
  df <- filter(geardf, geartype==mygear) %>%
    mutate(lag = lag)
  
  return(df)
}


#Apply over geartype
geardf <- map_df(unique(geardf$geartype), function(x){
  optLag(x)
})

#Output to stata.
haven::write_dta(#Make dist an integer so can declare tsset in Stata. I will recenter dist in Stata before computing confidence intervals.
  geardf %>%  mutate(dist = if_else(type=="inner",dist+.5,dist-.5)), 
 path="Data/supfig7dat.dta")


#Function to read in confidence intervals generated in stata
readFun <- function(mygear){
  
  read <- read_csv(paste0("Data/Confidence_Intervals/",mygear,".csv")) %>% 
    mutate(geartype=mygear)
  
  return(read)
}


#Load confidence intervals 
cidat <- map_df(unique(geardf$geartype), function(x){
  readFun(x)
})

#Create dist variable
cidat$dist <- gsub( "\\..*$", "", cidat$parm)
cidat$dist <- as.numeric(cidat$dist) - 50.5

#Create type variable
cidat$type <- "outer"
cidat$type[grep("1.inner",cidat$parm)] <- "inner"

#Drop non-relevant parameter values (e.g. inner with dist = -30 doesn't actually exist)
cidat <- filter(cidat, (type=="outer" & dist <= -1.5) | 
                  (type == "inner" & dist >= 1.5))

geardf <- left_join(geardf, 
                        dplyr::select(cidat, estimate, min95, max95, dist, type, geartype), 
                        by = c("dist", "type", "geartype"))

#Check that point estimates are close to actual values (i.e. Stata regression was correct)
sum(geardf$hours_msqkm - geardf$estimate)

geardf <- mutate(geardf, type = as.factor(type))

#Want high seas to come first on plot
geardf$type <- relevel(geardf$type, ref = "outer")

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
                      plot.margin = unit(c(.01,.01,.01,.01),"in"),
                      plot.tag = element_text(family = "sans", size = 9, face='bold')
)

textdf <- rbind(
  cbind(-25,0,"High seas"),
  cbind(25,0,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))

mybreaks <- seq(from=0,to=100000,by=25000)

#Drift
supfig7b <- ggplot(data = filter(geardf, geartype == "drifting_longlines")) + 
  geom_point(aes(x=dist, y = hours_msqkm, shape = type),color = "firebrick", size = .5) + 
  geom_smooth(aes(x=dist, y = hours_msqkm, shape = type),
              formula = y~poly(x,3), method = "lm",se = FALSE, color = "firebrick", size = .3) + 
  scale_y_continuous(TeX("Hours of drifting longline fishing per m. $\ \ km^2$"),  
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(0, max(geardf$max95[geardf$geartype=="drifting_longlines"]))) + 
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) +   myThemeStuff + 
  geom_ribbon(aes(x=dist,ymin=min95,ymax=max95,shape=type),alpha=.3,fill="firebrick") + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  theme(
        legend.position = c(0.89,.88), 
        legend.margin = margin(0,0,0,0,unit="cm"), 
        legend.key.height = unit(0, unit = "cm"),
        legend.key.width=unit(0,unit="cm"),
        legend.title.align = 0) + 
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "b")

ggsave("Figures/supfig7b.png",supfig7b,
       width=88,height=54.83077,units="mm",dpi=1200) 

#Fixed
mybreaks <- seq(from=0,to=10000,by=2500)

supfig7c <- ggplot(data = filter(geardf, geartype == "fixed_gear")) + 
  geom_point(aes(x=dist, y = hours_msqkm, shape = type),color = "firebrick", size = .5) + 
  geom_smooth(aes(x=dist, y = hours_msqkm, shape = type),
              formula = y~poly(x,3), method = "lm",se = FALSE, color = "firebrick", size = .3) + 
  scale_y_continuous(TeX("Hours of fixed gear fishing per m. $\ \ km^2$"),  
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(0, max(geardf$max95[geardf$geartype=="fixed_gear"]))) + 
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) +   myThemeStuff + 
  geom_ribbon(aes(x=dist,ymin=min95,ymax=max95,shape=type),alpha=.3,fill="firebrick") + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  theme(
    legend.position = c(0.89,.88), 
    legend.margin = margin(0,0,0,0,unit="cm"), 
    legend.key.height = unit(0, unit = "cm"),
    legend.key.width=unit(0,unit="cm"),
    legend.title.align = 0) + 
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "c")

ggsave("Figures/supfig7c.png",supfig7c,
       width=88,height=54.83077,units="mm",dpi=1200) 


#Purse
mybreaks <- seq(from=0,to=4000,by=1000)

supfig7d <- ggplot(data = filter(geardf, geartype == "purse_seines")) + 
  geom_point(aes(x=dist, y = hours_msqkm, shape = type),color = "firebrick", size = .5) + 
  geom_smooth(aes(x=dist, y = hours_msqkm, shape = type),
              formula = y~poly(x,3), method = "lm",se = FALSE, color = "firebrick", size = .3) + 
  scale_y_continuous(TeX("Hours of purse seine fishing per m. $\ \ km^2$"),  
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(0, max(geardf$max95[geardf$geartype=="purse_seines"]))) + 
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) +   myThemeStuff + 
  geom_ribbon(aes(x=dist,ymin=min95,ymax=max95,shape=type),alpha=.3,fill="firebrick") + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  theme(
    legend.position = c(0.89,.88), 
    legend.margin = margin(0,0,0,0,unit="cm"), 
    legend.key.height = unit(0, unit = "cm"),
    legend.key.width=unit(0,unit="cm"),
    legend.title.align = 0) + 
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "d")

ggsave("Figures/supfig7d.png",supfig7d,
       width=88,height=54.83077,units="mm",dpi=1200) 

#Squid
mybreaks <- seq(from=0,to=150000,by=50000)

supfig7e <- ggplot(data = filter(geardf, geartype == "squid_jigger")) + 
  geom_point(aes(x=dist, y = hours_msqkm, shape = type),color = "firebrick", size = .5) + 
  geom_smooth(aes(x=dist, y = hours_msqkm, shape = type),
              formula = y~poly(x,3), method = "lm",se = FALSE, color = "firebrick", size = .3) + 
  scale_y_continuous(TeX("Hours of squid jigger fishing per m. $\ \ km^2$"),  
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(0, max(geardf$max95[geardf$geartype=="squid_jigger"]))) + 
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) +   myThemeStuff + 
  geom_ribbon(aes(x=dist,ymin=min95,ymax=max95,shape=type),alpha=.3,fill="firebrick") + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  theme(
    legend.position = c(0.89,.88), 
    legend.margin = margin(0,0,0,0,unit="cm"), 
    legend.key.height = unit(0, unit = "cm"),
    legend.key.width=unit(0,unit="cm"),
    legend.title.align = 0) + 
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "e")

ggsave("Figures/supfig7e.png",supfig7e,
       width=88,height=54.83077,units="mm",dpi=1200) 


#Trawlers
mybreaks <- seq(from=0,to=60000,by=20000)

supfig7f <- ggplot(data = filter(geardf, geartype == "trawlers")) + 
  geom_point(aes(x=dist, y = hours_msqkm, shape = type),color = "firebrick", size = .5) + 
  geom_smooth(aes(x=dist, y = hours_msqkm, shape = type),
              formula = y~poly(x,3), method = "lm",se = FALSE, color = "firebrick", size = .3) + 
  scale_y_continuous(TeX("Hours of trawler fishing per m. $\ \ km^2$"),  
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(min(geardf$min95[geardf$geartype=="trawlers"]), max(geardf$max95[geardf$geartype=="trawlers"]))) + 
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) +   myThemeStuff + 
  geom_ribbon(aes(x=dist,ymin=min95,ymax=max95,shape=type),alpha=.3,fill="firebrick") + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  theme(
    legend.position = c(0.89,.88), 
    legend.margin = margin(0,0,0,0,unit="cm"), 
    legend.key.height = unit(0, unit = "cm"),
    legend.key.width=unit(0,unit="cm"),
    legend.title.align = 0) + 
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "f")

ggsave("Figures/supfig7f.png",supfig7f,
       width=88,height=54.83077,units="mm",dpi=1200) 



#Also make plot of all unauthorized foreign fishing for comparison (same as Fig. 2C)
load("Data/ais_vestype_100km.Rdata")

aisdf <- filter(aisdf, abs(dist) <=50 & group == "bad_for") %>% 
  arrange(dist, type)

#Load CI generated in make_fig2_ci.do (optimal lag determined in make_fig2.R)
cibad <- read_csv("Data/Confidence_Intervals/fig2c_ci.csv") %>% mutate(group = "bad_for")

#Create dist variable
cibad$dist <- gsub( "\\..*$", "", cibad$parm)
cibad$dist <- as.numeric(cibad$dist) - 50.5

#Create type variable
cibad$type <- "outer"
cibad$type[grep("1.inner",cibad$parm)] <- "inner"

#Drop non-relevant parameter values (e.g. inner with dist = -30 doesn't actually exist)
cibad <- filter(cibad, (type=="outer" & dist <= -1.5) | 
                  (type == "inner" & dist >= 1.5))

#Only need estimate (which should be very close to hours_msqkm), min95, max95, dist, type, group
aisdf <- left_join(aisdf, 
                        dplyr::select(cibad, estimate, min95, max95, dist, type, group), 
                        by = c("dist", "type", "group"))

sum(aisdf$hours_msqkm - aisdf$estimate)

aisdf$type <- as.factor(aisdf$type)
#Want high seas to come first on plot
aisdf$type <- relevel(aisdf$type, ref = "outer")



#Label deterrence effect
#Brace points
detlab_brace <- rbind(
  c(3.5,aisdf$estimate[aisdf$group=="bad_for" & aisdf$dist==-1.5]),
  c(7.5,aisdf$estimate[aisdf$group=="bad_for" & aisdf$dist==-1.5]),
  c(7.5,aisdf$estimate[aisdf$group=="bad_for" & aisdf$dist==1.5]),
  c(3.5,aisdf$estimate[aisdf$group=="bad_for" & aisdf$dist==1.5])
) %>% as.data.frame() %>% tbl_df()
names(detlab_brace) <- c("x","y")

#Text label
detlab_text <- c("Deterrence effect",
                 8.5,
                 (aisdf$estimate[aisdf$group=="bad_for" & aisdf$dist==-1.5] + 
                    aisdf$estimate[aisdf$group=="bad_for" & aisdf$dist==1.5])/2) %>% 
  t() %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character)
names(detlab_text) <- c("text","x","y")
detlab_text <- mutate(detlab_text, x= as.numeric(x),y=as.numeric(y))

mybreaks <- seq(from=0,to=250000,by=50000)

#Plot
supfig7a <- ggplot(data = filter(aisdf, group == "bad_for")) + 
  geom_point(aes(x=dist, y = hours_msqkm, shape = type),color = "firebrick", size = .5) + 
  geom_smooth(aes(x=dist, y = hours_msqkm, shape = type),
              formula = y~poly(x,3), method = "lm",se = FALSE, color = "firebrick", size = .3) + 
  scale_y_continuous(TeX("Hours of AIS fishing per million $\ \ km^2$"), 
                     breaks = mybreaks, 
                     labels = mybreaks %>% prettyNum(","), 
                     limits = c(0, max(aisdf$max95[aisdf$group=="bad_for"]))) + 
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) +   myThemeStuff + 
  geom_ribbon(aes(x=dist,ymin=min95,ymax=max95,shape=type),alpha=.3,fill="firebrick") + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  geom_path(data=detlab_brace, aes(x=x,y=y), size = .2,col="black") + 
  geom_text(data=detlab_text, aes(x=x,y=y,label=text),hjust=0,
            family="sans",size=3)+
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans")+
  theme(plot.margin = unit(c(0,.2,0,.01),"in"),
        legend.position = c(0.89,.88), 
        legend.margin = margin(0,0,0,0,unit="cm"), 
        legend.key.height = unit(0, unit = "cm"),
        legend.key.width=unit(0,unit="cm"),
        legend.title.align = 0,
        plot.tag = element_text(family = "sans", size = 9)) + 
  labs(tag = "a")

ggsave("Figures/supfig7a.png",supfig7a,
       width=88,height=54.83077,units="mm",dpi=1200) 
