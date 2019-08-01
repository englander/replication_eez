#Make Figure 4
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")

#No scientific notation
options(scipen=999)

library(dplyr)
library(ggplot2)
library(sf)
library(lfe)
library(rworldmap)
library(Formula)
library(latex2exp)
library(purrr)
library(sandwich)
library(readr)
library(haven)

#Make Fig 4a
load("Data/ais_cross_100km.Rdata")

#Filter to unauthorized foreign fishing effort within 50 km of boundary
aisdf <- filter(aisdf, dist <= 50 & group == "bad_for")

#Load average NPP within 50 km of each EEZ-sea region's high seas boundary between 2012 and 2016
load("Data/avgnpp.Rdata")

#Try first with averaged across MarRegion, not differentiating between inner and outer
aisdf <- left_join(aisdf, avgnpp, by = "MarRegion")

#Create an inner indicator variable so type:MarRegion not colinear with FE
aisdf <- mutate(aisdf, inner = if_else(type=="inner",1,0))

#Center data on middle of integer bin
aisdf$dist <- aisdf$dist - .5

aisdf$MarRegion <- as.factor(aisdf$MarRegion)

aisdf <- mutate(aisdf, dist2 = dist^2, dist3 = dist^3)

##Binned regressions
#Given quantile, output df with quantile assignments as a column based on MarRegion avg npp
quantFun <- function(myquant){
  
  #Filter avgnpp df to MarRegions with unauthorized foreign fishing
  mynpp <- filter(avgnpp, MarRegion %in% aisdf$MarRegion)
  
  #Quantiles
  qvec <- quantile(mynpp$npp, probs = seq(0,1,myquant))
  
  #Add column onto mynpp
  mynpp$npp_quant <- cut(mynpp$npp, breaks = qvec, include.lowest = TRUE)
  
  #Rename
  names(mynpp)[length(mynpp)] <- paste0("npp_quant",1/myquant)
  
  #Join onto aisdf
  regdf <- aisdf %>% 
    #Get quantiles
    left_join(dplyr::select(mynpp, -npp), by = c("MarRegion"))
  
  return(regdf)
}

#Function to do regression given myquant
regFun <- function(myquant){
  
  #Make df to use in regression
  df <- quantFun(myquant)
  
  #Formula for regression
  form <- paste0("hours_thsqkm ~ inner:npp_quant",1/myquant," + dist:npp_quant",1/myquant,
                 " + dist2:npp_quant",1/myquant," + dist3:npp_quant",1/myquant,
                 " + dist:inner:npp_quant",1/myquant," + dist2:inner:npp_quant",1/myquant,
                 " + dist3:inner:npp_quant",1/myquant," |npp_quant",1/myquant) %>%
    as.Formula()
  
  #Run regression
  reg <- felm(form, data = df)
  
  return(reg)
  
}

#Theme for ggplot
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
                      plot.margin = unit(c(0,.1,0,.01),"in"),
                      plot.tag = element_text(family = "sans", size = 9, face = 'bold')
)


#Run regression with 20 NPP quantile bins
badmodel <- regFun(.05)

#First grab relevant coefficients
badcoefs <- coefficients(badmodel)
badcoefs <- badcoefs[grep("inner:npp_quant",names(badcoefs))]
#Drop those interacted with distance
badcoefs <- badcoefs[-grep("dist",names(badcoefs))]

#Coefficients are ordered by smallest to largest quantile
#Add an xvar for plotting
badcoefs <- as.data.frame(badcoefs) %>% mutate(xvar = 1:20)

#Flip coefficients so that plot is proportion of deterrence effect
badcoefs <- mutate(badcoefs, flip = -badcoefs)

#Calculate proportion
badcoefs <- mutate(badcoefs, prop = flip/(sum(badcoefs)*-1))

#Want quantiles making a positive contribution filled green and negative filled red
badcoefs <- mutate(badcoefs, contr = if_else(prop>0,"pos","neg") %>% as.factor())
badcoefs$contr <- relevel(badcoefs$contr, ref = "pos")

fig4a <- ggplot(data = badcoefs, aes(x=xvar, y = prop,fill = contr)) + 
  geom_bar(stat="identity", color="black", alpha = .3, size = .1,
           position=position_dodge()) + 
  scale_fill_manual(values = c("springgreen4","firebrick")) + 
  guides(fill=FALSE) + 
  myThemeStuff + 
  scale_x_continuous("EEZ value group", breaks = 1:(1/.05)) + 
  scale_y_continuous("Contribution to total deterrence effect (%)", 
                     breaks = c(0,.2,.4,.6), 
                     labels = c("0%","20%","40%","60%")) + 
  labs(tag= "a")

ggsave("Figures/fig4a.pdf",fig4a,
       width=88,height=54.83077,units="mm",dpi=1200)

#Is trend in deterrence effects statistically significant?
lm(prop ~ xvar, data=badcoefs) %>% summary() #Yes. p-value = .012

#clean up
rm(list=ls())


#Now create Figs 4b, c, and d

load("Data/ais_badfor.Rdata")

#Load average NPP
load("Data/avgnpp.Rdata")

#What is median average npp for MarRegions in aisdf?
mednpp <- filter(avgnpp,  MarRegion %in% unique(aisdf$MarRegion)) %>% 
  summarise(mednpp = median(npp)) %>% as.matrix() %>% as.numeric()

#Calculate whether MarRegion is above or below median npp
nppmed <- filter(avgnpp, MarRegion %in% unique(aisdf$MarRegion)) %>% 
  mutate(above = if_else(npp >= mednpp, 1, 0)) %>% 
  dplyr::select(-npp)

#Join onto aisdf
aisdf <- left_join(aisdf, nppmed, by = "MarRegion")

#Clean up
rm(avgnpp, nppmed, mednpp)

#Create an indicator variable for whether geartype is drifting_longlines
aisdf <- mutate(aisdf, drifting = if_else(geartype=="drifting_longlines",1,0))

#Sum hours to dist-type-above-drifting level
plotdf <- group_by(aisdf, dist, type, above, drifting) %>% 
  summarise(hours = sum(hours)) %>% ungroup()

#Center on middle of integer bin
plotdf$dist <- plotdf$dist - .5

#Put outer on left side of plot
plotdf <- mutate(plotdf, dist = if_else(type=="outer",-dist,dist))

#Get total area of each 1 km buffer
load("Data/ais_vestype_100km.Rdata")

aisdf <- filter(aisdf, abs(dist) <=50) %>% 
  distinct(dist, type, Area_km2)

#Join onto plotdf
plotdf <- left_join(plotdf, aisdf, by = c("dist","type"))

#Calculate hours per million km^2
plotdf <- mutate(plotdf, hours_msqkm = (hours/Area_km2)*10^6)

rm(aisdf)

#Calculate optimal lag for confidence intervals
plotdf <- mutate(plotdf, absdist = abs(dist)) %>% 
  mutate(dist2 = absdist^2, dist3 = absdist^3)

#Create an inner indicator variable
plotdf <- mutate(plotdf, inner = if_else(type=="inner",1,0))

abovenarrow <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                    inner:dist2 + inner:dist3, data = filter(plotdf, above==1 & drifting==0))

#Bandwidth
bwNeweyWest(abovenarrow, kernel = "Quadratic Spectral") #4
bwNeweyWest(abovenarrow, kernel = "Bartlett") #4.97

belownarrow <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                    inner:dist2 + inner:dist3, data = filter(plotdf, above==0 & drifting==0))

#Bandwidth
bwNeweyWest(belownarrow, kernel = "Quadratic Spectral") #4.3
bwNeweyWest(belownarrow, kernel = "Bartlett") #5.1

abovedrift <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                   inner:dist2 + inner:dist3, data = filter(plotdf, above==1 & drifting==1))

#Bandwidth
bwNeweyWest(abovedrift, kernel = "Quadratic Spectral") #4.3
bwNeweyWest(abovedrift, kernel = "Bartlett") #5.2

belowdrift <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                   inner:dist2 + inner:dist3, data = filter(plotdf, above==0 & drifting==1))

#Bandwidth
bwNeweyWest(belowdrift, kernel = "Quadratic Spectral") #.9
bwNeweyWest(belowdrift, kernel = "Bartlett") #.2

rm(abovedrift, belowdrift, abovenarrow, belownarrow)

#Also want to make a plot for all fishing types
allfish <- group_by(plotdf, dist, type, above, Area_km2, absdist, dist2, dist3, inner) %>% 
  summarise(hours = sum(hours)) %>% ungroup() %>%
  mutate(hours_msqkm = (hours/Area_km2)*1000000)

allabove <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                 inner:dist2 + inner:dist3, data = filter(allfish, above==1))

#Bandwidth
bwNeweyWest(allabove, kernel = "Quadratic Spectral") #3.8
bwNeweyWest(allabove, kernel = "Bartlett") #4.4

allbelow <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                 inner:dist2 + inner:dist3, data = filter(allfish, above==0))

#Bandwidth
bwNeweyWest(allbelow, kernel = "Quadratic Spectral") #3.8
bwNeweyWest(allbelow, kernel = "Bartlett") #3.6

#Stack allfish onto plotdf
plotdf <- bind_rows(plotdf, mutate(allfish, drifting = as.numeric(NA)))

#Output df for stata to create confidence intervals
#Make distance an integer again (I will center it again in stata)
statout <- mutate(plotdf, dist = if_else(type=="inner",dist+.5,dist-.5))

haven::write_dta(statout, path = "Data/ais_fig4dat.dta")
rm(statout, allfish, allabove, allbelow)

#Load CI generated in ais_nppdrift_ci.do 
cidat <- bind_rows(
  read_csv("Data/Confidence_Intervals/fig4d_above_ci.csv") %>% mutate(above = 1, drifting = 0),
  read_csv("Data/Confidence_Intervals/fig4d_below_ci.csv") %>% mutate(above = 0, drifting = 0),
  read_csv("Data/Confidence_Intervals/fig4c_above_ci.csv") %>% mutate(above = 1, drifting = 1),
  read_csv("Data/Confidence_Intervals/fig4c_below_ci.csv") %>% mutate(above = 0, drifting = 1), 
  read_csv("Data/Confidence_Intervals/fig4b_above_ci.csv") %>% mutate(above = 1, drifting = as.numeric(NA)),
  read_csv("Data/Confidence_Intervals/fig4b_below_ci.csv") %>% mutate(above = 0, drifting = as.numeric(NA))
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

plotdf <- left_join(plotdf, 
                    dplyr::select(cidat, estimate, min95, max95, dist, type, above, drifting), 
                    by = c("dist", "type", "above","drifting"))

#Check that point estimates are close to actual values (i.e. Stata regression was correct)
sum(plotdf$hours_msqkm - plotdf$estimate)

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
                      plot.margin = unit(c(0,.01,0,.01),"in"),
                      plot.tag = element_text(family = "sans", size = 9, face='bold')
)


plotdf$type <- as.factor(plotdf$type)
#Want high seas to come first on plot
plotdf$type <- relevel(plotdf$type, ref = "outer")

plotdf$above <- as.factor(plotdf$above)
#Make it so that above comes first
plotdf$above <- relevel(plotdf$above, ref = "1")

#Write High seas and EEZs as text above axis instead of legend
textdf <- rbind(
  cbind(-25,-10000,"High seas"),
  cbind(25,-10000,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))


mybreaks <- seq(from=0,to=200000,by=50000)

fig4d <- 
  ggplot(data=filter(plotdf, drifting==0), aes(x=dist,y=hours_msqkm)) + 
  geom_point(aes(shape=type,color = above, fill = above), size = .5) + 
  geom_smooth(aes(shape=type,color = above, fill = above),formula = y~poly(x,3), 
              method = "lm",se = FALSE, size = .3) + 
  scale_color_manual("Value group",values = c("purple","dodgerblue2"),
                     labels = c("Above median","Below median")) + 
  scale_fill_manual("Value group",values = c("purple","dodgerblue2"),
                    labels = c("Above median","Below median")) + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) + 
  geom_vline(xintercept=0, color = "red") + 
  myThemeStuff + 
  theme(legend.position = c(0.87,.88), 
        legend.margin = margin(0,0,0,0,unit="cm"), 
        legend.key.height = unit(0, unit = "cm"),
        legend.key.width=unit(0,unit="cm"),
        legend.title.align = 0,
        plot.margin = unit(c(0,.01,0,.1),"in")) + 
  geom_ribbon(aes(ymin=min95,ymax=max95,shape=type,color = above, fill = above),
              alpha=.3, linetype =0 ) +
  guides(fill = FALSE, 
         color = guide_legend(order=1, override.aes=list(fill=NA)),
         shape = FALSE) + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  scale_y_continuous(TeX("Non-drifting longline fishing hours per m. $\ \ km^2$"),
                     breaks = mybreaks, 
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(-10000,max(plotdf$max95[plotdf$drifting==0]))) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "d")


ggsave("Figures/fig4d.pdf",fig4d,
       width=88,height=54.83077,units="mm",dpi=1200) 


#Drift
textdf <- rbind(
  cbind(-25,-1000,"High seas"),
  cbind(25,-1000,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))

mybreaks <- seq(from=0,to=75000,by=25000)

fig4c <- 
  ggplot(data=filter(plotdf, drifting==1), aes(x=dist,y=hours_msqkm)) + 
  geom_point(aes(shape=type,color = above, fill = above), size = .5) + 
  geom_smooth(aes(shape=type,color = above, fill = above),formula = y~poly(x,3), 
              method = "lm",se = FALSE, size = .3) + 
  scale_color_manual("Value group",values = c("purple","dodgerblue2"),
                     labels = c("Above median","Below median")) + 
  scale_fill_manual("Value group",values = c("purple","dodgerblue2"),
                    labels = c("Above median","Below median")) + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) + 
  geom_vline(xintercept=0, color = "red") + 
  myThemeStuff + 
  theme(legend.position = c(0.87,.88), 
        legend.margin = margin(0,0,0,0,unit="cm"), 
        legend.key.height = unit(0, unit = "cm"),
        legend.key.width=unit(0,unit="cm"),
        legend.title.align = 0,
        plot.margin = unit(c(0,.1,0,.01),"in")) + 
  geom_ribbon(aes(ymin=min95,ymax=max95,shape=type,color = above, fill = above),
              alpha=.3, linetype =0 ) +
  guides(fill = FALSE, 
         color = guide_legend(order=1, override.aes=list(fill=NA)),
         shape = FALSE) + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  scale_y_continuous(TeX("Drifting longline fishing hours per m. $\ \ km^2$"),
                     breaks = mybreaks, 
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(-1000,max(plotdf$max95[plotdf$drifting==1]))) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "c")


ggsave("Figures/fig4c.pdf",fig4c,
       width=88,height=54.83077,units="mm",dpi=1200) 


#Plot all fishing
textdf <- rbind(
  cbind(-25,-5000,"High seas"),
  cbind(25,-5000,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))

mybreaks <- seq(from=0,to=200000,by=50000)

fig4b <- 
  ggplot(data=filter(plotdf, is.na(drifting)), aes(x=dist,y=hours_msqkm)) + 
  geom_point(aes(shape=type,color = above, fill = above), size = .5) + 
  geom_smooth(aes(shape=type,color = above, fill = above),formula = y~poly(x,3), 
              method = "lm",se = FALSE, size = .3) + 
  scale_color_manual("Value group",values = c("purple","dodgerblue2"),
                     labels = c("Above median","Below median")) + 
  scale_fill_manual("Value group",values = c("purple","dodgerblue2"),
                    labels = c("Above median","Below median")) + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) + 
  geom_vline(xintercept=0, color = "red") + 
  myThemeStuff + 
  theme(legend.position = c(0.87,.88), 
        legend.margin = margin(0,0,0,0,unit="cm"), 
        legend.key.height = unit(0, unit = "cm"),
        legend.key.width=unit(0,unit="cm"),
        legend.title.align = 0,axis.title.y = element_text(hjust = 1),
        plot.margin = unit(c(0,.01,0,.1),"in")) + 
  geom_ribbon(aes(ymin=min95,ymax=max95,shape=type,color = above, fill = above),
              alpha=.3, linetype =0 ) +
  guides(fill = FALSE, 
         color = guide_legend(order=1, override.aes=list(fill=NA)),
         shape = FALSE) + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  scale_y_continuous(TeX("Unauthorized foreign fishing hours per m. $\ \ km^2$"),
                     breaks = mybreaks, 
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(-5000,max(plotdf$max95[is.na(plotdf$drifting)]))) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "b")


ggsave("Figures/fig4b.pdf",fig4b,
       width=88,height=54.83077,units="mm",dpi=1200) 




#Calculate log treatment effects
plotdf <- mutate(plotdf, logh = log(hours_msqkm), above_ind = if_else(above==1,1,0))

abovenarrow <- lm(logh ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                    inner:dist2 + inner:dist3, data = filter(plotdf, above==1 & drifting==0))
100*(exp(coefficients(abovenarrow)["inner"]) - 1) #-83%

belownarrow <- lm(logh ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                    inner:dist2 + inner:dist3, data = filter(plotdf, above==0 & drifting==0))
100*(exp(coefficients(belownarrow)["inner"]) - 1) #6%

abovedrift <- lm(logh ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                   inner:dist2 + inner:dist3, data = filter(plotdf, above==1 & drifting==1))
100*(exp(coefficients(abovedrift)["inner"]) - 1) #-74%


belowdrift <- lm(logh ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                   inner:dist2 + inner:dist3, data = filter(plotdf, above==0 & drifting==1))
100*(exp(coefficients(belowdrift)["inner"]) - 1) #-6%

aboveall <- lm(logh ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                 inner:dist2 + inner:dist3, data = filter(plotdf, above==1 & is.na(drifting)))
100*(exp(coefficients(aboveall)["inner"]) - 1) #-85%

belowall <- lm(logh ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                 inner:dist2 + inner:dist3, data = filter(plotdf, above==0 & is.na(drifting)))
100*(exp(coefficients(belowall)["inner"]) - 1) #-5%




