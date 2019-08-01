#Make Supplementary Figure 10
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")
options(scipen=999)

library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(lfe)

#Load data
load("Data/supfig10dat.Rdata")

#Load AIS data
load("Data/ais_cross_100km.Rdata")

aisdf <- filter(aisdf, dist <= 50 & group == "bad_for")

#Make Fig 10a first. 

#Missing one MarRegion (none of its species had migratory information in fishbase or sealifebase)
filter(supfig10dat, is.na(oceano)) %>% distinct(MarRegion)

#That's ok. Drop this MarRegion for estimating Fig. 10a. It will be included when creating Fig. 10b
aisdf <- filter(aisdf, MarRegion != 
                   supfig10dat$MarRegion[is.na(supfig10dat$oceano)])

supfig10dat <- filter(supfig10dat, !is.na(oceano))

#Create an inner indicator variable so type:MarRegion not colinear with FE
aisdf <- mutate(aisdf, inner = if_else(type=="inner",1,0))

#Shift distance in by .5
aisdf$dist <- aisdf$dist - .5

aisdf <- mutate(aisdf, dist2 = dist^2, dist3 = dist^3)

##Binned regressions

#Define 20 quantiles for MarRegions
qvec <- quantile(supfig10dat$oceano, probs = seq(0,1,1/20))

#Add column onto supfig10dat
supfig10dat$quant <- cut(supfig10dat$oceano, breaks = qvec, include.lowest = TRUE)

#All groups have 8-9 MarRegions
group_by(supfig10dat, quant) %>% summarise(count = n())

supfig10dat$quant <- as.character(supfig10dat$quant)

#Join onto aisdf
aisdf <- left_join(aisdf, dplyr::select(supfig10dat, MarRegion, quant), by = 'MarRegion')

aisdf$MarRegion <- as.factor(aisdf$MarRegion)

#Regression
reg <- felm(hours_thsqkm ~ inner:quant + dist:quant
            + dist2:quant + dist3:quant
            + dist:inner:quant + dist2:inner:quant
            + dist3:inner:quant |quant, data = aisdf)

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

#First grab relevant coefficients
badcoefs <- coefficients(reg)
badcoefs <- badcoefs[grep("inner:quant",names(badcoefs))]
#Drop those interacted with distance
badcoefs <- badcoefs[-grep("dist",names(badcoefs))]

#Reorder coefficients from smallest to largest
badcoefs <- c(badcoefs[20],badcoefs[1:19])

#Add an xvar for plotting
badcoefs <- as.data.frame(badcoefs) %>% mutate(xvar = 1:20)

#Flip coefficients so that plot is proportion of deterrence effect
badcoefs <- mutate(badcoefs, flip = -badcoefs)

#Calculate proportion
badcoefs <- mutate(badcoefs, prop = flip/(sum(badcoefs)*-1))

#Want quantiles making a positive contribution filled green and negative filled red
badcoefs <- mutate(badcoefs, contr = if_else(prop>0,"pos","neg") %>% as.factor())
badcoefs$contr <- relevel(badcoefs$contr, ref = "pos")

#fig 10a
supfig10a <- ggplot(data = badcoefs, aes(x=xvar, y = prop)) + 
    geom_bar(aes(fill = contr),stat="identity", color="black", alpha = .3, size = .1,
             position=position_dodge()) + 
    geom_smooth(formula = y~poly(x,1), method = "lm", se = FALSE,col="black",size=.2) + 
    scale_fill_manual(values = c("springgreen4","firebrick")) + 
    guides(fill=FALSE) + 
    myThemeStuff + 
    scale_x_continuous("Oceanodromous fraction group", breaks = 1:(1/.05)) + 
    scale_y_continuous("Contribution to total deterrence effect (%)", 
                       breaks = c(0,.2,.4,.6), 
                       labels = c("0%","20%","40%","60%")) + 
    labs(tag= "a")

ggsave("Figures/supfig10a.png",supfig10a,
       width=88,height=54.83077,units="mm",dpi=1200) 

#Is the trend significant?
lm(prop ~ xvar, data=badcoefs) %>% summary()
#No. p = .206. Note in Appendix and figure caption that linear trend is not significant.


##Now make fig 10b
rm(list=ls())

#Load data
load("Data/supfig10dat.Rdata")

#Load AIS data
load("Data/ais_cross_100km.Rdata")

aisdf <- filter(aisdf, dist <= 50 & group == "bad_for")

#Create an inner indicator variable so type:MarRegion not colinear with FE
aisdf <- mutate(aisdf, inner = if_else(type=="inner",1,0))

#Shift distance in by .5
aisdf$dist <- aisdf$dist - .5

aisdf <- mutate(aisdf, dist2 = dist^2, dist3 = dist^3)

##Binned regressions

#Define 20 quantiles for MarRegions
qvec <- quantile(supfig10dat$outflow, probs = seq(0,1,1/20))

#Add column onto supfig10dat
supfig10dat$quant <- cut(supfig10dat$outflow, breaks = qvec, include.lowest = TRUE)

#All groups have 8-9 MarRegions
group_by(supfig10dat, quant) %>% summarise(count = n())

supfig10dat$quant <- as.character(supfig10dat$quant)

#Join onto aisdf
aisdf <- left_join(aisdf, dplyr::select(supfig10dat, MarRegion, quant), by = 'MarRegion')

aisdf$MarRegion <- as.factor(aisdf$MarRegion)

#Regression
reg <- felm(hours_thsqkm ~ inner:quant + dist:quant
            + dist2:quant + dist3:quant
            + dist:inner:quant + dist2:inner:quant
            + dist3:inner:quant |quant, data = aisdf)

#First grab relevant coefficients
badcoefs <- coefficients(reg)
badcoefs <- badcoefs[grep("inner:quant",names(badcoefs))]
#Drop those interacted with distance
badcoefs <- badcoefs[-grep("dist",names(badcoefs))]

#Reorder coefficients from smallest to largest
badcoefs <- c(badcoefs[20],badcoefs[1:19])

#Add an xvar for plotting
badcoefs <- as.data.frame(badcoefs) %>% mutate(xvar = 1:20)

#Flip coefficients so that plot is proportion of deterrence effect
badcoefs <- mutate(badcoefs, flip = -badcoefs)

#Calculate proportion
badcoefs <- mutate(badcoefs, prop = flip/(sum(badcoefs)*-1))

#Want quantiles making a positive contribution filled green and negative filled red
badcoefs <- mutate(badcoefs, contr = if_else(prop>0,"pos","neg") %>% as.factor())
badcoefs$contr <- relevel(badcoefs$contr, ref = "pos")


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
                      plot.tag = element_text(family = "sans", size = 9,face='bold')
)


supfig10b <- ggplot(data = badcoefs, aes(x=xvar, y = prop)) + 
  geom_bar(aes(fill = contr),stat="identity", color="black", alpha = .3, size = .1,
           position=position_dodge()) + 
  geom_smooth(formula = y~poly(x,1), method = "lm", se = FALSE,col="black",size=.2) + 
  scale_fill_manual(values = c("springgreen4","firebrick")) + 
  guides(fill=FALSE) + 
  myThemeStuff + 
  scale_x_continuous("Ocean current direction group", breaks = 1:(1/.05)) + 
  scale_y_continuous("Contribution to total deterrence effect (%)", 
                     breaks = c(0,.2,.4,.6), 
                     labels = c("0%","20%","40%","60%")) + 
  labs(tag= "b")

ggsave("Figures/supfig10b.png",supfig10b,
       width=88,height=54.83077,units="mm",dpi=1200) 

#Is linear trend significant?
lm(prop ~ xvar, data=badcoefs) %>% summary()
#No. p = .89