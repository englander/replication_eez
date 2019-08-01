#Make Figure 2
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")

#No scientific notation
options(scipen=999)

library(dplyr)
library(ggplot2)
library(latex2exp)
library(lfe)
library(readr)
library(purrr)
library(sandwich)
library(haven)

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
                      plot.margin = unit(c(.01,.01,.01,.01),"in"),
                      plot.tag = element_text(family = "sans", size = 9, face = 'bold')
)


load("Data/ais_vestype_100km.Rdata")

#Export to stata to calculate confidence intervals
#Make distance an integer again (I will center it again in stata)
statout <- mutate(aisdf, dist = if_else(type=="inner",dist+.5,dist-.5))

haven::write_dta(statout, path = "Data/ais_vestype_100km.dta")

#Filter to within 50 km of a boundary
aisdf <- filter(aisdf, abs(dist) <=50) %>% 
  arrange(dist, type, group)

#Sum fishing hours over vessel type
tot <- group_by(aisdf, dist, type, absdist, dist2, dist3, Area_km2, inner) %>% 
  summarise(hours = sum(hours)) %>% ungroup() %>%
  mutate(hours_msqkm = (hours/Area_km2)*1000000)

#Export to stata to calculate confidence intervals for Fig 2A
#First make distance back to integers
tot_stata <- mutate(tot, dist = if_else(type=="outer",dist-.5,dist+.5)) %>% 
  dplyr::select(-absdist, -dist2, -dist3)

write_dta(tot_stata, path = "Data/ais_tot.dta")
rm(tot_stata)

#Determine optimal bandwidth for Newey-West standard errors
{

  totresids <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                    inner:dist2 + inner:dist3, data = tot)
  
  #Bandwidth
  bwNeweyWest(totresids, kernel = "Quadratic Spectral") #.4
  bwNeweyWest(totresids, kernel = "Bartlett") #1.7


  badforresids <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                     inner:dist2 + inner:dist3, data = filter(aisdf, group=="bad_for"))

  #Bandwidth
bwNeweyWest(badforresids, kernel = "Quadratic Spectral") #2.8
bwNeweyWest(badforresids, kernel = "Bartlett") #3.1


domresids <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                    inner:dist2 + inner:dist3, data = filter(aisdf, group=="dom"))

bwNeweyWest(domresids, kernel = "Quadratic Spectral") #5.3
bwNeweyWest(domresids, kernel = "Bartlett") #6.3



okforresids <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                    inner:dist2 + inner:dist3, data = filter(aisdf, group=="ok_for"))

bwNeweyWest(okforresids, kernel = "Quadratic Spectral") #3.2
bwNeweyWest(okforresids, kernel = "Bartlett") #3.3

rm(totresids, badforresids, domresids, okforresids)

}

#Load CI generated in make_Fig2_ci.do
citot <- read_csv("Data/Confidence_Intervals/fig2a_ci.csv") %>% mutate(group = "tot")
cibad <- read_csv("Data/Confidence_Intervals/fig2c_ci.csv") %>% mutate(group = "bad_for")
ciok <- read_csv("Data/Confidence_Intervals/fig2d_okfor_ci.csv") %>% mutate(group = "ok_for")
cidom <- read_csv("Data/Confidence_Intervals/fig2d_dom_ci.csv") %>% mutate(group = "dom")

#Stack
cidat <- bind_rows(citot, cibad, ciok, cidom)

#Create dist variable
cidat$dist <- gsub( "\\..*$", "", cidat$parm)
cidat$dist <- as.numeric(cidat$dist) - 50.5

#Create type variable
cidat$type <- "outer"
cidat$type[grep("1.inner",cidat$parm)] <- "inner"

#Drop non-relevant parameter values (e.g. inner with dist = -30 doesn't actually exist)
cidat <- filter(cidat, (type=="outer" & dist <= -1.5) | 
                  (type == "inner" & dist >= 1.5))

#Only need estimate (which should be very close to hours_msqkm), min95, max95, dist, type, and group variables
#First add tot to aisdf
aisdf <- bind_rows(aisdf, 
                        mutate(tot, group = "tot"))

aisdf <- left_join(aisdf, 
                        dplyr::select(cidat, estimate, min95, max95, dist, type, group), 
                        by = c("dist", "type", "group"))

#Check that point estimates are close to actual values (i.e. Stata regression was correct)
sum(aisdf$hours_msqkm - aisdf$estimate)


aisdf$type <- as.factor(aisdf$type)
#Want high seas to come first on plot
aisdf$type <- relevel(aisdf$type, ref = "outer")

aisdf$group <- as.factor(aisdf$group)

#Write High seas and EEZs as text above axis instead of legend
textdf <- rbind(
  cbind(-25,0,"High seas"),
  cbind(25,0,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))


#2A
mybreaks <- seq(from=0,to=250000,by=50000)

fig2a <- ggplot(data = filter(aisdf, group == "tot"), 
                             aes(x=dist, y = hours_msqkm)) + 
  geom_point(aes(shape=type),color = "lightseagreen", size = .5) + 
  geom_smooth(aes(shape=type),formula = y~poly(x,3), 
              method = "lm",se = FALSE, color = "lightseagreen", size = .3) + 
  scale_y_continuous(TeX("Hours of AIS fishing per million $\ \ km^2$"),
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(0, max(aisdf$max95))) +
  geom_vline(xintercept=0, color="red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) + 
  myThemeStuff + 
  geom_ribbon(aes(ymin=min95,ymax=max95,shape=type),alpha=.3,fill="lightseagreen") + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  theme(plot.margin = unit(c(0,.2,0,.01),"in"),
        legend.position = c(0.89,.88), 
        legend.margin = margin(0,0,0,0,unit="cm"), 
        legend.key.height = unit(0, unit = "cm"),
        legend.key.width=unit(0,unit="cm"),
        legend.title.align = 0) + 
  guides(shape = FALSE) + 
    geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "a")

    
ggsave("Figures/fig2a.pdf",fig2a,
       width=88,height=54.83077,units="mm",dpi=1200) 


#2C

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
fig2c <- ggplot(data = filter(aisdf, group == "bad_for")) + 
  geom_point(aes(x=dist, y = hours_msqkm, shape = type),size = .5,color="firebrick") + 
  geom_smooth(aes(x=dist, y = hours_msqkm, shape = type),
              formula = y~poly(x,3), method = "lm",se = FALSE, size = .3,color="firebrick") + 
  scale_y_continuous(TeX("Unauthorized foreign fishing hours per m. $\ \ km^2$"), 
                    breaks = mybreaks, 
                    labels = mybreaks %>% prettyNum(","), 
                    limits = c(0, max(aisdf$max95[aisdf$group=="bad_for"]))) + 
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) +   
  myThemeStuff + 
  geom_ribbon(aes(x=dist,ymin=min95,ymax=max95,shape=type),alpha=.3,fill="firebrick") + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  geom_path(data=detlab_brace, aes(x=x,y=y), size = .2,col="black") + 
    geom_text(data=detlab_text, aes(x=x,y=y,label=text),hjust=0,
              family="sans",size=3,col="black") +
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans", col = "black")+
  theme(plot.margin = unit(c(0,.2,0,.01),"in"),
        legend.position = c(0.87,.88), 
        legend.margin = margin(0,0,0,0,unit="cm"), 
        legend.key.height = unit(0, unit = "cm"),
        legend.key.width=unit(0,unit="cm"),
        legend.title.align = 0,axis.title.y = element_text(hjust = 1)) + 
  labs(tag = "c")

ggsave("Figures/fig2c.pdf",fig2c,
       width=88,height=54.83077,units="mm",dpi=1200) 


#Fig 2d


#Make it so that ok_for is first on group legend, then dom
aisdf$group <- relevel(aisdf$group, ref = "ok_for")
aisdf$group <- relevel(aisdf$group, ref = "bad_for")

mybreaks <- seq(from=0,to=32000,by=8000)

fig2d <- ggplot(data = filter(aisdf, 
                                          group == "ok_for" | group=="dom"), 
                             aes(x=dist, y = hours_msqkm)) + 
  geom_point(aes(shape=type,color = group, fill = group), size = .5) + 
  geom_smooth(aes(shape=type,color = group, fill = group),formula = y~poly(x,3), 
              method = "lm",se = FALSE, size = .3) + 
  scale_y_continuous(TeX("Hours of AIS fishing per million $\ \ km^2$"), 
                     breaks = mybreaks, 
                     labels = mybreaks %>% prettyNum(","), 
                     limits = c(0, (max(aisdf$max95[aisdf$group=="dom" | aisdf$group=="ok_for"])+1))) + 
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) + 
    scale_color_manual("Vessel type",
                       values = c("darkorange1","dodgerblue2"),
                       labels = c("Authorized foreign","Domestic")) +
    scale_fill_manual("Vessel type",
                      values = c("darkorange1","dodgerblue2"), 
                      labels = c("Authorized foreign","Domestic")) + 
    geom_ribbon(aes(ymin=min95,ymax=max95,shape=type,color = group, fill = group),
                alpha=.3, linetype =0 ) +
  guides(fill = FALSE, 
         color = guide_legend(order=1, override.aes=list(fill=NA)),
         shape = FALSE) + 
  myThemeStuff + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  theme(legend.position = c(0.17,.88), 
          legend.margin = margin(0,0,0,0,unit="cm"), 
          legend.key.height = unit(0, unit = "cm"),
        legend.key.width=unit(0,unit="cm"), 
          plot.margin = unit(c(0,.01,0,.2),"in"),
        legend.key.size=unit(0,unit="cm")) + 
  labs(tag = "d") + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans")



ggsave("Figures/fig2d.pdf",fig2d,
       width=88,height=54.83077,units="mm",dpi=1200) 





#Now make Fig 2b
#Load VBD data summed over days and over EEZ-sea regions to integer bin level 
load("Data/vbd_100km.Rdata")

#Export to stata to calculate confidence intervals
#Make distance an integer again (I will center it again in stata)
statout <- mutate(vbd, dist = if_else(type=="inner",dist+.5,dist-.5))

haven::write_dta(statout, path = "Data/vbd_100km.dta")

#Filter to within 50 km of boundary
vbd <- filter(vbd, absdist <= 50) %>% 
  arrange(dist)

#Determine optimal bandwidth for Newey-West standard errors
resids <- lm(count_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
               inner:dist2 + inner:dist3, data = vbd)

#Bandwidth
bwNeweyWest(resids, kernel = "Quadratic Spectral") #4.8
bwNeweyWest(resids, kernel = "Bartlett") #5.8

#Load CI generated in make_Fig2_ci.do
cidat <- read_csv("Data/Confidence_Intervals/fig2b_ci.csv")

#Create dist variable
cidat$dist <- gsub( "\\..*$", "", cidat$parm)
cidat$dist <- as.numeric(cidat$dist) - 50.5

#Create type variable
cidat$type <- "outer"
cidat$type[grep("1.inner",cidat$parm)] <- "inner"

#Drop non-relevant parameter values (e.g. inner with dist = -30 doesn't actually exist)
cidat <- filter(cidat, (type=="outer" & dist <= -1.5) | 
                  (type == "inner" & dist >= 1.5))


#Only need estimate (which should be very close to hours_thsqkm), min95, max95, dist, type
vbd <- left_join(vbd, 
                            dplyr::select(cidat, estimate, min95, max95, dist, type), 
                            by = c("dist", "type"))

#Check that point estimates are close to actual values (i.e. Stata regression was correct)
sum(vbd$count_msqkm - vbd$estimate)

vbd$type <- as.factor(vbd$type)
#Want high seas to come first on plot
vbd$type <- relevel(vbd$type, ref = "outer")

#Label Axes
textdf <- rbind(
  cbind(-25,0,"High seas"),
  cbind(25,0,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))

mybreaks <- seq(from=0,to=8000,by=2000)

#plot
fig2b <- ggplot(data = vbd, 
                  aes(x=dist, y = count_msqkm)) + 
  geom_point(aes(shape=type),color = "black", size = .5) + 
  geom_smooth(aes(shape=type),formula = y~poly(x,3), method = "lm",se = FALSE, color = "black", size = .3) + 
  geom_vline(xintercept=0,color="red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) + 
  myThemeStuff + 
  geom_ribbon(aes(shape=type,ymin=min95,ymax=max95),alpha=.2,fill="black") + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
  scale_y_continuous(TeX("Nighttime vessel count per million $\ km^2$"),
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(","),
                     limits = c(0, max(vbd$max95))) + 
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") +
  theme(legend.position = c(0.11,.88), 
        legend.margin = margin(0,0,0,0,unit="cm"), 
        legend.key.height = unit(0, unit = "cm"),
        legend.key.width=unit(0,unit="cm"),
        legend.title.align = 0) + 
  labs(tag = "b")

ggsave("Figures/fig2b.pdf",fig2b,
       width=88,height=54.83077,units="mm",dpi=1200) 

