#Make Supplementary Figure 1
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")

library(dplyr)
library(haven)
library(ggplot2)
library(sandwich)
library(latex2exp)
library(tidyr)
library(readr)

load("Data/supfig1df.Rdata")

#What should Newey-West bandwidths be?

#depth
depthreg <- lm(depth ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                inner:dist2 + inner:dist3, data = supfig1df)

#Bandwidth
bwNeweyWest(depthreg, kernel = "Quadratic Spectral") #8.9
bwNeweyWest(depthreg, kernel = "Bartlett") #17.2


#npp
nppreg <- lm(npp ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                 inner:dist2 + inner:dist3, data = supfig1df)

#Bandwidth
bwNeweyWest(nppreg, kernel = "Quadratic Spectral") #31.3.
bwNeweyWest(nppreg, kernel = "Bartlett") #139.7
#Use 32 as lag instead of 140 in this case because 140 is too large

#sst
sstreg <- lm(sst ~ inner + absdist + dist2 + dist3 + inner:absdist + 
               inner:dist2 + inner:dist3, data = supfig1df)

#Bandwidth
bwNeweyWest(sstreg, kernel = "Quadratic Spectral") #3.7
bwNeweyWest(sstreg, kernel = "Bartlett") #3.6

#Output data for stata to compute confidence intervals
write_dta(mutate(supfig1df, #Make dist an integer so can tsset in Stata. I will recenter dist in Stata after I have declared tsset.
                        dist = if_else(type=="outer",dist-.5,dist+.5)),
                 path = "Data/supfig1dat.dta")

rm(depthreg, sstreg, nppreg)

#Drop extra columns
supfig1df <- dplyr::select(supfig1df, -absdist, -dist2, -dist3, -inner)

#Create one row for each variable-dist
supfig1df <- gather(supfig1df, variable, value,  -dist, -type)

#Load confidence intervals created in Stata
cidat <- bind_rows(
  read_csv("Data/Confidence_Intervals/supfig1a_ci.csv") %>% mutate(variable='depth'),
  read_csv("Data/Confidence_Intervals/supfig1b_ci.csv") %>% mutate(variable='npp'),
  read_csv("Data/Confidence_Intervals/supfig1c_ci.csv") %>% mutate(variable='sst')
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

supfig1df <- left_join(supfig1df, 
                       dplyr::select(cidat, estimate, min95, max95, dist, variable), 
                       by = c("dist", "variable"))

#If value is missing, set estimate, min95, and max95 equal to NA too
supfig1df$estimate[is.na(supfig1df$value)] <- as.numeric(NA)
supfig1df$min95[is.na(supfig1df$value)] <- as.numeric(NA)
supfig1df$max95[is.na(supfig1df$value)] <- as.numeric(NA)

#Check that point estimates are close to actual values (i.e. Stata regression was correct)
sum(supfig1df$value[!is.na(supfig1df$value)] - supfig1df$estimate[!is.na(supfig1df$value)])

supfig1df$type <- as.factor(supfig1df$type)
#Want high seas to come first on plot
supfig1df$type <- relevel(supfig1df$type, ref = "outer")

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

#Add high seas and EEZ text
textdf <- rbind(
  cbind(-25,-4.19,"High seas"),
  cbind(25,-4.19,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))

#Make figure supfig1a
supfig1a <- ggplot(data = filter(supfig1df, variable=='depth'), 
                    aes(x=dist, y = value)) + 
  geom_point(aes(shape=type),color = "lightseagreen", size = .5) + 
  geom_smooth(aes(shape=type),formula = y~poly(x,3), 
              method = "lm",se = FALSE, color = "lightseagreen", size = .3) + 
  scale_y_continuous("Average Depth (km)") +
  geom_vline(xintercept=0, color="red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) + 
  myThemeStuff + 
  guides(shape = FALSE) + 
  geom_ribbon(aes(ymin=min95,ymax=max95,shape=type),alpha=.3,fill="lightseagreen") + 
  scale_x_continuous("Distance to EEZ-high seas boundary (km)") +
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "a")

ggsave("Figures/supfig1a.png",supfig1a,
       width=88,height=54.83077,units="mm",dpi=1200) 


#Make supfig1b

#Add high seas and EEZ text
textdf <- rbind(
  cbind(-25,360,"High seas"),
  cbind(25,360,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))

#Make plot
supfig1b <- ggplot(data = filter(supfig1df, variable=='npp'), 
                  aes(x=dist, y = value)) + 
  geom_point(aes(shape=type),color = "lightseagreen", size = .5) + 
  geom_smooth(aes(shape=type),formula = y~poly(x,3), 
              method = "lm",se = FALSE, color = "lightseagreen", size = .3) + 
  scale_y_continuous(TeX("Average NPP (mg Carbon / $ km^2$ / day)")) +
  geom_vline(xintercept=0, color="red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) + 
  myThemeStuff + 
  guides(shape = FALSE) + 
  geom_ribbon(aes(ymin=min95,ymax=max95,shape=type),alpha=.3,fill="lightseagreen") + 
  scale_x_continuous("Distance to EEZ-high seas boundary (km)") +
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "b")

ggsave("Figures/supfig1b.png",supfig1b,
       width=88,height=54.83077,units="mm",dpi=1200) 

#Make supfig1c

textdf <- rbind(
  cbind(-25,23.3,"High seas"),
  cbind(25,23.3,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))

#Make plot
supfig1c <- ggplot(data = filter(supfig1df, variable=='sst'), 
                  aes(x=dist, y = value)) + 
  geom_point(aes(shape=type),color = "lightseagreen", size = .5) + 
  geom_smooth(aes(shape=type),formula = y~poly(x,3), 
              method = "lm",se = FALSE, color = "lightseagreen", size = .3) + 
  scale_y_continuous(TeX("Average SST (degrees)")) +
  geom_vline(xintercept=0, color="red") + 
  scale_shape_manual("Location",
                     values = c(1,16),
                     labels = c("High seas","EEZs")) + 
  myThemeStuff + 
  guides(shape = FALSE) + 
  geom_ribbon(aes(ymin=min95,ymax=max95,shape=type),alpha=.3,fill="lightseagreen") + 
  scale_x_continuous("Distance to EEZ-high seas boundary (km)") +
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") + 
  labs(tag = "c")

ggsave("Figures/supfig1c.png",supfig1c,
       width=88,height=54.83077,units="mm",dpi=1200) 
