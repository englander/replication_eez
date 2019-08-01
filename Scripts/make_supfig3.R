#Make Supplementary Figure 3
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

library(dplyr)
library(ggplot2)
library(purrr)
library(lfe)
library(latex2exp)
library(sandwich)
library(readr)
library(haven)

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
                      plot.tag = element_text(family = "sans", size = 9, face = 'bold')
)

load("Data/detdf.Rdata")

#Determine optimal bandwidth
badforresids <- lm(count_tenthveshours ~ inner + absdist + dist2 + dist3 + inner:dist + 
                     inner:dist2 + inner:dist3, data = filter(detdf, group=="bad_for"))


#Bandwidth
bwNeweyWest(badforresids, kernel = "Quadratic Spectral") #5.3
bwNeweyWest(badforresids, kernel = "Bartlett") #7.6


okforresids <- lm(count_tenthveshours ~ inner + absdist + dist2 + dist3 + inner:dist + 
                    inner:dist2 + inner:dist3, data = filter(detdf, group=="ok_for"))


#Bandwidth
bwNeweyWest(okforresids, kernel = "Quadratic Spectral") #2.7
bwNeweyWest(okforresids, kernel = "Bartlett") #2.3


domresids <- lm(count_tenthveshours ~ inner + absdist + dist2 + dist3 + inner:dist + 
                  inner:dist2 + inner:dist3, data = filter(detdf, group=="dom"))


#Bandwidth
bwNeweyWest(domresids, kernel = "Quadratic Spectral") #9.8
bwNeweyWest(domresids, kernel = "Bartlett") #21.1

rm(badforresids, domresids, okforresids)

#Export to stata to calculate confidence intervals
#Make distance an integer again (I will center it again in stata)
statout <- mutate(detdf, dist = if_else(type=="inner",dist+.5,dist-.5))

haven::write_dta(statout, path = "Data/detdf.dta")

#Load confidence intervals created in stata
#Load confidence intervals created in Stata
cidat <- bind_rows(
  read_csv("Data/Confidence_Intervals/supfig3a_ci.csv") %>% mutate(group='bad_for'),
  read_csv("Data/Confidence_Intervals/supfig3b_ci.csv") %>% mutate(group='ok_for'),
  read_csv("Data/Confidence_Intervals/supfig3c_ci.csv") %>% mutate(group='dom')
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

detdf <- left_join(detdf, 
                       dplyr::select(cidat, estimate, min95, max95, group, dist, type), 
                       by = c("group","dist", "type"))

#Check that point estimates are close to actual values (i.e. Stata regression was correct)
sum(detdf$count_tenthveshours - detdf$estimate)

detdf$type <- as.factor(detdf$type)
detdf$type <- relevel(detdf$type, ref = "outer")


textdf <- rbind(
  cbind(-25,0,"High seas"),
  cbind(25,0,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))


#bad_for
mybreaks = seq(from=0,to=8,by=2)

supfig3a <- ggplot(data = filter(detdf, group == "bad_for"),
                              aes(x=dist)) + 
  geom_point(aes(y = count_tenthveshours,shape=type), color = "firebrick", size = .7) + 
  geom_smooth(aes(y=estimate, shape=type),formula = y~poly(x,3), method = "lm",se = FALSE, color = "firebrick", size = .5) + 
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Outside EEZ",
                     values = c(1,16),
                     labels = c("Yes","No")) + 
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") +
  myThemeStuff + 
  geom_ribbon(aes(ymin=min95,ymax=max95,shape=type),alpha=.3,fill="firebrick") + 
  myThemeStuff + 
  scale_y_continuous(TeX("Trans. off events per 10,000 vessel hours"),
                     #limits = c(0, max(rawdf$max95)),
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(",")) + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) + 
  labs(tag = "a")


ggsave("Figures/supfig3a.png",supfig3a,
       width=88,height=54.83077,units="mm",dpi=1200) 


#dom
mybreaks = seq(from=0,to=12,by=3)

supfig3c <- ggplot(data = filter(detdf, group == "dom"),
                           aes(x=dist)) + 
  geom_point(aes(y = count_tenthveshours,shape=type), color = "dodgerblue2", size = .7) + 
  geom_smooth(aes(y=estimate, shape=type),formula = y~poly(x,3), method = "lm",se = FALSE, color = "dodgerblue2", size = .5) + 
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Outside EEZ",
                     values = c(1,16),
                     labels = c("Yes","No")) + 
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") +
  myThemeStuff + 
  geom_ribbon(aes(ymin=min95,ymax=max95,shape=type),alpha=.3,fill="dodgerblue2") + 
  myThemeStuff + 
  scale_y_continuous(TeX("Trans. off events per 10,000 vessel hours"),
                     #limits = c(0, max(rawdf$max95)),
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(",")) + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) + 
  labs(tag = "c")

ggsave("Figures/supfig3c.png",supfig3c,
       width=88,height=54.83077,units="mm",dpi=1200) 

#ok_for
mybreaks = seq(from=0,to=15,by=5)

supfig3b <- ggplot(data = filter(detdf, group == "ok_for"),
                             aes(x=dist)) + 
  geom_point(aes(y = count_tenthveshours,shape=type), color = "darkorange1", size = .7) + 
  geom_smooth(aes(y=estimate, shape=type),formula = y~poly(x,3), method = "lm",se = FALSE, color = "darkorange1", size = .5) + 
  geom_vline(xintercept=0, color = "red") + 
  scale_shape_manual("Outside EEZ",
                     values = c(1,16),
                     labels = c("Yes","No")) + 
  guides(shape = FALSE) + 
  geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans") +
  myThemeStuff + 
  geom_ribbon(aes(ymin=min95,ymax=max95,shape=type),alpha=.3,fill="darkorange1") + 
  myThemeStuff + 
  scale_y_continuous(TeX("Trans. off events per 10,000 vessel hours"),
                     #limits = c(0, max(rawdf$max95)),
                     breaks = mybreaks,
                     labels = mybreaks %>% prettyNum(",")) + 
  scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) + 
  labs(tag = "b")


ggsave("Figures/supfig3b.png",supfig3b,
       width=88,height=54.83077,units="mm",dpi=1200) 

