#Make Supplementary Figure 6
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")
options(scipen=999)

library(ggplot2)
library(dplyr)
library(purrr)
library(latex2exp)
library(sandwich)
library(readr)
library(lfe)
library(haven)

load("Data/supfig6df.Rdata")

#Given df, calculate optimal lag
optLag <- function(df){
  
  #drift
  reg <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
              inner:dist2 + inner:dist3, data = df)
  
  #Bandwidth
  quad <- bwNeweyWest(reg, kernel = "Quadratic Spectral")
  bart <- bwNeweyWest(reg, kernel = "Bartlett")
  
  #Take max and round up to nearest integer
  lag <- max(quad, bart) %>% ceiling()
  
  #Add lag as column onto df
  df <- mutate(df, lag = lag)
  
  return(df)
}

#Create unique cross-sectional identifier
medchardf <- mutate(medchardf, crossunit = paste0(var,as.character(above),group))

#Apply over crossunit
medchardf <- map_df(unique(medchardf$crossunit), function(x){
  optLag(filter(medchardf, crossunit==x))
})

#Write out for Stata
haven::write_dta(#Make dist an integer so can declare tsset in Stata. I will recenter dist in Stata before computing confidence intervals.
  medchardf %>% 
    mutate(dist = if_else(type=="inner",dist+.5,dist-.5)), 
  path = "Data/supfig6dat.dta")

#Function to read in confidence intervals generated in stata
readFun <- function(fname){
  
  read <- read_csv(paste0("Data/Confidence_Intervals/",fname,".csv")) %>% 
    mutate(crossunit = fname)
  
  return(read)
}

#Apply over vector of unique strboth
fnames <- unique(medchardf$crossunit)

#Make figure for tonnage
fnames <- fnames[grep("tonnage",fnames)]
medchardf <- filter(medchardf, var=='tonnage')

cidat <- map_df(fnames, function(x){
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

#Join
medchardf <- left_join(medchardf, 
                    dplyr::select(cidat, estimate, min95, max95, dist, type, crossunit), 
                    by = c("dist", "type", "crossunit"))

medchardf <- mutate(medchardf, type = as.factor(type))

#Want high seas to come first on plot
medchardf$type <- relevel(medchardf$type, ref = "outer")

#Check that point estimates are close to actual values (i.e. Stata regression was correct)
sum(medchardf$hours_msqkm - medchardf$estimate)

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
                      plot.tag = element_text(family = "sans", size = 9,face='bold')
)


#Write High seas and EEZs as text above axis instead of legend
textdf <- rbind(
  cbind(-25,0,"High seas"),
  cbind(25,0,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))

medchardf$above <- as.factor(medchardf$above)
#Want above median to come first on plot
medchardf$above <- relevel(medchardf$above, ref = "1")

plotFun <- function(mygroup, mychar, legtit, mytag, mycol, 
                    legend_h, legend_w){
  
  #DF for plot
  df <- filter(medchardf, group == mygroup & var==mychar)
  
  myplot <- 
    ggplot(data=df, aes(x=dist,y=hours_msqkm)) + 
    geom_point(aes(color = type, shape = above), size = .5) + 
    geom_smooth(aes(color = type, shape = above, linetype = above),formula = y~poly(x,3), 
                method = "lm",se = FALSE, size = .3) + 
    geom_ribbon(aes(ymin=min95,ymax=max95,fill = type, shape=above),
                alpha=.3, col=NA) +
    scale_color_manual("",values=c(mycol, mycol)) + 
    scale_fill_manual("",values=c(mycol, mycol)) + 
    scale_shape_manual(legtit,
                       values = c(16,1),
                       labels = c("Above median","Below median")) +
    scale_linetype_manual(legtit,
                          values = c("solid","dashed"),
                          labels = c("Above median","Below median")) +
    guides(color = FALSE, fill=FALSE, 
           linetype=FALSE,
           shape = guide_legend(order=1)) + 
    geom_vline(xintercept=0, color = "red") + 
    myThemeStuff + 
    theme(
      legend.position = c(legend_h, legend_w),
      legend.margin = margin(0,0,0,0,unit="cm"), 
      legend.key.height = unit(0, unit = "cm"),
      legend.key.width=unit(0,unit="cm"),
      legend.title.align = 0
    ) + 
    scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
    scale_y_continuous(TeX("Hours of AIS fishing per m. $\ \ km^2$")) + 
    labs(tag = mytag) + 
    geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans")
  
  
  ggsave(paste0("Figures/supfig6",mytag,".png"),
        myplot, 
        width=88,height=54.83077,units="mm",dpi=1200) 
  
}

plotFun("bad_for","tonnage","Gross Tonnage","a","firebrick",
        .87, .88)

plotFun("ok_for","tonnage","Gross Tonnage","b","darkorange1",
        .13, .25)

plotFun("dom","tonnage","Gross Tonnage","c","dodgerblue2",
        0.13,.88)


