#Make Supplementary Figure 8
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")
options(scipen=999)

library(dplyr)
library(ggplot2)
library(latex2exp)
library(purrr)
library(readr)
library(sandwich)

#Want to use total buffer area for each integer bin
load("Data/ais_vestype_100km.Rdata")

binarea <- filter(aisdf, abs(dist)<=50) %>% 
  dplyr::select(dist, type, Area_km2) %>% distinct()

#Load ais data
load("Data/ais_badfor.Rdata")


#For given geartype and number of top countries, calculate flag state nations
topFun <- function(gear,top,bottom){
  
  df <- filter(aisdf, geartype %in% gear) %>%
    group_by(flag) %>%
    summarise(hours = sum(hours)) %>% 
    arrange(desc(hours))
  
  flags <- df$flag[top:bottom]
  
  return(flags)
  
}
  
#Given desired geartype and number of flag state countries, create a df for plotting
makeDF <- function(gear, top, bottom){
  
  df <- filter(aisdf, geartype %in% gear & flag %in% topFun(gear, top, bottom)) %>%
    group_by(flag, dist, type) %>% 
    summarise(hours = sum(hours)) %>% ungroup() %>% 
    mutate(dist = dist-.5) %>% 
    mutate(dist = if_else(type=="outer",-dist,dist)) %>%
    #Add bin integer area
    left_join(binarea, by = c("dist","type")) %>%
    #Calculate hours per million km^2
    mutate(hours_msqkm = (hours/Area_km2)*10^6) %>% 
    mutate(absdist = abs(dist), inner = if_else(type=="inner",1,0)) %>% 
    mutate(dist2 = absdist^2, dist3 = absdist^3)
  
  #Add gear variable
  if(length(gear)>1){
    df <- mutate(df, gear = "all")
  } else{
    df <- mutate(df, gear = gear)
  }
  
  return(df)
}

#Given makeDF, calculate optimal lag for a flag
optLag <- function(df){

  #drift
  reg <- lm(hours_msqkm ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                inner:dist2 + inner:dist3, data = df)
  
  #Bandwidth
  quad <- bwNeweyWest(reg, kernel = "Quadratic Spectral")
  bart <- bwNeweyWest(reg, kernel = "Bartlett")
  
  #Take max and round up to nearest integer
  lag <- max(quad, bart) %>% ceiling()
  
  return(lag)
}


#Create df of optimal lag for all gear-flag combinations
#Apply over this vector
gearlist <- list(unique(aisdf$geartype), 
                "drifting_longlines",
                "fixed_gear",
                "purse_seines",      
                "squid_jigger",
                "trawlers")

#Create df for stata
statdf <- map_df(gearlist, function(x){
  makeDF(x, 1, 3)
})

#Need to add zeros
zerodf <- distinct(statdf, dist, type, Area_km2, absdist, inner, dist2, dist3)

distflaggear <- distinct(statdf, flag, gear)

zerodf <- lapply(1:18, function(x){
  mutate(zerodf, flag = distflaggear[x,1] %>% as.character(), 
         gear = distflaggear[x,2] %>% as.character())
})

zerodf <- do.call("rbind", zerodf)

zerodf <- left_join(zerodf, statdf)

zerodf$hours[is.na(zerodf$hours)] <- 0
zerodf$hours_msqkm[is.na(zerodf$hours_msqkm)] <- 0

#Calculate optimal lag
zerodf <- map_df(1:18, function(x){
  
  df <- filter(zerodf, flag == distflaggear[x,1] %>% as.character() & 
                 gear == distflaggear[x,2] %>% as.character())
  
  lag <- optLag(df)

  df <- mutate(df, lag = lag)
})

#Create unique identifier
zerodf$strboth <- paste0(zerodf$flag,zerodf$gear,zerodf$lag)


#Write out for Stata
haven::write_dta(#Make dist an integer so can declare tsset in Stata. I will recenter dist in Stata before computing confidence intervals.
  mutate(zerodf, dist = if_else(type=="outer",dist-.5,dist+.5)), 
                 "Data/supfig8dat.dta")

rm(gearlist, statdf, aisdf, binarea, distflaggear)

#Function to read in confidence intervals generated in stata
readFun <- function(fname){
  
  read <- read_csv(paste0("Data/Confidence_Intervals/",fname,".csv")) %>% 
    mutate(strboth = fname)

  return(read)
}

#Apply over vector of unique strboth
fnames <- unique(zerodf$strboth)

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

zerodf <- left_join(zerodf, 
                    dplyr::select(cidat, estimate, min95, max95, dist, type, strboth), 
                    by = c("dist", "type", "strboth"))

zerodf <- mutate(zerodf, type = as.factor(type))

#Want high seas to come first on plot
zerodf$type <- relevel(zerodf$type, ref = "outer")

#Check that point estimates are close to actual values (i.e. Stata regression was correct)
sum(zerodf$hours_msqkm - zerodf$estimate)

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


plotFun <- function(mygear, geartit, mytag){

  #DF for plot
  df <- filter(zerodf, gear == mygear)# %>% rename(`Flag State` = flag)
  
  #y-axis title
  ytit <- paste0("Hours of",geartit," fishing per m. $\ \ km^2$")
  
  myplot <- 
    ggplot(data=df, aes(x=dist,y=hours_msqkm)) + 
    geom_point(aes(shape=type,color = flag, fill = flag), size = .5) + 
    geom_smooth(aes(shape=type,color = flag, fill = flag),formula = y~poly(x,3), 
                method = "lm",se = FALSE, size = .3) + 
    scale_color_viridis_d("Flag State") + 
    scale_fill_viridis_d("Flag State") + 
    scale_shape_manual("Location",
                       values = c(1,16),
                       labels = c("High seas","EEZs")) + 
    geom_vline(xintercept=0, color = "red") + 
    myThemeStuff + 
    theme(
      legend.margin = margin(0,0,0,0,unit="cm"), 
      legend.key.height = unit(0, unit = "cm"),
      legend.key.width=unit(0,unit="cm"),
      legend.title.align = 0
    ) + 
    geom_ribbon(aes(ymin=min95,ymax=max95,shape=type,color = flag, fill = flag),
                alpha=.3, linetype =0 ) +
    guides(fill = FALSE, 
           color = guide_legend(order=1, override.aes=list(fill=NA))) + 
    scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
    scale_y_continuous(TeX(ytit)) + 
    labs(tag = mytag)
  
  ggsave(paste0("Figures/supfig8",mytag,".png"),
        myplot, 
        width=88,height=54.83077,units="mm",dpi=1200) 
  
}


plotFun("all","","a")
plotFun("drifting_longlines"," drifting longline","b")
plotFun("fixed_gear"," fixed gear","c")
plotFun("purse_seines"," purse seine","d")
plotFun("squid_jigger"," squid jigger","e")
plotFun("trawlers"," trawler","f")

