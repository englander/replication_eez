#Make Supplementary Figures 4a to e
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")

library(dplyr)
library(ggplot2)
library(sandwich)
library(lfe)
library(latex2exp)
library(readr)

options(scipen=999)

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

#Load data
load("Data/ais_vestype_100km.Rdata")

#Only need unauthorized foreign
aisdf <- filter(aisdf, group == "bad_for")


##Estimate optimal lags
{
  a <- lm(hours_msqkm ~ inner + absdist + inner:absdist, data = filter(aisdf, absdist<50))
  
  #Bandwidth
  bwNeweyWest(a, kernel = "Quadratic Spectral") #4.7
  bwNeweyWest(a, kernel = "Bartlett") #5.97

  b <- lm(hours_msqkm ~ inner + absdist + inner:absdist, data = filter(aisdf, absdist<50 & absdist > 10))
  
  #Bandwidth
  bwNeweyWest(b, kernel = "Quadratic Spectral") #4.6
  bwNeweyWest(b, kernel = "Bartlett") #5.5
  
  c <- lm(hours_msqkm ~ inner + absdist + inner:absdist, data = filter(aisdf, absdist<100 & absdist > 10))
  
  #Bandwidth
  bwNeweyWest(c, kernel = "Quadratic Spectral") #5.3
  bwNeweyWest(c, kernel = "Bartlett") #7.4
  
  d <- lm(hours_msqkm ~ inner + absdist + inner:absdist, data = filter(aisdf, absdist<50 & absdist > 25))
  
  #Bandwidth
  bwNeweyWest(d, kernel = "Quadratic Spectral") #4.1
  bwNeweyWest(d, kernel = "Bartlett") #4.7
  
  e <- lm(hours_msqkm ~ inner + absdist + inner:absdist, data = filter(aisdf, absdist<100 & absdist > 25))
  
  #Bandwidth
  bwNeweyWest(e, kernel = "Quadratic Spectral") #5.1
  bwNeweyWest(e, kernel = "Bartlett") #6.7
    
  rm(a, b, c, d, e)
}

##Bring in confidence intervals from Stata
#Function to load one confidence interval dataset and clean
loadCI <- function(letter, donut, band){
  
  cidat <- read_csv(paste0("Data/Confidence_Intervals/supfig4",letter,"_ci.csv"))
  
  #Create dist column
  cidat$dist <- gsub( "\\..*$", "", cidat$parm)
  cidat$dist <- as.numeric(cidat$dist) - (band+.5)
  
  #Create type variable
  cidat$type <- "outer"
  cidat$type[grep("1.inner",cidat$parm)] <- "inner"
  
  #Drop non-relevant parameter values (e.g. inner with dist = -30 doesn't actually exist)
  cidat <- filter(cidat, (type=="outer" & dist <= -1.5) | 
                    (type == "inner" & dist >= 1.5))
  
  #Only need estimate (which should be very close to hours_msqkm), min95, max95, dist, type
  cidat <- dplyr::select(cidat, estimate, min95, max95, dist, type)
  
  return(cidat)
}

#Join confidence interval onto data
joinFun <- function(letter, donut, band){
  
  cidat <- loadCI(letter, donut, band)
  
  #Join cidat onto aisdf
  plotdf <- left_join(aisdf, cidat, by = c("dist","type"))
  
  #Drop rows outside bandwidth
  plotdf <- filter(plotdf, absdist < band)
  
  #Set hours_msqkm NA for dist inside donut
  plotdf$hours_msqkm[plotdf$absdist<donut] <- NA
  
  return(plotdf)
}


##Plot
#Write High seas and EEZs as text above axis instead of legend
textdf <- rbind(
  cbind(-25,0,"High seas"),
  cbind(25,0,"EEZs")
) %>% as.data.frame() %>% tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))

aisdf$type <- as.factor(aisdf$type)
#Want high seas to come first on plot
aisdf$type <- relevel(aisdf$type, ref = "outer")

#Write a function to create plot of given bandwidth, donut hole, and letter
#My side means whether to add .05 in margin to left or right side
donutplot <- function(letter, donut, band, mybreaks, myside){
  
  #Create df for use in plotting
  plotdf <- joinFun(letter, donut, band)
  
  #Add .05 in margin to left or right side of plot?
  if(myside=="right"){
    mymarg <- c(.01,.05,.01,.01)
  } else{
    mymarg <- c(.01,.01,.01,.05)
  }
  
  plot <- ggplot(data = plotdf, aes(x=dist)) + 
    geom_point(aes(y = hours_msqkm, shape = type),size = .3,color="firebrick") + 
    geom_line(aes(y=estimate, shape = type), size = .3, color="firebrick") + 
    scale_y_continuous(TeX("Unauthorized foreign fishing hours per m. $\ \ km^2$"),
                       breaks = mybreaks,
                       labels = mybreaks %>% prettyNum(","),
                       limits = c(0, max(plotdf$max95))) +
    geom_vline(xintercept=0, color = "red") + 
    scale_shape_manual("Location",
                       values = c(16,1)) + 
    myThemeStuff + 
    geom_ribbon(aes(ymin=min95,ymax=max95,shape=type),alpha=.3,fill="firebrick") + 
    scale_x_continuous(TeX("Distance to EEZ-high seas boundary (km)")) +
    guides(shape = FALSE) + 
    geom_text(data=textdf, aes(x=x,y=y,label=label),  size = 2.5, family = "sans", col = "black")+
    theme(plot.margin = unit(mymarg,"in"),
          legend.position = c(0.87,.88), 
          legend.margin = margin(0,0,0,0,unit="cm"), 
          legend.key.height = unit(0, unit = "cm"),
          legend.key.width=unit(0,unit="cm"),
          legend.title.align = 0,axis.title.y = element_text(hjust = 1),
          plot.tag = element_text(family = "sans", size = 9, face='bold')) + 
    labs(tag = letter)
  
  #If donut>0, draw vertical lines and write Donut Hole
  if(donut>0){
    
    donuttext <- cbind(0,100000,"Donut Hole") %>% as.data.frame() %>%
      tbl_df() %>% mutate_all(as.character) %>%
  rename(x=V1,y=V2,label=V3) %>% mutate(x=as.numeric(x), y=as.numeric(y))
    
    #Make textsize small if donut==10 and band==100
    if(donut==10 & band==100){
      textsize=1.25
    } else{
      textsize = 2.5
    }
    plot <- plot + 
      geom_segment(aes(x=-donut, y=75000, xend=-donut, yend=125000), size = .2) + 
       geom_segment(aes(x=donut, y=75000, xend=donut, yend=125000), size = .2) + 
        geom_text(data=donuttext, aes(x=x,y=y,label=label), size = textsize, family = 'sans', col = 'black')
  }
  
  #Save
  ggsave(paste0("Figures/supfig4",letter,".png"),plot,
         width=88,height=54.83077,units="mm",dpi=1200) 
  
  return(plot)
}

#0 out of 50 (for comparsion)
donutplot("a", 0, 50, seq(from=0,to=200000,by=50000), "right")

#10 out of 50
donutplot("b",10, 50,  seq(from=0,to=200000,by=50000), "left")

#10 out of 100
donutplot("c",10, 100, seq(from=0,to=200000,by=50000), "right")

#25 out of 50
donutplot("d",25, 50,  seq(from=0,to=250000,by=50000), "left")

#25 out of 100
donutplot("e",25, 100, seq(from=0,to=200000,by=50000), "right")


