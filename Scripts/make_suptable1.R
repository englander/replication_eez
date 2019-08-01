#Make Supplementary Table 1
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")
options(scipen=999)

library(dplyr)
library(xtable)
library(sandwich)

#Load AIS data
load("Data/ais_vestype_100km.Rdata")

tabdf <- filter(aisdf, abs(dist) <=50)

tabdf <- mutate(tabdf, inner = if_else(type=="inner",1,0), 
                     logh = log(hours_msqkm))

#Add rows for total AIS data
tabdf <- bind_rows(tabdf,
                   group_by(tabdf, dist, type, Area_km2, absdist, dist2, dist3, inner) %>% 
                     summarise(hours = sum(hours)) %>% ungroup() %>% 
                     mutate(hours_msqkm = (hours/Area_km2)*10^6) %>%
                     mutate(logh = log(hours_msqkm)) %>% 
                     mutate(group = "tot")
)

#Rename dependent variables
tabdf <- rename(tabdf, dep = hours, dep_area = hours_msqkm, logdep = logh)

#Load VBD data
load("Data/vbd_100km.Rdata")

vbd <- filter(vbd, absdist <= 50) %>% 
  mutate(logdep = log(count_msqkm), group = "vbd") %>% 
  rename(dep = count, dep_area = count_msqkm)

#Bind onto tabdf
tabdf <- bind_rows(tabdf, vbd)

#Given model object, calculate optimal lag using two kernels and output max 
maxLag <- function(mymod){
  
  #Take maximimum of optimal lag
  quadlag <- bwNeweyWest(mymod, kernel = "Quadratic Spectral")
  bartlag <- bwNeweyWest(mymod, kernel = "Bartlett")
 
  #Output max
  out <- ceiling(max(quadlag, bartlag))
  
  return(out)
   
}

#Regression function for levels
levReg <- function(mygroup){
  
  usedf <- filter(tabdf, group==mygroup) %>% arrange(dist)
  
  #Level regression
  levelmod <- lm(dep_area ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                   inner:dist2 + inner:dist3, data = usedf)
  
  #Calculate optimal lag
  mylag <- maxLag(levelmod)
  
  #Get Newey-West SE
  levelse <- NeweyWest(levelmod, lag = mylag) %>% diag() %>% sqrt()
  
  #Save coefficient, se, lag, and degrees of freedom 
  out <- c(
    coefficients(levelmod)["inner"],
    levelse["inner"],
    mylag,
    summary(levelmod)[["df"]][2]
  )
   
  names(out) <- c("coef","se","lag","dof")
  
  return(out)
}

#Regression function for logs
logReg <- function(mygroup){
  
  usedf <- filter(tabdf, group==mygroup) %>% arrange(dist)
  
  #Level regression
  logmod <- lm(logdep ~ inner + absdist + dist2 + dist3 + inner:absdist + 
                   inner:dist2 + inner:dist3, data = usedf)
  
  #Calculate optimal lag
  mylag <- maxLag(logmod)
  
  #Get Newey-West SE
  logse <- NeweyWest(logmod, lag = mylag) %>% diag() %>% sqrt()
  
  #Save coefficient, se, lag, and degrees of freedom 
  out <- c(
    coefficients(logmod)["inner"],
    logse["inner"],
    mylag,
    summary(logmod)[["df"]][2]
  )
  
  names(out) <- c("coef","se","lag","dof")
  
  return(out)
}

#Format coefficient
formCoef <- function(coef, dig){
  
  #Round
  roundcoef <- round(coef, dig) %>% as.character()
  
  #If rounded to integer, need to add "." to end
  if(length(grep("\\.",roundcoef))==0){
    roundcoef <- paste0(roundcoef, ".")
  }
  
  #Add an extra zero beyond the decimal point if needed to get same length
  #Do coef first
  roundcoef <- sapply(seq_len(length(roundcoef)), function(x){
    if(gsub(".*\\.","",roundcoef[x]) %>% nchar() < dig){
      #Needed length
      zerosneeded <- dig - gsub(".*\\.","",roundcoef[x]) %>% nchar()
      roundcoef[x] <- paste0(roundcoef[x],paste0(rep(0,zerosneeded),collapse=""))
    } else{
      roundcoef[x]
    }
  })
  
  #Add commas if necessary
  roundcoef <- prettyNum(roundcoef, ",")
  
  return(roundcoef)
}


#Given coefficients, Newey se, degrees of freedom (dof), and number of digits to round to, 
#return formatted SE
formSE <- function(coef, se, dof, dig){

  #Round
  roundse <- round(se, dig) %>% as.character()
  
  #If rounded to integer, need to add "." to end
  if(length(grep("\\.",roundse))==0){
    roundse <- paste0(roundse, ".")
  }
  
  #Add zeros if necessary
  roundse <- sapply(seq_len(length(roundse)), function(x){
    if(gsub(".*\\.","",roundse[x]) %>% nchar() < dig){
      #Needed length (could need one extra zero or two)
      zerosneeded <- dig - gsub(".*\\.","",roundse[x]) %>% nchar()
      roundse[x] <- paste0(roundse[x],paste0(rep(0,zerosneeded),collapse=""))
    } else{
      roundse[x]
    }
  })
  
  #Add commas if necessary
  roundse <- prettyNum(roundse, ",")
  
  #Add parentheses 
  roundse <- paste0("(",roundse,")")

  #Add stars if applicable
  tstat <- coef / se
  pval <- 2*pt(abs(tstat), df = dof, lower.tail = FALSE)
  
  if(pval<.1 & pval>=.05){
    roundse <- paste0(roundse,"*")
  } else if(pval<.05 & pval>=.01){
    roundse <- paste0(roundse,"**")
  } else if(pval < .01){
    roundse <- paste0(roundse,"***")
  }

  return(roundse)
}

#Make an entire column for table
formCol <- function(mygroup, levdig, logdig){
  
  #Level regression
  mylev <- levReg(mygroup)
  
  #Level coefficient
  levcoef <- formCoef(mylev["coef"],levdig)
  
  #Level SE
  levse <- formSE(mylev["coef"], mylev["se"], mylev["dof"], levdig)
  
  #Format lags
  levlag <- prettyNum(mylev["lag"])
  
  #Log regression
  mylog <- logReg(mygroup)
  
  #Log coefficient
  logcoef <- formCoef(mylog["coef"],logdig)
  
  #Log SE
  logse <- formSE(mylog["coef"], mylog["se"], mylog["dof"], logdig)
  
  #Format lags
  loglag <- prettyNum(mylog["lag"])
  
  #Finally, calculate percentage change
  perchange <- (100*(exp(mylog["coef"])-1)) %>% 
    formCoef(1)
  
  perchange <- paste0(perchange,"\\%")
  
  out <- c(
    levcoef, levse, levlag, 
    logcoef, logse, loglag,
    perchange
  )
  
  return(out)
  
}


#Make table
table <- matrix(NA,nrow=8,ncol=6)

table[1,] <- c("","(1)","(2)","(3)","(4)","(5)")
table[,1] <- c("","Levels","","NW lag","Logs","","NW lag","Percentage")

table[2:8,2] <- formCol("tot",0,2)
table[2:8,3] <- formCol("vbd",0,2)
table[2:8,4] <- formCol("bad_for",0,2)
table[2:8,5] <- formCol("ok_for",0,2)
table[2:8,6] <- formCol("dom",0,2)

myxtable <- xtable(table)

caption(myxtable) <- c("Caption.")

align(myxtable) <- c("l","l",rep("c",5))

label(myxtable) <- "tab:net"

print(myxtable, floating = TRUE, caption.placement="top",sanitize.text.function = identity,
      include.colnames=F,include.rownames=F,table.placement="h!",
      hline.after=NULL,
      add.to.row=list(
        pos = list(0,1,4,7,nrow(table)),
        command = c(
          paste0("\\toprule & Fig. 2A & Fig. 2B & Fig. 2C & \\multicolumn{2}{c}{Fig. 2D} \\\\ 
                  & Total AIS & VBD & Unauth. For. & Auth. For. & Domestic \\\\ "),
          " \\midrule "," \\midrule "," \\midrule ",
          paste0("\\bottomrule ")
        )),
      type = "latex",file="Tables/suptable1.tex")
