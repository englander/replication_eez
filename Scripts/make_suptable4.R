#Make Supplementary Table 4
rm(list=ls())
#Set working directory
setwd("C:/Users/englander/Box Sync/VMS/Nature Sustainability/replication_files/")
options(scipen=999)

library(dplyr)
library(sandwich)
library(xtable)

#Load data
load("Data/ais_vestype_100km.Rdata")

#Only need unauthorized foreign
aisdf <- filter(aisdf, group == "bad_for")

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
levReg <- function(donut, band){
  
  usedf <- filter(aisdf, absdist > donut & absdist < band)
  
  #Level regression
  levelmod <- lm(hours_msqkm ~ inner + absdist + inner:absdist, data = usedf)
  
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
logReg <- function(donut, band){
  
  usedf <- filter(aisdf, absdist > donut & absdist < band) %>% 
    mutate(loghours_msqkm = log(hours_msqkm))

  #Level regression
  logmod <- lm(loghours_msqkm ~ inner + absdist + inner:absdist, data = usedf)
  
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
formCol <- function(donut, band, levdig, logdig){
  
  #Level regression
  mylev <- levReg(donut, band)
  
  #Level coefficient
  levcoef <- formCoef(mylev["coef"],levdig)
  
  #Level SE
  levse <- formSE(mylev["coef"], mylev["se"], mylev["dof"], levdig)
  
  #Format lags
  levlag <- prettyNum(mylev["lag"])
  
  #Log regression
  mylog <- logReg(donut, band)
  
  #Log coefficient
  logcoef <- formCoef(mylog["coef"],logdig)
  
  #Log SE
  logse <- formSE(mylog["coef"], mylog["se"], mylog["dof"], logdig)
  
  #Format lags
  loglag <- prettyNum(mylog["lag"])
  
  #Finally, calculate percentage change
  perchange <- (100*(exp(mylog["coef"])-1))
  
  #Calculate percentage difference with A
  if(donut==0 & band == 50){
    #Leave blank if A itself
    perdif <- ""
  } else{
    #I know that the percentage treatment effect for A is -87.89978
    perdif <- (perchange - -87.89978) %>% formCoef(1)
    perdif <- paste0(perdif, "\\%")
  }

  perchange <- formCoef(perchange, 1)
  perchange <- paste0(perchange,"\\%")
  
  out <- c(
    levcoef, levse, levlag, 
    logcoef, logse, loglag,
    perchange, perdif, 
    paste0(donut, " km"), paste0(band, " km"),
    mylev["dof"]+4
  )
  
  return(out)
  
}

table <- matrix(NA,nrow=12,ncol=6)

table[1,] <- c("","(a)","(b)","(c)","(d)","(e)")
table[,1] <- c("","Levels","","NW lag","Logs","","NW lag","Percentage","Difference","Donut hole", "Bandwidth","Observations")

table[2:12,2] <- formCol(0, 50, 0, 2)
table[2:12,3] <- formCol(10, 50, 0, 2)
table[2:12,4] <- formCol(10, 100, 0, 2)
table[2:12,5] <- formCol(25, 50, 0, 2)
table[2:12,6] <- formCol(25, 100, 0, 2)

myxtable <- xtable(table)

align(myxtable) <- c("l","l",rep("c",5))

label(myxtable) <- "tab:donut"

print(myxtable, floating = TRUE, caption.placement="top",sanitize.text.function = identity,
      include.colnames=F,include.rownames=F,table.placement="h!",
      hline.after=NULL,
      add.to.row=list(
        pos = list(0,1,4,7,9,nrow(table)),
        command = c(
          paste0("\\toprule"),
          " \\midrule "," \\midrule "," \\midrule "," \\midrule ",
          paste0("\\bottomrule ")
          )),
      type = "latex",file="Tables/suptable4.tex")

