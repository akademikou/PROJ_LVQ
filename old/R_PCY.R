setwd('/root/R_PROJ_LVQ/')
# setwd("/root/")
# setwd("C:/Users/akade/Documents/R_PROJ_LVQ")
cat("\014") 
rm(list = ls())
set.seed(Sys.time())
options(device = "RStudioGD")
options(device = "x11")
dev_width  <- 1024
dev_height <- 768
dev_width  <- 9
dev_height <- 7
dev_unit <- "in"
while (!is.null(dev.list())){dev.off()}

library (xts)
library (dplyr)     # lead lag
library (lubridate)
library (class)     # LVQ
library(caret)      # confusionMatrix
library(quantmod)
source("~/R_FUNC/func_BEST.R")
source("~/R_FUNC/func_FILE.R")
source("~/R_FUNC/func_BACKTEST.R")
source("~/R_FUNC/func_CI.R")
source("~/R_FUNC/func_auxM3.R")

# ########################################################################
PAR <- GET_PAR()
TI         <- READ_WITH_TIME("ASSET_TI.txt")
TI         <- xts(TI,order.by = as.POSIXct(index(TI)))
########################################################################
C    <- TI$Close 
PAR_K <- 3 # 3 14
PAR_U <- 25
PAR_D <- 50
PAR_A <- 0.025
nEMA_K     <- EMA   (C    , n = PAR_K, wilder = TRUE)
nEMA_U     <- EMA   (C    , n = PAR_U, wilder = TRUE)
nSMA_K     <- SMA   (C    , n = 3)
nSMA_U     <- SMA   (C    , n = 14)
PCY        <- CALC_PCY(nEMA_K,nEMA_U,PAR_D,PAR_A)
TI<-TI[500:5800,]
PCY<-PCY[500:5800,]
PA <- PCY
PE <- PCY
for(i in 2:(nrow(PCY)-1)){
  P_DEL<- (coredata(PCY[i,1])-coredata(PCY[(i-1),1]))
  if(P_DEL<0){PA[i,1]<-NA}else{PE[i,1]<-NA}
   #      if(PCY[i,2]>  5){PCY[i,3]<-  100}
   # else if(PCY[i,2]< -5){PCY[i,3]<- -100}
   # else                 {PCY[i,3]<- PCY[i-1,3]}
}

AL_SAT<-data.frame(PCY[,3]/100)
colnames(AL_SAT)<-'EMIR'
AL_SAT     <- as.xts(x=coredata(AL_SAT[,1]),order.by = as.POSIXct(index(TI)))
colnames(AL_SAT)<-'EMIR'
# AL_SAT$EMIR <- lead(AL_SAT$EMIR,1)
AL_SAT$EMIR[is.na(AL_SAT$EMIR)] <-0
info_3 <- TRADE_PERFORMANCE_TRADE(TI,coredata(AL_SAT$EMIR),PAR)
# stop()
lim1 <- c(min(TI$Close),max(TI$Close))
lim2 <- c(min(PCY[,2]),max(PCY[,2]))
lim3 <- c(min(PCY[,1]),max(PCY[,1]))

dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
plot.new()
par(mar=c(5, 10, 4, 6) + 0.1) # alt sol
                 matplot(PCY[,2], type='s', axes=TRUE ,col="black",lwd=2, lty=1, ylab='',ylim=lim2)
par(new=TRUE);   matplot(PCY[,1], type='s', axes=FALSE,col="blue" ,lwd=2, lty=1, ylab='',ylim=lim3)
axis(2, ylim=lim2      , col="blue" ,col.axis="blue" ,las=1)
axis(4, ylim=lim3      , col="blue" ,col.axis="blue" ,las=1)
grid(nx = 5, ny = 9, col = "black", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()
###################################33
dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
plot.new()
par(mar=c(5, 10, 4, 6) + 0.1) # alt sol
matplot(TI$Close , type='l', axes=FALSE,col="black",lwd=2, lty=1, ylab='',ylim=lim1)
par(new=TRUE); matplot(PCY[,2]  , type='s', axes=TRUE,col="blue" ,lwd=2, lty=1, ylab='',ylim=lim2)
par(new=TRUE); matplot(PCY[,2]*0, type='s', axes=FALSE,col="blue",lwd=2, lty=1, ylab='',ylim=lim2) 
par(new=TRUE); matplot(PA [,1]  , type='s', axes=FALSE,col="green",lwd=2, lty=1, ylab='',ylim=lim3)
par(new=TRUE); matplot(PE [,1]  , type='s', axes=FALSE,col="red"  ,lwd=2, lty=1, ylab='',ylim=lim3)
par(new=TRUE); matplot(PCY[,3]  , type='s', axes=FALSE,col="yellow",lwd=2, lty=1, ylab='')
  
mtext("date" ,side=1,col="black",line=2)
mtext("close",side=2,col="red"  ,line=2) 
mtext("buy"  ,side=4,col="blue" ,line=2)
  
# axis(2, ylim=lim2      , col="blue" ,col.axis="blue" ,las=1)
axis(4, ylim=lim2      , col="blue" ,col.axis="blue" ,las=1)
grid(nx = 5, ny = 9, col = "black", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
box()