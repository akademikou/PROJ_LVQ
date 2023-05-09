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
dev_unit   <- "in"

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
# TI  <- READ_WITH_TIME("ASSET_TI.txt")
# TI  <- xts(TI,order.by = as.POSIXct(index(TI)))
# oTI <- TI
# for(i in 8:ncol(TI)){
#   ENB=max(abs(max(TI[,i])),abs(min(TI[,i])))
#   oTI[,i] <- TI[,i]*100/ENB
# }
# oTI$RSI <- TI$RSI
# WRITE_WITH_TIME(oTI,"ASSET_TI1.txt")
# 
# stop()


PAR <- GET_PAR()
DATA_TAM   <-    1:2000
DATA_TRAIN <-    1:1000
DATA_TEST  <- 1001:2000
TI         <- READ_WITH_TIME("ASSET_TI.txt")
TI         <- xts(TI,order.by = as.POSIXct(index(TI)))


AL_SAT     <- read.table("EMIR_0.txt",header = FALSE)
AL_SAT     <- as.xts(x=coredata(AL_SAT[,1]),order.by = as.POSIXct(index(TI)))
colnames(AL_SAT)<-'EMIR'
# AD<-names(TI)
# for(i in 1:ncol(TI)){
#   print(c(AD[i],round(min(TI[,i]),2),round(max(TI[,i]),2)))
# }
########################################################################
TI     <- TI[DATA_TAM,]
CIZ1   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ1)<-c('EMIR')
CIZ2   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ2)<-c('EMIR')
CIZ3   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ3)<-c('EMIR')
CIZ4   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ4)<-c('EMIR')
AL_SAT <- AL_SAT[DATA_TAM,]
# AL_SAT$EMIR <- lag(AL_SAT$EMIR,2)
# AL_SAT$EMIR[is.na(AL_SAT$EMIR)] <-0




df_TAM     <- data.frame(lag(coredata(TI$EMAC ),0),
                         lag(coredata(TI$EMAC ),1),
                         lag(coredata(TI$EMAC ),2),
                         
                         lag(coredata(TI$RSIC ),0),
                         lag(coredata(TI$RSIC ),1),
                         lag(coredata(TI$RSIC ),2),
                         
                         lag(coredata(TI$ATR  ),0),
                         lag(coredata(TI$ATR  ),1),
                         lag(coredata(TI$ATR  ),2),
                         
                         lag(coredata(TI$WPR  ),0),
                         lag(coredata(TI$WPR  ),1),
                         lag(coredata(TI$WPR  ),2),
                         
                         lag(coredata(TI$MFI  ),0),
                         lag(coredata(TI$MFI  ),1),
                         lag(coredata(TI$MFI  ),2),
                         
                         lag(coredata(TI$STOCH),0),
                         lag(coredata(TI$STOCH),1),
                         lag(coredata(TI$STOCH),2),
                         
                         lag(coredata(TI$RSI  ),0),
                         lag(coredata(TI$RSI  ),1),
                         lag(coredata(TI$RSI  ),2),
                         
                         AL_SAT$EMIR)
df_TAM[is.na(df_TAM)] <-0

TI$RSI     <- ((TI$RSI+100)*0.5)
df_TRAIN   <- data.frame(df_TAM[DATA_TRAIN,])
df_TEST    <- data.frame(df_TAM[DATA_TEST ,])
n          <- names(df_TAM)
o_name     <- n[length(n)]
i_name     <- n[!n %in% o_name]
nINP       <- (ncol(df_TAM)-1)
IN         <- (1:nINP)
OUT        <- (ncol(df_TAM))
rownames(df_TRAIN) <- 1:nrow(df_TRAIN)



RW1  <- read.table("LVQW1.txt",header = FALSE)



CIZ3$EMIR[DATA_TAM]<-AL_SAT[DATA_TAM,1]
CIZ2$EMIR <- ORDER_KON(AL_SAT$EMIR,TI$RSI,TI$Close)
# CIZ3$EMIR <- ORDER_KON(CIZ3$EMIR,TI$RSI,TI$Close)
# nC<-30
# nPSS <- nC/2
# D    <-numeric(nC);
# P3   <-numeric(nrow(df_TAM));
# for(k in 1:nrow(df_TAM)){
#   for (i in 1:nC){
#     D[i] <- 0
#     for(j in IN){
#       err  <- (df_TAM[[k,j]]-RW1[[i,j]]);
#       D[i] <- D[i]+(err*err)
#     }
#   }
#   if(which.min(D)<=nPSS){CIZ2$EMIR[k]<-(-1);}else{CIZ2$EMIR[k]<-(1);}
# }
CIZ2$EMIR[1]<-0; CIZ2$EMIR[nrow(df_TAM)]<-0; CIZ2$EMIR[is.na(CIZ3$EMIR)] <-0
CIZ3$EMIR[1]<-0; CIZ3$EMIR[nrow(df_TAM)]<-0; CIZ3$EMIR[is.na(CIZ3$EMIR)] <-0

info_2 <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ2$EMIR),PAR)
info_3 <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ3$EMIR),PAR)
print("TRADE_PERFORMANCE_CALCULATIONS")
info_1 <- TRADE_PERFORMANCE_CALCULATIONS(TI,coredata(CIZ3$EMIR),PAR)
########################################################################
if(0){

  n      <- nrow(df_TRAIN)
  nC     <- 2*5 # merkez sayısı
  ITER   <- 10000*nC
  OK     <- 0.0001
  
  cl     <- factor(df_TRAIN[,OUT])
  cl[is.na(cl)] <-1
  
  label_TEST <- factor(df_TEST[,OUT],levels <- c(-1,1))
  label_TAM  <- factor(df_TAM [,OUT])
  
  R0 <- lvqinit(df_TRAIN[,IN],cl,size=nC,k=1)
  rownames(R0$x)<-c(1:nC)
  R1 <- olvq1  (df_TRAIN[,IN],cl,R0,niter=ITER,alpha=OK)
  R2 <- lvq1   (df_TRAIN[,IN],cl,R1,niter=ITER,alpha=OK)
  R3 <- lvq2   (df_TRAIN[,IN],cl,R2,niter=ITER,alpha=OK,win=0.3)
  R4 <- lvq3   (df_TRAIN[,IN],cl,R3,niter=ITER,alpha=OK,win=0.3,epsilon=0.1)
  
  P1 <- lvqtest(R1, df_TEST[,IN]);
  P2 <- lvqtest(R2, df_TEST[,IN]);
  P3 <- lvqtest(R3, df_TEST[,IN]);
  P4 <- lvqtest(R4, df_TEST[,IN]);
  
  CM1<-confusionMatrix(P1,label_TEST)
  CM2<-confusionMatrix(P2,label_TEST)
  CM3<-confusionMatrix(P3,label_TEST)
  CM4<-confusionMatrix(P4,label_TEST)
  if(CM1$overall[1]<CM2$overall[1]){CM1 <- CM2; R1<-R2}
  if(CM1$overall[1]<CM3$overall[1]){CM1 <- CM3; R1<-R3}
  if(CM1$overall[1]<CM4$overall[1]){CM1 <- CM4; R1<-R4}
  
  
  RW0  <- read.table("LVQW0.txt",header = FALSE)
  RW1  <- read.table("LVQW1.txt",header = FALSE)
  
  for(i in 1:nC){
    for(j in IN){
      R2$x[i,j]<-RW0[[i,j]]
      R3$x[i,j]<-as.numeric(RW1[[i,j]])
    }
  }
  cl       <-numeric(nC);
  cl[       1: nC/2] <-(-1)
  cl[(nC/2+1): nC  ] <-( 1)
  R2$cl     <- factor(cl)
  R3$cl     <- factor(cl)
  P1 <- lvqtest(R1, df_TAM[,IN]);
  P2 <- lvqtest(R2, df_TAM[,IN]);
  P3 <- lvqtest(R3, df_TAM[,IN]);
  ###############################################
  
  nPSS <- nC/2
  D    <-numeric(nC);
  P3   <-numeric(nrow(df_TAM));
  for(k in 1:nrow(df_TAM)){
    for (i in 1:nC){
      D[i] <- 0
      for(j in IN){
        err  <- (df_TAM[[k,j]]-RW1[[i,j]]);
        D[i] <- D[i]+(err*err)
      }
    }
    if(which.min(D)<=nPSS){P3[k]<-(-1);}else{P3[k]<-(1);}
  }
  P3     <- factor(P3)
  CM1<-confusionMatrix(P1,label_TAM); print(CM1$table);
  CM2<-confusionMatrix(P2,label_TAM); print(CM2$table);
  CM3<-confusionMatrix(P3,label_TAM); print(CM3$table);
  print(c(CM1$overall[1],CM2$overall[1],CM3$overall[1]))

  P1<-as.numeric(P1)*2-3
  P2<-as.numeric(P2)*2-3
  P3<-as.numeric(P3)*2-3
  
  P1 <- xts(as.integer(P1),order.by = as.POSIXct(index(TI[DATA_TAM,])))
  P2 <- xts(as.integer(P2),order.by = as.POSIXct(index(TI[DATA_TAM,])))
  P3 <- xts(as.integer(P3),order.by = as.POSIXct(index(TI[DATA_TAM,])))
  P1[1,1]<-0; P1[nrow(TI),1]<-0;
  P2[1,1]<-0; P2[nrow(TI),1]<-0;
  P3[1,1]<-0; P3[nrow(TI),1]<-0;
  
  CIZ1 <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI)))
  CIZ2 <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI)))
  CIZ3 <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI)))
  names(CIZ1)<-c('EMIR')
  names(CIZ2)<-c('EMIR')
  names(CIZ3)<-c('EMIR')
  
  CIZ1$EMIR[DATA_TRAIN]<-AL_SAT$EMIR[DATA_TRAIN]
  CIZ1$EMIR[DATA_TEST ]<-P1[DATA_TEST]
  CIZ2$EMIR[DATA_TAM  ]<-P3[DATA_TAM]
  CIZ3$EMIR[DATA_TAM  ]<-P3[DATA_TAM]
  
  CIZ1$EMIR <- lead(coredata(CIZ1$EMIR),1)
  CIZ2$EMIR <- lead(coredata(CIZ2$EMIR),0)
  CIZ3$EMIR <- lead(coredata(CIZ3$EMIR),0)
  
  EMIR  <- read.table("EMIR.txt",header = FALSE)
  CIZ3$EMIR[DATA_TAM]<-EMIR[DATA_TAM,1]
  
  CIZ1$EMIR[1]<-0; CIZ1$EMIR[nrow(df_TAM)]<-0; CIZ1$EMIR[is.na(CIZ1$EMIR)] <-0
  CIZ2$EMIR[1]<-0; CIZ2$EMIR[nrow(df_TAM)]<-0; CIZ2$EMIR[is.na(CIZ2$EMIR)] <-0
  CIZ3$EMIR[1]<-0; CIZ3$EMIR[nrow(df_TAM)]<-0; CIZ3$EMIR[is.na(CIZ3$EMIR)] <-0

  # CIZ1$EMIR <- ORDER_KON(CIZ1$EMIR,TI$RSI,TI$Close)
  CIZ2$EMIR <- ORDER_KON(CIZ2$EMIR,TI$RSI,TI$Close)
  # CIZ3$EMIR <- ORDER_KON(CIZ3$EMIR,TI$RSI,TI$Close)
  
  # CIZ1$EMIR[DATA_TEST,]<-0
  # CIZ2$EMIR[DATA_TEST,]<-0
  # CIZ3$EMIR[nrow(df_TRAIN)-1]<-0
  # CIZ3$EMIR[nrow(df_TRAIN)]<-0
  # CIZ3$EMIR[DATA_TEST,]<-0
  
  A<-cbind(CIZ3$EMIR[DATA_TAM]-EMIR[DATA_TAM,],
           CIZ3$EMIR[DATA_TAM]-CIZ2$EMIR[DATA_TAM])
  

  # info_1 <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ1$EMIR),PAR)
  info_2 <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ2$EMIR),PAR)
  info_3 <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ3$EMIR),PAR)
  
  # print("TRADE_PERFORMANCE_CALCULATIONS")
  # info_1 <- TRADE_PERFORMANCE_CALCULATIONS(TI,coredata(CIZ3$EMIR),PAR)
}
########################################################################
if(0){
  CIZ<-TI[,1:6]
  lim <- c(-100,100)
  dev.new(width = 15, height = 7, unit = "px",noRStudioGD = TRUE)
  my.chob <-chartSeries(CIZ, theme=chartTheme('white'),
                        type = c("auto", "matchsticks"),
                        show.grid = TRUE,
                        major.ticks='auto', minor.ticks=TRUE,
                        multi.col = FALSE,
                        name="BTC",
                        plot = TRUE,
                        TAsep=';',
                        TA = list(
                          "addBBands()",
                          # " addRSI(n = 14, maType = 'EMA', wilder = TRUE,yrange=lim)",
                          " addTA(TI$RSI         , col='red' , type='l',lwd=1,legend=NULL,yrange=lim)",
                          # " addTA(TI$OSC*0.3     , col='red' , type='l',lwd=1,legend=NULL,yrange=lim)",
                          " addTA(AL_SAT$EMIR*100, col='blue', type='l',lwd=1,on=2,legend=NULL,yrange=lim)"
                          # " addTA(TI$OBV*0.5+50 , col='red', type='l',lwd=1,on=2,legend=NULL,yrange=lim)"
                        )
  )
}
########################################################################
if(0){
  lim <- c(min(TI$Close),max(TI$Close))
  dev.new(width = 1024, height = 768, unit = "px",noRStudioGD = TRUE)
  plot.new()
  par(mar=c(5, 10, 4, 6) + 0.1) # alt sol
  matplot(AL_SAT$EMIR, type='l', axes=FALSE,col="red" ,lwd=2, lty=1, ylab='')
  par(new=TRUE)
  matplot(TI$Close   , type='l', axes=TRUE ,col="blue",lwd=2, lty=1, ylab='')
  mtext("date" ,side=1,col="black",line=2)
  mtext("close",side=2,col="red"  ,line=2) 
  mtext("buy"  ,side=4,col="blue" ,line=2)
  axis(4, ylim=lim      , col="blue" ,col.axis="blue" ,las=1)
  grid(nx = 5, ny = 9, col = "black", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  box()
}
########################################################################
warnings()
print("SON")
