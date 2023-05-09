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

library(xts)
library(quantmod)
library(lubridate)
library(dplyr)        # lead lag
library(class)        # LVQ
library(caret)        # confusionMatrix
library(cvms)         # confusion matrix plot
library(rsvg)         # confusion matrix plot
library(ggnewscale)   # confusion matrix plot
library(broom)        # confusion matrix plot
library(tibble)       # confusion matrix plot
library(ggimage)      # confusion matrix plot

source("~/R_FUNC/func_BEST.R")
source("~/R_FUNC/func_FILE.R")
source("~/R_FUNC/func_BACKTEST.R")
source("~/R_FUNC/func_CI.R")
source("~/R_FUNC/func_auxM3.R")
########################################################################
PAR <- GET_PAR()
PSO <- read.table("PSO_OPT.txt",header = FALSE)
TI  <- READ_WITH_TIME("ASSET_TI.txt")
TI  <- xts(TI,order.by = as.POSIXct(index(TI)))
df  <- data.frame(lag(coredata(TI$PCD  ),0), lag(coredata(TI$PCD  ),1), lag(coredata(TI$PCD  ),2),
                  lag(coredata(TI$EMAC ),0), lag(coredata(TI$EMAC ),1), lag(coredata(TI$EMAC ),2),
                  lag(coredata(TI$RSIC ),0), lag(coredata(TI$RSIC ),1), lag(coredata(TI$RSIC ),2),
                  lag(coredata(TI$ATR  ),0), lag(coredata(TI$ATR  ),1), lag(coredata(TI$ATR  ),2),
                  lag(coredata(TI$WPR  ),0), lag(coredata(TI$WPR  ),1), lag(coredata(TI$WPR  ),2),
                  lag(coredata(TI$MFI  ),0), lag(coredata(TI$MFI  ),1), lag(coredata(TI$MFI  ),2),
                  lag(coredata(TI$STOCH),0), lag(coredata(TI$STOCH),1), lag(coredata(TI$STOCH),2),
                  lag(coredata(TI$RSI  ),0), lag(coredata(TI$RSI  ),1), lag(coredata(TI$RSI  ),2))
df[is.na(df)] <-0

DATA_TAM   <-    1:1500
DATA_TRAIN <-    1:1000
DATA_TEST  <- 1001:1500

nPSS     <- 5

seviye_3 <-  c(-1,0,1)
seviye_2 <-  c(-1,1)
seviye   <- seviye_2
if(length(seviye)==2){
  BEST <- read.table("BEST_poz.txt",header = FALSE)
  LVQ  <- read.table("LVQW1.txt"   ,header = FALSE)
  nH   <- (2*nPSS)
  cl   <-numeric(nH);
  cl[(0*nH/2+1):(1*nH/2)] <-(-1)
  cl[(1*nH/2+1):(2*nH/2)] <-( 1)

}
if(length(seviye)==3){
  BEST <- read.table("BEST_ord.txt",header = FALSE)
  LVQ  <- read.table("LVQW0.txt"   ,header = FALSE)
  nH   <- (3*nPSS)
  cl   <-numeric(nH);
  cl[(0*nH/3+1):(1*nH/3)] <-(-1)
  cl[(1*nH/3+1):(2*nH/3)] <-( 0)
  cl[(2*nH/3+1):(3*nH/3)] <-( 1)
  # BEST$EMIR[1]<-0
}

BEST     <- as.xts(x=coredata(BEST[,2]),order.by = as.POSIXct(index(TI)))
colnames(BEST)<-'EMIR'

# BEST$EMIR <- lag(BEST$EMIR,2)
# BEST$EMIR[is.na(BEST$EMIR)] <-0

df_TAM <- data.frame(df,BEST$EMIR)

df_TAM[is.na(df_TAM)] <-0
BEST <- BEST[DATA_TAM,]

df_TAM     <- data.frame(df_TAM[DATA_TAM  ,])
# df_TRAIN   <- data.frame(df_TAM[DATA_TRAIN,])
# df_TEST    <- data.frame(df_TAM[DATA_TEST ,])

TI$RSI     <- ((TI$RSI+100)*0.5)
########################################################################
# TI     <- TI[DATA_TAM,]
CIZ1 <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ1)<-c('EMIR')
CIZ2 <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ2)<-c('EMIR')
CIZ3 <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ3)<-c('EMIR')
CIZ4 <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ4)<-c('EMIR')
PSO  <- read.table("PSO_OPT.txt",header = FALSE)
nGIR <- (ncol(df))
nPAR <- (ncol(PSO)-3)
################################################################################
n          <- names(df_TAM)
o_name     <- n[length(n)]
i_name     <- n[!n %in% o_name]
nINP       <- (ncol(df_TAM)-1)
IN         <- (1:nINP)
OUT        <- (ncol(df_TAM))
# rownames(df_TRAIN) <- 1:nrow(df_TRAIN)
########################################################################
if(1){
  Ta<-TI[DATA_TAM]
  # rm(TI)
  ITER   <- 10000*nH
  OK     <- 0.0001
  
  clR     <- factor(df_TAM[DATA_TRAIN,OUT])
  clR[is.na(cl)] <-1
  
  # label_TRAIN <- factor(df_TRAIN[,OUT],levels <- seviye)
  # label_TEST  <- factor(df_TEST [,OUT],levels <- seviye)
  label_TAM   <- factor(df_TAM  [,OUT],levels <- seviye)
  
  R0 <- lvqinit(df_TAM[DATA_TRAIN,IN],clR,size=nH,k=1)
  rownames(R0$x)<-c(1:nH)
  R1 <- olvq1  (df_TAM[DATA_TRAIN,IN],label_TAM[DATA_TRAIN],R0,niter=ITER,alpha=OK)
  R2 <- lvq1   (df_TAM[DATA_TRAIN,IN],label_TAM[DATA_TRAIN],R1,niter=ITER,alpha=OK)
  R3 <- lvq2   (df_TAM[DATA_TRAIN,IN],label_TAM[DATA_TRAIN],R2,niter=ITER,alpha=OK,win=0.3)
  R4 <- lvq3   (df_TAM[DATA_TRAIN,IN],label_TAM[DATA_TRAIN],R3,niter=ITER,alpha=OK,win=0.3,epsilon=0.1)
  ####################################################################
  P1 <- lvqtest(R1, df_TAM[DATA_TRAIN,IN]);
  P2 <- lvqtest(R2, df_TAM[DATA_TRAIN,IN]);
  P3 <- lvqtest(R3, df_TAM[DATA_TRAIN,IN]);
  P4 <- lvqtest(R4, df_TAM[DATA_TRAIN,IN]);
  CM1<-confusionMatrix(P1,label_TAM[DATA_TRAIN])
  CM2<-confusionMatrix(P2,label_TAM[DATA_TRAIN])
  CM3<-confusionMatrix(P3,label_TAM[DATA_TRAIN])
  CM4<-confusionMatrix(P4,label_TAM[DATA_TRAIN])
  if(CM1$overall[1]<CM2$overall[1]){R1<-R2}
  if(CM1$overall[1]<CM3$overall[1]){R1<-R3}
  if(CM1$overall[1]<CM4$overall[1]){R1<-R4}
  ####################################################################
  P11 <- lvqtest(R1, df_TAM[DATA_TRAIN,IN]);
  P12 <- lvqtest(R1, df_TAM[DATA_TEST ,IN]);
  P13 <- lvqtest(R1, df_TAM[DATA_TAM  ,IN]);
  CM1<-confusionMatrix(P11,label_TAM[DATA_TRAIN]);
  CM2<-confusionMatrix(P12,label_TAM[DATA_TEST ]);
  CM3<-confusionMatrix(P13,label_TAM[DATA_TAM  ]);
  print('lvqtest from R df_TRAIN');  print(CM1$overall[1])
  print('lvqtest from R df_TEST' );  print(CM2$overall[1])
  print('lvqtest from R df_TAM'  );  print(CM3$overall[1])
  print('________________')
  INFO_CONF(HEDEF=label_TAM[DATA_TRAIN],TAHMIN=P11,CM=CM1)
  # INFO_CONF(HEDEF=label_TAM[DATA_TEST ],TAHMIN=P12,CM=CM2)
  # INFO_CONF(HEDEF=label_TAM[DATA_TAM  ],TAHMIN=P13,CM=CM3)
  ####################################################################
  for(i in 1:nH){
    for(j in IN){ R1$x[i,j]<-LVQ[i,j] }
  }
  R1$cl     <- factor(cl)

  P21 <- lvqtest(R1, df_TAM[DATA_TRAIN,IN]);
  P22 <- lvqtest(R1, df_TAM[DATA_TEST ,IN]);
  P23 <- lvqtest(R1, df_TAM[DATA_TAM  ,IN]);
  CM1<-confusionMatrix(P21,label_TAM[DATA_TRAIN]);
  CM2<-confusionMatrix(P22,label_TAM[DATA_TEST ]);
  CM3<-confusionMatrix(P23,label_TAM[DATA_TAM  ]);
  print('lvqtest from C++ df_TRAIN');  print(CM1$overall[1])
  print('lvqtest from C++ df_TEST' );  print(CM2$overall[1])
  print('lvqtest from C++ df_TAM'  );  print(CM3$overall[1])
  print('________________')
  # print(CM2)
  INFO_CONF(HEDEF=label_TAM[DATA_TRAIN],TAHMIN=P21,CM=CM1)
  # INFO_CONF(HEDEF=label_TAM[DATA_TEST ],TAHMIN=P22,CM=CM2)
  # INFO_CONF(HEDEF=label_TAM[DATA_TAM  ],TAHMIN=P23,CM=CM3)
  ####################################################################
  # C++ ile elde edilen LVQ sonucları
  nH   <- (3*nPSS)
  cl   <-numeric(nH);
  cl[(0*nH/3+1):(1*nH/3)] <-(-1)
  cl[(1*nH/3+1):(2*nH/3)] <-( 0)
  cl[(2*nH/3+1):(3*nH/3)] <-( 1)
  
  
  P1<- (as.numeric(as.character(P1)))
  line <- 1
  W    <- (PSO[line,1:(ncol(PSO)-3)]);
  ind <-1
  
  R1$x <- matrix(0, nH,length(IN))
  for(i in 1:nH){
    for(j in IN){
      R1$x[i,j]<-W[[ind]]
      ind <- (ind+1)
    }
  }

  R1$cl     <- factor(cl)
  P31 <- lvqtest(R1, df_TAM[DATA_TRAIN,IN]);
  P32 <- lvqtest(R1, df_TAM[DATA_TEST ,IN]); 
  P33 <- lvqtest(R1, df_TAM[DATA_TAM  ,IN]);
  
  
  CIZ4$EMIR[DATA_TRAIN]<-(as.numeric(as.character(P31))) # PSO with C LVQ
  CIZ4$EMIR <- ORDER_KON(CIZ4$EMIR, TI$RSI,TI$Close,length(DATA_TRAIN))
  for(i in DATA_TRAIN[length(DATA_TRAIN)]:2){
    if(CIZ4$EMIR[(i-1)]==0){CIZ4$EMIR[(i-1)] <- CIZ4$EMIR[(i)]}
    P31[i]<-CIZ4$EMIR[(i)]
  }
  P31[1]<-CIZ4$EMIR[(1)]
  P31 <- droplevels(P31)
  
  CIZ4$EMIR[DATA_TEST]<-(as.numeric(as.character(P32))) # PSO with C LVQ
  CIZ4$EMIR <- ORDER_KON(CIZ4$EMIR,TI$RSI,TI$Close,length(DATA_TEST))
  for(i in DATA_TEST[length(DATA_TEST)]:2){
    if(CIZ4$EMIR[(i-1)]==0){CIZ4$EMIR[(i-1)] <- CIZ4$EMIR[(i)]}
    P32[i]<-CIZ4$EMIR[(i)]
  }
  P32[1]<-CIZ4$EMIR[(1)]
  P32 <- droplevels(P32)
  
  CIZ4$EMIR[DATA_TAM]<-(as.numeric(as.character(P33))) # PSO with C LVQ
  CIZ4$EMIR <- ORDER_KON(CIZ4$EMIR,TI$RSI,TI$Close,length(DATA_TAM))
  for(i in DATA_TAM[length(DATA_TAM)]:2){
    if(CIZ4$EMIR[(i-1)]==0){CIZ4$EMIR[(i-1)] <- CIZ4$EMIR[(i)]}
    P33[i]<-CIZ4$EMIR[(i)]
  }
  P33[1]<-CIZ4$EMIR[(1)]
  P33 <- droplevels(P33)
  
  CM1<-confusionMatrix(P31,label_TAM[DATA_TRAIN]);
  # CM2<-confusionMatrix(P32,label_TAM[DATA_TEST ]);
  CM3<-confusionMatrix(P33,label_TAM[DATA_TAM  ]);


  print('PSO df_TRAIN');  print(CM1$overall[1])
  print('PSO df_TEST' );  print(CM2$overall[1])
  print('PSO df_TAM'  );  print(CM3$overall[1])
  print('________________')
  # print(CM2)
  INFO_CONF(HEDEF=label_TAM[DATA_TRAIN],TAHMIN=P31,CM=CM1)
  # INFO_CONF(HEDEF=label_TAM[DATA_TRAIN],TAHMIN=P32,CM=CM2)
  # INFO_CONF(HEDEF=label_TAM[DATA_TRAIN],TAHMIN=P33,CM=CM3)
  ###############################################
  # USTTEKI HESAPLARLA AYNI 
  # P1<-as.numeric(P1[])#*2-3
  # D    <-numeric(nH);
  # for(k in DATA_TRAIN){
  #   for (i in 1:nH){
  #     D[i] <- 0
  #     for(j in IN){
  #       err  <- (df_TRAIN[[k,j]]-R1$x[[i,j]]);
  #       D[i] <- D[i]+(err*err)
  #     }
  #   }
  #   RES <- floor((3*which.min(D)-1)/nH)
  #   P1[[k]]<- (RES-1)
  # }
  # P41    <- factor(P1);  CM1<- confusionMatrix(P1,label_TRAIN);
  # 
  # 
  # print('PSO with C LVQ df_TRAIN');  print(CM1$overall[1])
  # print('PSO with C LVQ df_TEST' );  print(CM2$overall[1])
  # print('PSO with C LVQ df_TAM'  );  print(CM3$overall[1])
  # print('________________')
  # INFO_CONF(HEDEF=label_TRAIN,TAHMIN=P41,CM=CM1)
  # INFO_CONF(HEDEF=label_TEST ,TAHMIN=P42,CM=CM2)
  # INFO_CONF(HEDEF=label_TAM  ,TAHMIN=P43,CM=CM3)
  # print(CM1)
  ###############################################
  # P33 <- as.factor(CIZ4$EMIR[DATA_TAM])

  P11<- (as.numeric(as.character(P11)))
  P12<- (as.numeric(as.character(P12)))
  P13<- (as.numeric(as.character(P13)))
  
  P21<- (as.numeric(as.character(P21)))
  P22<- (as.numeric(as.character(P22)))
  P23<- (as.numeric(as.character(P23)))
  
  P31<- (as.numeric(as.character(P31)))
  P32<- (as.numeric(as.character(P32)))
  P33<- (as.numeric(as.character(P33)))
  CIZ1$EMIR[DATA_TAM]<-BEST$EMIR[DATA_TAM];
  CIZ2$EMIR[DATA_TAM]<-P13 # lvqtest
  CIZ3$EMIR[DATA_TAM]<-P23 # PSO with Rs lvqtest
  CIZ4$EMIR[DATA_TAM]<-P33 # PSO with C LVQ
  
  CIZ1$EMIR <- lead(coredata(CIZ1$EMIR),1); CIZ1$EMIR[is.na(CIZ1$EMIR)] <-0
  CIZ2$EMIR <- lead(coredata(CIZ2$EMIR),1); CIZ2$EMIR[is.na(CIZ2$EMIR)] <-0
  # CIZ3$EMIR <- lead(coredata(CIZ3$EMIR),1); CIZ3$EMIR[is.na(CIZ3$EMIR)] <-0
  # CIZ4$EMIR <- lead(coredata(CIZ4$EMIR),1); CIZ4$EMIR[is.na(CIZ4$EMIR)] <-0
  # CIZ1$EMIR <- ORDER_KON(CIZ1$EMIR,TI$RSI,TI$Close,length(DATA_TAM))
  # CIZ2$EMIR <- ORDER_KON(CIZ2$EMIR,TI$RSI,TI$Close,length(DATA_TAM))
  # CIZ3$EMIR <- ORDER_KON(CIZ3$EMIR,TI$RSI,TI$Close,length(DATA_TAM))
  CIZ4$EMIR <- ORDER_KON(CIZ4$EMIR,TI$RSI,TI$Close,length(DATA_TAM))
  
  T1<-coredata(CIZ1$EMIR[DATA_TAM])
  T2<-coredata(CIZ2$EMIR[DATA_TAM])
  T3<-coredata(CIZ3$EMIR[DATA_TAM])
  T4<-coredata(CIZ4$EMIR[DATA_TAM])
  
  TRADE_PERFORMANCE_TRADE(Ta,T1,PAR)
  TRADE_PERFORMANCE_TRADE(Ta,T2,PAR)
  TRADE_PERFORMANCE_TRADE(Ta,T3,PAR)
  TRADE_PERFORMANCE_TRADE(Ta,T4,PAR)
}
########################################################################
if(0){
  CIZ<-TI[,1:6]
  lim <- c(-100,100)
  dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
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
                          " addTA(BEST$EMIR*100, col='blue', type='l',lwd=1,on=2,legend=NULL,yrange=lim)"
                          # " addTA(TI$OBV*0.5+50 , col='red', type='l',lwd=1,on=2,legend=NULL,yrange=lim)"
                        )
  )
}
########################################################################
if(0){
  lim <- c(min(TI$Close),max(TI$Close))
  dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
  plot.new()
  par(mar=c(5, 10, 4, 6) + 0.1) # alt sol
  matplot(BEST$EMIR, type='l', axes=FALSE,col="red" ,lwd=2, lty=1, ylab='')
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