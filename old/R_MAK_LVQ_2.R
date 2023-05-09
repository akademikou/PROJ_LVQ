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
library(lubridate)
library(dplyr)     # lead lag
library(class)     # LVQ
library(caret)     # confusionMatrix
library(quantmod)
source("~/R_FUNC/func_BEST.R")
source("~/R_FUNC/func_FILE.R")
source("~/R_FUNC/func_BACKTEST.R")
source("~/R_FUNC/func_CI.R")
source("~/R_FUNC/func_auxM3.R")

PSO <- read.table("PSO_OPT.txt",header = FALSE)
SIL <- read.table("old_PSO_OPT.txt",header = FALSE)
SIL <- rbind(SIL,PSO)
SIL <- SIL[!duplicated(SIL), ]
write.table(SIL, file = "old_PSO_OPT.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
########################################################################
PAR <- GET_PAR()
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
# AD<-names(TI)
# for(i in 1:ncol(TI)){print(c(AD[i],round(min(TI[,i]),2),round(max(TI[,i]),2)))}
TI$RSI     <- ((TI$RSI+100)*0.5)
########################################################################
# TI     <- TI[DATA_TAM,]
CIZ1   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ1)<-c('EMIR')
CIZ2   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ2)<-c('EMIR')
CIZ3   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ3)<-c('EMIR')
CIZ4   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ4)<-c('EMIR')
PSO  <- read.table("PSO_OPT.txt",header = FALSE)
nGIR <- (ncol(df))
nPAR <- (ncol(PSO)-3)
nPSS <- 5
nH   <- (3*nPSS)
################################################################################
# C++ EMIRLERI ILE ANALIZ
if(1){
  DEL <- 100
  for(line in 1:(nrow(PSO))){
    ilk  <- (PSO[line,(ncol(PSO)-1)]+1)
    son  <- (PSO[line,(ncol(PSO)-0)])
    a2   <- (son+  1); u2 <- (son+DEL);
    a1   <- (a2-DEL); u1 <- (u2-DEL);
    if(u1>nrow(TI)){break}
    if(u2>nrow(TI)){break}
    print(c(ilk,son,a1,u1,a2,u2))
    
    NAME <- paste('EMIR_RUN_',toString(line-1),'.txt',sep='')
    tmp  <- read.table(NAME,header = FALSE)

    CIZ1$EMIR <- 0
    CIZ1$EMIR[a1:u1]<- tmp[a1:u1,1]
    CIZ2$EMIR<-(CIZ2$EMIR+CIZ1$EMIR)

    CIZ1$EMIR <- 0
    CIZ1$EMIR[a2:u2]<- tmp[a2:u2,1]
    CIZ3$EMIR<-(CIZ3$EMIR+CIZ1$EMIR)
    # if(line==1){break;}
  }
  # CIZ3$EMIR <- ORDER_KON(CIZ3$EMIR,TI$RSI,TI$Close,son)
  info <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ2$EMIR),PAR)
  info <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ3$EMIR),PAR)
 # stop()
}
################################################################################
# LVQ PSO ILE ANALIZ
if(1){
  DEL <- 100
  CIZ4$EMIR <- 0;
  D    <- numeric(nH);
  for(line in 1:(nrow(PSO))){
    ilk  <- (PSO[line,(ncol(PSO)-1)]+1)
    son  <- (PSO[line,(ncol(PSO)-0)])
    a2   <- (son+  1); u2 <- (son+DEL);
    
    W    <- (PSO[line,1:(ncol(PSO)-3)]);
    
    for(k in a2:u2){
      if(k>nrow(df)){break;}
      ind  <- 1;
      D1 <- as.numeric((df[k,1:nGIR]))
      for (i in 1:nH){
        D[i] <- 0
        D2 <- as.numeric((W[1,ind:(ind+nGIR-1)]))
        err  <- (D1-D2);
        D[i] <- sum(err*err);
        ind  <- (ind+nGIR);
    #  for(j in 1:nGIR){
    #   err  <- (df[[k,j]]-W[[ind]]);
    #   D[i] <- (D[i]+(err*err));
    #   ind  <- (ind+1);
    #  }
      }
      OUT <- which.min(D);
      net <- floor((OUT-1)/nPSS)
           if(net == 0){nEMIR <- (-1);}
      else if(net == 1){nEMIR <- ( 0);}
      else if(net == 2){nEMIR <- ( 1);}
      else{print('HATA'); stop()}
  
      CIZ4$EMIR[k] <- nEMIR
    }
  }
  CIZ4$EMIR <- ORDER_KON(CIZ4$EMIR,TI$RSI,TI$Close,son)
  info <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ4$EMIR),PAR)
  stop()
}
########################################################################
warnings()
print("SON")