C<-Sys.info()
if(C[1]=='Windows'){setwd("C:/Users/akade/Documents/R_PROJ_LVQ")}
if(C[1]=='Linux'  ){setwd('/root/R_PROJ_LVQ/')}
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
# while (!is.null(dev.list())){dev.off()}
# git remote set-url origin https://github.com/akademikou/PROJ_LVQ.git
# usethis::use_git_config(user.name="akademikou",user.email="akademikou@gmail.org")

library(xts)
library(lubridate)
library(dplyr)     # lead lag
library(class)     # LVQ
library(caret)     # confusionMatrix
library(quantmod)
source('~/R_FUNC/func_BEST.R')
source('~/R_FUNC/func_FILE.R')
source('~/R_FUNC/func_BACKTEST.R')
source('~/R_FUNC/func_CI.R')
source('~/R_FUNC/func_auxM3.R')

VER      <- 'V3'
curryncy <- 'BTC'
F_NAME_1 <- paste(VER,'_',curryncy,'_005/PSO_OPT_',curryncy,'.txt',sep='')
F_NAME_3 <- paste(VER,'_',curryncy,'_005/ASSET_',curryncy,'.txt',sep='')
F_NAME_4 <- paste(VER,'_',curryncy,'_005/EMIR_TAM.txt',sep='')

F_NAME_1 <- 'PSO_OPT.txt'
F_NAME_2 <- 'old_PSO_OPT.txt'
# F_NAME_4 <- paste('EMIR_TAM.txt',sep='')
F_NAME_5 <- 'EMIR_RUN_'

PSO <- read.table(F_NAME_1,header = FALSE); EN <- ncol(PSO)
SIL <- read.table(F_NAME_2,header = FALSE)
SIL <- rbind(SIL[,1:(EN-3)],PSO[,1:(EN-3)])
SIL <- SIL[!duplicated(SIL), ]
write.table(SIL, file = F_NAME_2, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
print('OK')

########################################################################
PAR <- GET_PAR()
TI  <- READ_WITH_TIME(F_NAME_3)
TI  <- xts(TI,order.by = as.POSIXct(index(TI)))
TI$Open  <- TI$Open /coredata(TI$Close[[1001]])
TI$High  <- TI$High /coredata(TI$Close[[1001]])
TI$Low   <- TI$Low  /coredata(TI$Close[[1001]])
TI$Close <- TI$Close/coredata(TI$Close[[1001]])
tmp1 <-  ((lag(coredata(TI$RSI ),0)-lag(coredata(TI$RSI ),1))*2)
tmp2 <-  ((lag(coredata(TI$RSI ),1)-lag(coredata(TI$RSI ),2))*2)
tmp3 <-  ((lag(coredata(TI$RSI ),2)-lag(coredata(TI$RSI ),3))*2)

df  <- data.frame(coredata(TI$RSIC),
                  coredata(TI$RSI ),
                  coredata(TI$WPR ),
                  coredata(TI$CCI ),
                  coredata(TI$ADX ),
                  tmp1)
if (VER == 'V1' || VER == 'V2'){
  df  <- cbind(df,data.frame(
    lag(coredata(TI$RSIC),1),
    lag(coredata(TI$RSI ),1),
    lag(coredata(TI$WPR ),1),
    lag(coredata(TI$CCI ),1),
    lag(coredata(TI$ADX ),1),
    tmp2))
}
if (VER == 'V1' ){
  df  <- cbind(df,data.frame(
    lag(coredata(TI$RSIC ),2),
    lag(coredata(TI$RSI  ),2),
    lag(coredata(TI$WPR  ),2),
    lag(coredata(TI$CCI  ),2),
    lag(coredata(TI$ADX  ),2),
    tmp3))
}
if(VER =='V2'){df <- cbind(df,100)}
if(VER =='V3'){df <- cbind(df,df,df)}
df[is.na(df)] <-0

tmp<-names(df)
tmp[6]<-'dRSI'
names(df)
names(df)<-tmp
for(i in 1:ncol(df)){
  ENB=max(max(df[,i]),abs(min(df[,i])))
  df[,i]<-(100*df[,i]/ENB)
}
TI$RSI     <- ((TI$RSI+100)*0.5)
########################################################################
CIZ1   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ1)<-c('EMIR')
CIZ2   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ2)<-c('EMIR')
CIZ3   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ3)<-c('EMIR')
CIZ4   <- xts(numeric(nrow(TI)),order.by = as.POSIXct(index(TI))); names(CIZ4)<-c('EMIR')
nGIR <- (ncol(df))
nPAR <- (ncol(PSO)-3)
nPSS <- 5
nH   <- (3*nPSS)
################################################################################
# C++ EMIRLERI ILE ANALIZ
if(0){
  DEL <- 100
  for(line in 1:(nrow(PSO))){
    ilk  <- (PSO[line,(ncol(PSO)-1)]+1)
    son  <- (PSO[line,(ncol(PSO)-0)])
    a2   <- (son+  1); u2 <- (son+DEL);
    a1   <- (a2-DEL); u1 <- (u2-DEL);
    if(u1>nrow(TI)){break}
    if(u2>nrow(TI)){break}
    print(c(ilk,son,a1,u1,a2,u2))
    
    NAME <- paste(F_NAME_5,toString(line-1),'.txt',sep='')
    tmp  <- read.table(NAME,header = FALSE)

    CIZ1$EMIR <- 0
    CIZ1$EMIR[a1:u1]<- tmp[a1:u1,1]
    CIZ2$EMIR<-(CIZ2$EMIR+CIZ1$EMIR)

    CIZ1$EMIR <- 0
    CIZ1$EMIR[a2:u2]<- tmp[a2:u2,1]
    CIZ3$EMIR<-(CIZ3$EMIR+CIZ1$EMIR)
    # if(line==1){break;}
  }
  CIZ3$EMIR <- ORDER_KON(CIZ3$EMIR,TI$RSI,TI$Close,son)
  info <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ2$EMIR),PAR)
  title('EMIR_RUN_TRAIN')
  info <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ3$EMIR),PAR)
  title('EMIR_RUN_TEST')
 # stop()
}
################################################################################
if(1){
  tmp  <- read.table(F_NAME_4,header = FALSE);  
  CIZ1$EMIR <- tmp[,1]
  # CIZ1$EMIR <- lag(coredata(CIZ1$EMIR),1); CIZ1$EMIR[is.na(CIZ1$EMIR)] <-0
  # PAR$TxnFees <- 0.005
  info <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ1$EMIR),PAR)
  title(F_NAME_4, col.main='blue', cex.main=2, font.main=4, adj=0, line=0)
}
################################################################################
# LVQ PSO ILE ANALIZ
if(1){
  BOY <- ncol(PSO)
  DEL <- 100
  CIZ4$EMIR <- 0;
  D    <- numeric(nH);
  for(line in 1:(nrow(PSO))){
    ilk  <- (PSO[line,(BOY-1)]+1)
    son  <- (PSO[line,(BOY-0)])
    
    a2   <- (son+  1); u2 <- (son+DEL);
    a1   <- (a2-DEL) ; u1 <- (u2-DEL);

    print(c(a2,u2))
    W    <- (PSO[line,1:(BOY-3)]);
    for(k in a2:u2){
      if(k>nrow(df)){break;}
      BSL <- 1
      BTS <- nGIR/3
      oEMIR <-  coredata(CIZ4$EMIR[k-1])
      if(VER == 'V2'){D1[nGIR] <- (CoEMIR*100)}
      if(VER == 'V1'){BSL <- 1; BTS <- nGIR;}
      if(VER == 'V3'){
             if(oEMIR == -1){BSL <- ((0*nGIR/3)+1); BTS <- ((1*nGIR/3));}
        else if(oEMIR ==  0){BSL <- ((1*nGIR/3)+1); BTS <- ((2*nGIR/3));}
        else if(oEMIR ==  1){BSL <- ((2*nGIR/3)+1); BTS <- ((3*nGIR/3));}
      }
      # print(c(nGIR,CIZ4$EMIR[(k-1)] ,BSL,SON))
      ARA <- (BSL:BTS)
      D1  <- as.numeric((df[k,ARA]))
      for (i in 1:nH){
        err  <- (D1-as.numeric(W[1,ARA]));
        D[i] <- sum(err*err);
        ARA  <- (ARA+nGIR);
      }
      OUT <- which.min(D[1:nH]);
      net <- floor((OUT-1)/nPSS)
           if(net == 0){nEMIR <- (-1);}
      else if(net == 1){nEMIR <- ( 0);}
      else if(net == 2){nEMIR <- ( 1);}
      else{print('HATA'); stop()}
      if(nEMIR == 0){nEMIR <- CIZ4$EMIR[(k-1)]}
      CIZ4$EMIR[k] <- EXP_KNOW(nEMIR,coredata(CIZ4$EMIR[(k-1)]),k,TI$RSI)
      # CIZ4$EMIR[k] <- nEMIR
    }
  }
  CIZ4$EMIR[nrow(CIZ4)]  <- 0
  # CIZ4$EMIR <- ORDER_KON(CIZ4$EMIR,TI$RSI,TI$Close,son)
  # info <- TRADE_PERFORMANCE_CALCULATIONS(TI,coredata(CIZ4$EMIR),PAR)
  info <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ4$EMIR),PAR)
  YAZI <- paste('CALC_C_TEST\n',F_NAME_1)
  title(YAZI, col.main='blue', cex.main=2, font.main=4, adj=0, line=0)
}
########################################################################
warnings()
print("SON")
