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

F_NAME_1 <- 'PSO_OPT.txt'
F_NAME_2 <- 'old_PSO_OPT.txt'
F_NAME_3 <- 'ASSET_BTC.txt'
F_NAME_4 <- 'EMIR_TAM.txt'
F_NAME_5 <- 'EMIR_RUN_'

PSO <- read.table(F_NAME_1,header = FALSE); EN <- ncol(PSO)
SIL <- read.table(F_NAME_2,header = FALSE)
SIL <- rbind(SIL[,1:(EN-3)],PSO[,1:(EN-3)])
SIL <- SIL[!duplicated(SIL), ]

write.table(SIL, file = F_NAME_2, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
print('OK')
# stop()
########################################################################
PAR <- GET_PAR()
TI  <- READ_WITH_TIME(F_NAME_3)
TI  <- xts(TI,order.by = as.POSIXct(index(TI)))
TI$Open  <- TI$Open /coredata(TI$Close[[1001]])
TI$High  <- TI$High /coredata(TI$Close[[1001]])
TI$Low   <- TI$Low  /coredata(TI$Close[[1001]])
TI$Close <- TI$Close/coredata(TI$Close[[1001]])

# ENB=max(abs(TI$PCD  )); print(ENB); TI$PCD  <-(TI$PCD   *100/ENB)
# ENB=max(abs(TI$EMAC )); print(ENB); TI$EMAC <-(TI$EMAC  *100/ENB)
# ENB=max(abs(TI$RSIC )); print(ENB); TI$RSIC <-(TI$RSIC  *100/ENB)
# ENB=max(abs(TI$ATR  )); print(ENB); TI$ATR  <-(TI$ATR   *100/ENB)
# ENB=max(abs(TI$STOCH)); print(ENB); TI$STOCH<-(TI$STOCH *100/ENB)

df  <- data.frame(lag(coredata(TI$PCD  ),0), lag(coredata(TI$PCD  ),1), lag(coredata(TI$PCD  ),2),
                  lag(coredata(TI$EMAC ),0), lag(coredata(TI$EMAC ),1), lag(coredata(TI$EMAC ),2),
                  lag(coredata(TI$RSIC ),0), lag(coredata(TI$RSIC ),1), lag(coredata(TI$RSIC ),2),
                  lag(coredata(TI$ATR  ),0), lag(coredata(TI$ATR  ),1), lag(coredata(TI$ATR  ),2),
                  lag(coredata(TI$WPR  ),0), lag(coredata(TI$WPR  ),1), lag(coredata(TI$WPR  ),2),
                  lag(coredata(TI$MFI  ),0), lag(coredata(TI$MFI  ),1), lag(coredata(TI$MFI  ),2),
                  lag(coredata(TI$STOCH),0), lag(coredata(TI$STOCH),1), lag(coredata(TI$STOCH),2),
                  lag(coredata(TI$RSI  ),0), lag(coredata(TI$RSI  ),1), lag(coredata(TI$RSI  ),2))
df[is.na(df)] <-0



# LIM<-data.frame(matrix(0,nrow=ncol(df),ncol=4))
# for(i in 1:ncol(df)){
#   LIM[i,1]<-max(PSO[,i])
#   LIM[i,2]<-min(PSO[,i])
#   LIM[i,3]<-max(df [,i])
#   LIM[i,4]<-min(df [,i])
# }
# stop()
# AD<-names(TI)
# for(i in 1:ncol(TI)){print(c(AD[i],round(min(TI[,i]),2),round(max(TI[,i]),2)))}
TI$RSI     <- ((TI$RSI+100)*0.5)
########################################################################
# TI     <- TI[DATA_TAM,]
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
  PAR$TxnFees <- 0.005
  info <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ1$EMIR),PAR)
  title(F_NAME_4, col.main='blue', cex.main=2, font.main=4, adj=0, line=0)
}
################################################################################
# LVQ PSO ILE ANALIZ
if(0){
  DEL <- 100
  CIZ4$EMIR <- 0;
  D    <- numeric(nH);
  for(line in 1:(nrow(PSO))){
    ilk  <- (PSO[line,(ncol(PSO)-1)]+1)
    son  <- (PSO[line,(ncol(PSO)-0)])
    
    a2   <- (son+  1); u2 <- (son+DEL);
    a1   <- (a2-DEL) ; u1 <- (u2-DEL);

    print(c(a2,u2))
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
      # CIZ4$EMIR[k] <- nEMIR
      if(nEMIR == 0){nEMIR <- CIZ4$EMIR[(k-1)]}
      CIZ4$EMIR[k]  <- EXP_KNOW(nEMIR,coredata(CIZ4$EMIR[(k-1)]),k,TI$RSI)

    }
  }
  CIZ4$EMIR[nrow(CIZ4)]  <- 0
  # CIZ4$EMIR <- ORDER_KON(CIZ4$EMIR,TI$RSI,TI$Close,son)
  # info <- TRADE_PERFORMANCE_CALCULATIONS(TI,coredata(CIZ4$EMIR),PAR)
  info <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ4$EMIR),PAR)
  YAZI <- paste('CALC_C_TEST\n',F_NAME_1)
  title(YAZI, col.main='blue', cex.main=2, font.main=4, adj=0, line=0)
  # col.main: Changed title font color to blue.
  # cex.main: Increased title font to twice the default size.
  # font.main: Changed title font style to italic.
  # adj: Moved title all the way to the left.
  # line: Moved title down to touch the top of the plot.
}
########################################################################
warnings()
print("SON")
