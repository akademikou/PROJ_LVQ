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

library(xts)
library(lubridate)
library(dplyr)     # lead lag
library(class)     # LVQ
library(caret)     # confusionMatrix
library(quantmod)
library(ggplot2)
library(hrbrthemes)
library(polycor)
source('~/R_FUNC/func_BEST.R')
source('~/R_FUNC/func_FILE.R')
source('~/R_FUNC/func_BACKTEST.R')
source('~/R_FUNC/func_CI.R')
source('~/R_FUNC/func_auxM3.R')
source('~/R_FUNC/func_DATA_PROCESS.R')
########################################################################
PAR <- GET_PAR()
TI  <- READ_WITH_TIME('ASSET_BTC.txt')
TI  <- xts(TI,order.by = as.POSIXct(index(TI)))

df   <- data.frame(coredata(TI$RSIC),
                   coredata(TI$SMAC),
                   coredata(TI$MFI ),
                   coredata(TI$RSI ),
                   coredata(TI$WPR ),
                   coredata(TI$PCD ),
                   coredata(TI$CCI ),
                   coredata(TI$ADX ),
                   coredata(TI$ROC ),
                  (coredata(TI$RSI)-lag(coredata(TI$RSI ),1))*2)
df[is.na(df)] <-0
df <- as.xts(x=coredata(df),order.by = as.POSIXct(index(TI)))

tmp<-names(df)
tmp[10]<-'dRSI'
names(df)
names(df)<-tmp
for(i in 1:ncol(df)){
  ENB=max(max(df[,i]),abs(min(df[,i])))
  df[,i]<-(100*df[,i]/ENB)
}

# TI$Open  <- TI$Open /coredata(TI$Close[[1001]])
# TI$High  <- TI$High /coredata(TI$Close[[1001]])
# TI$Low   <- TI$Low  /coredata(TI$Close[[1001]])
# TI$Close <- TI$Close/coredata(TI$Close[[1001]])
# HLC  <- TI[,c("High","Low",'Close')]
# HL   <- TI[,c("High","Low")]
# C    <- TI$Close
# V    <- TI$Volume
# nOBV       <- OBV   (C    , V)
# 
# PAR_K <- 3
# PAR_U <- 25
# PAR_D <- 50
# PAR_A <- 0.025
# nEMA_K     <- EMA   (C    , n = PAR_K, wilder = TRUE)
# nEMA_U     <- EMA   (C    , n = PAR_U, wilder = TRUE)
# nSMA_K     <- SMA   (C    , n = 3)
# nSMA_U     <- SMA   (C    , n = 14)
# PCY        <- CALC_PCY(nEMA_K,nEMA_U,PAR_D,PAR_A)
# TI$OBV<-PCY    [,c(1)]
# TI[is.na(TI)] <-0

YAZI_1 <- 'BEST_poz_05'
YAZI_2 <- 'BEST_ord_05'
tmp1 <- read.table(paste(YAZI_1,'.txt',sep = ""),header = FALSE)
tmp2 <- read.table(paste(YAZI_2,'.txt',sep = ""),header = FALSE)
AL_SAT <- as.xts(x=data.frame(coredata(tmp1[,2]),
                              coredata(tmp2[,2])),
                 order.by = as.POSIXct(index(TI)))
rm(tmp1)
rm(tmp2)
colnames(AL_SAT)<-c('POZ','ORD')



#[ 1] "Open"    "High"      "Low"   "Close" "Volume"  "ClCl"  "MAX_RET" "PCD"      
#[ 9] "EMAC"    "SMAC"      "RSIC"  "EMA"   "SMA"     "SAR"   "BB_MID"  "RS"       
#[17] "PC"      "HISTOGRAM" "OBV"   "ROC"   "STOCH"   "RSI"   "WPR"     "MFI"      
#[25] "CCI"     "ATR"       "ADX"   "ARN"   "STD"     "OSC"   "oEMAC"   "oSMAC"    
#[33] "oRSEC"   "oRSIC"     "oSTC"  "oRSI"  "oWPR"    "oMFI"  "oPCZ"    "oADX"     
#[41] "oARN"    "oOSC"  

# df<-(df*(coredata(TI$SAR  )))
# df$PCD<-((coredata(TI$BB_MID )))
# df$PCD[which(coredata(TI$ADX ) > 0)] <-  1
# df$PCD[which(coredata(TI$ADX ) < 0)] <- -1
# df$PCD<-(coredata(TI$WPR  )-lag(coredata(TI$WPR  ),1))


df <- cbind(AL_SAT,coredata(df))

d<-0
df$ORD  <- lag(df$ORD,d)
df$POZ  <- lag(df$POZ,d)
df$ORD [is.na (df$ORD) ] <-0
df$POZ [is.na (df$POZ) ] <-0
 DATA_CORRELATION(df ,names(df [,1:ncol(df )]))

# A <-  polychor(coredata(AL_SAT$EMIR),coredata(df$PCD), ML=TRUE, std.err=TRUE)
# print(A) 
###################################################################
ORD_B <- df[df$ORD ==  1,]
ORD_S <- df[df$ORD == -1,]
POZ_B <- df[df$POZ ==  1,]
POZ_S <- df[df$POZ == -1,];
if(0){
  Ms <- mean(ORD_S$PCD)
  Mb <- mean(ORD_B$PCD)
  M1o <- min(Ms,Mb)
  M2o <- max(Ms,Mb)
  SB <- nrow(ORD_S[ORD_S$PCD > M2o,])
  SN <- nrow(ORD_S[ORD_S$PCD < M2o,] & ORD_S[ORD_S$PCD > M1o,])
  SS <- nrow(ORD_S[ORD_S$PCD < M1o,])
  BB <- nrow(ORD_B[ORD_B$PCD > M2o,])
  BN <- nrow(ORD_B[ORD_B$PCD < M2o,] & ORD_B[ORD_B$PCD > M1o,])
  BS <- nrow(ORD_B[ORD_B$PCD < M1o,])
  ordSELL <- c(BS,SN,SS)
  ordBUY  <- c(BB,BN,SB)
  INFO_ORD1<- data.frame(ordBUY,ordSELL)
  O1 <- (SS+BB)/(SS+BB+SB+BS)

  Ms <- mean(POZ_S$PCD)
  Mb <- mean(POZ_S$PCD)
  M1p <- min(Ms,Mb)
  M2p <- max(Ms,Mb)
  SB <- nrow(POZ_S[POZ_S$PCD > M2p,])
  SN <- nrow(POZ_S[POZ_S$PCD < M2p,] & POZ_S[POZ_S$PCD > M1p,])
  SS <- nrow(POZ_S[POZ_S$PCD < M1p,])
  BB <- nrow(POZ_B[POZ_B$PCD > M2p,])
  BN <- nrow(POZ_B[POZ_B$PCD < M2p,] & POZ_B[POZ_B$PCD > M1p,])
  BS <- nrow(POZ_B[POZ_B$PCD < M1p,])
  ordSELL <- c(BS,SN,SS)
  ordBUY  <- c(BB,BN,SB)
  INFO_POZ1<- data.frame(ordBUY,ordSELL)
  P1 <- (SS+BB)/(SS+BB+SB+BS)
  
  AL_SAT$cORD <- df$PCD*0
  AL_SAT$cORD[which(coredata(df$PCD ) > M2o)] <- -1
  AL_SAT$cORD[which(coredata(df$PCD ) < M1o)] <-  1
  AL_SAT$cPOZ <- df$PCD*0
  AL_SAT$cPOZ[which(coredata(df$PCD ) > M2p)] <-  1
  AL_SAT$cPOZ[which(coredata(df$PCD ) < M1p)] <- -1
  

  
  
  S1b <- nrow(AL_SAT[AL_SAT$cPOZ ==  1 & AL_SAT$POZ ==   1,])
  S2b <- nrow(AL_SAT[AL_SAT$cPOZ ==  1 & AL_SAT$POZ ==   0])
  S3b <- nrow(AL_SAT[AL_SAT$cPOZ ==  1 & AL_SAT$POZ ==  -1,])
  S1h <- nrow(AL_SAT[AL_SAT$cPOZ ==  0 & AL_SAT$POZ ==   1,])
  S2h <- nrow(AL_SAT[AL_SAT$cPOZ ==  0 & AL_SAT$POZ ==   0])
  S3h <- nrow(AL_SAT[AL_SAT$cPOZ ==  0 & AL_SAT$POZ ==  -1,])
  S1s <- nrow(AL_SAT[AL_SAT$cPOZ == -1 & AL_SAT$POZ ==   1,])
  S2s <- nrow(AL_SAT[AL_SAT$cPOZ == -1 & AL_SAT$POZ ==   0])
  S3s <- nrow(AL_SAT[AL_SAT$cPOZ == -1 & AL_SAT$POZ ==  -1,])
  ordSELL <- c(S1s,S2s,S3s)
  ordBUY  <- c(S1b,S2b,S3b)
  ordHOLD <- c(S1h,S2h,S3h)
  INFO_POZ2<- data.frame(ordBUY,ordHOLD,ordSELL)
  P2 <- (S1b+S2h+S3s)/(S1b+S2b+S3b+S1h+S2h+S3h+S1s+S2s+S3s)
  
  S1b <- nrow(AL_SAT[AL_SAT$cORD == 1 & AL_SAT$ORD ==   1,])
  S2b <- nrow(AL_SAT[AL_SAT$cORD == 1 & AL_SAT$ORD ==   0])
  S3b <- nrow(AL_SAT[AL_SAT$cORD == 1 & AL_SAT$ORD ==  -1,])
  S1h <- nrow(AL_SAT[AL_SAT$cORD == 0 & AL_SAT$ORD ==   1,])
  S2h <- nrow(AL_SAT[AL_SAT$cORD == 0 & AL_SAT$ORD ==   0])
  S3h <- nrow(AL_SAT[AL_SAT$cORD == 0 & AL_SAT$ORD ==  -1,])
  S1s <- nrow(AL_SAT[AL_SAT$cORD == -1 & AL_SAT$ORD ==   1,])
  S2s <- nrow(AL_SAT[AL_SAT$cORD == -1 & AL_SAT$ORD ==   0])
  S3s <- nrow(AL_SAT[AL_SAT$cORD == -1 & AL_SAT$ORD ==  -1,])
  ordSELL <- c(S1s,S2s,S3s)
  ordBUY  <- c(S1b,S2b,S3b)
  ordHOLD <- c(S1h,S2h,S3h)
  INFO_ORD2<- data.frame(ordBUY,ordHOLD,ordSELL)
  O2 <- (S1b+S2h+S3s)/(S1b+S2b+S3b+S1h+S2h+S3h+S1s+S2s+S3s)
  
  rownames(INFO_ORD1)<-c('tiBUY','tiHOLD','tiSELL')
  print('____________________________')
  print(INFO_ORD1)
  print(O1)
  print('____________________________')
  
  rownames(INFO_ORD2)<-c('tiBUY','tiHOLD','tiSELL')
  print('____________________________')
  print(INFO_ORD2)
  print(O2)
  print('____________________________')
  
  rownames(INFO_POZ1)<-c('tiBUY','tiHOLD','tiSELL')
  print('____________________________')
  print(INFO_POZ1)
  print(P1)
  print('____________________________')
  
  rownames(INFO_POZ2)<-c('tiBUY','tiHOLD','tiSELL')
  print('____________________________')
  print(INFO_POZ2)
  print(P2)
  print('____________________________')
}
###################################################################
if(0){
  H_DATA <- data.frame(coredata(AL_SAT$POZ),
                       coredata(AL_SAT$ORD),
                       coredata(TI$WPR),
                       coredata(TI$ROC),
                       coredata(TI$CCI),
                       coredata(df$PCD))
  DATA_CORRELATION(H_DATA ,names(H_DATA [,1:ncol(H_DATA )]))
}
###################################################################

lab <- c('RSIC','SMAC','MFI','RSI','WPR','PCD','CCI','ADX','ROC','dRSI')
ind <- c(    1,     2,     3,    4,    5,    6,    7,    8,    9,   10)+2
for(i in 1:10){
  SINIR<- c(min(df[,ind[i]]),max(df[,ind[i]]))
  if(0){
    dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
    par(mfrow = c(2, 2),     # 2x2 layout
        oma = c(5, 4, 0, 0)+0.1, # two rows of text at the outer left and bottom margin
        mar = c(0, 0, 5, 5)+0.1, # space for one row of text at ticks and to separate plots
        mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row
    # xpd = NA)            # allow content to protrude into outer margin (and beyond)

    A1 <- hist(ORD_S[,ind[i]],main=paste("ORDER SELL ",lab[i]),xlab=lab[i],breaks=20,col="red", freq=FALSE, probability = TRUE,xlim=SINIR)
    abline(v = mean(ORD_S[,ind[i]]), col = 'blue'  , lwd = 3)
    lines(  density(ORD_S[,ind[i]]), col = 'black', lwd = 3)
    
    A2 <- hist(ORD_B[,ind[i]],main=paste("ORDER BUY ",lab[i]),xlab=lab[i],breaks=20,col="green",freq=FALSE,probability = TRUE,xlim=SINIR)
    abline(v = mean(ORD_B[,ind[i]]), col = 'blue'  , lwd = 3)
    lines(  density(ORD_B[,ind[i]]), col = 'black', lwd = 3)

    A3 <- hist(POZ_S[,ind[i]],main=paste("POZ SELL ",lab[i]),xlab=lab[i],breaks=20,col="red", freq=FALSE, probability = TRUE,xlim=SINIR)
    abline(v = mean(POZ_S[,ind[i]]), col = 'blue'  , lwd = 3)
    lines(  density(POZ_S[,ind[i]]), col = 'black', lwd = 3)
    
    A4 <- hist(POZ_B[,ind[i]],main=paste("POZ BUY ",lab[i]),xlab=lab[i],breaks=20,col="green",freq=FALSE,probability = TRUE,xlim=SINIR)
    abline(v = mean(POZ_B[,ind[i]]), col = 'blue'  , lwd = 3)
    lines(  density(POZ_B[,ind[i]]), col = 'black', lwd = 3)
    # title(YAZI_2, col.main='blue', cex.main=2, font.main=4, adj=0, line=0)
    stop()
  }
  if(0){
    dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
    data <- data.frame(
      type = c( rep(paste("SELL ",lab[i]), nrow(ORD_S)), 
                rep(paste("BUY  ",lab[i]), nrow(ORD_B)),
                rep(paste("ALL  ",lab[i]), nrow(df  ))),
      value = rbind( coredata(ORD_S[,ind[i]]),
                     coredata(ORD_B[,ind[i]]),
                     coredata(df  [,ind[i]]))
    )
    colnames(data)<-c('type','value')
    p <- data %>%
      ggplot( aes(x=value, fill=type)) +
      ggtitle(YAZI_2) +
      # geom_histogram(aes(y=..ncount..), color="black", alpha=0.6, position = 'identity',bins=100)+
      geom_histogram(aes(y=..density..), color="black", alpha=0.6, position = 'identity',bins = 100) +
      geom_density(alpha=.2) +
      geom_vline(aes(xintercept=mean(ORD_S[,ind[i]])), color="red4"  , linetype="dashed", size=2)+
      geom_vline(aes(xintercept=mean(ORD_B[,ind[i]])), color="green4", linetype="dashed", size=2)+
      scale_fill_manual(values=c("blue", "green",'red')) +
      theme_ipsum() +
      xlim(SINIR[1],SINIR[2])
    labs(fill="")
    print(p)
  }
  
  if(1){
    DEV <- max(sd(ORD_S[,ind[i]]),sd(ORD_B[,ind[i]]))
    SINIR[1] <-(SINIR[1] +0*DEV)
    SINIR[2] <-(SINIR[2] -0*DEV)
    data_ORD <- data.frame(
      type = c( rep(paste("SELL ",lab[i]), nrow(ORD_S)), 
                rep(paste("BUY  ",lab[i]), nrow(ORD_B)) ),
      value = rbind( coredata(ORD_S[,ind[i]]), coredata(ORD_B[,ind[i]]) )
    )
    colnames(data_ORD)<-c('type','value')
    data_POZ <- data.frame(
      type = c( rep(paste("SELL ",lab[i]), nrow(POZ_S)), 
                rep(paste("BUY  ",lab[i]), nrow(POZ_B)) ),
      value = rbind( coredata(POZ_S[,ind[i]]), coredata(POZ_B[,ind[i]]) )
    )
    colnames(data_POZ)<-c('type','value')

    GEN <- (SINIR[2]-SINIR[1])/30
    p1 <- data_ORD %>%
      ggplot( aes(x=value, fill=type)) +
      ggtitle(YAZI_2) +
      # geom_histogram(aes(y=..ncount..), color="black", alpha=0.6, position = 'identity',bins=100)+
      geom_histogram(aes(y=..count..), color="black", alpha=0.6, position = 'identity',binwidth = GEN) +
      geom_density(alpha=.2,aes(y=after_stat(count)*GEN)) +
      geom_vline(aes(xintercept=mean(ORD_S[,ind[i]])), color="red4"  , linetype="dashed", size=2)+
      geom_vline(aes(xintercept=mean(ORD_B[,ind[i]])), color="green4", linetype="dashed", size=2)+
      scale_fill_manual(values=c("green", "red")) +
      theme_ipsum() +
      xlim(SINIR[1],SINIR[2])
      labs(fill="")

    p2 <- data_POZ %>%
      ggplot( aes(x=value, fill=type)) +
      ggtitle(YAZI_1) +
      # geom_histogram(aes(y=..ncount..), color="black", alpha=0.6, position = 'identity',bins=100)+
      geom_histogram(aes(y=..count..), color="black", alpha=0.6, position = 'identity',binwidth = GEN) +
      geom_density(alpha=.2,aes(y=after_stat(count)*GEN)) +
      # geom_line(aes(y = ..count..), stat="density", size = 1, colour="black", linetype=1) +
      geom_vline(aes(xintercept=mean(POZ_S[,ind[i]])), color="red4"  , linetype="dashed", size=2)+
      geom_vline(aes(xintercept=mean(POZ_B[,ind[i]])), color="green4", linetype="dashed", size=2)+
      scale_fill_manual(values=c("green", "red")) +
      theme_ipsum() +
      xlim(SINIR[1],SINIR[2])
    labs(fill="")

    library(gridExtra)
    dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
    grid.arrange(p1, p2, ncol=1, nrow =2)
  }
}
