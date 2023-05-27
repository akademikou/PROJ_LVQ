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


PAR  <- GET_PAR()
BNB  <- READ_WITH_TIME("ASSET_BNB.txt")
BTC  <- READ_WITH_TIME("ASSET_BTC.txt")
ETH  <- READ_WITH_TIME("ASSET_ETH.txt")
XRP  <- READ_WITH_TIME("ASSET_XRP.txt")

BNB  <- xts(BNB,order.by = as.POSIXct(index(BNB)))
BTC  <- xts(BTC,order.by = as.POSIXct(index(BTC)))
ETH  <- xts(ETH,order.by = as.POSIXct(index(ETH)))
XRP  <- xts(XRP,order.by = as.POSIXct(index(XRP)))

BNB[,1:4] <- BNB[,1:4]/coredata(BNB$Close[[1000]])
BTC[,1:4] <- BTC[,1:4]/coredata(BTC$Close[[1000]])
ETH[,1:4] <- ETH[,1:4]/coredata(ETH$Close[[1000]])
XRP[,1:4] <- XRP[,1:4]/coredata(XRP$Close[[1000]])

F_NAME_1 <- "VERI_BNB/EMIR_TAM.txt"
F_NAME_2 <- "VERI_BTC/EMIR_TAM.txt"
F_NAME_3 <- "VERI_ETH/EMIR_TAM.txt"
F_NAME_4 <- "VERI_XRP/EMIR_TAM.txt"

CIZ1   <- xts(numeric(nrow(BNB)),order.by = as.POSIXct(index(BNB))); names(CIZ1)<-c('EMIR')
CIZ2   <- xts(numeric(nrow(BTC)),order.by = as.POSIXct(index(BTC))); names(CIZ2)<-c('EMIR')
CIZ3   <- xts(numeric(nrow(ETH)),order.by = as.POSIXct(index(ETH))); names(CIZ3)<-c('EMIR')
CIZ4   <- xts(numeric(nrow(XRP)),order.by = as.POSIXct(index(XRP))); names(CIZ4)<-c('EMIR')

tmp  <- read.table(F_NAME_1,header = FALSE);  CIZ1$EMIR <- tmp[,1]
tmp  <- read.table(F_NAME_2,header = FALSE);  CIZ2$EMIR <- tmp[,1]
tmp  <- read.table(F_NAME_3,header = FALSE);  CIZ3$EMIR <- tmp[,1]
tmp  <- read.table(F_NAME_4,header = FALSE);  CIZ4$EMIR <- tmp[,1]
###################################################################################
if(1){
  C<-data.frame(index(BNB))
  C<-format(C,'%Y/%m/%d')
  T<-seq(0, nrow(C), by = 1000)
  
  RENK =c("black","green","red",'blue')
  
  WALLET_1 <- ORDER_EVALUATE(coredata(CIZ1$EMIR),BNB$Close,PAR$TxnFees)
  WALLET_2 <- ORDER_EVALUATE(coredata(CIZ2$EMIR),BTC$Close,PAR$TxnFees)
  WALLET_3 <- ORDER_EVALUATE(coredata(CIZ3$EMIR),ETH$Close,PAR$TxnFees)
  WALLET_4 <- ORDER_EVALUATE(coredata(CIZ4$EMIR),XRP$Close,PAR$TxnFees)
  
  # SINIR_x=c(1,5876)
  SINIR_1=c(min(BNB$Close,BTC$Close,ETH$Close,XRP$Close),
            max(BNB$Close,BTC$Close,ETH$Close,XRP$Close))
  
  SINIR_2=c(min(WALLET_1[,5],WALLET_2[,5],WALLET_3[,5],WALLET_4[,5]),
            max(WALLET_1[,5],WALLET_2[,5],WALLET_3[,5],WALLET_4[,5]))
  
  dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)

  par(mfrow = c(2, 1),     # 2x2 layout
      oma = c(5, 4, 0, 0)+0.1, # two rows of text at the outer left and bottom margin
      mar = c(0, 0, 3, 3)+0.1, # space for one row of text at ticks and to separate plots
      mgp = c(2, 1, 0))    # axis label at 2 rows distance, tick labels at 1 row
      # xpd = NA)            # allow content to protrude into outer margin (and beyond)
  
  matplot(data.frame(BNB$Close,BTC$Close,ETH$Close,XRP$Close),  axes=FALSE,
        type=c('l','l','l','l'),col =RENK,lwd =c(1,1,1,1),lty =c(1,1,1,1), ylab='',xaxt='n',ylim=SINIR_1,
        xaxs = "i", yaxs = "i")
  grid(nx = 0, ny = NULL, col = "gray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  axis(side=2, ylim=SINIR_1, col="black",col.axis="black",las=1)
  abline( v=T, col="gray", lty=3)
  abline( v=1000, col="gray", lty=3,lwd=3)
  box()
  legend("topleft",legend=c("BNB","BTC",'ETH','XRP'),lwd=3,col=RENK)
  title(xlab = "time",
        ylab = "normalized asset price",
        outer = TRUE, line = 3)
  ###################################################################################
  matplot(data.frame(WALLET_1[,5],WALLET_2[,5],WALLET_3[,5],WALLET_4[,5]),  axes=FALSE,
        type=c('l','l','l','l'),col =RENK,lwd =c(1,1,1,1),lty =c(1,1,1,1), ylab='',xaxt='n',ylim=SINIR_2,
        xaxs = "i", yaxs = "i")
  grid(nx = 0, ny = NULL, col = "gray", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  axis(side=2, ylim=SINIR_2, col="black",col.axis="black",las=1)
  text(x=T,  par("usr")[3],labels = C[T,1], srt = 70, pos = 1,offset = 2, xpd = TRUE)
  abline( v=T, col="gray", lty=3)
  abline( v=1000, col="gray", lty=3,lwd=3)
  box()
  legend("topleft",legend=c("ret_BNB","ret_BTC",'ret_ETH','ret_XRP'),lwd=3,col=RENK)
  ###################################################################################

}
if(1){
###################################################################################
  dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
  par(mfrow = c(2,2),
      oma = c(5,4,0,0) + 0.1,
      mar = c(0,0,3,3) + 0.1)
  ###################################################################################
  matplot(data.frame(BNB$Close,1+WALLET_1[,5]),  axes=FALSE,
          type=c('l','l','l','l'),col =RENK,lwd =c(1,1,1,1),lty =c(1,1,1,1), ylab='',xaxt='n',
          xaxs = "i", yaxs = "i")
  grid(nx = 0, ny = NULL, col = "red", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  axis(side=2, ylim=SINIR_1, col="red",col.axis="red",las=1)
  abline( v=T, col="gray", lty=3)
  abline( v=1000, col="gray", lty=3,lwd=3)
  box()
  legend("topleft",legend=c("BNB","ret_BNB"),lwd=3,col=RENK)
  ###################################################################################
  matplot(data.frame(BTC$Close,1+WALLET_2[,5]),  axes=FALSE,
          type=c('l','l','l','l'),col =RENK,lwd =c(1,1,1,1),lty =c(1,1,1,1), ylab='',xaxt='n',
          xaxs = "i", yaxs = "i")
  grid(nx = 0, ny = NULL, col = "red", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  axis(side=2, ylim=SINIR_1, col="red",col.axis="red",las=1)
  abline( v=T, col="gray", lty=3)
  abline( v=1000, col="gray", lty=3,lwd=3)
  box()
  legend("topleft",legend=c("BTC","ret_BTC"),lwd=3,col=RENK)
  ###################################################################################
  matplot(data.frame(ETH$Close,1+WALLET_3[,5]),  axes=FALSE,
          type=c('l','l','l','l'),col =RENK,lwd =c(1,1,1,1),lty =c(1,1,1,1), ylab='',xaxt='n',
          xaxs = "i", yaxs = "i")
  grid(nx = 0, ny = NULL, col = "red", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  axis(side=2, ylim=SINIR_1, col="red",col.axis="red",las=1)
  abline( v=T, col="gray", lty=3)
  abline( v=1000, col="gray", lty=3,lwd=3)
  box()
  legend("topleft",legend=c("ETH","ret_ETH"),lwd=3,col=RENK)
  ###################################################################################
  matplot(data.frame(XRP$Close,1+WALLET_4[,5]),  axes=FALSE,
          type=c('l','l','l','l'),col =RENK,lwd =c(1,1,1,1),lty =c(1,1,1,1), ylab='',xaxt='n',
          xaxs = "i", yaxs = "i")
  grid(nx = 0, ny = NULL, col = "red", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  axis(side=2, ylim=SINIR_1, col="red",col.axis="red",las=1)
  abline( v=T, col="gray", lty=3)
  abline( v=1000, col="gray", lty=3,lwd=3)
  box()
  legend("topleft",legend=c("XRP","ret_XRP"),lwd=3,col=RENK)
  ###################################################################################
}
###################################################################################

if(0){
  info1 <- TRADE_PERFORMANCE_TRADE(BNB,coredata(CIZ1$EMIR),PAR)
  title(main = F_NAME_1, sub = "Sub-title", xlab = "X axis", ylab = "Y axis",
        cex.main = 2,   font.main= 4, col.main= "red",
        cex.sub = 0.75, font.sub = 3, col.sub = "green", col.lab ="darkblue"
  )
  info2 <- TRADE_PERFORMANCE_TRADE(BTC,coredata(CIZ2$EMIR),PAR)
  title(main = F_NAME_2, sub = "Sub-title", xlab = "X axis", ylab = "Y axis",
        cex.main = 2,   font.main= 4, col.main= "red",
        cex.sub = 0.75, font.sub = 3, col.sub = "green", col.lab ="darkblue"
  )
  info3 <- TRADE_PERFORMANCE_TRADE(ETH,coredata(CIZ3$EMIR),PAR)
  title(main = F_NAME_3, sub = "Sub-title", xlab = "X axis", ylab = "Y axis",
        cex.main = 2,   font.main= 4, col.main= "red",
        cex.sub = 0.75, font.sub = 3, col.sub = "green", col.lab ="darkblue"
  )
  info4 <- TRADE_PERFORMANCE_TRADE(XRP,coredata(CIZ4$EMIR),PAR)
  title(main = F_NAME_4, sub = "Sub-title", xlab = "X axis", ylab = "Y axis",
        cex.main = 2,   font.main= 4, col.main= "red",
        cex.sub = 0.75, font.sub = 3, col.sub = "green", col.lab ="darkblue"
  )
}
