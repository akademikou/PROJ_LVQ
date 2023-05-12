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


PAR <- GET_PAR()
BNB  <- READ_WITH_TIME("ASSET_BNB.txt")
BTC  <- READ_WITH_TIME("ASSET_BTC.txt")
ETH  <- READ_WITH_TIME("ASSET_ETH.txt")
XRP  <- READ_WITH_TIME("ASSET_XRP.txt")

BNB  <- xts(BNB,order.by = as.POSIXct(index(BNB)))
BTC  <- xts(BTC,order.by = as.POSIXct(index(BTC)))
ETH  <- xts(ETH,order.by = as.POSIXct(index(ETH)))
XRP  <- xts(XRP,order.by = as.POSIXct(index(XRP)))

BNB$Close <- BNB$Close/coredata(BNB$Close[[1]])
BTC$Close <- BTC$Close/coredata(BTC$Close[[1]])
ETH$Close <- ETH$Close/coredata(ETH$Close[[1]])
XRP$Close <- XRP$Close/coredata(XRP$Close[[1]])

if(1){
  C<-data.frame(index(BNB))
  C<-format(C,'%Y/%m/%d')
  T<-seq(1, nrow(C), by = nrow(C)/10)
  
  SINIR=c(min(BNB$Close,BTC$Close,ETH$Close,XRP$Close),
          max(BNB$Close,BTC$Close,ETH$Close,XRP$Close))
  
  dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
  plot.new()
  par(mar = c(15, 4, 4, 4) + 0.3)  # Leave space for z axis
  matplot(data.frame(BNB$Close,BTC$Close,ETH$Close,XRP$Close),  axes=FALSE,
          type=c('l','l','l','l'),
          col =c("black","green","red",'blue'),
          lwd =c(1,1,1,1),lty =c(1,1,1,1), ylab='',xaxt='n')
  
  grid(nx = 0, ny = NULL, col = "red", lty = "dotted",lwd = par("lwd"), equilogs = TRUE)
  axis(side=2, ylim=SINIR, col="red",col.axis="red",las=1)
  # axis(side=1,at=T,labels=C[T,1],las=2)
  # axis(side=1,at=T,labels = FALSE)
  text(x=T,  par("usr")[3],labels = C[T,1], srt = 70, pos = 1,offset = 2, xpd = TRUE)
  abline( v=T, col="gray", lty=3)
  box()
}
