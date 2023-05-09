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
library(lubridate)
library(quantmod)
library(tidyverse)
require(neuralnet)
library(class) # LVQ
library(caret)
library(GGally)
library(e1071) # FCM
library(corrplot)
library(factoextra)
source("~/R_FUNC/func_BEST.R")
source("~/R_FUNC/func_FILE.R")
source("~/R_FUNC/func_BACKTEST.R")
source("~/R_FUNC/func_CI.R")
source("~/R_FUNC/func_auxM3.R")

# LEAD SONUNCU NA OLUR
# listenin i. elemanını i-1. elemanı olarak yazar
# yeni tarihli veriyi daha önceki tarihli verinin yerine yazar

# LAG 1. NA OLUR
# listenin i. elemanını i+1. elemanı olarak yazar
# eski tarihli veriyi daha önceki tarihli verinin yerine yazar.
# EMIR CLOSE ILE HESAPLANDI ISE BIR SONRAKI CLOSE'DA ISLEM GORECEGI CIN
# LAG EDILMELI ClCl kullanılabilmesi için

#___________________________________________________________________________________
set.seed(Sys.time())
#graphics.off()
options(device = "RStudioGD")
options(device = "x11")
#___________________________________________________________________________________
PAR <- GET_PAR()
DATA_TAM   <-   1:1000
DATA_TRAIN <-   1:500
DATA_TEST  <- 501:1000
# AL_SAT   <- READ_WITH_TIME("BEST.txt")
TI         <- READ_WITH_TIME("ASSET_TI.txt")
TI         <- xts(TI,order.by = as.POSIXct(index(TI)))
# AL_SAT   <- GET_BEST_ORDER_1(TI,PAR)
AL_SAT     <- read.table("BEST_ord.txt",header = FALSE)
AL_SAT     <- as.xts(x=coredata(AL_SAT[,2]),order.by = as.POSIXct(index(TI)))
colnames(AL_SAT)<-'EMIR'
# AD<-names(TI)
# for(i in 1:ncol(TI)){
#   print(c(AD[i],round(min(TI[,i]),2),round(max(TI[,i]),2)))
# }
# stop()
##############################################
##############################################
AL_SAT<- AL_SAT[DATA_TAM,]
TI    <- TI[DATA_TAM,]

TI$A0 <- lag(coredata(TI$ATR),0)
TI$A1 <- lag(TI$A0,1)
TI$A2 <- lag(TI$A1,1)

TI$B0 <- lag(coredata(TI$RSIC),0)
TI$B1 <- lag(TI$B0,1)
TI$B2 <- lag(TI$B1,1)*0

TI$C0 <- lag(coredata(TI$EMA),0)
TI$C1 <- lag(TI$C0,1)
TI$C2 <- lag(TI$C1,1)

TI$D0 <- lag(coredata(TI$STOCH),0)
TI$D1 <- lag(TI$D0,1)
TI$D2 <- lag(TI$D1,1)*0

TI$E0 <- lag(coredata(TI$OBV),0)
TI$E1 <- lag(TI$E0,1)
TI$E2 <- lag(TI$E1,1)*0
TI[is.na(TI)] <-0

df_TAM     <- data.frame(TI$A0 ,TI$A1 ,TI$A2 ,
                         TI$B0 ,TI$B1 ,TI$B2 ,
                         TI$C0 ,TI$C1 ,TI$C2 ,
                         TI$D0 ,TI$D1 ,TI$D2 ,
                         TI$E0 ,TI$E1 ,TI$E2 ,
                         AL_SAT$EMIR)

df_TRAIN   <- data.frame(df_TAM[DATA_TRAIN,])
df_TEST    <- data.frame(df_TAM[DATA_TEST ,])
n          <- names(df_TAM)
o_name     <- n[length(n)]
i_name     <- n[!n %in% o_name]
nINP       <- (ncol(df_TAM)-1)
IN         <- (1:nINP)
OUT        <- (ncol(df_TAM))
rownames(df_TRAIN) <- 1:nrow(df_TRAIN)
#___________________________________________________________________________________
TI$RSI <- ((TI$RSI+100)/2)
#___________________________________________________________________________________

#___________________________________________________________________________________
if(0){# FC-means & K-means
  nC <- 10 # kume sayısı
  cm <- cmeans(df_TRAIN[,IN],centers=nC,iter.max=1000,dist="euclidean",m=2)
  km <- kmeans(df_TRAIN[,IN],centers=nC)

  W      <- as.data.frame(cm$membership)
  center <- as.data.frame(cm$centers)
  
  c   <- nrow(center)
  dim <- ncol(center)
  df_TAM[,OUT] <- df_TAM[,OUT] == 1
  

  D_TAM   <- HESAP_D (df_TAM[,IN],center)
  W_TAM   <- ((HESAP_WF(D_TAM,m=2)-0.5)*2)
  W_TAM   <- data.frame(cbind(W_TAM,df_TAM[,OUT]))
  W_TRAIN <- data.frame(W_TAM[DATA_TRAIN,])
  W_TEST  <- data.frame(W_TAM[DATA_TEST ,])
  
  LAYER  <- c(1)
  ESIK   <- 0.5

  nn     <- TRADE_ANN(W_TRAIN,ESIK,LAYER)
  
  R_TAM  <- predict(nn,W_TAM,all.units = FALSE)
  CIZ5   <- as.matrix((R_TAM[,1]>0.5)*2-1)
  CIZ5   <- lead(coredata(CIZ5,1))
  CIZ5[is.na(CIZ5)] <-0
  
  
  R_TEST <- predict(nn,W_TEST,all.units = FALSE)
  CIZ6   <- as.matrix((R_TEST[,1]>0.5)*2-1)
  CIZ6   <- (rbind(as.matrix(df_TRAIN[,OUT])*2-1,as.matrix(CIZ6)))
  # CIZ6   <- lead(coredata(CIZ6),1)
  CIZ6[is.na(CIZ6)] <-0
  
  # CIZ5 <- ORDER_KON(CIZ5,TI$RSI,TI$Close)
  # CIZ6 <- ORDER_KON(CIZ6,TI$RSI,TI$Close)
  info_1 <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ5),PAR)
  info_2 <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ6),PAR)
  
  if(0){
    X     <- matrix(0,ncol=dim+1,nrow=100*100)
    ind <- 1
    for(i in 1:100){
      for(j in 1:100){
        X[ind,1] <- (i-50)*2
        X[ind,2] <- (j-50)*2
        ind <- (ind+1)
      }
    }
    n     <- nrow(X)
    tmp_D <- matrix(0,ncol=c,nrow=n)
    tmp_W <- matrix(0,ncol=c,nrow=n)
    tmp_D <- HESAP_D (tmp_D,X,center)
    tmp_W <- HESAP_WF(tmp_W,tmp_D,m=2)
  
    XX    <- matrix(0,ncol=100,nrow=100)
    YY    <- matrix(0,ncol=100,nrow=100)
    ZZ1   <- matrix(0,ncol=100,nrow=100)
    ZZ2   <- matrix(0,ncol=100,nrow=100)
    ZZ3   <- matrix(0,ncol=100,nrow=100)
    ZZ4   <- matrix(0,ncol=100,nrow=100)
    ind <- 1
    for(i in 1:100){
      for(j in 1:100){
        XX[i,j]<- (i-50)*2
        YY[i,j]<- (j-50)*2
        ZZ1[i,j]<- tmp_W[ind,1]
        ZZ2[i,j]<- tmp_W[ind,2]
        ZZ3[i,j]<- tmp_W[ind,3]
        ZZ4[i,j]<- tmp_W[ind,4]
        ind <- (ind+1)
      }
    }
    library(rgl)
    AD <-names(c('in_1','in_2'))
  
    jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
    colorzjet <- jet.colors(100)  # 100 separate color 
    RENK_1 <- colorzjet[ findInterval(ZZ1, seq(min(ZZ1), max(ZZ1), length=100))]
    RENK_2 <- colorzjet[ findInterval(ZZ2, seq(min(ZZ2), max(ZZ2), length=100))]
    RENK_3 <- colorzjet[ findInterval(ZZ3, seq(min(ZZ3), max(ZZ3), length=100))]
    RENK_4 <- colorzjet[ findInterval(ZZ4, seq(min(ZZ4), max(ZZ4), length=100))]
    open3d()
    persp3d(XX,YY,ZZ1,xlab=AD[3],ylab=AD[2],zlab=AD[1],color=RENK_1 )
    persp3d(XX,YY,ZZ2,xlab=AD[3],ylab=AD[2],zlab=AD[1],color=RENK_2,add = TRUE)
    persp3d(XX,YY,ZZ3,xlab=AD[3],ylab=AD[2],zlab=AD[1],color=RENK_3,add = TRUE)
    persp3d(XX,YY,ZZ4,xlab=AD[3],ylab=AD[2],zlab=AD[1],color=RENK_4,add = TRUE)
    clear3d(type = "lights")
    light3d(theta=0, phi=0)
    light3d(theta=0, phi=0)  # twice as much light.
    grid3d("x")
    grid3d("y")
    grid3d("z")
  }
  
  if(0){
    print(cm)
    print(km)
    # corrplot(cm$membership, is.corr = FALSE)
    print(
      fviz_cluster(list(data = df_TRAIN, cluster=cm$cluster),
                   choose.vars = c("BB_MID", "RSI"),
                   stand = FALSE,
                   ellipse = TRUE,
                   ellipse.type = "norm",
                   ellipse.level = 0.68,
                   palette = "jco",
                   ggtheme = theme_minimal())
    )
    dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
    plot.new()
    print(
      fviz_cluster(list(data = df_TRAIN, cluster=km$cluster),
                   choose.vars = c("BB_MID", "RSI"),
                   stand = FALSE,
                   ellipse = TRUE,
                   ellipse.type = "norm",
                   ellipse.level = 0.68,
                   palette = "jco",
                   ggtheme = theme_minimal())
    )
  }
}
#___________________________________________________________________________________
if(1){
  # df_TRAIN[,1:5]<- lead(coredata(df_TRAIN[,1:5],1))
  # df_TRAIN[is.na(df_TRAIN)] <-1
  # df_TRAIN$EMIR<- lead(coredata(df_TRAIN$EMIR,1))
  # df_TRAIN$EMIR[1]<-1
  # df_TRAIN$EMIR[is.na(df_TRAIN$EMIR)] <-1
  
  df_TRAIN$EMIR <- df_TRAIN$EMIR == 1
  LAYER  <- c(20,20,3)
  ESIK   <- 1.5
  nn     <- TRADE_ANN(df_TRAIN,ESIK,LAYER)

  R_TAM  <- predict(nn,df_TAM,all.units = FALSE)
  CIZ5   <- as.matrix((R_TAM[,1]>0.5)*2-1)
  CIZ5   <- lead(coredata(CIZ5,1))
  CIZ5[is.na(CIZ5)] <-0


  R_TEST <- predict(nn,df_TEST,all.units = FALSE)
  CIZ6   <- as.matrix((R_TEST[,1]>0.5)*2-1)
  CIZ6   <- (rbind(as.matrix(df_TRAIN[,OUT])*2-1,as.matrix(CIZ6)))
  # CIZ6   <- lead(coredata(CIZ6),1)
  CIZ6[is.na(CIZ6)] <-0
  
  TI <- TI[,1:4]
  info_1 <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ5),PAR)
  info_2 <- TRADE_PERFORMANCE_TRADE(TI,coredata(CIZ6),PAR)
}
#___________________________________________________________________________________
