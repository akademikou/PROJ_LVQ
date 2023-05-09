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

library(dplyr)     # lead lag
library(lubridate)
library(quantmod)
library(ggplot2)
library(plotly)
library(rgl)
library(RColorBrewer)
library(oce)
library(colorRamps)
library(gplots)

source("~/R_FUNC/func_BEST.R")
source("~/R_FUNC/func_FILE.R")
source("~/R_FUNC/func_BACKTEST.R")

if(1){
  PAR <- GET_PAR()
  F1 <-   1:1000 # 5895
  
  TI      <- READ_WITH_TIME("ASSET_TI.txt")
  TI      <- xts(TI,order.by = as.POSIXct(index(TI)))
  
  AL_SAT1 <- read.table("BEST_ord.txt",header = FALSE)
  AL_SAT1 <- as.xts(x=coredata(AL_SAT1[,2]),order.by = as.POSIXct(index(TI)))
  colnames(AL_SAT1)<-'EMIR'
  
  AL_SAT2 <- read.table("BEST_poz.txt",header = FALSE)
  AL_SAT2 <- as.xts(x=coredata(AL_SAT2[,2]),order.by = as.POSIXct(index(TI)))
  colnames(AL_SAT2)<-'EMIR'
  
  # AL_SAT3 <- GET_BEST_ORDER_1(TI,PAR)
  # AL_SAT3 <- as.xts(x=coredata(AL_SAT3[,2]),order.by = as.POSIXct(index(TI)))
  # colnames(AL_SAT3)<-'EMIR'
  
  TI      <- TI[F1,]
  AL_SAT1 <- AL_SAT1[F1,]
  AL_SAT2 <- AL_SAT2[F1,]
  # AL_SAT2 <- AL_SAT2[F1,]
  
  AL_SAT2$EMIR <- lead(coredata(AL_SAT2$EMIR),1)
  AL_SAT2[is.na(AL_SAT2)] <-0
  
  info_2 <- TRADE_PERFORMANCE_TRADE(TI,coredata(AL_SAT2$EMIR),PAR)
  stop()
}
########################################################################
if(0){
  CIZ<-TI[,1:6]
  lim2 <- c(-150,150)
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
                          " addTA(TI$Close, col='blue', type='s',lwd=2,  on=1,   legend=NULL,yrange=lim2)",
                          " addTA(AL_SAT2$EMIR, col='black', type='s',lwd=1,  on=1,   legend=NULL,yrange=lim2)"
                        )
  )
  stop()
}
########################################################################

PSO  <- read.table("PSO_OPT.txt",header = FALSE)
nPAR <- (ncol(PSO)-3)
nPSS <- 5
nC   <- (3*nPSS)
nGIR <- (nPAR/nC)

line <- 2
ilk  <- (PSO[line,(ncol(PSO)-1)]+1)
son  <- (PSO[line,(ncol(PSO)-0)])+nrow(df)*0
W    <- (PSO[line,1:(ncol(PSO)-3)]);
D    <- numeric(nC);

x  <-seq(-100,100,1);
y  <-seq(-100,100,1); 

XX <- matrix(nrow = length(x), ncol=length(y))
YY <- matrix(nrow = length(x), ncol=length(y))
Z1 <- matrix(nrow = length(x), ncol=length(y))
Z2 <- matrix(nrow = length(x), ncol=length(y))
Z3 <- matrix(nrow = length(x), ncol=length(y))

i<-1
for(val_1 in x) {
  j<-1
  for(val_2 in y) {
    ind  <- 1;
    for (ii in 1:nC){
      D[ii] <- 0
      ind <- ((ii-1)*nGIR)
      err1  <- (val_1-W[[(16+ind)]]);
      err2  <- (val_2-W[[(13+ind)]]);
      D[ii] <- sqrt((err1*err1)+(err2*err2));

    }
    OUT <- which.min(D);
    net <- floor((OUT-1)/nPSS)
    if(net == 0){nEMIR <- (-1);}
    else if(net == 1){nEMIR <- ( 0);}
    else if(net == 2){nEMIR <- ( 1);}
    else{print('HATA'); stop()}
    
    
    XX[i,j]<- val_1
    YY[i,j]<- val_2
    Z1[i,j]<- D[OUT]
    Z2[i,j]<- OUT
    Z3[i,j]<- nEMIR
    j<-j+1
  }
  i<-i+1
}
Z1[is.na(Z1)] <-0
Z2[is.na(Z2)] <-0
Z3[is.na(Z3)] <-0

AD <-names(c('in_1','in_2'))


xlim = range(x, finite = TRUE)
ylim = range(y, finite = TRUE)
zlim1= range(Z1,finite = TRUE)
zlim2= range(Z2,finite = TRUE)
zlim3= range(Z3,finite = TRUE)

# A <- colorRampPalette(c("red", "blue", "green" ))
# RRR <- A(15) 
# RRR <- rainbow(15)
# RRR <- brewer.pal(n = 11, name = "Spectral")
# RRR <- rich.colors(15)
RRR <- c('red'  ,'red1'  ,'red2'  ,'red3'  ,'red4',
         'green','green1','green2','green3','green4',
         'blue' ,'blue1' ,'blue2' ,'blue3' ,'blue4')


RRR1 <- blue2green2red(150)
RRR2 <- blue2green2red(15)
RRR3 <- c('blue','green','red')
if(0){
  dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
  plot.new()
  imagep(x ,y, Z1,col = RRR)
  dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
  plot.new()
  imagep(x ,y, Z2,col = RRR)
  dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
  plot.new()
  imagep(x ,y, Z3,col = RRR)
}
#############################################################################
if(0){
  fig1 <- plot_ly(z=Z1,x=u1$U,y=u2$U,type = "contour",
                 colorscale = 'jet', autocontour = TRUE,
                 contours = list(start = 1,end = 15,size = 1,showlabels = TRUE),
                 line = list(smoothing = 0.85), width = 600, height = 500)
  config(fig1, staticPlot = TRUE)
  fig1 <- fig1 %>% colorbar(title = "clusters")
  
  fig2 <- plot_ly(z=Z2,x=u1$U,y=u2$U,type = "contour",
                  colorscale = 'jet', autocontour = TRUE,
                  contours = list(start = 1,end = 15,size = 1,showlabels = TRUE),
                  line = list(smoothing = 0.85), width = 600, height = 500)
  config(fig2, staticPlot = TRUE)
  fig2 <- fig2 %>% colorbar(title = "clusters")
  
  fig3 <- plot_ly(z=Z3,x=u1$U,y=u2$U,type = "contour",
                  colorscale = 'jet',
                  autocontour = TRUE,
                  contours = list(start = 1,end = 15,size = 1,showlabels = TRUE),
                  line = list(smoothing = 0.85), width = 600, height = 500)
  config(fig3, staticPlot = TRUE)
  fig3 <- fig3 %>% colorbar(title = "clusters")
}
#############################################################################
# dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
# plot.new()
# par(mar=c(5, 10, 4, 6) + 0.1) # alt sol   
# filled.contour(x, y, Z1, xlim=xlim,ylim=ylim,zlim=zlim1,
#                levels = 1:150, nlevels = 150, col = RRR1)

# dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
# plot.new()
# par(mar=c(5, 10, 4, 6) + 0.1) # alt sol   
# filled.contour(x, y, Z2, xlim=xlim,ylim=ylim,zlim=zlim2,
#                levels = 1:15, nlevels = 15, col = RRR2)
# 
dev.new(width = dev_width, height = dev_height, unit = dev_unit,noRStudioGD = TRUE)
plot.new()
par(mar=c(5, 10, 4, 6) + 0.1) # alt sol   
filled.contour(x, y, Z3, xlim=xlim,ylim=ylim,zlim=zlim3,
               levels = -1.5:1.5, nlevels = 3, col = RRR3)

#############################################################################
if(0){
  open3d()
  persp3d(XX,YY,z=Z1,xlab=AD[3],ylab=AD[2],zlab=AD[1],col=RRR1[ findInterval(Z1, seq(min(Z1), max(Z1), length=150))])
  # mesh3d(XX,YY,Z1) # persp3d surface3d
  aspect3d(1,1,1); axes3d()
  clear3d(type = "lights")
  light3d(theta=30, phi=30)
  grid3d("x"); grid3d("y"); grid3d("z")
  
  open3d()
  persp3d(XX,YY,z=Z2,xlab=AD[3],ylab=AD[2],zlab=AD[1],col=RRR2[ findInterval(Z2, seq(min(Z2), max(Z2), length=15))])
  aspect3d(1,1,1); axes3d()
  clear3d(type = "lights")
  light3d(theta=30, phi=30)
  grid3d("x"); grid3d("y"); grid3d("z")
  
  open3d()
  persp3d(XX,YY,z=Z3,xlab=AD[3],ylab=AD[2],zlab=AD[1],col=RRR3[ findInterval(Z3, seq(min(Z3), max(Z3), length=3))])
  aspect3d(1,1,1); axes3d()
  clear3d(type = "lights")
  light3d(theta=30, phi=30)
  grid3d("x"); grid3d("y"); grid3d("z")
}