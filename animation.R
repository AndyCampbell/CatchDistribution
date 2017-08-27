#animation investigations

rm(list=ls())
gc()

load("C://Stocks//MAC-NEA//StockCoordination//CatchMaps//RData//coast.rda")

#load(".//..//RData//coast.rda")
#load(".//..//RData//NEAFC.rda")
source("SRAnalyFuncs.R")

library(dplyr)

yrs<-seq(2007,2014)
months <- seq(1,12)

allDat <- lapply(months,function(x){
  CBySR <- lapply(yrs, FUN=fSubset, src = ".\\Data\\WGCatchBySR.csv", ptype="M", pnum=x)
  names(CBySR) <- yrs
  MonthDat <- rbind_all(lapply(CBySR, FUN="[", i=1:4))
  MonthDat <- MonthDat %>% dplyr::group_by(SR,Lat,Lon) %>% dplyr::summarise(Tot = sum(Tot))
})

names(allDat) <- months

#monthly 
jpeg(filename=".\\Plots\\MbySRAllYears.jpg", width=1200, height=1600, quality=100)

#layout for 12 plots on 1 page
layout(matrix(seq(1,12),nrow=4,ncol=3,byrow=TRUE))
par(omi=c(1.0,1.0,0,0))
#bottom,left,top,right
par(mar=c(0,0,0,0)+0.2)

blnxlabs <- c(F,F,F,F,F,F,F,F,F,T,T,T)
blnylabs <- c(T,F,F,T,F,F,T,F,F,T,F,F)

for (q in seq(1:12)) {
  
  dfSR <- allDat[[q]]
  
  fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),xlabs=blnxlabs[q],ylabs=blnylabs[q])
  
  with(filter(dfSR, Tot<100 & Tot>=1),
       for (i in 1:nrow(filter(dfSR, Tot<100))){
         polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                 c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                 col="lightpink", border="grey")
       }
  )
  
  with(filter(dfSR, Tot<1000 & Tot>=100),
       for (i in 1:nrow(filter(dfSR, Tot<1000 & Tot>=100))){
         polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                 c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                 col="lightpink3", border="grey")
       }
  )
  
  with(filter(dfSR, Tot<10000 & Tot>=1000),
       for (i in 1:nrow(filter(dfSR, Tot<10000 & Tot>=1000))){
         polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                 c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                 col="firebrick2", border="grey")
       }
  )
  
  with(filter(dfSR, Tot>=10000),
       for (i in 1:nrow(filter(dfSR, Tot>=10000))){
         polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                 c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                 col="firebrick4", border="grey")
       }
  )
  
  fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),refresh=TRUE,xlabs=blnxlabs[q],ylabs=blnylabs[q])
  
  text(10,50,labels=paste0("M",q),cex=2)
  
}

dev.off()



