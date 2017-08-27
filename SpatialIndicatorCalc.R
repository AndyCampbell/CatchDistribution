#centre of gravity, inertia and isotropy/anisotropy calculations

rm(list=ls())
gc()

source("SRAnalyFuncs.R")
load(".//..//RData//coast.rda")

#select a test dataset 
#e.g. January 2013

dfSR <- fSubset(y = 2012, ptype = "M", pnum = 1)      

#calculate spatial indicators
Ind <- fSpatialInd(dfSR)

#plot
jpeg(filename=".//Plots//IndicatorTest.jpg",width=1200, height=1600, quality=100)

fPlotBaseMap(xlim=c(-20,8),ylim=c(44,64))

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

fPlotBaseMap(xlim=c(-20,8),ylim=c(44,64),refresh=TRUE)

#add centre of gravity
points(Ind$CoG[2],Ind$CoG[1],pch=20,col="black",cex=6)

#add principal axes
lines(Ind$Ax1[3:4],Ind$Ax1[1:2],lwd=3)
lines(Ind$Ax2[3:4],Ind$Ax2[1:2],lwd=3)


dev.off()

