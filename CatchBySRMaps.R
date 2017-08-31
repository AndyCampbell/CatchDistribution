#Generate catch by SR maps

#read in raw data, should be in the Data directory and in a file called WGCatchBySR.csv
#it's generated from the exchange sheets by script ExtractCatchBySR.R

rm(list=ls())
gc()

load(".//..//Data//RData//coast.rda")
load(".//..//Data//RData//NEAFC.rda")
source("SRAnalyFuncs.R")


#quarterly WG report maps
#for (y in seq(2013,2016)){
for (y in seq(2016,2016)){
  for (q in 1:4){

    #1 quarterly plot per page
    #jpeg(filename=paste0(".\\Plots\\WGPlots\\CBySR",y,"_Q",q,".jpg"),
    #     width=1200, height=1600, quality=100)

    jpeg(filename=paste0(".\\Plots\\WGPlots\\",y,"\\NEAFC_CBySR",y,"_Q",q,".jpg"),
         width=1200, height=1600, quality=100)
    
    dfSR <- fSubset(y = y, ptype = "Q", pnum = q)

    blnxlabs <- c(T,T,T,T)
    blnylabs <- c(T,T,T,T)
    
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
    
    lines(Banana$Lon,Banana$Lat)
    lines(Southern$Lon,Southern$Lat)
    
    legend(x="bottomright",
           c("<100t","100t to 1000t","1000t to 10000t",">10000t"),
           fill=c("antiquewhite","lightpink","firebrick2","firebrick4"),
           border="black",
           cex=2,
           title = paste0("Q",q," ",y))
    
    dev.off()
      
  }
}



#quarterly by country
for (cry in c('IC','NO','DE','DK','BE','LT','ES','IE','PL','FO','BQ','NL','RU','SE','UKE','UKN','UKS','FR','GL')) {
  #for (y in seq(2013,2016)){
   for (y in seq(2016,2016)){
     for (q in 1:4){
      
      #1 quarterly plot per page
      #jpeg(filename=paste0(".\\Plots\\WGPlots\\",cry,"_CBySR",y,"_Q",q,".jpg"),
      #     width=1200, height=1600, quality=100)
      
      jpeg(filename=paste0(".\\Plots\\WGPlots\\",y,"\\",cry,"_NEAFC_CBySR",y,"_Q",q,".jpg"),
           width=1200, height=1600, quality=100)
      
      dfSR <- fSubset(y = y, ptype = "Q", pnum = q, Cry = cry)
      
      blnxlabs <- c(T,T,T,T)
      blnylabs <- c(T,T,T,T)
      
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
      
      lines(Banana$Lon,Banana$Lat)
      lines(Southern$Lon,Southern$Lat)
      
      legend(x="bottomright",
             c("<100t","100t to 1000t","1000t to 10000t",">10000t"),
             fill=c("antiquewhite","lightpink","firebrick2","firebrick4"),
             border="black",
             cex=2,
             title = paste(cry," Q",q," ",y,sep=""))
      
      dev.off()
      
    }
  }
}


#monthly
for (y in seq(2013,2016)){
  for (m in 1:12){
    
    #1 plot per page
    #jpeg(filename=paste0(".\\Plots\\WGPlots\\CBySR",y,"_M",m,".jpg"),
    #     width=1200, height=1600, quality=100)
    
    jpeg(filename=paste0(".\\Plots\\WGPlots\\",y,"\\NEAFC_CBySR",y,"_M",m,".jpg"),
         width=1200, height=1600, quality=100)
    
    dfSR <- fSubset(y = y, ptype = "M", pnum = m)
    
    blnxlabs <- c(T,T,T,T,T,T,T,T,T,T,T,T)
    blnylabs <- c(T,T,T,T,T,T,T,T,T,T,T,T)
    
    fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),xlabs=blnxlabs[m],ylabs=blnylabs[m])
    
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
    
    fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),refresh=TRUE,xlabs=blnxlabs[m],ylabs=blnylabs[m])
    
    lines(Banana$Lon,Banana$Lat)
    lines(Southern$Lon,Southern$Lat)
    
    legend(x="bottomright",
           c("<100t","100t to 1000t","1000t to 10000t",">10000t"),
           fill=c("antiquewhite","lightpink","firebrick2","firebrick4"),
           border="black",
           cex=2,
           title = paste("M",m,y,sep=" "))
    
    dev.off()
    
  }
}


#no guarantees from here!


############################################################################################


for (y in seq(1998,2015)) {

  jpeg(filename=paste0(".\\Plots\\QCBySR",y,".jpg"),width=1200, height=1600, quality=100)
  
  #layout for 4 plots on 1 page
  layout(matrix(seq(1,4),nrow=2,ncol=2,byrow=TRUE))
  par(omi=c(1.0,1.0,0,0))
  #bottom,left,top,right
  par(mar=c(0,0,0,0)+0.2)
  
  blnxlabs <- c(F,F,T,T)
  blnylabs <- c(T,F,T,F)
  
  for (q in seq(1:4)) {
    
    dfSR <- fSubset(y = y, ptype = "Q", pnum = q)
    
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
    
    text(10,50,labels=paste0("Q",q," ",y),cex=2)
    
  }
  
  dev.off()

  
  
  #monthly 
  jpeg(filename=paste0(".\\Plots\\MCBySR",y,".jpg"),width=1200, height=1600, quality=100)
  
  #layout for 12 plots on 1 page
  layout(matrix(seq(1,12),nrow=4,ncol=3,byrow=TRUE))
  par(omi=c(1.0,1.0,0,0))
  #bottom,left,top,right
  par(mar=c(0,0,0,0)+0.2)
  
  blnxlabs <- c(F,F,F,F,F,F,F,F,F,T,T,T)
  blnylabs <- c(T,F,F,T,F,F,T,F,F,T,F,F)
  
  for (q in seq(1:12)) {
    
    dfSR <- fSubset(y = y, ptype = "M", pnum = q)
    
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
    
    text(10,50,labels=paste0("M",q," ",y),cex=2)
    
  }
  
  dev.off()
    
}



#winter maps (Oct-Mar), DE,DK,UKS,IE & NL only
for (y in seq(2006,2015)) {
  
  jpeg(filename=paste0(".\\Plots\\MCBySR",y,"-",y+1,".jpg"),width=1600, height=1200, quality=100)

  #layout for 6 plots on 1 page (landscape)
  layout(matrix(seq(1,6),nrow=2,ncol=3,byrow=TRUE))
  par(omi=c(1.0,1.0,1.0,1.0))
  #bottom,left,top,right
  par(mar=c(0,0,0,0)+0.2)
  
  blnxlabs <- c(F,F,F,T,T,T)
  blnylabs <- c(T,F,F,T,F,F)

  for (q in c(10,11,12,1,2,3)) {

    if (q>3){
      dfSR <- fSubset(y = y, ptype = "M", pnum = q, Cry = c("UKS","IE","DE","DK","NL"))      
    } else {
      dfSR <- fSubset(y = y+1, ptype = "M", pnum = q, Cry = c("UKS","IE","DE","DK","NL"))
    }
    
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
    
    if (q>3) {
      text(10,50,labels=paste0("M",q,",",y),cex=2)} 
    else {
      text(10,50,labels=paste0("M",q,",",y+1),cex=2)
    }
    
  }
  
  dev.off()
  
}


#catch by month 2006-2015 for selected nations "UKS","IE","DE","DK","NL"
#includes centre of gravity and principal axes

Month_long <- c("January","February","March","April","May","June","July","August","September","October","November","December")
Month_short <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

for (m in 1:12) {
  
  #Monthly maps 2006-2015 DE,DK,UKS,IE & NL only
  jpeg(filename=paste0(".//Plots//MCBySR_",Month_long[m],".jpg"),width=1600, height=1200, quality=100)
  
  #layout for 8 plots on 1 page (landscape)
  layout(matrix(seq(1,8),nrow=2,ncol=4,byrow=TRUE))
  par(omi=c(1.0,1.0,1.0,1.0))
  #bottom,left,top,right
  par(mar=c(0,0,0,0)+0.2)
  
  for (y in 2006:2015){
  
    dfSR <- fSubset(y = y, ptype = "M", pnum = m, Cry = c("UKS","IE","DE","DK","NL"))      
    
    #fPlotBaseMap(xlim=c(-20,8),ylim=c(44,64))
    fPlotBaseMap(xlim=c(-14,6),ylim=c(44,62))
    
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
    
    #fPlotBaseMap(xlim=c(-20,8),ylim=c(44,64),refresh=TRUE)
    fPlotBaseMap(xlim = c(-14,6), ylim = c(44,62), refresh = TRUE)
    
    text(2,48,paste(y,Month_short[m],sep=","),cex=2)
    
    Ind <- fSpatialInd(dfSR)
    
    #add centre of gravity
    points(Ind$CoG[2],Ind$CoG[1],pch=20,col="black",cex=6)
    text(3,47,paste0(round(Ind$CoG[2],2),",",round(Ind$CoG[1],2)),cex=2)
    
    #add principal axes
    lines(Ind$Ax1[3:4],Ind$Ax1[1:2],lwd=3)
    lines(Ind$Ax2[3:4],Ind$Ax2[1:2],lwd=3)
    
    #inertia
    text(3,46,round(Ind$Inertia,2),cex=2)
    
  }
  
  
  dev.off()

}


#Banana hole maps - 14th Oct 2015, request from Henrik
#WG report maps
for (y in seq(2013,2015)){
  for (q in 1:4){
    
    #1 quarterly plot per page
    #jpeg(filename=paste0(".\\Plots\\WGPlots\\CBySR",y,"_Q",q,".jpg"),
    #     width=1200, height=1600, quality=100)
    
    jpeg(filename=paste0(".\\Plots\\WGPlots\\NEAFC_CBySR",y,"_Q",q,".jpg"),
         width=1200, height=1600, quality=100)
    
    dfSR <- fSubset(y = y, ptype = "Q", pnum = q)
    
    blnxlabs <- c(T,T,T,T)
    blnylabs <- c(T,T,T,T)
    
    fPlotBaseMap(xlim=c(-8,10),ylim=c(64,77),xlabs=blnxlabs[q],ylabs=blnylabs[q])
    
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
    
    fPlotBaseMap(xlim=c(-8,10),ylim=c(64,77),refresh=TRUE,xlabs=blnxlabs[q],ylabs=blnylabs[q])
    
    lines(Banana$Lon,Banana$Lat)
    lines(Southern$Lon,Southern$Lat)
    
    legend(x="bottomright",
           c("<100t","100t to 1000t","1000t to 10000t",">10000t"),
           fill=c("antiquewhite","lightpink","firebrick2","firebrick4"),
           border="black",
           cex=2,
           title = paste("Q",q,y,sep=" "))
    
    dev.off()
    
  }
}
