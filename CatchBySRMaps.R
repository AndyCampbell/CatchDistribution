#Generate catch by SR maps

#read in raw data, should be in the Data directory and in a file called WGCatchBySR.csv
#it's generated from the exchange sheets by script ExtractCatchBySR.R

#Change Log
#15/01/2019 - WKIBPNEAMac ggplot maps
#08/01/2019 - WKIBPNEAMac maps
#21/08/2018 - WGWIDE 2018 maps
#11/10/2018 - Irish mackerel catch by week (for Roisin Pinfield)
#05/11/2018 - 2018 Stockbook maps
#20/11/2018 - BW maps by country (for BIM report)


rm(list=ls())
gc()

library(dplyr)
library(geosphere)
library(rgdal)
library(gpclib)
library(maptools)
library(rgeos)
library(ggplot2)
library(RColorBrewer)
library(reshape)
library(dplyr)
library(ggpubr)     #arranging ggplots on a grid
library(xtable)     #latex tables

gpclibPermit()

load(".//..//Data//RData//coast.rda")
load(".//..//Data//RData//NEAFC.rda")
load(".//..//Data//RData//SR.rda")
dfSR$SR <- levels(dfSR$Rect)[dfSR$Rect]     #SR as a character

source("SRAnalyFuncs.R")

Months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
Months.Abbrev <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
Four.Colours <- c("lightpink", "lightpink3", "firebrick2","firebrick4")
Catch.Bins <- c("1t-100t","100t-1000t","1000t-3000t",">3000t")
Scanned.Bins <- c("1t-100t","100t-1000t","1000t-3000t",">3000t")
Catch.Limits <- c(1,100,1000,3000)
Scanned.Limits <- c(1,100,1000,3000)

#15th January - WKIBPNEAMac
#migrating to ggplot maps and including plot of catches scanned for tags and tag return numbers

eco <- subset(read.csv(file=file.path(".","..","Data","RData","iarc.eco.2008.csv"), header = TRUE, sep=","), iso2 != "EU27")
map <- readShapeSpatial(fn=file.path(".","..","Data","maps","countries//CNTR_RG_60M_2006"), proj4string = CRS("+proj=longlat"))
map <- reshape::rename(map, c(CNTR_ID = "iso2")) #; summary(map)

map@data$id <- rownames(map@data)
map.points <- fortify(map, region = "id")
map.df <- dplyr::inner_join(map.points, map@data, by = "id")
map.df <- dplyr::inner_join(map.df, eco, by = "iso2")

#tag data
#created by separate R project first
load(file=file.path("C:","Stocks","mac.27.nea","DataAnalysis","RFID","RFID.RData"))

dfCatches2 <- dplyr::mutate(dfCatches,
                            ScanDate = lubridate::ymd_hms(Processing_Date),
                            Julian = lubridate::yday(ScanDate),
                            ScanYear = lubridate::year(ScanDate),
                            ScanMonth = lubridate::month(ScanDate),
                            PType = "M",
                            Nation = dplyr::case_when(Nation == "Eire" ~ "IE",
                                                      Nation == "Norway" ~ "NO",
                                                      Nation == "Sweden" ~ "SE",
                                                      Nation == "GB" ~ "UKS",
                                                      Nation == "IS" ~ "IC",
                                                      TRUE ~ Nation)) %>%
  select(Year= ScanYear, Month=ScanMonth, PType, Date=ScanDate, Julian,
         SR = ices_rectangle, Ctry=Nation, Catch=Weight, AvgWeight=WeightAvg) %>%
  left_join(select(dfSR,SR,South,North,East,West,MidLat,MidLon)) %>%
  select(Ctry, Year, SR, Lat=MidLat, Lon=MidLon, PType, PNum = Month, Catch)

dfTot <- dfCatches2 %>%
  group_by(Year,SR,Lat,Lon) %>%
  summarise(Tot=sum(Catch)/1e3)

dfMonthly <- dfCatches2 %>%
  group_by(Year,PNum,SR,Lat,Lon) %>%
  summarise(Tot=sum(Catch)/1e3)

#check for any unmatched stat rectangles
sum(is.na(dfCatches2$North))

#number of recaptures (specific missions (experiments))
dfRecp <- dplyr::filter(dfExpeditions,!is.na(RecaptureDate) & 
                          Mission %in% c(2011808,2012836,2013828,2014809,2015830,2016832,2017837,2018828))

#retrieve the catch details for the recapture
dfRecp2 <- dplyr::left_join(dfRecp,select(dfCatches,pkID,ices_rectangle,Processing_Date,Nation),by=c("CatchID"="pkID"))

dfRecp3 <- dfRecp2 %>%
  select(ices_rectangle,Processing_Date,Nation) %>%
  mutate(ScanDate = lubridate::ymd_hms(Processing_Date)) %>%
  mutate(ScanYear = lubridate::year(ScanDate)) %>%
  mutate(ScanMonth = lubridate::month(ScanDate)) %>%
  select(SR=ices_rectangle,ScanYear,ScanMonth,Nation) %>%
  filter(!is.na(SR) & !is.na(Nation)) %>%
  mutate(Nation = case_when(Nation == "Eire" ~ "IE",
                            Nation == "Norway" ~ "NO",
                            Nation == "Sweden" ~ "SE",
                            Nation == "GB" ~ "UKS",
                            Nation == "IS" ~ "IC",
                            TRUE ~ Nation)) %>%
  group_by(ScanYear,ScanMonth,SR,Nation) %>%
  summarise(count=n()) %>%
  left_join(select(dfSR,SR,South,North,East,West,MidLat,MidLon))

dfRecp <- dfRecp %>%
  select(ICES,ReleseDate,RecaptureDate) %>%
  mutate(Release = lubridate::ymd_hms(ReleseDate),
         Recapture = lubridate::ymd_hms(RecaptureDate)) %>%
  select(ICES,Release,Recapture)

dfScannedCatch <- dfCatches2 %>%
  group_by(Ctry,Year,SR,Lat,Lon,PType,PNum) %>%
  summarise(Tot=sum(Catch)/1e3)

#join with number of recaptures
dfScannedCatch <- dplyr::full_join(dfScannedCatch,
                                    dplyr::select(dfRecp3,Ctry=Nation,Year=ScanYear,PNum=ScanMonth,SR=SR,count))
dfScannedCatch$count[is.na(dfScannedCatch$count)]<-0

table(dfScannedCatch$Year)
table(dfScannedCatch$Ctry)

#all catch by SR data
dfCatchBySR <- read.table(file = ".\\Data\\WGCatchBySR.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#select countries, years 
dfCatchBySR <- dplyr::filter(dfCatchBySR, Ctry %in% c('FO','IC','NO','UKS','IE','DK') & Year>=2012 & Year<=2017)
#all RFID data
dfRFID <- dplyr::filter(dfScannedCatch, Ctry %in% c('FO','IC','NO','UKS','IE','DK') & Year>=2012 & Year<=2017)

#generate the plots
#for (cry in c('IC','FO','NO','IC','UKS')) {
for (cry in c('DK')) {

  #country annual totals
  dfCountryCatch <- dfCatchBySR %>%
    filter(Ctry == cry & PType == "M") %>%
    select(Ctry,Year,Catch) %>%
    group_by(Ctry,Year) %>%
    summarise(Total = sum(Catch))

  dfCountryRFID <- ungroup(dfRFID) %>%
    filter(Ctry == cry & PType == "M") %>%
    select(Ctry,Year,Tot,count) %>%
    group_by(Ctry,Year) %>%
    summarise(Scanned = sum(Tot), Fish=sum(count))
  
  dfCountry <- dplyr::full_join(dfCountryCatch,dfCountryRFID)
  dfCountry <- dplyr::mutate(dfCountry, Prop = Scanned/Total)
  
  dfCountry.Table <- xtable(dfCountry,digits=c(0,0,0,0,0,0,2))
  writeLines(text = print(dfCountry.Table, type="latex"),
             con = file.path(paste0(cry,".tex")))
    
  for (y in seq(2012,2017)){
    
    #monthly totals
    dfCountryCatch <- dfCatchBySR %>%
      filter(Ctry == cry & Year == y & PType == "M") %>%
      select(Ctry,Year,PNum,Catch) %>%
      group_by(Ctry,Year,PNum) %>%
      summarise(Catch = sum(Catch)) %>%
      full_join(data.frame(Ctry=rep(cry,12),Year=rep(y,12),PNum=seq(1,12),stringsAsFactors=FALSE)) %>%
      arrange(PNum) %>%
      mutate(Month = Months[PNum]) %>%
      mutate(Catch = coalesce(Catch,0)) %>%
      select(Ctry,Year,Month,Catch)
    
    dfCountryRFID <- ungroup(dfRFID) %>%
      filter(Ctry == cry & Year == y & PType == "M") %>%
      select(Ctry,Year,PNum,Tot,count) %>%
      group_by(Ctry,Year,PNum) %>%
      summarise(Scanned = sum(Tot), Fish=sum(count)) %>%
      mutate(Month = Months[PNum]) %>%
      select(Ctry,Year,Month,Scanned,Fish)
      
    dfCountry <- dplyr::full_join(dfCountryCatch,dfCountryRFID)
    dfCountry <- dplyr::mutate(dfCountry, Prop = Scanned/Catch)
    
    dfCountry.Table <- xtable(dfCountry, digits=c(0,0,0,0,0,0,0,2))
    writeLines(text = print(dfCountry.Table, type="latex"),
               con = file.path(paste0(cry,"_",y,".tex")))
    
    #reset plot counter
    p <- 0
    
    for (m in 1:12){

      #Scotland
      if (cry=='UKS'){xlim <- c(-16,8);ylim <- c(50,66)}
      if (cry=='DK'){xlim <- c(-16,8);ylim <- c(50,66)}
      if (cry=='IE'){xlim <- c(-16,8);ylim <- c(50,66)}
      if (cry=='FO'){xlim <- c(-16,8);ylim <- c(54,70)}
      if (cry=='NO'){xlim <- c(-12,12);ylim <- c(54,70)}
      if (cry=='IC'){xlim <- c(-26,12);ylim <- c(60,68)}
      
      lon.dist <- geosphere::distGeo(p1=c(min(xlim),mean(ylim)),p2=c(max(xlim),mean(ylim)))
      lat.dist <- geosphere::distGeo(p1=c(mean(xlim),min(ylim)),p2=c(mean(xlim),max(ylim)))
      aspect <- lon.dist/lat.dist
      
      #catch by SR first
      dfSR <- fSubset(y = y, ptype = "M", pnum = m, Cry=cry)
      dfSR <- dplyr::filter(dfSR, Tot>Catch.Limits[1])

      catchmap <- ggplot(map.df) +
        coord_cartesian(xlim = xlim, ylim = ylim) +
        geom_hline(yintercept=seq(40,80,by=0.5), linetype="solid", color = "grey") +
        geom_vline(xintercept=seq(-30,20), linetype="solid", color = "grey") +
        aes(long, lat, group=group) +
        geom_path(color = "white") +
        geom_polygon() +
        theme_map()
      
      if (nrow(dfSR)>0) {
        
        #create data frame with polygons for catch
        dfPolys <- data.frame(SR=c(),Lat=c(),Lon=c(),Catch=c())
        
        for (i in 1:nrow(dfSR)){
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSR$SR[i],Lat=dfSR$Lat[i]-0.25,Lon=dfSR$Lon[i]-0.5,
                                                         Catch=fCatchBin(dfSR$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSR$SR[i],Lat=dfSR$Lat[i]-0.25,Lon=dfSR$Lon[i]+0.5,
                                                         Catch=fCatchBin(dfSR$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSR$SR[i],Lat=dfSR$Lat[i]+0.25,Lon=dfSR$Lon[i]+0.5,
                                                         Catch=fCatchBin(dfSR$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSR$SR[i],Lat=dfSR$Lat[i]+0.25,Lon=dfSR$Lon[i]-0.5,
                                                         Catch=fCatchBin(dfSR$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
        }
        
        dfPolys$Catch <- factor(dfPolys$Catch,levels=Catch.Bins)
        
        catchmap <- catchmap + geom_polygon(data=dfPolys, 
                                            aes(x=Lon,y=Lat,group=SR,fill=Catch)) +
          scale_fill_manual(values = Four.Colours, drop=FALSE)
        
        catchmap <- catchmap + geom_polygon(data=map.df,
                                            aes(long, lat, group=group))
        
      }
      
      catchmap <- catchmap + guides(fill=guide_legend(title=paste(cry,y,Months.Abbrev[m],sep=",")))
        
      #now scanned catch and tag returns
      dfSquares <- dplyr::filter(dfScannedCatch, Ctry==cry & Year==y & PNum==m & Tot>0)

      RFIDmap <- ggplot(map.df) + 
        coord_cartesian(xlim = xlim, ylim = ylim) +
        geom_hline(yintercept=seq(40,80,by=0.5), linetype="solid", color = "grey") +
        geom_vline(xintercept=seq(-30,20), linetype="solid", color = "grey") +
        aes(long, lat, group=group) +
        geom_path(color = "white") + 
        geom_polygon() + 
        theme_map()

      if(nrow(dfSquares)>0) {
        
        dfPolys <- data.frame(SR=c(),Lat=c(),Lon=c(),Scanned=c(),Recap=c())
        
        for (i in 1:nrow(dfSquares)){
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSquares$SR[i],
                                                         Lat=dfSquares$Lat[i]-0.25,
                                                         MidLat=dfSquares$Lat[i],
                                                         Lon=dfSquares$Lon[i]-0.5,
                                                         MidLon=dfSquares$Lon[i],
                                                         Scanned=fCatchBin(dfSquares$Tot[i],Limits=Scanned.Limits,Labels=Scanned.Bins),
                                                         Recap = dfSquares$count[i],
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSquares$SR[i],
                                                         Lat=dfSquares$Lat[i]-0.25,
                                                         MidLat=dfSquares$Lat[i],
                                                         Lon=dfSquares$Lon[i]+0.5,
                                                         MidLon=dfSquares$Lon[i],
                                                         Scanned=fCatchBin(dfSquares$Tot[i],Limits=Scanned.Limits,Labels=Scanned.Bins),
                                                         Recap = dfSquares$count[i],
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSquares$SR[i],
                                                         Lat=dfSquares$Lat[i]+0.25,
                                                         MidLat=dfSquares$Lat[i],
                                                         Lon=dfSquares$Lon[i]+0.5,
                                                         MidLon=dfSquares$Lon[i],
                                                         Scanned=fCatchBin(dfSquares$Tot[i],Limits=Scanned.Limits,Labels=Scanned.Bins),
                                                         Recap = dfSquares$count[i],
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSquares$SR[i],
                                                         Lat=dfSquares$Lat[i]+0.25,
                                                         MidLat=dfSquares$Lat[i],
                                                         Lon=dfSquares$Lon[i]-0.5,
                                                         MidLon=dfSquares$Lon[i],
                                                         Scanned=fCatchBin(dfSquares$Tot[i],Limits=Scanned.Limits,Labels=Scanned.Bins),
                                                         Recap = dfSquares$count[i],
                                                         stringsAsFactors = FALSE))
        }
        
        if(nrow(dfPolys)>0){dfPolys$Scanned <- factor(dfPolys$Scanned,levels=Scanned.Bins)}
        
        RFIDmap <- RFIDmap + 
          geom_polygon(data=dfPolys,aes(x=Lon,y=Lat,group=SR,fill=Scanned)) +
          scale_fill_manual(values = Four.Colours, drop=FALSE) +
          geom_text(data=select(dfPolys,MidLon,MidLat,Recap),inherit.aes=FALSE,aes(x=MidLon,y=MidLat,label=Recap))
          
      }

      #side by side for comparison        
      if(cry=='IC'){
        pall <- ggarrange(catchmap,RFIDmap,nrow=2,ncol=1)  
      } else {
        pall <- ggarrange(catchmap,RFIDmap,nrow=1,ncol=2)
      }
  
      #increment plot counter
      p <- p + 1
        
      assign(paste0('Plot',p),pall)

      ggexport(pall, 
               filename=file.path(".","Plots","WKIBPNEAMac",y,cry,paste0(paste(y,Months.Abbrev[m],cry,sep="_"),".png")),
               width=800,height=500)
      
    } #end month

    #output pdf
    #ggexport(Plot1,Plot2,Plot3,Plot4,Plot5,Plot6,Plot7,Plot8,Plot9,Plot10,Plot11,Plot12,
    #         filename=file.path(".","Plots","WKIBPNEAMac",paste0(cry,"_",y,".pdf")))
    
    #remove plot objects
    rm("Plot1","Plot2","Plot3","Plot4","Plot5","Plot6","Plot7","Plot8","Plot9","Plot10","Plot11","Plot12")
    
  } #end year
  
} #end country






#9th January - WKIBPNEAMac
#Catch by SR by Month by Country

#for (cry in c('IC','NO','DK','FO','UKS','IE')) {
for (cry in c('UKS')) {
  
  for (y in seq(2017,2017)){
    
    for (m in 1:12){
  
      #Iceland
      xlim <- c(-36,12);ylim <- c(50,70)
      #Scotland
      if (cry=='UKS'){
        xlim <- c(-16,8);ylim <- c(50,66)
      }
      
      
      lon.dist <- geosphere::distGeo(p1=c(min(xlim),mean(ylim)),p2=c(max(xlim),mean(ylim)))
      lat.dist <- geosphere::distGeo(p1=c(mean(xlim),min(ylim)),p2=c(mean(xlim),max(ylim)))
      aspect <- lon.dist/lat.dist

      dfSR <- fSubset(y = y, ptype = "M", pnum = m, Cry=cry)

      if (nrow(dfSR)>0) {
        
        jpeg(filename=paste0(".\\Plots\\WKIBPNEAMac\\",y,"\\",cry,"\\",y,"_",Months.Abbrev[m],"_",cry,"_CBySR.jpg"),
             width=1500, height=1500/aspect, quality=100)
        
        #axes labels?
        #blnxlabs <- c(T,T,T,T,T,T,T,T,T,T,T,T)
        #blnylabs <- c(T,T,T,T,T,T,T,T,T,T,T,T)
        blnxlabs <- blnylabs <- rep(F,12)
        
        fPlotBaseMap(xlim=xlim,ylim=ylim,xlabs=blnxlabs[m],ylabs=blnylabs[m],xaxis=FALSE,yaxis=FALSE,ICES=FALSE)
        
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
        
        with(filter(dfSR, Tot<3000 & Tot>=1000),
             for (i in 1:nrow(filter(dfSR, Tot<3000 & Tot>=1000))){
               polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                       c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                       col="firebrick2", border="grey")
             }
        )
        
        with(filter(dfSR, Tot>=3000),
             for (i in 1:nrow(filter(dfSR, Tot>=3000))){
               polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                       c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                       col="firebrick4", border="grey")
             }
        )
        
        fPlotBaseMap(xlim=xlim,ylim=ylim,refresh=TRUE,xlabs=blnxlabs[m],ylabs=blnylabs[m],xaxis=FALSE,yaxis=FALSE,ICES=FALSE)
        
        #lines(Banana$Lon,Banana$Lat)
        #lines(Southern$Lon,Southern$Lat)
        
        #no legend required
        legend(x="bottomright",
               c("<100t","100t to 1000t","1000t to 3000t",">3000t"),
               fill=c("lightpink","lightpink3","firebrick2","firebrick4"),
               border="black",
               cex=3)
        
        dev.off()
      
      }
      
    } #month loop
  } #year loop
} #country loop

#end WKIBPNEAMac


#11th October 2018
#Irish catch by week (for Roisin Pinfield)

for (y in seq(2017,2017)){
  for (w in 1:52){
    
    dfSR <- fSubset(src = paste0(".\\Data\\CatchBySR_ByWeekNo_",y,".csv"), y = y, ptype = "W", pnum = w)

    if (nrow(dfSR)>0){
      
      jpeg(filename=paste0(".\\Plots\\IEByWeek\\IE_",y,"_W",w,".jpg"),
           width=1200, height=1600, quality=100)

      fPlotBaseMap(xlim=c(-18,4),ylim=c(48,64),xlabs=T,ylabs=T)
      
      with(filter(dfSR, Tot<10 & Tot>=1),
           for (i in 1:nrow(filter(dfSR, Tot<10))){
             polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                     c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                     col="lightpink", border="grey")
           }
      )
      
      with(filter(dfSR, Tot<100 & Tot>=10),
           for (i in 1:nrow(filter(dfSR, Tot<100 & Tot>=10))){
             polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                     c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                     col="lightpink3", border="grey")
           }
      )
      
      with(filter(dfSR, Tot<1000 & Tot>=100),
           for (i in 1:nrow(filter(dfSR, Tot<1000 & Tot>=100))){
             polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                     c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                     col="firebrick2", border="grey")
           }
      )
      
      with(filter(dfSR, Tot>=1000),
           for (i in 1:nrow(filter(dfSR, Tot>=1000))){
             polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                     c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                     col="firebrick4", border="grey")
           }
      )
      
      fPlotBaseMap(xlim=c(-18,4),ylim=c(48,64),xlabs=T,ylabs=T,refresh=T)
      
      lines(Banana$Lon,Banana$Lat)
      lines(Southern$Lon,Southern$Lat)
      
      legend(x="bottomright",
             c("<10t","10t to 100t","100t to 1000t",">1000t"),
             fill=c("antiquewhite","lightpink","firebrick2","firebrick4"),
             border="black",
             cex=2,
             title = paste0(y," Week ",w))
      
      dev.off()
      
    } 
    
  }
}



#21st August 2018
#WGWIDE2018 plots
for (y in seq(2017,2017)){
  
  jpeg(filename=paste0(".\\Plots\\WGPlots\\",y,"\\NEAFC_CBySR",y,".jpg"),
       width=1200, height=1600, quality=100)
  
  dfSR <- fSubset(y = y, ptype = "Y", pnum = y)
  
  fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),xlabs=T,ylabs=T)
  
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
  
  fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),refresh=TRUE,xlabs=T,ylabs=T)
  
  lines(Banana$Lon,Banana$Lat)
  lines(Southern$Lon,Southern$Lat)
  
  legend(x="bottomright",
         c("<100t","100t to 1000t","1000t to 10000t",">10000t"),
         fill=c("antiquewhite","lightpink","firebrick2","firebrick4"),
         border="black",
         cex=2,
         title = paste0(y))
  
  dev.off()
  
}

#quarterly WG report maps
for (y in seq(2017,2017)){
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
for (cry in c('IC','NO','DE','DK','BE','ES','IE','PL','FO','BQ','NL','RU','SE','UKE','UKN','UKS','FR','GL','PT')) {
  for (y in seq(2017,2017)){
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
for (y in seq(2017,2017)){
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

############END WGWIDE 2018######################################



##############################Stockbook#######################################################################
#05/11/2018 Updated for 2018 Stockbook
#18/10/2017 Plots for species summary pages in 2017 Stockbook

#Mackerel
#plot of catch distribution for entire year for 1) all nations 2) Ireland only
#no legend, no grid lines, no axis notations


#data year, reporting year
data.y <- 2017; rep.y <- 2018

#jpeg(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\Total",data.y,"_MAC_CBySR.jpg"),width=1200, height=1600, quality=100)
png(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\Total",data.y,"_MAC_CBySR.png"),width=1200, height=1600)

dfSR <- fSubset(y = data.y, ptype = "Y", pnum = data.y)

fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

with(filter(dfSR, Tot<100 & Tot>=1),
     for (i in 1:nrow(filter(dfSR, Tot<100))){
       polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
               c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
               col="lightpink", border="lightpink")
     }
)

with(filter(dfSR, Tot<1000 & Tot>=100),
     for (i in 1:nrow(filter(dfSR, Tot<1000 & Tot>=100))){
       polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
               c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
               col="lightpink3", border="lightpink3")
     }
)

with(filter(dfSR, Tot<10000 & Tot>=1000),
     for (i in 1:nrow(filter(dfSR, Tot<10000 & Tot>=1000))){
       polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
               c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
               col="firebrick2", border="firebrick2")
     }
)

with(filter(dfSR, Tot>=10000),
     for (i in 1:nrow(filter(dfSR, Tot>=10000))){
       polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
               c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
               col="firebrick4", border="firebrick4")
     }
)

fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

dev.off()

#Irish only

#jpeg(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\IE",data.y,"_MAC_CBySR.jpg"),width=1200, height=1600, quality=100)
png(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\IE",data.y,"_MAC_CBySR.png"),width=1200, height=1600)

dfSR <- fSubset(y = data.y, ptype = "Y", pnum = data.y, Cry = 'IE')

fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

with(filter(dfSR, Tot<100 & Tot>=1),
     for (i in 1:nrow(filter(dfSR, Tot<100))){
       polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
               c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
               col="lightpink", border="lightpink")
     }
)

with(filter(dfSR, Tot<1000 & Tot>=100),
     for (i in 1:nrow(filter(dfSR, Tot<1000 & Tot>=100))){
       polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
               c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
               col="lightpink3", border="lightpink3")
     }
)

with(filter(dfSR, Tot<10000 & Tot>=1000),
     for (i in 1:nrow(filter(dfSR, Tot<10000 & Tot>=1000))){
       polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
               c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
               col="firebrick2", border="firebrick2")
     }
)

with(filter(dfSR, Tot>=10000),
     for (i in 1:nrow(filter(dfSR, Tot>=10000))){
       polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
               c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
               col="firebrick4", border="firebrick4")
     }
)

fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

dev.off()


#Monthly

for (m in 1:12){
  
  jpeg(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\",data.y,"_MAC_CBySR_M",m,".jpg"),width=1200, height=1600, quality=100)
  
  dfSR <- fSubset(y = data.y, ptype = "M", pnum = m)
  
  blnxlabs <- c(F,F,F,F,F,F,F,F,F,F,F,F)
  blnylabs <- c(F,F,F,F,F,F,F,F,F,F,F,F)
  
  fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),xaxis=F,xlabs=blnxlabs[m],
               yaxis=F,ylabs=blnylabs[m],SR=F,ICES=F)
  
  with(filter(dfSR, Tot<100 & Tot>=1),
       for (i in 1:nrow(filter(dfSR, Tot<100))){
         polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                 c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                 col="lightpink", border="lightpink")
       }
  )
  
  with(filter(dfSR, Tot<1000 & Tot>=100),
       for (i in 1:nrow(filter(dfSR, Tot<1000 & Tot>=100))){
         polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                 c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                 col="lightpink3", border="lightpink3")
       }
  )
  
  with(filter(dfSR, Tot<10000 & Tot>=1000),
       for (i in 1:nrow(filter(dfSR, Tot<10000 & Tot>=1000))){
         polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                 c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                 col="firebrick2", border="firebrick2")
       }
  )
  
  with(filter(dfSR, Tot>=10000),
       for (i in 1:nrow(filter(dfSR, Tot>=10000))){
         polygon(c(Lon[i]-0.5,Lon[i]+0.5,Lon[i]+0.5,Lon[i]-0.5,Lon[i]-0.5),
                 c(Lat[i]-0.25,Lat[i]-0.25,Lat[i]+0.25,Lat[i]+0.25,Lat[i]-0.25),
                 col="firebrick4", border="firebrick4")
       }
  )
  
  fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),refresh=TRUE,xaxis=F,xlabs=blnxlabs[m],
               yaxis=F,ylabs=blnylabs[m],SR=FALSE,ICES=FALSE)
  
  text(x = -31, y = 76, labels = Months[m], cex = 4)
  
  dev.off()
  
}



#Boarfish

#annual
#for (y in seq(2007,2016)) {
for (y in seq(2007,2017)) {
    
  #jpeg(filename = paste0(".\\Plots\\Stockbooks\\2017\\IE",y,"_BOC_CBySR.jpg"),width=1200, height=1600, quality=100)
  png(filename = paste0(".\\Plots\\Stockbooks\\",2018,"\\IE",data.y,"_BOC_CBySR.png"),width=1200, height=1600)
  
  #basebap
  fPlotBaseMap(xlim=c(-17,5),ylim=c(40,62),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)
  
  #annual data
  dfSub <- fSubset(src = ".\\Data\\boc.27.6-8WGCatchBySR.csv",y = y, ptype = "Y", pnum = y)
  
  range(dfSub$Tot)
  
  fPlotDist(dfSub,min=0,max=10,fill.col="lightpink",border.col="lightpink")
  fPlotDist(dfSub,min=10,max=100,fill.col="lightpink3",border.col="lightpink3")
  fPlotDist(dfSub,min=100,max=1000,fill.col="firebrick2",border.col="firebrick2")
  fPlotDist(dfSub,min=1000,max=1e10,fill.col="firebrick4",border.col="firebrick4")
  
  fPlotBaseMap(xlim=c(-17,5),ylim=c(40,62),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)
  
  legend(x = "bottomright",
         legend = c("<10t","10t to 100t","100t to 1000t",">1000t"),
         fill = c("lightpink","lightpink3","firebrick2","firebrick4"),
         border = "black",
         cex = 4,
         title = paste0("Annual Catch ",y))
  
  dev.off()
}

#mean 2007-2015
#jpeg(filename = paste0(".\\Plots\\Stockbooks\\2017\\IE_2007_2015_BOC_CBySR.jpg"),
#     width=1200, height=1600, quality=100)
#mean 2007-2016
png(filename = paste0(".\\Plots\\Stockbooks\\",2018,"\\IE_2007_2016_BOC_CBySR.png"),width=1200, height=1600)

#basebap
fPlotBaseMap(xlim=c(-17,5),ylim=c(40,62),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
#dfSub <- fSubset(src = ".\\Data\\boc.27.6-8WGCatchBySR.csv",y = seq(2007,2015), ptype = "Y")
dfSub <- fSubset(src = ".\\Data\\boc.27.6-8WGCatchBySR.csv",y = seq(2007,2016), ptype = "Y")

range(dfSub$Tot)
range(dfSub$Avg)

fPlotDist(dfSub,min=0,max=10,fill.col="lightpink",border.col="lightpink",avg=TRUE)
fPlotDist(dfSub,min=10,max=100,fill.col="lightpink3",border.col="lightpink3",avg=TRUE)
fPlotDist(dfSub,min=100,max=1000,fill.col="firebrick2",border.col="firebrick2",avg=TRUE)
fPlotDist(dfSub,min=1000,max=1e10,fill.col="firebrick4",border.col="firebrick4",avg=TRUE)

fPlotBaseMap(xlim=c(-17,5),ylim=c(40,62),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

# legend(x = "bottomright",
#        legend = c("<10t","10t to 100t","100t to 1000t",">1000t"),
#        fill = c("lightpink","lightpink3","firebrick2","firebrick4"),
#        border = "black",
#        cex = 4,
#        title = "Average catch, 2007-2015")
legend(x = "bottomright",
       legend = c("<10t","10t to 100t","100t to 1000t",">1000t"),
       fill = c("lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = "Average catch, 2007-2016")

dev.off()


#horse mackerel

#y <- 2016
#jpeg(filename = paste0(".\\Plots\\Stockbooks\\2017\\WG",y,"_HOM_CBySR.jpg"),width=1200, height=1600, quality=100)

y <- 2017
#jpeg(filename = paste0(".\\Plots\\Stockbooks\\2018\\WG",y,"_HOM_CBySR.jpg"),width=1200, height=1600, quality=100)
png(filename = paste0(".\\Plots\\Stockbooks\\2018\\WG",y,"_HOM_CBySR.png"),width=1200, height=1600)

#basebap
fPlotBaseMap(xlim=c(-15,10),ylim=c(37,66),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
dfSub <- fSubset(src = ".\\Data\\hom.27WGCatchBySR.csv",y = y, ptype = "Y", pnum = y)

range(dfSub$Tot)

fPlotDist(dfSub,min=0,max=1,fill.col="antiquewhite",border.col="antiquewhite")
fPlotDist(dfSub,min=1,max=10,fill.col="lightpink",border.col="lightpink")
fPlotDist(dfSub,min=10,max=100,fill.col="lightpink3",border.col="lightpink3")
fPlotDist(dfSub,min=100,max=1000,fill.col="firebrick2",border.col="firebrick2")
fPlotDist(dfSub,min=1000,max=1e10,fill.col="firebrick4",border.col="firebrick4")

fPlotBaseMap(xlim=c(-15,10),ylim=c(37,66),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

legend(x = "bottomright",
       legend = c("<1t","1t to 10t","10t to 100t","100t to 1000t",">1000t"),
       fill = c("antiquewhite","lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = paste0("Catch ",y))

dev.off()


#Irish catch
#y <- 2016
#jpeg(filename = paste0(".\\Plots\\Stockbooks\\2017\\IE",y,"_HOM_CBySR.jpg"),width=1200, height=1600, quality=100)

y <- 2017

#jpeg(filename = paste0(".\\Plots\\Stockbooks\\2018\\IE",y,"_HOM_CBySR.jpg"), width=1200, height=1600, quality=100)
png(filename = paste0(".\\Plots\\Stockbooks\\2018\\IE",y,"_HOM_CBySR.png"),width=1200, height=1600)

#basebap
fPlotBaseMap(xlim=c(-15,10),ylim=c(37,66),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
dfSub <- fSubset(src = ".\\Data\\hom.27WGCatchBySR.csv",y = y, ptype = "Y", pnum = y, Cry='IE')

range(dfSub$Tot)

fPlotDist(dfSub,min=0,max=1,fill.col="antiquewhite",border.col="antiquewhite")
fPlotDist(dfSub,min=1,max=10,fill.col="lightpink",border.col="lightpink")
fPlotDist(dfSub,min=10,max=100,fill.col="lightpink3",border.col="lightpink3")
fPlotDist(dfSub,min=100,max=1000,fill.col="firebrick2",border.col="firebrick2")
fPlotDist(dfSub,min=1000,max=1e10,fill.col="firebrick4",border.col="firebrick4")

fPlotBaseMap(xlim=c(-15,10),ylim=c(37,66),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

legend(x = "bottomright",
       legend = c("<1t","1t to 10t","10t to 100t","100t to 1000t",">1000t"),
       fill = c("antiquewhite","lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = paste0("Catch ",y))

dev.off()


#Blue Whiting - all fleets
y <- 2017
png(filename = paste0(".\\Plots\\Stockbooks\\2018\\WG",y,"_WHB_CBySR.png"),width=1200, height=1600)

#basebap
fPlotBaseMap(xlim=c(-17,10),ylim=c(37,70),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.csv",y = y, ptype = "Y", pnum = y)

range(dfSub$Tot)

fPlotDist(dfSub,min=0,max=1,fill.col="antiquewhite",border.col="antiquewhite")
fPlotDist(dfSub,min=1,max=100,fill.col="lightpink",border.col="lightpink")
fPlotDist(dfSub,min=100,max=1000,fill.col="lightpink3",border.col="lightpink3")
fPlotDist(dfSub,min=1000,max=15000,fill.col="firebrick2",border.col="firebrick2")
fPlotDist(dfSub,min=15000,max=1e10,fill.col="firebrick4",border.col="firebrick4")

fPlotBaseMap(xlim=c(-17,10),ylim=c(37,70),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

legend(x = "bottomright",
       legend = c("<1t","1t to 100t","100t to 1000t","1000t to 15000t",">15000t"),
       fill = c("antiquewhite","lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = paste0("Catch ",y))

dev.off()


#Blue Whiting - by country
crys <- c("DK","FO","FR","DE","IE","IC","LT","NL","NO","PT","RU","UKS","SE","ES","UKE")
y <- 2017

for (c in crys) {

  png(filename = paste0(".\\Plots\\Stockbooks\\2018\\",c,y,"_WHB_CBySR.png"),width=1200, height=1600)

  #basebap
  fPlotBaseMap(xlim=c(-17,10),ylim=c(37,70),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

  #annual data
  dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.csv",y = y, ptype = "Y", pnum = y, Cry=c)

  range(dfSub$Tot)

  fPlotDist(dfSub,min=0,max=1,fill.col="antiquewhite",border.col="antiquewhite")
  fPlotDist(dfSub,min=1,max=100,fill.col="lightpink",border.col="lightpink")
  fPlotDist(dfSub,min=100,max=1000,fill.col="lightpink3",border.col="lightpink3")
  fPlotDist(dfSub,min=1000,max=15000,fill.col="firebrick2",border.col="firebrick2")
  fPlotDist(dfSub,min=15000,max=1e10,fill.col="firebrick4",border.col="firebrick4")

  fPlotBaseMap(xlim=c(-17,10),ylim=c(37,70),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

  legend(x = "bottomright",
         legend = c("<1t","1t to 100t","100t to 1000t","1000t to 15000t",">15000t"),
         fill = c("antiquewhite","lightpink","lightpink3","firebrick2","firebrick4"),
         border = "black",
         cex = 4,
         title = paste0(c," Catch ",y))

  dev.off()
  
}




#############################################################################################################
#31/08/2017 WGWIDE 2017 

#annual plot
for (y in seq(2016,2016)){
  
  jpeg(filename=paste0(".\\Plots\\WGPlots\\",y,"\\NEAFC_CBySR",y,".jpg"),
       width=1200, height=1600, quality=100)
  
  dfSR <- fSubset(y = y, ptype = "Y", pnum = y)
  
  fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),xlabs=T,ylabs=T)
  
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
  
  fPlotBaseMap(xlim=c(-36,20),ylim=c(36,76),refresh=TRUE,xlabs=T,ylabs=T)
  
  lines(Banana$Lon,Banana$Lat)
  lines(Southern$Lon,Southern$Lat)
  
  legend(x="bottomright",
         c("<100t","100t to 1000t","1000t to 10000t",">10000t"),
         fill=c("antiquewhite","lightpink","firebrick2","firebrick4"),
         border="black",
         cex=2,
         title = paste0(y))
  
  dev.off()
  
}

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
for (cry in c('IC','NO','DE','DK','BE','LT','ES','IE','PL','FO','BQ','NL','RU','SE','UKE','UKN','UKS','FR','GL','PT')) {
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
