#Generate catch by SR maps

#read in raw data, should be in the Data directory and in a file called WGCatchBySR.csv
#this file is generated from the exchange sheets by script ExtractCatchBySR.R

#Change Log
#17/01/2022 - WHB data for Hans
#06/10/2021 - Boarfish maps for Stockbook
#27/09/2021 - BW. MAC, HOM Stockbook maps
#01/06/2021 - maps for RCG ISSG (Freezer sampling) presentation
#28/10/2020 - Mackerel distribution workshop, looking at IE in particular
#17/10/2020 - Coastal States - BW catch dist
#01/10/2020 - HOM Stockbook plots
#29/09/2020 - MAC stockbook maps
#28/09/2020 - BW stockbook maps
#05/11/2019 - mac maps for Stockbook
#10/09/2019 - BW maps (update of BIM report)
#15/01/2019 - WKIBPNEAMac ggplot maps
#08/01/2019 - WKIBPNEAMac maps
#21/08/2018 - WGWIDE 2018 maps
#11/10/2018 - Irish mackerel catch by week (for Roisin Pinfield)
#05/11/2018 - 2018 Stockbook maps
#20/11/2018 - BW maps by country (for BIM report)


rm(list=ls())
gc()
try(dev.off(),silent=TRUE)

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
library(tools)

gpclibPermit()

load(".//..//Data//RData//coast.rda")
load(".//..//Data//RData//NEAFC.rda")
load(".//..//Data//RData//SR.rda")
dfSR$SR <- levels(dfSR$Rect)[dfSR$Rect]     #SR as a character
load(".//..//Data//RData//EEZ.rda") #eez boundaries

source("SRAnalyFuncs.R")

Months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
Months.Abbrev <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
Four.Colours <- c("lightpink", "lightpink3", "firebrick2","firebrick4")
Catch.Bins <- c("1t-100t","100t-1000t","1000t-3000t",">3000t")
Scanned.Bins <- c("1t-100t","100t-1000t","1000t-3000t",">3000t")
Scanned.Bins <- c("1t-100t","100t-1000t","1000t-3000t",">3000t")
Catch.Limits <- c(1,100,1000,3000)
Scanned.Limits <- c(1,100,1000,3000)
Seven.Colours <- rev(heat.colors(7))

#map details
eco <- subset(read.csv(file=file.path(".","..","Data","RData","iarc.eco.2008.csv"), header = TRUE, sep=","), iso2 != "EU27")
map <- readShapeSpatial(fn=file.path(".","..","Data","maps","countries//CNTR_RG_60M_2006"), proj4string = CRS("+proj=longlat"))
map <- reshape::rename(map, c(CNTR_ID = "iso2")) #; summary(map)

map@data$id <- rownames(map@data)
map.points <- fortify(map, region = "id")
map.df <- dplyr::inner_join(map.points, map@data, by = "id")
map.df <- dplyr::inner_join(map.df, eco, by = "iso2")

#17/01/2022
#Hans request - rectangle WHB data for 2016-2020 for non-eu countries
#data retrieved from WGWIDE Sharepoint 17/01/2022 (WHB catchesbyrect WHB in folder 06.Data/_catch_by_rectangle)
#saved to csv
dfWHB <- read.table(file = file.path(getwd(),"Data","whb.27.1-91214WGCatchBySR.wgwide2021.csv"), 
                    header = TRUE, sep = ",", stringsAsFactors = FALSE, strip.white = TRUE)

#only interested in 2016-2020
dfWHB <- dfWHB %>% filter(year>=2016 & year<=2020)
#quick look at total catches
dfWHB %>% group_by(year) %>% summarise(Tot=sum(catch))

#assign bloc EU/Non-EU/UK
dfWHB$bloc <- NA
dfWHB$bloc[dfWHB$country %in% c("GBR","GBR.EW","GBR.N","GBR.S")] <- "UK"
dfWHB$bloc[dfWHB$country %in% c("DEU","DNK","ESP","FRA","GER","IRL","LTU","NLD","POL","PRT","SWE")] <- "EU"
dfWHB$bloc[dfWHB$country %in% c("FRO","GRL","ISL","NOR","RUS")] <- "Non-EU"

dfWHB <- dfWHB %>% group_by(year,rect,bloc) %>% summarise(catch=sum(catch))

dfWHB %>% group_by(year) %>% summarise(Tot=sum(catch))

write.table(dfWHB, file=file.path(getwd(),"Data","whb_2016_2020.csv"), 
            quote=FALSE, row.names=FALSE, sep=",")

#06/10/2021 - Boarfish
y <- 2020
png(filename = paste0(".\\Plots\\Stockbooks\\",2021,"\\IE",y,"_BOC_CBySR.png"),width=1200, height=1600)

fPlotBaseMap(xlim=c(-19,7),ylim=c(45,63),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)
dfSub <- fSubset(src = ".\\Data\\boc.27.6-8_CBySR.csv",y = y, ptype = "Y", pnum = y)

range(dfSub$Tot)

fPlotDist(dfSub,min=0,max=10,fill.col="lightpink",border.col="lightpink")
fPlotDist(dfSub,min=10,max=100,fill.col="lightpink3",border.col="lightpink3")
fPlotDist(dfSub,min=100,max=1000,fill.col="firebrick2",border.col="firebrick2")
fPlotDist(dfSub,min=1000,max=1e10,fill.col="firebrick4",border.col="firebrick4")

fPlotBaseMap(xlim=c(-19,7),ylim=c(45,63),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

legend(x = "bottomright",
       legend = c("<10t","10t to 100t","100t to 1000t",">1000t"),
       fill = c("lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = "Boarfish Catch")
dev.off()

#mean 2018-2020

#06/10/2021
png(filename = paste0(".\\Plots\\Stockbooks\\",2021,"\\IE","_3yr","_BOC_CBySR.png"),width=1200, height=1600)

fPlotBaseMap(xlim=c(-19,7),ylim=c(45,63),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)
dfSub <- fSubset(src = ".\\Data\\boc.27.6-8_CBySR.csv",y = seq(2018,2020), ptype = "Y")

fPlotDist(dfSub,min=0,max=10,fill.col="lightpink",border.col="lightpink",avg=TRUE)
fPlotDist(dfSub,min=10,max=100,fill.col="lightpink3",border.col="lightpink3",avg=TRUE)
fPlotDist(dfSub,min=100,max=1000,fill.col="firebrick2",border.col="firebrick2",avg=TRUE)
fPlotDist(dfSub,min=1000,max=1e10,fill.col="firebrick4",border.col="firebrick4",avg=TRUE)

fPlotBaseMap(xlim=c(-19,7),ylim=c(45,63),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

legend(x = "bottomright",
       legend = c("<10t","10t to 100t","100t to 1000t",">1000t"),
       fill = c("lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = "Boarfish Catch")
dev.off()


#27/09/2021
#Blue Whiting Stockbook plots
#whb/WGWIDE2020_CatchesByRect_WHB.xlsx retreived from WGWIDE2020 Sharepoint. Saved into csv format from Excel

#all fleets
y <- 2020 #data year
png(filename = paste0(".\\Plots\\Stockbooks\\2021\\WG",y,"_WHB_CBySR.png"),width=1200, height=1600)

#baseMap
fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.wgwide2021.csv",y = y, ptype = "Y", pnum = y)

range(dfSub$TOT)

fPlotDist(dfSub,min=0,max=1,fill.col="antiquewhite",border.col="antiquewhite")
fPlotDist(dfSub,min=1,max=100,fill.col="lightpink",border.col="lightpink")
fPlotDist(dfSub,min=100,max=1000,fill.col="lightpink3",border.col="lightpink3")
fPlotDist(dfSub,min=1000,max=15000,fill.col="firebrick2",border.col="firebrick2")
fPlotDist(dfSub,min=15000,max=1e10,fill.col="firebrick4",border.col="firebrick4")

fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

legend(x = "bottomright",
       legend = c("<1t","1t to 100t","100t to 1000t","1000t to 15000t",">15000t"),
       fill = c("antiquewhite","lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = paste0("Catch ",y))

dev.off()


#Blue Whiting - by country
#crys <- c("DK","FO","FR","DE","IE","IC","LT","NL","NO","PT","RU","UKS","SE","ES","UKE")
crys <- c("DEU","DNK","ESP","FRA","FRO","GBR.EW","GBR.N","GBR.S","GRL","IRL","ISL","NLD","NOR","POL","PRT","RUS","SWE")

for (c in crys) {
  
  png(filename = paste0(".\\Plots\\Stockbooks\\2021\\",c,y,"_WHB_CBySR_IE_EEZ.png"),width=1200, height=1600)
  
  #basebap
  fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)
  
  #annual data
  dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.wgwide2021.csv",y = y, ptype = "Y", pnum = y, Cry=c)
  
  range(dfSub$TOT)
  
  fPlotDist(dfSub,min=0,max=1,fill.col="antiquewhite",border.col="antiquewhite")
  fPlotDist(dfSub,min=1,max=100,fill.col="lightpink",border.col="lightpink")
  fPlotDist(dfSub,min=100,max=1000,fill.col="lightpink3",border.col="lightpink3")
  fPlotDist(dfSub,min=1000,max=15000,fill.col="firebrick2",border.col="firebrick2")
  fPlotDist(dfSub,min=15000,max=1e10,fill.col="firebrick4",border.col="firebrick4")
  
  fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

  lines(x=dat.eez.irl$long[dat.eez.irl$hole==FALSE],y=dat.eez.irl$lat[dat.eez.irl$hole==FALSE],col="black")
  
  legend(x = "bottomright",
         legend = c("<1t","1t to 100t","100t to 1000t","1000t to 15000t",">15000t"),
         fill = c("antiquewhite","lightpink","lightpink3","firebrick2","firebrick4"),
         border = "black",
         cex = 4,
         title = paste0(c," Catch ",y))
  
  dev.off()
  
}


#Mackerel Stockbook plots
#plot of catch distribution for entire year for 1) all nations 2) Ireland only
#no legend, no grid lines, no axis notations

#data year, reporting year
data.y <- 2020; rep.y <- 2021

#jpeg(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\Total",data.y,"_MAC_CBySR.jpg"),width=1200, height=1600, quality=100)
png(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\Total",data.y,"_MAC_CBySR.png"),width=1200, height=1600)

dfSR <- fSubset(y = data.y, ptype = "Y", pnum = data.y)

fPlotBaseMap(xlim=c(-32,16),ylim=c(36,72),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

with(filter(dfSR, TOT<100 & TOT>=1),
     for (i in 1:nrow(filter(dfSR, TOT<100))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="lightpink", border="lightpink")
     }
)

with(filter(dfSR, TOT<1000 & TOT>=100),
     for (i in 1:nrow(filter(dfSR, TOT<1000 & TOT>=100))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="lightpink3", border="lightpink3")
     }
)

with(filter(dfSR, TOT<10000 & TOT>=1000),
     for (i in 1:nrow(filter(dfSR, TOT<10000 & TOT>=1000))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="firebrick2", border="firebrick2")
     }
)

with(filter(dfSR, TOT>=10000),
     for (i in 1:nrow(filter(dfSR, TOT>=10000))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="firebrick4", border="firebrick4")
     }
)

fPlotBaseMap(xlim=c(-32,16),ylim=c(36,72),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)


legend(x = "bottomright",
       legend = c("<100t","100t to 1000t","1000t to 10000t",">15000t"),
       fill = c("lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = paste0("Catch ",data.y))

dev.off()

#Irish only

#jpeg(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\IE",data.y,"_MAC_CBySR.jpg"),width=1200, height=1600, quality=100)
png(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\IE",data.y,"_MAC_CBySR.png"),width=1200, height=1600)

dfSR <- fSubset(y = data.y, ptype = "Y", pnum = data.y, Cry = 'IE')

fPlotBaseMap(xlim=c(-32,16),ylim=c(36,72),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

with(filter(dfSR, TOT<100 & TOT>=1),
     for (i in 1:nrow(filter(dfSR, TOT<100))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="lightpink", border="lightpink")
     }
)

with(filter(dfSR, TOT<1000 & TOT>=100),
     for (i in 1:nrow(filter(dfSR, TOT<1000 & TOT>=100))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="lightpink3", border="lightpink3")
     }
)

with(filter(dfSR, TOT<10000 & TOT>=1000),
     for (i in 1:nrow(filter(dfSR, TOT<10000 & TOT>=1000))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="firebrick2", border="firebrick2")
     }
)

with(filter(dfSR, TOT>=10000),
     for (i in 1:nrow(filter(dfSR, TOT>=10000))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="firebrick4", border="firebrick4")
     }
)

fPlotBaseMap(xlim=c(-32,16),ylim=c(36,72),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

legend(x = "bottomright",
       legend = c("<100t","100t to 1000t","1000t to 10000t",">15000t"),
       fill = c("lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = paste0("IRL Catch ",data.y))

dev.off()


#Horse Mackerel
data.y <- 2020
rep.y <- 2021

png(filename = file.path(getwd(),"Plots","Stockbooks",rep.y,paste0("WG",data.y,"_HOM_CBySR.png")),width=1200, height=1600)

#basemap
fPlotBaseMap(xlim=c(-15,10),ylim=c(37,66),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
dfSub <- fSubset(src = ".\\Data\\hom.27WGCatchBySR.csv",y = data.y, ptype = "Y", pnum = data.y)

dfSub <- dfSub[!is.na(dfSub$LAT),]
dfSub <- dfSub[!is.na(dfSub$LON),]
dfSub <- dfSub[dfSub$TOT>0,]

range(dfSub$TOT)

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
       title = paste0("Catch ",data.y))

dev.off()


#Irish catch
png(filename = file.path(getwd(),"Plots","Stockbooks",rep.y,paste0("IE",data.y,"_HOM_CBySR.png")),width=1200, height=1600)

#basebap
fPlotBaseMap(xlim=c(-15,10),ylim=c(37,66),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
dfSub <- fSubset(src = ".\\Data\\hom.27WGCatchBySR.csv",y = data.y, ptype = "Y", pnum = data.y, Cry='IRL')

range(dfSub$TOT)

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
       title = paste0("IRL Catch ",data.y))

dev.off()



#end 27/09/2021




#01/06/2021
#Freezer catches (NL,DE,UKE) of mackerel/bw/hom for RCG ISSG
#uncomment below as required for annual/monthly/species of interest
for (y in seq(2014,2018)) {
  
  data.y <- y
  rep.y <- 2021
  
  #png(filename = file.path(getwd(),"Plots","RCG",paste0("Freezers",data.y,"_Mac_CBySR.png")),width=1200, height=1600)
  #png(filename = file.path(getwd(),"Plots","RCG",paste0("Freezers",data.y,"_Mac_CBySR_Q4.png")),width=1200, height=1600)

  #png(filename = file.path(getwd(),"Plots","RCG",paste0("Freezers",data.y,"_BW_CBySR.png")),width=1200, height=1600)
  #png(filename = file.path(getwd(),"Plots","RCG",paste0("Freezers",data.y,"_BW_CBySR_Q4.png")),width=1200, height=1600)
  
  #png(filename = file.path(getwd(),"Plots","RCG",paste0("Freezers",data.y,"_HOM_CBySR.png")),width=1200, height=1600)
  #png(filename = file.path(getwd(),"Plots","RCG",paste0("Freezers",data.y,"_HOM_CBySR_Q4.png")),width=1200, height=1600)
  
  #herring file - no data on north sea?
  #png(filename = file.path(getwd(),"Plots","RCG",paste0("Freezers",data.y,"_HER_CBySR.png")),width=1200, height=1600)
  #png(filename = file.path(getwd(),"Plots","RCG",paste0("Freezers",data.y,"_HOM_CBySR_Q4.png")),width=1200, height=1600)
  
    
  #basemap
  fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=T)
  
  #dfSub <- fSubset(src = ".\\Data\\mac.27.neaWGCatchBySR.wgwide2020.csv",y = data.y, ptype = "Y", pnum = data.y, Cry = c("DEU","NLD","GBR.E"))
  #dfSub <- fSubset(src = ".\\Data\\mac.27.neaWGCatchBySR.wgwide2020.csv",y = data.y, ptype = "Q", pnum = 4, Cry = c("DEU","NLD","GBR.E"))

  #dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.wgwide2020.csv",y = data.y, ptype = "Y", pnum = data.y, Cry = c("DEU","NLD","GBR.E"))
  #dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.wgwide2020.csv",y = data.y, ptype = "Q", pnum = 4, Cry = c("DEU","NLD","GBR.E"))
  
  #dfSub <- fSubset(src = ".\\Data\\hom.WGCatchBySR.wgwide.2020.csv",y = data.y, ptype = "Y", pnum = data.y, Cry = c("DEU","NLD","GBR.E"))
  #dfSub <- fSubset(src = ".\\Data\\hom.WGCatchBySR.wgwide.2020.csv",y = data.y, ptype = "Q", pnum = 4, Cry = c("DEU","NLD","GBR.E"))

  #dfSub <- fSubset(src = ".\\Data\\her.WGCatchBySR.wgwide.2020.csv",y = data.y, ptype = "Y", pnum = data.y, Cry = c("DEU","NLD","GBR.E"))
  #dfSub <- fSubset(src = ".\\Data\\hom.WGCatchBySR.wgwide.2020.csv",y = data.y, ptype = "Q", pnum = 4, Cry = c("DEU","NLD","GBR.E"))
  
  range(dfSub$TOT)
  
  fPlotDist(dfSub,min=0,max=10,fill.col="antiquewhite",border.col="antiquewhite")
  fPlotDist(dfSub,min=10,max=100,fill.col="lightpink",border.col="lightpink")
  fPlotDist(dfSub,min=100,max=1000,fill.col="lightpink3",border.col="lightpink3")
  fPlotDist(dfSub,min=1000,max=10000,fill.col="firebrick2",border.col="firebrick2")
  fPlotDist(dfSub,min=10000,max=1e10,fill.col="firebrick4",border.col="firebrick4")
  
  #lines(x=dat.eez.irl$long[dat.eez.irl$hole==FALSE],y=dat.eez.irl$lat[dat.eez.irl$hole==FALSE],col="black")
  #lines(x=dat.eez.UK$long[dat.eez.UK$hole==FALSE],y=dat.eez.UK$lat[dat.eez.UK$hole==FALSE],col="black")
  #lines(x=dat.eez.FR$long[dat.eez.FR$hole==FALSE],y=dat.eez.FR$lat[dat.eez.FR$hole==FALSE],col="black")
  #lines(x=dat.eez.FO$long[dat.eez.FO$hole==FALSE],y=dat.eez.FO$lat[dat.eez.FO$hole==FALSE],col="black")
  #lines(x=dat.eez.IC$long[dat.eez.IC$hole==FALSE],y=dat.eez.IC$lat[dat.eez.IC$hole==FALSE],col="black")
  #lines(x=dat.eez.GL$long[dat.eez.GL$hole==FALSE],y=dat.eez.GL$lat[dat.eez.GL$hole==FALSE],col="black")
  #lines(x=dat.eez.NO$long[dat.eez.NO$hole==FALSE],y=dat.eez.NO$lat[dat.eez.NO$hole==FALSE],col="black")
  
  fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=T)
  
  legend(x = "bottomright",
         legend = c("<10t","10t to 100t","100t to 1000t","1000t to 10000t",">10000t"),
         fill = c("antiquewhite","lightpink","lightpink3","firebrick2","firebrick4"),
         border = "black",
         cex = 4,
         title = paste0("Catch ",data.y))
  
  dev.off()
}



#EEZ - slow code, objects saved to RData file
# path.eez.world.v11 <- file.path(".","World_EEZ_v11_20191118")
# fnam.eez.world.v11 <- "eez_v11.shp"
# 
# eez.world.v11 <- rgdal::readOGR(dsn = path.eez.world.v11, 
#                         layer = file_path_sans_ext(fnam.eez.world.v11))
# 
# dat.eez.irl <- eez.world.v11[eez.world.v11@data$TERRITORY1 == "Ireland", ]
# dat.eez.UK <- eez.world.v11[eez.world.v11@data$TERRITORY1 == "United Kingdom", ]
# dat.eez.FR <- eez.world.v11[eez.world.v11@data$TERRITORY1 == "France", ]
# dat.eez.FO <- eez.world.v11[eez.world.v11@data$TERRITORY1 == "Faeroe", ]
# dat.eez.IC <- eez.world.v11[eez.world.v11@data$TERRITORY1 == "Iceland", ]
# dat.eez.NO <- eez.world.v11[eez.world.v11@data$TERRITORY1 == "Norway", ]
# dat.eez.GL <- eez.world.v11[eez.world.v11@data$TERRITORY1 == "Greenland", ]
# 
# fortify.shape <- function(x){
#   x@data$id <- rownames(x@data)
#   x.f <- ggplot2::fortify(x, region = "id")
#   x.join <- dplyr::inner_join(x.f, x@data, by = "id")
# }
# 
# dat.eez.irl <- fortify.shape(dat.eez.irl) 
# dat.eez.UK <- fortify.shape(dat.eez.UK) 
# dat.eez.FR <- fortify.shape(dat.eez.FR) 
# dat.eez.FO <- fortify.shape(dat.eez.FO) 
# dat.eez.IC <- fortify.shape(dat.eez.IC) 
# dat.eez.GL <- fortify.shape(dat.eez.GL) 
# dat.eez.NO <- fortify.shape(dat.eez.NO) 
# 
# #save eez objects
# save(list=c("eez.world.v11","dat.eez.irl","dat.eez.UK","dat.eez.FR","dat.eez.FO","dat.eez.IC","dat.eez.GL","dat.eez.NO"),
#      file = ".//..//Data//RData//EEZ.rda")
load(".//..//Data//RData//EEZ.rda")

#samples by SR - an output from the 4SforPelagics RProject
load("C://Analysis//4SforPelagics//RData//MacSamplesBySR.RData")

#IMR Mackerel workshop collaboration follow up

#for (y in seq(2000,2019)) {
for (y in seq(2011,2019)) {
    
  data.y <- y
  
  #Irish catch
  #png(filename = file.path(getwd(),"Plots","IMR Mac Workshop",paste0("IE",data.y,"_MAC_CBySR.png")),width=1200, height=1600)
  png(filename = file.path(getwd(),"Plots","IMR Mac Workshop",paste0("IE_",data.y-1,"-",data.y,"_MAC_CBySR.png")),width=1200, height=1600)
  
  #basemap
  fPlotBaseMap(xlim=c(-15,6),ylim=c(47,64),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=T,SRNames=T,ICES=T)
  
  #annual data
  #dfSub <- fSubset(src = ".\\Data\\mac.27.neaWGCatchBySR.wgwide2020.csv",y = data.y, ptype = "Y", pnum = data.y, Cry = "IRL")
  #really, we want to look at the winter period i.e. Q3/4 for y-1 and Q1/2 for y
  dfSub <- dplyr::bind_rows(
    dplyr::left_join(
      fSubset(src = ".\\Data\\mac.27.neaWGCatchBySR.wgwide2020.csv",y = data.y-1, ptype = "Q", pnum = c(3,4), Cry = "IRL"),
      dfSamplesBySR %>%
        filter(SampleYear==data.y-1 & SampleQuarter %in% c(3,4)) %>%
        group_by(SampleYear,SR) %>%
        summarise(Samples=n()),
      by=c("SR"="SR","YEAR"="SampleYear")) %>%
      tidyr::replace_na(list(Samples=0)),
    dplyr::left_join(
      fSubset(src = ".\\Data\\mac.27.neaWGCatchBySR.wgwide2020.csv",y = data.y, ptype = "Q", pnum = c(1,2), Cry = "IRL"),
      dfSamplesBySR %>%
        filter(SampleYear==data.y & SampleQuarter %in% c(1,2)) %>%
        group_by(SampleYear,SR) %>%
        summarise(Samples=n()),
      by=c("SR"="SR","YEAR"="SampleYear")) %>%
      tidyr::replace_na(list(Samples=0))
  ) %>%
    group_by(SR,LAT,LON) %>%
    summarise(TOT=sum(TOT),Samples=sum(Samples))
               
  
  #append information on number of samples
  # dfSub <- dplyr::left_join(dfSub,
  #                           dfSamplesBySR %>%
  #                             filter(SampleYear==data.y-1 & SampleQuarter %in% c(3,4)) %>%
  #                             group_by(SampleYear,SR) %>%
  #                             summarise(Samples=n()),
  #                           by=c("SR"="SR","YEAR"="SampleYear")) %>%
  #   tidyr::replace_na(list(Samples=0))
  
  range(dfSub$TOT)
  
  fPlotDist(dfSub,min=0,max=1,fill.col="antiquewhite",border.col="antiquewhite")
  fPlotDist(dfSub,min=1,max=10,fill.col="lightpink",border.col="lightpink")
  fPlotDist(dfSub,min=10,max=100,fill.col="lightpink3",border.col="lightpink3")
  fPlotDist(dfSub,min=100,max=1000,fill.col="firebrick2",border.col="firebrick2")
  fPlotDist(dfSub,min=1000,max=1e10,fill.col="firebrick4",border.col="firebrick4")
  
  fPlotBaseMap(xlim=c(-15,6),ylim=c(47,64),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=T,SRNames=T,ICES=T)
  
  #SR names for rectangles with catch
  for (r in dfSub$SR){
    if ((sum(dfSR$Rect==r)) == 1){
    text(x=dfSR$MidLon[dfSR$Rect==r],
         y=dfSR$MidLat[dfSR$Rect==r],
         labels=paste0(r,"\n",ceiling(dfSub$TOT[dfSub$SR==r]),"\n",
                       ifelse(dfSub$Samples[dfSub$SR==r]>0,dfSub$Samples[dfSub$SR==r],"")),
                       adj=c(0.5,0.5),cex=0.75)}
  }
  
  text(-12,64,paste0(data.y-1,"-",data.y),cex=4)
  
  dev.off()
}






#17/10/2020 Coastal States Blue Whiting Dist investigations
for (y in seq(2016,2019)) {
  data.y <- y
  rep.y <- 2020
  
  png(filename = file.path(getwd(),"Plots","Coastal States",rep.y,paste0("WG",data.y,"_BW_CBySR.png")),width=1200, height=1600)
  
  #basemap
  fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)
  
  #annual data
  dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.wgwide2020.csv",y = data.y, ptype = "Y", pnum = data.y)
  
  range(dfSub$TOT)
  
  fPlotDist(dfSub,min=0,max=10,fill.col="antiquewhite",border.col="antiquewhite")
  fPlotDist(dfSub,min=10,max=100,fill.col="lightpink",border.col="lightpink")
  fPlotDist(dfSub,min=100,max=1000,fill.col="lightpink3",border.col="lightpink3")
  fPlotDist(dfSub,min=1000,max=10000,fill.col="firebrick2",border.col="firebrick2")
  fPlotDist(dfSub,min=10000,max=1e10,fill.col="firebrick4",border.col="firebrick4")

  lines(x=dat.eez.irl$long[dat.eez.irl$hole==FALSE],y=dat.eez.irl$lat[dat.eez.irl$hole==FALSE],col="black")
  lines(x=dat.eez.UK$long[dat.eez.UK$hole==FALSE],y=dat.eez.UK$lat[dat.eez.UK$hole==FALSE],col="black")
  lines(x=dat.eez.FR$long[dat.eez.FR$hole==FALSE],y=dat.eez.FR$lat[dat.eez.FR$hole==FALSE],col="black")
  lines(x=dat.eez.FO$long[dat.eez.FO$hole==FALSE],y=dat.eez.FO$lat[dat.eez.FO$hole==FALSE],col="black")
  lines(x=dat.eez.IC$long[dat.eez.IC$hole==FALSE],y=dat.eez.IC$lat[dat.eez.IC$hole==FALSE],col="black")
  lines(x=dat.eez.GL$long[dat.eez.GL$hole==FALSE],y=dat.eez.GL$lat[dat.eez.GL$hole==FALSE],col="black")
  lines(x=dat.eez.NO$long[dat.eez.NO$hole==FALSE],y=dat.eez.NO$lat[dat.eez.NO$hole==FALSE],col="black")
  
  fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)
  
  legend(x = "bottomright",
         legend = c("<10t","10t to 100t","100t to 1000t","1000t to 10000t",">10000t"),
         fill = c("antiquewhite","lightpink","lightpink3","firebrick2","firebrick4"),
         border = "black",
         cex = 4,
         title = paste0("Catch ",data.y))
  
  dev.off()
}



#01/10/2020 HOM Stockbook plots
#horse mackerel
data.y <- 2019
rep.y <- 2020

png(filename = file.path(getwd(),"Plots","Stockbooks",rep.y,paste0("WG",data.y,"_HOM_CBySR.png")),width=1200, height=1600)

#basemap
fPlotBaseMap(xlim=c(-15,10),ylim=c(37,66),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
dfSub <- fSubset(src = ".\\Data\\hom.27WGCatchBySR.csv",y = data.y, ptype = "Y", pnum = data.y)

range(dfSub$TOT)

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
       title = paste0("Catch ",data.y))

dev.off()


#Irish catch
png(filename = file.path(getwd(),"Plots","Stockbooks",rep.y,paste0("IE",data.y,"_HOM_CBySR.png")),width=1200, height=1600)

#basebap
fPlotBaseMap(xlim=c(-15,10),ylim=c(37,66),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
dfSub <- fSubset(src = ".\\Data\\hom.27WGCatchBySR.csv",y = data.y, ptype = "Y", pnum = data.y, Cry='IRL')

range(dfSub$TOT)

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
       title = paste0("IRL Catch ",data.y))

dev.off()

#end 1/10/2020

#29/09/2020 Mackerel Stockbook plots
#plot of catch distribution for entire year for 1) all nations 2) Ireland only
#no legend, no grid lines, no axis notations

#data year, reporting year
data.y <- 2019; rep.y <- 2020

#jpeg(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\Total",data.y,"_MAC_CBySR.jpg"),width=1200, height=1600, quality=100)
png(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\Total",data.y,"_MAC_CBySR.png"),width=1200, height=1600)

dfSR <- fSubset(y = data.y, ptype = "Y", pnum = data.y)

fPlotBaseMap(xlim=c(-32,16),ylim=c(36,72),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

with(filter(dfSR, TOT<100 & TOT>=1),
     for (i in 1:nrow(filter(dfSR, TOT<100))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="lightpink", border="lightpink")
     }
)

with(filter(dfSR, TOT<1000 & TOT>=100),
     for (i in 1:nrow(filter(dfSR, TOT<1000 & TOT>=100))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="lightpink3", border="lightpink3")
     }
)

with(filter(dfSR, TOT<10000 & TOT>=1000),
     for (i in 1:nrow(filter(dfSR, TOT<10000 & TOT>=1000))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="firebrick2", border="firebrick2")
     }
)

with(filter(dfSR, TOT>=10000),
     for (i in 1:nrow(filter(dfSR, TOT>=10000))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="firebrick4", border="firebrick4")
     }
)

fPlotBaseMap(xlim=c(-32,16),ylim=c(36,72),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)


legend(x = "bottomright",
       legend = c("<100t","100t to 1000t","1000t to 10000t",">15000t"),
       fill = c("lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = paste0("Catch ",data.y))

dev.off()

#Irish only

#jpeg(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\IE",data.y,"_MAC_CBySR.jpg"),width=1200, height=1600, quality=100)
png(filename = paste0(".\\Plots\\Stockbooks\\",rep.y,"\\IE",data.y,"_MAC_CBySR.png"),width=1200, height=1600)

dfSR <- fSubset(y = data.y, ptype = "Y", pnum = data.y, Cry = 'IE')

fPlotBaseMap(xlim=c(-32,16),ylim=c(36,72),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

with(filter(dfSR, TOT<100 & TOT>=1),
     for (i in 1:nrow(filter(dfSR, TOT<100))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="lightpink", border="lightpink")
     }
)

with(filter(dfSR, TOT<1000 & TOT>=100),
     for (i in 1:nrow(filter(dfSR, TOT<1000 & TOT>=100))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="lightpink3", border="lightpink3")
     }
)

with(filter(dfSR, TOT<10000 & TOT>=1000),
     for (i in 1:nrow(filter(dfSR, TOT<10000 & TOT>=1000))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="firebrick2", border="firebrick2")
     }
)

with(filter(dfSR, TOT>=10000),
     for (i in 1:nrow(filter(dfSR, TOT>=10000))){
       polygon(c(LON[i]-0.5,LON[i]+0.5,LON[i]+0.5,LON[i]-0.5,LON[i]-0.5),
               c(LAT[i]-0.25,LAT[i]-0.25,LAT[i]+0.25,LAT[i]+0.25,LAT[i]-0.25),
               col="firebrick4", border="firebrick4")
     }
)

fPlotBaseMap(xlim=c(-32,16),ylim=c(36,72),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

legend(x = "bottomright",
       legend = c("<100t","100t to 1000t","1000t to 10000t",">15000t"),
       fill = c("lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = paste0("IRL Catch ",data.y))

dev.off()

#end 29/09/2020

#28/09/2020
#Blue Whiting Stockbook plots
#whb/WGWIDE2020_CatchesByRect_WHB.xlsx retreived from WGWIDE2020 Sharepoint. Saved into csv format from Excel

#all fleets
y <- 2019 #data year
png(filename = paste0(".\\Plots\\Stockbooks\\2020\\WG",y,"_WHB_CBySR.png"),width=1200, height=1600)

#basebap
fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.wgwide2020.csv",y = y, ptype = "Y", pnum = y)

range(dfSub$TOT)

fPlotDist(dfSub,min=0,max=1,fill.col="antiquewhite",border.col="antiquewhite")
fPlotDist(dfSub,min=1,max=100,fill.col="lightpink",border.col="lightpink")
fPlotDist(dfSub,min=100,max=1000,fill.col="lightpink3",border.col="lightpink3")
fPlotDist(dfSub,min=1000,max=15000,fill.col="firebrick2",border.col="firebrick2")
fPlotDist(dfSub,min=15000,max=1e10,fill.col="firebrick4",border.col="firebrick4")

fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

legend(x = "bottomright",
       legend = c("<1t","1t to 100t","100t to 1000t","1000t to 15000t",">15000t"),
       fill = c("antiquewhite","lightpink","lightpink3","firebrick2","firebrick4"),
       border = "black",
       cex = 4,
       title = paste0("Catch ",y))

dev.off()


#Blue Whiting - by country
#crys <- c("DK","FO","FR","DE","IE","IC","LT","NL","NO","PT","RU","UKS","SE","ES","UKE")
crys <- c("DEU","DNK","ESP","FRA","FRO","GBR.EW","GBR.N","GBR.S","GRL","IRL","ISL","NLD","NOR","POL","PRT","RUS","SWE")

for (c in crys) {
  
  png(filename = paste0(".\\Plots\\Stockbooks\\2020\\",c,y,"_WHB_CBySR.png"),width=1200, height=1600)
  
  #basebap
  fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)
  
  #annual data
  dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.wgwide2020.csv",y = y, ptype = "Y", pnum = y, Cry=c)
  
  range(dfSub$TOT)
  
  fPlotDist(dfSub,min=0,max=1,fill.col="antiquewhite",border.col="antiquewhite")
  fPlotDist(dfSub,min=1,max=100,fill.col="lightpink",border.col="lightpink")
  fPlotDist(dfSub,min=100,max=1000,fill.col="lightpink3",border.col="lightpink3")
  fPlotDist(dfSub,min=1000,max=15000,fill.col="firebrick2",border.col="firebrick2")
  fPlotDist(dfSub,min=15000,max=1e10,fill.col="firebrick4",border.col="firebrick4")
  
  fPlotBaseMap(xlim=c(-19,10),ylim=c(37,70),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)
  
  legend(x = "bottomright",
         legend = c("<1t","1t to 100t","100t to 1000t","1000t to 15000t",">15000t"),
         fill = c("antiquewhite","lightpink","lightpink3","firebrick2","firebrick4"),
         border = "black",
         cex = 4,
         title = paste0(c," Catch ",y))
  
  dev.off()
  
}

#end 28/09/2020



#05/11/2019 Stockbook plots
#Mackerel
#plot of catch distribution for entire year for 1) all nations 2) Ireland only
#no legend, no grid lines, no axis notations

#data year, reporting year
data.y <- 2018; rep.y <- 2019

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


#horse mackerel
png(filename = file.path(getwd(),"Plots","Stockbooks",rep.y,paste0("WG",data.y,"_HOM_CBySR.png")),width=1200, height=1600)

#basemap
fPlotBaseMap(xlim=c(-15,10),ylim=c(37,66),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
dfSub <- fSubset(src = ".\\Data\\hom.27WGCatchBySR.csv",y = data.y, ptype = "Y", pnum = data.y)

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
       title = paste0("Catch ",data.y))

dev.off()


#end Stockbook 2019 (05/11/2019)



#Irish catch
png(filename = file.path(getwd(),"Plots","Stockbooks",rep.y,paste0("IE",rep.y,"_HOM_CBySR.png")),width=1200, height=1600)

#basebap
fPlotBaseMap(xlim=c(-15,10),ylim=c(37,66),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
dfSub <- fSubset(src = ".\\Data\\hom.27WGCatchBySR.csv",y = data.y, ptype = "Y", pnum = data.y, Cry='IE')

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
       title = paste0("Catch ",data.y))

dev.off()




#10/09/2019
#Blue Whiting catch plots (for BIM request)

#Blue Whiting - all fleets
y <- 2018
png(filename = paste0(".\\Plots\\Stockbooks\\2019\\WG",y,"_WHB_CBySR.png"),width=1200, height=1600)

#basebap
fPlotBaseMap(xlim=c(-17,10),ylim=c(37,70),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

#annual data
#dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.csv",y = y, ptype = "Y", pnum = y)
dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.wgwide2019.csv",y = y, ptype = "Y", pnum = y)

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
#crys <- c("DK","FO","FR","DE","IE","IC","LT","NL","NO","PT","RU","UKS","SE","ES","UKE")
crys <- c("DEU","DNK","NOR","FRO","ISL","RUS","NLD","UKS","IRL","SCO")
y <- 2018

for (c in crys) {
  
  png(filename = paste0(".\\Plots\\Stockbooks\\2019\\",c,y,"_WHB_CBySR.png"),width=1200, height=1600)
  
  #basebap
  fPlotBaseMap(xlim=c(-17,10),ylim=c(37,70),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)
  
  #annual data
  dfSub <- fSubset(src = ".\\Data\\whb.27.1-91214WGCatchBySR.wgwide2019.csv",y = y, ptype = "Y", pnum = y, Cry=c)
  
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

#end 10/09/2019

#WKRRMAC - request from Martin Pastoors
#annual plots

df1 <- read.table(file = paste0(".\\Data\\WGCatchBySR.csv"), header = TRUE, 
                  sep = ",", stringsAsFactors = FALSE)

dfAllCtry <- df1 %>%
  group_by(Year,SR,Lat,Lon) %>% 
  summarise(Tot=sum(Catch))

write.table(dfAllCtry,file="CBySR_Rob.csv",sep=",",quote=FALSE,row.names=FALSE)

#plot settings
Catch.Bins <- c("<1t","1t-10t","1t-100t","100t-1000t","1000t-5000t","5000t-10000t",">10000t")
Catch.Limits <- c(0,1,10,100,1000,5000,10000)
Seven.Colours <- rev(heat.colors(7))
xlim=c(-36,20)
ylim=c(36,76)
#xlim <- c(-8,8);ylim <- c(57,63)

for (y in seq(2010,2017)){
  
  df.y <- dplyr::filter(df1,Year==y)

  catchmap <- ggplot(map.df) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    geom_hline(yintercept=seq(34,76,by=0.5), linetype="solid", color = "grey") +
    geom_vline(xintercept=seq(-42,32), linetype="solid", color = "grey") +
    aes(long, lat, group=group) +
    geom_path(color = "white") +
    geom_polygon() +
    theme_map()
  
  dfRect <- dplyr::filter(df1,Year==y) %>%
    group_by(Year,SR,Lat,Lon) %>% 
    summarise(Tot=sum(Catch))

  if (nrow(dfRect)>0) {
    
    #create data frame with polygons for catch
    dfPolys <- data.frame(SR=c(),Lat=c(),Lon=c(),Catch=c())
    
    for (i in 1:nrow(dfRect)){
      dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]-0.25,Lon=dfRect$Lon[i]-0.5,
                                                     Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                     stringsAsFactors = FALSE))
      dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]-0.25,Lon=dfRect$Lon[i]+0.5,
                                                     Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                     stringsAsFactors = FALSE))
      dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]+0.25,Lon=dfRect$Lon[i]+0.5,
                                                     Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                     stringsAsFactors = FALSE))
      dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]+0.25,Lon=dfRect$Lon[i]-0.5,
                                                     Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                     stringsAsFactors = FALSE))
    }
    
    dfPolys$Catch <- factor(dfPolys$Catch,levels=Catch.Bins)
    
    catchmap <- catchmap + geom_polygon(data=dfPolys,
                                        aes(x=Lon,y=Lat,group=SR,fill=Catch)) +
      scale_fill_manual(values = Seven.Colours, drop=FALSE) + 
      theme(legend.position = c(0.0, 0.6), legend.text = element_text(size=10), 
            legend.title = element_text(size=10))
    
    #catchmap <- catchmap + guides(fill=guide_legend(title=plotTitle))
    
    catchmap <- catchmap + geom_polygon(data=map.df,
                                        aes(long, lat, group=group))
    
    ggsave(filename = file.path(".","Plots","WKRRMAC",paste0(y,"_AnnCatch.pdf")),
           plot = catchmap,
           width = 10,
           height = 10)
    
  }
}

#end WKRRMAC



#22nd January - webex
#exploration of fishing pattern for NO, IE and UKS , IVa Q4
#raw data
df1 <- read.table(file = paste0(".\\Data\\WGCatchBySR.csv"), header = TRUE, 
                  sep = ",", stringsAsFactors = FALSE)


df2 <- dplyr::filter(df1,Year %in% c(2012,2013,2014,2015,2016,2017))
df2 <- dplyr::left_join(df2,dplyr::select(dfSR,SR=Rect,AreaKm2,Div))
#df2 <- dplyr::filter(df2,Div %in% c("IIa","IVa"))
df2 <- dplyr::filter(df2,Div %in% c("IVa"))
df2 <- dplyr::filter(df2,PNum %in% c(10,11,12))
#keep those countries with samples from IIa and IVa in Q4
#df2 <- dplyr::filter(df2,Ctry %in% c("DK","FO","IE","NO","UKS"))
df2 <- dplyr::filter(df2,Ctry %in% c("IE","NO","UKS"))
df2 <- df2 %>% group_by(Ctry,Year,SR,Lat,Lon,PNum) %>% summarise(Tot=sum(Catch))


#df2 <- dplyr::filter(df1,Ctry %in% c("NO","IE","UKS") & Year %in% c(2012,2013,2014,2015,2016,2017))
#df2 <- dplyr::left_join(df2,dplyr::select(dfSR,SR=Rect,AreaKm2,Div))
#df2 <- dplyr::filter(df2,Div=="IVa" & PNum %in% c(10,11,12))
#df2 <- dplyr::filter(df2,Div=="VIa" & PNum %in% c(1,2,3))
#df2 <- df2 %>% group_by(Ctry,Year,SR,Lat,Lon,PNum) %>% summarise(Tot=sum(Catch))
#df2 <- df2 %>% group_by(Ctry,Year,SR,Lat,Lon) %>% summarise(Tot=sum(Catch))

Catch.Bins <- c("<1t","1t-10t","1t-100t","100t-1000t","1000t-5000t","5000t-10000t",">10000t")
Catch.Limits <- c(0,1,10,100,1000,5000,10000)
Seven.Colours <- rev(heat.colors(7))

#IIa, IVa Q4 2017 plots
y <- 2017

for (c in names(table(df2$Ctry))) {
  
  for (m in c(10,11,12)) {

    xlim <- c(-8,8);ylim <- c(57,63)
    
    catchmap <- ggplot(map.df) +
      coord_cartesian(xlim = xlim, ylim = ylim) +
      geom_hline(yintercept=seq(34,76,by=0.5), linetype="solid", color = "grey") +
      geom_vline(xintercept=seq(-42,32), linetype="solid", color = "grey") +
      aes(long, lat, group=group) +
      geom_path(color = "white") +
      geom_polygon() +
      theme_map()

    dfRect <- dplyr::filter(df2,Year==y & Ctry==c & PNum==m)

    if (nrow(dfRect)>0) {
      
      #create data frame with polygons for catch
      dfPolys <- data.frame(SR=c(),Lat=c(),Lon=c(),Catch=c())
      
      for (i in 1:nrow(dfRect)){
        dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]-0.25,Lon=dfRect$Lon[i]-0.5,
                                                       Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                       stringsAsFactors = FALSE))
        dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]-0.25,Lon=dfRect$Lon[i]+0.5,
                                                       Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                       stringsAsFactors = FALSE))
        dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]+0.25,Lon=dfRect$Lon[i]+0.5,
                                                       Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                       stringsAsFactors = FALSE))
        dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]+0.25,Lon=dfRect$Lon[i]-0.5,
                                                       Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                       stringsAsFactors = FALSE))
      }
      
      dfPolys$Catch <- factor(dfPolys$Catch,levels=Catch.Bins)
      
      catchmap <- catchmap + geom_polygon(data=dfPolys,
                                          aes(x=Lon,y=Lat,group=SR,fill=Catch)) +
        scale_fill_manual(values = Seven.Colours, drop=FALSE) + 
        theme(legend.position = "none")
        
        #theme(legend.position = c(0.0, 0.6), legend.text = element_text(size=10), 
        #      legend.title = element_text(size=10))

      if(m==10){plotTitle <- paste(c,"Oct",as.character(y))}
      if(m==11){plotTitle <- paste(c,"Nov",as.character(y))}
      if(m==12){plotTitle <- paste(c,"Dec",as.character(y))}
      
      #catchmap <- catchmap + guides(fill=guide_legend(title=plotTitle))
      
      catchmap <- catchmap + geom_polygon(data=map.df,
                                          aes(long, lat, group=group))
      
    }
    
    catchmap <- catchmap + annotate(geom="text", x=-8, y=63, label=plotTitle, hjust=0)
    
    assign(paste0(c,m),catchmap)
    
  }
}

#landscape plot
#pall <- ggarrange(NO10,IE10,UKS10,DK10,FO10,
#                  NO11,IE11,UKS11,DK11,FO11,
#                  NO12,IE12,UKS12,DK12,FO12,
#                  nrow=3,ncol=5)

#ggsave(filename = file.path(".","Plots","WKIBPNEAMac",paste0("IIa_IVa_Q4_",y,"_Landscape.png")),
#       plot = pall,
#       width = 16,
#       height = 10)

#portrait plot
#pall <- ggarrange(NO10,NO11,NO12,
#                  IE10,IE11,IE12,
#                  UKS10,UKS11,UKS12,
#                  DK10,DK11,DK12,
#                  FO10,FO11,FO12,
#                  nrow=5,ncol=3)

pall <- ggarrange(NO10,NO11,NO12,
                  IE10,IE11,IE12,
                  UKS10,UKS11,UKS12,
                  nrow=3,ncol=3)

#ggsave(filename = file.path(".","Plots","WKIBPNEAMac",paste0("IIa_IVa_Q4_",y,"_Portrait.png")),
#       plot = pall,
#       width = 10,
#       height = 16)

ggsave(filename = file.path(".","Plots","WKIBPNEAMac",paste0("IVa_Q4_",y,"_Portrait.png")),
       plot = pall,
       width = 10,
       height = 10)


#VIa, Q1 plots
for (y in c(2012,2013,2014,2015,2016,2017)){
  
  for (c in c("NO","IE","UKS")) {
    
    for (m in c(1,2,3,13)) {
      
      #IVa
      xlim <- c(-14,0);ylim <- c(52,60)
      
      catchmap <- ggplot(map.df) +
        coord_cartesian(xlim = xlim, ylim = ylim) +
        geom_hline(yintercept=seq(34,76,by=0.5), linetype="solid", color = "grey") +
        geom_vline(xintercept=seq(-42,32), linetype="solid", color = "grey") +
        aes(long, lat, group=group) +
        geom_path(color = "white") +
        geom_polygon() +
        theme_map()
      
      if (m==13) {
        dfRect <- dplyr::filter(df2,Year==y & Ctry==c) %>%
          group_by(Ctry,Year,SR,Lat,Lon) %>% 
          summarise(Tot=sum(Tot))
      } else {
        dfRect <- dplyr::filter(df2,Year==y & Ctry==c & PNum==m)
      }
      
      if (nrow(dfRect)>0) {
        
        #create data frame with polygons for catch
        dfPolys <- data.frame(SR=c(),Lat=c(),Lon=c(),Catch=c())
        
        for (i in 1:nrow(dfRect)){
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]-0.25,Lon=dfRect$Lon[i]-0.5,
                                                         Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]-0.25,Lon=dfRect$Lon[i]+0.5,
                                                         Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]+0.25,Lon=dfRect$Lon[i]+0.5,
                                                         Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]+0.25,Lon=dfRect$Lon[i]-0.5,
                                                         Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
        }
        
        dfPolys$Catch <- factor(dfPolys$Catch,levels=Catch.Bins)
        
        catchmap <- catchmap + geom_polygon(data=dfPolys,
                                            aes(x=Lon,y=Lat,group=SR,fill=Catch)) +
          scale_fill_manual(values = Seven.Colours, drop=FALSE) + 
          theme(legend.position = c(0.0, 0.6), legend.text = element_text(size=10), 
                legend.title = element_text(size=10))
        
        if(m==1){plotTitle <- paste(c,"Apr",as.character(y))}
        if(m==2){plotTitle <- paste(c,"Feb",as.character(y))}
        if(m==3){plotTitle <- paste(c,"Mar",as.character(y))}
        if(m==13){plotTitle <- paste(c,"Q1",as.character(y))}
        
        catchmap <- catchmap + guides(fill=guide_legend(title=plotTitle))
        
        catchmap <- catchmap + geom_polygon(data=map.df,
                                            aes(long, lat, group=group))
        
      }
      
      
      if(m==1){m1<-catchmap}
      if(m==2){m2<-catchmap}
      if(m==3){m3<-catchmap}
      if(m==13){Q1<-catchmap}
      
    }   #month loop
    
    pall <- ggarrange(m1,m2,m3,Q1,nrow=2,ncol=2)
    
    ggsave(filename = file.path(".","Plots","WKIBPNEAMac",paste0("VIa_",c,"_",y,".png")),
           plot = pall,
           width = 16,
           height = 10)
    
  }
  
}


#IVa,Q4 plots
for (y in c(2012,2013,2014,2015,2016,2017)){

  for (c in c("NO","IE","UKS")) {
      
    for (m in c(10,11,12,13)) {
      
      #IVa
      xlim <- c(-6,8);ylim <- c(56,63)
    
      catchmap <- ggplot(map.df) +
        coord_cartesian(xlim = xlim, ylim = ylim) +
        geom_hline(yintercept=seq(34,76,by=0.5), linetype="solid", color = "grey") +
        geom_vline(xintercept=seq(-42,32), linetype="solid", color = "grey") +
        aes(long, lat, group=group) +
        geom_path(color = "white") +
        geom_polygon() +
        theme_map()
    
      if (m==13) {
        dfRect <- dplyr::filter(df2,Year==y & Ctry==c) %>%
          group_by(Ctry,Year,SR,Lat,Lon) %>% 
          summarise(Tot=sum(Tot))
      } else {
          dfRect <- dplyr::filter(df2,Year==y & Ctry==c & PNum==m)
      }
    
      if (nrow(dfRect)>0) {
        
        #create data frame with polygons for catch
        dfPolys <- data.frame(SR=c(),Lat=c(),Lon=c(),Catch=c())
        
        for (i in 1:nrow(dfRect)){
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]-0.25,Lon=dfRect$Lon[i]-0.5,
                                                         Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]-0.25,Lon=dfRect$Lon[i]+0.5,
                                                         Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]+0.25,Lon=dfRect$Lon[i]+0.5,
                                                         Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
          dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfRect$SR[i],Lat=dfRect$Lat[i]+0.25,Lon=dfRect$Lon[i]-0.5,
                                                         Catch=fCatchBin(dfRect$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                         stringsAsFactors = FALSE))
        }
        
        dfPolys$Catch <- factor(dfPolys$Catch,levels=Catch.Bins)
        
        catchmap <- catchmap + geom_polygon(data=dfPolys,
                                            aes(x=Lon,y=Lat,group=SR,fill=Catch)) +
          scale_fill_manual(values = Seven.Colours, drop=FALSE) + 
          theme(legend.position = c(0.0, 0.6), legend.text = element_text(size=10), 
                legend.title = element_text(size=10))

        if(m==10){plotTitle <- paste(c,"Oct",as.character(y))}
        if(m==11){plotTitle <- paste(c,"Nov",as.character(y))}
        if(m==12){plotTitle <- paste(c,"Dec",as.character(y))}
        if(m==13){plotTitle <- paste(c,"Q4",as.character(y))}
        
        catchmap <- catchmap + guides(fill=guide_legend(title=plotTitle))
        
        catchmap <- catchmap + geom_polygon(data=map.df,
                                            aes(long, lat, group=group))
        
      }
    

      if(m==10){m10<-catchmap}
      if(m==11){m11<-catchmap}
      if(m==12){m12<-catchmap}
      if(m==13){Q4<-catchmap}
      
    }   #month loop
    
    pall <- ggarrange(m10,m11,m12,Q4,nrow=2,ncol=2)
    
    ggsave(filename = file.path(".","Plots","WKIBPNEAMac",paste0("IVa_",c,"_",y,".png")),
           plot = pall,
           width = 16,
           height = 10)
  
}
  
}


#20th January
#Request from Steve
#Catch dist for all countries (combined) for July and August combined from 1995-present.
#Total fishing area based on SR area

#all catch by SR data
dfCatchBySR <- read.table(file = ".\\Data\\WGCatchBySR.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#table of catch by month by country (DE,ES,FO,FR,GL,IC,IE,NL,NO,RU,UKS)
for (c in c("DE","ES","FO","FR","GL","IC","IE","NL","NO","RU","UKS")){
  dfTemp <- data.frame(Year=c(),Month=c(),Tot=c())
  for (y in seq(1998,2017)) {
    for (m in seq(1,12)) {
      dfSubset <- fSubset(ptype="M",pnum=m,y=y,Cry=c)
      dfTemp=dplyr::bind_rows(dfTemp,data.frame(Year=y,Month=m,Tot=sum(dfSubset$Tot)))
    }
  }
  dfTable <- xtable(tidyr::spread(dfTemp,key="Month",val="Tot"),digits=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))
  writeLines(text=print(dfTable,include.rownames = FALSE, type="latex"),
             con = file.path(getwd(),"Tex",paste0(c,"CatchByMonth.tex")))
}

#years with data
yrs <- as.numeric(names(table(dfCatchBySR$Year)))

dfAllYears <- data.frame(Year=c(),SR=c(),Lat=c(),Lon=c(),Tot=c(),AreaKm2=c(),Div=c())
dfAllYears.AllMonths <- data.frame(Year=c(),SR=c(),Lat=c(),Lon=c(),Tot=c(),AreaKm2=c(),Div=c())
dfAllYears.ByQ <- data.frame(Year=c(),Q=c(),SR=c(),Lat=c(),Lon=c(),Tot=c(),AreaKm2=c(),Div=c())

for (y in yrs){
  dfCatchBySR.Q1 <- fSubset(ptype="Q",pnum=1,y=y)
  dfCatchBySR.Q2 <- fSubset(ptype="Q",pnum=2,y=y)
  dfCatchBySR.Q3 <- fSubset(ptype="Q",pnum=3,y=y)
  dfCatchBySR.Q4 <- fSubset(ptype="Q",pnum=4,y=y)
  dfCatchBySR.Jul <- fSubset(ptype="M",pnum=7,y=y)
  dfCatchBySR.Aug <- fSubset(ptype="M",pnum=8,y=y)
  
  
  #dfCatchBySR.AllMonths <- fSubset(ptype="M",pnum=seq(1,12),y=y)
  
  #Norway only
  #dfCatchBySR.Jul <- fSubset(ptype="M",pnum=7,y=y,Cry="NO")
  #dfCatchBySR.Aug <- fSubset(ptype="M",pnum=8,y=y,Cry="NO")
  dfCatchBySR.AllMonths <- fSubset(ptype="M",pnum=seq(1,12),y=y,Cry="NO")
  
  #Russia only
  #dfCatchBySR.Jul <- fSubset(ptype="M",pnum=7,y=y,Cry="RU")
  #dfCatchBySR.Aug <- fSubset(ptype="M",pnum=8,y=y,Cry="RU")
  
  dfJulandAug <- dplyr::bind_rows(dfCatchBySR.Jul,dfCatchBySR.Aug)
  dfJulandAug <- dfJulandAug %>% dplyr::group_by(Year,SR,Lat,Lon) %>% summarise(Tot=sum(Tot))
  dfCatchBySR.AllMonths <- dfCatchBySR.AllMonths %>% dplyr::group_by(Year,SR,Lat,Lon) %>% summarise(Tot=sum(Tot))
  dfCatchBySR.Q1 <- dfCatchBySR.Q1 %>% dplyr::group_by(Year,SR,Lat,Lon) %>% summarise(Tot=sum(Tot))
  dfCatchBySR.Q2 <- dfCatchBySR.Q2 %>% dplyr::group_by(Year,SR,Lat,Lon) %>% summarise(Tot=sum(Tot))
  dfCatchBySR.Q3 <- dfCatchBySR.Q3 %>% dplyr::group_by(Year,SR,Lat,Lon) %>% summarise(Tot=sum(Tot))
  dfCatchBySR.Q4 <- dfCatchBySR.Q4 %>% dplyr::group_by(Year,SR,Lat,Lon) %>% summarise(Tot=sum(Tot))
  
  #retrieve the Area and Division
  dfJulandAug <- dplyr::left_join(dfJulandAug,select(dfSR,SR=Rect,AreaKm2,Div))
  dfCatchBySR.AllMonths <- dplyr::left_join(dfCatchBySR.AllMonths,select(dfSR,SR=Rect,AreaKm2,Div))
  dfCatchBySR.Q1 <- dplyr::left_join(dfCatchBySR.Q1,select(dfSR,SR=Rect,AreaKm2,Div))
  dfCatchBySR.Q2 <- dplyr::left_join(dfCatchBySR.Q2,select(dfSR,SR=Rect,AreaKm2,Div))
  dfCatchBySR.Q3 <- dplyr::left_join(dfCatchBySR.Q3,select(dfSR,SR=Rect,AreaKm2,Div))
  dfCatchBySR.Q4 <- dplyr::left_join(dfCatchBySR.Q4,select(dfSR,SR=Rect,AreaKm2,Div))
  
  dfAllYears <- dplyr::bind_rows(dfAllYears,dfJulandAug)
  dfAllYears.AllMonths <- dplyr::bind_rows(dfAllYears.AllMonths,dfCatchBySR.AllMonths)
  dfAllYears.ByQ <- dplyr::bind_rows(dfAllYears.ByQ,
                                     data.frame(Year=dfCatchBySR.Q1$Year,
                                                Q=paste0("Q",rep(1,length(dfCatchBySR.Q1$Year))),
                                                SR=dfCatchBySR.Q1$SR,
                                                Lat=dfCatchBySR.Q1$Lat,
                                                Lon=dfCatchBySR.Q1$Lon,
                                                Tot=dfCatchBySR.Q1$Tot,
                                                AreaKm2=dfCatchBySR.Q1$AreaKm2,
                                                Div=dfCatchBySR.Q1$Div,
                                                stringsAsFactors = FALSE))
  dfAllYears.ByQ <- dplyr::bind_rows(dfAllYears.ByQ,
                                     data.frame(Year=dfCatchBySR.Q2$Year,
                                                Q=paste0("Q",rep(2,length(dfCatchBySR.Q2$Year))),
                                                SR=dfCatchBySR.Q2$SR,
                                                Lat=dfCatchBySR.Q2$Lat,
                                                Lon=dfCatchBySR.Q2$Lon,
                                                Tot=dfCatchBySR.Q2$Tot,
                                                AreaKm2=dfCatchBySR.Q2$AreaKm2,
                                                Div=dfCatchBySR.Q2$Div,
                                                stringsAsFactors = FALSE))
  dfAllYears.ByQ <- dplyr::bind_rows(dfAllYears.ByQ,
                                     data.frame(Year=dfCatchBySR.Q3$Year,
                                                Q=paste0("Q",rep(3,length(dfCatchBySR.Q3$Year))),
                                                SR=dfCatchBySR.Q3$SR,
                                                Lat=dfCatchBySR.Q3$Lat,
                                                Lon=dfCatchBySR.Q3$Lon,
                                                Tot=dfCatchBySR.Q3$Tot,
                                                AreaKm2=dfCatchBySR.Q3$AreaKm2,
                                                Div=dfCatchBySR.Q3$Div,
                                                stringsAsFactors = FALSE))
  dfAllYears.ByQ <- dplyr::bind_rows(dfAllYears.ByQ,
                                     data.frame(Year=dfCatchBySR.Q4$Year,
                                                Q=paste0("Q",rep(4,length(dfCatchBySR.Q4$Year))),
                                                SR=dfCatchBySR.Q4$SR,
                                                Lat=dfCatchBySR.Q4$Lat,
                                                Lon=dfCatchBySR.Q4$Lon,
                                                Tot=dfCatchBySR.Q4$Tot,
                                                AreaKm2=dfCatchBySR.Q4$AreaKm2,
                                                Div=dfCatchBySR.Q4$Div,
                                                stringsAsFactors = FALSE))
  
}


table(dfAllYears$Year)
#unmatched rectangles - reporting issues
table(dfAllYears[is.na(dfAllYears$Area),]$SR)
table(dfAllYears[is.na(dfAllYears$Div),]$SR)

dfAllYears.AllMonths[is.na(dfAllYears.AllMonths$Div),]

#Seven.Colours <- rev(heat.colors(7))

#all for over 1000
#Catch.Bins <- c("1000t-2500t","2500t-5000t","5000t-7500t","7500t-10000t",">10000t")
#Catch.Limits <- c(1000,2500,5000,7500,10000)

#single country
Catch.Bins <- c("<1t","1t-10t","1t-100t","100t-1000t","1000t-5000t","5000t-10000t",">10000t")
Catch.Limits <- c(0,1,10,100,1000,5000,10000)

Bin.Colours <- rev(heat.colors(length(Catch.Bins)))

#plots
for (y in yrs){
  
  xlim <- c(-36,30);ylim <- c(36,73)
  
  catchmap <- ggplot(map.df) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    geom_hline(yintercept=seq(34,76,by=0.5), linetype="solid", color = "grey") +
    geom_vline(xintercept=seq(-42,32), linetype="solid", color = "grey") +
    aes(long, lat, group=group) +
    geom_path(color = "white") +
    geom_polygon() +
    theme_map()

  #dfSR2Plot <- dplyr::filter(dfAllYears,Year==y)
  #all months, rects with 1000t or more
  #dfSR2Plot <- dplyr::filter(dfAllYears.AllMonths,Year==y & Tot>1000)
  #all months
  dfSR2Plot <- dplyr::filter(dfAllYears.AllMonths,Year==y)
  
  if (nrow(dfSR2Plot)>0) {

    #create data frame with polygons for catch
    dfPolys <- data.frame(SR=c(),Lat=c(),Lon=c(),Catch=c())

    for (i in 1:nrow(dfSR2Plot)){
      dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSR2Plot$SR[i],Lat=dfSR2Plot$Lat[i]-0.25,Lon=dfSR2Plot$Lon[i]-0.5,
                                                     Catch=fCatchBin(dfSR2Plot$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                     stringsAsFactors = FALSE))
      dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSR2Plot$SR[i],Lat=dfSR2Plot$Lat[i]-0.25,Lon=dfSR2Plot$Lon[i]+0.5,
                                                     Catch=fCatchBin(dfSR2Plot$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                     stringsAsFactors = FALSE))
      dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSR2Plot$SR[i],Lat=dfSR2Plot$Lat[i]+0.25,Lon=dfSR2Plot$Lon[i]+0.5,
                                                     Catch=fCatchBin(dfSR2Plot$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                     stringsAsFactors = FALSE))
      dfPolys <- dplyr::bind_rows(dfPolys,data.frame(SR=dfSR2Plot$SR[i],Lat=dfSR2Plot$Lat[i]+0.25,Lon=dfSR2Plot$Lon[i]-0.5,
                                                     Catch=fCatchBin(dfSR2Plot$Tot[i],Limits=Catch.Limits,Labels=Catch.Bins),
                                                     stringsAsFactors = FALSE))
    }

    dfPolys$Catch <- factor(dfPolys$Catch,levels=Catch.Bins)

    catchmap <- catchmap + geom_polygon(data=dfPolys,
                                        aes(x=Lon,y=Lat,group=SR,fill=Catch)) +
      scale_fill_manual(values = Bin.Colours, drop=FALSE)

    catchmap <- catchmap + geom_polygon(data=map.df,
                                        aes(long, lat, group=group))

  }

  catchmap <- catchmap + ggtitle(paste(y,"All SR, Norway"))
    
  catchmap <- catchmap + guides(fill=guide_legend(title=paste(y)))
  
  #fishing area
  catchmap <- catchmap + annotate("text",x=xlim[1],y=ylim[2],hjust=0,
                                  label=paste("Fishing Area = ",sum(dfSR2Plot$AreaKm2,na.rm=TRUE)/1e6, " million km2"))
  #total catch
  catchmap <- catchmap + annotate("text",x=xlim[1],y=ylim[2]-2,hjust=0,
                                  label=paste("Total = ",sum(dfSR2Plot$Tot,na.rm=TRUE), " t"))
  
  ggsave(filename = file.path(".","Plots","WKIBPNEAMac",paste0("All_NO_",y,".png")),
         plot = catchmap,
         width = 8,
         height = 5)

}


#all records
dfFishingArea <- dfAllYears %>% 
  group_by(Year) %>%
  summarise(FishingArea=sum(AreaKm2,na.rm=TRUE)/1e6)

dfFishingArea <- dplyr::left_join(dfFishingArea,
                                  filter(dfAllYears,Tot>1) %>% 
                                    group_by(Year) %>%
                                    summarise(FishingArea_o1t=sum(AreaKm2,na.rm=TRUE)/1e6))

dfFishingArea <- dplyr::left_join(dfFishingArea,
                                  filter(dfAllYears,Tot>10) %>% 
                                    group_by(Year) %>%
                                    summarise(FishingArea_o10t=sum(AreaKm2,na.rm=TRUE)/1e6))

dfFishingArea <- dplyr::left_join(dfFishingArea,
                                  filter(dfAllYears,Tot>100) %>% 
                                    group_by(Year) %>%
                                    summarise(FishingArea_o100t=sum(AreaKm2,na.rm=TRUE)/1e6))

dfFishingArea <- dplyr::left_join(dfFishingArea,
                                  filter(dfAllYears,Tot>1000) %>% 
                                    group_by(Year) %>%
                                    summarise(FishingArea_o1000t=sum(AreaKm2,na.rm=TRUE)/1e6))


#quick look
plot(dfFishingArea$Year,dfFishingArea$FishingArea,xlab="Year",ylab="Area",ylim=c(0,3),type="l")
lines(dfFishingArea$Year,dfFishingArea$FishingArea_o1t)
lines(dfFishingArea$Year,dfFishingArea$FishingArea_o10t)
lines(dfFishingArea$Year,dfFishingArea$FishingArea_o100t)
lines(dfFishingArea$Year,dfFishingArea$FishingArea_o1000t)

dfFA.Table <- xtable(select(dfFishingArea,Year,AllRects=FishingArea,
                            Over1t=FishingArea_o1t,Over10t=FishingArea_o10t,
                            Over100t=FishingArea_o100t,Over1000t=FishingArea_o1000t),
                     digits=c(0,0,2,2,2,2,2))
print(dfFA.Table, type="latex")


#by division
table(dfAllYears.AllMonths$Year)
dfIIa <- dfAllYears.AllMonths %>%
  filter(Div=="IIa") %>%
  group_by(Year) %>%
  summarise(TotIIa=sum(Tot))
dfIVa <- dfAllYears.AllMonths %>%
  filter(Div=="IVa") %>%
  group_by(Year) %>%
  summarise(TotIVa=sum(Tot))
dfVIa <- dfAllYears.AllMonths %>%
  filter(Div=="VIa") %>%
  group_by(Year) %>%
  summarise(TotVIa=sum(Tot))
dfAll <- dfAllYears.AllMonths %>%
  filter(!is.na(Div)) %>%
  group_by(Year) %>%
  summarise(Tot=sum(Tot))

dfByDiv <- dplyr::bind_cols(dfAll,select(dfIIa,TotIIa))
dfByDiv <- dplyr::bind_cols(dfByDiv,select(dfIVa,TotIVa))
dfByDiv <- dplyr::bind_cols(dfByDiv,select(dfVIa,TotVIa))
dfByDiv$PropIIa <- dfByDiv$TotIIa/dfByDiv$Tot
dfByDiv$PropIVa <- dfByDiv$TotIVa/dfByDiv$Tot
dfByDiv$PropVIa <- dfByDiv$TotVIa/dfByDiv$Tot

dfByDiv.Table <- xtable(dfByDiv,digits=c(0,0,0,0,0,0,2,2,2))
print(dfByDiv.Table, type="latex")

#quarterly report
dfT <- dfAllYears.ByQ
dfAllYears.ByQ <- tidyr::spread(dfAllYears.ByQ,key='Q',value='Tot',fill=0)
dfAllYears.ByQ <- dfAllYears.ByQ %>% dplyr::mutate(Annual = Q1+Q2+Q3+Q4)

#check annuals 
dfByQ <- dplyr::group_by(dfAllYears.ByQ,Year) %>% 
  summarise(Tot=sum(Annual), Q1=sum(Q1), Q2=sum(Q2), Q3=sum(Q3), Q4=sum(Q4)) %>%
  mutate(PQ1=Q1/Tot,PQ2=Q2/Tot,PQ3=Q3/Tot,PQ4=Q4/Tot)

dfByQ.Table <- xtable(select(dfByQ,Year,Q1=PQ1,Q2=PQ2,Q3=PQ3,Q4=PQ4),digits=c(0,0,2,2,2,2))
print(dfByQ.Table, type="latex")


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

#basemap
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
png(filename = paste0(".\\Plots\\Stockbooks\\2019\\WG",y,"_WHB_CBySR.png"),width=1200, height=1600)

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
