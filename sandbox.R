#UKS, IE, NO IVa Q4
rm(list=ls())
library(dplyr)

#raw data
df1 <- read.table(file = paste0(".\\Data\\WGCatchBySR.csv"), header = TRUE, 
                  sep = ",", stringsAsFactors = FALSE)
load(".//..//Data//RData//SR.rda")
dfSR$SR <- levels(dfSR$Rect)[dfSR$Rect]     #SR as a character

df2 <- dplyr::filter(df1,Ctry %in% c("NO","IE","UKS") & Year %in% c(2015,2016,2017))
df2 <- dplyr::left_join(df2,dplyr::select(dfSR,SR=Rect,AreaKm2,Div))
df2 <- dplyr::filter(df2,Div=="IVa" & PNum %in% c(10,11,12))
df2 <- df2 %>% group_by(Ctry,Year,SR,PNum) %>% summarise(Catch=sum(Catch))
  







#check subset function
rm(list=ls())
#raw data
df1 <- read.table(file = paste0(".\\Data\\WGCatchBySR.csv"), header = TRUE, 
                  sep = ",", stringsAsFactors = FALSE)
head(df1)
df1 <- dplyr::filter(df1,Year==1998)

#totals by country, reporting period
dfTot <- df1 %>% group_by(Ctry,PType) %>% summarise(Tot=sum(Catch))
sum(dfTot$Tot)
#each country only reports by one period type - good

#now compare withcall to fSubSet function
source("SRAnalyFuncs.R")
df2 <- fSubset(y=1998,ptype="Y")
sum(df2$Tot)

df3 <- fSubset(y=1998,ptype = "M",pnum=seq(1,12))
sum(df3$Tot)

#the two sums should match



library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(jpeg)
library(grid)

graphics.dir <- file.path(getwd(),'Plots','WKIBPNEAMac')
p1 <- jpeg::readJPEG(file.path(graphics.dir,'2017_Apr_DK_CBySR.jpg'))
p2 <- jpeg::readJPEG(file.path(graphics.dir,'2017_Apr_FO_CBySR.jpg'))
p3 <- jpeg::readJPEG(file.path(graphics.dir,'2017_Apr_UKS_CBySR.jpg'))
p4 <- jpeg::readJPEG(file.path(graphics.dir,'2017_Apr_IC_CBySR.jpg'))
g1 <- rasterGrob(p1)
g2 <- rasterGrob(p2)
g3 <- rasterGrob(p3)
g4 <- rasterGrob(p4)

pall <- ggarrange(g1,g2,g3,g4,nrow=2,ncol=2)  
  
ggexport(pall, filename="pAll.pdf")


#ggplot basemap
library(maps)

for (i in 1:length(coast)){
  if (!is.null(coast[[i]])){
    if (coast[[i]]$fill==TRUE) {
      polygon(coast[[i]]$Lon,coast[[i]]$Lat,col="grey")
    } else {
      polygon(coast[[i]]$Lon,coast[[i]]$Lat,col="white")             
    }  
  }
}

dfTemp <- data.frame(long = t2$Lon, lat = t2$Lat)

g <- ggplot()
for (i in 1:length(coast)){
  if (!is.null(coast[[i]])){
    dfTemp <- data.frame(long = t2$Lon, lat = t2$Lat)
    g <- g + geom_path(data = dfTemp, aes(x = long, y = lat))
#    if (coast[[i]]$fill==TRUE) {
#      polygon(coast[[i]]$Lon,coast[[i]]$Lat,col="grey")
#    } else {
#      polygon(coast[[i]]$Lon,coast[[i]]$Lat,col="white")             
#    }  
  }
}

ggplot() + geom_path(data = dfTemp, aes(x = long, y = lat)))


