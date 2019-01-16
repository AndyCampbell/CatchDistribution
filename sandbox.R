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


