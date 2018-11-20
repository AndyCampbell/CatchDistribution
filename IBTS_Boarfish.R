#IBTS presence/absence map for boarfish

rm(list=ls())
gc()

library(dplyr)

load(".//..//Data//RData//coast.rda")
load(".//..//Data//RData//NEAFC.rda")
load(".//..//Data//RData//SR.rda")
SR$SR <- levels(SR$Rect)[SR$Rect]     #SR as a character

source("SRAnalyFuncs.R")

#dfSR <- read.table(file = ".//Data//boc.ibts.txt", header = TRUE, sep = " ", stringsAsFactors = FALSE)
#this file retrieve from the pelagic data folder
dfSR <- read.table(file = ".//Data//ibts.data.assessment.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

dfSR <- dplyr::select(dfSR,SR=Rectangle,No.30)
dfSR$Pos <- 0
dfSR$Pos[dfSR$No.30>0]<-1000
dfSR <- unique(dfSR)
dfSR <- dfSR %>% dplyr::group_by(SR) %>% dplyr::summarise(Tot=sum(Pos))

png(filename = paste0(".\\Plots\\Stockbooks\\",2018,"\\IBTS_BOC.png"),width=1200, height=1600)

fPlotBaseMap(xlim=c(-17,5),ylim=c(40,62),xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)
fPlotDist(dfSR,min=-1,max=10,fill.col="white",border.col="black")
fPlotDist(dfSR,min=100,max=1e9,fill.col="red",border.col="black")
fPlotBaseMap(xlim=c(-17,5),ylim=c(40,62),refresh=TRUE,xaxis=F,xlabs=F,yaxis=F,ylabs=F,SR=F,ICES=F)

legend(x = "bottomright",
       legend = c("Present","Absent"),
       fill = c("red","white"),
       border = "black",
       cex = 4,
       title = paste0("IBTS Boarfish")
       )

dev.off()
