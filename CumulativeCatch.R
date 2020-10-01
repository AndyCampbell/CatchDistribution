#WKIBPNEAMac

#cumulative catch
#12th December 2018
#Cab Inn, Copenhagen

#17th January 2019
#further dev in prep for upcoming webex

rm(list=ls())
gc()

library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(tidyverse)
library(xtable)

source("SRAnalyFuncs.R")

out.path <- file.path(getwd(),"Tex")

dfCatches <- data.frame(Year=seq(1998,2017),
                        ICES=c(666664,640311,738608,737462,772905,679288,660491,549514,481181,
                               586206,623165,737969,875515,946661,894684,933165,1394454,1208990,
                               1094066,1155944))

#IESSNS survey timings
IESSNS <- data.frame(Year=c(2010,seq(2012,2018)),
                     start=c(as.Date("2010/07/08","%Y/%m/%d"),as.Date("2012/07/02","%Y/%m/%d"),
                             as.Date("2013/07/02","%Y/%m/%d"),as.Date("2014/07/02","%Y/%m/%d"),
                             as.Date("2015/07/01","%Y/%m/%d"),as.Date("2016/07/01","%Y/%m/%d"),
                             as.Date("2017/07/03","%Y/%m/%d"),as.Date("2018/06/30","%Y/%m/%d")),
                     end=c(as.Date("2010/08/21","%Y/%m/%d"),as.Date("2012/08/10","%Y/%m/%d"),
                           as.Date("2013/08/09","%Y/%m/%d"),as.Date("2014/08/12","%Y/%m/%d"),
                           as.Date("2015/08/10","%Y/%m/%d"),as.Date("2016/07/31","%Y/%m/%d"),
                           as.Date("2017/08/04","%Y/%m/%d"),as.Date("2018/08/06","%Y/%m/%d")))

#calculate day number
IESSNS <- dplyr::mutate(IESSNS,
                        start.day = 12*lubridate::yday(start)/365,
                        end.day = 12*lubridate::yday(end)/365)

xtable(IESSNS)

#read in the catch data (catch by stat rect)
df1 <- read.table(file = paste0(".\\Data\\WGCatchBySR.csv"), header = TRUE, 
                  sep = ",", stringsAsFactors = FALSE)

#period totals
dfPTotals <- dplyr::group_by(df1,Ctry,Year,PType,PNum) %>% summarise(PerTot=sum(Catch))
df1 <- dplyr::inner_join(df1,dfPTotals)
#period type totals
dfPTTotals <- dplyr::group_by(dfPTotals,Ctry,Year,PType) %>% summarise(PerTypeTot=sum(PerTot))
df1 <- dplyr::inner_join(df1,dfPTTotals)
#annual totals
dfAnnTotals <- dplyr::group_by(dfPTTotals,Ctry,Year) %>% summarise(AnnTot=sum(PerTypeTot))
df1 <- dplyr::inner_join(df1,dfAnnTotals)

#any fleets with different period types in the same year?
df1[!df1$PerTypeTot==df1$AnnTot,]
#nope - good

#total by year/period type
dfByPType <- df1 %>% group_by(Year,PType) %>% 
  summarise(Tot=sum(Catch)) %>% 
  spread(key="PType",value="Tot",fill=0) %>%
  mutate(Tot = M+Q) %>%
  mutate(pByQ = Q/Tot) %>%
  mutate(pByM = M/Tot) %>%
  select(Year,Tot,pByQ,pByM)

xtable(dfByPType,digits=c(0,0,0,2,2))


#total reported by SRd
dfSR <- dplyr::group_by(df1,Year) %>% summarise(Reported=sum(Catch))
dfCatches$SR <- dfSR$Reported
dfCatches$Prop <- dfCatches$SR/dfCatches$ICES

xtable(dfCatches)

dfSummary <- dplyr::select(df1,Ctry,Year,PType) %>% distinct()
dTemp<-xtabs(~Ctry + Year, data=dfSummary)
matPType <- matrix(data=NA,nrow=nrow(dTemp),ncol=ncol(dTemp),
                   dimnames=list(rownames(dTemp),colnames(dTemp)))
dfMon <- dplyr::filter(dfSummary,PType=="M")
for (i in 1:nrow(dfMon)){
  matPType[dfMon[i,]$Ctry,as.character(dfMon[i,]$Year)]<-"M"
}
dfQur <- dplyr::filter(dfSummary,PType=="Q")
for (i in 1:nrow(dfQur)){
  matPType[dfQur[i,]$Ctry,as.character(dfQur[i,]$Year)]<-"Q"
}

dfPType <- as.data.frame(matPType)
#major fleets only
dfPType.Table <- xtable(dfPType[c('DE','DK','ES','FO','FR','GL','IC','IE','NL','NO','PT','RU','SE','UKE','UKN','UKS'),1:10])
#writeLines(text = print(dfPType.Table, type="latex"),con = file.path(out.path,"DataByPeriodType1.tex"))
dfPType.Table <- xtable(dfPType[c('DE','DK','ES','FO','FR','GL','IC','IE','NL','NO','PT','RU','SE','UKE','UKN','UKS'),11:20])
#writeLines(text = print(dfPType.Table, type="latex"),con = file.path(out.path,"DataByPeriodType2.tex"))

#period types for the data
xtabs(~PType + Year, data=df1)

#quarterly records by year/country - relatively sparse back to 2006
xtabs(~Ctry + Year, data = filter(df1,PType=="Q"))

#convert quarterly data to monthly, assuming equal catches in each month of the quarter
dfQ <- dplyr::filter(df1,PType=="Q") %>%
  group_by(Year,Ctry,PNum) %>%
  mutate(Per1 = 3*(PNum-1)+1, Per2 = Per1+1, Per3 = Per2+1, MPType="M", MCatch = Catch/3) %>%
  ungroup() %>%
  select(Ctry,Year,SR,Lat,Lon,PType=MPType,Per1,Per2,Per3,Catch=MCatch) %>%
  tidyr::gather(key="key",value="PNum",Per1,Per2,Per3) %>%
  select(Ctry,Year,SR,Lat,Lon,PType,PNum,Catch)

df1 <- bind_rows(df1,dfQ)

#monthly data
df2 <- dplyr::filter(df1,PType=="M") %>%
  select(Year,PNum,Catch) %>%
  group_by(Year,PNum) %>%
  summarise(Tot=sum(Catch)) %>%
  mutate(Cumulative = cumsum(Tot)/1e3,AnnualTot=max(Cumulative),Norm=Cumulative/AnnualTot,Prop=(Tot/1e3)/AnnualTot)

#by country
df3 <- dplyr::filter(df1,PType=="M") %>%
  select(Ctry,Year,PNum,Catch) %>%
  group_by(Ctry,Year,PNum) %>%
  summarise(Tot=sum(Catch)) %>%
  mutate(Cumulative = cumsum(Tot), Prop=Cumulative/max(Cumulative))
df3$XPoint <- (df3$PNum-1)+0.5

#zeros
#dfZero <- data.frame(Year=seq(1998,2017),PNum=0,Tot=0,Cumulative=0,AnnualTot=0,Norm=0)

#stick it all together
#df2 <- bind_rows(df2,dfZero)
df2$XPoint <- (df2$PNum-1)+0.5

#generate the plots
for (y in names(table(df2$Year))){

  p <- ggplot(filter(df2,Year==y), aes(x=XPoint, y=Norm)) + geom_line(size=1) +
    geom_point(size=2) +
    scale_x_continuous(limits=c(0,12),breaks=seq(0,12),labels=rep("",13)) +
    scale_y_continuous(limits=c(-0.06,1.03)) +
    ggtitle(paste0(y," ",round(max(df2[df2$Year==y,]$Cumulative)),"kt"," (Total Catch ",
                   round(ices.catch[as.character(y)]/1e3),"kt)")) + 
    theme_bw() + labs(x="" ,y="Catch Fraction") +
    annotate("text",
             x=c(seq(0.5,11.5,by=1)),
             y=c(0.0),
             label=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),hjust=0.5,vjust=1) +
    annotate("text",
             x=c(seq(0.5,11.5,by=1)),
             y=c(-0.03),
             label=paste0("(",as.character(round(100*df2[df2$Year==y,]$Prop,0)),"%)"),hjust=0.5,vjust=1) +
    annotate("text",
             x=c(seq(0.5,11.5,by=1)),
             y=c(-0.06),
             label=paste0("(",as.character(100*round(df2[df2$Year==y,]$Norm,2)),"%)"),hjust=0.5,vjust=1)
  
  #countries
  if (nrow(filter(df3,Year==y & Ctry=="NO"))>0){p <- p + geom_line(data=filter(df3,Year==y & Ctry=="NO"),aes(x=XPoint,y=Prop,group=1),linetype=2,size=1,colour="blue")}
  if (nrow(filter(df3,Year==y & Ctry=="IE"))>0){p <- p + geom_line(data=filter(df3,Year==y & Ctry=="IE"),aes(x=XPoint,y=Prop,group=1),linetype=2,size=1,colour="green")}
  if (nrow(filter(df3,Year==y & Ctry=="RU"))>0){p <- p + geom_line(data=filter(df3,Year==y & Ctry=="RU"),aes(x=XPoint,y=Prop,group=1),linetype=2,size=1,colour="red")}
  if (nrow(filter(df3,Year==y & Ctry=="UKS"))>0){p <- p + geom_line(data=filter(df3,Year==y & Ctry=="UKS"),aes(x=XPoint,y=Prop,group=1),linetype=2,size=1,colour="cyan")}
  if (nrow(filter(df3,Year==y & Ctry=="IC"))>0){p <- p + geom_line(data=filter(df3,Year==y & Ctry=="IC"),aes(x=XPoint,y=Prop,group=1),linetype=2,size=1,colour="black")}
  if (nrow(filter(df3,Year==y & Ctry=="FO"))>0){p <- p + geom_line(data=filter(df3,Year==y & Ctry=="FO"),aes(x=XPoint,y=Prop,group=1),linetype=2,size=1,colour="orange")}
  
  if (y %in% IESSNS$Year) {p <- p +
    geom_polygon(data=data.frame(x=c(IESSNS[IESSNS$Year==y,]$start.day,
                                     IESSNS[IESSNS$Year==y,]$end.day,
                                     IESSNS[IESSNS$Year==y,]$end.day,
                                     IESSNS[IESSNS$Year==y,]$start.day),
                                 y=c(0,0,1,1)),mapping=aes(x=x,y=y),alpha=0.2)
  }
  
  p <- p + annotate("text",x=c(1,3,5,7,9,11)+0.1,y=1.03,label=c("NO","IE","RU","UKS","IC","FO"),hjust=0)
  p <- p + annotate("segment",x=0.5,xend=1,y=1.03,yend=1.03,colour="blue",size=1,linetype=2)
  p <- p + annotate("segment",x=2.5,xend=3,y=1.03,yend=1.03,colour="green",size=1,linetype=2)
  p <- p + annotate("segment",x=4.5,xend=5,y=1.03,yend=1.03,colour="red",size=1,linetype=2)
  p <- p + annotate("segment",x=6.5,xend=7,y=1.03,yend=1.03,colour="cyan",size=1,linetype=2)
  p <- p + annotate("segment",x=8.5,xend=9,y=1.03,yend=1.03,colour="black",size=1,linetype=2)
  p <- p + annotate("segment",x=10.5,xend=11,y=1.03,yend=1.03,colour="orange",size=1,linetype=2)
  
  assign(paste0("y",y),p)
  #save individual plots
  ggplot2::ggsave(filename=file.path(out.path,paste0(y,".png")),plot=p)

}

#generate the plots
# for (y in names(table(df2$Year))){
#   
#     p <- ggplot(filter(df2,Year==y), aes(x=PNum, y=Norm)) + geom_line() +
#     scale_x_continuous(limits=c(0,12),breaks=seq(0,12),labels=seq(0,12)) +
#     scale_y_continuous(limits=c(0,1)) +
#     ggtitle(paste0(y," ",round(max(df2[df2$Year==y,]$Cumulative)),"kt"," (Total Catch ",
#                    round(ices.catch[as.character(y)]/1e3),"kt)")) + 
#     theme_bw() + labs(x="Month" ,y="Catch Fraction") 
#     
#     if (y %in% IESSNS$Year) {p <- p +
#       geom_polygon(data=data.frame(x=c(IESSNS[IESSNS$Year==y,]$start.day,
#                                      IESSNS[IESSNS$Year==y,]$end.day,
#                                      IESSNS[IESSNS$Year==y,]$end.day,
#                                      IESSNS[IESSNS$Year==y,]$start.day),
#                                  y=c(0,0,1,1)),mapping=aes(x=x,y=y),alpha=0.2)
#     }
#  
#     #assign(paste0("y",y),p)
#     
#     ggplot2::ggsave(filename=file.path(out.path,paste0(y,".png")),plot=p)
#     
# }

#combine plots into a single pdf document
# pall <- ggarrange(mget(paste0("y",seq(2010,2011))),y2011,y2012,y2013,
#                   y2014,y2015,y2016,y2017,
#                   nrow=2,ncol=2)

#pall <- ggarrange(y1998,y1999,y2000,y2001,y2002,y2003,
#                  y2004,y2005,y2006,y2007,y2008,y2009,
#                  y2010,y2011,y2012,y2013,y2014,y2015,
#                  y2016,y2017,
#                  nrow=2,ncol=2)

#ggexport(pall, filename=file.path(out.path,"CumulativeCatch.pdf")

