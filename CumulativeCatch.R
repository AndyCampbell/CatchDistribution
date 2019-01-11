#WKIBPNEAMac

#cumulative catch
#12th December 2018
#Cab Inn, Copenhagen


rm(list=ls())
gc()

library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)

source("SRAnalyFuncs.R")

ices.catch <- c(666664,640311,738608,737462,772905,679288,660491,549514,481181,
                586206,623165,737969,875515,946661,894684,933165,1394454,1208990,
                1094066,1155944)

names(ices.catch) <- as.character(seq(1998,2017))

#IESSNS timings
IESSNS <- data.frame(Year=c(2010,seq(2012,2018)),
                     start=c(as.Date("2010/07/08","%Y/%m/%d"),as.Date("2012/07/02","%Y/%m/%d"),
                             as.Date("2013/07/02","%Y/%m/%d"),as.Date("2014/07/02","%Y/%m/%d"),
                             as.Date("2015/07/01","%Y/%m/%d"),as.Date("2016/07/01","%Y/%m/%d"),
                             as.Date("2017/07/03","%Y/%m/%d"),as.Date("2018/06/30","%Y/%m/%d")),
                     end=c(as.Date("2010/08/21","%Y/%m/%d"),as.Date("2012/08/10","%Y/%m/%d"),
                           as.Date("2013/08/09","%Y/%m/%d"),as.Date("2014/08/12","%Y/%m/%d"),
                           as.Date("2015/08/10","%Y/%m/%d"),as.Date("2016/08/31","%Y/%m/%d"),
                           as.Date("2017/08/04","%Y/%m/%d"),as.Date("2018/08/06","%Y/%m/%d")))

IESSNS <- dplyr::mutate(IESSNS,
                        start.day = 12*lubridate::yday(start)/365,
                        end.day = 12*lubridate::yday(end)/365)

#read in the data
df1 <- read.table(file = paste0(".\\Data\\WGCatchBySR.csv"), header = TRUE, 
                  sep = ",", stringsAsFactors = FALSE)

#period types for the data
xtabs(~PType + Year, data=df1)

#quarterly records by year/country - relatively sparse back to 2006
xtabs(~Ctry + Year, data = filter(df1,PType=="Q"))

#quarterly->monthly
dfQ <- dplyr::filter(df1,PType=="Q") %>%
  group_by(Year,Ctry,PNum) %>%
  mutate(Per1 = 3*(PNum-1)+1, Per2 = Per1+1, Per3 = Per2+1, MPType="M", MCatch = Catch/3) %>%
  ungroup() %>%
  select(Ctry,Year,SR,Lat,Lon,PType=MPType,Per1,Per2,Per3,Catch=MCatch) %>%
  gather(key="key",value="PNum",Per1,Per2,Per3) %>%
  select(Ctry,Year,SR,Lat,Lon,PType,PNum,Catch)

df1 <- bind_rows(df1,dfQ)

#monthly
df2 <- dplyr::filter(df1,PType=="M") %>%
  select(Year,PNum,Catch) %>%
  group_by(Year,PNum) %>%
  summarise(Tot=sum(Catch)) %>%
  mutate(Cumulative = cumsum(Tot)/1e3,AnnualTot=max(Cumulative),Norm=Cumulative/AnnualTot)

dfZero <- data.frame(Year=seq(1998,2017),PNum=0,Tot=0,Cumulative=0,AnnualTot=0,Norm=0)

df2 <- bind_rows(df2,dfZero)

for (y in names(table(df2$Year))){
#for (y in seq(2010,2017)){
    p <- ggplot(filter(df2,Year==y), aes(x=PNum, y=Norm)) + geom_line() +
    scale_x_continuous(limits=c(0,12),breaks=seq(0,12),labels=seq(0,12)) +
    scale_y_continuous(limits=c(0,1)) +
    ggtitle(paste0(y," ",round(max(df2[df2$Year==y,]$Cumulative)),"kt"," (Total Catch ",
                   round(ices.catch[as.character(y)]/1e3),"kt)")) + 
    theme_bw() + labs(x="Month" ,y="Catch Fraction") 
    
    if (y %in% IESSNS$Year) {p <- p +
      geom_polygon(data=data.frame(x=c(IESSNS[IESSNS$Year==y,]$start.day,
                                     IESSNS[IESSNS$Year==y,]$end.day,
                                     IESSNS[IESSNS$Year==y,]$end.day,
                                     IESSNS[IESSNS$Year==y,]$start.day),
                                 y=c(0,0,1,1)),mapping=aes(x=x,y=y),alpha=0.2)
    }
  
  assign(paste0("y",y),p)
  
}

#combine plots into a single pdf document
# pall <- ggarrange(mget(paste0("y",seq(2010,2011))),y2011,y2012,y2013,
#                   y2014,y2015,y2016,y2017,
#                   nrow=2,ncol=2)

pall <- ggarrange(y1998,y1999,y2000,y2001,y2002,y2003,
                  y2004,y2005,y2006,y2007,y2008,y2009,
                  y2010,y2011,y2012,y2013,y2014,y2015,
                  y2016,y2017,
                  nrow=2,ncol=2)

ggexport(pall, filename="CumulativeCatch.pdf")

