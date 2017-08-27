#Reporting of data availability 
#Proportion of WG catch reported by SR
#Analysis of fleets

#Oct 18th 2016 - catch in NEAFC area

rm(list=ls())
gc()

library(dplyr)
source("SRAnalyFuncs.R")

#WG Catch (taken from WGWIDE2016 report)

WGCatch <- c(666664,640311,738608,737462,772905,679288,660491,549514,481181,586206,623165,737969,875515,946661,894684,933165,1394454,1208990)
names(WGCatch) <- seq(1998,2015)

#load data
df <- tbl_df(read.table(file = ".\\Data\\WGCatchBySR.csv", header = TRUE, sep = ","))

#total catch reported by year
Annual <- 
  df %>%
  group_by(Year) %>%
  summarise(Tot = sum(Catch)) %>%
  mutate(WGCatch = WGCatch) %>%
  mutate(Prop = 100*Tot/WGCatch)

View(Annual)


#NEAFC catches
#stat rectangles within the NEAFC zone
## 2.a1, 2.b1, 14b1 all rectangles

SR_NEAFC <- c("47B3","47B4", "47B5","47B6", "47B7", "47B8", "47B9", "47C0", "47C1", "47C2",
            "48B4","48B5", "48B6","48B7", "48B8", "48B9", "48C0", "48C1", "48C2",
            "49B4","49B5", "49B6","49B7","49B8", "49B9", "49C0", "49C1", "49C2",
            "50B5","50B6", "50B7", "50B8", "50B9", "50C0", "50C1", "50C2",
            "51B5","51B6","51B7", "51B8", "51B9", "51C0", "51C1",
            "52B6","52B7", "52B8", "52B9", "52C0",
            "53B6","53B7", "53B8","53B9","54B8","54B9",
            "58E9",
            "59E7","59E8", "59E9","59F0",
            "60E4","60E5", "60E6", "60E7", "60E8", "60E9", "60F0","60F1",
            "61E4","61E5", "61E6", "61E7", "61E8","61E9", "61F0", "61F1", "61F2",
            "62E4","62E5", "62E6", "62E7", "62E8", "62E9", "62F0", "62F1", "62F2",
            "63E4","63E5", "63E6", "63E7", "63E8", "63E9", "63F0", "63F1", "63F2",
            "64E5","64E6", "64E7", "64E8", "64E9", "64F0", "64F1", "64F2",
            "65E7","65E8", "65E9", "65F0", "65F1", "65F2",
            "66E9","66F0", "66F1", "66F2", "66F3",
            "67F0", "67F1", "67F2", "67F3",
            "68F1","68F2","68F3", "68F4",
            "69F2","69F3", "69F4","69F5",
            "70F2","70F3", "70F4","70F5", "70F6",
            "71F2","71F3", "71F4","71F5","71F6", "71F7",
            "72F2","72F3", "72F4","72F5", "72F6", "72F7", "72F8",
            "73F2","73F3", "73F4", "73F5", "73F6", "73F7","73F8", "73F9",
            "74F1","74F2", "74F3", "74F4", "74F5", "74F6", "74F7", "74F8",
            "75F0","75F1", "75F2", "75F3","75F4", "75F5", "75F6", "75F7",
            "76E9","76F0","76F1", "76F2", "76F3", "76F4", "76F5", "76F6",
            "77E5","77E6", "77E7", "77E8", "77E9", "77F0", "77F1","77F2", "77F3", "77F4", "77F5",
            "78E6","78E7", "78E8", "78E9", "78F0", "78F1", "78F2", "78F3", "78F4",
            "79E6","79E7", "79E8", "79E9", "79F0", "79F1", "79F2",
            "80E6","80E7", "80E8", "80E9", "80F0",
            "81E6","81E7", "81E8", "81E9")



#catches in NEAFC
for (y in seq(2012,2015)){
  AnnTot <- 0
  AnnNEAFCTot <- 0
  for (q in 1:4){
    dfSR <- fSubset(y = y, ptype = "Q", pnum = q)
    AnnTot <- AnnTot + sum(dfSR$Tot)
    AnnNEAFCTot <- AnnNEAFCTot + sum(subset(dfSR,SR%in%SR_NEAFC)$Tot)
    cat(y,q,sum(dfSR$Tot),sum(subset(dfSR,SR%in%SR_NEAFC)$Tot),"\n")
  }
  #annual
  cat(y,AnnTot,AnnNEAFCTot,AnnNEAFCTot/AnnTot,"\n")
}





### RECTANGLES GREATER THAN 50% IN THE NEAFC AREA


#NEAFC catches
#stat rectangles within the NEAFC zone (eyeball exercise on those straddling, more than
#half the rectangle in the zone results in inclusion)
SR_NEAFC <- c( "58E9",
               "59E7","59E8","59E9","59F0",
               "60E5","60E6","60E7","60E8","60E9","60F0","60F1",
               "61E4","61E5","61E6","61E7","61E8","61E9","61F0","61F1","61F2",
               "62E4","62E5","62E6","62E7","62E8","62E9","62F0","62F1","62F2",
               "63E4","63E5","63E6","63E7","63E8","63E9","63F0","63F1","63F2",
               "64E5","64E6","64E7","64E8","64E9","64F0","64F1","64F2",
               "65E7","65E8","65E9","65F0","65F1","65F2",
               "66E9","66F0","66F1","66F2","66F3",
               "67F0","67F1","67F2","67F3",
               "68F1","68F2","68F3","68F4",
               "69F2","69F3","68F4","69F5",
               "70F2","70F3","70F4","70F5","70F6",
               "71F2","71F3","71F4","71F5","71F6","71F7",
               "72F2","72F3","72F4","72F5","72F6","72F7","72F8",
               "73F2","73F3","73F4","73F5","73F6","73F7","73F8","73F9",
               "74F1","74F2","74F3","74F4","74F5","74F6","74F7","74F8",
               "75F0","75F1","75F2","75F3","75F4","75F5","75F6","75F7",
               "76E9","76F0","76F1","76F2","76F3","76F4","76F5","76F6",
               "77E6","77E7","77E8","77E9","77F0","77F1","77F2","77F3","77F4","77F5",
               "78E5","78E6","78E7","78E8","78E9","78F0","78F1","78F2","78F3","78F4",
               "79E6","79E7","79E8","79E9","79F0","79F1","79F2",
               "80E6","80E7","80E8","80E9","80F0","80F1",
               "81E6","81E7","81E8",
               "82E7")

#catches in NEAFC
for (y in seq(2012,2015)){
  AnnTot <- 0
  AnnNEAFCTot <- 0
  for (q in 1:4){
    dfSR <- fSubset(y = y, ptype = "Q", pnum = q)
    AnnTot <- AnnTot + sum(dfSR$Tot)
    AnnNEAFCTot <- AnnNEAFCTot + sum(subset(dfSR,SR%in%SR_NEAFC)$Tot)
    cat(y,q,sum(dfSR$Tot),sum(subset(dfSR,SR%in%SR_NEAFC)$Tot),"\n")
  }
  #annual
  cat(y,AnnTot,AnnNEAFCTot,AnnNEAFCTot/AnnTot,"\n")
}




