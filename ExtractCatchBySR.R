#CatchBySR analysis and data compilation
#27/01/2015
#updates 05/03/2015 (moved scripts, reorganised data sources)
#new approach now that monthly data has been entered into the excel spreadsheets.
#each year (xxxx) is contained in it's own folder xxxx_Data within the StockCoordination Data folder
#if within this folder there is a subfolder 'Finalxls' then the excel sheets will be in here
#there will be a further subfolder called 'Latest' which contains the most up to date data.
#this should match that contained in another subfolder which will be named according to when the latest changes were 
#made (usually coincident with a WG)

#this script will examine folder structure to
#1 - determine if a xlsx file exists
#2 - if so load it
#3 - if there is monthly data then load it. This will be on worksheet called 'Area-WG'
#4 - if there is no monthly data but quarterly data then load it. This will be on worksheet called 'QArea-WG'
#5 - if there is no catch by SR then proceed to the next file

#Statistical rectangle data will be taken from the master list (StatRectMaster.xlsx) in the RData folder
#data is written to a csv file in the Data subfolder in the current directory containing the following columns
#Year,Period,Country,SR,Lat,Lon,Catch
#with Lat and Lon the midpoints of the stat rectangle

#clean up
rm(list=ls())
gc()

library(RODBC)
options(java.parameters = "-Xmx1024m")
library(XLConnect)
library(dplyr)
library(tidyr)

processFile <- function(f,srcpath,tgtpath){
  
  cat(f,"\n")
  
  #load the workbook
  wb <- loadWorkbook(paste0(srcpath,"\\",f), create=FALSE)
  
  #check if a worksheet exists for monthly catch by SR data
  if (existsSheet(wb,"Area-WG")){
    #monthly sheet exists
    cat("Monthly data\n")
    #read in data
    dfSR <- tbl_df(readWorksheet(wb, sheet = "Area-WG", startRow = 21, 
                                 startCol = 1, endCol = 13, header = TRUE, 
                                 colTypes = c("character",rep("numeric",12))))
                   
    #check if there is any data returned
    #cat(nrow(dfSR),"\n")
    #annual totals
    dfSR[is.na(dfSR)]<-0
    dfSR <- mutate(dfSR,Tot = Jan + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec)  
    #subset the data to those rows only containing data    
    dfSR <- filter(dfSR,Tot>0)

    #check for missing rectangles in the master list
    dfMissing <- anti_join(dfSR,select(dfMasterSR,RECT,Latitude,Longitude),by="RECT")
    if (nrow(dfMissing)>0) {
      if (!(nrow(dfMissing)==1 & toupper(dfMissing$RECT) %in% c("UKN","UNKNOWN","UNALLOCATED"))){
        cat(nrow(dfMissing)," SR missing from master list! (check dfMissing)\n")
        cat(dfMissing$RECT,"\n")
        #browser()
      }
    }
    
    #join to master SR data for midpoint lat, lon and ICES division
    dfSR <- inner_join(dfSR,select(dfMasterSR,RECT,Latitude,Longitude),by="RECT")
    
    if (nrow(dfSR)>0) {
      cat(sum(dfSR$Tot),"t reported\n")
      #add country code (retrieved from filename), year and period type
      dfSR <- mutate(dfSR, ctry = substring(f,1,nchar(f)-10), year = substring(f,nchar(f)-8,nchar(f)-5), ptype = "M")
      #set column order for output
      dfSR <- dfSR[,c("ctry","year","RECT","Latitude","Longitude","ptype","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")]
      #rename some columns (so that the gather function below uses the correct values for the months i.e. 1 instead of Jan)
      colnames(dfSR) <- c("ctry","year","SR","Lat","Lon","ptype","1","2","3","4","5","6","7","8","9","10","11","12")
      #split the columns for quarters into separate rows
      dfSR <- gather(dfSR,"pnum","Catch",7:18)
      write.table(filter(dfSR,Catch>0), file = paste0(tgtpath, "WGCatchBySR.csv"), append = TRUE, quote = FALSE,
                  sep = ",", row.names = FALSE, col.names = FALSE)
    }
    
    
    
  } else if (existsSheet(wb,"QArea-WG")) {
    
    #quarterly sheet exists
    cat("Quarterly data\n")
    #read in data
    dfSR <- tbl_df(readWorksheet(wb, sheet = "QArea-WG", startRow = 17, 
                                 startCol = 1, endCol = 5, header = TRUE, 
                                 colTypes = c("character",rep("numeric",4))))
    
    #check if there is any data returned
    #cat(nrow(dfSR),"\n")
    #annual totals
    dfSR[is.na(dfSR)]<-0
    dfSR <- mutate(dfSR,Tot = Quarter.1 + Quarter.2 + Quarter.3 + Quarter.4)  
    #subset the data to those rows only containing data    
    dfSR <- filter(dfSR,Tot>0)

    #check for missing rectangles in the master list
    dfMissing <- anti_join(dfSR,select(dfMasterSR,RECT,Latitude,Longitude),by="RECT")
    if (nrow(dfMissing)>0) {
      if (!(nrow(dfMissing)==1 & toupper(dfMissing$RECT) %in% c("UKN","UNKNOWN","UNALLOCATED"))){
        cat(nrow(dfMissing)," SR missing from master list!\n")
        browser()
      }
    }
    
    #join to master SR data for midpoint lat, lon and ICES division
    dfSR <- inner_join(dfSR,select(dfMasterSR,RECT,Latitude,Longitude),by="RECT")
    
    if (nrow(dfSR)>0) {
      cat(sum(dfSR$Tot),"t reported\n")
      #add country code (retrieved from filename), year and period type
      dfSR <- mutate(dfSR, ctry = substring(f,1,nchar(f)-10), year = substring(f,nchar(f)-8,nchar(f)-5), ptype = "Q")
      #set column order for output
      dfSR <- dfSR[,c("ctry","year","RECT","Latitude","Longitude","ptype","Quarter.1","Quarter.2","Quarter.3","Quarter.4")]
      #rename some columns (so that the gather function below uses the correct values for the quarters i.e. 1 instead of Quarter.1)
      colnames(dfSR) <- c("ctry","Year","SR","Lat","Lon","ptype","1","2","3","4")
      #split the columns for quarters into separate rows
      dfSR <- gather(dfSR,"pnum","Catch",7:10)
      write.table(filter(dfSR,Catch>0), file = paste0(tgtpath,"WGCatchBySR.csv"), append = TRUE, quote = FALSE,
                  sep = ",", row.names = FALSE, col.names = FALSE)
    }
    
  } else {
    #no SR data sheet exists
    cat("No SR data\n")
  }
  
  rm(wb)
  gc()
  
}

#SR master data
wb <- loadWorkbook(".//..//Data//RData//StatRectMaster.xlsx", create=FALSE)

dfMasterSR <- readWorksheet(wb, sheet = "Sheet1", startRow = 1, startCol = 1,
                            endCol = 16, header = TRUE, 
                            colTypes = c("character", rep("numeric",14), "character"))

rm(wb)
gc()

#output
write(c("Ctry","Year","SR","Lat","Lon","PType","PNum","Catch"), file = ".\\Data\\WGCatchBySR.csv", sep = ",", ncolumns = 8)

#year files
yfiles <- list.files(path=".\\..\\Data\\",pattern="_Data")

for (y in yfiles) {
    
  cat(y,"\n")
  
  if (file.exists(paste0(".\\..\\Data\\",y,"\\Finalxls\\Latest"))) {
    
    #list of data files
    dfiles <- list.files(path=paste0(".\\..\\Data\\",y,"\\Finalxls\\Latest"),pattern=".xlsx")
    #browser()
    #ignore temp files (those starting with ~)
    if (any(grep(pattern="~",dfiles))) dfiles <- dfiles[-grep(pattern="~",dfiles)]

    if (length(dfiles)>0) {
      lapply(dfiles,processFile,
             srcpath = paste0(".\\..\\Data\\",y,"\\Finalxls\\Latest"),
             tgtpath = ".\\Data\\")  
    }
    
  }
  
}
