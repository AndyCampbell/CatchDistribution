#functions to support plotting of catch by SR

fPlotDist <- function(dfSub, min, max, fill.col, border.col, avg=FALSE){
  
  if (!avg) {
    dfSub <- filter(dfSub, Tot<=max & Tot>min)
  } else {
    dfSub <- filter(dfSub, Avg<=max & Avg>min)
  }
  
  dfSub <- inner_join(dfSub,SR,by=c("SR"="SR"))
  
  for (i in 1:nrow(dfSub)){
    polygon(c(dfSub$West[i],dfSub$East[i],dfSub$East[i],dfSub$West[i],dfSub$West[i]),
            c(dfSub$South[i],dfSub$South[i],dfSub$North[i],dfSub$North[i],dfSub$South[i]),
            col=fill.col, border=border.col)}
}


fPlotBaseMap <- function(xlim=c(-12,8),ylim=c(42,65),xaxis=TRUE,xlabs=TRUE,yaxis=TRUE,
                         ylabs=TRUE,refresh=FALSE,SR=TRUE,ICES=TRUE){
  
  #plots coast, SR boundaries, 
  #ICES area boundaries (ICES=TRUE)
  
  if (!refresh) {
    plot(c(xlim[1],xlim[2],xlim[2],xlim[1]),
         c(ylim[2],ylim[2],ylim[1],ylim[1]),
         xlab="",
         ylab="",
         type="n",
         axes=FALSE)}
  
  if (SR) {
    abline(h=seq(30,80,by=0.5),col="grey")
    abline(v=seq(-40,30,by=1),col="grey")
  }
  
  for (i in 1:length(coast)){
    if (!is.null(coast[[i]])){
      if (coast[[i]]$fill==TRUE) {
        polygon(coast[[i]]$Lon,coast[[i]]$Lat,col="grey")
      } else {
        polygon(coast[[i]]$Lon,coast[[i]]$Lat,col="white")             
      }  
    }
  }
  
  if (ICES) {
    #ices areas
    #IVa
    lines(x=c(8,-4,-4),y=c(62,62,58),col="black")
    lines(x=c(-2,7,7),y=c(57.5,57.5,58),col="black")
    #IVb
    lines(x=c(9,8,8,7),y=c(57,57,57.5,57.5),col="black")
    lines(x=c(8,0),y=c(53.5,53.5),col="black")
    #IVc
    lines(x=c(2,0),y=c(51,51),col="black")
    #VIId
    lines(x=c(-2,-2),y=c(51,49.7),col="black")
    #VIIe
    lines(x=c(-5.2,-7,-7,-5,-5,-4),y=c(50,50,49.5,49.5,48,48),col="black")
    #VIIf
    lines(x=c(-7,-7,-6,-6,-5,-5),y=c(50,50.5,50.5,51,51,51.7),col="black")
    #VIIa
    lines(x=c(-5,-8),y=c(52,52),col="black")
    lines(x=c(-5,-6),y=c(55,55),col="black")
    #VIIg
    lines(x=c(-7,-9,-9),y=c(50,50,52),col="black")
    #VIIh
    lines(x=c(-5,-9,-9,-7),y=c(48,48,50,50),col="black")
    #VIIj
    lines(x=c(-9,-12,-12,-9),y=c(48,48,52.5,52.5),col="black")
    #VIIb
    lines(x=c(-12,-12,-8),y=c(52.5,54.5,54.5),col="black")
    #VIa
    lines(x=c(-12,-12,-5,-5,-4,-4),y=c(54.5,60,60,60.5,60.5,58),col="black")
    #VIb
    lines(x=c(-12,-18,-18,-12),y=c(54.5,54.5,60,60),col="black")
    #VIIc
    lines(x=c(-12,-18,-18,-12),y=c(52.5,52.5,54.5,54.5),col="black")
    #VIIk
    lines(x=c(-12,-18,-18,-12),y=c(48,48,52.5,52.5),col="black")
    #VIIIa
    lines(x=c(-8,-8,-6,-6,-5,-5,-1),y=c(48,47.5,47.5,47,47,46,46),col="black")
    #VIIIb
    lines(x=c(-4,-4,-3,-3,-2,-2),y=c(46,45.5,45.5,44.5,44.5,43),col="black")
    #VIIId
    lines(x=c(-11,-11,-3),y=c(48,44.5,44.5),col="black")
    #VIIIc
    lines(x=c(-11,-11,-9),y=c(44.5,43,43),col="black")
    #VIIIe
    lines(x=c(-18,-18,-11),y=c(48,43,43),col="black")
    #IXa
    lines(x=c(-11,-11,-5.6),y=c(43,36,36),col="black")
    #IXa
    lines(x=c(-11,-18,-18,-11),y=c(43,43,36,36),col="black")
    #X
    lines(x=c(-18,-42,-42,-18),y=c(43,43,36,36),col="black")
    #XII
    lines(x=c(-15,-27,-27,-42,-42,-18,-18,-15,-15),y=c(62,62,59,59,48,48,60,60,62),col="black")
    #XIVa
    lines(x=c(-27,-27,-11,-11),y=c(69,68,68,85),col="black")
    #XIVb
    lines(x=c(-27,-27,-44,-44),y=c(68,59,59,61),col="black")
    #Va
    lines(x=c(-11,-27,-27,-15,-15,-11,-11),y=c(68,68,62,62,63,63,68),col="black")
    #Vb
    lines(x=c(-4,-15,-15,-5,-5,-4,-4),y=c(63,63,60,60,60.5,60.5,63),col="black")
    #IIa
    lines(x=c(6,-4,-4,-11,-11,30,30,26,26),y=c(62,62,63,63,73.5,73.5,72,72,70),col="black")
  }
  
  for (i in 1:length(coast)){
    if (!is.null(coast[[i]])){
      if (coast[[i]]$fill==TRUE) {
        polygon(coast[[i]]$Lon,coast[[i]]$Lat,col="grey")
      } else {
        polygon(coast[[i]]$Lon,coast[[i]]$Lat,col="white")             
      }  
    }
  }
  
  box()
  
  if (xaxis) {axis(side=1,at=seq(xlim[1],xlim[2],by=2),cex.axis=1,labels=xlabs)}
  if (yaxis) {axis(side=2,at=seq(ylim[1],ylim[2],by=2),cex.axis=1,las=2,labels=ylabs)}
  
}


fSubset <- function(src = ".\\Data\\WGCatchBySR.csv", y, ptype, pnum, Cry){
  
  require(dplyr)
  
  #subset the given dataframe for the year,quarter/month specified
  #sum all catches per rectangle
  
  #src - the source data file
  #y - the source year(s)
  #ptype/pnum - the required period type and number

  months <- list("Q1"=c(1,2,3),"Q2"=c(4,5,6),"Q3"=c(7,8,9),"Q4"=c(10,11,12))
  quarters <- c(1,1,1,2,2,2,3,3,3,4,4,4)
  
  #read in the data
  df <- read.table(file = src, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  #select year(s)
  #df <- filter(df, Year == y)
  df <- filter(df, Year %in% y)
  
  #select countries
  if (!(missing(Cry))) {df <- filter(df, Ctry %in% Cry)}
  
  #annual data
  #if pnum is specified then return the summed catches for pnum
  #otherwise, sum the catches over all years in y
  if (toupper(ptype) == "Y"){
    
    if (!(missing(pnum))){
      df <- df %>%
        filter(Year %in% pnum) %>%
        group_by(Year,SR,Lat,Lon) %>%
        summarise(Tot=sum(Catch)) %>%
        select(Year,SR,Lat,Lon,Tot)
    } else {
      df <- df %>%
        group_by(Year,SR,Lat,Lon) %>%
        summarise(SubTot=sum(Catch)) %>%
        group_by(SR,Lat,Lon) %>%
        summarise(Tot=sum(SubTot),Avg=sum(SubTot)/length(y))
    }
    
    
  } else if (toupper(ptype) == "Q") {
    #quarterly

    df <- filter(df, (PType=="Q" & PNum %in% pnum) | (PType=="M" & PNum %in% unlist(months[paste0('Q',pnum)])))

    #data frame to return
    df <- select(group_by(df,Year,SR,Lat,Lon),Year,SR,Lat,Lon,Catch) %>% summarise(Tot=sum(Catch))
    
  } else if (toupper(ptype) == "M") {     #monthly
    
    #monthly data
    dfM <- filter(df, PType=="M" & PNum %in% pnum)
    #cat(nrow(dfM),"monthly records\n")
    #quarterly data 
    dfQ <- filter(df, PType=="Q" & PNum %in% quarters[pnum])
    #cat(nrow(dfQ),"quarterly records\n")
    
    #divide catch by 3
    dfQ <- mutate(dfQ, CatchM = Catch/3)
    #remove original Catch column
    dfQ <- select(dfQ,-Catch)
    
    names(dfQ) <- names(dfM)
    df <- bind_rows(dfM,dfQ)

    #data frame to return
    df <- select(group_by(df,Year,SR,Lat,Lon),Year,SR,Lat,Lon,Catch) %>% summarise(Tot=sum(Catch))
  
  } else if (toupper(ptype) == "W") {     #weekly

    #weekly
    df <- filter(df, PType=="W" & PNum==pnum)

    #data frame to return
    df <- select(group_by(df,Year,SR,Lat,Lon),Year,SR,Lat,Lon,Catch) %>% summarise(Tot=sum(Catch))
    
  }
  
  df
  
}

fSpatialInd <- function(df){
  
  #calculate the centre of gravity of the supplied catches
  #and the inertia (the variance of the location of catches i.e. the mean square distance between a catch
  #and the centre of gravity of the catches
  #input is a data frame that must contain the columns
  #Lat, Lon and Tot
  
  x <- df$Lat
  y <- df$Lon
  z <- df$Tot
  
  #centre of gravity
  xg <- sum(x*z)/sum(z)
  yg <- sum(y*z)/sum(z)
  
  #inertia
  dx <- x - xg
  dy <- y - yg
  d <- sqrt(dx^2 + dy^2)
  i <- sum(z*d^2)/sum(z)
  
  #isotropy,anisotropy
  M11 <- sum(dx^2 * z)
  M22 <- sum(dy^2 * z)
  M21 <- sum(dx*dy*z)
  M12 <- M21
  
  #2x2 matrix for eigenvector analysis
  M <- matrix(c(M11,M12,M21,M22), byrow = T, ncol = 2)
  
  #eigenvectors
  x1 <- eigen(M)$vectors[1, 1]
  y1 <- eigen(M)$vectors[2, 1]
  x2 <- eigen(M)$vectors[1, 2]
  y2 <- eigen(M)$vectors[2, 2]
  r1 <- eigen(M)$values[1]/(eigen(M)$values[1] + eigen(M)$values[2])
  
  #principal axis coordinates
  e1 <- (y1/x1)^2
  sx1 <- x1/abs(x1)
  sy1 <- y1/abs(y1)
  sx2 <- x2/abs(x2)
  sy2 <- y2/abs(y2)
  xa <- xg + sx1 * sqrt((r1 * i)/(1 + e1))
  ya <- yg + sy1 * sqrt((r1 * i)/(1 + (1/e1)))
  xb <- 2 * xg - xa
  yb <- 2 * yg - ya
  xc <- xg + sx2 * sqrt(((1 - r1) * i)/(1 + (1/e1)))
  yc <- yg + sy2 * sqrt(((1 - r1) * i)/(1 + e1))
  xd <- 2 * xg - xc
  yd <- 2 * yg - yc
  Imax <- r1*i 
  Imin <- (1-r1)*i
  Iso <- sqrt(Imin/Imax)
  
  #coordinates of first principal axis of inertia
  xaxe1 <- c(xa, xb)
  yaxe1 <- c(ya, yb)
  
  #coordinates of second principal axis of inertia
  xaxe2 <- c(xc, xd)
  yaxe2 <- c(yc, yd)
  
  #returns a numeric vector of length 2 (Lat and Lon in decimal degrees)
  return(list("CoG" = c(xg,yg),
              "Inertia" = i,
              "Iso" = Iso,
              "Ax1" = c(xaxe1,yaxe1),
              "Ax2" = c(xaxe2,yaxe2)))
  
}


#ggplot map themes
# theme_map = function(base_size=9, base_family="")
# {
#     require(grid)
#     theme_osa(base_size=base_size, base_family=base_family) %+replace%
#     theme(axis.line=element_blank(),
#           axis.text.x=element_blank(),
#           axis.text.y=element_blank(),
#           axis.ticks=element_blank(),
#           axis.title.x=element_blank(),
#           axis.title.y=element_blank(),
#           legend.justification = c(0,0), # bottom of box
#           legend.position      = c(0,0), # bottom of picture
#           panel.background=element_blank(),
#           panel.border=element_rect(colour = "grey90", size = 1, fill = NA),
#           panel.grid.major=element_blank(),
#           panel.grid.minor=element_blank(),
#           panel.margin=unit(0, "lines"),
#           plot.background=element_blank()
#           )
# }

theme_map = function(base_size=9, base_family="")
{
  require(grid)
  theme_osa(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.justification = c(0,0), # bottom of box
          legend.position      = c(0,0), # bottom of picture
          panel.grid = element_line(colour = "black")
    )
}


theme_osa = function(base_size=9, base_family="")
{
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.title.x=element_text(vjust=0),
          axis.title.y=element_text(angle=90, vjust=0.3),
          axis.text=element_text(),
          axis.ticks=element_line(colour="black", size=0.25),
          legend.background=element_rect(fill=NA, colour=NA),
          legend.direction="vertical",
          legend.key=element_rect(fill=NA, colour="white"),
          legend.text=element_text(),
          legend.title=element_text(face="bold", hjust=0),
          panel.border=element_rect(fill=NA, colour="black"),
          panel.grid.major=element_line(colour="grey92", size=0.3, linetype=1),
          panel.grid.minor=element_blank(),
          plot.title=element_text(vjust=1),
          strip.background=element_rect(fill="grey90", colour="black", size=0.3),
          strip.text=element_text()
    )
}


fCatchBin <- function(Catch,Limits,Labels){
  
  for (i in 1:(length(Labels)-1)){if(Catch>Limits[i] & Catch<=Limits[i+1]) return(Labels[i])}
  if(Catch>Limits[length(Limits)]){return(Labels[length(Limits)])}
  return("NA")
  
}