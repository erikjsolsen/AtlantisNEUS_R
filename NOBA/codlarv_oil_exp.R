#' @title Creating cod larvae pollution exposuer forcing file 
#' @description Creates .NETCDF forcing file of cod-larvae exposed to oil above lethal threshold scaling for use in pollution module of the NOBA Atlantis model (trunk mode)
#' @details Not working the way I intended. Have abandoned this approach as I'll be using pubilshed mortality data to parameterize the Atlantis oil-spill pollution module
#' @author Erik Olsen
#'  
#'  -----------------
#'  Libraries
library(roxygen2)
library(sp)
library(rgdal)
library(R.matlab)
library(data.table)

#' ----------------------------
#' Global constants
#' 
#' Lethal PAH threshold concentration for fish larvae 
PAHthresh <- c(1) 

#' ----------------------------
#' Global functions

AboveThresh <- function(x)
{
  length(subset(x, x>PAHthresh))
}

#' ----------------------------
#' Importing NOBA shape file
setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/nordic_grid_220812")
NOBAsp <- readOGR(dsn = ".", "MENUIIareasPolNewId_grass_tol0p01")
setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/spatial")

slotNames(NOBAsp) # look at the slotnames
names(NOBAsp)
str(NOBAsp, max.level=3)

#changes projection to LAT LON using projection information gathered from the 'slotNames' command above
NOBAsp2 <- spTransform(NOBAsp, CRS("+proj=longlat +ellps=GRS80"))

#' -------------------
#' import .MAT files of LON-LAT larvae postions and Exposure at each time-step
setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/OilSpill/Larvae")
Exp<-readMat("Exp.mat")
LonLat<-readMat("LONLAT.mat")

#' -------------------
#' Create a data frame of Atlantis boxes (60 rows), and days (90 columns)
BoxExp<-as.data.frame(matrix(nrow=60, ncol=90), row.names=c(0:59))
BoxExp.dt<-data.table(BoxExp)

Day<-as.data.frame(matrix(nrow=94500, ncol=3), row.names=c(0:59))
colnames(Day)<-c("LON", "LAT", "Exp")

for (i in 1:length(BoxExp)){
  Day$LON<-LonLat$LONcod[,i]
  Day$LAT<-LonLat$LATcod[,i]
  Day$Exp<-Exp$Exp[,i]
  
  Day.sp<-SpatialPointsDataFrame(Day[1:2], Day[3])
  proj4string(Day.sp) <- CRS("+proj=longlat +ellps=GRS80") 
  poly.proj <- proj4string(NOBAsp2) 
  Day.sp <- spTransform(Day.sp,CRS(poly.proj)) 
  
  # assigns box number to each larvae position
  box.values <- over(Day.sp, NOBAsp2)
  box.values<-cbind(Day, box.values$nyId)
  
  colnames(box.values)<-c("LON", "LAT", "Exp", "BoxId")
  box.values<-subset(box.values, BoxId != "NA")
  box.values[is.na(box.values)] <- 0

  box.values.dt<-data.table(box.values[,3:4])
  paffect<-box.values.dt[,list(AboveThresh=AboveThresh(Exp)/length(Exp)), by=BoxId]
  setkey(paffect, BoxId)
  
  #box.no<-over(NOBAsp2, Day.sp, fn=length) #number of larvae pr box
  #box.exp<-over(NOBAsp2, Day.sp, fn=AboveThresh) #no exposed (above threshold) pr box
  #box.values[is.na(box.values)] <- 0
  #box.frac<-box.exp/box.no
  #BoxExp[,i]<-box.frac
}

BoxExp.dt[BoxExp.dt$V1==6,]
