#' @title Adding polygon properties to a table of points. 
#' @details TESTING Stage
#' @description 
#' @author Erik Olsen

#' --------------------------
#' global libraries
library(roxygen2)
library(sp)
library(rgdal)

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

#' ----------------------------
#' importing points file
setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/spatial")
nobapoints<-as.data.frame(read.csv("test_points_NOBA.csv"))
nobapoints.sp<-SpatialPointsDataFrame(nobapoints[1:2], nobapoints[3])
proj4string(nobapoints.sp) <- CRS("+proj=longlat +ellps=GRS80") 
poly.proj <- proj4string(NOBAsp2) 
nobapoints.sp2 <- spTransform(nobapoints.sp,CRS(poly.proj)) 

#' ---------------------------
#' creating table of box numbers with mean value (e.g. pollution level) pr box
#' 
box.values1 <- over(NOBAsp2, nobapoints.sp2, fn=mean) 
box.values1[is.na(box.values1)] <- 0

