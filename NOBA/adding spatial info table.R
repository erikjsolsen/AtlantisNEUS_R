#' @title Adding polygon properties to a table of points. 
#' @details TESTING Stage
#' @description Must run 'mapping NOBA.R' script first to get NOBA spatial geometry and the 'NOBAsp2' var
#' @inheritParams 'NOBAsp2'
#' @
library(roxygen2)
#'
#' @author Erik Olsen

#' --------------------------
#' global libraries
library(sp)

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
box.values <- over(NOBAsp2, nobapoints.sp2, fn=mean) 
box.values[is.na(box.values)] <- 0

