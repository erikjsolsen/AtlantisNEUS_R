### NOBA Atlantis FISHERIES HARBOURS 
# analysis of Norwegian fisheries harbours
# By: Erik Olsen
# Created: 4.11.2014
# Updated: 4.11.2014

library(lattice) #load lattice library
library(RColorBrewer)
library(plyr)
library(ggplot2)

setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/fiskeri")

harbours<-read.csv("fishing by kommune 2013_utf.csv", row.names=1)

## Barplot of harbours by landings
harbours.bar<-subset(harbours, Landings>1000) 

sort.harbours<-harbours.bar[order(harbours.bar$Landings,harbours.bar$Vessels,-harbours.bar$Fishers),]


harbours.names<-rownames(sort.harbours)
op <- par(oma=c(5,7,1,1))
par(mar = rep(2, 4))
barplot(sort.harbours$Landings, names.arg=harbours.names, horiz=TRUE, col="lightskyblue2", cex.names=0.6, las=1, main="Landings(t) pr. county (kommune) > 1000t annual landing")
