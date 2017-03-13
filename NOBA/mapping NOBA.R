### Mapping NOBA Atlantis Geometry
# By: Erik Olsen
# Created: 9.12.2014
# Updated: 

library(lattice) #load lattice library
library(RColorBrewer)
library(plyr)
library(ggplot2)
library("sp")
library("geosphere")
library(rgdal)
library(rgeos)
library("maps")
library("ggmap")
library("RgoogleMaps")
library("maptools", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

###import NOBA shape-file instead of the BGM file

setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/nordic_grid_220812")
NOBAsp <- readShapePoly("MENUIIareasPolNewId_grass_tol0p01.shp", ,proj4string=crswgs84,verbose=TRUE)
states=readShapePoly("/home/mithil/java/R/statesp020.shp",proj4string=crswgs84,verbose=TRUE)
setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/spatial")

slotNames(NOBAsp) # look at the slotnames
names(NOBAsp)
str(NOBAsp, max.level=3)

#changes projection to LAT LON using projection information gathered from the 'slotNames' command above
NOBAsp2 <- spTransform(NOBAsp, CRS("+proj=longlat +ellps=GRS80"))


### Create NOBA area map with numbers for each region
NOBA.f<-fortify(NOBAsp2, region="nyId") #creates X - Y points of the polygons

cnames <- aggregate(cbind(long, lat) ~ id, data=NOBA.f, FUN=function(x)mean(range(x)))

#alternative way of getting area numbers & midpoints
cnames2<-as.data.frame(cbind(NOBAsp2@data$XcentrLon, NOBAsp2@data$YcentrLat, NOBAsp2@data$box_id))
colnames(cnames2)<-c("LON", "LAT", "BoxNo")

# use world map as background
world<- map_data("world") 
NOBAmap1 <- ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray65", fill="gray65") +  coord_cartesian(xlim = c(-27, 70), ylim=c(58, 85))
NOBAmap1

NOBAmap1 <- NOBAmap1 + geom_polygon(data=NOBA.f, aes(x=long, y=lat, group=group),colour="slategray", fill="slategray1", label=id) +theme_bw() + ggtitle("Norwegian - Barents Sea ATLANTIS model area") + theme(plot.title = element_text(size=16, face="bold")) 

NOBAmap1
ggsave("NOBA Atlantis map wo no.pdf", scale = 1, dpi = 400)

NOBAmap1 + geom_text(data=cnames, aes(x=long, y=lat, group=id, label = id), size=4) 



ggsave("NOBA Atlantis map.pdf", scale = 1, dpi = 400)
ggsave("NOBA Atlantis map.png", scale = 1, dpi = 400)

#test map with new numbering
#NOBAmap2 <- ggplot(NOBA.f, aes(x=long, y=lat, group=id)) + geom_polygon(data=NOBA.f, aes(x=long, y=lat, group=group),colour="tomato3", fill="lightskyblue1", label=id) + geom_text(data=cnames2, aes(x=LON, y=LAT, group=BoxNo, label = BoxNo), size=4)
#NOBAmap2 + geom_point(data=OilPoints, aes(x=LON, y=LAT))
#NOBAmap2

#-----------------------------------------------------------
### mapping with google-earth
#Not working - only generates a square map...
nn<-as.matrix(NOBA.f[,1:2])
bb<-bbox(nn)

NOBAGooglM1 <- ggmap(get_map(location = bb))
