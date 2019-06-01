### Mapping NOBA Atlantis Geometry
# By: Erik Olsen
# Created: 9.12.2014
# Updated: 29.05.2019

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
library("maptools")
library("mapproj")
library("dplyr")

###import NOBA shape-file instead of the BGM file

setwd("~/ownCloud/Research/atlantis/NOBA/spatial/nordic_grid_220812")
#<<<<<<< Updated upstream
dsn<-getwd()
ogrInfo(dsn=dsn,layer="MENUIIareasPolNewId_grass_tol0p01")
NOBAsp <- readShapePoly("MENUIIareasPolNewId_grass_tol0p01.shp", proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"),verbose=TRUE)
#states=readShapePoly("/home/mithil/java/R/statesp020.shp",proj4string=crswgs84,verbose=TRUE)
#setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/spatial")


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
NOBAmap1 <- ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray65", fill="gray65") +  coord_map(projection="lambert", parameters = c(0, 65), xlim = c(-27, 70), ylim=c(58, 85))
NOBAmap1


NOBAmap1 <- NOBAmap1 + geom_polygon(data=NOBA.f, aes(x=long, y=lat, group=group),colour="slategray", fill=NA, label=id) +theme_bw() + ggtitle("Norwegian - Barents Sea ATLANTIS model area") + theme(plot.title = element_text(size=16, face="bold")) 

NOBAmap1
#ggsave("NOBA Atlantis map wo no.pdf", scale = 1, dpi = 400)

NOBAmap2 <- NOBAmap1 + geom_text(data=cnames, aes(x=long, y=lat, group=id, label = id), size=3) 
NOBAmap2
ggsave("NOBA Atlantis map.pdf", scale = 1, dpi = 400)
ggsave("NOBA Atlantis map.png", scale = 1, dpi = 400)


# Survey coverage in relation to NOBA Model
#------------------

# load survey files & extract position data
w2016 <- read.csv2("../surveys/vin2016_s.csv", dec=".")
w2018 <- read.csv2("../surveys/vin2018_s.csv", dec=".")
w2019 <- read.csv2("../surveys/vin2019_s.csv", dec=".", sep=",")

w_pos <- rbind(w2016[,c(1,10,11)], w2018[,c(1,10,11)], w2019[,c(1,10,11)]) # winter survey positions



# Plot survey point on NOBA map
wintermap <- ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray65", fill="gray65") +  coord_map(projection="lambert", parameters = c(0, 65), xlim = c(-27, 70), ylim=c(58, 85)) + geom_point(data=w_pos, aes(x=lon, y=lat, group=id)) + geom_polygon(data=NOBA.f, aes(x=long, y=lat, group=group),colour="slategray", fill=NA, label=id) + ggtitle("Winter survey 2016, 2018 & 2019 in NOBA model") + geom_text(data=cnames, aes(x=long, y=lat, group=id, label = id), size=3, colour="red") 
ggsave("../surveys/wintersurvey_NOBA.png", scale=1, dpi=400)


spawnmap <- NOBAmap2 + geom_point() #Lofoten Spawning survey data
summermap <- NOBAmap2 + geom_point() #summer ecosystem survey



# Count number of stations in each polygon user ´over´function
# winter survey
coords = cbind(w_pos$lon, w_pos$lat)
colnames(coords) <- c("long", "lat")
w_sp = SpatialPoints(coords, proj4string=CRS("+proj=longlat +ellps=GRS80"))

res <- over(w_sp, NOBAsp2) #p=points as SpatialPoints, x=SpatialPolygonDataframe
#table(res$NAME_1) # count points

w_tbl <- count(res, box_id)

write.csv2(w_tbl, file="../surveys/winter_NOBA.csv")




############
#-----------------------------------------------------------
### mapping with google-earth
#Not working - only generates a square map...
nn<-as.matrix(NOBA.f[,1:2])
bb<-bbox(nn)

NOBAGooglM1 <- ggmap(get_map(location = bb), maptype="terrain")
