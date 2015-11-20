# Mapping NEUS model area and component info on a map of US states
# By: Erik Olsen
# Date: 21.01.2014

# example


library(rgdal)
library(ggplot2)
library(rgeos)
library(maps)
library(ggmap)
library(RgoogleMaps)
library(mapdata)
library(RColorBrewer)
library("maptools", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")


<<<<<<< HEAD
setwd("~/Documents/Research/Atlantis NEUS Spatial/NEUS Shape") #directory with NEUS shape files
=======
#setwd("~/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS Shape") #directory with NEUS shape files
setwd("/Volumes/My Passport/Atlantis/Atlantis NEUS/NEUS Shape")
>>>>>>> origin/master

NEUSarea <- readOGR(dsn = ".", "neus30_2006v2") #import shapefile 
# 12. jan 2014 - the original shape file had errors in the area numbering. Fixed the numbering manually in Manifold and exported the shapefile again
slotNames(NEUSarea) # look at the slotnames
names(NEUSarea)
str(NEUSarea, max.level=3)

#changes projection to LAT LON using projection information gathered from the 'slotNames' command above
NEUSarea@proj4string
NEUSarea<-spTransform(NEUSarea, CRS("+proj=longlat +ellps=GRS80")) #transform to LAT and LONG coordinates




# give correct box numbers
NEUSarea$AAREA

AreaNames<-c(25,16, 0, 15,8, 1, 18, 19, 23, 2, 10, 24, 3, 29, 22, 5, 17, 12, 11, 27, 21, 4, 20, 28, 7, 14, 6, 26, 9, 13)
NEUSarea$AAREA<-AreaNames

# p <- ggplot(NEUSarea@data)

# project colours
# blues: "#A6CEE3" "#1F78B4" 
# greens: "#B2DF8A" "#33A02C"  
# oranges: "#FDBF6F" "#FF7F00" 

## Plotting the Atlantis geometry

NEUS.f<-fortify(NEUSarea, region="AAREA") #creates X - Y points of the polygons

cnames <- aggregate(cbind(long, lat) ~ id, data=NEUS.f, FUN=function(x)mean(range(x)))


Map <- ggplot(NEUS.f, aes(long, lat, group = group, fill =id, label=id)) + geom_polygon(fill="gray80", colour="white") +   coord_equal() + labs(x = "Longitude", y = "Latitude") +   theme_bw() + ggtitle("Northeast US (NEUS) Atlantis model area") + theme(legend.title = element_text(size=14, face="bold")) + theme(plot.title = element_text(size = rel(1.5), face = "bold"))+ theme( axis.text.x = element_text(hjust = 0, colour = "grey20", size=14))+ theme( axis.text.y = element_text(colour = "grey20", size=14)) + theme(axis.title.x = element_text(size=14, face="bold")) + theme(axis.title.y = element_text(size=14, face="bold")) + geom_text(data=cnames, aes(x=long, y=lat, group=id, label = id), size=4)

Map # plots NEUS map area



## Adding world map

all_states <- map_data("state") # get US State dataset from maps library
all_states$id<-0 # add an ID column so that NEUS.f and all_states can be compared. 
world<-map_data("worldHires")
world$id<-0
NAm<-rbind(subset(world, region=Canada), subset(world, region=USA))

#Map <- Map + geom_polygon( data=all_states, aes(x=long, y=lat, group=group),colour="white", fill="grey52" )  + coord_cartesian(xlim = c(-77, -63), ylim = c(34, 48) ) + theme_bw()
Map0 <- Map + geom_polygon( data=NAm, aes(x=long, y=lat, group=group),colour="white", fill="grey62" )  + coord_cartesian(xlim = c(-77, -63), ylim = c(34, 48) ) + geom_text(data=cnames, aes(x=long, y=lat, group=id, label = id), size=4) #+ coord_map()


#adds the US state map to the Atlantis ggplot object, limiting it to the spatial region of NEUS

Map0 # plots the NEUS map with the world map (showing US and Canada)

#setwd("~/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS Shape") #directory with NEUS shape files
ggsave("NEUS area states and CAN.pdf", width = 15, height = 15, dpi = 400) # save plot to file


### NEUS Model map for PlosONE skill assessment publication
bboxes<-c(0, 23, 24,  25, 26, 27, 28, 29)
NEUS.f$BB<-c("Main")
NEUS.f[NEUS.f$id %in% bboxes,]$BB<-c("BBox")
bb.colors<-c("gold2", "deepskyblue1", "red")

# extract area 23 and 24 these are hidden by islands
a23_24<-rbind(subset(NEUS.f, id==23), subset(NEUS.f, id==24))

# extract Boston and New York
data(us.cities)
cities<-rbind(subset(us.cities, name=="Boston MA"), subset(us.cities, name=="New York NY"))
cities$group<-0
cities$BB<-c("X")
cities$id<-0
NewYork<-data.frame(x=-73.94,y=40.67, group=1 )
NewYork$BB<-c("X")
NewYork$id<-0


bb.map<-ggplot(NEUS.f, aes(long, lat, group = group, fill =BB, label=id))  + geom_polygon(colour="white") + theme_bw() + scale_fill_manual(values=bb.colors, guide=FALSE) + geom_polygon( data=NAm, aes(x=long, y=lat, group=group),colour="white", fill="grey80" )  + coord_cartesian(xlim = c(-77, -63), ylim = c(34, 48) )

bb.map + geom_polygon(data=a23_24, colour="white", fill="gold2", aes(x=long, y=lat, group=group)) + geom_point(data=cities, aes(x=long, y=lat)) + geom_text(data=cities, size=6,  hjust=1.1, aes(x=long, y=lat, label=name))

ggsave("NEUS area states REV.tiff")


#####
## Map showing spatial scenario areas
CW<-c(7)
GOM<-c(10,11, 12, 16, 17, 18, 19,  20) 
GB<-c(8, 9, 12, 13, 14,  15)
NEUS.f$CA<-c("No")
NEUS.f[NEUS.f$id %in% CW,]$CA<-c("WF")
NEUS.f[NEUS.f$id %in% GOM,]$CA<-c("GOM")
NEUS.f[NEUS.f$id %in% GB,]$CA<-c("GB")
#NEUS.f[NEUS.f$id %in% 8,]$CA<-c("CW&GB")


mapcolors<-brewer.pal(4, "Set2")
mapcolors[4]<-mapcolors[3]
mapcolors[3]<-c("#CCCCCC")

Map2<-ggplot(NEUS.f, aes(long, lat, group = group, fill =CA, label=id)) + geom_polygon(colour="white") + theme_bw() + scale_fill_manual(values=mapcolors, guide=FALSE)

Map2

# add world map

Map2 <- Map2 +  geom_polygon( data=NAm, aes(x=long, y=lat, group=group),colour="white", fill="grey62" )  + coord_cartesian(xlim = c(-77, -63), ylim = c(34, 48) ) 


ggsave("Spatial scenarios NEUS area.pdf", width = 15, height = 15, dpi = 400) 
ggsave("Spatial scenarios NEUS area.png", width = 10, height = 10, dpi = 400) 



+ geom_text(data=cnames, aes(x=long, y=lat, group=id, label = id), size=4)




### Mapping EcoRegions on top of NEUS-Atlantis map
# NOAA NEFSC Ecoregions
setwd("~/Documents/G-copy/USA studieopphold/atlantis/Ecoregions") 

GB <- readOGR(dsn = ".", "GB_core") #import shapefile for Georges Bank
GoM <- readOGR(dsn = ".", "GoM_core") #import shapefile  Gulf of Maine
SS <- readOGR(dsn = ".", "SS_core") #import shapefile Scotian Shelf
MAB <- readOGR(dsn = ".", "mab_core") #import shapefile Mid Atlantic Bigh

slotNames(GB) #Check variable names to 

#creates X-Y points for polygons for the various regions using the 'fortify command
GB.f<-fortify(GB) 
GoM.f <- fortify(GoM)
SS.f <- fortify(SS)
MAB.f <- fortify(MAB)

# add ecoregions to NEUS map
Map1 <- Map0 + geom_polygon( data=GB.f, aes(x=long, y=lat, group = group),colour="red3", fill="NA") + geom_polygon( data=GoM.f, aes(x=long, y=lat, group = group),colour="green4", fill="NA")  + geom_polygon( data=SS.f, aes(x=long, y=lat, group = group),colour="navy", fill="NA")  + geom_polygon( data=MAB.f, aes(x=long, y=lat, group = group),colour="gray24", fill="NA")  

Map1

setwd("~/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS Shape")
ggsave("NEUS area EcoRegions.pdf", scale = 1, width = 10, height = 10, dpi = 400) # save plot to file

### Plotting NEUS areas on Google Map
# there is some problem with projection...

#al1 <- get_map(location = c(lon = -70, lat = 42), zoom = 6, maptype = 'satellite')
#NEUSGooglM1 <- ggmap(al1) 
#NEUSGooglM1 + geom_polygon(data=NEUS.f, aes(x=long, y=lat, group = group, fill =id, label=id), colour="darkseagreen2", fill=NA) 

#+ geom_text(data=cnames, aes(x=long, y=lat, group=id, label = id), colour="white", size=4) + coord_map()


