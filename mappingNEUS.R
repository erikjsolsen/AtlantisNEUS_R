# Mapping NEUS model area and component info on a map of US states
# By: Erik Olsen
# Date: 21.01.2014

# example


library(rgdal)

setwd("~/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS Shape") #directory with NEUS shape files

NEUSarea <- readOGR(dsn = ".", "neus30_2006v2") #import shapefile 
# 12. jan 2014 - the original shape file had errors in the area numbering. Fixed the numbering manually in Manifold and exported the shapefile again
slotNames(NEUSarea) # look at the slotnames
names(NEUSarea)
str(NEUSarea, max.level=3)

#changes projection to LAT LON using projection information gathered from the 'slotNames' command above
NEUSarea@proj4string
NEUSarea<-spTransform(NEUSarea, CRS("+proj=longlat +ellps=GRS80")) #transform to LAT and LONG coordinates

library(ggplot2)

# p <- ggplot(NEUSarea@data)


## Plotting the Atlantis geometry
library(rgeos)

NEUS.f<-fortify(NEUSarea, region="AAREA") #creates X - Y points of the polygons

cnames <- aggregate(cbind(long, lat) ~ id, data=NEUS.f, FUN=function(x)mean(range(x)))

Map <- ggplot(NEUS.f, aes(long, lat, group = group, fill =id, label=id)) + geom_polygon(colour="darkseagreen2", fill="steelblue1") +   coord_equal() + labs(x = "Longitude", y = "Latitude") + 
  ggtitle("NEUS map")  #+ geom_text(size=3)#actual ggplot2 function creating a ggplot map object called 'Map'

Map <- Map + geom_text(data=cnames, aes(x=long, y=lat, group=id, label = id), size=4) + coord_map()

Map # plots NEUS map area


## Adding map of US states to the Atlantis geometry (previous map)
library("maps", lib.loc="/Users/eriko/Library/R/3.0/library")
all_states <- map_data("state") # get US State dataset from maps library
all_states$id<-0 # add an ID column so that NEUS.f and all_states can be compared. 

Map <- Map + geom_polygon( data=all_states, aes(x=long, y=lat, group=group),colour="white", fill="grey52" ) + xlim(-77,-63) +ylim(34,48) #adds the US state map to the Atlantis ggplot object, limiting it to the spatial region of NEUS

Map # plots the NEUS map with the US states
ggsave("NEUS area states.pdf", scale = 1, dpi = 400) # save plot to file



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
Map1 <- Map + geom_polygon( data=GB.f, aes(x=long, y=lat, group = group),colour="red3", fill="NA") #add Georges bank to plot 
Map1 <- Map1 + geom_polygon( data=GoM.f, aes(x=long, y=lat, group = group),colour="green4", fill="NA") #add Gulf og Maine to
Map1 <- Map1 + geom_polygon( data=SS.f, aes(x=long, y=lat, group = group),colour="navy", fill="NA") #add Scotian Shelf to
Map1 <- Map1 + geom_polygon( data=MAB.f, aes(x=long, y=lat, group = group),colour="gray24", fill="NA") #add Mid-Atlantic Bigh to
Map1 <- Map1 + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey52" ) + xlim(-77,-63) +ylim(34,48) #adds the US state map to the Atlantis ggplot object, limiting it to the spatial region of NEUS

Map1

ggsave("NEUS area EcoRegions.pdf", scale = 1, dpi = 400) # save plot to file
